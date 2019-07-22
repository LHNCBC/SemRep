:- use_module(library('clpqr/monash')).

/*
 **********************************************************************
 *
 *      CLP(R) Version 2.0	(Example Programs Release)
 *	(C) Copyright, March 1986, Monash University
 *
 **********************************************************************
 */

% use clpr -z 1e-20

/*
 * RKF45 implementation in CLP(R): See [Harland & Michaylov].
 *
 * Assumes that the function is evaluated by a predicate of the form
 *
 *	eval(T, Y, Yp), where T is the independent variable,
 *			Y the current vector of solutions,
 *			Yp the new vector derivative 
 *
 * e.g. eval(T, [Y1,Y2], [T*T + Y1*Y1, T*T - Y2*Y2]).
 *
 * Note: machine epsilon is available as #zero (pre-defined constant).
 *
 * Separate "mode" from the rest - may help efficiency
 */

/*
 * solve(Y, T, Tout, Incr, Relerr, Abserr)
 * Predicate called by the user - if he wants to specify a mode, he calls
 * solve/7 direct (only needed for single_step mode).
 *
 * Y - 		initial position vector
 * T -		starting point for independent variable
 * Tout - 	last point at which output is desired
 * Incr - 	default output increment 
 * Relerr - 	relative error tolerance
 * Abserr - 	absolute error tolerance
 */

solve(Y, Tstart, Tend, Incr, Relerr, Abserr) :-
	solve(Y, Tstart, Tend, Incr, Relerr, Abserr, normal).

/*
 *  Error checks
 */
solve(_, _, _, 0, _, _, normal) :-
	printf("Increment must be non-zero for normal mode\n",[]).
solve(_, _, _, _, Relerr, _, _) :-
	{Relerr < 0},
	printf("Relative error must be non-negative\n",[]).
solve(_, _, _, _, _, Abserr, _) :-
	{Abserr < 0},
	printf("Relative error must be non-negative\n",[]).
solve(_, T, T, _, _, _, _) :-
	printf("No interval\n",[]).

/*
 * Everything seems ok, so proceed
 */
solve(Y, T, Tend, Incr, Relerr, Abserr, Mode) :-
	set_yp(T, Y, Yp),		% Nfe = Nfe + 1
	output(T, Y),
	solve1(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
	       work(Yp, 1, 0)).		% Work out the rest
	       %	Nfe = 1
	       %	Kop = 0

/*
 * Commence main iteration
 */
solve1(user(_, T, Tend, Incr, _, _, _), work(_, _, Nfe, _, _)) :-
	{abs(T - Tend) < Incr / 4},
	printf("\nIteration finished\n------------------\n",[]),
	printf(" %d  derivative evaluations\n",[Nfe]).
solve1(user(Y, T, Tend, Incr, Relerr, Abserr, reset(Mode, relerr)), _) :-
	remin(Remin),
	{Relerr < 2* #zero + Remin},
	printf("Warned you to reset it larger than %f\n", [Relerr]).
solve1(user(Y, T, Tend, Incr, Relerr, Abserr, reset(Mode, relerr)),
	work(Yp, H, Nfe, Kop, Tout)) :-
	remin(Remin),
	{Relerr >= 2* #zero + Remin},
	solve1(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
		work(Yp, H, Nfe, Kop, Tout)).
solve1(user(Y, T, Tend, Incr, Relerr, Abserr, reset(Mode, abserr, Oerr)), _) :-
	{Abserr <= Oerr},
	printf("Abserr must be increased - not set to %f\n", [Abserr]).
solve1(user(Y, T, Tend, Incr, Relerr, Abserr, reset(Mode, abserr, Oerr)),
	work(Yp, H, Nfe, Kop, Tout)) :-
	{Abserr > Oerr},
	solve1(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
		work(Yp, H, Nfe, Kop, Tout)).
solve1(user(Y, T, Tend, Incr, Relerr, Abserr, Mode), work(Yp, Nfe, Kop)) :-
	% First time - need to set H
	set_tout(T, Tend, Incr, Tout),	% Next output point
	set_hinit(T, Tout-T, Y, Yp, Relerr, Abserr, H),
	set_kop(Kop, H, Tout-T, Newkop),
	stop_or_iter(user(Y, T, Tend, Incr, Relerr, Abserr, normal),
		     work(Yp, H, Nfe, Newkop, Tout)).
solve1(user(Y, T, Tend, Incr, Relerr, Abserr, Mode), 
	% H already set - continue on
	work(Yp, H, Nfe, Kop, _)) :-
	set_tout(T, Tend, Incr, Tout),
	sign(Tout-T, S),
	set_kop(Kop, S*H, Tout-T, Newkop),
	stop_or_iter(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
		     work(Yp, S*H, Nfe, Newkop, Tout)).

/*
 * Perform certain error checks
 */
stop_or_iter(User, work(Yp, H, Nfe, Kop, Tout)) :-
	{Kop >= 100},
	check_out(User, work(Yp, H, Nfe, 0, Tout), output_excess).
stop_or_iter(user(Y, T, Tend, Incr, Relerr, Abserr, Mode), Work) :-
	remin(Remin),
	{Relerr < 2* #zero + Remin},
	check_out(user(Y, T, Tend, Incr, Relerr, Abserr, Mode), Work, relerr).
stop_or_iter(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
	     work(Yp, H, Nfe, Kop, Tout)) :-
	{Abs = abs(Tout-T)}, 
	{Tabs = abs(T)},
	% Use Euler approximation
	{Abs <= 26* #zero*Tabs},
	set_euler(Tout-T, Y, Yp, NewY),
	% Nfe = Nfe + 1
	set_yp(Tout, NewY, NewYp),		
	print_or_it(user(NewY, Tout, Tend, Incr, Relerr, Abserr, 
		mode(Mode, output)), work(NewYp, H, Nfe+1, Kop, Tout)).
stop_or_iter(User, Work) :-
	% Go on with main loop .......
	iter(User, Work).

iter(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
     work(Yp, H, Nfe, Kop, Tout)) :-
	set_modeh(Tout-T, H, NewH, Out),
	iter1(user(Y, T, Tend, Incr, Relerr, Abserr, mode(Mode, Out, nofail)),
	     work(Yp, NewH, Nfe, Kop, Tout)).

iter1(user(Y, T, Tend, Incr, Relerr, Abserr, mode(Mode, Out, Hmode)),
      work(Yp, H, Nfe, Kop, Tout)) :-
	max_func_eval(Maxfunc),
	{Nfe >= Maxfunc},
	check_out(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
	     work(Yp, H, Nfe, Kop, Tout), max_func).
iter1(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
      work(Yp, H, Nfe, Kop, Tout)) :-
      fehl(Y, T, Yp, H, Sum, Abserr, Relerr, Errest, Ind),
      stop_or_iter1(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
      		    work(Yp, H, Nfe+5, Kop, Tout), Sum, Errest, Ind).

stop_or_iter1(user(Y, T, Tend, Incr, Relerr, Abserr, mode(Mode, Out, Hmode)),
      	      Work, _, _, vanished) :-
	check_out(user(Y, T, Tend, Incr, Relerr, Abserr, Mode), Work, vanished).
stop_or_iter1(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
      	      work(Yp, H, Nfe, Kop, Tout), Sum, Esttol, okay) :-
	{Esttol > 1},
	set_smin(Esttol, S),
	check_h(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
		work(Yp, S*H, Nfe, Kop, Tout)).
stop_or_iter1(user(Y, T, Tend, Incr, Relerr, Abserr, mode(Mode, Out, Hmode)),
      	      work(Yp, H, Nfe, Kop, Tout), Sum, Esttol, okay) :-
	{Esttol <= 1},
	% Nfe = Nfe+1
	set_yp(T+H, Sum, NewYp),
	set_hmax(Esttol, Hmode, T, H, NewH),
	print_or_it(user(Sum, T+H, Tend, Incr, Relerr, Abserr, 
		mode(Mode, Out)), work(NewYp, NewH, Nfe+1, Kop, Tout)).

print_or_it(user(Y, T, Tend, Incr, Relerr, Abserr, 
		mode(normal,proceed)), Work) :-
	iter(user(Y, T, Tend, Incr, Relerr, Abserr, normal), Work).
print_or_it(user(Y, T, Tend, Incr, Relerr, Abserr, 
		mode(normal, output)), Work) :-
	output(T, Y),
	solve1(user(Y, T, Tend, Incr, Relerr, Abserr, normal), Work).
print_or_it(user(Y, T, Tend, Incr, Relerr, Abserr, 
		mode(single_step, _)), Work) :-
	check_out(user(Y, T, Tend, Incr, Relerr, Abserr, single_step), Work).

check_h(user(Y, T, Tend, Incr, Relerr, Abserr, mode(Mode, _, _)),
	work(Yp, H, Nfe, Kop, Tout)) :-
	{Habs = abs(H)}, 
	{Tabs = abs(T)},
	{Habs <= 26* #zero*Tabs},
	printf("Tabs = %e , Habs = %e , #zero  = %e \n",[Tabs,Habs,#zero]),
	check_out(user(Y, T, Tend, Incr, Relerr, Abserr, Mode),
		  work(Yp, H, Nfe, Kop, Tout), step_small).
check_h(user(Y, T, Tend, Incr, Relerr, Abserr, mode(Mode, Out, _)), Work) :-
	iter1(user(Y, T, Tend, Incr, Relerr, Abserr, mode(Mode,proceed,hfail)),
	      Work).

/*
 * check_out(user(Y, Tstart, Tend, Incr, Relerr, Abserr, Mode),
 *           work(Yp, H, Nfe, Kop, Tout), Flag)
 * Checks the status variable Flag, and takes appropriate action.
 * See if there is a user-defined procedure
 */
check_out(User, Work, Flag) :- 
	user_error(User, Work, Flag), 
	!.
% No user procedure, so use the default ones.
check_out(user(Y, T, Tend, Incr, Relerr, Abserr, single_step),
       Work, single_step) :-
	printf("(single_step) T: %10.6f      Y: %10.6f\n",[T,Y]),
	solve(Y, T, Tend, Incr, Relerr, Abserr, single_step),!.
check_out(user(Y, T, Tend, Incr, Relerr, Abserr, Mode), Work, relerr) :-
	% Iflag = 3
	remin(Remin),
	{Rer = 3* #zero + Remin},
	printf("Relative error too small - reset to %f\n", [Rer]),
	solve1(user(Y, T, Tend, Incr, Rer, Abserr, reset(Mode, relerr)), Work).
check_out(User, work(Yp, H, Nfe, Kop, Tout), max_func) :-
	% Iflag = 4
	max_func_eval(Max),
	{Nfe >= Max},
	printf("Lots of function evaluations - %f needed so far\n",[Max]),
	solve1(User, work(Yp, H, 0, Kop, Tout)).
check_out(user(Y, T, Tend, Incr, Relerr, Abserr, Mode), Work, vanished) :-
	% Iflag = 5
	abmin(Absmin),
	printf("Abserr reset to %f\n", [Absmin]),
	solve1(user(Y, T, Tend, Incr, Relerr, 10*Abserr + Absmin, 
		reset(Mode, abserr, Abserr)), Work).
check_out(user(Y, T, Tend, Incr, Relerr, Abserr, normal), Work, step_small) :-
	% Iflag = 6
	printf("Relerr too small; reset to %f\n", [10*Relerr]),
	solve1(user(Y, T, Tend, Incr, 10*Relerr, Abserr, normal), Work).
check_out(User, work(Yp, H, Nfe, Kop, Tout), output_excess) :-
	% Iflag = 7
	printf("Excessive output; this will be inefficient",[]),
	solve1(User, work(Yp, H, Nfe, 0, Tout)).
check_out(_, _, stuffed) :- 
	% Iflag = 8
	printf("Improper call - wrong number of arguments?\n").
check_out(_, _, missing) :-
	printf("User-supplied predicate eval is missing or wrong\n"),
	printf("Format is eval(T, Y, Yp),\n",[]),
	printf("where T is the independent variable, ",[]),
	printf("Y the vector, and Yp its derivative.",[]),
	printf("Y and Yp are CLP lists - [Y1, Y2, Y3], [Yp1, Yp2, Yp3] etc.",[]).
check_out(_, _, Other) :-
	printf("Damned if I know what''s wrong!\n",[]).

/*
 * output(T, Y).
 * output(user(Y, T, Tend, Incr, Relerr, Abserr), work(Yp, H, Nfe, Kop, Tout))
 * Output predicate. If there is a user_output predicate (matching the above
 * format), then it is used. Otherwise, T and Y are written to standard output.
 */
output(T, Y) :-
	user_output(T, Y), 
	!.
output(T, Y) :-
	printf("T: %10.6f      Y: ",[T]),
	write_list(Y).

write_list([L|List]) :-
	printf(" %10.6f ",[L]), 
	write_list(List).
write_list([]) :-
	nl.

/*
 * Set routines - setting values of variables given certain conditions etc.
 */

set_yp(T, Y, Yp) :-	
	eval(T, Y, Yp).
set_yp(T, [Y], [Yp]) :-	
	% For one var case with no list functor
	eval(T, Y, Yp).	

set_hinit(T, Dt, Y, Yp, Relerr, Abserr, H) :-
	{H1 = abs(Dt)},
	set_maxtol(H1, Y, Yp, Relerr, Abserr, Maxtol),
	set_h1(H1, T, Dt, Maxtol, H).

set_maxtol(H, [Y|Yrest], [Yp|Yprest], Relerr, Abserr, Maxtol) :-
	{Yabs = abs(Y)},
	{Tol1 = Relerr*Yabs + Abserr},
	{Tol1 > 0},
	{Ypabs = abs(Yp)},
	set_maxh(Ypabs, H, Tol1, H1),
	set_maxtol(H1, Yrest, Yprest, Relerr, Abserr, Tol2),
	{Maxtol = max(Tol1, Tol2)}.
set_maxtol(H, [Y|Yrest], [Yp|Yprest], Relerr, Abserr, Maxtol) :-
	{Yabs = abs(Y)},
	{Relerr*Yabs + Abserr <= 0},
	set_maxtol(H, Yrest, Yprest, Relerr, Abserr, Maxtol).
set_maxtol(_, [], [], _, _, 0).

set_maxh(Z, H, _, H) :-
	{Z = 0}.
set_maxh(Ypabs, H, Tol, NewH) :-
	{Ypabs > 0},
	{NewH = min(H, pow(Tol/Ypabs, 1/5))}.	% From abs(Yp)*h**5 <= Tol

set_h1(H, T, Dt, Tol, NewH) :-
	{NewH = 26* #zero*Max},
	{Tol <= 0},
	{Tabs = abs(T)}, 
	{Dtabs = abs(Dt)},
	{Max = max(Tabs, Dtabs)}.
set_h1(H, T, Dt, Tol, NewH) :-
	{Tol > 0},
	{Tabs = abs(T)}, 
	{Dtabs = abs(Dt)},
	{Max = max(Tabs, Dtabs)},
	{NewH = max(H, 26* #zero*Max)}.

set_modeh(Dt, H, H, proceed) :-
	{Dtabs = abs(Dt)}, 
	{Habs = abs(H)},
	{Dtabs >= 2*Habs}.
set_modeh(Dt, H, NewH, proceed) :-
	{NewH = Dt/2},
	{Habs = abs(H)}, 
	{Dtabs = abs(Dt)},
	{Dtabs < 2*Habs},
	{Dtabs > Habs}.
set_modeh(Dt, H, Dt, output) :-
	{Habs = abs(H)}, 
	{Dtabs = abs(Dt)},
	{Dtabs <= Habs}.

set_kop(Kop, H, Dt, NewK) :-
	{NewK = Kop+1},
	{Habs = abs(H)}, 
	{Dabs = abs(Dt)},
	{Habs >= 2*Dabs}.
set_kop(Kop, H, Dt, Kop) :-
	{Habs = abs(H)}, 
	{Dabs = abs(Dt)},
	{Habs < 2*Dabs}.

set_smin(Esttol, Z ) :-
	{Z = 0.9/(pow(Esttol, 1/5))},
	{Esttol < 59049}.
set_smin(Esttol, 0.1) :- 
	{Esttol >= 59049}.

set_hmax(Esttol, Hmode, T, H, Z) :-
	{Z = Sign*H1},
	set_smax(Esttol, Hmode, S),
	{Habs = abs(H)}, 
	{Tabs = abs(T)},
	{H1 = max(S*Habs, 26* #zero*Tabs)},
	sign(H, Sign).

set_smax(Esttol, nofail, S) :-
	{S = 5},
	{Esttol <= 0.0001889568}.
set_smax(Esttol, nofail, S ) :-
	{S = 0.9/(pow(Esttol, 1/5))},
	{Esttol > 0.0001889568}.
set_smax(Esttol, hfail, S) :-
	{S = 1},
	{Esttol <= 0.0001889568}.
set_smax(Esttol, hfail, S) :- 
	{Esttol > 0.0001889568},
	{S = min(0.9/(pow(Esttol, 1/5)), 1)}.

set_euler(Dt, [Y|Yrest], [Yp|Yprest], [NewY|NewYrest]) :-
	{NewY = Y + Dt*Yp},
	set_euler(Dt, Yrest, Yprest, NewYrest).
set_euler(_, [], [], []).

/*
 * set_tout(T, Tend, Incr, Tout)
 * Tout is the next output point, T the current value, Tend the final point
 * and Incr the set increment.
 *
 * We add Incr to T and proceed.
 * Note that Incr may be negative.
 */
set_tout(T, Tend, Incr, Tend) :-
	{Abs = abs(Tend - (T + Incr))},
	{Abs < Incr}.
set_tout(T, Tend, Incr, Tout) :-
	{Tout = T + Incr},
	{Abs = abs(Tend - (T + Incr))},
	{Abs >= Incr}.

/*
 * fehl(Y, T, Yp, H, Sum, Abserr, Relerr, Errest, Errind)
 * Predicate to perform the evaluation of Fehlberg formulae
 * 
 * Sum is the new estimate for Y
 * Abserr is the absolute error.
 * Relerr is the relative error.
 * Errest is the Fehlberg estimate of the error.
 * Errind indicates whether Abs is too small or not.
 * If so, Errind is set to "vanished". Otherwise, it is "okay".
 */
fehl(Y, T, Yp, H, S, Abs, Rel, Err, Ind) :-
	{Ch1 = H/4}, 
	{Ch2 = H*3/32}, 
	{Ch3 = H/2197},
	{Ch4 = H/4104}, 
	{Ch5 = H/20520}, 
	{Ch6 = H/7618050},
	fehl1(Y, T, Yp, H, ch(Ch1, Ch2, Ch3, Ch4, Ch5, Ch6), 
		S, Abs, Rel, Err, Ind).

fehl1(Y, T, Yp, H, ch(Ch1, Ch2, Ch3, Ch4, Ch5, Ch6), S, Abs, Rel, Err, Ind) :-
	% calculate each of the Fi .....
	set_f1(Y, Yp, Ch1, P1),			set_yp(T+Ch1, P1, F1),
	set_f2(Y, Yp, Ch2, F1, P2), 		set_yp(T+ 3*H/8, P2, F2),
	set_f3(Y, Yp, Ch3, F1, F2, P3), 	set_yp(T + 12*H/13, P3, F3),
	set_f4(Y, Yp, Ch4, F1, F2, F3, P4),  	set_yp(T + H, P4, F4),
	set_f5(Y, Yp, Ch5, F1, F2, F3, F4, P5),	set_yp(T + H/2, P5, F5),
	% .. then the Y estimate ....
	set_sum(Y, Yp, Ch6, F2, F3, F4, F5, S),
	% .. and finally the error
	set_err(Y, Yp, Abs, Rel, S, f(F2, F3, F4, F5), H, 0, Err, Ind).

set_f1([Y|Yr], [Yp|Ypr], Ch, [P|Pr]) :-
	{P = Y + Ch*Yp},
	set_f1(Yr, Ypr, Ch, Pr).
set_f1([], [], _, []).

set_f2([Y|Yr], [Yp|Ypr], Ch, [F1|F1r], [P|Pr]) :-
	{P = Y + Ch*(Yp + 3*F1)},
	set_f2(Yr, Ypr, Ch, F1r, Pr).
set_f2([], [], _, [], []).

set_f3([Y|Yr], [Yp|Ypr], Ch, [F1|F1r], [F2|F2r], [P|Pr]) :-
	{P = Y + Ch*(1932*Yp + (7296*F2 - 7200*F1))},
	set_f3(Yr, Ypr, Ch, F1r, F2r, Pr).
set_f3([], [], _, [], [], []).

set_f4([Y|Yr], [Yp|Ypr], Ch, [F1|F1r], [F2|F2r], [F3|F3r], [P|Pr]) :-
	{P = Y + Ch*((8341*Yp - 845*F3) + (29440*F2 - 32832*F1))},
	set_f4(Yr, Ypr, Ch, F1r, F2r, F3r, Pr).
set_f4([], [], _, [], [], [], []).

set_f5([Y|Yr], [Yp|Ypr], Ch, [F1|F1r], [F2|F2r], [F3|F3r], [F4|F4r], [P|Pr]) :-
	{P = Y + Ch*((0-6080*Yp + (9295*F3 - 5643*F4)) + (41040*F1-28352*F2))},
	set_f5(Yr, Ypr, Ch, F1r, F2r, F3r, F4r, Pr).
set_f5([], [], _, [], [], [], [], []).

set_sum([Y|Yr], [Yp|Ypr], Ch, [F2|F2r], [F3|F3r], [F4|F4r], [F5|F5r], [S|Sr]) :-
	{S = Y + Ch*((902880*Yp + (3855735*F3 - 1371249*F4)) + (3953664*F2 + 277020*F5))},
	set_sum(Yr, Ypr, Ch, F2r, F3r, F4r, F5r, Sr).
set_sum([], [], _, [], [], [], [], []).

set_err([Y|Yr], Yp, Abs, Rel, [S|Sr], f(F2, F3, F4, F5), H, Oerr, Err, Ind) :-
	{Yabs = abs(Y)}, 
	{Sabs = abs(S)},
	{Et = Yabs + Sabs + Abs},
	check_err(Et, [Y|Yr], Yp, [S|Sr], f(F2, F3, F4, F5),
		Abs, Rel, H, Oerr, Err, Ind).
set_err([], [], _, Rel, [], _, H, Err, Err1, okay) :-
	{Err1 = Habs*Err*(2/Rel)/752400},
	{Habs = abs(H)}.

check_err(Et, Y, Yp, S, _, _, _, _, _, _, vanished) :-
	{Et <= 0}.
check_err(Et, [Y|Yr], [Yp|Ypr], [S|Sr], f([F2|R2], [F3|R3], [F4|R4], [F5|R5]),
		Abs, Rel, H, Oerr, Err, Ind) :-
	{Et > 0},
	{Ee = abs( (0-2090*Yp + (21970*F3 - 15048*F4)) + (22528*F2 - 27360*F5))},
	{Nerr = max(Oerr, Ee/Et)},
	set_err(Yr, Ypr, Abs, Rel, Sr, f(R2, R3, R4, R5), H, Nerr, Err, Ind).

% Low level stuff - primitives etc.

sign(V, S) :-
	{S = 1},
	{V >= 0}.
sign(V, S) :-
	{S = 0-1},
	{V < 0}.

% Magic numbers
remin(0.000000000001).	% 1.0e-012 - from FM&M.
abmin(0.000000001).	% 1.0e-009 - from FM&M.
max_func_eval(3000).	% Also from FF&M.

eval(T, [Y1, Y2, Y3, Y4], [Y3, Y4, Y5, Y6]) :-
	{Y5 = 0-Y1/R},
	{Y6 = 0-Y2/R},
	{R1 = Y1*Y1 + Y2*Y2},
	{R = R1*pow(R1, 0.5)/((#p/4)*(#p/4))}.

user_output(T, [Y1, Y2, Y3, Y4]) :-
	{V1 = T},
	{V2 = Y1},
	{V3 = Y2},
	printf("Point %10.5f : %10.5f %10.5f\n",[V1,V2,V3]).

user_error(user(Y, T, Tend, Incr, Rer, Abs), 
	work(Yp, H, Nfe, Kop, Tout), max_func) :-
	printf("Too hard for me - try something more accurate\n",[]).

user_error(user(Y, T, Tend, Incr, Rer, Abs), 
	work(Yp, H, Nfe, Kop, Tout), output_excess) :-
	printf("Outputs too often - try another method\n",[]).

user_error(user(Y, T, Tend, Incr, Rer, Abs), 
	work(Yp, H, Nfe, Kop, Tout), step_small) :-
	printf("Can''t achieve required accuracy\n",[]).

go:-
	{Ecc = 0.25},
	solve([1-Ecc, 0, 0, (#p/4)*pow((1+Ecc)/(1-Ecc), 0.5)], 0, 3, 0.5, 0.000000001, 0).

% Output:
%
%  Point    0.00000 :    0.75000    0.00000
%  Point    0.50000 :    0.61977    0.47779
%  Point    1.00000 :    0.29442    0.81218
%  Point    1.50000 :   -0.10518    0.95804
%  Point    2.00000 :   -0.49030    0.93987
%  Point    2.50000 :   -0.81394    0.79959
%  Point    3.00000 :   -1.05403    0.57571
%  
%  Iteration finished
%  ------------------
%   439  derivative evaluations

?- printf("\n>>> Sample goal: go/0\n", []).
