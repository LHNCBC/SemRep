:- use_module(library('clpqr/monash')).

/*
 **********************************************************************
 *
 *      CLP(R) Version 2.0	(Example Programs Release)
 *	(C) Copyright, March 1986, Monash University
 *
 **********************************************************************
 */

% 
% Transistor amplifier design and analysis package.
% The goal
%              ?- go1.
% analyses an existing amplifier circuit, while the goal
%              ?- go2.
% imposes certain design constraints on an amplifier of
% a certain form and then determines suitable component values,
% by choosing them from a list of available (preffered) components.
%

/****************************************************************************/
/*  Major goals                                                             */
/****************************************************************************/
dc_analysis(Vcc1,Vcc2,Circuit):-
	solve_dc(mean,Circuit ,[n(cc1,Vcc1,[_]),n(cc2,Vcc2,[_]),n(gnd,0,[_])],
		 Nodelist,Collector_Currents),
	current_solve(Nodelist),
	print_value(Nodelist),
	print_circuit(Circuit).

full_analysis(Vcc1,Vcc2,Circuit,In,Out,Type,Stability,Gain,Inresist,Outresist):-
	{Inresist = -1 / Iin},
	{Gain = Vout},
	{Outresist = -1 / Iout},
	circuit(Vcc1, Vcc2, Circuit,In, Out,Type),
	solve_dc(mean,Circuit ,[n(cc1,Vcc1,[_]), n(cc2,Vcc2,[_]), n(gnd,0,[_])],
		 Nodelist,Collector_Currents),
	current_solve(Nodelist),
	%print_value(Nodelist),
	stability(Vcc1,Vcc2,Circuit,Collector_Currents,Stability),
	printf("Stab %\n",[Stability]),
	solve_ss(Circuit,Collector_Currents,
		[n(cc1,0,[_]),n(cc2,0,[_]),n(gnd,0,[_]),
		 n(In,1,[Iin]),n(Out,Vout,[])],Nodelist2),
	current_solve(Nodelist2),
	%print_value(Nodelist2),
	solve_ss(Circuit,Collector_Currents,
		[n(cc1,0,[_]),n(cc2,0,[_]),n(gnd,0,[_]),
		 n(Out,1,[Iout])],Nodelist3),
	%print_value(Nodelist3),
	current_solve(Nodelist3),
	%print_value(Nodelist3),
	printf("Outresist % \n",[Outresist]),
	print_circuit(Circuit).
	
/****************************************************************************/
/*  small signal equivalent solve                                          */
/****************************************************************************/
solve_ss([],[], List,List).
solve_ss([[Component,_,Data,Points]|Rest],CCin,Innodes,Outnodes):-
	connecting(Points,Volts,Amps,Innodes,Tmpnodes),
	component_ss(Component,Data,Volts,Amps,CCin,CCout),
	solve_ss(Rest,CCout,Tmpnodes,Outnodes).

component_ss(resistor,R,[V1,V2],[I,I2],Cc,Cc):-	
	{V1-V2 = R*I},
	{I2 = -I},
	resistor_val(R).
component_ss(capacitor,_,[V1,V2],[I,I2],Cc,Cc):-
	{V1 = V2},
	{I2 = -I}.
component_ss(transistor,[npn,Code,active],[Vb,Vc,Ve], [Ib,Ic,Ie],[Icol|CC],CC):-
	{Vb - Ve = (Beta * Vt / Icol) * Ib},
	{Ic = Beta * Ib},
	{Ie + Ic + Ib = 0},
	transistor_type(Type,Code,Beta,_,_,Vt,mean).
	

/****************************************************************************/
/*  dc component solving                                                    */
/****************************************************************************/
solve_dc(_, [], List, List, []).
solve_dc(Kind,[[Component,_,Data,Points] | Rest], Inlist,Outlist,CCin):-
	connecting(Points, Volts, Amps, Inlist,Tmplist),
	component_dc(Component,Data,Volts,Amps,CCin,CCout,Kind),
	solve_dc(Kind, Rest, Tmplist,Outlist,CCout).

component_dc(resistor,R,[V1,V2],[I,-1*I],Cc,Cc,_):-
	{V1-V2 = R*I},
	resistor_val(R).
component_dc(capacitor,_,[V1,V2],[0,0],Cc,Cc,_).
component_dc(transistor,[Type,Code,State],Volts, [Ib,Ic,Ie],[Ic|CC],CC,Kind):-
	transistor_type(Type,Code,Beta,Vbe,Vcesat,_,Kind),
	transistor_state(Type,State,Beta,Vbe,Vcesat,Volts,[Ib,Ic,Ie]).
component_dc(diode,[Code,State],Volts,Amps,Cc,Cc,_):-
	diode_type(Code,Vf,Vbreak),
	diode_state(State,Vf,Vreak,Volts,Amps).

/****************************************************************************/
/*   diode and transistor states / and relationships                        */
/****************************************************************************/
diode_state(forward,Vf,Vbreak,[Vp,Vm],[I, -1*I]):-
	/* forward biased */
	{Vp - Vm = Vf},
	{I >= 0}.
diode_state(reverse,Vf,Vbreak,[Vp,Vm],[I, -1*I]):-
	/* reverse biased */
	{Vp - Vm < Vf},
	{Vm - Vp < Vbreak},
	{I = 0}.

transistor_state(npn, active, Beta, Vbe,_,[Vb, Vc, Ve], [Ib, Ic, Ie]):-
	{Vb = Ve + Vbe},
	{Vc >= Vb},
	{Ib >= 0},
	{Ic = Beta*Ib},
	{Ie+Ib+Ic = 0}. 
transistor_state(pnp, active, Beta, Vbe,_,[Vb, Vc, Ve], [Ib, Ic, Ie]):-
	{Vb = Ve + Vbe},
	{Vc <= Vb},
	{Ib <= 0},
	{Ic = Beta*Ib},
	{Ie+Ib+Ic = 0}.
transistor_state(npn, saturated, Beta, Vbe, Vcesat,[Vb, Vc, Ve], [Ib, Ic, Ie]):-
	{Vb = Ve + Vbe},
	{Vc = Ve + Vcesat},
	{Ib >= 0},
	{Ic >= 0},
	{Ie+Ib+Ic = 0}. 
transistor_state(pnp, saturated, Beta, Vbe, Vcesat,[Vb, Vc, Ve], [Ib, Ic, Ie]):-
	{Vb = Ve + Vbe},
	{Vc = Ve + Vcesat},
	{Ib <= 0},
	{Ic <= 0},
	{Ie+Ib+Ic = 0}. 
transistor_state(npn, cutoff, Beta, Vbe, Vcesat,[Vb, Vc, Ve], [Ib, Ic, Ie]):-
	{Vb <= Ve + Vbe},
	{Ib = 0},
	{Ic = 0},
	{Ie = 0}.
transistor_state(pnp, cutoff, Beta, Vbe, Vcesat,[Vb, Vc, Ve], [Ib, Ic, Ie]):-
	{Vb >= Ve + Vbe},
	{Ib = 0},
	{Ic = 0},
	{Ie = 0}.

/****************************************************************************/
/*   connecting components routines                                         */
/****************************************************************************/
connecting([],[],[],List,List).
connecting([P|PR],[V|VR],[I|IR], Inlist,Outlist):-
	connect(P,V,I,Inlist,Tmplist),
	connecting(PR,VR,IR,Tmplist,Outlist).

connect(P,V,I,[],[n(P,V,[I])]):-!.
connect(P,V,I, [n(P,V,Ilist) | Rest],[n(P,V,[I|Ilist])|Rest]):-!.
connect(P,V,I, [A|Rest], [A|Newrest]) :-
	connect(P,V,I, Rest, Newrest).

/****************************************************************************/
/*  Stability Analysis                                                      */
/****************************************************************************/
stability(Vcc1,Vcc2,Circuit, CollectorCurrents, Stability):-
	solve_dc(minn,Circuit ,[n(cc1,Vcc1,[_]),n(cc2,Vcc2,[_]),n(gnd,0,[_])],
		 Nodelist1,MinCurrents),
	current_solve(Nodelist1),
	% printf(" Min %\n Minmodes \n",[MinCurrents]),
	% print_value(Nodelist1),
	solve_dc(maxx,Circuit ,[n(cc1,Vcc1,[_]),n(cc2,Vcc2,[_]),n(gnd,0,[_])],
		 Nodelist2,MaxCurrents),
	current_solve(Nodelist2),
	% printf(" Max %\n Maxnodes\n",[MaxCurrents]),
	% print_value(Nodelist2),
	calculate(MinCurrents,MaxCurrents,CollectorCurrents,Stability).
	
calculate(MinCurrents,MaxCurrents,CollectorCurrents,Stability):-
	cal(MinCurrents,MaxCurrents,CollectorCurrents,Percents),
	% printf(" Percent % \n",[Percents]),
	maxi(Percents,0,Stability).

cal([Min|Rin],[Max|Rax],[Ic|Rc],[Pc|Rpc]):-
	{Pc=max(Ic-Min,Max-Ic)},
	cal(Rin,Rax,Rc,Rpc).
cal([],[],[],[]).

maxi([N1|R],N2,P):-
	{M=max(N1,N2)},
	maxi(R,M,P).
maxi([],P,Q) :- {P = Q}.
/****************************************************************************/
/*  Miscellaneous things                                                    */
/****************************************************************************/
current_solve([]).
current_solve([n(_,_,L) | Rest]) :-
	kcl(L),
	current_solve(Rest).

print_value([]).
print_value([n(P,V,I) | Rest]) :-
	printf("% at % %\n",[P,V,I]),
	print_value(Rest).

print_circuit([]).
print_circuit([[Comp,Name,Data,Points] | Rest]) :-
	printf("  % at % %\n",[Comp,Name,Data]),
	print_circuit(Rest).

sum([X|T],Z) :-
	{X+P = Z},
	sum(T,P).
sum([],Z) :-
	{Z = 0}.

kcl(L) :- 
	sum(L,0).

/****************************************************************************/
/* Database of circuits and components                                      */
/****************************************************************************/
resistor_val(100).
resistor_val(50).
resistor_val(27).
resistor_val(5).
resistor_val(2).
resistor_val(1).

diode_type(di1, 0.6, 100).

transistor_type(npn, tr0, 100, 0.7, 0.3, 0.025,mean).
transistor_type(npn, tr0, 50, 0.8, 0.3, 0.025,minn).
transistor_type(npn, tr0, 150, 0.6, 0.3, 0.025,maxx).

transistor_type(pnp, tr1, 100, -0.7, -0.3, 0.025,mean).
transistor_type(pnp, tr1, 50, -0.8, -0.3, 0.025,minn).
transistor_type(pnp, tr1, 150, -0.6, -0.3, 0.025,maxx).

circuit(15,0,[
		[capacitor,c1,c1,[in,b]],
		[resistor,r1,R1,[b,cc1]],
		[resistor,r2,R2,[b,gnd]],
		[transistor,tr,[npn,tr0,active],[b,c,e]],
		[resistor,re,Re,[e,gnd]],
		[capacitor,c2,c2,[c,out]],
		[resistor,rc,Rc,[c,cc1]],
		[capacitor,c3,c3,[e,gnd]]],
	in,out,common_emitter).

circuit(15,0,[
		[capacitor,c1,C1,[gnd,b]],
		[resistor,r1,R1,[b,cc1]],
		[resistor,r2,R2,[b,gnd]],
		[transistor,tr,[pnp,tr1,active],[b,c,e]],
		[resistor,re,Re,[e,gnd]],
		[capacitor,c2,C2,[c,in]],
		[resistor,rc,Rc,[c,cc1]],
		[capacitor,c3,C3,[e,out]]],
	in,out,common_base).

circuit(15,0,[
		[capacitor,c1,C1,[in,b]],
		[resistor,r1,R1,[b,cc1]],
		[resistor,r2,R2,[b,gnd]],
		[transistor,tr,[npn,tr0,active],[b,cc1,e]],
		[resistor,re,Re,[e,gnd]],
		[capacitor,c3,C3,[e,out]]],
	in,out,emitter_follower).

go1:- dc_analysis(15,-12,[
		[diode,d1,[di1,St1],[a,gnd]],
		[diode,d2,[di1,St2],[a,cc1]],
		[resistor,r1,100,[a,cc1]],
		[diode,d3,[di1,St3],[a,p1]],
		[diode,d4,[di1,St4],[p1,b]],
		[resistor,r2,100,[b,cc2]],
		[transistor,tr,[npn,tr0,State],[b,c,gnd]],
		[resistor,rc,100,[c,cc1]]]).

% Answer:
%  cc1 at 15 [0, 0.144, 0, -0.144]
%  cc2 at -12 [-0.114, 0.114]
%  gnd at 0 [0, -0.03, 0.03]
%  a at 0.6 [0.114, -0.144, 0, 0.03]
%  p1 at 0 [0.114, -0.114]
%  b at -0.6 [0, 0.114, -0.114]
%  c at 15 [0, 0]
%    diode at d1 [di1, forward]
%    diode at d2 [di1, reverse]
%    resistor at r1 100
%    diode at d3 [di1, forward]
%    diode at d4 [di1, forward]
%    resistor at r2 100
%    transistor at tr [npn, tr0, cutoff]
%    resistor at rc 100

go2 :- {Stability < 0.5,
	Gain > 0.5,
	Inresistance >= 25, Outresistance <= 2},
	full_analysis(15, _, Circuit, _, _, emitter_follower, Stability,
		      Gain, Inresistance, Outresistance),
	write(Circuit),nl.

% Answer:
%  Stab 0.001947
%  Outresist 0.857107 
%    capacitor at c1 Data_48
%    resistor at r1 100
%    resistor at r2 100
%    transistor at tr [npn, tr0, active]
%    resistor at re 100
%    capacitor at c3 Data_52
%  [[capacitor, c1, C1_48, [in, b]], [resistor, r1, 100, [b, cc1]], 
%    [resistor, r2, 100, [b, gnd]], [transistor, tr, [npn, tr0, active], 
%    [b, cc1, e]], [resistor, re, 100, [e, gnd]], [capacitor, c3, C3_52, 
%    [e, out]]]

?- printf("\n>>> Sample goals: go1/0, go2/0\n", []).
