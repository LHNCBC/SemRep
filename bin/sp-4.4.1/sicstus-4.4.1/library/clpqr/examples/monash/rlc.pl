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
% Package for sinusoidal steady state analysis of RLC circuits.
% The two goals     ?- go1    and     ?- go2      analyse two
% circuits of moderate complexity.
%


circuit_solve(W, L, G, Selection) :-
	get_node_vars(L, NV),
	solve(W, L, NV, Handles, G),
	format_print(Handles, Selection).

get_node_vars([[Comp, Num, X, Ns]|Ls], NV) :-
	get_node_vars(Ls, NV1),
	insert_list(Ns, NV1, NV).
get_node_vars([], []).

insert_list([N|Ns], NV1, NV3) :-
	insert_list(Ns, NV1, NV2),
	insert(N, NV2, NV3).
insert_list([], NV, NV).

insert(N, [[N, V, I]|NV1], [[N, V, I]|NV1]) :- !.
insert(N, [[N1, V, I]|NV1], [[N1, V, I]|NV2]) :-
	insert(N, NV1, NV2).
insert(N, [], [[N, V, c(0,0)]]).

eps(0.001).

solve(W, [X|Xs], NV, [H|Hs], G) :-
	addcomp(W, X, NV, NV1,H),
	solve(W, Xs, NV1, Hs, G).
solve(W, [], NV, [], G) :- 
	!,
	zero_currents(NV),
	ground_nodes(NV, G).

zero_currents([[N, V, c(X,Y)]|Ls]) :-
	zero(X),
	zero(Y),
	zero_currents(Ls).
zero_currents([]).

zero(X) :-
	{X = 0}.
zero(X) :- 
	ground(X),
	%printf("Is it ground: % ?\n", [X]),
	eps(EPS),
	{X > -EPS},
	{X < EPS}.

/* There is some redundancy setting all node current sums to zero - We only
 need to some of them to zero.  Setting all of them to zero can cause some
 problems because of numerical instability.  */

ground_nodes(Vs,[N|Ns]) :-
	ground_node(Vs, N),
	ground_nodes(Vs, Ns).
ground_nodes(Vs, []).
ground_node([[N, c(Z1,Z2), I]|Vs], N) :-
	{Z1 = 0},
	{Z2 = 0},
	!.
ground_node([[N1, V, I]|Vs], N) :- ground_node(Vs, N).
ground_node([], N) :- printf("Error could be: node %doesn't exist\n", [N]).


% ***** CLAUSES TO DEFINE COMPONENT CHARACTERISTICS *****

addcomp(W, [Comp2, Num, X, [N1, N2]], NV, NV2, [Comp2, Num, X, [N1, V1, I1], [N2, V2, I2]]):-  
	c_neg(I1, I2),
	iv_reln(Comp2, I1, V, X, W),
	c_add(V, V2, V1),
	subst([N1, V1, Iold1], [N1, V1, Inew1], NV, NV1),
	subst([N2, V2, Iold2], [N2, V2, Inew2], NV1, NV2),
	c_add(I1,Iold1,Inew1),
	c_add(I2,Iold2,Inew2).

% Specific current/voltage relations for the two terminal components
iv_reln(resistor, I, V, R, W) :- 
	c_mult(I,c(R,0),V).
iv_reln(voltage_source, I, V, V, W). 
iv_reln(isource, I, V, I, W). 
iv_reln(capacitor, I, V, C, W) :- 
	c_mult( c(0,W * C),V,I).
iv_reln(inductor, I, V, L, W) :- 
	c_mult(c(0,W * L),I,V).
iv_reln(connection, I, c(0,0), L, W).
iv_reln(open, c(0,0), V, L, W).
iv_reln(diode, I, V, D, W) :- diode(D, I, V).

diode(in914, c(I,0), c(V, 0)) :-
	{V < -100},
	{DV = V + 100},
	{I = 10*DV}.
diode(in914, c(I,0), c(V, 0)) :-
	{V >= -100},
	{V < 0.6},
	{I = 0.001*V}.
diode(in914, c(I,0), c(V, 0)) :-
	{V >= 0.6},
	{DV = V - 0.6},
	{I = 100*DV}.

addcomp(W, [transistor, Num, X, [N1, N2, N3]], NV, NV3, [transistor, Num, X, [N1, V1, I1], [N2, V2, I2], [N3, V3, I3]]):-  
	transistor(X, R, Gain),
	c_add(I1, I3, IT),
	c_neg(I2, IT),
	c_add(Vin, V2, V1),
	c_mult(I1, c(R, 0), Vin),
	c_mult(I1, c(Gain, 0), I3),
	subst([N1, V1, Iold1], [N1, V1, Inew1], NV, NV1),
	subst([N2, V2, Iold2], [N2, V2, Inew2], NV1, NV2),
	subst([N3, V3, Iold3], [N3, V3, Inew3], NV2, NV3),
	subst([N4, V4, Iold4], [N4, V4, Inew4], NV3, NV4),
	c_add(I1,Iold1,Inew1),
	c_add(I2,Iold2,Inew2),
	c_add(I3,Iold3,Inew3),
	c_add(I4,Iold4,Inew4).

transistor(bc108, 1000, 100).

addcomp(W, [transformer, Num, X, [N1, N2, N3, N4]], NV, NV4, [transformer, Num, X, [N1, V1, I1], [N2, V2, I2], [N3, V3, I3], [N4, V4, I4]]):-  
	c_neg(I1, I2),
	c_neg(I3, I4),
	c_add(Vin, V2, V1),
	c_add(Vout, V4, V3),
	c_mult(Vout, c(X, 0), Vin),
	c_mult(I1, c(X, 0), I4),
	subst([N1, V1, Iold1], [N1, V1, Inew1], NV, NV1),
	subst([N2, V2, Iold2], [N2, V2, Inew2], NV1, NV2),
	subst([N3, V3, Iold3], [N3, V3, Inew3], NV2, NV3),
	subst([N4, V4, Iold4], [N4, V4, Inew4], NV3, NV4),
	c_add(I1,Iold1,Inew1),
	c_add(I2,Iold2,Inew2),
	c_add(I3,Iold3,Inew3),
	c_add(I4,Iold4,Inew4).


subst(X, Y, [X|L1], [Y|L1]) :- !.
subst(X, Y, [Z|L1], [Z|L2]) :- 
	subst(X,Y,L1,L2).
subst(X, Y, [], L2) :- 
	printf("Node list incomplete\n", []).


% ***** COMPLEX NUMBER ARITHMETIC *****

c_mult(c(Re1,Im1),c(Re2,Im2),c(Re3,Im3)) :- 
	{Re3 = Re1*Re2 - Im1*Im2},
	{Im3 = Re1*Im2 + Re2*Im1}.

c_add(c(Re1,Im1),c(Re2,Im2),c(Re3,Im3)) :- 
	{Re3 = Re1 + Re2},
	{Im3 = Im1 + Im2}.

c_neg(c(Re,Im),c(Re1,Im1)) :-  
	{Re1 = -Re, Im1 = -Im }.

c_eq(c(Re1,Im1),c(Re2,Im2)) :- 
	{Re1 = Re2, Im1 = Im2 }.

c_real(c(Re,Im),Re).

c_imag(c(Re,Im),Im).


% ****** PRINTOUT ROUTINES ******

format_print(H, []) :- 
	!, 
	all_print(H).  
	% If no selection is given then
	% print out all nodes.
format_print(H, Selection) :-
	selective_print(H, Selection).
	% Otherwise print the selection.

selective_print(Ls, [N|Ns]) :-
	print_nodes(Ls, N, 0),
	print_comps(Ls, N),
	selective_print(Ls, Ns).
selective_print(Ls, []).

print_nodes([[Comp, Num, X|Nodes]|L], N1, Heading_flag_1) :-
	rlc_member(N1, Nodes),
	{Heading_flag_2 = Heading_flag_1 + 1},
	heading(N1, Heading_flag_2),
	all_print([[Comp, Num, X|Nodes]]),
	print_nodes(L, N1, Heading_flag_2).
print_nodes([[Comp, Num, X|Nodes]|L], N1, Heading_flag) :-
	print_nodes(L, N1, Heading_flag).
print_nodes([], N1, Heading_flag).

print_comps([[Comp, Num, X|Nodes]|L], Num) :- 
	all_print([[Comp, Num, X|Nodes]]),
	print_comps(L, Num).
print_comps([[Comp, Num, X|Nodes]|L], Num1) :-
	print_comps(L, Num1).
print_comps([], Num).

heading(N, 1) :-
	!, 
	printf("\nCOMPONENT CONNECTIONS TO NODE %\n", [N]).
heading(N, X).

rlc_member(N1, [[N1, V, I]|Ls]).
rlc_member(N1, [[N2, V, I]|Ls]) :-
	rlc_member(N1, Ls).

write_units(resistor, X) :-
	printf("% Ohms",[X]).
write_units(capacitor, X) :-
	printf("% Farads",[X]).
write_units(inductor, X) :-
	printf("% Henrys", [X]).
write_units(current_source, X) :-
	printf("% Ampere", [X]).
write_units(voltage_source, X) :-
	printf("% Volts", [X]).
write_units(diode, X) :-
	printf("type %", [X]).
write_units(transistor, X) :-
	printf("type %    (base, emitter, collector)", [X]).
write_units(transformer, X) :-
	printf("ratio of %", [X]).

all_print([[Comp, Num, X|Nodes]|L]) :-
	printf("% %: ", [Comp, Num]),
	write_units(Comp, X),
	printf("\n", []),
	pr_nodes(Nodes),
	all_print(L).
all_print([]).

pr_nodes([[N1, V1, I1]|X]) :-
        printf("        Node %\n", [N1]),
        printf("                Voltage %\n", [V1]),
        printf("                Current %\n", [I1]),
        pr_nodes(X).
pr_nodes([]) :-
	printf("\n", []).


go1:-
	W = 10,
	Vs = 10,
	R = 10,
	L = 0.9,
	C = 0.007,
	circuit_solve(W,
		      [ 
			[voltage_source,v1,c(Vs,0),[n1,n7]],
			[resistor,r1,R,[n1,n2]],
			[resistor,r2,R,[n1,n3]],
			[resistor,r3,R,[n1,n4]],
			[resistor,r4,R,[n1,n5]],
			[resistor,r5,R,[n1,n6]],
			[inductor,l1,L,[n2,n7]],
			[inductor,l2,L,[n3,n7]],
			[inductor,l3,L,[n4,n7]],
			[inductor,l4,L,[n5,n7]],
			[inductor,l5,L,[n6,n7]],
			[capacitor,c1,C,[n2,n3]],
			[capacitor,c2,C,[n2,n4]],
			[capacitor,c3,C,[n2,n5]],
			[capacitor,c4,C,[n2,n6]],
			[capacitor,c5,C,[n3,n4]],
			[capacitor,c6,C,[n3,n5]],
			[capacitor,c7,C,[n3,n6]],
			[capacitor,c8,C,[n4,n5]],
			[capacitor,c9,C,[n4,n6]],
			[capacitor,c10,C,[n5,n6]]
		      ],
		      [n7],
		      [r1, r3, l1, l3, c8, l5]
		     ).

% Output:
%  resistor r1: 10 Ohms
%          Node n1
%                  Voltage c(10, 0)
%                  Current c(0.552486, -0.497238)
%          Node n2
%                  Voltage c(4.475138, 4.972376)
%                  Current c(-0.552486, 0.497238)
%  
%  resistor r3: 10 Ohms
%          Node n1
%                  Voltage c(10, 0)
%                  Current c(0.552486, -0.497238)
%          Node n4
%                  Voltage c(4.475138, 4.972376)
%                  Current c(-0.552486, 0.497238)
%  
%  inductor l1: 0.9 Henrys
%          Node n2
%                  Voltage c(4.475138, 4.972376)
%                  Current c(0.552486, -0.497238)
%          Node n7
%                  Voltage c(0, 0)
%                  Current c(-0.552486, 0.497238)
%  
%  inductor l3: 0.9 Henrys
%          Node n4
%                  Voltage c(4.475138, 4.972376)
%                  Current c(0.552486, -0.497238)
%          Node n7
%                  Voltage c(0, 0)
%                  Current c(-0.552486, 0.497238)
%  
%  capacitor c8: 0.007 Farads
%          Node n4
%                  Voltage c(4.475138, 4.972376)
%                  Current c(0, 0)
%          Node n5
%                  Voltage c(4.475138, 4.972376)
%                  Current c(0, 0)
%  
%  inductor l5: 0.9 Henrys
%          Node n6
%                  Voltage c(4.475138, 4.972376)
%                  Current c(0.552486, -0.497238)
%          Node n7
%                  Voltage c(0, 0)
%                  Current c(-0.552486, 0.497238)
%  


go2:-
	Vs = 10,
	Tr1 = 5,
	Tr2 = 0.2,
	R1 = 1000,
	R2 = 200,
	R3 = 50,
	R4 = 30,
	circuit_solve(W,
		      [ 
			[voltage_source, v1, c(Vs,0),[in, ground1]],
			[resistor, r1, R2, [in, n1]],
			[transformer, t1, Tr1,[n1, ground1, n2, ground2]],
			[resistor, r2, R1, [n2, base]],
			[transistor, q1, bc108, [base, n3, n4]],
			[resistor, r3, R2, [n3, ground2]],
			[resistor, r3, R2, [n4, ground2]],
			[transformer, t2, Tr2,[n4, ground2, out, ground3]],
			[resistor, r5, R4, [out, ground3]]
		      ],
		      [ground1, ground2, ground3],
		      [n4, out]).

% Output:
%  COMPONENT CONNECTIONS TO NODE n4
%  transistor q1: type bc108    (base, emitter, collector)
%          Node base
%                  Voltage c(1.909222, 0)
%                  Current c(9.005764e-005, 0)
%          Node n3
%                  Voltage c(1.819164, 0)
%                  Current c(-0.009096, 0)
%          Node n4
%                  Voltage c(-0.010742, 0)
%                  Current c(0.009006, 0)
%  
%  resistor r3: 200 Ohms
%          Node n4
%                  Voltage c(-0.010742, 0)
%                  Current c(-5.371231e-005, 0)
%          Node ground2
%                  Voltage c(0, 0)
%                  Current c(5.371231e-005, 0)
%  
%  transformer t2: ratio of 0.2
%          Node n4
%                  Voltage c(-0.010742, 0)
%                  Current c(-0.008952, 0)
%          Node ground2
%                  Voltage c(0, 0)
%                  Current c(0.008952, 0)
%          Node out
%                  Voltage c(-0.053712, 0)
%                  Current c(0.00179, 0)
%          Node ground3
%                  Voltage c(0, 0)
%                  Current c(-0.00179, 0)
%  
%  
%  COMPONENT CONNECTIONS TO NODE out
%  transformer t2: ratio of 0.2
%          Node n4
%                  Voltage c(-0.010742, 0)
%                  Current c(-0.008952, 0)
%          Node ground2
%                  Voltage c(0, 0)
%                  Current c(0.008952, 0)
%          Node out
%                  Voltage c(-0.053712, 0)
%                  Current c(0.00179, 0)
%          Node ground3
%                  Voltage c(0, 0)
%                  Current c(-0.00179, 0)
%  
%  resistor r5: 30 Ohms
%          Node out
%                  Voltage c(-0.053712, 0)
%                  Current c(-0.00179, 0)
%          Node ground3
%                  Voltage c(0, 0)
%                  Current c(0.00179, 0)
%  

?- printf("\n>>> Sample goals: go1/0, go2/0\n", []).
