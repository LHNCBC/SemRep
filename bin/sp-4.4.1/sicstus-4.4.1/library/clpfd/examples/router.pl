/*********************************************************************
------------------------------------------------------------
%   File   : route.pl
%   Author : Neng-Fa ZHOU
%   Adapted by: Mats Carlsson
%   Date   : 1995 
%   Modified in November 1996
%   Purpose: A multi-layer channel router in CLP(FD)
------------------------------------------------------------
Problem Description:
VLSI layout design consists of two phases: the first phase, called
placement, determines the positions of the modules on the VLSI chip,
and the second phase, called routing, connects the modules with
wiring. Channel routing is a kind of routing where the routing area is
restricted to a rectangular channel. A channel consists of two
parallel rows with terminals on them. A connection requirement, called
a net, specifies the terminals that must be interconnected through a
routing path. A channel routing problem is to find routing paths for a
given set of nets in a given channel such that no paths overlap each
other.

Acknowledgement:
Part of the work was supported by AITEC.

References:
Simonis, H., Channel Routing Seen as a Constraint Problem, Tech. Rep., 
TR-LP-51, ECRC, Munich, July, 1990. 

Huitoze, S.L. and Dechier D., Channel Routing with clp(FD), Proc. of 
2nd PACT, 1996.

Zhou, N.F., A Logic Programming Approach to Channel Routing, Proc. 
12th ICLP, Kanazawa, MIT Press, 159-173, 1995.

Zhou, N.F., Channel Routing with Constraint Logic Programming and Delay,
Proc. 9th IEA-AIE, Fukuoka, Gordon and Breach Pub. 383-388, 1996.
*********************************************************************/
:- module(router, [route/6]).

:- use_module(library(clpfd)).

% Unused, for testing(?)
:- public
           go1/0,
           go2/0,
           go3/0,
           go4/0.

go1:- % one horizontal layer channel 
    N=72, L=1, T=28, C=174, Order=1534,		
    route(_Lab,N,L,T,C,Order).

go2:- % two horizontal layer channel 
    N=72, L=2, T=10, C=174, Order=1534,		
    route(_Lab,N,L,T,C,Order).

go3:- % three horizontal layer channel 
    N=72, L=3, T=7, C=174, Order=1534,		
    route(_Lab,N,L,T,C,Order).

go4:- % four horizontal layer channel 
    N=72, L=4, T=5, C=174, Order=1534,		
    route(_Lab,N,L,T,C,Order).

route(Lab,N,L,T,C,Order):-
    route(Lab,N,L,T,C,Order,Vector,Time),
    format('~d layers took ~d msec\n', [L,Time]),
    output(N,L,T,C,Vector),
    true.

route(Lab,N,L,T,_C,Order,Vector,Time):-
    functor(Vector,dvars,N),  % generate variables, N<256 in some systems
    Vector=..[dvars|Vars],
    build_domain_vars(1,N,Vector,TVars), % TVars-- variables for tracks
	init_domains(0, L, T, Hole, Dom\/Hole),
	(   foreach(X,TVars),
	    param(Dom)
	do  X in Dom
	),
    functor(G,graphs,N),      % generate constraint graphs Gv and Gh
    build_constraint_graphs(N,Vector,G),
    compute_depth(N,Vector,G),
    generate_constraints(1,N,Vector,G,T), % generate constraints from Gv and Gh
    order_vars(Vars,Vars1,Order,G), % order variables based on Gv and Gh beforehand
    statistics(runtime,[Start|_]),
    route(Lab,Vars1),
    statistics(runtime,[End|_]),
    Time is End-Start.

init_domains(L, L, _, Dom, Dom) :- !.
init_domains(I, L, T, Dom0, Dom) :-
	J is I+1,
	B is (I+I+1)*T,
	A is B-T+1,
	init_domains(J, L, T, Dom0\/(A..B), Dom).

/*****************************************************************
Each net is represented as a term:

    net(N0,Head,Tail,Terminals,Layer,Track,Depth)

where N0 is the number of the net, Head is the left-most terminal 
Number, Tail is the right-most terminal Number, Terminals is a set 
of terminals in the net, Layer is a variable which will hold the 
layer for the net, and Track is a domain variable, Depth is the 
depth of the net in the vertical constraint graph.
*****************************************************************/
build_domain_vars(N0,N,_Vector,TVars):-
    N0>N,!,TVars=[].
build_domain_vars(N0,N,Vector,TVars):-
    net(N0,Terminals),
    net_head(Terminals,Head),
    net_tail(Terminals,Tail),
    Var=net(N0,Head,Tail,Terminals,_Layer,Track,_Depth),
    arg(N0,Vector,Var),
    TVars=[Track|TVars1],
    N1 is N0+1,
    build_domain_vars(N1,N,Vector,TVars1).

/********************************************************************
The constraint graphs Gv and Gh are represented as a functor whose I'th
argument has the form:

    node(I,Tag,As,Bs,Hs)

where As is the list of nets lying directly above I and Bs is the list
of nets lying directly below I in Gv, Hs is a list of nets connected to
I in Gh, and Tag is used to indicate whether the net has been removed 
from the graphs.
*********************************************************************/
build_constraint_graphs(N,Vector,G):-
    build_constraint_graphs(1,N,Vector,G).

build_constraint_graphs(N0,N,_Vector,_G):-
    N0>N,!.
build_constraint_graphs(N0,N,Vector,G):-
    arg(N0,Vector,Net0),
    Node=node(N0,_Tag,As,Bs,Hs),
    compute_As_Bs_Hs(N0,Net0,1,N,Vector,G,[],As,[],Bs,[],Hs),
    arg(N0,G,Node),
    N1 is N0+1,
    build_constraint_graphs(N1,N,Vector,G).

compute_As_Bs_Hs(_N,_Net,From,To,_Vector,_G,As0,As,Bs0,Bs,Hs0,Hs):-
    From>To,!,
    As=As0, Bs=Bs0, Hs=Hs0.
compute_As_Bs_Hs(N,Net,From,To,Vector,G,As0,As,Bs0,Bs,Hs0,Hs):-
    From=:=N,!,
    From1 is From+1,
    compute_As_Bs_Hs(N,Net,From1,To,Vector,G,As0,As,Bs0,Bs,Hs0,Hs).
compute_As_Bs_Hs(N,Net,From,To,Vector,G,As0,As,Bs0,Bs,Hs0,Hs):-
    arg(From,Vector,Net0),
    arg(From,G,Node),
    (above(Net0,Net)->As1=[Node|As0];As1=As0),
    (above(Net,Net0)->Bs1=[Node|Bs0];Bs1=Bs0),
    head_field(Net,H),
    tail_field(Net,T),
    head_field(Net0,H0),
    tail_field(Net0,T0),
    ((H>=H0,H=<T0; H0>=H,H0=<T)->Hs1=[Node|Hs0];Hs1=Hs0),
    From1 is From+1,
    compute_As_Bs_Hs(N,Net,From1,To,Vector,G,As1,As,Bs1,Bs,Hs1,Hs).

compute_depth(N,Vector,G):-
    top_nodes(1,N,G,Vector,TopNodes),
    compute_depth_loop(TopNodes,Vector).

top_nodes(N0,N,_G,_Vector,TopNodes):-
    N0>N,!,
    TopNodes=[].
top_nodes(N0,N,G,Vector,TopNodes):-
    arg(N0,G,Node),
    Node = node(_,_,[],_Bs,_Hs),!, %As ==[]
    N1 is N0+1,
    arg(N0,Vector,Net),
    depth_field(Net,0),
    TopNodes=[Node|TopNodes1],
    N1 is N0+1,
    top_nodes(N1,N,G,Vector,TopNodes1).
top_nodes(N0,N,G,Vector,TopNodes):-
    N1 is N0+1,
    top_nodes(N1,N,G,Vector,TopNodes).

compute_depth_loop(TopNodes,Vector):-
    compute_depth_top_down(TopNodes,Vector,Cont),
    (var(Cont),!;
     compute_depth_loop(TopNodes,Vector)).

compute_depth_top_down([],_Vector,_Cont).
compute_depth_top_down([node(N,_,_As,Bs,_Hs)|Nodes],Vector,Cont):-
    arg(N,Vector,Net),
    depth_field(Net,Depth),
    nonvar(Depth),!,
    compute_depth_top_down(Bs,Vector,Cont),
    compute_depth_top_down(Nodes,Vector,Cont).
compute_depth_top_down([node(N,_,As,Bs,_Hs)|Nodes],Vector,Cont):-
    maximum_depth(As,0,Depth,Vector),!,
    Depth1 is Depth+1,
    arg(N,Vector,Net),
    depth_field(Net,Depth1),
    Cont=1,
    compute_depth_top_down(Bs,Vector,Cont),
    compute_depth_top_down(Nodes,Vector,Cont).
compute_depth_top_down([_Node|Nodes],Vector,Cont):-
    compute_depth_top_down(Nodes,Vector,Cont).

maximum_depth([],Depth0,Depth,_Vector):-
    Depth=Depth0.
maximum_depth([node(A,_,_,_,_)|As],Depth0,Depth,Vector):-
    arg(A,Vector,Net),
    depth_field(Net,D),
    nonvar(D),!,
    (D>Depth0->Depth1=D; Depth1=Depth0),
    maximum_depth(As,Depth1,Depth,Vector).

%%%%%%%%%%%%%%%%%%%%%%% generate constraints %%%%%%%%%%%%%%%%%%%%%%% 
generate_constraints(N0,N,_Vector,_G,_MaxT):-
    N0>N,!.
generate_constraints(N0,N,Vector,G,MaxT):-
    arg(N0,G,node(_,_,_As,Bs,Hs)),
    arg(N0,Vector,Net0),
    track_field(Net0,Track0),
    vertical_constraints(Track0,Bs,Vector,MaxT),
    horizontal_constraints(Track0,Hs,Vector),
    N1 is N0+1,
    generate_constraints(N1,N,Vector,G,MaxT).

vertical_constraints(_Track0,[],_Vector,_MaxT).
vertical_constraints(Track0,[node(N,_,_,_,_)|Ns],Vector,MaxT):-
    arg(N,Vector,Net),
    track_field(Net,Track),
    my_above(Track0, Track, MaxT),
    vertical_constraints(Track0,Ns,Vector,MaxT).

horizontal_constraints(_Track0,[],_Vector).
horizontal_constraints(Track0,[node(N,_,_,_,_)|Ns],Vector):-
    arg(N,Vector,Net),
    track_field(Net,Track),
    Track0#\=Track,
    horizontal_constraints(Track0,Ns,Vector).

my_above(Track0, Track, MaxT) :-
	NT is -MaxT,
	D #= Track0-Track,	% interval consistent
	D in (inf..NT)\/(1..sup).


%%%%%%%%%%%%%%%%% Other library routines %%%%%%%%%%%%%%%%%%%%%%%
above(Var1,Var2):-
    terms_field(Var1,Terms1),
    terms_field(Var2,Terms2),
    above2(Terms1,Terms2).

above2([t(K)|_],[b(K)|_]):-!.
above2(Ts1,Ts2):-
    [T1|Ts3]=Ts1,
    [T2|Ts4]=Ts2,
    arg(1,T1,C1),
    arg(1,T2,C2),
    (C1<C2 -> above2(Ts3,Ts2); above2(Ts1,Ts4)).

net_head([H|_],H1):-
    arg(1,H,H1).

net_tail([Tail],Tail1):-!,
    arg(1,Tail,Tail1).
net_tail([_|Tail],Tail1):-
    net_tail(Tail,Tail1).

head_field(Var,Head):-
    arg(2,Var,Head).

tail_field(Var,Tail):-
    arg(3,Var,Tail).

terms_field(Var,Terms):-
    arg(4,Var,Terms).

% [PM] 4.3.1 Unused. Avoid unused-predicate warning.
:- public layer_field/2.

layer_field(Var,Layer):-
    arg(5,Var,Layer).

track_field(Var,Track):-
    arg(6,Var,Track).

depth_field(Var,Depth):-
    arg(7,Var,Depth).

% [PM] 4.3.1 Unused. Avoid unused-predicate warning.
:- public height_field/2.

height_field(Var,Depth):-
    arg(8,Var,Depth).

%%%%%%%%%%%%%%%%%%%%%%% enumerate values %%%%%%%%%%%%%%%%%%%%%%% 
route(Lab,Nets) :-
	(   foreach(net(_N,_Head,_Tail,_Terms,_Layer,Track,_Depth),Nets),
	    foreach(Track,Vars)
	do  true
	),
	labeling(Lab, Vars).

%%%%%%%%%%%%%%%%%%%%%% Ordering variables %%%%%%%%%%%%%%%%%%%%%%%%%
order_vars([],OrderedVars,_Order,_G):-
    OrderedVars=[].
order_vars(Vars,OrderedVars,Order,G):-
    OrderedVars=[BestVar|OrderedVars1],
    choose(Vars,Order,BestVar,Rest,G),
    arg(1,BestVar,Net),
    remove_net(Net,G),
    order_vars(Rest,OrderedVars1,Order,G).

choose([Var|Vars],Order,BestVar,Rest,G):-
    evaluate_dvar(Order,Var,EValue,G),
    choose(Vars,Order,Var,EValue,BestVar,Rest,G).

choose([],_Order,Var,_EValue,BestVar,Rest,_G):-
    BestVar=Var,
    Rest=[].
choose([Var|Vars],Order,Var1,EValue1,BestVar,Rest,G):-
    evaluate_dvar(Order,Var,EValue,G),
    compare_dvar(Var1,EValue1,Var,EValue,GoodVar,GoodEValue,BadVar),
    Rest=[BadVar|Rest1],
    choose(Vars,Order,GoodVar,GoodEValue,BestVar,Rest1,G).

compare_dvar(Var1,[],Var2,_EValue2,GoodVar,GoodEValue,BadVar):-
    GoodVar=Var1,
    GoodEValue=[],
    BadVar=Var2.
compare_dvar(Var1,[X|EValue1],Var2,[Y|_EValue2],GoodVar,GoodEValue,BadVar):-
    X>Y,!,
    GoodVar=Var1,
    GoodEValue=[X|EValue1],
    BadVar=Var2.
compare_dvar(Var1,[X|_EValue1],Var2,[Y|EValue2],GoodVar,GoodEValue,BadVar):-
    X<Y,!,
    GoodVar=Var2,
%   a bug found by Serge Le Huitouze
%    GoodEValue=[Y|EValue1], 
    GoodEValue=[Y|EValue2], 
    BadVar=Var1.
compare_dvar(Var1,[X|EValue1],Var2,[_Y|EValue2],GoodVar,GoodEValue,BadVar):-
    GoodEValue=[X|GoodEValue1],
    compare_dvar(Var1,EValue1,Var2,EValue2,GoodVar,GoodEValue1,BadVar).

/************************************************************* 
1: select first open nets, i.e., nets at the bottom of Gv
2: select those nets with the smallest domains, not used
3: select first those nets with the greatest degree in Gv 
4: select first those nets with the greatest degree in Gh
5: select first those nets lie deep in Gv
****************************************************************/
evaluate_dvar(134,Var,EValue,G):-
    arg(1,Var,N),
    is_open(N,Open,G),
    gv_degree(N,G,Degree1),
    gh_degree(N,G,Degree2),
    EValue=[Open,Degree1,Degree2].
evaluate_dvar(15,Var,EValue,G):-
    arg(1,Var,N),
    is_open(N,Open,G),
    depth_field(Var,Depth),
    EValue=[Open,Depth].
evaluate_dvar(1534,Var,EValue,G):-
    arg(1,Var,N),
    is_open(N,Open,G),
    depth_field(Var,Depth),
    gv_degree(N,G,Degree1),
    gh_degree(N,G,Degree2),
    EValue=[Open,Depth,Degree1,Degree2].

is_open(N,Open,G):-
    arg(N,G,node(_,_Tag,_As,Bs,_Hs)),
    empty_nodes(Bs),!,
    Open=1.
is_open(_N,Open,_G):-
    Open=0.

gv_degree(N,G,Degree):-
    arg(N,G,node(_,_,__,Bs,_)),
    degree(Bs,0,Degree).

gh_degree(N,G,Degree):-
    arg(N,G,node(_,_,_,_,Hs)),
    degree(Hs,0,Degree).

empty_nodes([]).
empty_nodes([node(_,Tag,_,_,_)|_]):-
    var(Tag),!,fail.
empty_nodes([_|Nodes]):-
    empty_nodes(Nodes).

degree([],N0,N):-N=N0.
degree([node(_,Tag,_,_,_)|Nodes],N0,N):-
    var(Tag),!,
    N1 is N0+1,
    degree(Nodes,N1,N).
degree([_|Nodes],N0,N):-
    degree(Nodes,N0,N).


%%%%%%%%%%%%%%%%%%%% Remove a net %%%%%%%%%%%%%%%
remove_net(N,G):-
    arg(N,G,Node),
    Node=node(_,1,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%% output %%%%%%%%%%%%%%%%%%%%%%% 
output(N,L,T,C,Vector):-
    functor(Vector1,dvars,N),
    decode_layer_track(N,Vector,Vector1,L,T),
    output(L,T,C,Vector1).

output(_,_,_,Vec) :- functor(Vec,_,N), output(0,N,Vec).

output(N,N,_) :- !.
output(I,N,Vec) :-
	J is I+1,
	arg(J,Vec,Net),
	portray_clause(Net),
	output(J,N,Vec).

decode_layer_track(0,_Vector,_Vector1,_MaxLayer,_MaxTrack):- !.
decode_layer_track(N,Vector,Vector1,MaxLayer,MaxTrack):-
    arg(N,Vector,Var),
    Var=net(N0,Head,Tail,Terminals,_Layer,TrackValue,Depth),
    NewVar=net(N0,Head,Tail,Terminals,NewLayer,NewTrack,Depth),
	TrackValue0 is TrackValue-1,
    NewLayer is TrackValue0//(2*MaxTrack) + 1,
    NewTrack is TrackValue0 mod MaxTrack + 1,
    arg(N,Vector1,NewVar),
    N1 is N-1,
    decode_layer_track(N1,Vector,Vector1,MaxLayer,MaxTrack).

/*** Zhou's code to generate LaTeX
output(MaxLayer,MaxTrack,MaxTerm,Dvars):-
    X is MaxTerm+1,
    Y is MaxLayer*(MaxTrack+3)+1,
    write_line(['\documentstyle[11pt,epsf]{article}']),
    write_line(['\topmargin=-0.5cm']),
    write_line(['\oddsidemargin=-1.5cm']),
    write_line(['\textheight=23cm \textwidth=30cm']),
    write_line(['\begin{document}']),
    write_line(['\begin{figure}[hbt]']),
    write_line(['\setlength{\unitlength}{1.5mm}']),
    write_line(['\begin','{',picture,'}','(',X,',',Y,')','(',0,',',0,')']),
    output_rows(MaxLayer,MaxTrack,MaxTerm),
    functor(Dvars,F,N),
    output_nets(MaxTrack,Dvars,1,N),
    write_line(['\end{picture}']),
    write_line(['\end{figure}']),
    write_line(['\end{document}']).

output_rows(Layer,MaxTrack,MaxTerm):-
    Layer=<0,!.
output_rows(Layer,MaxTrack,MaxTerm):-
    Top is Layer*(MaxTrack+3),
    Bottom is (Layer-1)*(MaxTrack+3)+2,
    Len is MaxTerm+1,
    write_line(['\put','(',0,',',Top,')','{\line(1,0){',Len,'}}']),
    write_line(['\multiput(1,',Top,')(1,0){',MaxTerm,'}{\circle*{.2}}']),
    write_line(['\put','(',0,',',Bottom,')','{\line(1,0){',Len,'}}']),
    write_line(['\multiput(1,',Bottom,')(1,0){',MaxTerm,'}{\circle*{.2}}']),
    Layer1 is Layer-1,
    output_rows(Layer1,MaxTrack,MaxTerm).

output_nets(MaxTrack,Dvars,N0,N):-
    N0>N,!.
output_nets(MaxTrack,Dvars,N0,N):-
    arg(N0,Dvars,Dvar),
    Dvar=net(N0,Head,Tail,Terms,Layer,Track,Depth),
    Y is (Layer-1)*(MaxTrack+3)+2+Track,
    Length is Tail-Head,
    write_line(['\put(',Head,',',Y,'){\line(1,0){',Length,'}}']),
    Top is Layer*(MaxTrack+3),
    Bottom is (Layer-1)*(MaxTrack+3)+2,
    Top_len is MaxTrack-Track+1,
    output_terminals(Top_len,Track,Y,Terms),
    N1 is N0+1,
    output_nets(MaxTrack,Dvars,N1,N).

output_terminals(Top_len,Bottom_len,Y,[]):-!.
output_terminals(Top_len,Bottom_len,Y,[t(X)|Terminals]):-
    (X=:=0->true;
    write_line(['\put(',X,',',Y,'){\line(0,1){',Top_len,'}}']),
    write_line(['\put(',X,',',Y,'){\circle*{0.2}}'])),
    output_terminals(Top_len,Bottom_len,Y,Terminals).
output_terminals(Top_len,Bottom_len,Y,[b(X)|Terminals]):-
    (X=:=0->true;
     write_line(['\put(',X,',',Y,'){\line(0,-1){',Bottom_len,'}}']),
     write_line(['\put(',X,',',Y,'){\circle*{0.2}}'])),
    output_terminals(Top_len,Bottom_len,Y,Terminals).

write_line([]):-
    nl.
write_line([X|L]):-
    write(X),
    write_line(L).
***/

%%%%%%%%%%%%%% Nets in Deutsch's difficult problem %%%%%%%%%%%%%%%%%%%%%
net(1,N):-N=[t(5),t(28)].
net(2,N):-N=[t(39),t(67)].
net(3,N):-N=[t(74),t(117)].
net(4,N):-N=[b(145),t(151)].
net(5,N):-N=[t(161),t(163)].
net(6,N):-N=[b(62),t(77)].
net(7,N):-N=[t(78),t(82)].
net(8,N):-N=[b(90),t(110),b(118),t(123)].
net(9,N):-N=[t(139),t(141),t(144),b(151),t(174)].
net(10,N):-N=[t(106),t(130),t(132),b(161),t(168)].
net(11,N):-N=[t(70),t(98),t(100)].
net(12,N):-N=[t(109),t(131),t(135),b(141),t(153),t(155),t(171)].
net(13,N):-N=[t(24),b(37),t(53),b(55),t(60),t(92),b(110)].
net(14,N):-N=[b(117),t(166)].
net(15,N):-N=[t(12),t(19)].
net(16,N):-N=[t(22),b(39),t(51),t(58),t(94),b(97),b(106),b(108),b(135),b(144),b(155),b(166)].
net(17,N):-N=[t(6),t(13),b(22),t(30),t(34),t(36),t(40)].
net(18,N):-N=[b(78),t(147),t(149)].
net(19,N):-N=[t(159),b(165)].
net(20,N):-N=[t(0),t(21),b(40),t(48),t(50),t(57),t(95)].
net(21,N):-N=[b(98),t(119)].
net(22,N):-N=[t(120),t(154),t(156)].
net(23,N):-N=[t(2),b(13)].
net(24,N):-N=[t(20),b(57),t(68),t(76),t(111),b(119),t(122)].
net(25,N):-N=[t(128),b(149),b(160),t(167)].
net(26,N):-N=[b(2),b(5),t(11),t(14),t(46),t(49)].
net(27,N):-N=[t(66),b(70)].
net(28,N):-N=[b(95),t(105),b(113),t(124),b(128)].
net(29,N):-N=[t(138),t(140)].
net(30,N):-N=[t(7),b(14)].
net(31,N):-N=[b(7),b(11),t(15),t(16),b(19)].
net(32,N):-N=[t(23),b(24)].
net(33,N):-N=[b(66),b(68),t(83),b(92),t(99),t(101),b(102)].
net(34,N):-N=[t(3),b(16),b(21),b(32),b(58),t(69),t(75),b(77),t(112),b(120),t(121)].
net(35,N):-N=[b(124),t(129)].
net(36,N):-N=[t(134),b(140),b(150),t(162),t(164),t(173)].
net(37,N):-N=[t(73),b(75)].
net(38,N):-N=[t(87),b(94),b(101),t(114),t(116)].
net(39,N):-N=[t(136),b(154)].
net(40,N):-N=[t(44),b(60),t(65),b(73),t(79),t(104),b(112),t(125),b(129)].
net(41,N):-N=[b(79),t(93)].
net(42,N):-N=[b(114),t(133)].
net(43,N):-N=[b(134),t(158)].
net(44,N):-N=[b(65),b(74)].
net(45,N):-N=[t(84),t(86),b(93),t(146),t(148)].
net(46,N):-N=[t(25),b(36),t(54),t(61),t(91),b(99),b(104),b(133),b(142),b(146),b(153),b(164)].
net(47,N):-N=[t(52),b(54)].
net(48,N):-N=[t(1),b(50),b(52)].
net(49,N):-N=[b(1),t(8),t(29),t(41),b(44),b(46),t(63)].
net(50,N):-N=[t(33),t(35)].
net(51,N):-N=[t(38),t(45),b(61),t(71),b(86)].
net(52,N):-N=[t(127),t(143),b(159)].
net(53,N):-N=[t(10),t(27),b(29),t(43)].
net(54,N):-N=[t(47),b(67),b(71),t(81),b(82),b(84),t(89),b(91)].
net(55,N):-N=[b(127),t(172)].
net(56,N):-N=[b(6),b(10),t(18)].
net(57,N):-N=[t(31),b(38)].
net(58,N):-N=[b(41),t(59)].
net(59,N):-N=[b(63),b(69),t(72),b(87)].
net(60,N):-N=[t(88),b(89)].
net(61,N):-N=[t(96),b(105)].
net(62,N):-N=[t(4),b(15),b(20),b(31),b(59),t(64),b(72),t(80),t(103),b(111),t(126),b(130)].
net(63,N):-N=[b(138),b(168),t(170)].
net(64,N):-N=[b(4),t(9),t(42),b(49),b(51),t(56)].
net(65,N):-N=[b(64),t(85),b(88)].
net(66,N):-N=[b(158),t(169)].
net(67,N):-N=[b(3),b(9),b(12),t(17)].
net(68,N):-N=[b(23),b(43),b(45)].
net(69,N):-N=[b(56),b(81),b(83)].
net(70,N):-N=[b(96),b(107),b(109),b(136),b(143),b(156),b(167)].
net(71,N):-N=[b(8),b(17)].
net(72,N):-N=[b(18),b(26),b(28)].

end_of_file.

1 layers took 110 msec
Tells detecting entailment: 2043
Tells pruning: 2907
Tells failing: 0
Total tells: 7029
Asks detecting entailment: 0
Total asks: 0
Constraints created: 2043

2 layers took 180 msec
Tells detecting entailment: 2043
Tells pruning: 2532
Tells failing: 0
Total tells: 5568
Asks detecting entailment: 0
Total asks: 0
Constraints created: 2043

3 layers took 180 msec
Tells detecting entailment: 2043
Tells pruning: 2464
Tells failing: 0
Total tells: 5442
Asks detecting entailment: 0
Total asks: 0
Constraints created: 2043

?- labeling([],Vars).
4 layers took 18840 msec
Tells detecting entailment: 131239
Tells pruning: 111900
Tells failing: 9967
Total tells: 205742
Asks detecting entailment: 0
Total asks: 0
Constraints created: 2043

?- labeling([ff],Vars).
4 layers took 190 msec
Tells detecting entailment: 2043
Tells pruning: 2293
Tells failing: 0
Total tells: 4539
Asks detecting entailment: 0
Total asks: 0
Constraints created: 2043

?- labeling([ffc],Vars).
4 layers took 220 msec
Tells detecting entailment: 2043
Tells pruning: 2281
Tells failing: 0
Total tells: 4573
Asks detecting entailment: 0
Total asks: 0
Constraints created: 2043

| ?- main(72, 2, 10, 174, 1534). % [ffc]
2 layers took 220 msec
Tells detecting entailment: 2043
Tells pruning: 2332
Tells failing: 0
Total tells: 4724
Asks detecting entailment: 0
Total asks: 0
Constraints created: 2043

| ?- main(72, 2, 10, 174, 1534). % [ffc,bisect]
2 layers took 300 msec
Tells detecting entailment: 2043
Tells pruning: 2743
Tells failing: 0
Total tells: 5471
Asks detecting entailment: 0
Total asks: 0
Constraints created: 2043

| ?- main(72, 1, 27, 174, 1534). [ffc]
| ?- fd_statistics.
Tells detecting entailment: 4023
Tells pruning: 3562
Tells failing: 12
Total tells: 9235
Asks detecting entailment: 0
Total asks: 0
Constraints created: 2043

| ?- main(72, 1, 27, 174, 1534). [ffc,bisect]
Tells detecting entailment: 4039
Tells pruning: 3674
Tells failing: 12
Total tells: 9491
Asks detecting entailment: 0
Total asks: 0
Constraints created: 2043

| ?- main(72, 2, 9, 174, 1534). [ffc,bisect]
% still running after 1888 CPU minutes

| ?- main(72, 3, 6, 174, 1534). [ffc,bisect]
% still running after 1400 CPU minutes

