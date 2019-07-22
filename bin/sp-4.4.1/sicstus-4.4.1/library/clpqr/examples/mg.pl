%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.2 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   mg.pl                                                  %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mg(P,T,I,B,MP) :-
	{
	    T = 1,
	    B + MP = P * (1 + I)
	}.
mg(P,T,I,B,MP) :-
	{
	    T > 1,
	    P1 = P * (1 + I) - MP,
	    T1 = T - 1
	},
	mg(P1, T1, I, B, MP).
