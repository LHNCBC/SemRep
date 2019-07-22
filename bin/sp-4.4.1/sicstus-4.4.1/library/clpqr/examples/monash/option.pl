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
% Algebraic combinations of options: see [Lassez, McAloon & Yap].
%

% Note that buy and sell are the negation of each other

% heaviside function
h(X,Y,Z) :-
	{Y < X , Z = 0}.
h(X,Y,Z) :-
	{Y >= X, Z = 1}. 

% ramp function
r(X,Y,Z) :-
	{Y < X , Z = 0}.
r(X,Y,Z) :-
	{Y >= X, Z = Y - X}.

% option valuation
% Variables are as previously described
value(Type,Buy_or_Sell,S,C,P,I,X,B,Value) :-
	check_param(S,C,P,I,X,B),
	get_sign(Buy_or_Sell,Sign),
	lookup_option(Type,S,C,P,I,X,B,
		     B1,B2,H1,H2,R1,R2),
	h(B1,S,T1),
	h(B2,S,T2),
	r(B1,S,T3),
	r(B2,S,T4),
	{Value = Sign*(H1*T1 + H2*T2 + R1*T3 + R2*T4)}.

% safety check
check_param(S,C,P,I,X,B) :-
	{S >= 0, C >= 0, P >= 0},
	{I >= 0, X >= 0, B >= 0 }.

% Buy or sell are just opposite
get_sign(buy,-1).
get_sign(sell,1).

% lookup option vector
lookup_option(Type,S,C,P,I,X,B,B1,B2,H1,H2,R1,R2) :- 
	table(Type,S,C,P,I,X,B,B1,B2,H1,H2,R1,R2).

% Table of values for B1,B2,H1,H2,R1,R2
% generic format - lookup_table(Type,Pos_neg,S,C,P,I,X,B,B1,B2,H1,H2,R1,R2).
% where K to R21 are obtained from the table
% M is a multiplier which is -1 or 1 depending on whether one
% is buying or selling the option
table(	stock,		S,	C,	P,	I,	X,	B,	0,	0,	S*(1+I),	0,	-1,	0).
table(	call,		S,	C,	P,	I,	X,	B,	0,	X,	C*(1+I),	0,	0,	-1).
table(	put,		S,	C,	P,	I,	X,	B,	0,	X,	P*(1+I)-X,	0,	1,	-1).
table(	bond,		S,	C,	P,	I,	X,	B,	0,	0,	B*(1+I),	0,	0,	0).


% A straightforward query is finding the
% value of selling a call option where
% the call price is 5, exercise price 50,
% interest rate 5% and current stock price 60.
% Note that this query assigns variables just
% to make the query look more readable
go1(Value) :- 
	Call=5,Exercise=50,Interest=0.05,Stock=60 ,
	value(call,sell,Stock,Call,_,Interest,Exercise,_,Value).

% Answer:
%  W = -4.75

% If we just the question upside down and
% ask what should the stock price be in order that
% my wealth is more than 5. Then we have

go2(Stock,W) :-
	{Call=5,Exercise=50,Interest=0.05 , W > 5},
	value(call,sell,Stock,Call,_,Interest,Exercise,_,W).

% Answer 1:
%  W = 5.25
%  50 > S
%  S >= 0

% Answer 2: 
%  W = 55.25 - S
%  50.25 > S
%  S >= 50


% straddle
% The general query is
% ?- Wealth=W1+W2,.... 
% value(call,buy,S,C,_,I,X,_,W1),
% value(put,buy,S,_,P,I,X,_,W2).

go3(Stock,W) :-
	{Call=5,Int=0.05,X=50,Put=7},
	{W >= 10},
	value(put,buy,Stock,_,Put,Int,X,_,W1),
	value(call,buy,Stock,Call,_,Int,X,_,W2),
	{W=W1+W2}.

% Answer 1:
%  S = 37.4 - W
%  W >= 10
%  S >= 0

% Answer 2: 
%  S = 62.6 + W
%  W >= 10
%  S >= 0


go4(S,W) :-
	{I=0.1,P1=10,X1=20},
	value(put,sell,S,_,P1,I,X1,_,W1),
	{P2=18,X2=40},
	value(put,buy,S,_,P2,I,X2,_,W2),
	{C3=15,X3=60},
	value(call,buy,S,C3,_,I,X3,_,W3),
	{C4=10,X4=80},
	value(call,sell,S,C4,_,I,X4,_,W4),
	{W=W1+W2+W3+W4}.

% Answer 1:
%  W = 5.7
%  20 > S
%  S >= 0

% Answer 2: 
%  S = 25.7 - W
%  5.7 >= W
%  14.3 + W > 0
%  S >= 0

% Answer 3: 
%  W = -14.3
%  S >= 40
%  60 > S

% Answer 4: 
%  S = 74.3 + W
%  14.3 + W >= 0
%  5.7 > W
%  S >= 0

% Answer 5:
%  W = 5.7
%  S >= 80

go5(S,W) :-
	{Put=5,Exercise=20,Interest=0.1},
	{S >= 0},
	{W > 100},
	value(put,buy,S,_,Put,Interest,Exercise,_,W).

% No solution.

?- printf("\n>>> Sample goals (some with multiple answers):\n", []),
   printf("    go1(W), go2(S, W), go3(S, W), go4(S, W), go5(S, W)\n", []).
