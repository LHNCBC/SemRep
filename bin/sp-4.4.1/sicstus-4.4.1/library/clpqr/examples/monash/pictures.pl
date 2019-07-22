:- use_module(library('clpqr/monash')).

/*
 **********************************************************************
 *
 *      CLP(R) Version 2.0	(Example Programs Release)
 *	(C) Copyright, March 1986, Monash University
 *
 **********************************************************************
 */

/*
 *	This example is credited to Richard Helm & Kim Marriot
 */
%
% Package which allows rule-based descriptions of pictures to be translated into
% commands in the "pic" package.
% for example, try       ?- tell(picout), go2, told.
% and then from the UNIX command line try
%      pic picout | troff | ...
%

inter( P1, P2 ,P3 ,P4 ,P5 ,P6 ) :-

	intersect(P1, P2, P3, P4, PP1 ) ,
	intersect(P1, P2, P5, P6, PP2 ) ,
	intersect(P5, P6, P3, P4, PP3 ) ,

	line( [ PP1, PP2 ] ),
	line( [ PP2, PP3 ] ),
	line( [ PP3, PP1 ] ),

	darrow( [ P1, P2 ] , dashed ),
	darrow( [ P3, P4 ] , dashed ),
	darrow( [ P5, P6 ] , dashed ),

	text( c1, P1,  rjust ),
	text( c2, P3,  rjust ),
	text( c3, P5,  rjust ).

intersect(P1,P2,P3,P4,PP) :-

	gradient(P1,P2,M1),
	gradient(P3,P4,M2),

	lineeq(P1,M1,C1),
	lineeq(P3,M2,C2),

	lineeq( PP, M1, C1 ),
	lineeq( PP, M2, C2 ).

lineeq(p(X,Y),M, C ) :-
	{Y = M * X  + C}.

gradient(p(P1x,P1y),p(P2x,P2y),M ) :-
	{M = ( P2y -P1y ) / ( P2x -P1x ) }. 

%--------------------
% the required pic interface
%--------------------

pstart(S) :-
	printf(".PS %gi\n" , [ S ] ).

pend :-
	printf(".PE\n", [] ).

box( box( p(Ox,Oy), p(Cx,Cy) ), DotDashInvis , Text ) :-
	{Mx = (Ox + Cx ) / 2 },
	{My = (Oy + Cy ) / 2 },
	{H = Cx - Cy },
	{W = Cy - Oy },
	printf( "box at %g,%g width %g hieght %g \n" , [ Mx,My,W,H ] ) .

line( [ p(Ox,Oy), p(Cx,Cy) ], Style ) :-
	printf( "line from %g,%g to %g,%g %\n" , [ Ox,Oy,Cx,Cy,Style ] ) .

darrow( [ p(Ox,Oy), p(Cx,Cy) ],Style ) :-
	printf( "line from %g,%g to %g,%g %\n" , [ Ox,Oy,Cx,Cy,Style ] ) .

arrow( [ p(Ox,Oy), p(Cx,Cy) ],Style ) :-
	printf( "line from %g,%g to %g,%g %\n" , [ Ox,Oy,Cx,Cy,Style ] ) .

line( [ p(Ox,Oy), p(Cx,Cy) ] ) :-
	printf( "line from %g,%g to %g,%g \n" , [ Ox,Oy,Cx,Cy ] ) .

text(  Text , p(Ox,Oy),  Just  ) :-
	printf( " %s %s %s at %g,%g %s \n" , [ '"', Text, '"', Ox, Oy, Just] ) .

quadrilateral( Pt1, Pt2, Pt3, Pt4, MPt1, MPt2, MPt3, MPt4) :- 
	parallelogram(MPt1, MPt2, MPt3, MPt4) ,
	lineWithMidPoint(Pt1,Pt2,MPt1) ,
	lineWithMidPoint(Pt2,Pt3,MPt2) ,
	lineWithMidPoint(Pt3,Pt4,MPt3) ,
	lineWithMidPoint(Pt4,Pt1,MPt4) ,
	line([ MPt1, MPt2 ], dashed),
	line([ MPt2, MPt3 ], dashed),
	line([ MPt3, MPt4 ], dashed),
	line([ MPt4, MPt1 ], dashed).

lineWithMidPoint( p(P1x,P1y), p(P2x,P2y) , p(Mx,My) ) :-
	{P1x - Mx = Mx - P2x },
	{P1y - My = My - P2y },
	line([ p(P1x,P1y) , p(P2x,P2y) ]).

parallelogram(p(P1x,P1y), p(P2x,P2y), p(P3x,P3y), p(P4x,P4y)) :-
	{P1x - P2x = P4x - P3x},
	{P1y - P2y = P4y - P3y},
	{P1x - P4x = P2x - P3x},
	{P1y - P4y = P2y - P3y}.

go1:- 
	pstart(2),
	inter( p(0,1), p(4,2) ,p(3.5,1) ,p(2,5) ,p(0.33,0.5) ,p(3,5.5 )  ),
	pend.

% Output:
%  .PS 2i
%  line from 3.2,1.8 to 0.688979,1.172245 
%  line from 0.688979,1.172245 to 2.302393,4.193619 
%  line from 2.302393,4.193619 to 3.2,1.8 
%  line from 0,1 to 4,2 dashed
%  line from 3.5,1 to 2,5 dashed
%  line from 0.33,0.5 to 3,5.5 dashed
%   " c1 " at 0,1 rjust 
%   " c2 " at 3.5,1 rjust 
%   " c3 " at 0.33,0.5 rjust 
%  .PE

go2:- 
	pstart(3),
	quadrilateral( p(0,0), p(8,10) ,p(10,10) ,p(10,1) ,_, _, _, _  ),
	quadrilateral( p(0,20), p(0,15) ,p(10,15) ,p(10,20) ,_, _, _, _  ),
	quadrilateral( p(5,30), p(10,23), p(15,30), p(10,25), _, _, _, _),
	pend.

% Output:
%  .PS 3i
%  line from 0,0 to 8,10 
%  line from 8,10 to 10,10 
%  line from 10,10 to 10,1 
%  line from 10,1 to 0,0 
%  line from 4,5 to 9,10 dashed
%  line from 9,10 to 10,5.5 dashed
%  line from 10,5.5 to 5,0.5 dashed
%  line from 5,0.5 to 4,5 dashed
%  line from 0,20 to 0,15 
%  line from 0,15 to 10,15 
%  line from 10,15 to 10,20 
%  line from 10,20 to 0,20 
%  line from 0,17.5 to 5,15 dashed
%  line from 5,15 to 10,17.5 dashed
%  line from 10,17.5 to 5,20 dashed
%  line from 5,20 to 0,17.5 dashed
%  line from 5,30 to 10,23 
%  line from 10,23 to 15,30 
%  line from 15,30 to 10,25 
%  line from 10,25 to 5,30 
%  line from 7.5,26.5 to 12.5,26.5 dashed
%  line from 12.5,26.5 to 12.5,27.5 dashed
%  line from 12.5,27.5 to 7.5,27.5 dashed
%  line from 7.5,27.5 to 7.5,26.5 dashed
%  .PE

?- printf("\n>>> Sample goals: go1/0, go2/0\n", []).
