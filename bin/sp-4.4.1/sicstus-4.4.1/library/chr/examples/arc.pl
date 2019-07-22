% arc-consistency
% thom fruehwirth, ECRC 941128, LMU 980312

:- use_module(library(chr)).
:- use_module(library(lists), [select/3]).

%% handler arc.

:- chr_constraint dom/2, con/3.
% dom(X,D) variable X can take values from finite domain D, a ground list
% con(C,X,Y) there is a constraint C between variables X and Y

dom(X,[Y]) ==> X=Y.  % only to make unique solutions visible as bindings

con(C,X,Y) \ dom(X,XD), dom(Y,YD) <=> 
	reduce(x_y,X,XD,Y,YD,C, NYD),
	reduce(y_x,Y,YD,X,XD,C, NXD), 
	\+ (XD=NXD,YD=NYD)
	| 
        dom(X,NXD),dom(Y,NYD).

  reduce(CXY,X,XD,Y,YD,C, NYD):-    % try to reduce domain by one element
	select(GY,YD,NYD1),
	\+ (member(GX,XD),test(CXY,C,GX,GY))
        -> reduce(CXY,X,XD,Y,NYD1,C, NYD)
        ;
	YD=NYD.

    test(x_y,C,GX,GY):-
        test(C,GX,GY).
    test(y_x,C,GX,GY):-             
        test(C,GY,GX).


% An Instance: Santa Claus Example (in German)

example([anna-Anna,berta-Berta,carola-Carola,carl-Carl]):-
	dom(Anna,[laetzchen,schlafmuetze,filzpantoffel]),
	dom(Berta,[laetzchen,schlafmuetze,filzpantoffel]),
	dom(Carola,[laetzchen,schlafmuetze,filzpantoffel]),
	dom(Carl,[schlafmuetze,filzpantoffel]),		
	con(mehr_als,Carl,Anna),
	con(mehr_als,Berta,Carl),
	con(mehr_als,Berta,Carola),
	con(mindestens_wie,Berta,Carola),
	con(gleich_wie,Carl,Carola).

test(mehr_als,Geschenk,Geschenk1) :- 
    preis(Geschenk,Preis),
    preis(Geschenk1,Preis1),
    Preis > Preis1.

test(mindestens_wie,Geschenk,Geschenk1) :-
    preis(Geschenk,Preis),
    preis(Geschenk1,Preis1),
    Preis >= Preis1.

test(gleich_wie,Geschenk,Geschenk1) :-
    preis(Geschenk,Preis),
    preis(Geschenk1,Preis).

preis(laetzchen,10).
preis(schlafmuetze,20).
preis(filzpantoffel,30).

% eof handler arc -------------------------------------
















