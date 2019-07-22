% Document type:  -*- Prolog -*-
% Filename:	  /amd/home/jojo/jnistuff/jnitest.pl
% Author:         Jesper Eskilson <jojo@scheutz.sics.se>
% Last-Update:    Time-stamp: <1998-01-09 1042 jojo>

connected(From, From, [From], _):- !.
connected(From, To, [From| Way], Been):-
        (   no_stop(From, Through)
        ;   
	    no_stop(Through, From)
	),
        not_been_before(Been, Through),
        connected(Through, To, Way, Been).

:- no_stop/2 is nondet.
no_stop('Stockholm', 'Katrineholm').
no_stop('Stockholm', 'Vasteras').
no_stop('Katrineholm', 'Hallsberg').
no_stop('Katrineholm', 'Linkoping').
no_stop('Hallsberg', 'Kumla').
no_stop('Hallsberg', 'Goteborg').
no_stop('Orebro', 'Vasteras').
no_stop('Orebro', 'Kumla').

not_been_before(Way, _) :- var(Way),!.
not_been_before([Been| Way], Am) :- 
        Been \== Am,
        not_been_before(Way, Am).
