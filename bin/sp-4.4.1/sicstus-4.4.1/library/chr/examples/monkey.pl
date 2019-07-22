%
% Monkey and Bananas:
%
% Forward chaining rules via CHR.
% rules inspired from ftp://ftp.cs.unibo.it:/pub/gaspari/fw_rules/
% Quite fast because no dynamic predicates are used to
% represent the facts.
% The amount of code generated is substantial however.
% Not optimized
%
% 970213 Christian Holzbaur

:- use_module(library(chr)).

% handler monkey.

:- chr_constraint phys_object/7, monkey/3, goal/5, found/0.
% explaination of constraints is missing here

:- op(900,fy,not).
% There is no such fact ('not exists' in SQL)
not Fact :- find_constraint( Fact, _), !, fail.
not _.


testcase(1) :-
	phys_object(bananas,9-9,light,ceiling,_,_,ok),
	phys_object(couch,7-7,heavy,floor,_,low,_),
	phys_object(ladder,4-3,light,floor,_,high,_),
	phys_object(blanket,7-7,light,_,_,_,_),
	phys_object(garbage_can,3-5,light,floor,_,low,_),
	monkey(7-7,couch,blanket),
	goal(active,holds,bananas,_,_).


 rule(1) @
  goal(active,on,floor,A,B), 
    monkey(D,E,F) <=>

    E\==floor
    |
    write('Jump onto the floor'),
    nl,
    monkey(D,floor,F),
    goal(satisfied,on,floor,A,B).


 rule(2) @
  monkey(A,floor,B) \ 
    goal(active,on,floor,D,E) <=>

    write('Monkey is already on floor'),
    nl,
    goal(satisfied,on,floor,D,E).


 rule(3) @
  phys_object(A,B,C,floor,D,E,F) \ 
    goal(active,on,A,H,I), 
      monkey(B,K,nothing) <=>

      K\==A
      |
      write('Climb onto '),
      write(A),
      nl,
      monkey(B,A,nothing),
      goal(satisfied,on,A,H,I).


 rule(4) @
  goal(active,on,A,B,C), 
    phys_object(A,E,F,G,H,I,J), 
      monkey(E,L,M) ==>

      M\==nothing
      |
      write('Put '),
      nl,
      goal(active,holds,nothing,O,P).


 rule(5) @
  goal(active,on,A,B,C), 
    phys_object(A,E,F,floor,G,H,I), 
      monkey(K,L,M) ==>

      K\==E
      |
      goal(active,at,nothing,O,E).


 rule(6) @
  phys_object(A,B,C,floor,D,E,F), 
    monkey(B,A,H) \ 
      goal(active,on,A,J,K) <=>

      write('Monkey is already on '),
      write(A),
      nl,
      goal(satisfied,on,A,J,K).


 rule(7) @
  goal(active,holds,nothing,A,B), 
    monkey(D,E,F), 
      phys_object(F,H,I,J,K,L,M) <=>

      F\==nothing
      |
      write('Drop '),
      write(F),
      nl,
      goal(satisfied,holds,nothing,A,B),
      monkey(D,E,nothing),
      phys_object(F,H,I,floor,K,L,M).


 rule(8) @
  goal(active,holds,nothing,A,B), 
    monkey(D,E,nothing) ==>

    write('Monkey is holding nothing'),
    nl,
    goal(satisfied,holds,nothing,A,B).


 rule(9) @
  phys_object(ladder,A,B,floor,C,D,E) \ 
    goal(active,holds,G,H,I), 
      phys_object(G,A,light,ceiling,K,L,M), 
        monkey(O,ladder,nothing) <=>

        not phys_object(Q,R,S,G,T,U,V)
        |
        write('Grab '),
        write(G),
        nl,
        monkey(O,ladder,G),
        phys_object(G,A,light,nothing,K,L,M),
        goal(satisfied,holds,G,H,I).


 rule(10) @
  goal(active,holds,A,B,C), 
    phys_object(A,E,light,ceiling,F,G,H), 
      phys_object(ladder,E,J,floor,K,L,M), 
        monkey(O,P,Q) ==>

        P\==ladder
        |
        goal(active,on,ladder,S,T).


 rule(11) @
  goal(active,holds,A,B,C), 
    phys_object(A,E,light,ceiling,F,G,H), 
      phys_object(ladder,J,K,L,M,N,O) ==>

      J\==E,
      not goal(active,at,ladder,Q,E)
      |
      goal(active,at,ladder,R,E).


 rule(12) @
  goal(active,holds,A,B,C), 
    phys_object(A,E,light,F,G,H,I), 
      monkey(E,floor,nothing) <=>

      F\==ceiling,
      not phys_object(L,M,N,A,O,P,Q)
      |
      write('Grab '),
      write(A),
      nl,
      phys_object(A,E,light,nothing,G,H,I),
      monkey(E,floor,A),
      goal(satisfied,holds,A,B,C).


 rule(13) @
  goal(active,holds,A,B,C), 
    phys_object(A,E,light,F,G,H,I), 
      monkey(E,F,K) ==>

      F\==ceiling,
      F\==floor
      |
      goal(active,on,floor,M,N).


 rule(14) @
  goal(active,holds,A,B,C), 
    phys_object(A,E,light,F,G,H,I), 
      monkey(K,L,M) ==>

      F\==ceiling,
      K\==E,
      not goal(active,at,nothing,O,P)
      |
      goal(active,at,nothing,Q,E).


 rule(15) @
  goal(active,holds,A,B,C), 
    phys_object(A,E,light,F,G,H,I), 
      monkey(E,K,L) ==>

      L\==nothing,
      L\==A,
      not goal(active,holds,nothing,N,O)
      |
      goal(active,holds,nothing,P,Q).


 rule(16) @
  goal(active,at,A,B,C), 
    monkey(E,floor,A), 
      phys_object(A,G,H,I,J,K,L) <=>

      E\==C
      |
      write('Move '),
      write(A),
      write(' to '),
      write(C),
      nl,
      phys_object(A,C,H,I,J,K,L),
      monkey(C,floor,A),
      goal(satisfied,at,A,B,C).


 rule(17) @
  goal(active,at,A,B,C), 
    monkey(E,F,A), 
      phys_object(A,H,I,J,K,L,M) ==>

      F\==floor,
      H\==C,
      not goal(active,on,floor,O,P)
      |
      goal(active,on,floor,Q,R).


 rule(18) @
  goal(active,at,A,B,C), 
    phys_object(A,E,light,F,G,H,I), 
      monkey(K,L,M) ==>

      E\==C,
      M\==A,
      not goal(active,holds,A,O,P)
      |
      goal(active,holds,A,Q,R).


 rule(19) @
  phys_object(A,B,light,C,D,E,F) \ 
    goal(active,at,A,H,B) <=>

    write('The object '),
    write(A),
    write(' is already at '),
    write(B),
    nl,
    goal(satisfied,at,A,H,B).


 rule(20) @
  goal(active,at,nothing,A,B), 
    monkey(B,floor,nothing) <=>

    write('Walk to '),
    write(B),
    nl,
    monkey(B,floor,nothing),
    goal(satisfied,at,nothing,A,B).


 rule(21) @
  goal(active,at,nothing,A,B), 
    monkey(D,floor,E), 
      phys_object(E,G,H,I,J,K,L) <=>

      D\==B
      |
      write('Walk to '),
      write(B),
      write(' carrying '),
      write(E),
      nl,
      monkey(B,floor,E),
      phys_object(E,B,H,I,J,K,L),
      goal(satisfied,at,nothing,A,B).


 rule(22) @
  goal(active,at,nothing,A,B), 
    monkey(D,E,F) ==>

    E\==floor,
    D\==B
    |
    goal(active,on,floor,H,I).


 rule(23) @
  monkey(A,B,C) \ 
    goal(active,at,nothing,E,A) <=>

    write('Monkey is already at '),
    write(A),
    nl,
    goal(satisfied,at,nothing,E,A).


 rule(24) @
  goal(satisfied,A,B,C,D) ==>

  not goal(active,F,G,H,I),
  not found
  |
  write('CONGRATULATIONS the goals are satisfied'),
  nl,
  found.


 rule(25) @
  goal(active,holds,A,B,C), 
    phys_object(A,E,light,nothing,F,G,H), 
      monkey(E,J,A) ==>

      write('Object '),
      write(A),
      write(' is already being held'),
      nl,
      goal(satisfied,holds,A,B,C).

end_of_file.
