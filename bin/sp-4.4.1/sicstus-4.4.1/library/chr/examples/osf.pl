% Order Sorted Feature Constraints -------------------------------------------
% following DEC-PRL Research Report 32, May 1993, by H. Ait-Kaci, A. Podelski
%           and S.C. Goldstein on "Order-Sorted Feature Theory Unification"
% see also cft.pl, kl-one.pl, type.pl
% 940603 ECRC, 980211, 980312 Thom Fruehwirth LMU for Sicstus CHR

:- use_module( library(chr)).

% handler osf.

% :- chr_option(already_in_store, on). 

:- op(150,xfx,'=>').    % label has value constraint
:- op(100,xfx,'::').    % sort constraint
:- op(100,xfx,'..').    % feature constraint
:- op(450,xfx,'##').    % equality constraint 
:- op(450,fx,'theory'). % OSF theory clause

:- chr_constraint (::)/2, (##)/2.

X :: Y \ X :: Y <=> true.
X ## Y \ X ## Y <=> true.


% OSF Term Dissolution
X::T <=> nonvar(T), \+ atomic(T) | dissolve(X,T).

	dissolve(X,T):- T=..[S|Ls], X::S, dissolve1(X,Ls).
	dissolve1(X,[]).
	dissolve1(X,[L1=>T1|Ls]):- X..L1##Y, dissolve0(Y,T1), dissolve1(X,Ls).
	dissolve0(Y,T):- var(T), !, Y=T.
	dissolve0(Y,X::T):- !, Y=X, dissolve(Y,T).
	dissolve0(Y,T):- Y::T.


% OSF Clause Normalization Rules
% see Figure 1, p. 6 of DEC-PRL RR 32

% (1) sort intersection
X::S1, X::S2 <=> atomic(S1),atomic(S2) | sort_intersection(S1,S2,S3), X::S3.

% (2) inconsistent sort
%     reflected down to built-in constraints true and fail
X::bot <=> fail.
X::top <=> true.

% (3) variable elimination
%     reflected down to built-in constraint for equality
X##Y <=> var(X) | X=Y.

% (4) feature decomposition
X..L##Y \ X..L##Z <=> Y=Z.


% OSF Theory Unification
%     preliminary version, theory represented by Prolog facts
X::S#Id, X.._##_ ==> atomic(S),theory X::T,functor(T,S,_) | X::T.


% EXAMPLES ---------------------------------------------------------------

% cyclic structure, page 1, DEC-PRL RR 32
eg1(P):- 
    P::person(name=>id(first=>string,
		  last=>S::string),
	 age=>30,
	 spouse=>person(name=>id(last=>S),
                        spouse=>P)).

% cyclic structure, p. 3, DEC-PRL RR 32
eg2(X):-
    X::cons(head=>1,tail=>X).
eg2a(X):-	% same as eg2(X)
     X::cons(head=>1,tail=>X), X::cons(head=>1,tail=>cons(head=>1,tail=>X)).

% p.17, DEC-PRL RR 32
eg3(X):-			
    X::s1(l1=>s),X::s2(l2=>s).

sort_intersection(s1,s2,s3).
sort_intersection(s2,s1,s3).

% non-empty theory
theory YS1::s1(l1=>Y1::s).
theory YS2::s2(l2=>Y2::s).
theory YS3::s3(l1=>Y3::s(l=>Y4::s),l2=>Y3).
theory YS::s(l=>Y5::s).

/*
| ?- eg1(X) ; eg2(X) ; eg2a(X) ; eg3(X).

X::person,
X..name##_A,
_A::id,
_A..first##_B,
_B::string,
_A..last##_C,
_C::string,
X..age##_D,
_D::30,
X..spouse##_E,
_E::person,
_E..name##_F,
_F::id,
_F..last##_C,
_E..spouse##X ? ;

X::cons,
X..head##_A,
_A::1,
X..tail##X ? ;

X::cons,
X..head##_A,
_A::1,
X..tail##X ? ;

X..l1##_A,
_A::s,
X::s3,
_A..l##_B,
_B::s,
X..l2##_A ? 

*/

% end of handler osf ----------------------------------------------------------
