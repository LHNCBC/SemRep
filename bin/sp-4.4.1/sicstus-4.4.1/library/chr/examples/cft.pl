% Feature Tree Constraints (CFT) ---------------------------------------------
% following Records for Logic Programming (Smolka,Treinen) JLP 1994:18:229-258
% 950512 Thom Fruehwirth ECRC, based on osf.pl, see also kl-one.pl, type.pl
% 980211, 980311 Thom Fruehwirth LMU for Sicstus CHR

:- use_module( library(chr)).

% handler cft.

:- op(100,xfx,'::').  % Variable::Sort/Expression  sort constraint
:- op(100,xfx,'@@').  % Variable@@LabelList        arity/label constraint
:- op(450,xfy,'##').  % Variable##Feature##Value   feature constraint
% in X@@A assumes that A is a sorted list of ground features 
% in X##F##Y assumes that feature F is a ground term and Y stays a variable or is atomic

:- chr_constraint (::)/2, (@@)/2, (##)/2.

% CFT Term Dissolution
X::T <=> nonvar(T), \+ atomic(T) | dissolve(X,T).

	dissolve(X,T):- 
                T=..[S|Ls], X::S, dissolve1(X,Ls,A), sort(A,As), X@@As.
	dissolve1(X,[],[]).
	dissolve1(X,[L1::T1|Ls],[L1|Ls1]):- 
		X##L1##TV, 
		(nonvar(T1) -> dissolve(TV,T1) ; TV=T1),
		dissolve1(X,Ls,Ls1).

%!!!   sort arity list, load member/2

% CFT Axiom scheme
% see section 3, p.235, p.236
% see proof of proposition 6.5, p.245

% (S) sort are pairwise disjoint
X::S1 \ X::S2 <=> S1=S2.

% (F) features are functional
X##L##Y \ X##L##Z <=> Y=Z.

% (A2) arities are unique
% sorting removes duplicate features
X@@A1 \ X@@A2 <=> A1=A2.

% (A1) If X has arity A, exactly the features in A are defined on X
X@@A, X##F##Y ==> member(F,A).

% (D) determinant
% not implemented yet


% EXAMPLES ---------------------------------------------------------------

% page 236, determinant
eg0([U,V,W]-[X,Y,Z]):-
	X::a(f::V,g::Y),
	Y::b(f::X,g::Z,h::u),
	Z::a(f::W,g::Y,h::Z).

% cyclic structure, adapted from page 1, DEC-PRL RR 32
eg1(P):- 
    P::person(name::id(first::_,
		  last::S),
	 age::30,
	 spouse::person(name::id(last::S),
                        spouse::P)).

% cyclic list, adapted from p. 3, DEC-PRL RR 32
eg2(X):-
X::cons(head::1,tail::X).
eg2a(X):-	% same result as eg2(X)
X::cons(head::1,tail::X), X::cons(head::1,tail::cons(head::1,tail::X)).

% adapted from p.17, DEC-PRL RR 32
eg3(X):-			
X::s1(l1::s),X::s2(l2::s).

/*

| ?- eg0(X); eg1(X) ; eg2(X) ; eg2a(X) ; eg3(X).

X = [_A,_B,_C]-[_D,_E,_F],
_D::a,
_D##f##_B,
_D##g##_E,
_D@@[f,g],
_E::b,
_E##f##_D,
_E##g##_F,
_E##h##_G,
_G::u,
_G@@[],
_E@@[f,g,h],
_F::a,
_F##f##_C,
_F##g##_E,
_F##h##_F,
_F@@[f,g,h] ? ;

X::person,
X##name##_A,
_A::id,
_A##first##_B,
_A##last##_C,
_A@@[first,last],
X##age##_D,
_D::30,
_D@@[],
X##spouse##_E,
_E::person,
_E##name##_F,
_F::id,
_F##last##_C,
_F@@[last],
_E##spouse##X,
_E@@[name,spouse],
X@@[age,name,spouse] ? ;

X::cons,
X##head##_A,
_A::1,
_A@@[],
X##tail##X,
X@@[head,tail] ? ;

X::cons,
X##head##_A,
_A::1,
_A@@[],
X##tail##X,
X@@[head,tail] ? ;

*/

% end of handler cft ----------------------------------------------------------




