% 980202, 980311 Thom Fruehwirth, LMU
% computes greatest common divisor of positive numbers written each as gcd(N)

:- use_module( library(chr)).

% handler gcd.

:- chr_constraint gcd/1.

gcd(0) <=> true.
gcd(N) \ gcd(M) <=> N=<M | L is M-N, gcd(L).
%gcd(N) \ gcd(M) <=> N=<M | L is M mod N, gcd(L).  % faster variant

/*
% Sample queries

gcd(2),gcd(3).

gcd(1.5),gcd(2.5).

gcd(37*11*11*7*3),gcd(11*7*5*3),gcd(37*11*5).
*/
