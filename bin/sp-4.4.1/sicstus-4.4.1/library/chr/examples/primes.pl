% Sieve of eratosthenes to compute primes
% thom fruehwirth 920218-20, 980311
% christian holzbaur 980207 for Sicstus CHR

:- use_module(library(chr)).

% handler primes.


% like chemical abstract machine

:- chr_constraint primes/1, prime/1.

primes(1) <=> true.
primes(N) <=> N>1 | M is N-1, prime(N), primes(M).

absorb(J) @ prime(I) \ prime(J) <=> J mod I =:= 0 | true.


% shorter variant

:- chr_constraint primes2/1.

primes2(N) ==> N>2 | M is N-1, primes2(M).

absorb2(J) @ primes2(I) \ primes2(J) <=> J mod I =:= 0 | true.


% faster variant

primes1(N):- primes1(2,N).

:- chr_constraint primes1/2, prime1/1.

primes1(N,M) <=> N> M | true.
primes1(N,M) <=> N=<M | N1 is N+1, prime1(N), primes1(N1,M).

absorb1(J) @ prime1(I) \ prime1(J) <=> J mod I =:= 0 | true.


% faster variant, rule order sensitive

:- chr_constraint primes3/1, prime3/1.

primes3(N) ==> prime3(2).
primes3(N),prime3(M) <=> M is N+1 | true.

prime3(N) ==> M is N+1, prime3(M).
absorb3(J) @ prime3(I) \ prime3(J) <=> J mod I =:= 0 | true.


% Concurrent program according to Shapiro

:- chr_constraint primes/2,integers/3,sift/2,filter/3.

primes(N,Ps) <=> integers(2,N,Ns), sift(Ns,Ps).

integers(F,T,Ns) <=> F > T | Ns=[].
integers(F,T,Ns) <=> F =< T | Ns=[F|Ns1], F1 is F+1, integers(F1,T,Ns1).

sift([P|Ns],Ps) <=> Ps=[P|Ps1], filter(Ns,P,Ns1), sift(Ns1,Ps1).
sift([],Ps) <=> Ps=[].

filter([X|In],P,Out) <=> 0 =\= X mod P | Out=[X|Out1], filter(In,P,Out1).
filter([X|In],P,Out) <=> 0 =:= X mod P | filter(In,P,Out).
filter([],P,Out) <=> Out=[].
