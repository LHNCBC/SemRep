%   Commonly used sample statistics.
%   Copyright (C) 2016, Swedish Institute of Computer Science.  All rights reserved.
%   Author: Mats Carlsson

:- module(statistics, [
	min/2,
	max/2,
	min_max/3,
	range/2,
	mode/2,
	mean/2,
	arithmetic_mean/2,
	weighted_mean/3,
	geometric_mean/2,
	harmonic_mean/2,
	central_moment/3,
	skewness/2,
	kurtosis/2,
	ml_variance/2,
	population_variance/2,
	sample_variance/2,
	unbiased_variance/2,
	weighted_variance/3,
	ml_standard_deviation/2,
	population_standard_deviation/2,
	sample_standard_deviation/2,
	unbiased_standard_deviation/2,
	weighted_standard_deviation/3,
	covariance/3,
	correlation/3,
	median/2,
	fractile/3,
	normalize/2
   ]).

:- use_module(library(lists), [
	keyclumped/2,
	sumlist/2
   ]).

%@  This library module provides commonly used sample and population statistics functions.
%@  In this module, a @var{Sample} is simply a proper list of numbers, normally floating-point;
%@  @var{Weight} is a proper list of numbers and should be of the same length as @var{Sample}.
%@  
%@  @strong{Please note:} These functions are plain textbook algorithms and we make no claims
%@  about numerical stability, avoiding loss of precision, etc.
%@  
%@  Exported predicates:
%@  @table @code

%@  @item min(@var{+Sample}, @var{-Value})
%@  @PLXindex {min/2 (statistics)}
%@  is true when @var{Value} is the smallest element of @var{Sample}.
min(Sample, Value) :-
	min_max(Sample, Value, _).

%@  @item max(@var{+Sample}, @var{-Value})
%@  @PLXindex {max/2 (statistics)}
%@  is true when @var{Value} is the largest element of @var{Sample}.
max(Sample, Value) :-
	min_max(Sample, _, Value).

%@  @item min_max(@var{+Sample}, @var{-Min}, @var{-Max})
%@  @PLXindex {min_max/3 (statistics)}
%@  is true when @var{Min} (@var{Max}) is the smallest (largest) element of @var{Sample}.
min_max([V0|Sample], Min, Max) :-
	(   foreach(X,Sample),
	    fromto(V0,Min1,Min2,Min),
	    fromto(V0,Max1,Max2,Max)
	do  Min2 is min(Min1,X),
	    Max2 is max(Max1,X)
	).

%@  @item range(@var{+Sample}, @var{-Value})
%@  @PLXindex {range/2 (statistics)}
%@  is true when @var{Value} is the difference between the largest and smallest elements of @var{Sample}.
range(Sample, Value) :-
	min_max(Sample, Min, Max),
	Value is Max-Min.

%@  @item mode(@var{+Sample}, @var{-Values})
%@  @PLXindex {mode/2 (statistics)}
%@  is true when @var{Values} is the most frequently occurring value(s) in @var{Sample}. 
%@  If there is a unique value with maximum frequency, this value is returned as the only element of @var{Values}. 
%@  Otherwise, @var{Values} contains the maximum frequency elements in increasing order.
%@  This predicate does not make much sense if the sample is continuous.
mode(Sample, Values) :-
	(   foreach(X,Sample),
	    foreach(X-(-1),KL1)
	do  true
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(Y-Terms,KL3),
	    foreach(Sum-Y,KL4)
	do  sumlist(Terms, Sum)
	),
	keysort(KL4, KL5),
	keyclumped(KL5, KL6),
	KL6 = [_-Values|_].

%@  @item mean(@var{+Sample}, @var{-Value})
%@  @itemx arithmetic_mean(@var{+Sample}, @var{-Value})
%@  @PLXindex {mean/2 (statistics)}
%@  @PLXindex {arithmetic_mean/2 (statistics)}
%@  is true when @var{Value} is the arithmetic mean of @var{Sample}.
arithmetic_mean(Sample, Value) :-
	mean(Sample, Value).

:- mean/2 is documented_as(arithmetic_mean/2).

mean(Sample, Value) :-
	(   foreach(X,Sample),
	    fromto(0,Sum1,Sum2,Sum)
	do  Sum2 is Sum1+X
	),
	length(Sample, N),
	Value is Sum/N.

%@  @item weighted_mean(@var{+Weight}, @var{+Sample}, @var{-Value})
%@  @PLXindex {weighted_mean/3 (statistics)}
%@  is true when @var{Value} is the arithmetic mean of @var{Sample} weighted by @var{Weight}.
weighted_mean(Weight, Sample, Value) :-
	(   foreach(X,Sample),
	    foreach(W,Weight),
	    fromto(0,SSum1,SSum2,SSum),
	    fromto(0,WSum1,WSum2,WSum)
	do  SSum2 is SSum1+W*X,
	    WSum2 is WSum1+W
	),
	Value is SSum/WSum.

%@  @item geometric_mean(@var{+Sample}, @var{-Value})
%@  @PLXindex {geometric_mean/2 (statistics)}
%@  is true when @var{Value} is the geometric mean of @var{Sample}.
geometric_mean(Sample, Value) :-
	(   foreach(X,Sample),
	    fromto(0,Sum1,Sum2,Sum)
	do  Sum2 is Sum1+log(X)
	),
	length(Sample, N),
	Value is exp(Sum/N).

%@  @item harmonic_mean(@var{+Sample}, @var{-Value})
%@  @PLXindex {harmonic_mean/2 (statistics)}
%@  is true when @var{Value} is the harmonic mean of @var{Sample}.
harmonic_mean(Sample, Value) :-
	(   foreach(X,Sample),
	    fromto(0,Sum1,Sum2,Sum)
	do  Sum2 is Sum1+1/X
	),
	length(Sample, N),
	Value is N/Sum.

%@  @item central_moment(@var{K}, @var{+Sample}, @var{-Value})
%@  @PLXindex {central_moment/3 (statistics)}
%@  is true when @var{Value} is the @var{K}-th central moment of @var{Sample}.
%@  Also known as the @var{K}-th central moment about the mean.
%@  @var{K} should be positive integer.
central_moment(K, Sample, Value) :-
	mean(Sample, Mean),
	(   foreach(X,Sample),
	    fromto(0,Sum1,Sum2,Sum),
	    param(K,Mean)
	do  Sum2 is Sum1+(X-Mean)**K
	),
	length(Sample, N),
	Value is Sum/N.

%@  @item skewness(@var{+Sample}, @var{-Value})
%@  @PLXindex {skewness/2 (statistics)}
%@  is true when @var{Value} is the skewness of @var{Sample}.
%@  This is a measure of the asymmetry of its distribution.
%@  A sample with negative skew is said to be @emph{left-skewed}.
%@  Most of its mass is on the right of the distribution, with the tail on the left.
%@  Vice versa for positive skew.
%@  A sample's skewness is undefined if its variance is zero.
skewness(Sample, Value) :-
	mean(Sample, Mean),
	(   foreach(X,Sample),
	    fromto(0,C31,C32,C33),
	    fromto(0,C21,C22,C23),
	    param(Mean)
	do  C32 is C31+(X-Mean)**3,
	    C22 is C21+(X-Mean)**2
	),
	length(Sample, N),
	C3 is C33/N,
	C2 is C23/N,
	Value is C3 * C2 ** -1.5.

%@  @item kurtosis(@var{+Sample}, @var{-Value})
%@  @PLXindex {kurtosis/2 (statistics)}
%@  is true when @var{Value} is the excess kurtosis of @var{Sample}.
%@  This is a measure of the peakedness of its distribution.
%@  A high kurtosis indicates that most of the sample's variance is 
%@  due to infrequent severe deviations, rather than frequent modest deviations.
%@  A sample's excess kurtosis is undefined if its variance is zero.
%@  In this implementation, the kurtosis of the normal distribution is 0.
kurtosis(Sample, Value) :-
	mean(Sample, Mean),
	(   foreach(X,Sample),
	    fromto(0,C41,C42,C43),
	    fromto(0,C21,C22,C23),
	    param(Mean)
	do  C42 is C41+(X-Mean)**4,
	    C22 is C21+(X-Mean)**2
	),
	length(Sample, N),
	Value is N * C43 / (C23*C23) - 3.

%@  @item ml_variance(@var{+Sample}, @var{-Value})
%@  @itemx population_variance(@var{+Sample}, @var{-Value})
%@  @PLXindex {ml_variance/2 (statistics)}
%@  @PLXindex {population_variance/2 (statistics)}
%@  is true when @var{Value} is the maximum likelihood estimate of the variance of @var{Sample}.
%@  Also known as the population variance, where the denominator is the length of @var{Sample}.
ml_variance(Sample, Value) :-
	central_moment(2, Sample, Value).

:- population_variance/2 is documented_as(ml_variance/2).

population_variance(Sample, Value) :-
	central_moment(2, Sample, Value).

%@  @item sample_variance(@var{+Sample}, @var{-Value})
%@  @itemx unbiased_variance(@var{+Sample}, @var{-Value})
%@  @PLXindex {sample_variance/2 (statistics)}
%@  @PLXindex {unbiased_variance/2 (statistics)}
%@  is true when @var{Value} is the unbiased estimate of the variance of @var{Sample}.
%@  Also known as the sample variance, where the denominator is the length of @var{Sample} minus one.
sample_variance(Sample, Value) :-
	unbiased_variance(Sample, Value).

:- unbiased_variance/2 is documented_as(sample_variance/2).

unbiased_variance(Sample, Value) :-
	mean(Sample, Mean),
	(   foreach(X,Sample),
	    fromto(0,Sum1,Sum2,Sum),
	    param(Mean)
	do  Sum2 is Sum1+(X-Mean)**2
	),
	length(Sample, N),
	Value is Sum/(N-1).

%@  @item weighted_variance(@var{+Weight}, @var{+Sample}, @var{-Value})
%@  @PLXindex {weighted_variance/3 (statistics)}
%@  is true when @var{Value} is the weighted (biased) estimate of the variance of @var{Sample}.
weighted_variance(Weight, Sample, Value) :-
	weighted_mean(Weight, Sample, Mean),
	(   foreach(X,Sample),
	    foreach(W,Weight),
	    fromto(0,SSum1,SSum2,SSum),
	    fromto(0,WSum1,WSum2,WSum),
	    param(Mean)
	do  SSum2 is SSum1+(X-Mean)**2,
	    WSum2 is WSum1+W
	),
	Value is SSum/WSum.

%@  @item ml_standard_deviation(@var{+Sample}, @var{-Value})
%@  @itemx population_standard_deviation(@var{+Sample}, @var{-Value})
%@  @PLXindex {ml_standard_deviation/2 (statistics)}
%@  @PLXindex {population_standard_deviation/2 (statistics)}
%@  is true when @var{Value} is the maximum likelihood estimate of the standard deviation of @var{Sample}.
%@  Also known as the population standard deviation, where the denominator is the length of @var{Sample}.
%@  Equals the square root of the population variance.
ml_standard_deviation(Sample, Value) :-
	ml_variance(Sample, Variance),
	Value is sqrt(Variance).

:- population_standard_deviation/2 is documented_as(ml_standard_deviation/2).

population_standard_deviation(Sample, Value) :-
	population_variance(Sample, Variance),
	Value is sqrt(Variance).

%@  @item sample_standard_deviation(@var{+Sample}, @var{-Value})
%@  @itemx unbiased_standard_deviation(@var{+Sample}, @var{-Value})
%@  @PLXindex {sample_standard_deviation/2 (statistics)}
%@  @PLXindex {unbiased_standard_deviation/2 (statistics)}
%@  is true when @var{Value} is the unbiased estimate of the standard deviation of @var{Sample}.
%@  Also known as the sample standard deviation, where the denominator is the length of @var{Sample} minus one.
%@  Equals the square root of the sample variance.
sample_standard_deviation(Sample, Value) :-
	sample_variance(Sample, Variance),
	Value is sqrt(Variance).

:- unbiased_standard_deviation/2 is documented_as(sample_standard_deviation/2).

unbiased_standard_deviation(Sample, Value) :-
	unbiased_variance(Sample, Variance),
	Value is sqrt(Variance).

%@  @item weighted_standard_deviation(@var{+Weight}, @var{+Sample}, @var{-Value})
%@  @PLXindex {weighted_standard_deviation/3 (statistics)}
%@  is true when @var{Value} is the weighted (biased) estimate of the standard deviation of @var{Sample}.
%@  Equals the square root of the weighted (biased) variance.
weighted_standard_deviation(Weight, Sample, Value) :-
	weighted_variance(Weight, Sample, Variance),
	Value is sqrt(Variance).

%@  @item covariance(@var{+Sample1}, @var{+Sample2}, @var{-Value})
%@  @PLXindex {covariance/3 (statistics)}
%@  is true when @var{Value} is the covariance of @var{Sample1} and @var{Sample2}.
% covariance(Sample1, Sample2, Value) :-
% 	mean(Sample1, Mean1),
% 	mean(Sample2, Mean2),
% 	(   foreach(X,Sample1),
% 	    foreach(Y,Sample2),
% 	    fromto(0,S1,S2,S3),
% 	    param(Mean1,Mean2)
% 	do  S2 is S1+(X-Mean1)*(Y-Mean2)
% 	),
% 	length(Sample1, N),
% 	Value is S3/(N-1).
covariance(Sample1, Sample2, Value) :-
	(   foreach(X,Sample1),
	    foreach(Y,Sample2),
	    fromto(0,Xsum1,Xsum2,Xsum),
	    fromto(0,Ysum1,Ysum2,Ysum),
	    fromto(0,XYsum1,XYsum2,XYsum)
	do  Xsum2 is Xsum1+X,
	    Ysum2 is Ysum1+Y,
	    XYsum2 is XYsum1+X*Y
	),
	length(Sample1, N),
	Value is (N*XYsum - Xsum*Ysum)/(N*N-N).

%@  @item correlation(@var{+Sample1}, @var{+Sample2}, @var{-Value})
%@  @PLXindex {correlation/3 (statistics)}
%@  is true when @var{Value} is the correlation of @var{Sample1} and @var{Sample2}.
% correlation(Sample1, Sample2, Value) :-
% 	sample_variance(Sample1, Variance1),
% 	sample_variance(Sample2, Variance2),
% 	covariance(Sample1, Sample2, Cov),
% 	Value is Cov/sqrt(Variance1*Variance2).
correlation(Sample1, Sample2, Value) :-
	(   foreach(X,Sample1),
	    foreach(Y,Sample2),
	    fromto(0,Xsum1,Xsum2,Xsum),
	    fromto(0,Ysum1,Ysum2,Ysum),
	    fromto(0,X2sum1,X2sum2,X2sum),
	    fromto(0,Y2sum1,Y2sum2,Y2sum),
	    fromto(0,XYsum1,XYsum2,XYsum)
	do  Xsum2 is Xsum1+X,
	    Ysum2 is Ysum1+Y,
	    X2sum2 is X2sum1+X*X,
	    Y2sum2 is Y2sum1+Y*Y,
	    XYsum2 is XYsum1+X*Y
	),
	length(Sample1, N),
	Value is (N*XYsum - Xsum*Ysum) / sqrt(N*X2sum - Xsum*Xsum) / sqrt(N*Y2sum - Ysum*Ysum).

%@  @item median(@var{+Sample}, @var{-Value})
%@  @PLXindex {median/2 (statistics)}
%@  is true when @var{Value} is the median of @var{Sample}, that is,
%@  the value separating the higher half of the sample from the lower half.
%@  If there are an even number of observations, then the median is defined to be the smaller middle value.
%@  Same as the 0.5-fractile of @var{Sample}.
median(Sample, Value) :-
	(   foreach(X,Sample),
	    foreach(X-1,KL1)
	do  true
	),
	keysort(KL1, KL2),
	length(KL2, N),
	Ordinal is (N-1)>>1,
	Ordinal >= 0,
	length(Skip, Ordinal),
	append(Skip, [Value-_|_], KL2).

%@  @item fractile(@var{P}, @var{+Sample}, @var{-Value})
%@  @PLXindex {fractile/3 (statistics)}
%@  is true when @var{Value} is the @var{P}-fractile of @var{Sample}, that is,
%@  the smallest value in the sample such that the fraction @var{P} of the sample is less than or equal to that value.
%@  @var{P} should be a number in (0.0,1.0].
fractile(P, Sample, Value) :-
	(   foreach(X,Sample),
	    foreach(X-1,KL1)
	do  true
	),
	keysort(KL1, KL2),
	length(KL2, N),
	Ordinal is integer(ceiling(P*N))-1,
	Ordinal >= 0,
	length(Skip, Ordinal),
	append(Skip, [Value-_|_], KL2).

%@  @item normalize(@var{+Sample}, @var{-Normalized})
%@  @PLXindex {normalize/2 (statistics)}
%@  is true when @var{Normalized} is the normalized @var{Sample}, so that
%@  @var{Normalized} has a mean of 0 and a population standard deviation of 1.
normalize(Sample, Normalized) :-
	mean(Sample, Mean),
	population_standard_deviation(Sample, Dev),
	(   foreach(S,Sample),
	    foreach(N,Normalized),
	    param(Mean,Dev)
	do  N is (S-Mean)/Dev
	).

%@  @end table

