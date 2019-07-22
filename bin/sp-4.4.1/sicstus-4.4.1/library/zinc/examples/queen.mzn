int: n;

array [1..n] of var 1..n: q;

constraint
    forall (i in 1..n, j in i+1..n) (
	q[i] != q[j] /\
	q[i] + i != q[j] + j /\
	q[i] - i != q[j] - j
    );

solve satisfy;

output ["A solution to the ", show(n), " Queens problem: ", show(q), "\n"];
