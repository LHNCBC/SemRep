% RUNS ON mzn_fd
% RUNS ON mzn_mer_fd
% RUNS ON mzn_cd_fd
% RUNS ON zinc_mip_hl

% vim: ft=zinc ts=4 sw=4 et
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Thu Feb 26 13:35:51 EST 2009
%
% Given an nrows x ncols matrix of 0..n, find an arrangement of dominoes
% AB where A, B in 0..n and A <= B that matches the matrix.

include "globals.mzn";

% ---- Parameters ----

int: n;                                 % Dominoes are {(A, B) | A, B in 0..n}
int: ncols;                             % Board width.
int: nrows;                             % Board height.
array [row, col] of tile: a;            % Board layout.

% Derived constants.

set of int: col = 1..ncols;
set of int: row = 1..nrows;
set of int: tile = 0..n;

% ---- Model ----

% Each interior square on the board must have the midpoint of exactly one
% domino on eactly one of its edges.  We define h[r, c] = 1 iff a domino
% straddles (r, c) and (r, c + 1).  We define v[r, c] = 1 iff a domino
% straddles (r, c) and (r + 1, c).

array [row, col] of var 0..1: h;
array [row, col] of var 0..1: v;

constraint forall (r in row) (h[r, ncols] = 0);
constraint forall (c in col) (v[nrows, c] = 0);

constraint
    forall (r in row, c in col) (
        h[r, c] + if 1 < c then h[r, c - 1] else 0 endif +
        v[r, c] + if 1 < r then v[r - 1, c] else 0 endif
    =
        1
    );

% Every domino has to appear exactly once.
%
constraint
    forall (A, B in tile where A <= B) (
        sum (r in row, c in col where c < ncols) (
            if a[r, c] = A /\ a[r, c + 1] = B then h[r, c] else 0 endif +
            if a[r, c] = B /\ a[r, c + 1] = A then h[r, c] else 0 endif
        ) +
        sum (r in row, c in col where r < nrows) (
            if a[r, c] = A /\ a[r + 1, c] = B then v[r, c] else 0 endif +
            if a[r, c] = B /\ a[r + 1, c] = A then v[r, c] else 0 endif
        ) = if A = B then 2 else 1 endif    % Doubles are counted twice.
    );

solve satisfy;

output [
   "h = ", show(h), "\n",
   "v = ", show(v), "\n"
];
