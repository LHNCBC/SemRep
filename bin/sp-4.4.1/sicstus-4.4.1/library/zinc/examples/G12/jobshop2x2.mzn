% RUNS ON mzn_fd
% RUNS ON mzn_mer_fd
% RUNS ON mzn_mer_lazyfd
% RUNS ON mzn_mer_lp
% RUNS ON mzn_cd_fd
% RUNS ON zinc_fdic_mznlib
% RUNS ON zinc_mip_hl
% RUNS ON minizinc_fd
%-----------------------------------------------------------------------------%
% Example from the MiniZinc paper:
% (square) job shop scheduling in MiniZinc
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Instance

size = 2;
d = [| 2,5
     | 3,4 |];

%-----------------------------------------------------------------------------%
% Model

int: size;                                  % size of problem
array [1..size,1..size] of int: d;          % task durations
int: total = sum(i,j in 1..size) (d[i,j]);  % total duration
array [1..size,1..size] of var 0..total: s; % start times
var 0..total: end;                          % total end time

predicate no_overlap(var int:s1, int:d1, var int:s2, int:d2) =
    s1 + d1 <= s2 \/ s2 + d2 <= s1;

constraint
    forall(i in 1..size) (
        forall(j in 1..size-1) (s[i,j] + d[i,j] <= s[i,j+1]) /\
        s[i,size] + d[i,size] <= end /\
        forall(j,k in 1..size where j < k) (
            no_overlap(s[j,i], d[j,i], s[k,i], d[k,i])
        )
    );

solve minimize end;

output [
  "jobshop2x2\n",
  "s[1..2, 1..2] = [", show(s[1, 1]), " ", show(s[1, 2]), "\n",
  "                 ", show(s[2, 1]), " ", show(s[2, 2]), "]\n"
];

%-----------------------------------------------------------------------------%
% Same in FlatZinc

% array[0..3] of var 0..14: s;
% var 0..14: end;
% var bool: b1;
% var bool: b2;
% var bool: b3;
% var bool: b4;
% constraint  int_lin_le     ([1,-1], [s[0], s[1]], -2);
% constraint  int_lin_le     ([1,-1], [s[1], end ], -5);
% constraint  int_lin_le     ([1,-1], [s[2], s[3]], -3);
% constraint  int_lin_le     ([1,-1], [s[3], end ], -4);
% constraint  int_lin_le_reif([1,-1], [s[0], s[2]], -2, b1);
% constraint  int_lin_le_reif([1,-1], [s[2], s[0]], -3, b2);
% constraint  bool_or(b1, b2, bool2var(true));
% constraint  int_lin_le_reif([1,-1], [s[1], s[3]], -5, b3);
% constraint  int_lin_le_reif([1,-1], [s[3], s[1]], -4, b4);
% constraint  bool_or(b3, b4, bool2var(true));
% solve minimize end;

%-----------------------------------------------------------------------------%
