% RUNS ON mzn_fd
% RUNS ON mzn_mer_fd
% RUNS ON mzn_mer_lazyfd
% RUNS %% ON mzn_cd_fd
% RUNS ON zinc_fdic_mznlib
% RUNS ON zinc_mip_hl
% RUNS ON minizinc_fd
% Example battleships problem for battleships.mzn.
%
include "battleships.mzn.model";

n = 10;

a = array2d(ROW, COL,
    [| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     | 0, _, _, 0, _, _, _, _, _, _, _, 0
     | 0, _, 0, _, _, _, _, _, _, _, _, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, _, _, 0, _, _, _, _, _, _, _, 0
     | 0, _, 0, _, _, _, _, _, _, _, _, 0
     | 0, _, _, 0, _, _, _, _, _, _, _, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     |]
);

constraint a[1, 2] > 0;
constraint a[8, 3] > 0;

row_sums = [3, 2, 1, 1, 0, 4, 0, 5, 2, 2];

col_sums = [5, 1, 2, 2, 0, 2, 2, 1, 2, 3];

n_classes = 4;

class_sizes = [4, 3, 2, 1];

