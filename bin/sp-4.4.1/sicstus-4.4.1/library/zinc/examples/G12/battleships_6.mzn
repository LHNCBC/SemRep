% RUNS ON mzn_fd
% RUNS ON mzn_mer_fd
% RUNS ON mzn_mer_lazyfd
% RUNS ON mzn_mer_lp
% RUNS %% ON mzn_cd_fd
% RUNS ON zinc_fdic_mznlib
% RUNS ON zinc_mip_hl
% Example battleships problem for battleships.mzn.
%
include "battleships.mzn.model";

n = 10;

a = array2d(ROW, COL,
    [| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, 0, _, _, _, _, _, _, _, _, _, 0
     | 0, _, 0, _, _, _, _, _, _, _, _, 0
     | 0, 0, _, _, _, _, _, _, _, _, _, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, _, _, _, _, _, _, _, _, _, _, 0
     | 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     |]
);

constraint a[7, 1] = 1;

row_sums = [0, 1, 6, 2, 2, 0, 1, 5, 0, 3];

col_sums = [4, 2, 3, 1, 1, 2, 1, 1, 0, 5];

n_classes = 4;

class_sizes = [4, 3, 2, 1];

