% RUNS ON mzn_fd
% RUNS ON mzn_mer_fd
% RUNS ON mzn_cd_fd
% RUNS ON zinc_fdic_mznlib
% RUNS ON minizinc_fd
%-----------------------------------------------------------------------------
% Grocery puzzle
%
% Guido Tack, tack@gecode.org
% 2007-02-22
%
% Ported from the Gecode example
%
%-----------------------------------------------------------------------------

% A kid goes into a grocery store and buys four items. The cashier
% charges $7.11, the kid pays and is about to leave when the cashier
% calls the kid back, and says "Hold on, I multiplied the four items
% instead of adding them; I'll try again; Hah, with adding them the
% price still comes to $7.11''. What were the prices of the four items?
%
% The model is taken from: Christian Schulte, Gert Smolka, Finite Domain
% Constraint Programming in Oz. A Tutorial. 2001.
% Available from: http://www.mozart-oz.org/documentation/fdt/

%-----------------------------------------------------------------------------
% Model

array[0..3] of var 0..711: item;

constraint
  item[0] + item[1] + item[2] + item[3] = 711 /\
  item[0] * item[1] * item[2] * item[3] = 711 * 100*100*100;
  
% symmetry breaking
constraint
  item[0] < item[1] /\
  item[1] < item[2] /\
  item[2] < item[3];
  
solve satisfy;

output [ "grocery ", show(item), "\n" ];
