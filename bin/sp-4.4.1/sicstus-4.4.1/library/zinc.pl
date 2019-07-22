/* Copyright(C) 2008, Swedish Institute of Computer Science */

%   File       : zinc.pl
%   Author     : agren
%   Purpose    : Main interface file for library(zinc).

:- module(zinc, [fzn_dump/2,
		 fzn_dump/3,
		 fzn_identifier/3,
		 fzn_load_file/2,
		 fzn_load_stream/2,
		 fzn_objective/2,
		 fzn_output/1,
		 fzn_post/1,
		 fzn_run_file/1,
		 fzn_run_file/2,
		 fzn_run_stream/1,
		 fzn_run_stream/2,
		 fzn_solve/1,
		 mzn_load_file/2,
		 mzn_load_file/3,
		 mzn_load_model/2,
		 mzn_load_model/3,
		 mzn_run_file/1,
		 mzn_run_file/2,
		 mzn_run_model/1,
		 mzn_run_model/2,
		 mzn_to_fzn/2,
		 mzn_to_fzn/3
		]).

:- use_module(library('zinc/flatzinc')).
:- use_module(library('zinc/minizinc')).
