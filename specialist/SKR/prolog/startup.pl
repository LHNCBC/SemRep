:- use_module(library(lists), [
	last/2
   ]).

:- use_module(library(system), [
	environ/2
   ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% STARTUP STUFF %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

l :- init_application_environment.

a :- environ('PWD', PWD), show_init_data(PWD).

init_application_environment :-
	% environ('GWA', GWA),
	environ('PWD',  PWD),
	% determine_environment(GWA, PWD, Env),
 	init_application(PWD),
	nl,
	show_init_data(PWD).

% If the user's home directory is a sub_atom of the current working directory,
% then the environment is DEVELOPMENT; otherwise, it's PRODUCTION.
% determine_environment(GWA, PWD, Environment) :-
% 	( sub_atom('SICS', PWD) ->
% 	  Environment = sics
% 	; sub_atom(GWA, PWD) ->
% 	  Environment = devl
% 	; Environment = prod
% 	).

init_application(PWD) :-
	% determine_application(PWD, App),
	% init_application_paths(App, Env),
	init_application_paths,
	% temp05(App),
 	compile_application(PWD)
	% announce_temp05(App)
      ; true.	

compile_application(PWD) :-
	concat_atoms([PWD, '/', 'loader.pl'], File),
	( file_exists(File) ->
	  compile(File)
        ; format(user_output, 'No loader.pl found in ~w.~n', [PWD])
	).

show_init_data(PWD) :-
	format(user_output, 'PWD:  ~w~n', [PWD]),
	possibly_environ('EXTRA_SICSTUS_ARGS', Args),
	format(user_output, 'ARGS: ~w~n', [Args]),
	format(user_output, '~nOpen Streams:~n', []),
	show_all_streams.
	% format('~nControl Options:~n', []).
	% format('~nFile Overrides:~n', []),
	% bcl override_file,
	% format('~nControl Option Overrides:~n', []),
	% bcl override_control_option.

possibly_environ(EnvironVar, Value) :-
	( environ(EnvironVar, Value) ->
	  true
	; Value = []
	).

% If 'USemrep' is a sub_atom of the current working directory,
% then the area is USEMREP; otherwise, it's ORIGINAL_SYSTEMS.
determine_application(_PWD, _Area) :- !.


determine_application(PWD, Area) :-
	( sub_atom('SKR', PWD) ->
	  Area = skr
	; sub_atom('public_mm', PWD) ->
	  Area = skr
	; environ('USER', USER),
	  ( USER == 'flang' ->
	    Area = skr
	  ; USER == 'alan' ->
	    Area = skr
	  ; format(user_output,
		   '~n~nNot in application directory; application env not initiated.~n~n', []),
	    Area = foobar
	  )
	).

% system_path_alias(demo).
% system_path_alias(helpsys).
% system_path_alias(language).
% system_path_alias(library).
% system_path_alias(messages).
% system_path_alias(package).
% system_path_alias(qplib).
% system_path_alias(quintus).
% system_path_alias(runtime).
% system_path_alias(system).
% system_path_alias(tutorial).
% 
% retract_user_defined_paths :-
% 	file_search_path(PathAlias, DirSpec),
% 	\+ system_path_alias(PathAlias),
% 	PathAlias \== prolog_utils,
% 	retract(file_search_path(PathAlias, DirSpec)),
% 	fail
%       ; true.

% iap :- init_application_paths(skr, devl).

% init_skr :- init_application_paths(skr, devl).

init_application_paths :-
	format(user_output, '~nInitiating paths....~n~n', []),
	max_area_length(MaxAreaLength),
	% retract_user_defined_paths,
	define_path(Area, Data),
	% translate_path(Data, App, Env, Path),
	% format(user_output, '~nTranslating ~w ~w ~w ~w.~n', [Data, App, Env, Path]),
	translate_path_test(Data, Path),
	atom_codes(Area, AreaString),
	length(AreaString, AreaLength),
	Padding is MaxAreaLength + 3 - AreaLength,
	format(user_output, '~*c~w : ~w~n', [Padding, 32, Area, Path]),
	assertz(file_search_path(Area, Path)),
        fail
      ; nl.

max_area_length(MaxAreaLength) :-
	setof(Length,
	      Area^Data^AreaString^(define_path(Area, Data),
				    name(Area, AreaString),
				    length(AreaString,Length)),
	      AllLengths),
        last(AllLengths, MaxAreaLength).

make_list(Term, List) :-
	( Term = [_|_] ->
	  List = Term
	; List = [Term]
	).

translate_path_test(Data, Path) :-
	( translate_path(Data, Path) ->
	  true
	; format(user_output, '~nWARNING: Translation of ~w failed!!~n', [Data])
	).

translate_path(Path, TranslatedPath) :-
 	translate_path_1(Path, TranslatedPath, []).

translate_path_1(Path, TranslatedPath, RestPath) :-
	( Path = [H|T] ->
	  true
	; H = Path,
	  T = []
	),
	translate_path_2(T, H, TempTranslatedPath, RestPath),
	% RestPath = [],
	concat_atoms(TempTranslatedPath, TranslatedPath).

translate_path_2([], Last, TranslatedLast, Rest) :-
	translate_one_path_element(Last, TranslatedLast, Rest).
translate_path_2([Next|T], H, TranslatedH, TranslatedT) :-
	translate_one_path_element(H, TranslatedH, TranslatedNext),
	translate_path_2(T, Next, TranslatedNext, TranslatedT).

translate_one_path_element(path(Component), Path, RestPath) :-
	!,
	static_path_data(Component, ComponentPath),
	make_list(ComponentPath, [H|T]),
	translate_path_2(T, H, Path, RestPath).
translate_one_path_element(env(Component), [Path,'/'|RestPath], RestPath) :-
	!,
	environ(Component, Path).
translate_one_path_element(Component, [Component,'/'|RestPath], RestPath).

static_path_data(home,			env('GWAH')).
static_path_data(nls,			env('NLS')).
static_path_data(specialist,            [env('NLS'), 		specialist]).
static_path_data(specialist_devl,       [path(home), 		specialist]).
static_path_data(specialist_prod,       [path(nls), 		specialist]).
static_path_data(skr_src_home,          env('SKR_SRC_HOME')).

static_path_data(saw_prod,              [path(specialist_prod), 'SAW']).
static_path_data(saw_src_home,          env('SAW_SRC_HOME')).
static_path_data(usemrep,               [path(saw_src_home)]).
static_path_data(abgene,		[path(saw_src_home),	abgene]).

define_path(abgene,             path(abgene)).
define_path(usemrep_main,       [path(usemrep),  	 usemrep_main]).
define_path(usemrep_lib,        [path(usemrep),  	 usemrep_lib]).
define_path(usemrep_domain,     [path(usemrep),  	 usemrep_domain]).


% These path definitions are the ones used in use_module declarations
define_path(home,               env('GWAH')).
define_path(lexicon,            [path(skr_src_home), lexicon, lexicon]).
% define_path(lexicon,            [path(skr_src_home), .., lexicon, lexicon]).
% define_path(lexicon,            [path(skr_src_home), .., .., lexicon, lexicon]).
define_path(metamap,            [path(skr_src_home), metamap]).
% define_path(metamap,            [path(skr_src_home), .., metamap]).
% define_path(metamap,            [path(skr_src_home), .., .., metamap]).
define_path(morph,		[path(skr_src_home), lexicon, morph]).
% define_path(morph,		[path(skr_src_home), .., lexicon, morph]).
% define_path(morph,		[path(skr_src_home), .., .., lexicon, morph]).
define_path(mmi,                [path(skr_src_home), mmi]).
% define_path(mmi,                [path(skr_src_home), .., mmi]).
% define_path(mmi,                [path(skr_src_home), .., .., mmi]).
define_path(skr,                [path(skr_src_home), skr]).
% define_path(skr,                [path(skr_src_home), .., skr]).
% define_path(skr,                [path(skr_src_home), .., .., skr]).
define_path(skr_db,             [path(skr_src_home), db]).
% define_path(skr_db,             [path(skr_src_home), .., db]).
% define_path(skr_db,             [path(skr_src_home), .., .., db]).
define_path(skr_lib,            [path(skr_src_home), lib]).
% define_path(skr_lib,            [path(skr_src_home), .., lib]).
% define_path(skr_lib,            [path(skr_src_home), .., .., lib]).
define_path(tagger,             [path(skr_src_home), tagger]).
% define_path(tagger,             [path(skr_src_home), .., tagger]).
% define_path(tagger,             [path(skr_src_home), .., .., tagger]).
define_path(text,	    	[path(skr_src_home), text]).
% define_path(text,	    	[path(skr_src_home), .., text]).
% define_path(text,	    	[path(skr_src_home), .., .., text]).
define_path(wsd,                [path(skr_src_home), 'WSD/WSD']).
% define_path(wsd,                [path(skr_src_home), .., 'WSD/WSD']).
% define_path(wsd,                [path(skr_src_home), .., .., 'WSD/WSD']).
define_path(mm_tools_lib,	[path(skr_src_home), '../tools/lib']).

