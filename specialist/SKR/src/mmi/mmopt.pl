% File:     mmopt.pl
% Module:   MetaMap Optimzer
% Author:   Lan
% Purpose:  Optimizes MM output for use with MMI


:- module(mmopt,[
    go/0,
    go/1,
    go/2,
    filter_mappings_to_mesh/3
    ]).

:- use_module(skr_db(db_access),[
    initialize_db_access/0,
    get_meta_mesh/2,
    get_mesh_mh/2,
    stop_db_access/0
    ]).

:- use_module(skr_lib(nls_system), [
    get_control_options_for_modules/2,
    reset_control_options/1,
    toggle_control_options/1,
    display_control_options_for_modules/2,
    display_current_control_options/0,
    control_option/1,
    parse_command_line/1,
    interpret_options/4,
    interpret_args/4,
    get_from_iargs/4
    ]).

:- use_module(skr_lib(efficiency),[
    maybe_atom_gc/2
    ]).

:- use_module(skr_lib(nls_strings),[
    portray_strings_double_quoted/1
    ]).


:- use_module(library(lists),[
    rev/2
    ]).

:- use_module(library(files),[
    close_all_streams/0
    ]).

:- use_module(library(basics),[
    member/2
    ]).

:- use_module(library(addportray),[
    add_portray/1
    ]).



/* go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)
*/

go :-
    go(halt).

go(HaltOption) :-
    parse_command_line(CLTerm),
    go(HaltOption,CLTerm).

go(HaltOption,command_line(Options,Args)) :-
    add_portray(portray_strings_double_quoted),
    reset_control_options(mmopt),
    format('~nMetaMap Optimizer (2008)~n~n',[]),
    (initialize_mmopt(Options,Args,InterpretedArgs) ->
        mmopt(InterpretedArgs)
    ;   usage
    ),
    stop_mmopt,
    (HaltOption==halt ->
        halt
    ;   true
    ).


/* initialize_mmopt(+Options, +Args, -InterpretedArgs)
*/

initialize_mmopt(Options,Args,InterpretedArgs) :-
    get_control_options_for_modules([mmopt],AllOptions),
    interpret_options(Options,AllOptions,mmopt,IOptions),
    \+member(iopt(help,_),IOptions),
    ArgSpec=[aspec(infile,mandatory,file,read,no_default,
                   'Input file containing MetaMap output'),
             aspec(outfile,mandatory,file,write,no_default,
                   'Output file of optimized MetaMap output')
            ],
    interpret_args(IOptions,ArgSpec,Args,InterpretedArgs),
    toggle_control_options(IOptions),
    display_current_control_options,
    initialize_mmopt,
    !.

initialize_mmopt :-
    (control_option(restrict_to_mesh) ->
        initialize_db_access
    ;   true
    ),
    !.
initialize_mmopt :-
    format('~nERROR: initialize_mmopt/0 failed.~n',[]),
    !,
    fail.

stop_mmopt :-
    (control_option(restrict_to_mesh) ->
        stop_db_access
    ;   true
    ),
    close_all_streams.


/* usage
*/

usage :-
    format('~nUsage: mmopt [<options>] <input file> <output file>~n~n',[]),
    format('  <input file> contains MetaMap output,~n',[]),
    format('  and <output file> contains optimized MetaMap output.~n~n',[]),
    display_control_options_for_modules(mmopt,[]).


/* mmopt(+InterpretedArgs)
   mmopt(+InStream, +OutStream)
*/

mmopt(InterpretedArgs) :-
    get_from_iargs(infile,name,InterpretedArgs,InFile),
    get_from_iargs(infile,stream,InterpretedArgs,InStream),
    get_from_iargs(outfile,name,InterpretedArgs,OutFile),
    get_from_iargs(outfile,stream,InterpretedArgs,OutStream),
    format('~n~nBeginning to process ~a sending output to ~a.~n~n',
           [InFile,OutFile]),
    mmopt(InStream,OutStream),
    close(OutStream),
    close(InStream),
    format('~nBatch processing is finished.~n',[]).

mmopt(InStream,OutStream) :-
    (control_option(restrict_to_mesh) ->
        OptMode=mo
    ;   OptMode=opt
    ),
    % do not use portrayed(true)
    write_term(OutStream,opt_mode(OptMode),[quoted(true)]),
    format(OutStream,'.~n',[]),
    repeat,
    maybe_atom_gc(DidGC,SpaceCollected),
    ((control_option(info), DidGC==yes) ->
        format(user_output,'Atom GC performed collecting ~d bytes.~n',
               [SpaceCollected])
    ;   true
    ),
    ((read(InStream,InTerm), InTerm\==end_of_file) ->
        optimize_term(InTerm,OptMode,OutTerm),
        (OutTerm==no ->
            true
	    % do not use portrayed(true)
        ;   write_term(OutStream,OutTerm,[quoted(true)]),
            format(OutStream,'.~n',[])
        ),
        fail
    ;   true
    ).

optimize_term(InTerm,OptMode,OutTerm) :-
    (   InTerm=opt_mode(_) ->
        OutTerm=no

    ;   InTerm=candidates(_) ->
        OutTerm=candidates(OptMode)

    ;   InTerm=mesh_candidates(_) ->
        OutTerm=no

    ;   (InTerm=mappings(Mappings), control_option(restrict_to_mesh)) ->
        filter_mappings_to_mesh(Mappings,OptMode,OutMappings),
        OutTerm=mappings(OutMappings)

    ;   InTerm=mappings(Mappings) ->
        filter_mappings(Mappings,OptMode,OutMappings),
        OutTerm=mappings(OutMappings)

    ;   InTerm=mesh_mappings(_) ->
        OutTerm=no

    ;   InTerm=isol_mesh(_) ->
        OutTerm=no

    ;   OutTerm=InTerm
    ),
    !.

filter_mappings_to_mesh([],_,[]) :-
    !.
filter_mappings_to_mesh(Mappings,OptMode,Result) :-
    Mappings=[map(BestScore,_)|_],
    filter_each_best_mapping_to_mesh(Mappings,BestScore,OptMode,
                                     FilteredMappings0),
    (FilteredMappings0==[] ->
        Result=[]
    ;   sort(FilteredMappings0,FilteredMappings), % remove dups
        order_mappings_rev(FilteredMappings,RevOrderedFilteredMappings),
        RevResult=[OptMode|RevOrderedFilteredMappings],
        rev(RevResult,Result)
    ).

filter_each_best_mapping_to_mesh([],_,_,[]).
filter_each_best_mapping_to_mesh(Atom,_,_,_) :-
    atom(Atom),
    format(user_output,'~NFATAL ERROR: mmopt should not be used with MetaMap~n',
           []),
    format(user_output,'             that has already been optimized!~n',[]),
    !,
    stop_mmopt,
    halt.
filter_each_best_mapping_to_mesh([map(FirstScore,_)|_Rest],Score,_,[]) :-
    FirstScore=\=Score,
    !.
filter_each_best_mapping_to_mesh([First|Rest],Score,OptMode,Result) :-
    filter_mapping_to_mesh(First,OptMode,FilteredFirst),
    (FilteredFirst==null ->
        Result=FilteredRest
    ;   Result=[FilteredFirst|FilteredRest]
    ),
    filter_each_best_mapping_to_mesh(Rest,Score,OptMode,FilteredRest).

filter_mapping_to_mesh(map(Score,Evs),OptMode,Result) :-
    filter_evs_to_mesh(Evs,OptMode,FilteredEvs),
    (FilteredEvs==[] ->
        Result=null
    ;   Result=map(Score,FilteredEvs)
    ).

filter_evs_to_mesh([],_,[]).
filter_evs_to_mesh([First|Rest],OptMode,[FilteredFirst|FilteredRest]) :-
    First = ev(NegValue,_CUI,_MetaTerm,Concept,_MetaWords,_SemTypes,
	       MatchMap,_InvolvedHead,_IsOvermatch,SourceInfo,PosInfo),
    (control_option(strict_mesh) ->
        get_meta_mesh(Concept,MeSH)
    ;   (get_meta_mesh(Concept,MeSH) ->
            true
        ;   MeSH0=Concept,
            get_mesh_mh(MeSH0,MeSH)
        )
    ),
    FilteredFirst = ev(NegValue,OptMode,MeSH,Concept,OptMode,OptMode,
		       MatchMap,OptMode,OptMode,SourceInfo,PosInfo),
    filter_evs_to_mesh(Rest,OptMode,FilteredRest).
filter_evs_to_mesh([_First|Rest],OptMode,FilteredRest) :-
    filter_evs_to_mesh(Rest,OptMode,FilteredRest).

% the "best" mapping chosen is the first one with maximal number of components
% i.e., number of evs
%choose_a_best_mapping([First|Rest],Best) :-
%    First=map(_,FirstEvs),
%    length(FirstEvs,Max),
%    choose_a_best_mapping(Rest,First,Max,Best).
%
%choose_a_best_mapping([],Best,_,Best).
%choose_a_best_mapping([First|Rest],_CurrentBest,Max,Best) :-
%    First=map(_,FirstEvs),
%    length(FirstEvs,N),
%    N > Max,
%    !,
%    choose_a_best_mapping(Rest,First,N,Best).
%choose_a_best_mapping([_|Rest],CurrentBest,Max,Best) :-
%    choose_a_best_mapping(Rest,CurrentBest,Max,Best).

% rather than choosing one mapping with maximal number of components, simply
% order the mappings by the number of components (reverse order!)
order_mappings_rev([],[]) :-
    !.
order_mappings_rev(Mappings,RevOrderedMappings) :-
    augment_mappings(Mappings,1,AugMappings),
    sort(AugMappings,SortedAugMappings),
    deaugment_mappings(SortedAugMappings,RevOrderedMappings).

augment_mappings([],_,[]).
augment_mappings([map(Score,Evs)|Rest],N,[augmap(NComp,N,Score,Evs)|AugRest]) :-
    length(Evs,NComp),
    NewN is N + 1,
    augment_mappings(Rest,NewN,AugRest).

deaugment_mappings([],[]).
deaugment_mappings([augmap(_,_,Score,Evs)|Rest],[map(Score,Evs)|DeaugRest]) :-
    deaugment_mappings(Rest,DeaugRest).


% keep all best mappings, not just the first
filter_mappings([],_,[]) :-
    !.
filter_mappings(Mappings,OptMode,Result) :-
    Mappings=[map(BestScore,_)|_],
    filter_each_best_mapping(Mappings,BestScore,OptMode,FilteredMappings),
    (FilteredMappings==[] ->
        Result=[]
    ;   append(FilteredMappings,[OptMode],Result)
    ).

filter_each_best_mapping([],_,_,[]).
filter_each_best_mapping(Atom,_,_,_) :-
    atom(Atom),
    format(user_output,'~NFATAL ERROR: mmopt should not be used with MetaMap~n',
           []),
    format(user_output,'             that has already been optimized!~n',[]),
    !,
    stop_mmopt,
    halt.
filter_each_best_mapping([map(BestScore,Evs)|Rest],BestScore,OptMode,
                         [map(BestScore,FilteredEvs)|FilteredRest]) :-
    !,
    filter_evs(Evs,OptMode,FilteredEvs),
    filter_each_best_mapping(Rest,BestScore,OptMode,FilteredRest).
filter_each_best_mapping(_,_,_,[]).

filter_evs([],_,[]).
filter_evs([First|Rest],OptMode,[FilteredFirst|FilteredRest]) :-
    First = ev(NegValue,_CUI,_MetaTerm,Concept,_MetaWords,_Semtypes,
	       MatchMap,_InvolvesHead,_IsOvermatch,SourceInfo,PosInfo),
    FilteredFirst = ev(NegValue,OptMode,OptMode,Concept,OptMode,OptMode,
		       MatchMap,OptMode,OptMode,SourceInfo,PosInfo),
    filter_evs(Rest,OptMode,FilteredRest).

