
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

% File:     sgml_extractor.pl
% Module:   SGML Extractor
% Author:   Lan
% Purpose:  Parses SGML, extracts section fragments,
%           and generates labeled utterances for use with MetaMap/MMI.


:- module(sgml_extractor,[
    go/0,
    go/1,
    go/2,
    initialize_sgml_extractor/0,
%    extract_utterances_from_sgml/2,
    extract_fields_from_sgml/2
    ]).


:- use_module(skr(skr_text_processing),[
    parse_lines_into_utterances/2
%    label_and_format_utterances/5
    ]).


:- use_module(skr(skr_utilities), [
    get_program_name/1
   ]).

:- use_module(skr_db(db_access),[
	default_full_year/1
    ]).

:- use_module(skr_lib(nls_system), [
    get_control_options_for_modules/2,
    reset_control_options/1,
    toggle_control_options/1,
    display_control_options_for_modules/2,
    display_current_control_options/2,
    control_option/1,
    parse_command_line/1,
    interpret_options/4,
    interpret_args/4,
    get_from_iargs/4
    ]).

:- use_module(skr_lib(nls_lists),[
    first_n_or_less/3
    ]).

:- use_module(skr_lib(nls_strings),[
    portray_strings_double_quoted/1,
    split_string/4,
    split_string_completely/3,
    trim_whitespace/2
    ]).

:- use_module(skr_lib(sicstus_utils),[
    ttyflush/0
    ]).


:- use_module(lexicon(lexical),[
    concatenate_strings/3
    ]).

:- use_module(library(addportray),[
    add_portray/1
    ]).

:- use_module(library(caseconv),[
    lower/2
    ]).

:- use_module(library(ctypes),[
    is_print/1
    ]).

:- use_module(library(lineio),[
    fget_line/2
    ]).

:- use_module(library(basics),[
    member/2,
    memberchk/2
    ]).

:- use_module(library(lists),[
    append/2,
    is_list/1,
    rev/2
    ]).

:- dynamic tag_counts/2.

/* go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)

go/0 is the executive predicate for SGML Extractor.
go/0 uses go/1 with HaltFlag set to halt.
go/1 parses the command line and calls go/2 which controls the processing.  */

go :-
    go(halt).

go(HaltOption) :-
    parse_command_line(CLTerm),
    go(HaltOption,CLTerm).

go(HaltOption,command_line(Options,Args)) :-
    add_portray(portray_strings_double_quoted),
    reset_control_options(sgml_extractor),
    get_program_name(ProgramName),
    default_full_year(FullYear),
    format('~nSGML Extractor~n~n',[]),
    (initialize_sgml_extractor(Options, Args, ProgramName, FullYear, InterpretedArgs) ->
        (sgml_extractor(InterpretedArgs); true)
    ;   usage
    ),
    (HaltOption==halt ->
        halt
    ;   true
    ).


/* initialize_sgml_extractor(+Options, +Args, +ProgramName, +FullYear, -InterpretedArgs)
   initialize_sgml_extractor

initialize_sgml_extractor/5 interprets command line options and arguments
(opening files as necessary) and, sets and displays the SGML Extractor control
options discovered, and then calls initialize_sgml_extractor/0.  It returns
InterpretedArgs for later use (e.g., the stream associated with a file).
initialize_sgml_extractor/0 initializes dynamic predicates it uses. */

initialize_sgml_extractor(Options, Args, ProgramName, FullYear, InterpretedArgs) :-
	get_control_options_for_modules([sgml_extractor], AllOptions),
	interpret_options(Options,AllOptions, sgml_extractor, IOptions),
	\+member(iopt(help,_), IOptions),
	ArgSpec = [aspec(sgml_file,mandatory,file,read,no_default,
			 'Input file containing SGML-delimited text'),
		   aspec(main_outfile,mandatory,file,write,no_default,
			 'Main output file for section fragments')
		  ],
	interpret_args(IOptions, ArgSpec, Args, InterpretedArgs),
	toggle_control_options(IOptions),
	display_current_control_options(ProgramName, FullYear),
	initialize_sgml_extractor.

initialize_sgml_extractor :-
    retractall(tag_counts(_,_)),
    !.


/* usage

usage/0 displays SGML Extractor usage.  */

usage :-
    format('~nUsage: sgml_extractor [<options>] <SGML file> <main output file>~n~n',[]),
    format('  <SGML file> contains SGML-delimited text, and~n',[]),
    format('  <main output file> contains section fragments.~n',[]),
    format('  For other files, see options -l and -t below.~n~n',[]),
    display_control_options_for_modules(sgml_extractor,[]).


/* sgml_extractor(+InterpretedArgs)
   sgml_extractor(+CitationStream, +MMOutputStream, +SummaryStream)

sgml_extractor/1 controls all processing.  It gets input and output file names
from InterpretedArgs and calls sgml_extractor/3 to complete the processing.  */

sgml_extractor(InterpretedArgs) :-
    get_from_iargs(sgml_file,name,InterpretedArgs,SGMLFile),
    get_from_iargs(sgml_file,stream,InterpretedArgs,SGMLStream),
    get_from_iargs(main_outfile,name,InterpretedArgs,MainOutFile),
    get_from_iargs(main_outfile,stream,InterpretedArgs,MainOutStream),
    format('~n~nBeginning to process ~a producing file ~a.~n',
           [SGMLFile,MainOutFile]),
    (get_from_iargs(labeled_utterance_file,name,InterpretedArgs,UttFile) ->
        get_from_iargs(labeled_utterance_file,stream,InterpretedArgs,UttStream),
        format('Writing labeled utterances to ~a.~n',[UttFile])
    ;   UttStream=none
    ),
    (get_from_iargs(tag_summary_file,name,InterpretedArgs,SummaryFile) ->
        get_from_iargs(tag_summary_file,stream,InterpretedArgs,SummaryStream),
        format('Writing tag summary to ~a.~n',[SummaryFile])
    ;   SummaryStream=none
    ),
    format('~n',[]),
    sgml_extractor(SGMLStream,MainOutFile,MainOutStream,UttStream,
                   SummaryStream),
    (SummaryStream==none ->
        true
    ;   close(SummaryStream)
    ),
    (UttStream==none ->
        true
    ;   close(UttStream)
    ),
    % MainOutStream has already been closed
    close(SGMLStream),
    format('~nBatch processing is finished.~n~n',[]).

% temp clause for testing
%sgml_extractor(SGMLStream,_,OutStream,_,_) :-
%    fget_all_non_null_lines(SGMLStream,Lines),
%    format(OutStream,'~p~n',[Lines]),
%    extract_utterances_from_sgml(Lines,Utterances),
%    format(OutStream,'~p~n',[Utterances]).
sgml_extractor(SGMLStream,MainOutFile,MainOutStream,UttStream,SummaryStream) :-
    parse_sgml(SGMLStream,ParsedSGML),
    format('SGML parsing is complete.~n',[]),
    ttyflush,
    (control_option(dump_parsed_sgml) ->
        format(MainOutStream,'~nParsed SGML:~n',[]),
        dump_sgml(ParsedSGML,MainOutStream),
        format('Parsed SGML has been dumped.~n',[]),
        ttyflush
    ;   true
    ),
    (SummaryStream==none ->
        true
    ;   write_tag_summary(ParsedSGML,SummaryStream),
        format('SGML tag summary has been created.~n',[]),
        ttyflush,
        close(SummaryStream)
    ),
    modify_sgml(ParsedSGML,ModifiedSGML),
    format('SGML modification is complete.~n',[]),
    ttyflush,
    (control_option(dump_modified_sgml) ->
        format(MainOutStream,'~nModified SGML:~n',[]),
        dump_sgml(ModifiedSGML,MainOutStream),
        format('Modified SGML has been dumped.~n',[]),
        ttyflush
    ;   true
    ),
    (control_option(dump_tag_sequences) ->
        format(MainOutStream,'~nTag sequences:~n',[]),
        dump_tag_sequences(ModifiedSGML,MainOutStream),
        format('Tag sequences have been dumped.~n',[]),
        ttyflush
    ;   true
    ),
    format(MainOutStream,'~nSection fragments:~n',[]),
    atom_codes(MainOutFile,MainOutFileString),
    split_string(MainOutFileString,".",FilePrefix,_),
    write_section_fragments(ModifiedSGML,FilePrefix,MainOutStream),
    format('Section fragments have been written.~n',[]),
    ttyflush,
    close(MainOutStream),
    (UttStream==none ->
        true
    ;   open(MainOutFile,read,FragmentStream),
        write_utts(FragmentStream,UttStream),
        format('Labeled utterances have been written.~n',[]),
        ttyflush,
        close(FragmentStream)
    ),
    !.
sgml_extractor(_,_,_,_,_) :-
    format('ERROR: sgml_extractor/5 failed.~n',[]).

%extract_utterances_from_sgml(Lines,Utterances) :-
%    modify_lines(Lines,NumberedLines),
%    parse_sgml(0:"",_:_LineOut,NumberedLines,_LinesOut,[],"NoTag","notag",
%	      "notag",[],_TagsOut,[],RevParsedSGML),
%    rev(RevParsedSGML,ParsedSGML),
%    modify_sgml(ParsedSGML,ModifiedSGML),
%    extract_and_form_utterances(ModifiedSGML,Utterances).

extract_fields_from_sgml(Lines,Fields) :-
    modify_lines(Lines,NumberedLines),
% temp
%format('NumberedLines:~n~p~n~n',[NumberedLines]),
    parse_sgml(0:"",_:_LineOut,NumberedLines,_LinesOut,[],"NoTag","notag",
	      "notag",[],_TagsOut,[],RevParsedSGML),
    rev(RevParsedSGML,ParsedSGML),
% temp
%format('ParsedSGML:~n~p~n~n',[ParsedSGML]),
    modify_sgml(ParsedSGML,ModifiedSGML),
% temp
%format('ModifiedSGML:~n~p~n~n',[ModifiedSGML]),
    extract_fields(ModifiedSGML,Fields).

extract_fields(SGML,Fields) :-
    extract_fields(SGML,"DOC",[],Fields0),
    rev(Fields0,Fields).

extract_fields([],_,FieldsIn,FieldsIn) :-
    !.
extract_fields([tag(_,FieldID,_,SGML)|Rest],FieldIDIn,FieldsIn,FieldsOut) :-
    !,
    extract_fields(SGML,FieldID,FieldsIn,FieldsInOut),
    extract_fields(Rest,FieldIDIn,FieldsInOut,FieldsOut).
extract_fields([text(Text)|Rest],FieldIDIn,FieldsIn,FieldsOut) :-
    !,
    extract_fields(Rest,FieldIDIn,[[FieldIDIn,Text]|FieldsIn],FieldsOut).
extract_fields([_Unknown|Rest],FieldIDIn,FieldsIn,FieldsOut) :-
    extract_fields(Rest,FieldIDIn,FieldsIn,FieldsOut).

modify_lines(Lines,NumberedLines) :-
    modify_lines(Lines,1,NumberedLines).

modify_lines([],_,[]).
modify_lines([First0|Rest],N,[N:First|NumberedRest]) :-
    replace_non_print_chars(First0,N,First1),
    trim_whitespace(First1,First),
    NewN is N + 1,
    modify_lines(Rest,NewN,NumberedRest).


%extract_and_form_utterances(ModifiedSGML,Utterances) :-
%    % combine write_section_fragments/3 and write_utts/2
%    extract_section_fragments(ModifiedSGML,SFrags),
%% temp
%format('SFrags:~n~p~n~n',[SFrags]),
%    form_utterances_from_sfrags(SFrags,Utterances).

%extract_section_fragments(SGML,SFrags) :-
%    extract_section_fragments(SGML,1,[],[],SFrags).
%
%extract_section_fragments([],_,_,_,[]) :-
%    !.
%extract_section_fragments([First|Rest],N,RevSec,RevLabel,Results) :-
%    !,
%    extract_section_fragments(First,N,RevSec,RevLabel,ExtractedFirst),
%    NewN is N + 1,
%    (ExtractedFirst==[] ->
%	Results=ExtractedRest
%    ;   append(ExtractedFirst,ExtractedRest,Results)
%    ),
%    extract_section_fragments(Rest,NewN,RevSec,RevLabel,ExtractedRest).
%extract_section_fragments(tag(SGMLType,Tag0,Tag,SGML),N,RevSec,RevLabel,
%			  Results) :-
%    !,
%    ((SGMLType==ue; SGMLType==se; SGMLType==uc) ->
%        % unscoped end tag, scoped end tag with no beginning tag, or comment
%        (SGML==[] ->
%            Results=[]
%        ;   format('ERROR: End/comment tag ~s has non-null scope: ~p~n',
%                   [Tag0,SGML])
%        )
%    ;   % all others (begin tags)
%        compute_simple_tag(Tag,SimpleTag),
%        (section_fragment_tag(SimpleTag) ->
%            extract_section_fragments(SGML,1,[N|RevSec],[SimpleTag|RevLabel],
%				      Results)
%        ;   extract_sfrag_text(SGML,[N|RevSec],RevLabel,Results)
%        )
%    ).
%extract_section_fragments(text(Text),N,RevSec,RevLabel,
%			  [sfrag(Sec,Label,Text)]) :-
%    !,
%    rev([N|RevSec],Sec),
%    rev(RevLabel,Label).
    

%extract_sfrag_text(SGML,RevSec,RevLabel,[sfrag(Sec,Label,Text)]) :-
%    rev(RevSec,Sec),
%    rev(RevLabel,Label),
%    extract_all_text(SGML,SGMLText0),
%    merge_adjacent_text(SGMLText0,SGMLText),
%    (SGMLText=[text(Text)] ->
%        true
%    ;   format('ERROR: extract_sfrag_text failed for ~p~n',[SGML])
%    ).


%form_utterances_from_sfrags([],[]).
%form_utterances_from_sfrags([sfrag(_Sec,Label0,Lines)|Rest],Utterances) :-
%    parse_lines_into_utterances(Lines,Utterances0),
%    concatenate_strings(Label0,".",Label),
%    label_and_format_utterances(Label,1,_LastN,Utterances0,FirstUtterances),
%    append(FirstUtterances,RestUtterances,Utterances),
%    form_utterances_from_sfrags(Rest,RestUtterances).


parse_sgml(SGMLStream,ParsedSGML) :-
    read_sgml(SGMLStream,1,Lines),
    parse_sgml(0:"",_:_LineOut,Lines,_LinesOut,[],"NoTag","notag","notag",
               [],_TagsOut,[],RevParsedSGML),
    !,
    rev(RevParsedSGML,ParsedSGML).

read_sgml(SGMLStream,N,[N:Line|Rest]) :-
    fget_line(SGMLStream,Line0),
    !,
    replace_non_print_chars(Line0,N,Line1),
    trim_whitespace(Line1,Line),
    NewN is N+1,
    read_sgml(SGMLStream,NewN,Rest).
read_sgml(_,_,[]).

replace_non_print_chars([],_,[]).
replace_non_print_chars([Char|Rest],N,[Char|ReplacedRest]) :-  % print -> self
    is_print(Char),
    !,
    replace_non_print_chars(Rest,N,ReplacedRest).
replace_non_print_chars([8'11|Rest],N,[0' |ReplacedRest]) :-    % tab -> space
    !,
    (control_option(warnings) ->
        format('~NWARNING: Replacing tab with space at line ~d~n',[N])
    ;   true
    ),
    replace_non_print_chars(Rest,N,ReplacedRest).
replace_non_print_chars([8'177|Rest],N,ReplacedRest) :-  % \177 (DEL) -> <null>
    !,
    (control_option(warnings) ->
        format('~NWARNING: Removing \177 (DEL) from line ~d~n',[N])
    ;   true
    ),
    replace_non_print_chars(Rest,N,ReplacedRest).
replace_non_print_chars([8'202|Rest],N,[0'e|ReplacedRest]) :-      % \202 -> e
    !,
    (control_option(warnings) ->
        format('~NWARNING: Replacing \202 with e at line ~d~n',[N])
    ;   true
    ),
    replace_non_print_chars(Rest,N,ReplacedRest).
replace_non_print_chars([8'204|Rest],N,[0'a|ReplacedRest]) :-      % \204 -> a
    !,
    (control_option(warnings) ->
        format('~NWARNING: Replacing \204 with a at line ~d~n',[N])
    ;   true
    ),
    replace_non_print_chars(Rest,N,ReplacedRest).
replace_non_print_chars([8'363|Rest],N,[0'o|ReplacedRest]) :-      % \363 -> o
    !,
    (control_option(warnings) ->
        format('~NWARNING: Replacing \363 with o at line ~d~n',[N])
    ;   true
    ),
    replace_non_print_chars(Rest,N,ReplacedRest).
replace_non_print_chars([Char|Rest],N,ReplacedRest) :-     % others -> <null>
    !,
    (control_option(warnings) ->
        format('~NWARNING: Removing non-print character \~8r from line ~d~n',
               [Char,N])
    ;   true
    ),
    replace_non_print_chars(Rest,N,ReplacedRest).


% grab the next input line
parse_sgml(_:"",O:LineOut,[N:Line|Rest],LinesOut,TextIn,BeginTag0,BeginTag,
           EndTag,TagsIn,TagsOut,SGMLIn,SGMLOut) :-
    !,
    parse_sgml(N:Line,O:LineOut,Rest,LinesOut,TextIn,BeginTag0,BeginTag,EndTag,
               TagsIn,TagsOut,SGMLIn,SGMLOut).
% no more input
parse_sgml(N:"",N:"",[],[],TextIn,BeginTag0,BeginTag,_EndTag,
           TagsIn,TagsOut,SGMLIn,SGMLOut) :-
    (BeginTag=="notag" ->
        (TextIn==[] ->
            SGMLOut=SGMLIn
        ;   rev(TextIn,Text),
            SGMLOut=[text(Text)|SGMLIn]
        )
    ;   (control_option(warnings) ->
            format('~NWARNING: Out of input while processing <~s>~n',
                   [BeginTag0])
        ;   true
        ),
        % premature, but try anyway
        (TagsIn==[] ->
            TagsOut=[]
        ;   TagsIn=[_|TagsOut]
        ),
        rev(TextIn,Text),
        (Text==[] ->
            rev(SGMLIn,SGML)
        ;   rev([text(Text)|SGMLIn],SGML)
        ),
        SGMLOut=[tag(sb,BeginTag0,BeginTag,SGML)]   % missing end tag
    ),
    !.
% handle end tags
parse_sgml(N:Line,O:LineOut,LinesIn,LinesOut,TextIn,BeginTag0,BeginTag,EndTag,
           TagsIn,TagsOut,SGMLIn,SGMLOut) :-
    split_tag(Line,Left,end,Tag0,Tag,Right),
    !,
    (   Tag=EndTag ->                      % normal ending
        (Left=="" ->
            rev(TextIn,Text)
        ;   rev([Left|TextIn],Text)
        ),
        (Text==[] ->
            rev(SGMLIn,SGML)
        ;   rev([text(Text)|SGMLIn],SGML)
        ),
        O=N,
        LineOut=Right,
        LinesOut=LinesIn,
        (TagsIn==[] ->
            TagsOut=[]
        ;   TagsIn=[_|TagsOut]
        ),
        SGMLOut=[tag(s,BeginTag0,BeginTag,SGML)]
    ;   unscoped_tag(Tag) ->               % ending unscoped tag
        (Left=="" ->
            rev(TextIn,Text)
        ;   rev([Left|TextIn],Text)
        ),
        (Text==[] ->
            SGMLInOut=[tag(ue,Tag0,Tag,[])|SGMLIn]
        ;   SGMLInOut=[tag(ue,Tag0,Tag,[]),text(Text)|SGMLIn]
        ),
        parse_sgml(N:Right,O:LineOut,LinesIn,LinesOut,[],
                   BeginTag0,BeginTag,EndTag,TagsIn,TagsOut,SGMLInOut,SGMLOut)
    ;   \+memberchk(Tag,TagsIn) ->         % ending scoped tag without beginning
        (control_option(warnings) ->
            format('~NWARNING: Found </~s> with missing <~s> (line ~d)~n',
                   [Tag,Tag,N])
        ;   true
        ),
        (Left=="" ->
            rev(TextIn,Text)
        ;   rev([Left|TextIn],Text)
        ),
        (Text==[] ->
            SGMLInOut=[tag(se,Tag0,Tag,[])|SGMLIn]
        ;   SGMLInOut=[tag(se,Tag0,Tag,[]),text(Text)|SGMLIn]
        ),
        parse_sgml(N:Right,O:LineOut,LinesIn,LinesOut,[],
                   BeginTag0,BeginTag,EndTag,TagsIn,TagsOut,SGMLInOut,SGMLOut)
    ;   length(LinesIn,NRemaining),
        (NRemaining < 10 ->
            (control_option(warnings) ->       % force completion if near end
                format('~NWARNING: Forcing completion of </~s> (line ~d)~n',
                       [EndTag,N])
            ;   true
            ),
            (Left=="" ->
                rev(TextIn,Text)
            ;   rev([Left|TextIn],Text)
            ),
            (Text==[] ->
                rev(SGMLIn,SGML)
            ;   rev([text(Text)|SGMLIn],SGML)
            ),
            O=N,
            LineOut=Right,
            LinesOut=LinesIn,
            TagsOut=TagsIn,
            SGMLOut=[tag(sb,BeginTag0,BeginTag,SGML)]
        ;   (control_option(warnings) ->       % bad ending; backtrack
                format('~NWARNING: Found </~s> looking for </~s> (line ~d)~n',
                       [Tag,EndTag,N])
            ;   true
            ),
            fail
        )
    ),
    !.
% first handle (scoped) begin tags assuming balancing end tags
parse_sgml(N:Line,O:LineOut,LinesIn,LinesOut,TextIn,BeginTag0,BeginTag,EndTag,
           TagsIn,TagsOut,SGMLIn,SGMLOut) :-
    split_tag(Line,Left,begin,Tag0,Tag,Right),
    compute_simple_tag(Tag,NewEndTag),
    \+unscoped_tag(NewEndTag),
% try avoiding this for now
%    (bad_nesting(NewEndTag,TagsIn) ->
%        (control_option(warnings) ->
%            format('~NWARNING: Found "~s" nested below ~p (line ~d)~n',
%                   [NewEndTag,TagsIn,N])
%        ;   true
%        ),
%        fail
%    ;   true
%    ),
    ((NewEndTag=="head", EndTag=="body") ->
        % special case: close body when new head is found
        (control_option(warnings) ->
            format('~NWARNING: Found <head> nested below <body>~n',[])
        ;   true
        ),
        (Left=="" ->
            rev(TextIn,Text)
        ;   rev([Left|TextIn],Text)
        ),
        (Text==[] ->
            rev(SGMLIn,SGML)
        ;   rev([text(Text)|SGMLIn],SGML)
        ),
        CurrentSGML=[tag(sb,BeginTag0,BeginTag,SGML)],
        (TagsIn==[] ->
            TagsInOut=[]
        ;   TagsIn=[_|TagsInOut]
        ),
        parse_sgml(N:Right,O:LineOut,LinesIn,LinesOut,[],
                   Tag0,Tag,NewEndTag,[NewEndTag|TagsInOut],TagsOut,[],NewSGML),
        append(NewSGML,CurrentSGML,SGMLOut)
    ;   % normal begin tag
        (Left=="" ->
            rev(TextIn,Text)
        ;   rev([Left|TextIn],Text)
        ),
        parse_sgml(N:Right,NIO:LineInOut,LinesIn,LinesInOut,[],
                   Tag0,Tag,NewEndTag,[NewEndTag|TagsIn],TagsInOut,[],NewSGML),
        (Text==[] ->
            append(NewSGML,SGMLIn,SGMLInOut)
        ;   append([NewSGML,[text(Text)],SGMLIn],SGMLInOut)
        ),
        parse_sgml(NIO:LineInOut,O:LineOut,LinesInOut,LinesOut,[],
                   BeginTag0,BeginTag,EndTag,TagsInOut,TagsOut,
                   SGMLInOut,SGMLOut)
    ).
% then handle begin tags as if they are unscoped
parse_sgml(N:Line,O:LineOut,LinesIn,LinesOut,TextIn,BeginTag0,BeginTag,EndTag,
           TagsIn,TagsOut,SGMLIn,SGMLOut) :-
    split_tag(Line,Left,begin,Tag0,Tag,Right),
    compute_simple_tag(Tag,NewEndTag),
    !,
    ((\+unscoped_tag(NewEndTag), control_option(warnings)) ->
        format('~NWARNING: Treating "~s" as unscoped (line ~d)~n',[NewEndTag,N])
    ;   true
    ),
    (Left=="" ->
        rev(TextIn,Text)
    ;   rev([Left|TextIn],Text)
    ),
    (unscoped_tag(NewEndTag) ->
        (Text==[] ->
            SGMLInOut=[tag(ub,Tag0,Tag,[])|SGMLIn]
        ;   SGMLInOut=[tag(ub,Tag0,Tag,[]),text(Text)|SGMLIn]
        )
    ;   (Text==[] ->            % signal missing end tag
            SGMLInOut=[tag(sb,Tag0,Tag,[])|SGMLIn]
        ;   SGMLInOut=[tag(sb,Tag0,Tag,[]),text(Text)|SGMLIn]
        )
    ),
    parse_sgml(N:Right,O:LineOut,LinesIn,LinesOut,[],
               BeginTag0,BeginTag,EndTag,TagsIn,TagsOut,SGMLInOut,SGMLOut).
%  comments
parse_sgml(N:Line,O:LineOut,LinesIn,LinesOut,TextIn,BeginTag0,BeginTag,EndTag,
           TagsIn,TagsOut,SGMLIn,SGMLOut) :-
    split_tag(Line,Left,comment,Tag0,Tag,Right),
    !,
    (Left=="" ->
        rev(TextIn,Text)
    ;   rev([Left|TextIn],Text)
    ),
    (Text==[] ->
        SGMLInOut=[tag(uc,Tag0,Tag,[])|SGMLIn]
    ;   SGMLInOut=[tag(uc,Tag0,Tag,[]),text(Text)|SGMLIn]
    ),
    parse_sgml(N:Right,O:LineOut,LinesIn,LinesOut,[],
               BeginTag0,BeginTag,EndTag,TagsIn,TagsOut,SGMLInOut,SGMLOut).
% possible split tag, i.e., a tag split across two or more lines
parse_sgml(_:Line,O:LineOut,[N:NextLine|Rest],LinesOut,TextIn,
           BeginTag0,BeginTag,EndTag,TagsIn,TagsOut,SGMLIn,SGMLOut) :-
    memberchk(0'<,Line),
    !,
    append([Line," ",NextLine],NewLine),
    parse_sgml(N:NewLine,O:LineOut,Rest,LinesOut,TextIn,
               BeginTag0,BeginTag,EndTag,TagsIn,TagsOut,SGMLIn,SGMLOut).
% no tags on this line; keep on going
parse_sgml(N:Line,O:LineOut,LinesIn,LinesOut,TextIn,BeginTag0,BeginTag,EndTag,
           TagsIn,TagsOut,SGMLIn,SGMLOut) :-
    parse_sgml(N:"",O:LineOut,LinesIn,LinesOut,[Line|TextIn],
               BeginTag0,BeginTag,EndTag,TagsIn,TagsOut,SGMLIn,SGMLOut).

/* unscoped_tag(?Tag)

unscoped_tag/1 is a factual predicate of tags which are to be considered
unscoped.  This includes many tags which normally occur with end tags
but may not (<a> and <tr> are good examples).  Any tag treated as unscoped
occurs in parsed SGML (as does an end tag if it occurs), but it has no
scope.  Thus these tags cannot contribute to a structural description
of text.  */

% "unary" tags
unscoped_tag("br") :- !.
unscoped_tag("hr") :- !.
unscoped_tag("img") :- !.
unscoped_tag("meta") :- !.  % new, maybe temporary
unscoped_tag([0'!|_]) :- !.
% other unscoped tags
unscoped_tag("a") :- !.
unscoped_tag("area") :- !.
unscoped_tag("atl") :- !.
unscoped_tag("b") :- !.
unscoped_tag("cit") :- !.
unscoped_tag("dd") :- !.
unscoped_tag("dl") :- !.
unscoped_tag("dt") :- !.
unscoped_tag("i") :- !.
unscoped_tag("input") :- !.
unscoped_tag("li") :- !.
unscoped_tag("ni") :- !.
unscoped_tag("p") :- !.
unscoped_tag("td") :- !.
unscoped_tag("th") :- !.
unscoped_tag("tip") :- !.
unscoped_tag("tr") :- !.


%bad_nesting(Tag,_Tags) :-
%    nestable(Tag),
%    !,
%    fail.
%bad_nesting(Tag,Tags) :-
%    memberchk(Tag,Tags),
%    !.
%
%nestable("table").


split_tag(String,Left,TagType,Tag1,Tag,Right) :-
    append([Left0,"<",Tag0,">",Right0],String),
    \+memberchk(0'<,Tag0),
    \+memberchk(0'>,Tag0),
    !,
    (   Tag0=[0'/|Tag1] ->            % end tag
        TagType=end
    ;   Tag0=[0'!,0'-,0'-|Tag1] ->    % comment: <!-- ... >
        TagType=comment
    ;   Tag1=Tag0,                    % begin tag
        TagType=begin
    ),
    lower(Tag1,Tag),
    trim_whitespace(Left0,Left),
    trim_whitespace(Right0,Right).


compute_simple_tag(Tag,SimpleTag) :-
    append([SimpleTag," ",_],Tag),
    !.
compute_simple_tag(Tag,Tag).


/* modify_sgml(+SGMLIn, -SGMLOut)

modify_sgml/2 modifies SGMLIn in several ways (see below) producing SGMLOut.  */

modify_sgml(SGML,ModifiedSGML) :-
    remove_unwanted_elements(SGML,SGML1),
    replace_all_special_characters(SGML1,SGML2),
    promote_first_body_elements(SGML2,SGML3),
    simplify_tagging(SGML3,SGML4),
    merge_adjacent_text(SGML4,ModifiedSGML).


/* remove_unwanted_elements(+SGMLIn, -SGMLOut)

remove_unwanted_elements/2 completely removes SGML elements with tags
satisfying unwanted_tags/1.  */

remove_unwanted_elements([],[]) :-
    !.
% unwanted tags
remove_unwanted_elements([tag(_,_,Tag,_)|Rest],ModifiedRest) :-
    compute_simple_tag(Tag,SimpleTag),
    unwanted_tag(SimpleTag),
    !,
    remove_unwanted_elements(Rest,ModifiedRest).
% other tags
remove_unwanted_elements([tag(SGMLType,Tag0,Tag,SGML)|Rest],
                         [tag(SGMLType,Tag0,Tag,ModifiedSGML)|ModifiedRest]) :-
    !,
    remove_unwanted_elements(SGML,ModifiedSGML),
    remove_unwanted_elements(Rest,ModifiedRest).
% everything else
remove_unwanted_elements([First|Rest],[First|ModifiedRest]) :-
    remove_unwanted_elements(Rest,ModifiedRest).

% Make sure these tags are NOT unscoped
unwanted_tag("ack") :- !.
unwanted_tag("bb") :- !.
unwanted_tag("bibl") :- !.
unwanted_tag("bm") :- !.
unwanted_tag("disc") :- !.
unwanted_tag("dscl") :- !.
unwanted_tag("fn") :- !.
unwanted_tag("nl") :- !.
unwanted_tag("pnl") :- !.


/* replace_all_special_characters(+SGMLIn, -SGMLOut)

replace_all_special_characters/2 replaces text of the form "&...;" with
a "normal" string according to replace_special_character/2.  */

replace_all_special_characters([],[]) :-
    !.
replace_all_special_characters([First|Rest],[ModifiedFirst|ModifiedRest]) :-
    !,
    replace_all_special_characters(First,ModifiedFirst),
    replace_all_special_characters(Rest,ModifiedRest).
replace_all_special_characters(tag(SGMLType,Tag0,Tag,SGML),
                               tag(SGMLType,Tag0,Tag,ModifiedSGML)) :-
    !,
    replace_all_special_characters(SGML,ModifiedSGML).
replace_all_special_characters(text(Text),text(ModifiedText)) :-
    replace_all_special_characters_aux(Text,ModifiedText).

replace_all_special_characters_aux([],[]) :-
    !.
replace_all_special_characters_aux([First|Rest],[ModifiedFirst|ModifiedRest]) :-
    !,
    replace_special_characters(First,ModifiedFirst),
    replace_all_special_characters_aux(Rest,ModifiedRest).

replace_special_characters(LineIn,LineOut) :-
    append([Left,"&",Special,";",Right],LineIn),
    !,
    replace_special_character(Special,ReplacedSpecial),
    append([Left,ReplacedSpecial,ReplacedRight],LineOut),
    replace_special_characters(Right,ReplacedRight).
replace_special_characters(LineIn,LineIn).

replace_special_character("#150"," ") :- !.
replace_special_character("#173","-") :- !.
replace_special_character("#38","&") :- !.
replace_special_character(">ndash","-") :- !.
replace_special_character("I","") :- !.
replace_special_character("M","") :- !.
replace_special_character("Oslash","O") :- !.
replace_special_character("Ouml","O") :- !.
replace_special_character("Sigma","Sigma") :- !.
replace_special_character("aelig","ae") :- !.
replace_special_character("agrave","a") :- !.
replace_special_character("alpha","alpha") :- !.
replace_special_character("amp","&") :- !.
replace_special_character("aring","a") :- !.
replace_special_character("auml","a") :- !.
replace_special_character("beta","beta") :- !.
replace_special_character("ccedil","C") :- !.
replace_special_character("dagger",".") :- !.
replace_special_character("degree"," degree") :- !.
replace_special_character("eacute","e") :- !.
replace_special_character("egrave","e") :- !.
replace_special_character("el"," ") :- !.
replace_special_character("em"," ") :- !.
replace_special_character("emdash","--") :- !.
replace_special_character("eq","=") :- !.
replace_special_character("euml","e") :- !.
replace_special_character("ge",">=") :- !.
replace_special_character("grave","") :- !.
replace_special_character("gt",">") :- !.
replace_special_character("gte",">=") :- !.
replace_special_character("half"," half") :- !.
replace_special_character("le","<=") :- !.
replace_special_character("lt","<") :- !.
replace_special_character("lte","<=") :- !.
replace_special_character("mdash","--") :- !.
replace_special_character("minus","-") :- !.
replace_special_character("mu","mu") :- !.
replace_special_character("mult","*") :- !.
replace_special_character("nacute","n") :- !.
replace_special_character("nbsp","n") :- !.
replace_special_character("ndash","-") :- !.
replace_special_character("ocirc","o") :- !.
replace_special_character("oslash","o") :- !.
replace_special_character("ouml","o") :- !.
replace_special_character("para",".") :- !.
replace_special_character("plusmn","+/-") :- !.
replace_special_character("pm","about") :- !.
replace_special_character("quot","""") :- !.
replace_special_character("rad","squareroot") :- !.
replace_special_character("reg","") :- !.
replace_special_character("sec","Section") :- !.
replace_special_character("sect","Section") :- !.
replace_special_character("sol","") :- !.
replace_special_character("times","*") :- !.
replace_special_character("uml","") :- !.
replace_special_character("uuml","u") :- !.
replace_special_character("zeta","zeta") :- !.
replace_special_character(SC,SC).


/* promote_first_body_elements(+SGMLIn, -SGMLOut)

promote_first_body_elements/2 replaces the first subelement of each <body>
with an <h0> element if the subelement begins with either <p> or <em>.  */

promote_first_body_elements([],[]) :-
    !.
promote_first_body_elements([First|Rest],[PromotedFirst|PromotedRest]) :-
    promote_first_body_elements(First,PromotedFirst),
    promote_first_body_elements(Rest,PromotedRest).
promote_first_body_elements(tag(SGMLType,Tag0,"body",SGML),
                            tag(SGMLType,Tag0,"body",PromotedSGML)) :-
    !,
    promote_first_element(SGML,PromotedSGML).
promote_first_body_elements(tag(SGMLType,Tag0,Tag,SGML),
                            tag(SGMLType,Tag0,Tag,PromotedSGML)) :-
    !,
    promote_first_body_elements(SGML,PromotedSGML).
promote_first_body_elements(text(Text),text(Text)).

promote_first_element([tag(SGMLType,_Tag0,"em",SGML)|Rest],
                      [tag(SGMLType,"h0","h0",SGML)|Rest]) :-
    !.
promote_first_element([tag(ub,_Tag0,"p",[])|Rest],
                      [tag(s,"h0","h0",PromotedSGML)|NewRest]) :-
    !,
    find_until_unscoped_end(Rest,"p",SGML,NewRest),
    extract_all_text(SGML,PromotedSGML0),
    (PromotedSGML0==[] ->
        PromotedSGML=[],
        DiscardedSGML=[]
    ;   PromotedSGML0=[PromotedFirst|DiscardedSGML],
        PromotedSGML=[PromotedFirst]
    ),
    ((\+DiscardedSGML==[], control_option(warnings)) ->
        format('~NWARNING: Discarding text while promoting <p> to <h0>: ~p~n',
               [DiscardedSGML])
    ;   true
    ).
promote_first_element(SGML,SGML).

find_until_unscoped_end([],_,[],[]) :-
    !.
find_until_unscoped_end([tag(ue,_,Tag,[])|Rest],Tag,[],Rest) :-
    !.
find_until_unscoped_end([First|Rest],Tag,[First|SGML],NewRest) :-
    find_until_unscoped_end(Rest,Tag,SGML,NewRest).

extract_all_text([],[]) :-
    !.
extract_all_text([First|Rest],Result) :-
    extract_all_text(First,ExtractedFirst),
    (is_list(ExtractedFirst) ->
        append(ExtractedFirst,ExtractedRest,Result)
    ;   Result=[ExtractedFirst|ExtractedRest]
    ),
    extract_all_text(Rest,ExtractedRest).
extract_all_text(tag(_,_,_,SGML),Result) :-
    !,
    extract_all_text(SGML,Result).
extract_all_text(text(Text),text(Text)).


/* simplify_tagging(+SGMLIn, -SGMLOut)

simplify_tagging/2 removes tag/4 elements with no content (i.e., SGML is [])
and promotes the SGML argument of tags satisfying non_structural_tag/1
(e.g., <a>, <i> and <sup>) effectively removing the tag but not its content.  */

simplify_tagging([],[]) :-
    !.
simplify_tagging([First|Rest],Result) :-
    simplify_tagging(First,ModifiedFirst),
    (is_list(ModifiedFirst) ->
        append(ModifiedFirst,ModifiedRest,Result)
    ;   Result=[ModifiedFirst|ModifiedRest]
    ),
    simplify_tagging(Rest,ModifiedRest).
simplify_tagging(tag(_SGMLType,_Tag0,_Tag,[]),[]) :-
    !.
simplify_tagging(tag(SGMLType,Tag0,Tag,SGML),Result) :-
    compute_simple_tag(Tag,SimpleTag),
    (non_structural_tag(SimpleTag) ->
        Result=ModifiedSGML
    ;   Result=tag(SGMLType,Tag0,Tag,ModifiedSGML)
    ),
    simplify_tagging(SGML,ModifiedSGML).
simplify_tagging(text(Text),text(Text)).

% physical markup tags
non_structural_tag("i") :- !.
non_structural_tag("b") :- !.
non_structural_tag("sub") :- !.
non_structural_tag("sup") :- !.
non_structural_tag("") :- !.
% special markup tags
non_structural_tag("a") :- !.
non_structural_tag("img") :- !.
non_structural_tag("font") :- !.
non_structural_tag("map") :- !.
non_structural_tag("area") :- !.
% misc markup tags
non_structural_tag([0'!|_]) :- !.


/* merge_adjacent_text(+SGMLIn, -SGMLOut)

merge_adjacent_text/2 replaces all sequences text(Text1), text(Text2) with
text(append(Text1,Text2)).  */

merge_adjacent_text([],[]) :-
    !.
merge_adjacent_text([tag(SGMLType,Tag0,Tag,SGML)|Rest],
                    [tag(SGMLType,Tag0,Tag,MergedSGML)|MergedRest]) :-
    !,
    merge_adjacent_text(SGML,MergedSGML),
    merge_adjacent_text(Rest,MergedRest).
merge_adjacent_text([text(First),text(Second)|Rest],Result) :-
    !,
    append(First,Second,Merged),
    merge_adjacent_text([text(Merged)|Rest],Result).
merge_adjacent_text([text(First)|Rest],[text(First)|MergedRest]) :-
    !,
    merge_adjacent_text(Rest,MergedRest).


dump_sgml(SGML,Stream) :-
    dump_sgml(SGML,0,Stream),
    (control_option(include_prolog_structure) ->
        format(Stream,'~n~nThe actual structure:~n~n~p~n',[SGML])
    ;   true
    ).

dump_sgml([],_,_) :-
    !.
dump_sgml([First|Rest],Level,Stream) :-
    dump_sgml(First,Level,Stream),
    dump_sgml(Rest,Level,Stream).
dump_sgml(tag(SGMLType,Tag0,_Tag,SGML),Level,Stream) :-
    !,
    (   (SGMLType==ue; SGMLType==se) ->
        format(Stream,'~*|</~s>~n',[Level,Tag0])   % unscoped end tag or scoped
                                                   %   end tag with no begining
    ;   SGMLType==uc ->                            % comment
        format(Stream,'~*|<!--~s>~n',[Level,Tag0])
    ;   format(Stream,'~*|<~s>~n',[Level,Tag0])    % all others
    ),
    NewLevel is Level+1,
    dump_sgml(SGML,NewLevel,Stream),
    (   SGMLType==s ->
        compute_simple_tag(Tag0,EndTag0),
        format(Stream,'~*|</~s>~n',[Level,EndTag0])
    ;   SGMLType==sb ->
        compute_simple_tag(Tag0,EndTag0),
        format(Stream,'~*|(</~s>)~n',[Level,EndTag0])
    ;   true
    ).
dump_sgml(text(Text),Level,Stream) :-
    compute_initial_string(Text,Initial),
    format(Stream,'~*|~s~n',[Level,Initial]).

compute_initial_string(Strings,String) :-
    concatenate_strings(Strings," ",String0),
    length(String0,N),
    (N < 63 ->
        String=String0
    ;   first_n_or_less(String0,62,String1),
        append(String1,"...",String)
    ).


write_tag_summary(ParsedSGML,SummaryStream) :-
    count_tags(ParsedSGML),
    write_tag_counts(SummaryStream).

count_tags([]).
count_tags([First|Rest]) :-
    !,
    count_tags(First),
    count_tags(Rest).
count_tags(tag(SGMLType,Tag0,Tag,SGML)) :-
    compute_simple_tag(Tag,SimpleTag),
    (memberchk(SGMLType,[s,sb,se,ub,ue,uc]) ->
        (memberchk(SGMLType,[s,sb,ub,uc]) ->    % beginning tag
            update_tag_counts(SGMLType,SimpleTag)
        ;   true
        ),
        (memberchk(SGMLType,[s,se,ue]) ->    % ending tag
            append(SimpleTag,"/",ModifiedEndTag),
            update_tag_counts(SGMLType,ModifiedEndTag)
        ;   true
        )
    ;   format('~NERROR: Unknown SGML type ~p for tag ~p~n',[SGMLType,Tag0])
    ),
    count_tags(SGML).
count_tags(text(_)).

update_tag_counts(SGMLType,Tag) :-
    (retract(tag_counts(typed(SGMLType,Tag),TypedCount)) ->
        NewTypedCount is TypedCount + 1,
        assert(tag_counts(typed(SGMLType,Tag),NewTypedCount))
    ;   assert(tag_counts(typed(SGMLType,Tag),1))
    ),
    (retract(tag_counts(untyped(Tag),UntypedCount)) ->
        NewUntypedCount is UntypedCount + 1,
        assert(tag_counts(untyped(Tag),NewUntypedCount))
    ;   assert(tag_counts(untyped(Tag),1))
    ).
    
write_tag_counts(SummaryStream) :-
    format(SummaryStream,'~nSimple tag counts:~n',[]),
    write_untyped_tag_counts(SummaryStream),
    format(SummaryStream,'~n~nTag counts by scope type:~n',[]),
    write_typed_tag_counts(SummaryStream).

write_untyped_tag_counts(SummaryStream) :-
    findall(utc(Tag,Count),tag_counts(untyped(Tag),Count),UTCs0),
    sort(UTCs0,UTCs),
    write_utcs(UTCs,SummaryStream).

write_utcs([],_).
write_utcs([utc(Tag0,Count)|Rest],SummaryStream) :-
    (append(Tag1,"/",Tag0) ->
        append("/",Tag1,Tag)
    ;   Tag=Tag0
    ),
    format(SummaryStream,'~t~d~5| ~s~n',[Count,Tag]),
    write_utcs(Rest,SummaryStream).

write_typed_tag_counts(SummaryStream) :-
    findall(tc(Tag,Type,Count),tag_counts(typed(Type,Tag),Count),TCs0),
    sort(TCs0,TCs),
    write_tcs(TCs,SummaryStream).

write_tcs([],_).
write_tcs([tc(Tag0,Type,Count)|Rest],SummaryStream) :-
    (append(Tag1,"/",Tag0) ->
        append("/",Tag1,Tag)
    ;   Tag=Tag0
    ),
    format(SummaryStream,'~t~d~5| ~s (~p)~n',[Count,Tag,Type]),
    write_tcs(Rest,SummaryStream).


dump_tag_sequences(SGML,Stream) :-
    dump_tag_sequences(SGML,0,[],Stream).

dump_tag_sequences([],_,_,_) :-
    !.
dump_tag_sequences([First|Rest],Level,RevTags,Stream) :-
    dump_tag_sequences(First,Level,RevTags,Stream),
    dump_tag_sequences(Rest,Level,RevTags,Stream).
dump_tag_sequences(tag(SGMLType,Tag0,Tag,SGML),Level,RevTags,Stream) :-
    !,
    compute_simple_tag(Tag,SimpleTag),
    NewLevel is Level+1,
    NewRevTags=[SimpleTag|RevTags],
    ((SGMLType==ue; SGMLType==se; SGMLType==uc) ->
        % unscoped end tag, scoped end tag with no beginning tag, or comment
        (SGML==[] ->
            true
        ;   format('ERROR: End/comment tag ~s has non-null scope: ~p~n',
                   [Tag0,SGML])
        )
    ;   % all others (begin tags)
        rev(NewRevTags,Tags),
        write_tags(Tags,Level,Stream)
    ),
    dump_tag_sequences(SGML,NewLevel,NewRevTags,Stream).
dump_tag_sequences(text(_),_Level,_RevTags,_Stream).

write_tags(Tags,Level,Stream) :-
    format(Stream,'~*|',[Level]),
    streamline_tags(Tags,StreamlinedTags),
    write_tags(StreamlinedTags,Stream).

streamline_tags([First,Second|Rest],StreamlinedTags) :-
    noisy_tag(First),
    !,
    streamline_tags([Second|Rest],StreamlinedTags).
streamline_tags(Tags,Tags).

noisy_tag([0'!|_]) :- !.
noisy_tag("tip") :- !.
noisy_tag("html") :- !.

write_tags([],Stream) :-
    format(Stream,'~n',[]).
write_tags([Singleton],Stream) :-
    !,
    format(Stream,'~s~n',[Singleton]).
write_tags([First|Rest],Stream) :-
    format(Stream,'~s.',[First]),
    write_tags(Rest,Stream).


write_section_fragments(SGML,FilePrefix,Stream) :-
    write_section_fragments(SGML,1,[],[FilePrefix],Stream).

write_section_fragments([],_,_,_,_) :-
    !.
write_section_fragments([First|Rest],N,RevSec,RevLabel,Stream) :-
    !,
    write_section_fragments(First,N,RevSec,RevLabel,Stream),
    NewN is N + 1,
    write_section_fragments(Rest,NewN,RevSec,RevLabel,Stream).
write_section_fragments(tag(SGMLType,Tag0,Tag,SGML),N,RevSec,RevLabel,
                        Stream) :-
    !,
    ((SGMLType==ue; SGMLType==se; SGMLType==uc) ->
        % unscoped end tag, scoped end tag with no beginning tag, or comment
        (SGML==[] ->
            true
        ;   format('ERROR: End/comment tag ~s has non-null scope: ~p~n',
                   [Tag0,SGML])
        )
    ;   % all others (begin tags)
        compute_simple_tag(Tag,SimpleTag),
        (section_fragment_tag(SimpleTag) ->
            write_section_fragments(SGML,1,[N|RevSec],[SimpleTag|RevLabel],
                                    Stream)
        ;   write_all_text(SGML,[N|RevSec],RevLabel,Stream)
        )
    ).
write_section_fragments(text(Text),N,RevSec,RevLabel,Stream) :-
    !,
    write_section_text(Text,[N|RevSec],RevLabel,Stream).

section_fragment_tag("abs") :- !.
section_fragment_tag("app") :- !.
section_fragment_tag("appx") :- !.
section_fragment_tag("bdy") :- !.
section_fragment_tag("body") :- !.
section_fragment_tag("bsec") :- !.
section_fragment_tag("cd") :- !.
section_fragment_tag("chp") :- !.
section_fragment_tag("cint") :- !.
section_fragment_tag("cvr") :- !.
section_fragment_tag("dev") :- !.
section_fragment_tag("div") :- !.
section_fragment_tag("editnote") :- !.
section_fragment_tag("evd") :- !.
section_fragment_tag("exb") :- !.
section_fragment_tag("fig") :- !.
section_fragment_tag("fm") :- !.
section_fragment_tag("fsec") :- !.
section_fragment_tag("fwd") :- !.
section_fragment_tag("gde") :- !.
section_fragment_tag("ge") :- !.
section_fragment_tag("glos") :- !.
section_fragment_tag("guide") :- !.
%section_fragment_tag("h0") :- !.       % special tag used for <p><em> promotion
%section_fragment_tag("h1") :- !.
%section_fragment_tag("h2") :- !.
%section_fragment_tag("h3") :- !.
%section_fragment_tag("h4") :- !.
%section_fragment_tag("h5") :- !.
%section_fragment_tag("h6") :- !.
section_fragment_tag("head") :- !.
section_fragment_tag("html") :- !.
section_fragment_tag("it") :- !.
section_fragment_tag("nit") :- !.
section_fragment_tag("part") :- !.
section_fragment_tag("pre") :- !.
section_fragment_tag("recmd") :- !.
section_fragment_tag("record") :- !.
section_fragment_tag("report") :- !.
section_fragment_tag("reportedby") :- !.
section_fragment_tag("sbt") :- !.
section_fragment_tag("sec") :- !.
section_fragment_tag("smtl") :- !.
section_fragment_tag("ss1") :- !.
section_fragment_tag("ss2") :- !.
section_fragment_tag("ss3") :- !.
section_fragment_tag("ss4") :- !.
section_fragment_tag("tbl") :- !.
section_fragment_tag("tdef") :- !.
section_fragment_tag("ti") :- !.
%section_fragment_tag("title") :- !.
section_fragment_tag("topic") :- !.
section_fragment_tag("type") :- !.


write_section_text(Strings,RevSec,RevLabel,Stream) :-
    rev(RevSec,Sec),
    rev(RevLabel,Label),
    write_section_and_label(Sec,Label,Stream),
    write_strings(Strings,Stream),
    format(Stream,'~n',[]).

write_section_and_label(Sec,Label,Stream) :-
    format(Stream,'SFrag = ',[]),
    write_section(Sec,Stream),
    format(Stream,' = ',[]),
    write_label(Label,Stream),
    format(Stream,'~n',[]).

write_section([],_).
write_section([First|Rest],Stream) :-
    (Rest==[] ->
        format(Stream,'~d',[First])
    ;   format(Stream,'~d.',[First])
    ),
    write_section(Rest,Stream).

write_label([],_).
write_label([First|Rest],Stream) :-
    (Rest==[] ->
        format(Stream,'~s',[First])
    ;   format(Stream,'~s.',[First])
    ),
    write_label(Rest,Stream).

write_strings([],_).
write_strings([First|Rest],Stream) :-
    length(First,N),
    (N < 80 ->
        format(Stream,'~s~n',[First])
    ;   write_string_across_lines(First,Stream)
    ),
    write_strings(Rest,Stream).

write_string_across_lines(String,Stream) :-
    split_string_completely(String," ",Words),
    write_words_across_lines(Words,Stream).

write_words_across_lines([],_).
write_words_across_lines([First|Rest],Stream) :-
    format(Stream,'~s',[First]),
    write_rest_words_across_lines(Rest,Stream),
    format(Stream,'~n',[]).

write_rest_words_across_lines([],_).
write_rest_words_across_lines([First|Rest],Stream) :-
    line_position(Stream,NLine),
    length(First,NFirst),
    N is NLine + NFirst,
    (N < 80 ->
        format(Stream,' ~s',[First])
    ;   format(Stream,'~n~s',[First])
    ),
    write_rest_words_across_lines(Rest,Stream).
    

write_all_text(SGML,RevSec,RevLabel,Stream) :-
%    rev(RevSec,Sec),
%    rev(RevLabel,Label),
%    format(Stream,'***** write_all_text: ~p ~p ~p~n',[Sec,Label,SGML]),
    extract_all_text(SGML,SGMLText0),
    merge_adjacent_text(SGMLText0,SGMLText),
    (SGMLText=[text(Text)] ->
        write_section_text(Text,RevSec,RevLabel,Stream)
    ;   format('ERROR: write_all_text failed for ~p~n',[SGML])
    ).
%write_all_text([],_,_,_) :-
%    !.
%write_all_text([First|Rest],RevSec,RevLabel,Stream) :-
%    !,
%    write_all_text(First,RevSec,RevLabel,Stream),
%    write_all_text(Rest,RevSec,RevLabel,Stream).
%write_all_text(tag(_SGMLType,_Tag0,_Tag,SGML),RevSec,RevLabel,Stream) :-
%    !,
%    write_all_text(SGML,RevSec,RevLabel,Stream).
%write_all_text(text(Text),RevSec,RevLabel,Stream) :-
%    write_section_text(Text,RevSec,RevLabel,Stream).


write_utts(FragmentStream,UttStream) :-
    initialize_fragment_stream(FragmentStream),
    repeat,
    (get_fragment(FragmentStream,Fragment) ->
        process_fragment(Fragment,UttStream),
        fail
    ;   true
    ),
    !.

initialize_fragment_stream(FragmentStream) :-
    fget_line(FragmentStream,Line),
    (Line=="Section fragments:" ->
        true
    ;   initialize_fragment_stream(FragmentStream)
    ).
initialize_fragment_stream(_) :-
    format('ERROR: Could not find sentence fragments to label.~n',[]),
    fail.

get_fragment(FragmentStream,_) :-
    at_end_of_stream(FragmentStream),
    !,
    fail.
get_fragment(FragmentStream,sfrag(Sec,Label,Lines)) :-
    fget_line(FragmentStream,Line),
    (split_string_completely(Line," = ",["SFrag",Sec,Label]) ->
        get_fragment_lines(FragmentStream,Lines)
    ;   format('ERROR: Found "~s" looking for section fragment label.~n',
               [Line]),
        fail
    ),
    !.

get_fragment_lines(FragmentStream,[]) :-
    at_end_of_stream(FragmentStream),
    !.
get_fragment_lines(FragmentStream,Result) :-
    fget_line(FragmentStream,Line),
    (Line=="" ->
        Result=[]
    ;   Result=[Line|Lines],
        get_fragment_lines(FragmentStream,Lines)
    ).

process_fragment(sfrag(Sec,Label,Lines),UttStream) :-
    parse_lines_into_utterances(Lines,Utterances),
    write_fragment_utterances(Utterances,Sec,Label,1,UttStream).

write_fragment_utterances([],_,_,_,_).
write_fragment_utterances([First|Rest],Sec,Label,N,UttStream) :-
    write_fragment_utterance(First,Sec,Label,N,UttStream),
    NewN is N + 1,
    write_fragment_utterances(Rest,Sec,Label,NewN,UttStream).

write_fragment_utterance(Words,Sec,Label,N,UttStream) :-
    format(UttStream,'[ ~s|~s|~d ]~n',[Sec,Label,N]),
    write_words_across_lines(Words,UttStream),
    format(UttStream,'~n',[]).

