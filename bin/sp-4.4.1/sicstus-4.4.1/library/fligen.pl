% fligen.pl -- development system specific parts of the FLI.
% Copyright (c) 1998-2012 SICS
% Internal helper module for splfr/splfd. Not intended to be used by end-users.

:- module(fligen, [prepare_foreign_resource/5,
                      %% SPLD
                      spld_prepare_resource_table/0,
                      spld_prepare_resource_table0/0,
                      splfr_prepare_foreign_resource/5]).
:- use_module(library(types), [illarg/3]).
:- use_module(library(lists), [select/3]).

%% XREF
% 	foreign_resource/2,
% 	foreign/2, foreign/3.

% Records the found foreign_resource/2 facts.
:- dynamic foreign_resource_/2.
% Records the found foreign_type/2 facts.
:- dynamic foreign_type_/2.
% Records the found foreign/[2,3] facts.
:- dynamic foreign_/3.

:- volatile
        foreign_resource_/2,
        foreign_type_/2,
        foreign_/3.

:- dynamic use_portray_hook/0.
:- volatile use_portray_hook/0.

% [PM] 4.2.1+ We can declare portray_hook/2 now that we no longer resides in Bips.
% halt with an error code (for make/splfr) if there are any errors.
% [PM] 4.3.3 Now also enabled for spld.
:- user:portray_hook/2 is throwing.
user:portray_hook(error, Message) :-
        use_portray_hook, !,
        retractall(use_portray_hook),
        print_message(error, Message),
        halt(1).

/* [PM] 3.9 generate a header file unless GlueHeaderFile is '' */
/* Imports is a list of prerequisite modules defining term expansions.
   Currently, Imports = [] or [str_decl].
*/
:-  splfr_prepare_foreign_resource/5 is throwing.
splfr_prepare_foreign_resource(Resource, SourceFile, CFile, HFile, Imports) :-
        %% 3.8.5 propagate errors to make
        asserta(use_portray_hook),
        catch(prepare_foreign_resource(Resource, SourceFile, CFile, HFile, Imports), E, (print_message(error, E), halt(1))),
        !, % redundant
	halt.
splfr_prepare_foreign_resource(_Resource, _SourceFile, _CFile, _HFile, _Imports) :-
	%% [MC] propagate failure to make
	halt(1).

prepare_foreign_resource(Resource, SourceFile, CFile, HFile, Imports) :-
	statistics(runtime, [T0|_]),
	Goal = prepare_foreign_resource(Resource,SourceFile,CFile,HFile),
	(   foreach(Lib,Imports)
	do  use_module(library(Lib))
	),
	retractall(foreign_resource_(_,_)),
	retractall(foreign_type_(_,_)),
	retractall(foreign_(_,_,_)),
	gather_declarations(Resource, SourceFile),
	(   foreign_resource_(Resource,Functions) ->
	    absolute_file_name(HFile, AbsHFile, [extensions(['.h'])]),
	    absolute_file_name(CFile, AbsCFile, [extensions(['.c'])]),
	    sp_ensure_c_name(Resource, ResourceName),
	    write_header_files(AbsHFile, AbsCFile, ResourceName, Functions, Goal)
	;   illarg(existence('foreign_resource',Resource,0), Goal, 2)
	),
	statistics(runtime, [T1|_]),
	Time is T1-T0,
	print_message(informational, generated(AbsHFile,Time)),
	print_message(informational, generated(AbsCFile,Time)).

write_header_files(AbsHFile, AbsCFile, Resource, Functions, _Goal) :-
	findall(X=Y, foreign_type_(X,Y), ForeignTypes),
	header_template(Resource, HLines0, HLines1),
	(   foreach(Name=Type, ForeignTypes),
	    fromto(HLines1, ['typedef ~s;'-[Decl]|S], S, HLines2)
	do  inside_out(Type, Name, Itype, Prim),
	    declaration(Name, Prim, Itype, Decl, [])
	),
	do_structs(ForeignTypes, HLines2, HLines3),
	c_functions(Functions, CFunctions, 'NULL', Init, 'NULL', DeInit),
	hfile_lines(CFunctions, Init, DeInit, preserve, HLines3, HLines4),
        HLines = ['#ifndef SP_FOREIGN_RESOURCE_~a_H_INCLUDED'-[Resource],
                  '#define SP_FOREIGN_RESOURCE_~a_H_INCLUDED 1'-[Resource]
                 | HLines0 ],
        HLines4 = ['#endif /* SP_FOREIGN_RESOURCE_~a_H_INCLUDED */'-[Resource]
                  | [] ],
	( AbsHFile == user -> HStream = user; open(AbsHFile, write, HStream) ),
	call_cleanup(print_lines(HLines, HStream),
                     close(HStream)),
	% HFile is almost, but not quite, duplicated into CFile
	header_template(Resource, CLines0, CLines1),
	hfile_lines(CFunctions, Init, DeInit, void, CLines1, CLines2),
	cfile_lines(Resource, CFunctions, Init, DeInit, CLines2, CLines3),
        CLines = ['#define SP_IN_~a_glue_c 1'-[Resource],
                  '#define SPAUX_H_DO_NOT_DECLARE_sp_GlobalSICStus 1'-[]
                 | CLines0 ],
        CLines3 = [],
	( AbsCFile == user -> Stream = user; open(AbsCFile, write, Stream) ),
        call_cleanup(print_lines(CLines, Stream),
                     close(Stream)).

%  inside_out(+Type, +Name, -Itype, -Iname)
%  Itype is an inside-out version of Type, where Name is the name
%  of the type, and Iname is the inside-out version of it.  This
%  is used because C type declarations are "inside-out" relative to
%  the structs package's declarations.  That is, where structs says
%  foo is of type pointer to integer, C says that there is an integer
%  that foo points to.  It gets much more complicated when the 
%  declaration is complex.

inside_out(pointer(Type), Name, Itype, Iname) :- !,
	inside_out(Type, pointer(Name), Itype, Iname).
inside_out(array(Num,Type), Name, Itype, Iname) :- !,
	inside_out(Type, array(Num,Name), Itype, Iname).
inside_out(array(Type), Name, Itype, Iname) :- !,
	inside_out(Type, array(Name), Itype, Iname).
inside_out(Type, Itype, Itype, Type).

/*
		Definite Clause Grammar for C type declarations
		(See Kernighan and Richie Appendix A for BNF)
*/
declaration(Name,Prim,Itype) -->
	type_specifier(Prim,Name),
	declarator(Itype).

type_specifier(struct(_), Name) --> !,
	struct_specifier(Name).
type_specifier(union(_), Name) --> !,
	union_specifier(Name).
%% [PM] 3.5 incomplete enums are illegal (and fatal on HPUX 11.00 cc).
%%          So instead of typedef enum foo bar; we must do something like
%%             typedef enum foo {...} bar;
%%          or possibly
%%             enum foo {...};
%%             typedef enum foo bar;
%%
%% type_specifier(enum(_), Name) --> !,
%% 	enum_specifier(Name).
type_specifier(enum(Fields), Name) --> !,
        enum_declaration_body(Fields,Name),
        " ".
type_specifier(Type, _) -->
	full_type_specifier(Type).

full_type_specifier(struct(Fields)) --> !,
	full_struct_specifier(Fields).
full_type_specifier(union(Members)) --> !,
	full_union_specifier(Members).
full_type_specifier(enum(Enums)) --> !,
	full_enum_specifier(Enums).
full_type_specifier(integer) --> !,
	"SP_integer ".
full_type_specifier(integer_64) --> !,
	"SP_int64 ".
full_type_specifier(integer_32) --> !,
	"int ".
full_type_specifier(integer_16) --> !,
	"short ".
full_type_specifier(integer_8) --> !,
	"char ".
full_type_specifier(unsigned) --> !,
	"SP_uinteger ".
full_type_specifier(unsigned_64) --> !,
	"SP_uint64 ".
full_type_specifier(unsigned_32) --> !,
	"unsigned int ".
full_type_specifier(unsigned_16) --> !,
	"unsigned short ".
full_type_specifier(unsigned_8) --> !,
	"unsigned char ".
full_type_specifier(float) --> !,
	"double ".
full_type_specifier(float_32) --> !,
	"float ".
full_type_specifier(atom) --> !,
	"SP_atom ".
full_type_specifier(string) --> !,
	"char *".
full_type_specifier(opaque) --> !,
	"void ".
full_type_specifier(address) --> !,
	"void *".
full_type_specifier(Ident) -->
	identifier(Ident),
	" ".

struct_specifier(Name) -->
	"struct _",
	identifier(Name),
	" ".

union_specifier(Name) -->
	"union _",
	identifier(Name),
	" ".

enum_specifier(Name) -->
	"enum _",
	identifier(Name),
	" ".

full_struct_specifier(Fields) -->
	"struct {",
	struct_decl_list(Fields),
	"} ".

full_union_specifier(Fields) -->
	"union {",
	struct_decl_list(Fields),
	"} ".

full_enum_specifier(Fields) -->
	"enum {",
	enum_list(Fields),
	"} ".


declarator(pointer(Type)) --> !,
	"*(",
	declarator(Type),
	")".
declarator(array(Num,Type)) --> !,
	"(",
	declarator(Type),
	")[",
	number(Num),
	"]".
declarator(array(Type)) --> !,			% C doesn't support unknown-
	"*(",					% size arrays properly, so
	declarator(Type),			% this hack should work.
	")", 					% Note that your C code must
	"/* really an unknown-size array */".	% reference foo->x[2] as
						% *(foo->x)+2 instead.  Sorry.
declarator(Name) -->
	identifier(Name).


do_structs([]) --> [].
do_structs([Name=Type|Decls]) -->
	{struct_union_enum_declaration(Type, Name, Decl, [])}, !,
	['~s;'-[Decl]],
	do_structs(Decls).
do_structs([_|Decls]) -->
	do_structs(Decls).

struct_union_enum_declaration(struct(Fields),Name) -->
        struct_declaration_body(Fields,Name).
struct_union_enum_declaration(union(Fields),Name) -->
        union_declaration_body(Fields,Name).
%% [PM] 3.5 Do nothing. The enum was declared as part of the typedef in pass 1.
%% [PM] 3.5 in effect, what QP 3.4 did (emits full declaration of the enum in second pass)
% struct_union_enum_declaration(enum(Fields),Name) -->
%         enum_declaration_body(Fields,Name), ";".

struct_declaration_body(Fields,Name) -->
	struct_specifier(Name),
	"{\n",
	struct_decl_list(Fields),
	"}".

union_declaration_body(Fields,Name) -->
	union_specifier(Name),
	"{\n",
	struct_decl_list(Fields),
	"}".

enum_declaration_body(Fields,Name) -->
	enum_specifier(Name),
	"{\n",
	enum_list(Fields),
	"}".


struct_decl_list([]) --> [].
struct_decl_list([Name:Type|Fields]) -->
	{inside_out(Type, Name, Itype, Prim)},
	struct_declaration(Prim, Itype),
	struct_decl_list(Fields).


struct_declaration(Prim, Itype) -->
	"\t",
	full_type_specifier(Prim),
	declarator(Itype),
	";\n".

enum_list([]) --> [].
enum_list([Name=Value|Fields]) --> !,
	"\t",
	identifier(Name),
	" = ",
	number(Value),
	",\n",
	enum_list(Fields).
enum_list([Name]) --> !,
        %% [PM] 3.5 Do not emit comma after last identifier. AIX C 6
        %% complains on terminating comman: 1506-275 (S) Unexpected
        %% text ',' encountered.
	"\t",
	identifier(Name),
	"\n".
enum_list([Name|Fields]) --> !,
	"\t",
	identifier(Name),
	",\n",
	enum_list(Fields).


identifier(Id, List, Rest) :-
	atom_codes(Id, Id_codes),
	append(Id_codes, Rest, List).

number(Num, List, Rest) :-
	number_codes(Num, Num_codes),
	append(Num_codes, Rest, List).

print_lines([], _).
print_lines([Fmt-Args|Lines], Stream) :-
	format(Stream, Fmt, Args),
	nl(Stream),
	print_lines(Lines, Stream).


hfile_lines(CFunctions, Init, DeInit, Typenames) -->
	(   {Init=='NULL'} -> []
	;   init_header(Init)
	),
	(   {DeInit=='NULL'} -> []
	;   init_header(DeInit)
	),
	extern_headers(CFunctions, Typenames).

header_template(Resource) -->
	['#ifndef SP_RESNAME'-[],
	 '# define SP_RESNAME ~a'-[Resource],
	 '#endif'-[],
	 '#if !SPDLL'-[],
	 '# ifndef SP_STATIC_FOREIGN_RESOURCE'-[],
	 '#  define SP_STATIC_FOREIGN_RESOURCE 1'-[],
	 '# endif'-[],
	 '#endif /* !SPDLL */'-[],
	 '#include <sicstus/sicstus.h>'-[],
	 '#include <stdlib.h>'-[],
         
	 '#if ((SP_DYNAMIC_FOREIGN_RESOURCE || SPDLL) && !MULTI_SP_AWARE)'-[],
	 '# ifndef sp_GlobalSICStus'-[],
	 '#  if !SP_NO_MANGLE_sp_GlobalSICStus'-[],
	 '#   define sp_GlobalSICStus SP_RESNAME_CATENATE(sp_GlobalSICStus,SP_RESNAME)'-[],
         '#   define sp_GlobalSICStus_MANGLED 1'-[],
	 '#  endif'-[],
	 '# endif /* !defined sp_GlobalSICStus */'-[],
	 '#endif /* ((SP_DYNAMIC_FOREIGN_RESOURCE || SPDLL) && !MULTI_SP_AWARE) */'-[],
         
         % '#if 1 || ((SP_DYNAMIC_FOREIGN_RESOURCE || SPDLL) && !MULTI_SP_AWARE)'-[],
         '#if (sp_GlobalSICStus_MANGLED && sp_GlobalSICStus_MANGLED_declared)'-[],
         ' /* sp_GlobalSICStus_~a already declared */'-[Resource],
         '#elif (!sp_GlobalSICStus_MANGLED && sp_GlobalSICStus_declared)'-[],
         ' /* sp_GlobalSICStus (unmangled) already declared */'-[],
         '#else /* Need to declare it (sp_GlobalSICStus may be a mangled name) */'-[],
         '  SP_BEGIN_DECL'-[],
         '   extern SICSTUS_API_STRUCT_TYPE *sp_GlobalSICStus;'-[],
         '#  if sp_GlobalSICStus_MANGLED'-[],
         '#   define sp_GlobalSICStus_MANGLED_declared 1'-[],
         '#  else'-[],
         '#   define sp_GlobalSICStus_declared 1'-[],
         '#  endif'-[],
         '  SP_END_DECL'-[],
         '# endif'-[],
         % '#endif /* ((SP_DYNAMIC_FOREIGN_RESOURCE || SPDLL) && !MULTI_SP_AWARE) */'-[],
         '#ifndef SP_CONTEXT_SWITCH_HOOK'-[],
	 '# define SP_CONTEXT_SWITCH_HOOK sp_context_switch_hook_~a'-[Resource],
	 '#endif'-[],
	 'SP_BEGIN_DECL'-[],
	 ' extern void SPCDECL SP_CONTEXT_SWITCH_HOOK (int);'-[],
	 'SP_END_DECL'-[]
        ].


init_header(Init) -->
	['SP_BEGIN_DECL'-[],
	 '#ifdef SP_MANGLE'-[],
	 '#define ~a SP_MANGLE(~a)'-[Init,Init],
	 '#endif'-[],
	 'extern void SPCDECL ~a PROTOTYPE((SPAPI_ARG_PROTO_DECL int));'-[Init],
	 'SP_END_DECL'-[]
        ].

extern_headers([], _Typenames) --> [].
extern_headers([F|Fs], Typenames) -->
        { encode_function_type(F, Typenames, Codes, RetType) },
	 ['SP_BEGIN_DECL'-[],
	  '#ifdef SP_MANGLE'-[],
	  '#define ~a SP_MANGLE(~a)'-[F,F],
	  '#endif'-[],
	  'extern ~a SPCDECL ~a PROTOTYPE(( ~a'-[RetType,F,ProtoDecl]
         ],
          extern_args(Codes, ProtoDecl),
	 ['SP_END_DECL'-[]
         ],
	extern_headers(Fs, Typenames).

extern_args([], 'SPAPI_ARG_PROTO_DECL0 ));') --> !.
extern_args([C-T], 'SPAPI_ARG_PROTO_DECL') --> !,
	{c_type_name(C, T, Name)},
	['~a));'-Name].
extern_args([C-T|CTs], ProtoDecl) -->
	{c_type_name(C, T, Name)},
	['~a,'-Name],
	extern_args(CTs, ProtoDecl).

:- mode encode_function_type(+,+,-,-).
encode_function_type(F, Typenames, Codes, RetType) :-
        encode_function_type(F, Typenames, Codes0),
        ( select(C-T, Codes0, Codes),
          C /\ 0x30 =:= 0x30
        ->  C1 is C /\ \0x30,
            c_type_name(C1, T, RetType)
        ;   Codes = Codes0,
            RetType = 'void'
        ).

:- mode encode_function_type(+,+,-).
encode_function_type(F, Typenames, Codes) :-
        foreign_(F, c, D), !,
        ( (   foreacharg(Arg,D),
              foreach(Code-Type,Codes),
              param(Typenames)
          do  encode_arg(Arg, Code, Typenames, Type)
          ) -> true
        ;   illarg(domain(term,'foreign declaration'), foreign(F,c,D), 3)
        ).

cfile_lines(Resource, CFunctions, Init, DeInit) -->
        { prolog:'$SP_MAINFUN_PARAMS_VERSION'(SP_MAINFUN_PARAMS_VERSION) },
	[
	 'static SP_mutex sp_resource_mutex_~a=SP_MUTEX_INITIALIZER;'-[Resource],
	 '#ifndef SPFUNCVARS'-[],
	 '#define SPFUNCVARS 1'-[],
	 '#include <stdio.h>'-[],
	 '#include <sicstus/spaux.c>'-[],
	 '#endif'-[],
	 'SP_BEGIN_DECL'-[],
	 'extern SPGLUEEXP SP_MainFun sp_main_SPENV_~a;'-[Resource],
	 'SP_END_DECL'-[]
        ],
        ['static const char *~a_prednames[] = {'-[Resource]],
        predname_lines(CFunctions),
        ['static int ~a_arities[] = {'-[Resource]],
        arity_lines(CFunctions),
        %% Native apply_asm */
        ['#if !SP_FLI_APPLY_ASM_GENERIC'-[]],
	['static SP_GlueFun *~a_funcs[] = {'-[Resource]],
	function_lines(CFunctions),
        ['#endif /* !SP_FLI_APPLY_ASM_GENERIC */'-[]],
        %% [PM] 4.2.1+ glue-based */
        ['#if SP_FLI_APPLY_ASM_GENERIC'-[]],
        glue_function_lines(CFunctions),
        ['static SP_GenericGlueFun *~a_funcs[] = {'-[Resource]],
        function_lines_generic(CFunctions),
        ['#endif /* SP_FLI_APPLY_ASM_GENERIC */'-[]],
        ['SPGLUEEXP1 int SPGLUEEXP2 SPCDECL sp_main_SPENV_~a(SP_MAINFUN_PARAMS *params)'-[Resource],
         '{'-[],
         '#if MULTI_SP_AWARE'-[],
         '   SPAPI_ARG_LOCAL_DECL'-[],
         '#endif'-[],
         '   return sp_main_helper(params, 0x~16R, GetSICStusDISPATCHAddress(), &sp_resource_mutex_~a, ~a_funcs, ~a_prednames, ~a_arities, ~a, ~a);'
        - 
         [SP_MAINFUN_PARAMS_VERSION,Resource,Resource,Resource,Resource,Init,DeInit],
         '}'-[]
        ],
        [].

function_lines([]) -->
	['0};'-[]].
function_lines([F|Fs]) -->
	['(SP_GlueFun *)~a,'-[F]],
	function_lines(Fs).

function_lines_generic([]) -->
        ['0};'-[]].
function_lines_generic([F|Fs]) -->
        ['&glue_~a,'-[F]], % no cast should be needed here
        function_lines_generic(Fs).

glue_function_lines([]) -->
        [].
glue_function_lines([F|Fs]) --> 
        {foreign_(F, c, D),
         functor(D,_,Arity),
         encode_function_type(F, void, Codes)}, !,
        ['static void SPCDECL glue_~a(sp_t_fli_call *a)'-[F]],
        ['{'-[]],
        ['   (void) a; /* Avoid unused argument warnings. */'-[] ],
        [ResultAssignment],
        ['~a('-[F]],
        ['#if MULTI_SP_AWARE'-[]],
        ['   (SPEnv*)a->stash_arg~a /* Cast should not be needed */'-[EnvSep]],
        ['#endif /* MULTI_SP_AWARE */'-[]],
        {
           ( Arity == 0 ->
             EnvSep = ''
           ; Codes=[C-_],
             decode_code(C, _, return) ->
             % No C arguments, the single Prolog argument corresponds to C function return value
             EnvSep = ''
           ; otherwise ->
             EnvSep = (',')
           )
        },
        glue_body_lines(Codes, F, D, 1,Arity, '   ', 0, 0, ResultAssignment),
        [');'-[]],
        ['}'-[]],
        glue_function_lines(Fs).


glue_body_lines([], _F, _D, _ArgNo,_Arity, _Sep, _IntegerIndex, _DoubleIndex, ResultAssignment) -->
        % If function is void
        { ResultAssignment = ''-[] },
        [].
glue_body_lines([C-_T|Codes], F, D, ArgNo,Arity, Sep, IntegerIndex, DoubleIndex, ResultAssignment) -->
        { decode_code(C, 'float', return) }, !,
        { ResultAssignment = ('a->double_return = /* ~w Cast should not be needed */'-[Spec]) },
        { arg(ArgNo,D, Spec),
          ArgNo1 is ArgNo+1 },
        glue_body_lines(Codes, F, D, ArgNo1,Arity, Sep, IntegerIndex, DoubleIndex, _IgnoredResultAssignment).
glue_body_lines([C-_T|Codes], F, D, ArgNo,Arity, Sep, IntegerIndex, DoubleIndex, ResultAssignment) -->
        { decode_code(C, _, return) }, !,
        { ResultAssignment = ('a->integer_return = (SP_integer) /* ~w */'-[Spec]) },
        { arg(ArgNo,D, Spec),
          ArgNo1 is ArgNo+1 },
        glue_body_lines(Codes, F, D, ArgNo1,Arity, Sep, IntegerIndex, DoubleIndex, _IgnoredResultAssignment).
glue_body_lines([C-_T|Codes], F, D, ArgNo,Arity, Sep, IntegerIndex, DoubleIndex, ResultAssignment) -->
        { decode_code(C, float, input) }, !,
        ['~aa->double_arg[~d] /* ~w */'-[Sep, DoubleIndex, Spec]],
        { DoubleIndex1 is DoubleIndex+1 },
        { arg(ArgNo,D, Spec),
          ArgNo1 is ArgNo+1 },
        glue_body_lines(Codes, F, D, ArgNo1,Arity, '  , ', IntegerIndex, DoubleIndex1, ResultAssignment).
glue_body_lines([C-_T|Codes], F, D, ArgNo,Arity, Sep, IntegerIndex, DoubleIndex, ResultAssignment) -->
        { decode_code(C, _, input) }, !,
        { c_type_name(C, CType) },
        ['~a(~w) a->integer_arg[~d] /* ~w */'-[Sep, CType, IntegerIndex, Spec]],
        { IntegerIndex1 is IntegerIndex+1 },
        { arg(ArgNo,D, Spec),
          ArgNo1 is ArgNo+1 },
        glue_body_lines(Codes, F, D, ArgNo1,Arity, '  , ', IntegerIndex1, DoubleIndex, ResultAssignment).
glue_body_lines([C-_T|Codes], F, D, ArgNo,Arity, Sep, IntegerIndex, DoubleIndex, ResultAssignment) -->
        { decode_code(C, _, output) }, !,
        { c_type_name(C, CType) },
        ['~a(~w) a->integer_arg[~d] /* ~w */'-[Sep, CType, IntegerIndex, Spec]],
        { IntegerIndex1 is IntegerIndex+1 },
        { arg(ArgNo,D, Spec),
          ArgNo1 is ArgNo+1 },
        glue_body_lines(Codes, F, D, ArgNo1,Arity, '  , ', IntegerIndex1, DoubleIndex, ResultAssignment).



predname_lines([]) -->
	['0};'-[]].
predname_lines([F|Fs]) -->
	{foreign_(F, c, D)}, !,
	{functor(D, P, _)},
	['"~a",'-[P]],
	predname_lines(Fs).

arity_lines([]) -->
	['-1};'-[]].
arity_lines([F|Fs]) -->
	{foreign_(F, c, D)}, !,
	{functor(D, _, A)},
	['~d,'-[A]],
	arity_lines(Fs).


encode_arg([-Arg], Code, Typenames, AddrType) :-
	encode2(Arg, Code0, Typenames, AddrType),
	Code is Code0 \/ 0x30.
encode_arg(+Arg, Code, Typenames, AddrType) :-
	encode2(Arg, Code, Typenames, AddrType).
encode_arg(-Arg, Code, Typenames, AddrType) :-
	encode2(Arg, Code0, Typenames, AddrType),
	Code is Code0 \/ 0x10.

encode2(integer,       0x00, _, void).
encode2(float,	       0x01, _, void).
encode2(atom,          0x02, _, void).
encode2(string,        0x03, _, void).
% Work around translation error for example in:
% :- foreign_type
% 	aliastest	= integer_32,
% 	aliasarray	= array(10, aliastest).
% foreign(test5, c, test5(+pointer(aliasarray), [-aliastest])).
encode2(address(AddrType), 0x04, preserve, Type) :-
	deref_type(AddrType, DerefType),
	array_type(DerefType, Type), !.
encode2(address(Type), 0x04, preserve, Type) :-
	atom(Type), !.
encode2(address(_),    0x04, _, void).
encode2(address,       0x04, _, void).
encode2(term,          0x05, _, void).
encode2(codes,         0x06, _, void).

decode_type_code(0x00, integer).
decode_type_code(0x01, float).
decode_type_code(0x02, atom).
decode_type_code(0x03, string).
decode_type_code(0x04, address).
decode_type_code(0x05, term).
decode_type_code(0x06, codes).

decode_direction_code(0x30, return).
decode_direction_code(0x10, output).
decode_direction_code(0x00, input).

:- mode split_code(+Code, -TypeCode, -DirectionCode).
split_code(Code, TypeCode, DirectionCode) :-
        TypeCode is Code /\ \0x30,
        DirectionCode is Code /\ 0x30.

:- mode decode_code(+Code, -Type, -Direction).
decode_code(Code, Type, Direction) :-
        split_code(Code, TypeCode, DirectionCode),
        decode_type_code(TypeCode, Type),
        decode_direction_code(DirectionCode, Direction).

:- deref_type/2 is nondet.
deref_type(X, Z) :-
	foreign_type_(X, Y),
	deref_type(Y, Z).
deref_type(X, X).

array_type(array(Type), Type).
array_type(array(_,Type), Type).

%% [PM] 4.0 Added SP_FLI_CONST (a.k.a. const) to make the C compiler
%% ensure that atom names etc are not modified by foreign code.
%% Lazy customers etc can pass -DSP_FLI_CONST='' to the C compiler

c_type_name(0x00,  _, 'SP_integer').
c_type_name(0x01,  _, 'double').
c_type_name(0x02,  _, 'SP_atom').
c_type_name(0x03,  _, 'char SP_FLI_CONST *').
c_type_name(0x04,  T, C) :-
	atom_concat(T, ' *', C).
c_type_name(0x05,  _, 'SP_term_ref').
c_type_name(0x06,  _, 'char SP_FLI_CONST *').
c_type_name(0x10,  _, 'SP_integer *').
c_type_name(0x11,  _, 'double *').
c_type_name(0x12,  _, 'SP_atom *').
c_type_name(0x13,  _, 'char SP_FLI_CONST **').
c_type_name(0x14,  T, C) :-
	atom_concat(T, ' **', C).
c_type_name(0x15,  _, 'SP_term_ref'). % N.B.
c_type_name(0x16,  _, 'char SP_FLI_CONST **').

c_type_name(Code, CType) :-
        c_type_name(Code, void, CType).

c_functions([], [], Init, Init, Deinit, Deinit).
c_functions([init(Init0)|Fs], Cs, _, Init, DeInit0, DeInit) :- !,
	c_functions(Fs, Cs, Init0, Init, DeInit0, DeInit).
c_functions([deinit(DeInit0)|Fs], Cs, Init0, Init, _, DeInit) :- !,
	c_functions(Fs, Cs, Init0, Init, DeInit0, DeInit).
c_functions([C|Fs], Cs, Init0, Init, DeInit0, DeInit) :- atom(C), !,
        Cs = [C|Cs1],
	c_functions(Fs, Cs1, Init0, Init, DeInit0, DeInit).
c_functions([_|Fs], Cs, Init0, Init, DeInit0, DeInit) :-
	c_functions(Fs, Cs, Init0, Init, DeInit0, DeInit).

%--------------------------------------------------------------------------
% Support

gather_declarations(Resource, SourceFile) :-
	absolute_file_name(SourceFile, AbsSourceFile, [access(exist),file_type(source)]),
	open(AbsSourceFile, read, Stream),
        call_cleanup(gather_declarations_stream(Stream, Resource), close(Stream)).

gather_declarations_stream(Stream, Resource) :-
	repeat,
	  read(Stream, Raw),
	  flids_intercept_command(Raw),
	  expand_term(Raw, Expanded),
	  expanded_member(Term, Expanded),
	  flids_intercept(Term, Resource),
	  Term = end_of_file,
          !.

expanded_member(Term, Expanded) :-
        expanded_member1(Expanded, Term).

expanded_member1(Expanded, Term) :-
        var(Expanded), !,
        Term = Expanded.
expanded_member1(Expanded, _Term) :-
        Expanded = [], !,
        fail.
expanded_member1(Expanded, Term) :-
        Expanded = [_|_], !,
        member(Term, Expanded).
expanded_member1(Expanded, Term) :-
        Term = Expanded.

flids_intercept(Term, Resource) :-
	foreign_decl(Term, Resource,SaveAs),
	\+call(SaveAs), !,
	assertz(SaveAs).
flids_intercept(Term, _) :-
	flids_intercept_command(Term).

flids_intercept_command(:-(Command)) :- !,
	flids_command(Command).
flids_intercept_command(_).

flids_command(op(Prec,Ass,Ops)) :- !,
	op(Prec, Ass, Ops).
flids_command(foreign_type(Types)) :- !,
	flids_foreign_typedefs(Types).
flids_command((Com1,Com2)) :- !,
	flids_command(Com1),
	flids_command(Com2).
flids_command(_).

%  flids_foreign_typedefs(+Typedefns)
%  Typedefns is a bunch of type definitions for which I must write
%  corresponding C typdefs.

flids_foreign_typedefs(','(Type1,Type2)) :-
	flids_foreign_typedefs(Type1),
	flids_foreign_typedefs(Type2).
flids_foreign_typedefs(Name=Type) :-
	assertz(foreign_type_(Name,Type)).


%% ------------------------------------------------------------------------
%% [PM] This so much begs for a real command line parser/regexp
%% package that I am not even going to bother to do this fast or
%% general.
%% Command line syntax
%% Args matching LHS=RHS becomes =(LHS, Re-parsed(RHS))
%% thus --foo=bar=baz becomes =('--foo', (bar=baz))
%% All args (and LHS, RHS above) are stripped of surrounding
%% (matching) single and double quotes. This is mainly a workaround
%% for the feature challanged shell used on Windows.
%%
argv_parse(Argv, Terms) :-
	(   foreach(A,Argv),
	    foreach(T,Terms)
	do  argv_symbol_to_term(A, T)
	).

%% Template looks like (Pattern -> Then; Else) If a term on Argv
%% unifies with Pattern then the term does not go on Rest. Instead the
%% unification specified by Then (Then is a term X=Y) is done. If no
%% Argv term unifies with Pattern then the unification performed by
%% Else is performed.  Either Then or Else can be ommitted.
%%
argv_parse(Argv, Rest, Templates) :-
	argv_parse(Argv, Terms),
	argv_parse_templates(Terms, Templates, Rest).


argv_parse_templates([], Templates, Rest) :-
	Rest = [],
	argv_template_defaults(Templates).
argv_parse_templates([Term|Terms], Templates0, Rest) :-
	(   select(Template, Templates0, Templates),
	    argv_template_split(Template, Pattern,Then,_Else),
	    Term = Pattern
	->  Then = (X=Y),
	    X=Y,
	    argv_parse_templates(Terms, Templates, Rest)
	;   Rest = [Term|Rest1],
	    argv_parse_templates(Terms, Templates0, Rest1)
	).

argv_template_defaults([]).
argv_template_defaults([Template|Templates]) :-
	argv_template_split(Template,_Pattern,_Then,Else),
	Else = (X=Y),
	X=Y,
	argv_template_defaults(Templates).


argv_template_split((Pattern0 -> Then0;Else0), Pattern,Then,Else) :- !,
	Pattern=Pattern0,
	Then=Then0,
	Else=Else0.
argv_template_split((Pattern0 -> Then0), Pattern,Then,Else) :- !,
	Pattern = Pattern0,
	Then=Then0,
	Else=(x=x).
argv_template_split((Pattern0;Else0), Pattern,Then,Else) :- !,
	Pattern = Pattern0,
	Then=(x=x),
	Else=Else0.

argv_symbol_to_term(Arg, Term) :-
	atom_codes(Arg, Codes),
	argv_codes_to_term(Codes, Term).

argv_codes_to_term(Codes, Term) :-
	append([0'\'|Codes1],[0'\'],Codes), !,
	argv_codes_to_term(Codes1, Term).
argv_codes_to_term(Codes, Term) :-
	append([0'"|Codes1],[0'"],Codes), !,
	argv_codes_to_term(Codes1, Term).
argv_codes_to_term(Codes, Term) :-
	argv_codes_split_at(Codes, 0'=, BeforeCodes, AfterCodes), !,
	Term = (Before=After),
	atom_codes(Before, BeforeCodes),
	argv_codes_to_term(BeforeCodes, Before),
	argv_codes_to_term(AfterCodes, After).
argv_codes_to_term(Codes, Term) :-
	atom_codes(Term, Codes).

argv_codes_split_at(Codes, Split, Before, After) :-
	append(Before, [Split|After], Codes), !.

sp_main_prefix(SP_MAIN_PREFIX) :-
	prolog:'$SP_MAIN_PREFIX'(SP_MAIN_PREFIX).

:- spld_prepare_resource_table/0 is throwing.
spld_prepare_resource_table :-
        % [PM] 4.3.3 halt with an error when something goes wrong.
        asserta(use_portray_hook),
	catch(spld_prepare_resource_table0, E, (print_message(error, E), halt(1))),
        !, % redundant
        halt.
spld_prepare_resource_table :-
        % [PM] 4.3.3 exit with error code on failure.
        halt(1).


spld_prepare_resource_table0 :-
	prolog_flag(argv,Argv0),
	argv_parse(Argv0, FList,
		   [
		    (('--outfile'=OutFile);OutFile=user),
		    (('--link-data') -> CLink=yes), (('--no-link-data') -> CLink=no),
		    (('--verbose') -> Verbose=yes; Verbose=no ), % currently not used but generated by spld
		    (('--append') -> Append=yes; Append=no)
		   ]
		  ),
	(   var(CLink) -> CLink = no
	;   true
	),
	Options = [append(Append), outfile(OutFile), clink(CLink)],
	prolog_prepare_resource_table(FList, Options).

prolog_prepare_resource_table(FList, Options) :-
	memberchk(outfile(OutFile), Options),
	( OutFile == user ->
	    S = user
	; memberchk(append(yes), Options) ->
	    open(OutFile, append, S)
	; otherwise ->
	    open(OutFile, write, S)
	),
	call_cleanup(prolog_prepare_resource_table1(FList, S, Options),
	( OutFile == user ->
	    true
	; otherwise ->
	    close(S)
	)).

prolog_prepare_resource_table1(FList, S, Options) :-
	write(S, '#include <sicstus/sicstus.h>\n\n'),
	prolog_prepare_resource_table_forward_declarations(FList, S, Options),
	%% sp_pre_linkage[]
	write(S, '\nSP_MainFun *sp_pre_linkage[] = {\n'),
	prolog_prepare_resource_table_pre_linkage(FList, S, Options),
	write(S, '0};\n\n'),
	prolog_prepare_data_resource_table_pre_map(FList, S, Options),
	write(S, '\n'),
	%% sp_pre_map[]
	write(S, 'char *sp_pre_map[] = {\n'),
	prolog_prepare_resource_table_pre_map(FList, S, Options),
	write(S, '0};\n').

prolog_prepare_data_resource_table_pre_map([], _S, _Options).
prolog_prepare_data_resource_table_pre_map([Ent|FList], S, Options) :-
	( Ent = (_DataFile=ResourceName) -> % data resource
	    sp_ensure_c_name(ResourceName, HexName),
	    /*

	    Construct a link like the following. This is used (by url.c) to
	convert the pointer to the name into a pointer to the resource
	header.
	
	static struct {
		       void* res;
		       const char name[strlen(<HEXNAME>)+1];
		       } SICStus_NAMED_DATA_RESOURCE_<HEXNAME> = {(void*)&SICStus_DATA_RESOURCE_<HEXNAME>, <HEXNAME>};

	    The expression (void*)&SICStus_DATA_RESOURCE_<HEXNAME> is 0 for
	resources that cannot be linked to directly from C (e.g., Win32
							   resources).
	
	    */
	atom_codes(HexName, HexCodes),
	    length(HexCodes, HexName_Len),
	    format(S, 'static struct {\n   void *res;\n', []),
	    format(S, '   char name[~d+1];\n', [HexName_Len]),
	    format(S, '} SICStus_NAMED_DATA_RESOURCE_~a = {',  [HexName]),
	    ( memberchk(clink(yes), Options) ->
		format(S, '(void*)&SICStus_DATA_RESOURCE_~a', [HexName])
	    ; otherwise ->	% no direct link in C
		write(S, '0')
	    ),
	    format(S, ', "~a"};\n', [HexName])
	; atom(Ent) ->		% foreign resource
	    true
	; otherwise ->
	    fail		% later give an error
	),
	prolog_prepare_data_resource_table_pre_map(FList, S, Options).



prolog_prepare_resource_table_forward_declarations([], _S, _Options).
prolog_prepare_resource_table_forward_declarations([Ent|FList], S, Options) :-
	( Ent = (DataFile=ResourceName) -> % data resource
	    ( memberchk(clink(yes), Options) ->
		sp_ensure_c_name(ResourceName, HexName),
                %% [PM] 3.12.5+, 4.0 SPRM 9340
                print_lines([
                             'SP_BEGIN_DECL'-[],
                             '/* This is the data header of the data resource ~w from ~w */'-[ResourceName, DataFile],
                             %% [PM] 4.2 Added correct dimension ([64]) so exactly match the declaration generated by resgen:to_cc/4
                             'extern const char SICStus_DATA_RESOURCE_~a[][64];'-[HexName],
                             'SP_END_DECL'-[],
                             ''-[]
                             ], S),
		true
	    ; otherwise ->	% no direct linking to the data resource in C
		true
	    )
	; atom(Ent) ->		% foreign resource
	    sp_ensure_c_name(Ent, HexName),
	    sp_main_prefix(SP_MAIN_PREFIX),
            %% [PM] 3.12.5+, 4.0 SPRM 9340
	    %% format1(S, '#ifndef GLUE_~a\n', [HexName], spld),
	    %% format1(S, '/* This is the setup routine of the foreign resource ~w */\n', [Ent], spld),
	    %% format1(S, 'extern SPGLUEEXP SP_MainFun ~a~a;\n#endif\n', [SP_MAIN_PREFIX, HexName], spld)
            print_lines([
                         '#ifndef GLUE_~a'-[HexName],
                         'SP_BEGIN_DECL'-[],
                         '/* This is the setup routine of the foreign resource ~w */'-[Ent],
                         'extern SPGLUEEXP SP_MainFun ~a~a;'-[SP_MAIN_PREFIX, HexName],
                         'SP_END_DECL'-[],
                         '#endif /* GLUE_~a */'-[HexName]],
                        S)
	; otherwise ->
	    fail		% later give an error
	),
	prolog_prepare_resource_table_forward_declarations(FList, S, Options).

prolog_prepare_resource_table_pre_linkage([], _S, _Options).
prolog_prepare_resource_table_pre_linkage([Ent|FList], S, Options) :-
	( Ent = (DataFile=ResourceName) -> % data resource
	    IsDataResource = yes,
	    F = ResourceName
	; atom(Ent) ->		% foreign resource
	    IsDataResource = no,
	    F = Ent
	; otherwise ->
	    fail		% later give an error
	),
	sp_main_prefix(SP_MAIN_PREFIX),
	sp_ensure_c_name(F, HexName),
	( IsDataResource == yes ->
	    format(S, '   0, /* data resource "~a" from ~w */\n', [F, DataFile])
	; otherwise ->
	    format(S, '   ~a~a,\n', [SP_MAIN_PREFIX, HexName]) 
	),
	prolog_prepare_resource_table_pre_linkage(FList, S, Options).


prolog_prepare_resource_table_pre_map([], _S, _Options).
prolog_prepare_resource_table_pre_map([Ent|FList], S, Options) :-
	( Ent = (DataFile=ResourceName) -> % data resource
	    %% format1(S, '	"~a", /* data from ~w */\n', [ResourceName, DataFile], spld)
	    sp_ensure_c_name(ResourceName, HexName),
	    format(S, '   SICStus_NAMED_DATA_RESOURCE_~a.name, /* data from ~w */\n', [HexName, DataFile])
	; atom(Ent) ->		% foreign resource
	    F=Ent,
            %% Explicit cast from ".." to char* to suppress C++ warnings
	    format(S, '   (char*)"~a",\n', [F])
	; otherwise ->
	    fail		% later give an error
	),
	prolog_prepare_resource_table_pre_map(FList, S, Options).

%% [PM] 3.9.1b1 This routine exists in library(resgen) AND Bips/flids.pl AND Emulator/foreign.c AND Utils/splfr.pl.in
%%              They *must* produce the same result!
sp_ensure_c_name(Name, HexName) :-
	atom_codes(Name, Codes),
	(   foreach(C,Codes),
	    fromto(HexCodes,HexCodes1,HexCodes2,[])
	do  sp_ensure_c_name_code(C, HexCodes1, HexCodes2)
	),
	atom_codes(HexName, HexCodes).

sp_ensure_c_name_code(C, HexCodes,HexCodes1) :-
	(   C>="0", C=<"9"
	;   C>="a", C=<"z"
	;   C>="A", C=<"Z"
	), !,
	HexCodes = [C|HexCodes1].
sp_ensure_c_name_code(C, [0'_, 0'0, 0'x, MSD, LSD|HexCodes],HexCodes) :- % "_0x"
	HiNibble is (C >> 4) /\ 0xF,
	LoNibble is C /\ 0xF,
	hexdigit(HiNibble, MSD),
	hexdigit(LoNibble, LSD).

hexdigit(C, D) :- C<10, !, D is "0"+C.
hexdigit(C, D) :- D is "A"+C-10.


%--------------------------------------------------------------------------
% Support
:- mode foreign_decl(+,+,- /* -(0) */).
foreign_decl(foreign_resource(R,Functions), R, foreign_resource_(R,Functions)).
foreign_decl(foreign(CName,Spec), _R, foreign_(CName,'c',Spec)).
foreign_decl(foreign(CName,Lang,Spec), _R, foreign_(CName,Lang,Spec)).

