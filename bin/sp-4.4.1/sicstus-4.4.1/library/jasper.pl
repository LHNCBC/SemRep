%%% -*- Mode: Prolog; Module: jasper; prolog-indent-width:3-*-
%%% CVS:            $Id$

/* Copyright (C) 1999, Swedish Institute of Computer Science */

%   File       : jasper.pl
%   Author     : Jesper Eskilson
%   Maintainer : Per Mildner
%   Updated    : 25 September 2000
%   Purpose    : Java interface

:- module(jasper,
	  [
	   jasper_initialize/1,
	   jasper_initialize/2,
	   jasper_deinitialize/1,
	   jasper_new_object/5, % 3.8.5 new API, same arity
           jasper_null/2,       % 3.8.6 Create a null object reference
           jasper_call/4,       % 3.8.5 new API
	   jasper_create_local_ref/3, % 3.8.5
	   jasper_create_global_ref/3,
	   jasper_delete_global_ref/2,
	   jasper_delete_local_ref/2,
           jasper_is_jvm/1,     % 3.8.5
	   jasper_is_object/1,  % 3.8.5
	   jasper_is_object/2,
           jasper_is_null/2,    % 3.8.6
	   jasper_is_same_object/3,
	   jasper_is_instance_of/3,
           jasper_object_class_name/3 % 3.8.5
	   ]).

:- use_module(library(types), [
	illarg/3,
	illarg/4
	]).

:- use_module(library(lists), [
	delete/3,
	is_list/1,
        suffix/2,
        reverse/2
	]).

%%%%%%%%%%%%%%%%%% 3.8.5 NEW stuff below

%% jasper_call(+JVM, +MethodSpec, +PredSpec, +Args)
jasper_call(JVM, MethodSpec, PredSpec, Args) :-
   Goal = jasper_call(JVM,MethodSpec,PredSpec,Args),
   must_be_jvm(JVM, Goal,1),
   must_be_method_spec(MethodSpec, Goal,2),
   MethodSpec = method(Class, MethodName, Flags),
   jasperi_make_typesig(PredSpec, TypeSig, Goal,3, Flags),
   functor(PredSpec, _PredName, Arity),
   ( ( nonvar(Args),
       functor(Args, _Name, Arity1),
       Arity1==Arity ) ->
       true
   ; otherwise ->
       %% NOTE: need better error message
       illarg(domain(term,foreign_arg_spec(java)), Goal,4, Args)
   ),
   ( Flags == [instance] ->
       jasperi_call_instance1(JVM, MethodName, TypeSig, PredSpec, Args)
   ; otherwise ->
       jasperi_call_static1(JVM, Class, MethodName, TypeSig, PredSpec, Args)
   ).

jasperi_make_typesig(MethodDesc,TypeSig1, _Culprit,_ArgNo, Flags) :-
   ground(MethodDesc),
   functor(MethodDesc, MethodName, _),
   make_type_signature(MethodName,MethodDesc,TypeSig,Flags),
   !,
   TypeSig1 = TypeSig.

jasperi_make_typesig(MethodDesc,_TypeSig, Culprit,ArgNo, _Flags) :-
   illarg(domain(term,foreign_arg_spec(java)),Culprit,ArgNo,MethodDesc).


%%%%%%%%%%%%%%%%%% 3.8.5 NEW stuff above

%%% This should be replaced be a actual test that the references are valid.
%%% Should fail for non objref. See must_be_object
jasper_objref(_JVM,Object) :- nonvar(Object),
   Object = '$java_object'(Ref),
   integer(Ref).

jasper_objref(_JVM,Object,Ref) :- nonvar(Object),
   Object = '$java_object'(Ref0),
   integer(Ref0),
   Ref = Ref0.


jasper_jvmref(JVM) :- nonvar(JVM),
   JVM = '$jvm'(X),
   integer(X).

%% jasper_object_class_name(+JVM, +Object, -Class)
jasper_object_class_name(JVM, Object, Class) :-
   Goal = jasper_object_class(JVM, Object, Class),
   must_be_jvm(JVM, Goal,1),
   must_be_object(JVM, Object, Goal,2),
   jasperi_object_class_name(JVM, Object, Class).


%%% If the object reference is -1, an error occured.
jasper_objref_isinvalid(_JVM,Object) :-
   jasper_objref(dummy,Object,Ref),
   jasper_objref_isinvalid1(Ref).

jasper_objref_isinvalid1(0).    % NULL_OBJREF in jasper.c
jasper_objref_isinvalid1(-1).   % !NULL_OBJREF

%%% Succeeds if Object is a proper Java object reference, otherwise an
%%% exception is raised.
must_be_object(JVM,Object,_Goal,_ArgNo) :-
	jasper_objref(JVM,Object),
	!.
must_be_object(_JVM,Object,Goal,ArgNo) :-
	illarg(domain(term,'object reference'),Goal,ArgNo,Object).

must_be_non_null_object(JVM,Object,_Goal,_ArgNo) :-
        jasper_objref(JVM,Object,Ref),
        Ref\==0,
	!.
must_be_non_null_object(_JVM,Object,Goal,ArgNo) :-
	illarg(domain(term,'non-null object reference'),Goal,ArgNo,Object).

must_be_jvm(JVM,Goal) :-
   ArgNo = 1,                   % common case
   must_be_jvm(JVM,Goal,ArgNo).

must_be_jvm(JVM,_Goal,_ArgNo) :-
	jasper_jvmref(JVM),
	!.
must_be_jvm(JVM,Goal,ArgNo) :-
	illarg(domain(term,jvm_reference),Goal,ArgNo,JVM).


must_be_method_spec(MethodSpec, _Goal,_ArgNo) :-
   ground(MethodSpec),
   MethodSpec = method(Class, MethodName, Flags),
   atom(Class),
   atom(MethodName),
   Flags = [Kind],
   ( Kind = instance ; Kind = static ),
   !.
must_be_method_spec(MethodSpec, Goal,ArgNo) :-
   illarg(domain(term,'method description'), Goal,ArgNo, MethodSpec).

must_be_arglist(ArgList, Goal, ArgNo) :-
	(   foreach(Arg,ArgList),
	    param([Goal,ArgNo])
	do  Arg =.. [Functor,_ArgRaw],
	    (   jasper_argmap(Functor,_IntRep) -> true ; 
		jasper_arg_specs(ArgSpecs),
		illarg(domain(term,one_of(ArgSpecs)),Goal,ArgNo,Arg)
	    )
	).

get_option(Template, Options0,Options1) :-
   memberchk(Template, Options0),
   delete(Options0, Template, Options1).

get_option(Template, Options0,Options1, _Default) :-
   memberchk(Template, Options0), !,
   delete(Options0, Template, Options1).
get_option(Template, Options0,Options1, Default) :-
   Template = Default,
   Options1 = Options0.

%% [PM] 3.8.5 NOTE: Consider to make this public
jasper_existing_jvm(JVM) :-
   jasperi_jvm(JVM0),           % if no existing JVM then JVM0 is []
   JVM0 = '$jvm'(_),
   JVM = JVM0.

%%% jasper_initialize(+Options,-JVM)
jasper_initialize(Options0,JVM) :-
   Goal = jasper_initialize(Options0,JVM),
   ( is_list(Options0) ->       % [PM] 3.10.1 
       true
   ; otherwise ->
       illarg(domain(list(callable), jni_option), Goal, 1)
   ),
   %% [PM] 3.8.5 New options to make it possible to obtain an already running JVM
   %% if_exists: ok (default, init if not there), error, fail (fail if there)
   %% if_not_exists: ok (the default), error, fail (fail if not there)
   get_option(if_exists(IfExistOpt), Options0,Options1, if_exists(ok)),
   get_option(if_not_exists(IfNotExistOpt), Options1,Options2, if_not_exists(ok)),
   ( jasper_existing_jvm(JVM0) ->       % there is already a JVM
       ( IfExistOpt == ok ->
           JVM = JVM0
       ; IfExistOpt == fail ->
           fail
       ; IfExistOpt == error ->
           throw(java_exception('Java VM already initialized'))
       ; otherwise ->
           illarg(domain(atom,one_of([ok,fail,error])),Goal,1)
       )
   ; otherwise ->               % no existing JVM
       ( IfNotExistOpt == ok ->
           true
       ; IfNotExistOpt == fail ->
           fail
       ; IfNotExistOpt == error ->
           throw(java_exception('Java VM does not exist'))
       ; otherwise ->
           illarg(domain(atom,one_of([ok,fail,error])),Goal,1)
       ),
       jasper_initialize1(Options2,JVM)
   ).


jasper_initialize1(Options0,JVM) :-
   ( get_option(classpath(ClassPath), Options0,Options) ->
       ( is_list(ClassPath) ->
	   (   foreach(CP,ClassPath),
	       foreach(AP,Path)
	   do  absolute_file_name(CP,AP)
	   )
       ; otherwise ->
           Path = ClassPath
       )
   ; otherwise ->
       Options = Options0,
       Path = ''
   ),
   length(Options,NumOpts),
   jasperi_initialize(NumOpts,Path,Options,JVM),
   %% [PM] jasperi_initialize now always creates the one and only SICStus object.
   %% jasper_new_object(JVM,'se/sics/jasper/SICStus',_SICStusRef).
   true.



%%% jasper_initialize(-JVM)
jasper_initialize(JVM) :-
	jasperi_initialize(0,'',[],JVM).

%%% jasper_deinitialize(+JVM)
%%% Fails if the JVM could not be deinitialized.
jasper_deinitialize(JVM) :-
	Goal = jasper_deinitialize(JVM),
	must_be_jvm(JVM,Goal),
	(   jasperi_deinitialize(JVM,0) -> true
	;   illarg(system('Could not deinitialize JVM'),Goal,1,JVM)
	).


jasper_new_object(JVM,Class,PredSpec_or_TypeSig,Args,Object) :-
   %% detect deprecated API jasper_new_object(+JVM,+Class,+TypeSig,+Args,-Object)
   nonvar(Args),
   (
     Args = [], PredSpec_or_TypeSig == '()V'
   ; Args = [_|_], atom(PredSpec_or_TypeSig)
   ),
   !,
   jasper_new_object_old(JVM,Class,PredSpec_or_TypeSig,Args,Object).

jasper_new_object(JVM, Class, PredSpec, Args, Object) :- % 3.8.5 New API
   Goal = jasper_new_object(JVM, Class, PredSpec, Args, Object),
   jasper_new_object1(JVM, Class, PredSpec, Args, Object, Goal).


%% convert old style arguments to new API
jasper_new_object_old(JVM,Class,TypeSig,Args,Object) :-
   Goal = jasper_new_object(JVM,Class,TypeSig,Args,Object),
   must_be_jvm(JVM,Goal),
   must_be_arglist(Args,Goal,4),
   jasperi_convert_old_argmap(Args, TypeSig, ArgTypes, ArgValues, Goal,3),
   PredSpec =.. [init|ArgTypes],
   NewArgs =.. [init|ArgValues],
   jasper_new_object1(JVM, Class, PredSpec, NewArgs, Object, Goal).


jasper_new_object1(JVM, Class, PredSpec, Args, Object, Goal) :-
   must_be_jvm(JVM, Goal,1),
   jasperi_make_typesig(PredSpec, TypeSig, Goal,3, [static]),
   functor(PredSpec, _PredName, Arity),
   ( ( nonvar(Args),
       functor(Args, _Name, Arity1),
       Arity1==Arity ) ->
       true
   ; otherwise ->
       %% NOTE: need better error message
       illarg(domain(term,foreign_arg_spec(java)), Goal,4, Args)
   ),
   %% The typesig indicates that it is a void method but the actual
   %% call behaves like a static method that returns an object of the
   %% class Class. Prepend this info to the PredSpec after creating
   %% the (void) typesig.
   PredSpec =.. [PredName|ArgTypes],
   InitSpec =.. [PredName,[-object(Class)]|ArgTypes],
   Args =.. [ArgName|Args1],
   InitArgs =.. [ArgName,Object|Args1],
   MethodName = '<init>',       % handled specially in jasperi_call_static1
   jasperi_call_static1(JVM,Class,MethodName,TypeSig,InitSpec,InitArgs).

%%% jasper_create_global_ref(+JVM,+Ref,-GlobalRef)
jasper_create_global_ref(JVM,Ref,GlobalRef) :-
	Goal = jasper_create_global_ref(JVM,Ref,GlobalRef),
	must_be_jvm(JVM,Goal),
	must_be_non_null_object(JVM,Ref,Goal,2),
	%% [PM] 3.8.5 Was: ( nonvar(GlobalRef) -> illarg(var,Goal,3) ; true ),
	jasperi_create_global_ref(JVM,Ref,GlobalRef0),
	( jasper_objref_isinvalid(JVM,GlobalRef0) ->
	    illarg(system('Could not create global reference'), Goal,0)
	;   GlobalRef = GlobalRef0
	).

%%% jasper_create_local_ref(+JVM,+Ref,-LocalRef)
jasper_create_local_ref(JVM,Ref,LocalRef) :-
	Goal = jasper_create_local_ref(JVM,Ref,LocalRef),
	must_be_jvm(JVM,Goal),
	must_be_non_null_object(JVM,Ref,Goal,2),
	jasperi_create_local_ref(JVM,Ref,LocalRef0),
	( jasper_objref_isinvalid(JVM,LocalRef0) ->
	    illarg(system('Could not create local reference'), Goal,0)
	;   LocalRef = LocalRef0
	).

%%% jasper_delete_global_ref(+JVM,+GlobalRef)
jasper_delete_global_ref(JVM,GlobalRef) :-
	Goal = jasper_delete_global_ref(JVM,GlobalRef),
	must_be_jvm(JVM,Goal),
	must_be_object(JVM,GlobalRef,Goal,2),
	jasperi_delete_global_ref(JVM,GlobalRef).

%%% jasper_delete_local_ref(+JVM,+LocalRef)
jasper_delete_local_ref(JVM,LocalRef) :-
   Goal = jasper_delete_local_ref(JVM,LocalRef),
   must_be_jvm(JVM,Goal),
   must_be_object(JVM,LocalRef,Goal,2),
   jasperi_delete_local_ref(JVM,LocalRef).

%%% jasper_is_jvm(+JVM).
jasper_is_jvm(JVM) :-
   jasper_jvmref(JVM).


%%% jasper_is_object(+Object).
jasper_is_object(Object) :-
   jasper_objref(dummy,Object).

%%% jasper_is_object(+JVM,+Object).
jasper_is_object(JVM,Object) :-
   jasper_objref(JVM,Object).

%% jasper_is_null(+JVM, +Object)
jasper_is_null(JVM, Object) :-
   Goal = jasper_is_null(JVM, Object),
   must_be_jvm(JVM, Goal, 1),
   jasper_objref(JVM,Object,Ref),
   Ref==0.

%% jasper_null(+JVM, -NullObject).
jasper_null(JVM, NullObject) :-
   must_be_jvm(JVM, jasper_null(JVM, NullObject), 1),
   NullObject = '$java_object'(0).



%%% jasper_is_same_object(+JVM,+Object1,+Object2)
jasper_is_same_object(JVM,Obj1,Obj2) :-
	Goal = jasper_is_same_object(JVM,Obj1,Obj2),
	must_be_object(JVM,Obj1,Goal,1),
	must_be_object(JVM,Obj2,Goal,2),
	jasperi_is_same_object(JVM,Obj1,Obj2,1).

%%% jasper_is_instance_of(+JVM,+Object,+ClassName)
jasper_is_instance_of(JVM,Object,ClassName) :-
	Goal = jasper_is_instance_of(JVM,Object,ClassName),
	must_be_object(JVM,Object,Goal,2),
	jasperi_is_instance_of(JVM,Object,ClassName,Result),
	( Result = -1 ->
	    illarg(system('class not found'),Goal,3)
	;   Result = 1
	).

%%% BEGIN flids.pl stuff
%% [PM] 4.3.3 flids.pl is gone, so the below code now is the only remaining copy.
%% [PM] 3.8.5 The original to this is in Bips/flids.pl. Keep them in synch!
make_type_signature(JavaMethod,Decl,TypeSig,Flags) :-
   %% If the method is an instance method, then the first argument is
   %% the method instance object and should not be included in the
   %% type signature.
   ( memberchk(instance,Flags) ->
       (   Decl =.. [_PredName,+object(_Class)|Specs] -> true
       ;   illarg(domain(term,one_of([object(_)])),
                         foreign(JavaMethod,java,Decl), 3, Decl)
       )
   ;   Decl =.. [_PredName|Specs]
   ),
   make_type_signature0(Specs,Specs0,RVal),
   append("(",Specs0,Specs1),
   append(Specs1,")",Specs2),
   append(Specs2,RVal,Specs3),
   atom_codes(TypeSig,Specs3).

make_type_signature0([],[],"V").
make_type_signature0([-Spec|Specs],Specs1,Rval) :- !,
	convert_type_to_signature(-Spec,SpecSig),
	append(SpecSig,Specs0,Specs1),
	make_type_signature0(Specs,Specs0,Rval).
make_type_signature0([+Spec|Specs],Specs1,Rval) :- !,
	convert_type_to_signature(+Spec,SpecSig),
	append(SpecSig,Specs0,Specs1),
	make_type_signature0(Specs,Specs0,Rval).
make_type_signature0([[-Spec]|Specs],Specs0,Rval) :- !,
	convert_type_to_signature([-Spec],Rval),
	make_type_signature0(Specs,Specs0,_).

%% [PM] 3.8.5 made steadfast and determinate by breaking out the table pred
convert_type_to_signature(Type,TypeSig) :-
   convert_type_to_signature1(Type, TypeSig0), !,
   TypeSig = TypeSig0.
% Default for [-Type] is same as +Type
convert_type_to_signature([-Type],TypeSig) :- !,
   convert_type_to_signature1(+Type,TypeSig).
% Default is to use same for '-' as for '+'
convert_type_to_signature(-Type,TypeSig) :- !,
   convert_type_to_signature1(+Type,TypeSig).


convert_type_to_signature1(+byte,"B") :- !.
convert_type_to_signature1(+char,"C") :- !.
convert_type_to_signature1(+short,"S") :- !.
convert_type_to_signature1(+integer,"I") :- !.
convert_type_to_signature1(+long,"J") :- !.
convert_type_to_signature1(+float,"F") :- !.
convert_type_to_signature1(+double,"D") :- !.
convert_type_to_signature1(+boolean,"Z") :- !.

convert_type_to_signature1(-byte,"Ljava/lang/Byte;") :- !.
convert_type_to_signature1(-char,"Ljava/lang/Character;") :- !.
convert_type_to_signature1(-short,"Ljava/lang/Short;") :- !.
convert_type_to_signature1(-integer,"Ljava/lang/Integer;") :- !.
convert_type_to_signature1(-long,"Ljava/lang/Long;") :- !.
convert_type_to_signature1(-float,"Ljava/lang/Float;") :- !.
convert_type_to_signature1(-double,"Ljava/lang/Double;") :- !.
convert_type_to_signature1(-boolean,"Ljava/lang/Boolean;") :- !.

convert_type_to_signature1(+object(Class),TypeSig) :- !,
   atom_codes(Class,ClassStr),
   append("L",ClassStr,TypeSig0),
   append(TypeSig0,";",TypeSig).

convert_type_to_signature1([-object(Class)],TypeSig) :- !,
   atom_codes(Class,ClassStr),
   append("L",ClassStr,TypeSig0),
   append(TypeSig0,";",TypeSig).	

convert_type_to_signature1(+term,"Lse/sics/jasper/SPTerm;") :- !.
convert_type_to_signature1(+atom,"Lse/sics/jasper/SPCanonicalAtom;") :- !.
convert_type_to_signature1(+codes,"Ljava/lang/String;") :- !.
convert_type_to_signature1(+chars,"Ljava/lang/String;") :- !. % [PM] 4.0.2+ legacy
convert_type_to_signature1(+string,"Ljava/lang/String;") :- !.

convert_type_to_signature1(-atom,"Lse/sics/jasper/SPTerm;") :- !. % [PM] 3.8.5 Was SPCanonicalAtom
convert_type_to_signature1(-codes,"Ljava/lang/StringBuffer;") :- !.
convert_type_to_signature1(-chars,"Ljava/lang/StringBuffer;") :- !. % [PM] 4.0.2+ legacy
convert_type_to_signature1(-string,"Ljava/lang/StringBuffer;") :- !.

convert_type_to_signature1([-codes],"Ljava/lang/String;") :- !.
convert_type_to_signature1([-chars],"Ljava/lang/String;") :- !. % [PM] 4.0.2+ legacy
convert_type_to_signature1([-string],"Ljava/lang/String;") :- !.

%%%% END OF flids.pl stuff

%% [PM] 4.1.3 SPIDER/xref
:- public jasper_convert_call/2.

%% [PM] 3.12.9, 4.0.2+ Porting help (not exported) for converting
%% old-style jasper_call_instance/jasper_call_static into new
%% jasper_call/4goals.
jasper_convert_call(jasper_call_instance(JVM,Object,MethodName,TypeSig,Args,RetVal), NewCall) :-
	Goal = jasper_convert_call(jasper_call_instance(JVM,Object,MethodName,TypeSig,Args,RetVal), NewCall),
	% must_be_jvm(JVM,Goal),
	% must_be_object(JVM,Object,Goal,1),
	must_be_arglist(Args,Goal,1),
        %% convert to new API
        jasperi_convert_old_argmap(Args, TypeSig, ArgTypes, ArgValues, Goal,1),
        jasper_convert_type_sig_retvalue(TypeSig, RetType),
        Class = 'Unknown Class',
        ( RetType == void ->
            PredSpec =.. [MethodName,+object(Class)|ArgTypes],
            NewArgs =.. [MethodName,Object|ArgValues]
        ; otherwise ->
            PredSpec =.. [MethodName,+object(Class),[-RetType]|ArgTypes],
            NewArgs =.. [MethodName,Object,RetVal0|ArgValues]
        ),
        RetVal = RetVal0,
        NewCall = jasper_call(JVM,method(Class, MethodName, [instance]),PredSpec,NewArgs).
jasper_convert_call(jasper_call_static(JVM,Class,MethodName,TypeSig,Args,RetVal), NewCall) :-
	Goal = jasper_convert_call(jasper_call_static(JVM,Class,MethodName,TypeSig,Args,RetVal), NewCall),
	%% must_be_jvm(JVM,Goal),
	must_be_arglist(Args,Goal,1),
        %% convert to new API
        jasperi_convert_old_argmap(Args, TypeSig, ArgTypes, ArgValues, Goal,1),
        jasper_convert_type_sig_retvalue(TypeSig, RetType),
        ( RetType == void ->
            PredSpec =.. [MethodName|ArgTypes],
            NewArgs =.. [MethodName|ArgValues]
        ; otherwise ->
            PredSpec =.. [MethodName,[-RetType]|ArgTypes],
            NewArgs =.. [MethodName,RetVal0|ArgValues]
        ),
        RetVal = RetVal0,
        NewCall = jasper_call(JVM,method(Class, MethodName, [static]),PredSpec,NewArgs).


%%% jasper_int_mangle_args/2 converts the argument-list into something which is
%%% easier to parse from C.
%%% [jboolean(true),jint(42),jshort(14)] -> [1,2,3] & [true,42,14]
%%% The types are mapped according to the predicate jasper_argmap/2.

jasper_argmap(jboolean,1).
jasper_argmap(jbyte,2).
jasper_argmap(jchar,3).
jasper_argmap(jdouble,4).
jasper_argmap(jfloat,5).
jasper_argmap(jint,6).
jasper_argmap(jlong,7).
jasper_argmap(jshort,8).
jasper_argmap(jobject,9).
jasper_argmap(jstring,10).
%%% [more to come...]

jasper_arg_specs([jboolean(_),
		  jbyte(_),
		  jchar(_),
		  jdouble(_),
		  jfloat(_),
		  jint(_),
		  jlong(_),
		  jshort(_),
		  jobject(_),
		  jstring(_)]).

%% 3.8.5 convert old style arg list to new style + arg values
jasperi_convert_old_argmap(Olds, TypeSig, News0, ArgValues0, _Goal,_ArgNo) :-
   atom(TypeSig),
   atom_codes(TypeSig, [0'(|SigCodes]),
   jasperi_convert_old_argmap1(Olds, SigCodes, News, ArgValues),
   !,
   News0 = News,
   ArgValues0 = ArgValues.
jasperi_convert_old_argmap(_Olds, TypeSig, _News0, _ArgValues0, Goal,ArgNo) :-
   %% [PM] 3.8.6 added illarg error instead of silent fail.
   illarg(domain(term,'type signature'), Goal, ArgNo, TypeSig).

jasperi_convert_old_argmap1([], Sig, [], []) :-
   %% sanity check
   Sig = [0')|_RetSig].
jasperi_convert_old_argmap1([Old|Olds], Sig, [+New|News], [ArgValue|ArgValues]) :-
   jasperi_convert_old_arg(Old, New, Sig,Sig1, ArgValue),
   jasperi_convert_old_argmap1(Olds, Sig1, News, ArgValues).

jasperi_convert_old_arg(jboolean(Value),boolean, [0'Z|Sig],Sig, Value).
jasperi_convert_old_arg(jbyte(Value),byte, [0'B|Sig],Sig, Value).
jasperi_convert_old_arg(jchar(Value),char, [0'C|Sig],Sig, Value).
jasperi_convert_old_arg(jdouble(Value),double, [0'D|Sig],Sig, Value).
jasperi_convert_old_arg(jfloat(Value),float, [0'F|Sig],Sig, Value).
jasperi_convert_old_arg(jint(Value),integer, [0'I|Sig],Sig, Value).
jasperi_convert_old_arg(jlong(Value),long, [0'J|Sig],Sig, Value).
jasperi_convert_old_arg(jshort(Value),short, [0'S|Sig],Sig, Value).
jasperi_convert_old_arg(jobject(Value),object(Class), [0'L|Sig0],Sig, Value) :-
   once(append(ClassCodes, [0';|Sig1], Sig0)),
   atom_codes(Class, ClassCodes),
   Sig = Sig1.
jasperi_convert_old_arg(jstring(Value),string, Sig0,Sig, Value) :-
   once(append("Ljava/lang/String;", Sig1, Sig0)),
   Sig = Sig1.

%% 3.8.5 extract return type from type sig for old style API
jasper_convert_type_sig_retvalue(TypeSig, RetType) :-
   atom_codes(TypeSig, SigChars),
   once(suffix(SigChars, [0')|RetSig])),
   ( RetSig = [Primitive] ->
       jasper_convert_primitive_type(Primitive, RetType)
   ; RetSig = [0'L|ClassCharsSemi] ->
       %% here we go
       reverse(ClassCharsSemi, [0';|ClassCharsRev]),
       reverse(ClassCharsRev, ClassChars),
       atom_codes(Class, ClassChars),
       ( Class = 'java/lang/String' -> % This is what the original predicates did.
           RetType = string
       ; otherwise ->
           RetType = object(Class)
       )
   ).

jasper_convert_primitive_type(0'B, byte).
jasper_convert_primitive_type(0'C, char).
jasper_convert_primitive_type(0'S, short).
jasper_convert_primitive_type(0'I, integer).
jasper_convert_primitive_type(0'J, long).
jasper_convert_primitive_type(0'F, float).
jasper_convert_primitive_type(0'D, double).
jasper_convert_primitive_type(0'Z, boolean).
jasper_convert_primitive_type(0'V, void). % [PD 3.8.6]

% ------------------------------------------------------------------------------
% Foreign resource definitions
% ------------------------------------------------------------------------------

%% foreign(jasperi_new_object_C, c, jasperi_new_object(+term,+string,+string,+term,+term,+integer,-term)).

%% [PM] 3.9 Used by Java to pass the SICStus object to the jasper
%% resource (via jasper_load/1 in Bips/intrins1.pl)
%% [PD] 3.9 removed
%foreign(jasperi_set_sicstus_object_C, c, jasperi_set_sicstus_object(+integer)).

foreign(jasperi_initialize_pl_C, c, jasperi_initialize(+integer,+term,+term,[-term])).
foreign(jasperi_deinitialize_C, c, jasperi_deinitialize(+term,[-integer])).
%% foreign(jasperi_call_static_C, c, jasperi_call_static(+term,+string,+string,+string,+term,+term,+integer,-term)).
foreign(jasperi_call_static1_C, c, jasperi_call_static1(+term,+string,+string,+string,+term,+term)).
%% foreign(jasperi_call_instance_C, c, jasperi_call_instance(+term,+term,+string,+string,+term,+term,+integer,-term)).
foreign(jasperi_call_instance1_C, c, jasperi_call_instance1(+term,+string,+string,+term,+term)).
foreign(jasperi_create_global_ref_C, c, jasperi_create_global_ref(+term,+term,[-term])).
foreign(jasperi_create_local_ref_C, c, jasperi_create_local_ref(+term,+term,[-term])).
foreign(jasperi_delete_global_ref_C, c, jasperi_delete_global_ref(+term,+term)).
foreign(jasperi_delete_local_ref_C, c, jasperi_delete_local_ref(+term,+term)).
foreign(jasperi_is_same_object_C, c, jasperi_is_same_object(+term,+term,+term,[-integer])).
foreign(jasperi_is_instance_of_C, c, jasperi_is_instance_of(+term,+term,+string,[-integer])).
%% [PM] 3.8.5 Return existing JVM if any
foreign(jasperi_jvm_C, c, jasperi_jvm(-term)).
%% [PM] 3.8.5 Return class name of an object, as an atom.
foreign(jasperi_object_class_name_C, c, jasperi_object_class_name(+term, +term, -term)).
%% [PD] 3.9 removed
%%% [PD] 3.9 Set thread server mode. For use by spnative.c only.
%foreign(jasperi_set_threadservermode_C, c, jasperi_set_threadservermode(+integer)).

%% [PM] 3.9 Obsolete: %% [PM] April 2000 ensure it's identical to the libjasper declaration below.
foreign_resource(jasper,
                 [
		  init(init_jasper),
		  deinit(deinit_jasper),
%% [PD] 3.9
%                  jasperi_set_sicstus_object_C,
		  jasperi_initialize_pl_C,
		  jasperi_deinitialize_C,
		  %% jasperi_new_object_C,
		  %% jasperi_call_static_C,
		  %% jasperi_call_instance_C,
                  jasperi_call_static1_C,
		  jasperi_call_instance1_C,
		  jasperi_create_global_ref_C,
		  jasperi_create_local_ref_C,
		  jasperi_delete_global_ref_C,
		  jasperi_delete_local_ref_C,
		  jasperi_is_same_object_C,
		  jasperi_is_instance_of_C,
                  jasperi_jvm_C,
                  jasperi_object_class_name_C
%% [PD] 3.9 removed
%		  jasperi_set_threadservermode_C
		 ]).

/* [PM] April 2000
This used to happen in 3.8.2 and earlier:

1. loading library(jasper) [this file] into sicstus loaded
   library/<host-type>/jasper.(dll/so) into sicstus.

2. loading java would load bin\jasper.dll (windows) or lib/libjasper.so
   (Solaris/Linux).

(3.) There was a third copy at a place [PM] has now forgotten.

The problem is that all the shared libraries are really identical and
only one should be loaded. A further compication is that Java will
look for a file named libjasper.so on Unix but jasper.dll on Win32. On
the other hand library(jasper) will load foreign resources from a file
name jasper.(dll/so) on all platforms.

What we should do, but has not, is to add a symbolic file_search_path that
includes the <prefix>/lib or <prefix>/bin directories so that
library(jasper) can load foreign resources from the right place. However,
the fact that Java on Unix will add a 'lib' prefix and look for 'libjasper'
instead of plain 'jasper' makes extending file_search_path
insufficient. Adding a symbolic file-search path has the added advantage
that SICStus can access its own "bin" directory in a "correct" way.

Furthermore, the internals of foreign resource handling expects the
resource name to be the same as the basename of the absolute path to the
shared library, which will not be true of libjasper.so. This could still be
fixed with a symbolic link from <prefix>/lib/libjasper.so to
<prefix>/lib/jasper.so but by then it gets so complicated that we decided
to postpone the right fix until and if a new directory structure for the
sicstus installation is decided on.

Instead we kludge horribly and add two foreign_resource facts above, one
for jasper (used on Win32) and one for libjasper (used on Unix).  The
unused one will do no harm. We then load differently named files from
different places depending on the platform we are running on.

[Jojo, 2000-04-14] In order for library('../../libjasper') to be found, it
has to be moved in place *before* jasper.po is built. It is therefore
necessary to do it in library/Makefile and not in Utils/Makefile (where it
was previously done).

[Jojo, 2000-04-17] library/Makefile had to be kludged in order to only
create one resource called "libjasper" under UNIX.

*/

%% [PM] 3.9 jasper is now an ordinary foreign resource (it is not linked to by Java)
:- load_foreign_resource(library(system(jasper))).
