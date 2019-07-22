/* Copyright(C) 1999, Swedish Institute of Computer Science */

:- module(comclient1,
          [
           comclient_is_object/1,
           comclient_valid_object/1,

           comclient_clsid_from_progid/2,
           comclient_clsid_from_progid/3,

           comclient_progid_from_clsid/2,
           comclient_progid_from_clsid/3,

           comclient_iid_from_name/2,
           comclient_iid_from_name/3,
           comclient_name_from_iid/2,
           comclient_name_from_iid/3,
          
           comclient_create_instance/2,
           %% not much point at present.
           % comclient_create_instance/3

           comclient_get_active_object/2,
           %% not much point at present.
           %% comclient_get_active_object/3,

           comclient_invoke_method_fun/3,
           comclient_invoke_method_proc/2,
           comclient_invoke_put/3,
           
           comclient_release/1,
           comclient_is_exception/1,
           comclient_exception_code/2,
           comclient_exception_culprit/2,
           comclient_exception_description/2,

           comclient_garbage_collect/0,
           comclient_equal/2
           ]).

:- use_module(library(lists), [
	is_list/1
	]).

%% [PM] 4.1.3 SPIDER/xref
:- public '$comclient_breakpoints_flag'/2.

%@  This library provides rudimentary access to COM automation objects. As 
%@  an example it is possible to manipulate Microsoft Office applications and
%@  Internet Explorer. It is not possible, at present, to build COM objects 
%@  using this library.
%@  
%@  Feedback is very welcome. Please contact SICStus support
%@  (@email{sicstus-support@@sics.se}) if you have suggestions for how this
%@  library could be improved.
%@  
%@  @menu
%@  * Preliminaries::               Preliminaries
%@  * Terminology::                 Terminology
%@  * COM Client Predicates::       Predicate Reference
%@  * COM Client Examples::         Examples
%@  @end menu
%@  
%@  @node Preliminaries
%@  @subsection Preliminaries
%@  
%@  In most contexts both atoms and code-lists are treated as
%@  strings. With the wide character support available in release 3.8 and
%@  later, is should now be possible to pass UNICODE atoms and strings
%@  successfully to the COM interface.
%@  
%@  @node Terminology
%@  @subsection Terminology
%@  
%@  @table @dfn
%@  @item ProgID
%@  @cindex ProgID
%@  A human readable name for an object class, typically as an atom,
%@  e.g.@: @code{'Excel.Application'}.
%@  
%@  @item CLSID @r{(Class Identifier)}
%@  @cindex CLSID
%@  A globally unique identifier of a class, typically as an atom,
%@  e.g.@: @code{'@{00024500-0000-0000-C000-000000000046@}'}.
%@  
%@  Where it makes sense a @var{ProgID} can be used instead of the
%@  corresponding @var{CLSID}.
%@  
%@  @item IID @r{(Interface Identifier)}
%@  @cindex IID
%@  A globally unique identifier of an
%@  interface. Currently only the @code{'IDispatch'} interface is used so you
%@  do not have to care about this.
%@  
%@  @item IName @r{(Interface Name)}
%@  @cindex IName
%@  The human readable name of an interface, e.g.@:
%@  @code{'IDispatch'}.
%@  
%@  Where it makes sense an @var{IName} can be used instead of the
%@  corresponding @var{IID}.
%@  
%@  @item Object
%@  @cindex Object
%@  A COM-object (or rather a pointer to an interface).
%@  
%@  @item ComValue
%@  @cindex ComValue
%@  A value that can be passed from COM to SICStus Prolog. Currently numeric types,
%@  booleans (treated as 1 for @code{true}, 0 for @code{false}), strings,
%@  and COM objects.
%@  
%@  @item ComInArg
%@  @cindex ComInArg
%@  A value that can be passed as an input argument to COM, currently
%@  one of:
%@  @table @asis
%@  @item atom
%@    Passed as a string (BSTR)
%@  @item numeric
%@    Passed as the corresponding number
%@  @item list
%@    A code-list is treated as a string.
%@  @item COM object
%@  A compound term referring to a COM object.
%@  @item  compound
%@   Other compound terms are presently illegal but  will be used to extend
%@  the permitted types.
%@  @end table
%@  
%@  @c [PM] 3.10.1b1 restriction lifted (or rather, comclient_is_object fixed(!)).
%@  @c It is, at present, not possible to pass an object as an
%@  @c argument. This restriction will be lifted.
%@  
%@  @item SimpleCallSpec
%@  @cindex SimpleCallSpec
%@  Denotes a single method and its arguments. As an example, to call
%@  the method named @code{foo} with the arguments @code{42} and the
%@  string @code{"bar"} the @var{SimpleCallSpec} would be the compound
%@  term @code{foo(42,'bar')} or, as an alternative, @code{foo(42,"bar")}.
%@                  
%@  The arguments of the compound term are treated as follows:
%@  @table @dfn
%@  @item ComInArg
%@  @cindex ComInArg
%@  See above
%@  @item variable
%@  @cindex variable
%@  The argument is assumed to be output. The variable is bound to
%@  the resulting value when the method returns.
%@  
%@  @item mutable
%@  @cindex mutable
%@  The argument is assumed to be input/output. The value of the
%@  mutable is passed to the method and when the method returns the
%@  mutable is updated with the corresponding return value.
%@  @end table
%@  
%@  
%@  @item CallSpec
%@  @cindex CallSpec
%@  Either a SimpleCallSpec or a list of CallSpecs. If it is a
%@  list then all but the last SimpleCallSpec are assumed to denote
%@  method calls that return a COM-object. So for instance the VB statement
%@  @code{app.workbooks.add} can be expressed either as:
%@  @example
%@  @group
%@  comclient_invoke_method_proc(App, [workbooks, add])
%@  @end group
%@  @end example
%@  @noindent or as
%@  @example
%@  @group
%@  comclient_invoke_method_fun(App, workbooks, WorkBooks),
%@  comclient_invoke_method_proc(WorkBooks, add),
%@  comclient_release(WorkBooks)
%@  @end group
%@  @end example
%@  
%@  @end table
%@  
%@  @node COM Client Predicates
%@  @subsection Predicate Reference
%@  
%@  @table @code

/***
**** FOREIGN FUNS
***/

%% CLSID and IID are GUIDs in textual form (e.g., from comclient_CLSIDStringFromProgID
foreign(comclient_create_instance, '$comclient_create_instance'(+term, +term, -term, -integer)).

%% wrapper for to GetActiveObject
%% CLSID and IID as for comclient_create_instance
foreign(comclient_get_active_object, '$comclient_get_active_object'(+term, +term, -term, -integer)).


%% comclient_CLSIDFromProgID(+ProgID:term, -CLSID:term, +atom?:integer, -result:integer)
foreign(comclient_CLSIDFromProgID, '$comclient_CLSIDFromProgID'(+term, -term, +integer, -integer)).
foreign(comclient_ProgIDFromCLSID, '$comclient_ProgIDFromCLSID'(+term, -term, +integer, -integer)).

%% comclient_INameToIID(+Name:term, -IID:term, +atom?:integer, -result:integer)
foreign(comclient_INameToIID, '$comclient_INameToIID'(+term, -term, +integer, -integer)).
%% comclient_IIDToName(+IID:term, -Name:term, +atom?:integer, -result:integer)
foreign(comclient_IIDToName, '$comclient_IIDToName'(+term, -term, +integer, -integer)).

%% '$finalize_external_object'(+Obj:term, -Existed:integer).
foreign('finalize_external_object', '$finalize_external_object'(+term, -integer)).

%% comclient_invoke(+Obj:term, +Goal:term, +Flags:term, -Result:term)
%% #define DISPATCH_METHOD         0x1
%% #define DISPATCH_PROPERTYGET    0x2
%% #define DISPATCH_PROPERTYPUT    0x4
%% #define DISPATCH_PROPERTYPUTREF 0x8

%% [PM] 4.1.3 not used (for a long time)
%% foreign(comclient_invoke, '$comclient_invoke'(+term, +term, +term, -term, -integer)).

foreign(comclient_invoke_ex, '$comclient_invoke_ex'(+term, +term, +term, -term, -term, -integer)).
foreign(comclient_equal, '$comclient_equal'(+term, +term, -integer)).
foreign(comclient_valid, '$comclient_valid'(+term, -integer)).

foreign(comclient_garbage_collect, '$comclient_garbage_collect'([-integer])).

%% For manual debugging
foreign(comclient_breakpoints_flag, '$comclient_breakpoints_flag'(+integer, [-integer])).

foreign_resource(comclient, [
                             comclient_CLSIDFromProgID,
                             comclient_IIDToName,
                             comclient_INameToIID,
                             comclient_ProgIDFromCLSID,
                             comclient_create_instance,
                             comclient_get_active_object,
                             %% comclient_invoke,
                             comclient_invoke_ex,
                             comclient_equal,
                             comclient_valid,
                             comclient_garbage_collect,
                             comclient_breakpoints_flag,
                             
                             init(comclient_init),
                             deinit(comclient_deinit),
                             'finalize_external_object'

                             ]).

:- load_foreign_resource(library(system(comclient))).


%@  @item comclient_garbage_collect
%@  @PLXindex {comclient_garbage_collect/0 (comclient)}
%@  Release Objects that are no longer reachable from SICStus
%@  Prolog. To achieve this the predicate
%@  @code{comclient_garbage_collect/0} performs an atom garbage
%@  collection, i.e.@: @code{garbage_collect_atoms/0}, so it should be used
%@  sparingly.

comclient_garbage_collect :-
   comclient_garbage_collect(_).

comclient_garbage_collect(Collected) :-
   garbage_collect_atoms,
   '$comclient_garbage_collect'(Collected0),
   Collected = Collected0.


%@  @item comclient_is_object(@var{+Object})
%@  @PLXindex {comclient_is_object/1 (comclient)}
%@  Succeeds if @var{Object} "looks like" an object. It does not check that
%@  the object is (still) reachable from SICStus Prolog, see
%@  @code{comclient_valid_object/1}. Currently an object looks like
%@  @code{'$comclient_object'(@var{stuff})} where @var{stuff} is some prolog
%@  term. Do not rely on this representation!

comclient_is_object(Obj) :- var(Obj), !,
   fail.
comclient_is_object('$comclient_object'(Ref)) :- 
   %% [PM] 3.10.1b1 no-one reported that comclient_is_object always failed!
   %% integer(Ref)
   atom(Ref).

%@  @item  comclient_valid_object(@var{+Object})
%@  @PLXindex {comclient_valid_object/1 (comclient)}
%@  Succeeds if @var{Object} is an object that is still available to
%@  SICStus Prolog.

comclient_valid_object(Obj) :-
   %% is_object guards against other types of external objects
   %% (Currently a non-issue though).
   comclient_is_object(Obj),
   '$comclient_valid'(Obj, Result),
   Result == 1.


%@  @item comclient_equal(@var{+Object1}, @var{+Object2})
%@  @PLXindex {comclient_equal/2 (comclient)}
%@  Succeeds if @var{Object1} and @var{Object2} are the same object. (It
%@  succeeds if their @code{'IUnknown'} interfaces are identical)

comclient_equal(Obj1, Obj2) :-
   %% is_object guards against other types of external objects
   %% (Currently a non-issue though).
   comclient_is_object(Obj1),
   comclient_is_object(Obj2),
   '$comclient_equal'(Obj1, Obj2, Result),
   Result == 1.


%@  @item comclient_clsid_from_progid(@var{+ProgID}, @var{-CLSID}).
%@  @PLXindex {comclient_clsid_from_progid/2 (comclient)}
%@  Obtain the @var{CLSID} corresponding to a particular @var{ProgID}. Uses
%@  the Win32 routine @code{CLSIDFromProgID}. You rarely need this since
%@  you can use the ProgID directly in most cases.

comclient_clsid_from_progid(ProgID, CLSID) :-
   WantAtom = true,
   comclient_clsid_from_progid(ProgID, CLSID, WantAtom).

comclient_clsid_from_progid(ProgID, CLSID, WantAtom) :-
   Culprit = comclient_clsid_from_progid(ProgID, CLSID, WantAtom),
   comclient_boolean_to_int(WantAtom, WantAtomFlag, Culprit, 3),
   %% All type errors in arguments should cause fail or exceptions before this point

   '$comclient_CLSIDFromProgID'(ProgID, CLSID1, WantAtomFlag, Result),
   comclient_process_result(Result, Culprit),
   CLSID = CLSID1.

%@  @item comclient_progid_from_clsid(@var{+CLSID}, @var{-ProgID}).
%@  @PLXindex {comclient_progid_from_clsid/2 (comclient)}
%@  Obtain the @var{ProgID} corresponding to a particular @var{CLSID}. Uses the
%@  Win32 routine @code{ProgIDFromCLSID}. Rarely needed. The @var{ProgID}
%@  returned will typically have the version suffix appended.
%@  
%@  Example, to determine what version of @code{Excel.Application} is installed:
%@  @example
%@  @group
%@  | ?- @kbd{comclient_clsid_from_progid('Excel.Application, CLSID),}
%@       @kbd{comclient_progid_from_clsid(CLSID, ProgID).}
%@  CLSID = '@{00024500-0000-0000-C000-000000000046@}',
%@  ProgID = 'Excel.Application.8'
%@  @end group
%@  @end example

comclient_progid_from_clsid(CLSID, ProgID) :-
   WantAtom = true,
   comclient_progid_from_clsid(CLSID, ProgID, WantAtom).

comclient_progid_from_clsid(CLSID, ProgID, WantAtom) :-
   Culprit = comclient_progid_from_clsid(CLSID, ProgID, WantAtom),
   comclient_boolean_to_int(WantAtom, WantAtomFlag, Culprit, 3),
   %% All type errors in arguments should cause fail or exceptions before this point

   '$comclient_ProgIDFromCLSID'(CLSID, ProgID1, WantAtomFlag, Result),
   comclient_process_result(Result, Culprit),
   ProgID = ProgID1.



%@  @item comclient_iid_from_name(@var{+IName}, @var{-IID})
%@  @PLXindex {comclient_iid_from_name/2 (comclient)}
%@  Look in the registry for the @var{IID} corresponding to a particular
%@  Interface. Currently of little use.
%@  @example
%@  @group
%@  | ?- @kbd{comclient_iid_from_name('IDispatch', IID).}
%@  IID = '@{00020400-0000-0000-C000-000000000046@}'
%@  @end group
%@  @end example

comclient_iid_from_name(InterfaceName, IID) :-
   WantAtom = true,
   comclient_iid_from_name(InterfaceName, IID, WantAtom).

comclient_iid_from_name(InterfaceName, IID, WantAtom) :-
   Culprit = comclient_iid_from_name(InterfaceName, IID, WantAtom),
   comclient_boolean_to_int(WantAtom, WantAtomFlag, Culprit, 3),
   %% All type errors in arguments should cause fail or exception before this point

   '$comclient_INameToIID'(InterfaceName, IID1, WantAtomFlag, Result),
   comclient_process_result(Result, Culprit),
   IID = IID1.

%@  @item comclient_name_from_iid(@var{+IID}, @var{-IName})
%@  @PLXindex {comclient_name_from_iid/2 (comclient)}
%@  Look in the registry for the name corresponding to a particular
%@  @var{IID}. Currently of little use.

comclient_name_from_iid(IID, InterfaceName) :-
   WantAtom = true,
   comclient_name_from_iid(IID, InterfaceName, WantAtom).

comclient_name_from_iid(IID, InterfaceName, WantAtom) :-
   Culprit = comclient_name_from_iid(IID, InterfaceName, WantAtom),
   comclient_boolean_to_int(WantAtom, WantAtomFlag, Culprit, 3),
   %% All type errors in arguments should cause fail or exception before this point

   '$comclient_IIDToName'(IID, InterfaceName1, WantAtomFlag, Result),
   comclient_process_result(Result, Culprit),
   InterfaceName = InterfaceName1.



%@  @item  comclient_create_instance(@var{+ID}, @var{-Object})
%@  @PLXindex {comclient_create_instance/2 (comclient)}
%@  Create an instance of the Class identified by the CLSID or ProgID @var{ID}.
%@  @example
%@  @group
%@  comclient_create_instance('Excel.Application', App)
%@  @end group
%@  @end example
%@  Corresponds to @code{CoCreateInstance}.

comclient_create_instance(CLSID, Obj) :-
   %% comclient_iid_from_name('IDispatch', IID),
   IID = '{00020400-0000-0000-C000-000000000046}', % i.e., IDispatch
   comclient_create_instance(CLSID, IID, Obj).

comclient_create_instance(CLSID, IID, Obj) :-
   Culprit = comclient_create_instance(CLSID, IID, Obj),
   %% More type tests later
   %% All type errors in arguments should cause fail or exception before this point

   '$comclient_create_instance'(CLSID, IID, Obj1, Result),
   comclient_process_result(Result, Culprit),
   Obj = Obj1.

%@  @item comclient_get_active_object(@var{+ID}, @var{-Object})
%@  @PLXindex {comclient_get_active_object/2 (comclient)}
%@  Retrieves a running object of the Class identified by the CLSID
%@  or ProgID @var{ID}.
%@  @example
%@  @group
%@  comclient_get_active_object('Excel.Application', App)
%@  @end group
%@  @end example
%@  An exception is thrown if there is no suitable running object.
%@  Corresponds to @code{GetActiveObject}.

comclient_get_active_object(CLSID, Obj) :-
   %% comclient_iid_from_name('IDispatch', IID),
   IID = '{00020400-0000-0000-C000-000000000046}', % i.e., IDispatch
   comclient_get_active_object(CLSID, IID, Obj).

comclient_get_active_object(CLSID, IID, Obj) :-
   Culprit = comclient_get_active_object(CLSID, IID, Obj),
   %% More type tests later
   %% All type errors in arguments should cause fail or exception before this point

   '$comclient_get_active_object'(CLSID, IID, Obj1, Result),
   comclient_process_result(Result, Culprit),
   Obj = Obj1.



%@  @item  comclient_invoke_method_fun(@var{+Object}, @var{+CallSpec}, @var{-ComValue})
%@  @PLXindex {comclient_invoke_method_fun/3 (comclient)}
%@  Call a method that returns a value. Also use this to get the
%@  value of properties.

%@  
comclient_invoke_method_fun(Obj, Goals, Value) :- Goals = [G|Gs],
   is_list(Goals), !,
   ( Gs = [] ->
       comclient_invoke_method_fun(Obj, G, Value)
   ; otherwise ->
       comclient_invoke_method_fun(Obj, G, Obj1),
       comclient_invoke_method_fun(Obj1, Gs, Value0),
       comclient_release(Obj1),
       Value = Value0
   ).
comclient_invoke_method_fun(Obj, Goal, Value) :-
   Culprit = comclient_invoke_method_fun(Obj, Goal, Value),
   comclient_check_type(com_object, Obj, Culprit, 1),
   comclient_check_type(nonvar, Goal, Culprit, 2),
   %% comclient_flags(['SPCOM_RETVAL', 'PROPERTY_METHOD', 'PROPERTY_GET'], Flags),
   Flags = 67,                  % 'PROPERTY_METHOD' \/ PROPERTY_GET \/ 'SPCOM_RETVAL'
   comclient_preprocess_method_goal(Goal, 0, GoalIn, Culprit, 2),
   %% '$comclient_invoke'(Obj, GoalIn, Flags, GoalOut, Result),
   %% comclient_process_result(Result, Culprit),
   '$comclient_invoke_ex'(Obj, GoalIn, Flags, GoalOut, Result, Code),
   comclient_process_result(Code, Result, Culprit),
   comclient_postprocess_method_goal(Goal, GoalOut, Culprit, 2),
   functor(Goal, _MethodName, Arity),
   RetvalIdx is Arity+1,
   arg(RetvalIdx, GoalOut, Value).

%@  @item  comclient_invoke_method_proc(@var{+Object}, @var{+CallSpec})
%@  @PLXindex {comclient_invoke_method_proc/2 (comclient)}
%@  Call a method that does not return a value.

comclient_invoke_method_proc(Obj, Goals) :- Goals = [G|Gs],
   is_list(Goals), !,
   ( Gs = [] ->
       comclient_invoke_method_proc(Obj, G)
   ; otherwise ->
       comclient_invoke_method_fun(Obj, G, Obj1),
       comclient_invoke_method_proc(Obj1, Gs),
       comclient_release(Obj1)
    ).
comclient_invoke_method_proc(Obj, Goal) :-
   Culprit = comclient_invoke_method_proc(Obj, Goal),
   comclient_check_type(com_object, Obj, Culprit, 1),
   comclient_check_type(nonvar, Goal, Culprit, 2),
   %% comclient_flags(['PROPERTY_METHOD'], Flags),
   Flags = 1,                   % 'PROPERTY_METHOD'
   comclient_preprocess_method_goal(Goal, 0, GoalIn, Culprit, 2),
   %% '$comclient_invoke'(Obj, GoalIn, Flags, GoalOut, Result),
   %% comclient_process_result(Result, Culprit),
   '$comclient_invoke_ex'(Obj, GoalIn, Flags, GoalOut, Result, Code),
   comclient_process_result(Code, Result, Culprit),
   comclient_postprocess_method_goal(Goal, GoalOut, Culprit, 2).

%@  @item  comclient_invoke_put(@var{+Object}, @var{+CallSpec}, @var{+ComInArg})
%@  @PLXindex {comclient_invoke_put/3 (comclient)}
%@  Set the property denoted by @var{CallSpec} to @var{ComValue}.
%@  Example: @code{comclient_invoke_put(App, visible, 1)}

comclient_invoke_put(Obj, Goals, Value) :- Goals = [G|Gs],
   is_list(Goals), !,
   ( Gs = [] ->
       comclient_invoke_put(Obj, G, Value)
   ; otherwise ->
       comclient_invoke_method_fun(Obj, G, Obj1),
       comclient_invoke_put(Obj1, Gs, Value),
       comclient_release(Obj1)
   ).
comclient_invoke_put(Obj, Goal, Value) :-
   Culprit = comclient_invoke_put(Obj, Goal, Value),
   comclient_check_type(com_object, Obj, Culprit, 1),
   comclient_check_type(nonvar, Goal, Culprit, 2),
   comclient_check_type(comvalue, Value, Culprit, 3),
   %% comclient_flags(['PROPERTY_PUT'], Flags),
   Flags = 4,                  % 'PROPERTY_PUT'
   comclient_preprocess_method_goal(Goal, 1, GoalIn, Culprit, 2),
   functor(GoalIn, _, ArityIn),
   arg(ArityIn, GoalIn, Value),
   %% Goal =.. [Method|Args],
   %% append(Args, [Value], Args1),
   %% Goal1 =.. [Method|Args1],
   %% '$comclient_invoke'(Obj, GoalIn, Flags, GoalOut, Result),
   %% comclient_process_result(Result, Culprit),
   '$comclient_invoke_ex'(Obj, GoalIn, Flags, GoalOut, Result, Code),
   comclient_process_result(Code, Result, Culprit),
   comclient_postprocess_method_goal(Goal, GoalOut, Culprit, 2).



comclient_preprocess_method_goal(Goal, ExtraArgs, GoalIn, Culprit,ArgNo) :-
   functor(Goal, MethodName, Arity),
   ArityIn is Arity+ExtraArgs,
   functor(GoalIn, MethodName, ArityIn),

   Limit is Arity+1,
   comclient_preprocess_method_goal1(1,Limit, Goal, GoalIn, Culprit, ArgNo).

comclient_preprocess_method_goal1(I,Limit, _Goal, _GoalIn, _Culprit, _ArgNo) :- I==Limit, !,
   true.
comclient_preprocess_method_goal1(I,Limit, Goal, GoalIn, Culprit, ArgNo) :-
   arg(I, Goal, Arg),
   comclient_preprocess_method_arg(Arg, ArgIn, Goal, I, Culprit, ArgNo),
   arg(I, GoalIn, ArgIn),
   I1 is I+1,
   comclient_preprocess_method_goal1(I1,Limit, Goal, GoalIn, Culprit, ArgNo).


comclient_preprocess_method_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- var(Arg), !,
   ArgIn = _.
comclient_preprocess_method_arg(Arg, ArgIn, Goal, I, Culprit, ArgNo) :- mutable(Arg), !,
   get_mutable(Arg1, Arg),
   comclient_preprocess_method_inout_arg(Arg1, ArgIn, Goal, I, Culprit, ArgNo).
comclient_preprocess_method_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- atomic(Arg), !,
   ArgIn = Arg.
comclient_preprocess_method_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- comclient_is_object(Arg), !,
   ArgIn = Arg.
comclient_preprocess_method_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- Arg = [_|_],
   is_list(Arg),
   !,
   %% Should verify that it is a list of (UNICODE) char codes
   ArgIn = Arg.
comclient_preprocess_method_arg(_Arg, _ArgIn, Goal, I, Culprit, ArgNo) :-
   comclient_argument_exception(invoke_method(Goal, I), Culprit, ArgNo).


comclient_preprocess_method_inout_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- atomic(Arg), !,
   ArgIn = Arg.
comclient_preprocess_method_inout_arg(Arg, ArgIn, _Goal, _I, _Culprit, _ArgNo) :- Arg = [_|_],
   %% Should also verify that it is a list of (UNICODE) char codes
   is_list(Arg), !,
   ArgIn = Arg.
comclient_preprocess_method_inout_arg(_Arg, _ArgIn, Goal, I, Culprit, ArgNo) :-
   comclient_argument_exception(invoke_method(Goal, I), Culprit, ArgNo).


comclient_postprocess_method_goal(Goal, GoalOut, Culprit,ArgNo) :-
   functor(Goal, _MethodName, Arity),

   Limit is Arity+1,
   comclient_postprocess_method_goal1(1,Limit, Goal, GoalOut, Culprit, ArgNo).


comclient_postprocess_method_goal1(I,Limit, _Goal, _GoalOut, _Culprit, _ArgNo) :- I==Limit, !,
   true.
comclient_postprocess_method_goal1(I,Limit, Goal, GoalOut, Culprit, ArgNo) :-
   arg(I, Goal, Arg),
   arg(I, GoalOut, ArgOut),     % [PM] 3.9.1 bind first to ensure update_mutable is called with non-var arg (SPRM 3427)
   comclient_postprocess_method_arg(Arg, ArgOut, Goal, I, Culprit, ArgNo),
   I1 is I+1,
   comclient_postprocess_method_goal1(I1,Limit, Goal, GoalOut, Culprit, ArgNo).


comclient_postprocess_method_arg(Arg, ArgOut, _Goal, _I, _Culprit, _ArgNo) :- var(Arg), !,
   Arg = ArgOut.
comclient_postprocess_method_arg(Arg, ArgOut, _Goal, _I, _Culprit, _ArgNo) :- mutable(Arg), !,
   update_mutable(ArgOut, Arg).
comclient_postprocess_method_arg(_Arg, _ArgOut, _Goal, _I, _Culprit, _ArgNo).


/*
comclient_transfer_result(Goal, Result) :-
   functor(Goal, _, Arity),
   Limit is Arity+1,
   comclient_transfer_result1(1,Limit, Goal, Result).

comclient_transfer_result1(I,Limit, _, _) :- I==Limit, !,
   true.
comclient_transfer_result1(I,Limit, Goal, Result) :-
   arg(I, Goal, Arg),
   %% Arguments that are variables are assumed to be output args.
   %% They are bound to the corresponding value in the Result.
   ( var(Arg) ->
       arg(I, Result, Arg)
   ; true
   ),
   I1 is I+1,
   comclient_transfer_result1(I1,Limit, Goal, Result).

*/

%% [PM] 4.1.3 SPIDER/xref
% All calls to comclient_flags/2 has been manually inlined
:- public comclient_flags/2.

comclient_flags(Flags, Bits) :-
   comclient_flags1(Flags, 0,Bits).

comclient_flags1([], Bits0,Bits) :-
   Bits = Bits0.
comclient_flags1([Flag|Flags], Bits0,Bits) :-
   comclient_flag(Flag, FlagBits),
   Bits1 is Bits0 \/ FlagBits,
   comclient_flags1(Flags, Bits1,Bits).


comclient_flag(Name, Bits) :- var(Name), !,
   comclient_argument_exception(nonvar, comclient_flag(Name, Bits), 1).
comclient_flag('SPCOM_RETVAL', Bits) :- !, %xref comclient.c
   Bits = 64.
comclient_flag('PROPERTY_METHOD', Bits) :- !,
   Bits = 1.
comclient_flag('PROPERTY_GET_', Bits) :- !,
   Bits = 2.
comclient_flag('PROPERTY_GET', Bits) :- !,
   Bits = 66.                   % 2 \/ SPCOM_RETVAL
comclient_flag('PROPERTY_PUT', Bits) :- !,
   Bits = 4.
comclient_flag(Name, Bits) :-
   Flags = ['SPCOM_RETVAL',
            'PROPERTY_METHOD', 'PROPERTY_GET', 'PROPERTY_GET_', 'PROPERTY_PUT'
	    ],
   comclient_argument_exception(oneof(Flags), comclient_flag(Name, Bits), 1).

%@  @item comclient_release(@var{+Object})
%@  @PLXindex {comclient_release/1 (comclient)}
%@  Release the object and free the datastructures used by SICStus
%@  Prolog to keep track of this object. After releasing an object the
%@  term denoting the object can no longer be used to access the
%@  object (any attempt to do so will raise an exception).
%@  @quotation
%@  @strong{Please note}: The same COM-object can be represented by different prolog 
%@  terms. A COM object is not released from SICStus Prolog until all
%@  such representations have been released, either
%@  explicitly by calling @code{comclient_release/1} or by
%@  calling @code{comclient_garbage_collect/0}.
%@  
%@  You cannot use @code{@var{Obj1} == @var{Obj2}} to determine whether two
%@  COM-objects are identical. Instead use @code{comclient_equal/2}.
%@  @end quotation

comclient_release(Obj) :-
   Culprit = comclient_release(Obj),
   comclient_check_type(com_object, Obj, Culprit, 1),
   '$finalize_external_object'(Obj, Existed),
   ( Existed == 1 ->
       true
   ; otherwise ->
       %% Did not exist (anymore, possibly)
       comclient_raise_exception(arg, Culprit)
   ).



%% xref WinError.h
comclient_process_result(HRESULT, Culprit) :-
   comclient_process_result(HRESULT, _, Culprit).

comclient_process_result(HRESULT, Excp, Culprit) :- var(HRESULT), !,
   comclient_raise_exception(HRESULT, Excp, Culprit).
comclient_process_result(0, _Excp, _Culprit) :- !, % S_OK
   true.
comclient_process_result(1, _Excp, _Culprit) :- !, % S_FALSE (we (will) use it to signal fail)
   !, fail.
comclient_process_result(HRESULT, Excp, Culprit) :- HRESULT < 0, !, 
   comclient_signed_to_unsigned_i4(HRESULT, HRESULT1),
   comclient_raise_exception(HRESULT1, Excp, Culprit).

comclient_process_result(HRESULT, _Excp,_Culprit) :- HRESULT > 0,
   %% Other success values are unexpected but I will treat them as S_OK for now
   true.


comclient_raise_exception(HR, Excp, Culprit) :- var(Excp), !,
   comclient_raise_exception(HR, Culprit).
comclient_raise_exception(HR, Excp, Culprit) :- Excp == [], !,
   comclient_raise_exception(HR, Culprit).
comclient_raise_exception(HR, Excp, Culprit) :-
   throw(comclient_error(HR, Excp, Culprit)).


comclient_raise_exception(HR, Culprit) :-
   throw(comclient_error(HR, Culprit)).

:- if(false).
%% Generic argument error at unknown position and type
comclient_argument_exception(Culprit) :-
   comclient_raise_exception(arg, Culprit).
:- endif.

%% Signal that argument ArgNo of goal Culprit was not of the ExpectedType
comclient_argument_exception(ExpectedType, Culprit, ArgNo) :-
   comclient_raise_exception(arg(ArgNo, ExpectedType), Culprit).



comclient_check_type(nonvar, Obj, Culprit, ArgNo) :-
   var(Obj), !,
   comclient_argument_exception(nonvar, Culprit, ArgNo).
comclient_check_type(nonvar, _Obj, _Culprit, _ArgNo) :- !,
   true.
comclient_check_type(com_object, X, Culprit, ArgNo) :- !,
   % for now
   comclient_check_type(match('$comclient_object'(_)), X, Culprit, ArgNo).
comclient_check_type(match(Pattern), X, _Culprit, _ArgNo) :-
   nonvar(X),
   \+ \+ X = Pattern,
   !,
   true.
comclient_check_type(match(Pattern), _X, Culprit, ArgNo) :- !,
   comclient_argument_exception(match(Pattern), Culprit, ArgNo).
comclient_check_type(oneof(List), X, _Culprit, _ArgNo) :-
   nonvar(X),
   \+ \+ member(X, List),
   !,
   true.
comclient_check_type(oneof(List), _X, Culprit, ArgNo) :- !,
   comclient_argument_exception(oneof(List), Culprit, ArgNo).
comclient_check_type(comvalue, X, _Culprit, _ArgNo) :-
   ground(X), !,                % for now
   true.
comclient_check_type(comvalue, _X, Culprit, ArgNo) :- !,
   comclient_argument_exception(comvalue, Culprit, ArgNo).
comclient_check_type(Type, X, Culprit, ArgNo) :-
   \+ Culprit = comclient_check_type(_,_,_,_),
   Culprit1 = comclient_check_type(Type, X, Culprit, ArgNo),
   KnownTypes = [nonvar, match(_), oneof(_), com_object],
   comclient_check_type(oneof(KnownTypes), Type, Culprit1, 1).


%@  @item comclient_is_exception(@var{+ExceptionTerm})
%@  @PLXindex {comclient_is_exception/1 (comclient)}
%@  Succeeds if @var{ExceptionTerm} is an exception raised by the
%@  comclient module.
%@  @example
%@  @group
%@  catch(<some code>,
%@        Exception,
%@        ( comclient_is_exception(E) ->
%@           handle_com_related_errors(E)
%@        ; otherwise -> % Pass other exceptions upwards
%@           throw(E)
%@        ))
%@  @end group
%@  @end example

comclient_is_exception(ExceptionTerm) :- var(ExceptionTerm), !,
   fail.
comclient_is_exception(comclient_error(_HR, _Culprit)).
comclient_is_exception(comclient_error(_HR, _Exception, _Culprit)).

%@  @item comclient_exception_code(@var{+ExceptionTerm}, @var{-ErrorCode})
%@  @PLXindex {comclient_exception_code/2 (comclient)}
comclient_exception_code(Excp, _HR) :- var(Excp), !, fail.
comclient_exception_code(comclient_error(HR, _Culprit), HR).
comclient_exception_code(comclient_error(HR, _Exception, _Culprit), HR).

%@  @itemx comclient_exception_culprit(@var{+ExceptionTerm}, @var{-Culprit})
%@  @PLXindex {comclient_exception_culprit/2 (comclient)}
comclient_exception_culprit(Excp, _HR) :- var(Excp), !, fail.
comclient_exception_culprit(comclient_error(_HR, Culprit), Culprit).
comclient_exception_culprit(comclient_error(_HR, _Exception, Culprit), Culprit).

%@  @itemx comclient_exception_description(@var{+ExceptionTerm}, @var{-Description})
%@  @PLXindex {comclient_exception_description/2 (comclient)}
%@  Access the various parts of a comclient exception. The @var{ErrorCode}
%@  is the @code{HRESULT} causing the exception. @var{Culprit} is a
%@  term corresponding to the call that gave an
%@  exception. @var{Description}, if available, is either a term
%@  @code{'EXCEPINFO'(@dots{})} corresponding to an @code{EXCEPINFO}
%@  structure or @code{'ARGERR'(MethodName, ArgNumber)}.
%@  
%@  The @code{EXCEPINFO} has six arguments corresponding to, and in the
%@  same order as, the arguments of the @code{EXCEPINFO} struct.

comclient_exception_description(Excp, _HR) :- var(Excp), !, fail.
comclient_exception_description(comclient_error(_HR, Exception, _Culprit), Exception).

comclient_boolean_to_int(Boolean, BooleanFlag, _Culprit, _ArgNo) :-
   comclient_boolean_to_int(Boolean, BooleanFlag1), !,
   BooleanFlag = BooleanFlag1.
comclient_boolean_to_int(_Boolean, _BooleanFlag, Culprit, ArgNo) :-
   comclient_argument_exception(boolean, Culprit, ArgNo).


%% Chould fail on illegal arg
comclient_boolean_to_int(Boolean, _) :- var(Boolean), !,
   fail.
comclient_boolean_to_int(true, 1).
comclient_boolean_to_int(false, 0).


%% Useful for HRESULT (best presented as an unsigned, 32 bit, long)
comclient_signed_to_unsigned_i4(SL, UL) :- SL >= 0, !,
   UL = SL.
comclient_signed_to_unsigned_i4(SL, UL) :-
   UL is SL + 1<<31 + 1<<31.

%@  @end table
%@  
%@  @node COM Client Examples
%@  @subsection Examples
%@  
%@  The following example launches @i{Microsoft Excel}, adds a new worksheet,
%@  fill in some fields and finally clears the worksheet and quits @emph{Excel}
%@  @example
%@  @group
%@  :- use_module(library(comclient)).
%@  :- use_module(library(lists)).
%@  
%@  test :-
%@     test('Excel.Application').
%@  
%@  test(ProgID) :-
%@     comclient_create_instance(ProgID, App),
%@     %% Visuall Basic: app.visible = 1 
%@     comclient_invoke_put(App, visible, 1),
%@     %% VB: app.workbooks.add
%@     comclient_invoke_method_proc(App, [workbooks, add]),
%@     %% VB: with app.activesheet
%@     comclient_invoke_method_fun(App, activesheet, ActiveSheet),
%@  
%@     Rows = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15],
%@     Cols = Rows,
%@     %% VB: .cells i,j . value = i+j/100 
%@     (
%@       member(I, Rows),
%@       member(J, Cols),
%@       ValIJ is I+J/100,
%@  
%@       comclient_invoke_put(ActiveSheet, [cells(I,J),value], ValIJ),
%@       fail
%@     ; true
%@     ),
%@     (
%@       member(I, Rows),
%@       member(J, Cols),
%@       %% retrieve cell values
%@       comclient_invoke_method_fun(ActiveSheet, [cells(I,J), value],CellValue),
%@       format(user_error, '~nCell(~w,~w) = ~w', [I,J,CellValue]),
%@       fail
%@     ; true
%@     ),
%@  
%@     Range = 'A1:O15',
%@     format(user_error, '~Npress return to clear range (~w)', [Range]),
%@     flush_output(user_error),
%@     get_code(_),
%@  
%@     %% VB: .range A1:O15 .Clear 
%@     comclient_invoke_method_proc(ActiveSheet, [range(Range),clear]),
%@     
%@     %% Avoid Excel query "do you want to save@dots{}"
%@     %%  VB: app.activeworkbook.saved = 1 
%@     comclient_invoke_put(App, [activeworkbook,saved], 1),
%@  
%@     format(user_error, '~Npress return to quit \'~w\'', [ProgID]),
%@     flush_output(user_error),
%@     get_code(_),
%@     
%@     %% VB: app.quit
%@     comclient_invoke_method_proc(App, quit),
%@  
%@     comclient_release(ActiveSheet),
%@     comclient_release(App).
%@     
%@  @end group
%@  @end example

