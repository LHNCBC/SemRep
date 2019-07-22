%  SCCS   : @(#)obj_decl.pl	20.3 11/17/94
%  Package: obj_decl
%  Authors: Peter Schachte
%  Purpose: Handle Class Declarations for Object Oriented Prolog Extension
%  Origin : 04 Nov 92
%
%	+---------------------------------------------------------------+
%	| Copyright (C) 1992 Quintus Corporation.  All rights reserved.	|
%	+---------------------------------------------------------------+
%
%			       Abstract
%
%  This module handles class declarations for the objects package.
%  The goal of this package is to provide a powerful object oriented
%  extension to Prolog that is very efficient.  Most object oriented
%  Prolog extensions use the Prolog database to store information,
%  rendering these packages less useful for large applications due to
%  the performance problems of the dynamic database.  This package
%  uses C-style structures to represent objects where possible,
%  providing faster access and much faster modification of objects.
%
%  This package also improves efficiency by using Prolog's
%  'term_expansion/6' facility.  This allows us to generate efficient
%  code for sending messages and for accessing and setting slots.


:- module(obj_decl, []).

:- use_module(library(alignments), [
        primitive_type_size/3,
        pointer_alignment/2
        ]).
:- use_module(library(types)).
:- use_module(library(lists), [
	select/3
	]).

/****************************************************************
			     Introduction

The goal of this package is to provide the following features.  How
each is implemented is described below.

++description

    Multiple inheritance
		Determine at compile-time which ancestor class a method is
		inherited from, and the offset in the descendant class of
		that class's slots.  Add this offset at runtime to the
		actual object when sending the message.
    Object itself knows its type
		A word in memory just before each instance is class name
		atom.
    Automatic generation of accessor/setter methods
		Slots declared 'public' have accessor and setter methods
		automatically generated.
    All external slot references go through methods
		The special forms for accessing and setting slots of a class
		are not available outside the definition of that class.  Use
		setter and accessor messages to get at slots outside of
		methods for that class.
    Compile-time inheritance
		Inheritance is precomputed.  Sending a message to an object
		jumps to the appropriate method clause with only one
		intervening (trivial) procedure call.
    As much done at compile-time as possible
		Simple methods, including most setting and accessing of
		slots, avoid even the one intervening procedure call.
		Sending these messages costs no more than an ordinary Prolog
		predicate call.
    Term-valued slots
		Implemented with a separate dynamic predicate for each slot
		name, indexed by object address.
    Can store instance of descendant class in slot
		If a slot is declared to be of class x, and y is a
		descendant class of x, this allows you to put in instance of
		class y in the slot.  Also, you can have a slot of type
		object which can hold an object of any class.  This is
		implemented by reading the atom just before the object to
		find its type when accessing the slot.  Similarly, we check
		the type of an object being stored in the slot to verify
		that it is a descendant of the specified class.
    Instance methods
		allow different instance of the same class to have different
		methods for the same message.  Like term slots with bodies.
		This is a powerful way to implement callbacks: the object
		sends itself a message, which can be different for each
		instance.
--description

These features would have been nice to have, but aren't implemented yet:

++description
    Garbage collector
		The most important omission of this package. This would
		require the cooperation of the Prolog system.  Prolog would
		really have to do the garbage collection, as there is no way
		for the objects package to know whether an object is
		referred to from the Prolog stack (or heap).
    Method selection based on multiple args, like Common Lisp
		Can't do it.  We must have circumscribed class definitions,
		so we can generate inherited code at compile-time.  This
		would allow methods to be defined anywhere.  Life wouild be
		better if we could generate code at LINK time.
    Parameterized types
		parameters would be stored in each object before class name
		atom.
    runtime mixing of mixins
		This would save you from having to create a class that just
		combined a few parent classes.  Instead you could create an
		instance of a set of classes, where the instance combined
		the features of these classes.
--description

****************************************************************/

/****************************************************************
		       Class Definition Syntax

$$ /<%%>/{\\bf %}/

 A class is defined with the following syntax:

++description
   class_defn	 	::= <class_begin> \{<clause>|<method>\} <class_end>
 
   class_begin		::= :- class <name> = <parent_or_slots>
				\{+ <parent_or_slots>\} .
   parent_or_slots	::= <name> | [] | [ <slot_def> \{, <slot_def>\} ]
   slot_def		::= <opt_visibility> <name> : <type>
   opt_visibility	::= <empty> | private | protected | public
   type			::= <atomic_type> | <array_type> | <name>

   method		::= <opt_mvisibility> <var> <message-operator> <message> :- <body>
   opt_mvisibility	::= <empty> | protected | public
   message		::= <callable_term>

   class_end		::= :- end_class <opt_name> . | <class_begin> | <eof>
   opt_name		::= <empty> | <name>
--description

****************************************************************/


:- op(1150, fx, [class, end_class, instance_method, class_method,
		 inherit, uninherit]).
:- op( 700, xfx, [<-]).  % Note: << and >> are built-in operators
:- op( 600, fy, [private, protected]).
:- op( 600, fy, [public]). % Note: redeclaring built-in operator


/****************************************************************

		    Information Kept About Classes

This package maintains information about each class and method
compiled.  This information is intended to be used later to compile
other classes, but can also be used as part of a development system or
other application that needs information about the known classes.

****************************************************************/

/*================================================================

			Predicates Maintained

The following predicates (in module obj_decl) are generated by this
package to keep track of the classes and methods that have been
defined.

++description
    class_size(Class, Size, Prefixsize)
	instances of Class are Size bytes + a Prefixsize byte header.

    class_file(Class, Filename)
	Class is defined in file Filename.

    term_class(Class, Term, Goal, Essence)
	Class is a term class whose instances are represented by
	terms that unify with Term and satisfy Goal.  Essence is a
	list of Variable:Type terms, where each Variable appears in
	Term, and each Type is the type of that variable.  If Essence
	is [], then each variable in Term is assumed to be a general
	term.

    subclass(Parent, Child)
	Child is a subclass of Parent.

    superclass(Child, Parent, Offset)
	Parent is a superclass of Child.  Parent's slots begin Offset bytes
	from the beginning of Child.

    direct_slot(Class, Slot, Type, Offset, Attribs)
	Slot is a direct (not inherited) slot of Class.  Contents of Slot
	are a descendent of Type, and can be found at Offset words from the
	beginning of an instance of Class.  Attribs is an extensible list of
	attributes of this slot.

    direct_message(Class, Messagename, Arity, Op)
	Messagename and Arity specify a direct (not inherited) message of
	Class, and Op is '<-', '>>', or '<<' specifying what kind of
	message.

    instance_method(Messagename, Arity, Class, Predname)
	Messagename and Arity specify a message which has been declared to
	be an instance method for Class.  Instance methods are always for
	the <- operator.  Predname is the name of the dynamic predicate
	that implements this method for this class.

    class_message(Class, Messagename, Arity, Op)
	Messagename and Arity specify a message which has been declared to
	be a class method for Class.  Methods are by default class methods,
	so this fact is only needed for a message which is declared to be an
	instance methods for some class, but class method for some
	descendant class.

    inherit_from(Class, Messagename, Arity, Op, Parent)
	The user has declared that Class should inherit message
	Messagename/Arity (of type Op) from Parent, which is one of
	Class's superclasses.

    uninherit_from(Class, Messagename, Arity, Op, Parent)
	The user has declared that Class should *not* inherit message
	Messagename/Arity (of type Op) from Parent, which is one of
	Class's superclasses, or is unbound.

    inline_method(Name, Arity, Class, Self, Op, Message, Body)
	The message Name/Arity (of type Op) is defined directly for Class as
	in inline method.  Message is the method head, and Body implements
	the method, where Self is the message recipient.

--description

Internally, this package maintains dynamic versions of all these preds, too,
which have the same name, with 'dyn_' on the front.  These are needed
because when compiling a file with qpc, clauses emitted earlier during
the compilation of the file are not available to be called when
compiling later parts of the file.  The 'dyn_' versions of these
predicates are retracted after the file is compiled.

There is also a version of each of these predicates for internal use
that checks both the dynamic and static versions, which has the same
name, but 'x_' prepended.

================================================================*/

:- multifile
	class_size/3,
	class_file/2,
	term_class/4,
	subclass/2,
	superclass/3,
	direct_slot/5,
	direct_message/4,
	(instance_method)/4,
	(class_method)/3,
	inherit_from/5,
	uninherit_from/5,
	inline_method/7.

% [PM] 4.2.1 SPIDER may complain about dynamic predicates not being volatile.
% I suspect that these should _not_ be volatile. TODO: suppress warning with
% "not volatile" coments, as appropriate.
:- dynamic
	dyn_class_size/3,
	dyn_class_file/2,
	dyn_term_class/4,
	dyn_subclass/2,
	dyn_superclass/3,
	dyn_direct_slot/5,
	dyn_direct_message/4,
	dyn_instance_method/4,
	dyn_class_method/3,
	dyn_inherit_from/5,
	dyn_uninherit_from/5,
	dyn_inline_method/7,
	inline_method_pos/5,
	current_clause_position/1,
	user_inlineable_parameters/2,
	forward_def_slot_type/2,
	method_stash/5,
	pending_class/1,
	pending_ordinary_class/0,
	declared_multifile_discontiguous/1.

%  debug_mode
%  True if we're currently in debug mode.  Defaults off.
:- dynamic debug_mode/0.

% [PM] 4.1.3 Added missing meta-predicate (for SPIDER xref)
:- meta_predicate with_effective_position(0,+).

% for spxref
:- public((
	try_to_inline/7,
	generate_inline/8,
	class_size/3,
	class_file/2,
	term_class/4,
	subclass/2,
	superclass/3,
	direct_slot/5,
	direct_message/4,
	(instance_method)/4,
	(class_method)/3,
	inherit_from/5,
	uninherit_from/5,
	inline_method/7
	  )).

% Allow files to be recompiled properly 
reinit_class(Classname) :-
	retractall(dyn_class_size(Classname,_,_)),
	retractall(dyn_class_file(Classname,_)),
	retractall(dyn_term_class(Classname,_,_,_)),
	retractall(dyn_subclass(_,Classname)),
	retractall(dyn_superclass(Classname,_,_)),
	retractall(dyn_direct_slot(Classname,_,_,_,_)),
	retractall(dyn_direct_message(Classname,_,_,_)),
	retractall(dyn_instance_method(Classname,_,_,_)),
	retractall(dyn_class_method(Classname,_,_)),
	retractall(dyn_inherit_from(Classname,_,_,_,_)),
	retractall(dyn_uninherit_from(Classname,_,_,_,_)),
	retractall(dyn_inline_method(_,_,Classname,_,_,_,_)),
	retractall(inline_method_pos(_, _, _, Classname, _)).

/*================================================================

			  Modules Maintained

Modules involved in this package:

++description

    objects	Defines predicates needed at runtime to use the
		class package facilities.

    obj_decl	(this module).  Contains code needed to compile
		class definitions and also to optimize all message
		sends.  Also defines needed operators.  This file
		should be loaded with
++verbatim
	:- load_files(library(obj_decl),
		[when(compile_time), if(changed)]).
--verbatim
		by any file that uses the objects package.  This module
		also contains clauses describing the classes that
		have been defined.

    class_message_clauses
		Contains all the clauses that connect a message with
		the methods, the code that implements them.  Each
		clause in the module is of the form:
++verbatim
	'<-msg'(cls(_),Ob,A,B,C,...) :-
		class_method_clauses:'anc<-msg'(A,Ob,Offset,B,C,...).
--verbatim
		where msg is any message, cls is any class that
		understands that message, anc is the ancestor of cls
		that cls inherits msg from, and '<-' is the message
		operator.  A, B, C, ...  are the arguments of message
		msg, and Ob is used to pass the (Self) object, which
		is the same as the first argument to msg, to the
		method without having to reconstruct it on the heap.
		Offset is the offset within cls at which anc's slots
		can be found.  This offset is used by the method if it
		needs to access the object's slots.  Note that the
		rather strange argument ordering and shuffling is done
		to take advantage of indexing and minimize argument
		movement, avoiding extra instructions.

                Also included in this module is a clause of the form
++verbatim
        '<-'(msg(A,B,C,...),Obj) :-
                '<-msg'(Obj,Obj,A,B,C,...).
--verbatim
                Note that this is version of the predicate '<-'/2 (and
	        the other message operators) is not exported, and it has
	        its arguments reversed from what you would expect.  It is
	        called from the public version of '<-'/2 in classes.pl
	        and the arguments are reversed to allow indexing on the 
	        message term.  This predicate serves to speed up messages
	        in the case where the message is unknown (variable) at
	        compile time.

    class_method_clauses
		Contains all static method bodies.

    class_instance_method
		Contains all instance method bodies.

    class_instance_default
		Contains all default clauses for instance methods.
		These are actually static clauses.  We override the
		default clause(s) for a specific instance by
		asserta-ing a clause in class_instance_method that
		begins with a cut.  This means that the initial
		clauses for instance methods do not need to be copied
		for each new instance of a class.

    class_term_slots
		Contains all term slot contents, stored as binary
		dynamic predicates, with predicate name being the
		name of the dynamic slot, first (index) argument
		being the address of the object, and the second
		argument being the value.
--description


================================================================*/


/****************************************************************

		     Handling a Class Declaration

****************************************************************/

/*================================================================

			    Top Level Code

================================================================*/

%  expand_class_begin(+Classname, +Classdef, -Clauses)
%  Clauses is a list of the clauses to be generated to implement the
%  definition of class Classname as Classdef.

expand_class_begin(Classname, Classdef, Clauses) :-
	finish_pending_class(_, Clauses, Clauses1),
        Error = error(_,_),
	on_exception(Error, expand_class_begin1(Classname,Classdef,Clauses1),
		(   Clauses1 = [],
		    Goal = (:- class Classname=Classdef),
		    identified_message(error, class_decl_error(Goal,Error))
		)).


expand_class_begin1(Classname, Classdef, Clauses) :-
	begin_pending_class(Classname),
	get_current_file(File),
	add_bookkeeping_fact(class_file(Classname,File), Clauses,
			     Clauses1),
	(   analyze_class_defn_hook(Classdef, Classname, Clauses1) ->
		true
	;   analyze_class_defn(Classdef, Classname, Clauses1)
	).



%  analyze_class_defn(+Classdef, +Classname, -Clauses)
%  Clauses is a list of the clauses that need to be generated to
%  define Classname as a class defined by Classdef.  This predicate
%  has a _hook version to allow users to extend the range of type
%  definitions allowed.

analyze_class_defn(term(Term), Classname, Clauses) :-
	!,
	analyze_class_defn(term(Term,true,term), Classname, Clauses).
analyze_class_defn(term(Term,Goal), Classname, Clauses) :-
	!,
	analyze_class_defn(term(Term,Goal,term), Classname, Clauses).
analyze_class_defn(term(Term,Goal,Essence), Classname, Clauses) :-
	!,
	%  a term class is a class whose instances are not
	%  represented as Prolog terms of arity one whose arguments
	%  are memory addresses.
	(   Essence == term ->
		true
	;   verify_essence(Essence)
	),
	add_bookkeeping_fact(term_class(Classname,Term,Goal,Essence),
			     Clauses, Clauses1),
	(   nonvar(Term),
	    functor(Term, Classname, 1) ->
		true				% term already looks like
						% an instance of Classname
	;   add_multifile_clause((objects:class_of_hook(Term,Classname):-Goal),
				 Clauses1, [])
	).
analyze_class_defn(Classdef, Classname, Clauses) :-
	assert(pending_ordinary_class),
	analyze_class_defn(Classdef, Parents, [], Direct_slots, [],
			   Clauses, Clauses1),
	class_family_clauses(Parents, Classname, 0, Ancsize, Clauses1,
			     Clauses2),
	(   setof(Attrib, default_slot_attrib(Attrib), Attribs0) ->
		true
	;   Attribs0 = []
	),
	record_direct_slots(Direct_slots, Classname, Ancsize, Size, Attribs0,
			    Clauses2, Clauses3),
	primitive_type_size(atom, HeaderSize, _), % [MC] 4.0.5
	add_bookkeeping_fact(class_size(Classname,Size,HeaderSize), Clauses3, Clauses4), % [MC] 4.0.5
	% this must come after add_bookkeeping_fact/3 call, because class
	% size must be known.
	record_slot_admin(Classname, Clauses4, []).


%  analyze_class_defn(+Classdef, -Parents, +Parents0, -Direct_slots,
%		+Direct_Slots0, -Clauses, +Clauses0)
%  Parents is a list of superclasses of a class defined by Classdef,
%  followed by Parents0, and Direct_Slots is a list of the slots
%  present in such a class in addition to the slots inherited from
%  Parents, followed by Direct_Slots0.  Clauses is a list of clauses
%  containing any information lost by remembering only the parent
%  classes and the direct slots from this declaration.  Note that
%  there is a _hook version of this predicate, so that extensions to
%  this package may define other kinds of class definition.

analyze_class_defn(Spec, Ps, Ps0, Ss, Ss0, Clauses, Clauses0) :-
	(   analyze_class_defn_hook(Spec, Ps, Ps0, Ss, Ss0, Clauses,
				    Clauses0) ->
		true
	;   analyze_class_defn1(Spec, Ps, Ps0, Ss, Ss0, Clauses, Clauses0) ->
		true
	;   throw(bad_defn(Spec))
	).


analyze_class_defn1(X+Y, Ps, Ps0, Slots, Slots0, Clauses, Clauses0) :-
	!,
	analyze_class_defn(X, Ps, Ps1, Slots, Slots1, Clauses,
			   Clauses1),
	analyze_class_defn(Y, Ps1, Ps0, Slots1, Slots0, Clauses1,
			   Clauses0).
analyze_class_defn1([], Ps, Ps, Slots, Slots, Clauses, Clauses) :-
	!.
analyze_class_defn1([S|Ss], Ps, Ps, [S|Slots1], Slots0, Clauses,
		Clauses) :-
	!,
	append(Ss, Slots0, Slots1).
analyze_class_defn1(Name, [Name|Ps], Ps, Slots, Slots, Clauses,
		Clauses) :-
	atom(Name),
	!,				% in case there are more clauses later
	(   defined_class(Name) -> true
	;   throw(bad_parent(Name))
	).


%  verify_essence(+Essence)
%  Essence is a valid essence argument of a term class specification
%  term.  If it is not, we raise an exception.

verify_essence(Essence) :-
	var(Essence),
	!,
	throw(bad_term_class_essence(Essence)).
verify_essence([]) :-
	!.
verify_essence([E|Es]) :-
	!,
	(   nonvar(E),
	    E = (E1=Var),
	    var(Var),
	    nonvar(E1),
	    E1 = (Name:Type),
	    atom(Name),
	    nonvar(Type),
	    slot_type_name(Type) -> true
	;   throw(bad_term_class_essence(E))
	),
	verify_essence(Es).
verify_essence(Essence) :-
	throw(bad_term_class_essence(Essence)).


/*================================================================

			   Handling Superclasses

================================================================*/

%  class_family_clauses(+Parents, +Class, +Size0, -Size, -Clauses, +Clauses0)
%  Clauses is the list of clauses needed to specify Parents as the parents
%  of Class.  Size0 is the offset from the beginning of Class that the first
%  parent on Parents should be placed, and Size is the size of all of
%  Class's parents together.  We create a subclass/2 and a superclass/3 fact
%  for each parent.

class_family_clauses([], _, Size, Size, Clauses, Clauses).
class_family_clauses([Parent|Parents], Class, Prefix, Size,
		Clauses, Clauses0) :-
	x_class_size(Parent, Psize, _),
	!,					% x_class_size not determinate
	% NB: the following assumes that nothing will need to be aligned on
	% more than a 4 byte boundry!!!  This should really become more
	% sophisticated, aligning based on the largest alignment needed
	% for any slot in Parent.
	primitive_type_size(float, _, Alignment), % [MC] 4.0.5
	Prefix1 is (Prefix+Alignment-1) /\ (-Alignment), % [MC] 4.0.5
	Prefix2 is Prefix1 + Psize,
	add_bookkeeping_fact(subclass(Parent,Class), Clauses, Clauses1),
	add_bookkeeping_fact(superclass(Class,Parent,Prefix1),
		Clauses1, Clauses2),
	class_family_clauses(Parents, Class, Prefix2, Size,
		Clauses2, Clauses0).


/*================================================================

			 Handling Slot Declarations

================================================================*/


%  record_direct_slots(+Slots, +Class, +Offset, -Size, +Attribs,
%		-Clauses, +Clauses0)
%  Clauses is the list of clauses needed to specify Slots as the slots of
%  Class, beginning at Offset bytes from the beginning of Class.  Clauses0
%  is the tail of Clauses after these clauses.  Size is the total size of
%  each instance of Class.  Attribs is the set of default slot attributes,
%  which each slot may override.

record_direct_slots([], _, Size, Size, _, Clauses, Clauses).
record_direct_slots([Slot|Slots], Class, Offset, Size, Attribs0,
		    Clauses, Clauses0) :-
	compile_slot(Slot, Class, Offset, Offset1, _, _, Attribs0, Clauses,
		     Clauses1),
	record_direct_slots(Slots, Class, Offset1, Size, Attribs0,
		Clauses1, Clauses0).



%  compile_slot(+Slotspec, +Class, +Offset0, -Next, -Name, -Type, +Attribs,
%		-Clauses, +Clauses0)
%  Clauses is the list of clauses needed to specify Slotspec as the slot of
%  Class that falls Offset0 bytes from the beginning of Class.  Clauses0
%  is the tail of Clauses after these clauses.  Next is the offset of the
%  next slot in Class (i.e., the address one byte after this slot).  Name
%  and Type are the name and type of this slot.

:- multifile compile_slot/9.

compile_slot(Name:Type0, Class, Offset0, Next, Name, Type, Attribs0,
		Clauses, Clauses0) :-
	(   inherited_slot(Class, Name, Oldtype, Offset1, _) ->
		% shadowing an existing slot:  place it where old slot was
		Next = Offset0
	;   Offset1 = Offset0,
	    Next = Next1,
	    Oldtype = 0				% couldn't possibly be a type
	),
	!,
	% Offset1 is the offset to start this slot at, and Next1 is the
	% next available byte after this slot (we don't care about this
	% when we're shadowing an old slot).
	(   compile_slot_type_hook(Type0, Class, Offset1, Attribs0,
				   Type, Offset, Next1, Attribs) ->
		true
	;   compile_slot_type(Type0, Class, Offset1, Attribs0,
			  Type, Offset, Next1, Attribs) ->
		true
	;   throw(bad_slot_type(Type0))
	),
	(   Oldtype \== 0,
	    \+ (Offset=Offset1,compatible_slot_types(Type, Oldtype)) ->
		throw(invalid_slot_shadow(Name,Type,Oldtype))
	;   true
	),
	record_slot_type(Class, Name, Type, Offset, Attribs,
			 Clauses, Clauses1),
	handle_slot_attribs(Attribs, Class, Name, Type, Clauses1, Clauses0).
compile_slot(Slot=Initval, Class, Offset, Next, Name, Type, Attribs0,
		Clauses, Clauses0) :- !,
	del_element(initvalue(_), Attribs0, Attribs1),
	compile_slot(Slot, Class, Offset, Next, Name, Type,
		[initvalue(Initval)|Attribs1], Clauses, Clauses0).
compile_slot(private(Slot), Class, Offset, Next, Name, Type, Attribs0,
		Clauses, Clauses0) :- !,
	del_element(visibility(_), Attribs0, Attribs1),
	add_element(visibility(private), Attribs1, Attribs2),
	compile_slot(Slot, Class, Offset, Next, Name, Type, Attribs2,
		Clauses, Clauses0).
compile_slot(protected(Slot), Class, Offset, Next, Name, Type, Attribs0,
		Clauses, Clauses0) :- !,
	del_element(visibility(_), Attribs0, Attribs1),
	add_element(visibility(protected), Attribs1, Attribs2),
	compile_slot(Slot, Class, Offset, Next, Name, Type, Attribs2,
		Clauses, Clauses0).
compile_slot(public(Slot), Class, Offset, Next, Name, Type, Attribs0,
		Clauses, Clauses0) :- !,
	del_element(visibility(_), Attribs0, Attribs1),
	add_element(visibility(public), Attribs1, Attribs2),
	compile_slot(Slot, Class, Offset, Next, Name, Type, Attribs2,
		Clauses, Clauses0).

%  compile_slot_type(+Slottype, +Class, +Offset0, +Attribs0,
%		-Type, -Offset, -Next, -Attribs)
%  Type and Name are the type and name of a slot in class Class
%  described with name Slotname and of type Slottype.  Type and Name
%  may be different than Slotname and Slottype, but generally Name
%  will be the same as Slotname.  Offset is the offset of the slot,
%  and Next is the offset of the next slot in this class (i.e., the
%  address one byte after this slot).  Offset0 is the offset of the
%  first byte available for this slot (the previous slot's Next).
%  Attribs is a list of the slot's attributes; Attribs0 is the list of
%  attributes previously known.  This predicate has a _hook version so
%  that extensions to this package can interpret new forms of slot
%  declaration, or interpret the usual forms differently.

compile_slot_type(term, _, Next, Attribs, term, Next, Next, Attribs) :-
	!.
compile_slot_type(object, _, Offset0, Attribs, object_ptr, Offset, Next,
		  Attribs) :-
	!,
	align_pointer(Offset0, Offset, Next).
compile_slot_type(Type, _, Offset0, Attribs, Type, Offset, Next, Attribs) :-
	primitive_type_size(Type, Size, Alignment),
	integer(Size),
	!,
	align(Offset0, Size, Alignment, Offset, Next).
compile_slot_type(Type, _, Offset0, Attribs, object_ptr(Type), Offset, Next,
		  Attribs) :-
	defined_class(Type),
	!,
	align_pointer(Offset0, Offset, Next).
compile_slot_type(Type0, Class, Offset0, Attr0, Type, Offset, Next, Attr) :-
	x_term_class(Type0, Term, _, Essence),
	!,
	(   Essence == term ->
		compile_slot_type(term, Class, Offset0, Attr0, Type, Offset,
				  Next, Attr)
	;   Type = term_object(Type0,Term,Slots),
	    align_pointer(Offset0, Offset, _),
	    align_essence(Essence, Class, 0, Attr0, Slots, Next1, Attr),
	    Next is Offset + Next1
	).
compile_slot_type(pointer(Type), _, Offset0, Attribs, struct_ptr(Type),
		  Offset, Next, Attribs) :-
	atom(Type),
	!,
	align_pointer(Offset0, Offset, Next).
compile_slot_type(Type, Class, Offset0, Attribs, object_ptr(Type), Offset, Next,
		  Attribs) :-
	!,
	forward_defined_slot_type(Type, Class),
	align_pointer(Offset0, Offset, Next).



%  record_slot_type(+Class, +Name, +Type, +Offset, +Attribs,
%		-Clauses, +Clauses0)
%  Record the fact that Name is a slot of type Type for Class stored at Offset 
%  from the beginning of the object.  This slot has attributes Attribs.

record_slot_type(Class, Name, Type, Offset, Attribs, Clauses, Clauses0) :-
	add_bookkeeping_fact(direct_slot(Class,Name,Type,Offset,Attribs),
		Clauses, Clauses0).


%  slot_type_name(+Type)
%  Type is a valid type name for a slot.  This predicate is declared
%  multifile so that extensions to this package can interpret new
%  forms of slot declaration.  Generally, this predicate should
%  be extended when compile_slot_type_hook/8 is extended.

:- multifile slot_type_name/1.

slot_type_name(Type) :-
	primitive_type_size(Type, Size, _),
	integer(Size),
	!.
slot_type_name(Type) :-
	defined_class(Type),
	!.
slot_type_name(Type) :-
	x_term_class(Type, _, _, _),
	!.
slot_type_name(pointer(Type)) :-
	atom(Type),
	!.
slot_type_name(term) :-
	!.

%  compatible_slot_types(+Type1, +Type2)
%  Type1 is the new type of a slot, and Type2 is the old type, and the
%  two specs are compatible.  This predicate is declared multifile so that
%  users can extend the kinds of types supported by this package.

:- multifile compatible_slot_types/2.

compatible_slot_types(Type, Type) :-
	!.					% identical types are always
						% compatible
compatible_slot_types(object_ptr(C1), object_ptr(C2)) :-
	!,
	class_ancestor(C1, C2).
compatible_slot_types(object_ptr(_), object_ptr) :-
	!.					% This pred is multifile



%  align_essence(+Essence, +Class, +Offset0, +Attr0, -Slots, -Next, -Attr)
%  Slots is a list of term_slot(Name,Var,Type,Offset) terms, each
%  defining a part of the essence of a term class term stored as a
%  slot of Class beginning at Offset0.  Essence is the list of
%  Var:Type terms specified as the Essence part of the term class
%  definition.  Attr0 is a list of the attributes of the slots of
%  Class up to this slot, and Attr is this list for the slots
%  including this slot.  Next is the offset of the next available byte
%  of Class instances (i.e., the size of structure allocated so far).

align_essence([], _, Next, Attr, [], Next, Attr).
align_essence([Name:Type0=Var|Es], Class, Offset0, Attr0,
	      [term_slot(Name,Var,Type,Offset)|Slots], Next, Attr) :-
	compile_slot_type(Type0, Class, Offset0, Attr0, Type, Offset,
			  Offset1, Attr1),
	align_essence(Es, Class, Offset1, Attr1, Slots, Next, Attr).	



%  handle_slot_attribs(+Attribs, +Class, +Name, +Type, -Clauses, +Clauses0) 
%  Clauses is the list of clauses defined by Attribs, the list of attributes
%  of slot Name of class Class, of type Type.  Clauses0 is the tail of
%  Clauses after these clauses.

handle_slot_attribs([], _, _, _, Clauses, Clauses).
handle_slot_attribs([Attr|Attrs],  Class, Name, Type, Clauses, Clauses0) :- 
	(   handle_slot_attrib_hook(Attr, Class, Name, Type, Clauses,
				    Clauses1) ->
		true
	;   handle_slot_attrib(Attr, Class, Name, Type, Clauses, Clauses1) ->
		true
	;   Clauses = Clauses1
	),
	handle_slot_attribs(Attrs, Class, Name, Type, Clauses1, Clauses0). 


%  handle_slot_attrib(+Attrib, +Class, +Name, +Type, -Clauses, +Clauses0)
%  Clauses is the list of clauses defined by Attrib, an attribute of slot
%  Name of class Class, of type Type.  Clauses0 is the tail of Clauses after
%  these clauses.  This predicate has a _hook version so that extensions to
%  this package can understand new slot attributes (which could be
%  introduced by adding a clause for default_slot_attrib/1, or by adding a
%  clause to compile_slot/9).

handle_slot_attrib(visibility(public), _, Name, _, Clauses, Clauses0) :-
	!,
	Term =.. [Name,Val],
	expand_method(_, >>, Term, fetch_slot(Name,Val), Clauses, Clauses1),
	expand_method(_, <<, Term, store_slot(Name,Val), Clauses1, Clauses0).
handle_slot_attrib(initvalue(_), _, _, _, Clauses, Clauses) :- 
	!.					% initializations handled later




:- mode default_slot_attrib(?).
:- multifile default_slot_attrib/1.

%  default_slot_attrib(-Attrib)
%  Attrib is a default attribute of a slot.  This predicate is declared
%  multifile so that extensions to this package can add default slot
%  attributes.
default_slot_attrib(visibility(private)).
% default_slot_attrib(public).
% default_slot_attrib(protected).

%  record_slot_admin(Class, Clauses, Clauses0)
%  Clauses is the list of clauses needed to implement slot initialization
%  and destruction for all the slots of class Class, followed by Clauses0.

record_slot_admin(Class, Clauses, Clauses0) :-
	(   setof(Slot, x_slot(Class,Slot), Slots),
	    instance_term(Class, Self, Test),
	    slot_admin(Slots, Class, Self, true, Init, true, Cleanup1) ->
		(   Init == true ->
			Clauses = Clauses1
		;   InitHead = class_message_clauses:initialize_slots(Self),
		    generate_method_clause(InitHead, Test, Init, Class, Self,
					   Self, '', '', inline, Clauses,
					   Clauses1)
		),
		instance_method_cleanup(Class, Self, Cleanup1, Cleanup),
		(   Cleanup == true ->
			Clauses1 = Clauses0
		;   CleanupHead = class_message_clauses:cleanup_slots(Self),
		    generate_method_clause(CleanupHead, Test, Cleanup, Class,
					   Self, Self, '', '', inline,
					   Clauses1, Clauses0)
		)
	;   Clauses = Clauses0
	).


%  slot_admin(+Slots, +Class, +Self, +Init0, -Init, +Cleanup0, -Cleanup)
%  Init is the goal to initialize all the slots named on Slots, which are
%  slots of Class, conjoined with Init0.  Similarly, Cleanup is the goal
%  to clean up the same slots, conjoined with Cleanup0.  Self is the object
%  whose slots are to be initialized and cleaned up.

slot_admin([], _, _, Init, Init, Cleanup, Cleanup).
slot_admin([Slot|Slots], Class, Self, Init0, Init, Cleanup0, Cleanup) :-
	x_slot(Class, Slot, Type, _, Attribs),
	(   slot_admin(Type, Slot, Attribs, Self, Init1, Cleanup1) ->
		conjoin(Init1, Init0, Init2),
		conjoin(Cleanup1, Cleanup0, Cleanup2)
	;   Init2 = Init0,
	    Cleanup2 = Cleanup0
	),
	slot_admin(Slots, Class, Self, Init2, Init, Cleanup2, Cleanup).


%  slot_admin(+Type, +Slot, +Attribs, +Self, -Init, -Cleanup)
%  Init is the goal to execute to properly initialize slot Slot of
%  type Type whose attributes are Attribs, and Cleanup is the goal to
%  clean it up before freeing the object it lives in.  Self is the
%  object whose slots are to be initialized and cleaned up.  This
%  predicate is declared multifile so that users may extend the set of
%  types supplied by this package.

:- multifile slot_admin/6.

slot_admin(Type, Slot, Attribs, _, store_slot(Slot,Val), true) :-
	primitive_type_size(Type, _, _),
	member(initvalue(Val), Attribs),
	% XXX TODO raise exception if Val is inappropriate for the slot.
	!.
slot_admin(atom, Slot, _, _, store_slot(Slot,''), store_slot(Slot,'')) :-
	!.
slot_admin(term_object(_,_,Subslots), Slot, Attribs, Self, Init, Cleanup) :-
	(   member(initvalue(Val), Attribs) ->
		% initializing whole term:  no need to initialize parts
		Init = store_slot(Slot,Val),
		Cleanup = true
	;   term_slot_admin(Subslots, Slot, Self, Init, Cleanup)
	),
	!.
slot_admin(object_ptr(Class), Slot, Attribs, _, Init, Cleanup) :-
	member(initvalue(Descriptor), Attribs),
	functor(Descriptor, Class, _),
	% XXX TODO raise exception if functor goal fails
	!,
	Init=(create(Descriptor,Obj),store_slot(Slot,Obj)),
	Cleanup=(fetch_slot(Slot,Obj),Obj\==null->destroy(Obj);true).
slot_admin(struct_ptr(_), _Slot, Attribs, _, _, _) :-
	member(initvalue(_Descriptor), Attribs),
	!,
	% XXX TODO raise exception to report an invalid initialization
	fail.
slot_admin(object_ptr, Slot, Attribs, _, Init, Cleanup) :-
	member(initvalue(Descriptor), Attribs),
	!,
	Init=(create(Descriptor,Obj),store_slot(Slot,Obj)),
	Cleanup=(fetch_slot(Slot,Obj),Obj\==null->destroy(Obj);true).
slot_admin(term, Slot, Attribs, Self, Init, Cleanup) :- !,
	(   member(initvalue(Val), Attribs) ->
		true
	;   Val = ''
	),
	arg(1, Self, Addressvar),
	term_slot_maintenance(Slot, Addressvar, Val, _, Init, Cleanup).

%  term_slot_admin(+Subslots, +Slot, +Self, -Init, -Cleanup)
term_slot_admin([], _, _, true, true).
term_slot_admin([term_slot(Name,_,Type,_)|Slots], Slot, Self, Init, Cleanup) :-
	subslot(Slot, Name, Subslot),
	(   slot_admin(Type, Subslot, [], Self, Init1, Cleanup1) ->
		true
	;   Init1 = true,
	    Cleanup1 = true
	),
	term_slot_admin(Slots, Slot, Self, Init2, Cleanup2),
	conjoin(Init1, Init2, Init),
	conjoin(Cleanup1, Cleanup2, Cleanup).

/*================================================================

			    Administration

================================================================*/

expand_class_end(Class, Clauses) :-
	finish_pending_class(Oldclass, Clauses, []),
	(   Oldclass == 0 ->
		(   nonvar(Class) -> Goal = (:-end_class(Class))
		;   Goal = (:- (end_class))
		),
		identified_message(warning,
			context_error(Goal, not_in_class_defn, 0))
	;   nonvar(Class), Class\==Oldclass ->
		identified_message(warning,
			consistency_error((:-end_class(Class)),
				Oldclass, Class, ''))
	;   true
	).


%  begin_pending_class(+Classname)
%  begin the definition of the Classname class.  We know that there is
%  currently no pending class definition.

begin_pending_class(Classname) :-
	Goal = (:- class Classname='...'),
	(   var(Classname) ->
	        illarg(var, Goal, 0, Classname)
	;   \+ atom(Classname) ->
	        illarg(type(atom), Goal, 0, Classname)
	;   x_class_file(Classname, Oldfile),
	    get_current_file(Newfile),
	    Oldfile \== Newfile ->
	        illarg(permission(redefine,class,''), Goal, 0, Classname)
	;   retractall(pending_ordinary_class),
	    assert(pending_class(Classname)),
	    reinit_class(Classname)
	).



%  finish_pending_class(-Classname, -Clauses, +Clauses0)
%  wrap up the definition of the currently pending class, which is
%  Classname.  Clauses is the list of clauses produced by this,
%  followed by Clauses0.  If no class defn is pending, Classname is
%  0 and Clauses=Clauses0.

finish_pending_class(Classname, Clauses, Clauses0) :-
	(   retract(pending_class(Classname)) ->
		handle_methods(Classname, Clauses, Clauses1),
	        gen_dynamics_for_term_slots(Classname, Clauses1, Clauses0)
	;   Classname = 0,
	    Clauses=Clauses0
	),
	retractall(pending_ordinary_class),
	retractall(user_inlineable_parameters(_,_)).


gen_dynamics_for_term_slots(_Classname) --> [].
/****
%  Term slots are represented as dynamic predicates: generate dynamic
%  declarations for them here to avoid warnings from qcon.  They are
%  initialized to the empty atom '' at object creation time, so declaring 
%  them dynamic is only to keep qcon quiet.
%  DISABLED FOR SP.

gen_dynamics_for_term_slots(Classname, 
	                   [class_term_slots:(:- dynamic List)|Clauses0], 
			   Clauses0) :-
	findall(Slot/2,
	        x_direct_slot(Classname,Slot,term,_,_),
		List).
****/

/***************************************************************

		     Handling Method Definitions

****************************************************************/

/*================================================================

		 Handling a Method Clause When Found

When a method clause is found, it is recorded for later analysis, and
no code is generated.  When the end of the class definition is
reached, all method clauses are analyzed, and the final clauses are
generated.  This allows us to determine which methods are simple and
defined with only a single clause; these methods are compiled more
efficiently.

================================================================*/

%  expand_method(+Self, +Op, +Message, +Body, -Clauses, +Clauses0)
%  Clauses is a list of the clauses needed to implement Body as the
%  method for Message as an Op message for the current pending class
%  if Self is the object receiving the message.  Clauses0 is the tail
%  of Clauses after these clauses.  Note that we generate
%  no code at this point; instead we save the method for later
%  analysis and code generation.

expand_method(Self, Op, Message, Body, Clauses, Clauses) :-
	% this horrible hack is here to keep obj_decl from trying
	% to expand the clauses for <-, << and >> in objects.pl
	% and complaining because they don't occur in a class defn.
	\+ prolog_load_context(module, objects),
	Head =.. [Op,Self,Message],		% for error messages
	(   \+ pending_class(_) ->
		identified_message(error, context_error((Head:-Body),
						   not_in_class_defn,0))
	% It doesn't make any sense for Self to be nonvar for ordinary
	% classes, but for term classes it is sensible, so this isn't a good
	% place to check for that case.  This test is now made in
	% expand_ordinary_method/5.
	;   % we save away method clauses so that we can properly decide
	    % how to handle them, having seen all the clauses for a
	    % particular method.
	    source_term_position(Pos),
	    assertz(method_stash(Message, Self, Op, Body, Pos))
	).

%  method_stash(*Message, *Self, *Op, *Body, *Position)
%  Dynamic predicate storing all the methods defined for the current
%  class.

/*================================================================

	      Handling All Methods for the Current Class

Here's the code that actually handles the stashed method clauses.

================================================================*/

%  handle_methods(+Classname, -Clauses, +Clauses0)
%  Clauses is a list of the clauses needed to implement all the
%  methods of class Classname, which is the current class, followed by
%  Clauses0.  This included method clauses, and the linkage clauses
%  which implement inheritance.

handle_methods(Classname, Clauses, Clauses0) :-
	(   setof(msg(Op,Name,Arity),
		  stashed_message(Classname, Op, Name, Arity), Msgs) ->
		handle_stashed_messages(Msgs, Classname, Clauses, Clauses1)
	;   Clauses1 = Clauses
	),
	retractall(method_stash(_,_,_,_,_)),
	generate_linkages(Classname, Clauses1, Clauses0).


%  stashed_message(+Class, -Op, -Name, -Arity)
%  Message Name/Arity of type Op is defined/declared for Class, which is the
%  current class.

stashed_message(Class, Op, Name, Arity) :-
	(   method_stash(Message, _, Op, _, _)
	;   Op = (<-),
	    dyn_instance_method(Name, Arity, Class, _)
	),
	functor(Message, Name, Arity).


%  handle_stashed_messages(+Msgs, +Class, -Clauses, +Clauses0)
%  Clauses is a list of all the clauses needed to implement the
%  messages on the list Msgs.  Msgs is a list of msg(Op,Name,Arity)
%  terms where each term indicates that there are some stashed clauses
%  for message Name/Arity of type Op.  Class is the class that defines
%  these methods.

handle_stashed_messages([], _, Clauses, Clauses).
handle_stashed_messages([msg(Op,Name,Arity)|Msgs], Class, Clauses,
		Clauses0) :-
	functor(Message, Name, Arity),
	findall(method(Message,Self,Body,Pos),
		method_stash(Message,Self,Op,Body,Pos),
		Methods),
	(   dyn_direct_message(_, Name, Arity, Op) ->
		Clauses = Clauses1		% message defined earlier in
						% this file
	;   get_current_file(ThisFile),
	    x_direct_message(Class1, Name, Arity, Op),
	    x_class_file(Class1, OtherFile),
	    OtherFile \== ThisFile ->
		Clauses = Clauses1		% message defined earlier in
						% another file
	;   define_message(Name, Arity, Op, Clauses, Clauses1)
	),
	add_bookkeeping_fact(direct_message(Class,Name,Arity,Op), Clauses1,
			     Clauses2),
	% insert call to hook pred here 
	(   Op == (<-),
	    is_instance_method(Name, Arity, Class, Predname) ->
		expand_instance_method(Methods, Class, Predname,
				       Clauses2, Clauses3)
	;   Methods = [method(Message1,Self1,Body1,Pos1)],
	    % a single clause:  inline if possible
	    with_effective_position(
		try_to_inline(Body1, Class, Self1, Op, Message1,
			      Clauses2, Clauses3),
		Pos1) ->
		true
	;   expand_ordinary_method(Methods, Class, Op, Clauses2, Clauses3)
	),
	handle_stashed_messages(Msgs, Class, Clauses3, Clauses0).


%  try_to_inline(+Body, +Class, +Self, +Op, +Message, -Clauses, +Clauses0)
%  Inline Body, the body for the Self Op Message message for class Class, if
%  possible.

try_to_inline(Body, Class, Self, Op, Message, Clauses, Clauses0) :-
	inlineable_body(Body, Class, Self, Op, Message),
	expand_inline_method(Self, Op, Message, Body, Class,
			     Clauses, Clauses0).



/*================================================================

			   Ordinary Methods

The aim of this package is to give the greatest possible performance
with the least possible memory usage (of course).  For message
sending, the compromise chosen in this package is to have a predicate
for every message name/arity, which dispatches on the message
recipient's class, calling the predicate that implements that method.
Thus there is a predicate, called a linkage predicate, for each
message in the system, with a clause for each class which understands
this message.  There is also a predicate, called a method predicate,
for each distinct implementation of this message.  Thus if one class
defines a particular message, and many classes inherit it, there will
only be one copy of the code implementing this message.

For example, if class c defines a message m/2:
++verbatim
	Self <- m(X,Y) :-
		Self <- m1(X),
		Self <- m1(Y).
--verbatim
this clause would be compiled into the following method clause:
++verbatim
	class_method_clauses:'c<-m'(X,Self,_,Y) :-
		class_message_clauses:'<-m1'(Self,Self,X),
		class_message_clauses:'<-m1'(Self,Self,Y).
--verbatim

In addition, no matter how many clauses for this method are defined,
there will be two linkage clauses:
++verbatim
        class_message_clauses:'<-'(m(X,Y), Self) :-
	        class_message_clauses:'<-m'(Self, Self, X, Y).
	class_message_clauses:'<-m'(c(_), Self, X, Y) :-
		class_method_clauses:'c<-m'(X, Self, 0, Y).
--verbatim

*Self* is passed twice to class_message_clauses:m1/3 in the above code so
that it can be used for indexing to dispatch to the correct method,
without having to be reconstructed on the heap when it needed to be
passed to the actual method.  The rather strange argument ordering and
shuffling is done to minimize argument movement, avoiding extra
instructions.

The anonymous third argument in the head of the clause for
class_method_clauses:'c<-m'/4 is used to handle some subtle issues arising
from multiple inheritance.  This will be more fully described
elsewhere.

If sc is later defined as a subclass of c which does not shadow c's
definition of m/2, then we will also generate another clause for
class_message_clauses:'<-m'/4:

++verbatim
	class_message_clauses:'<-m'(sc(_), Self, X, Y) :-
		class_method_clauses:'c<-m'(X, Self, 0, Y).
--verbatim



================================================================*/



%  expand_ordinary_method(+Method_clauses, +Class, +Op, -Clauses, +Clauses0)
%  Method_clauses is a list of method(Message,Self,Body) terms comprising all
%  the clauses of Message, a Op message, for Class.  Clauses is a list of the
%  clauses to be generated for this method, followed by the list Clauses0.

expand_ordinary_method([], _, _, Clauses, Clauses).
expand_ordinary_method([method(Message,Self,Body,Pos)|Cl], Class, Op,
		       Clauses, Clauses0) :-
	(   nonvar(Self),
	    \+ x_term_class(Class, _, _, _) ->
		Head =.. [Op,Self,Message],
		identified_message(warning, type_error(Head, 1,
						variable, Self))
	;   true
	),
	method_goal(Class, Message, Self, Op, Offset, Head),
	with_effective_position(
	    expand_method_body(Body, Class, Self, Op, Message, Addr, [],
			       ordinary, Body1),
	    Pos),
	handle_address_expressions(Class, Self, Offset, Addr, Body2, Body1),
	add_clause((Head:-Body2), Clauses, Clauses1),
	expand_ordinary_method(Cl, Class, Op, Clauses1, Clauses0).


handle_address_expressions(_, _, _, [], Goal, Goal) :-
	!.				% if there are no accesses, we don't
					% need to prepare to access anything.
handle_address_expressions(Class, Object, Offset, Specs, Goal, Goal0) :-
	defined_class(Class),
	!,
	(   var(Object) ->
		Goal = (arg(1,Object,Addr), Base is Addr+Offset, Goal0)
	;   Offset == 0 ->
		arg(1, Object, Base),
		Goal = Goal0
	;   arg(1, Object, Addr),
	    Goal = (Base is Addr+Offset, Goal0)
	),
	(   foreach(O-A,Specs),
	    param(Base)
	do  handle_address(A, Base, O)
	).
handle_address_expressions(Class, Object, _, Specs, Goal, Goal) :-
	x_term_class(Class, _, _, _),
	!,
	(   foreach(Object,Specs),
	    param(Object)
	do  true
	).


%  handle_address(+-Address_spec, +Base, +Offset)
%  This pred is pretty slippery.  The semantics is that Base+Offset
%  equals Address_spec.  Address_spec may take one of two forms:
%  expression(E) or sum(B,O).  In the former, E should be bound to an
%  expression representing Base+Offset; in the later, B should be
%  bound to Base and O should be bound to Offset.  A special case of
%  the former is when Offset (which must be bound at time of call)
%  equals 0, in which case we'd like E to be bound to just Base,
%  rather than Base+0.

:- multifile handle_address/3.
% [PM] 4.2.1 SPIDER will complain that handle_address/3 ought to use cut. I think the complaint is valid but I dare not modify the source.

handle_address(expression(Base), Base, 0) :-
	!.
handle_address(expression(Base+Offset), Base, Offset).	
handle_address(sum(Base,Offset), Base, Offset).



%  generate_ordinary_linkage(+Name, +Arity, +Class, +Defclass, +Op, +Offset,
%		-Clauses, +Clauses0)
%  Clauses is a list of the (single) linkage clause needed for class Class
%  to link to the method clause for message Name/Arity of type Op, defined
%  by class Defclass, followed by Clauses0.  Offset is the byte offset
%  of the start of Defclass's slots in Class.

generate_ordinary_linkage(Name, Arity, Class, Defclass, Op, Offset, Clauses,
		 Clauses0) :-
	functor(Message, Name, Arity),
	instance_term(Class, Index, Test),
	message_goal(Message, Index, Self, Op, Head),
	method_goal(Defclass, Message, Self, Op, Offset, Body0),
	% conjoin(!, Body0, Body1), strictly superfluous cut (?) [MC] 4.0.0
	Body1 = Body0,
	conjoin(Test, Body1, Body),
	add_locked_clause((Head:-Body), Clauses, Clauses0).


%  method_goal(+Class, +Message, +Self, +Op, +Offset, -Goal)
%  Goal is the goal to execute to send Op message Message to Self, which is an
%  instance of (a descendant of) Class.  Offset is the byte offset from the
%  start of Self to the start of the Class part of Self.

method_goal(Class, Message, Self, Op, Offset, Goal) :-
	functor(Message, Msg, _),
	atom_concat(Class, Op, ClassOp),
	atom_concat(ClassOp, Msg, Pred),
	compose_method_goal(Pred, Self, Message, Offset, Goal).

compose_method_goal(Predname, Self, Message, Offset,
		    class_method_clauses:Goal) :-
	Message =.. [_|Args],
	(   Args == [] ->
		Goal =.. [Predname,Self,Offset]
	;   Args = [Arg1|Restargs],
	    Goal =.. [Predname,Arg1,Self,Offset|Restargs]
	).


%  instance_term(+Class, -Term, -Test)
%  instance_term(+Class, -Term, -Test, -Key, -Keygoal)
%  If X is an instance of Class, then (Term = X, call(Test)) succeeds.
%  This test need not fail for all Xs that are not instances of Class,
%  but it should rule out all instances of other classes.  Key is a
%  term derived from Term that would serve well as a key for first
%  argument indexing, and Keygoal is a goal that when executed, will
%  bind Key to such a key.  instance_term/5 is declared multifile so
%  that extensions to this package can support alternative forms of
%  class.

:- multifile instance_term/5.

instance_term(Class, Term, Test) :-
	instance_term(Class, Term, Test, _, _).


instance_term(Class, Term, Test, Key, hash_term(Term,Key)) :-
	x_term_class(Class, Term, Test, _),
	!.
instance_term(Class, Term, true, Key, true) :-
	defined_class(Class),
	!,
	functor(Term, Class, 1),
	arg(1, Term, Key).


/*================================================================

			    Inline Methods

You may notice that if the method is quite simple, say a single goal,
then the linkage clause we go through to get to the method clause
is no bigger than the method clause itself.  It would be faster and
smaller if the linkage clause had the actual method inline, and we
never generated the method clause at all.  If there are many such
simple methods, as is typical of get and put methods, then this space
savings may be significant.

The choice of which methods are to be expanded inline like this is a
heuristic one.  In this implementation, I expand inline methods which
are implemented with a single clause containing no more than four
"cheap" builtin Prolog predicate calls and at most one call to a
non-builtin or non-cheap predicate.  Prolog builtins that are compiled
inline (i.e., without having to call a predicate) are considered
cheap.

================================================================*/


%  expand_inline_method(+Self, +Op, +Message, +Body, +Class, -Clauses,
%		+Clauses0)
%  Clauses is a list of the clauses needed to implement an inline
%  method.  Since inline methods are implemented entirely in 
%  linkage clauses, which will be handled elsewhere, the only thing to
%  do here is to generate a bookkeeping clause to tell us that Message
%  is an inline method.

expand_inline_method(Self, Op, Message, Body, Class, Clauses, Clauses0) :-
	functor(Message, Name, Arity),
	Clause = inline_method(Name,Arity,Class,Self,Op,Message,Body),
	add_bookkeeping_fact(Clause, Clauses, Clauses0),
	effective_source_term_position(Pos),
	assert(inline_method_pos(Name, Arity, Op, Class, Pos)).


%  generate_inline(+Class, +Defclass, +Self, +Op, +Message, +Body,
%		   -Clauses, +Clauses0)
%  Clauses is a list of the (single) inline linkage clause for the
%  Message Op message of Class, where Self is the object receiving the
%  message and Body is the body of the method.  Clauses0 is the tail
%  of Clauses after this clause.  Defclass is the class that actually defined
%  this message.

generate_inline(Class, Defcl, Self, Op, Message, Body0, Clauses, Clauses0) :-
	instance_term(Class, Index, Test),
	message_goal(Message, Index, Self, Op, Head),
	(   Class == Defcl ->
		Context = toplevel
	;   Context = inline
	),
	generate_method_clause(Head, Test, Body0, Class, Index, Self, Op,
			       Message, Context, Clauses, Clauses0).


%  generate_method_clause(+Head, +Test, +Body, +Class, +Index, +Self, +Op,
%			  +Message, +Context, -Clauses, +Clauses0)
%  Clauses is a list of the single method clause Head:-Test,Body1, where Body1
%  is Body with its appropriate method transformations performed, and Index is
%  a compile-time version of Self for indexing purposes.

generate_method_clause(Head, Test, Body0, Class, Index, Self, Op, Message,
		Context, Clauses, Clauses0) :-
	expand_method_body(Body0, Class, Self, Op, Message, Addr, [], Context,
			   Body1),
	handle_address_expressions(Class, Index, 0, Addr, Body2, Body1),
	% conjoin(!, Body2, Body3), strictly superfluous cut (?) [MC] 4.0.0
	Body2 = Body3,
	conjoin(Test, Body3, Body),
	add_locked_clause((Head:-Body), Clauses, Clauses0).



%  inlineable_body(+Body, +Class, +Self, +Op, +Message)
%  inlineable_body(+Body, +Class, +Self, +Op, +Message, +N0, -N, +M0, -M)
%  Body, the body of Op message Message for Class sent to Self, is
%  suitable to be inlined.  N0 is the number of inline (cheap) builtin
%  calls that are allowed, and M0 is the number of non-inline
%  predicates allowed; N and M are the number of inline and non-inline
%  predicates still allowed after Body is analyzed, respectively.

inlineable_body(Body, Class, Self, Op, Message) :-
	inlineable_parameters(N0, M0),
	inlineable_body(Body, Class, Self, Op, Message, N0, _, M0, _).

% the 4 and 1 below are arbitrary
inlineable_parameters(N, M) :-
	user_inlineable_parameters(N, M),
	!.
inlineable_parameters(4, 1).



inlineable_body(Body, _, _, _, _, _, _, _, _) :-
	var(Body),
	!,
	fail.				% don't inline metacalls
inlineable_body((A,B), Class, Self, Op, Message, N0, N, M0, M) :-
	!,
	inlineable_body(A, Class, Self, Op, Message, N0, N1, M0, M1),
	inlineable_body(B, Class, Self, Op, Message, N1, N, M1, M).
inlineable_body((A ; B), Class, Self, Op, Message, N0, N, M0, M) :-
	!,
	inlineable_body(A, Class, Self, Op, Message, N0, N1, M0, M1),
	inlineable_body(B, Class, Self, Op, Message, N1, N, M1, M).
inlineable_body((A->B), Class, Self, Op, Message, N0, N, M0, M) :-
	!,
	inlineable_body(A, Class, Self, Op, Message, N0, N1, M0, M1),
	inlineable_body(B, Class, Self, Op, Message, N1, N, M1, M).
inlineable_body(\+(A), Class, Self, Op, Message, N0, N, M0, M) :-
	!,
	inlineable_body(A, Class, Self, Op, Message, N0, N, M0, M).
inlineable_body(_:A, Class, Self, Op, Message, N0, N, M0, M) :-
	!,
	inlineable_body(A, Class, Self, Op, Message, N0, N, M0, M).
inlineable_body(Goal, Class, Self, Op, Message, N0, N, M0, M) :-
	(   inlineable_goal(Goal, Class, Self),
	    N0 > 0 ->
		N is N0-1,
		M = M0
	;   expand_method_goal(Goal, Class, Self, Op, Message, _, [], inline,
			       Goal1),
	    inlineable_body(Goal1, Class, Self, Op, Message, N0, N, M0, M) ->
		true
	;   % we know N0 >= 0
	    M0 > 0,
	    M is M0-1,
	    N = N0
	).

%  inlineable_goal(+Goal, +Class, +Self)
%  Goal is a "cheap" inline Prolog builtin predicate, or will be after
%  it is expanded as part of a method body.  Class and Self are the
%  class being defined and the recipient of the message being analyzed.

inlineable_goal(assign(_,_), _, _).
inlineable_goal(is(_,_), _, _).
inlineable_goal(=(_,_), _, _).
inlineable_goal(=:=(_,_), _, _).
inlineable_goal(=\=(_,_), _, _).
inlineable_goal(<(_,_), _, _).
inlineable_goal(=<(_,_), _, _).
inlineable_goal(>=(_,_), _, _).
inlineable_goal(>(_,_), _, _).
inlineable_goal(true, _, _).
inlineable_goal(fail, _, _).
inlineable_goal(compare(_,_,_), _, _).
inlineable_goal(==(_,_), _, _).
inlineable_goal(\==(_,_), _, _).
inlineable_goal(@<(_,_), _, _).
inlineable_goal(@=<(_,_), _, _).
inlineable_goal(@>=(_,_), _, _).
inlineable_goal(@>(_,_), _, _).
inlineable_goal(atom(_), _, _).
inlineable_goal(atomic(_), _, _).
inlineable_goal(callable(_), _, _).
inlineable_goal(compound(_), _, _).
inlineable_goal(db_reference(_), _, _).
inlineable_goal(float(_), _, _).
inlineable_goal(ground(_), _, _).
inlineable_goal(integer(_), _, _).
inlineable_goal(nonvar(_), _, _).
inlineable_goal(number(_), _, _).
inlineable_goal(simple(_), _, _).
inlineable_goal(var(_), _, _).


/*================================================================

		       Expanding Method Bodies

================================================================*/

%  expand_method_body(+Body0, +Class, +Self, +Op, +Message, -Addr, +Addr0,
%		+Context, -Body)
%  Body is Body0 expanded as appropriate for a method for Op message
%  Message for class Class.  Self can be used in Body as the object
%  receiving the message.  Addr is a list of Offset-Address pairs, followed by
%  Addr0.  In each pair, Offset is bound to an offset from the start
%  address of the Class part of Self.  Each Address is either a term
%  expression(E) where E is a variable to (later) be bound to an
%  expression that evaluates to the address of the specified datum; or
%  Address is bound to a term sum(B,O) where B and O are (later) to be
%  bound to the base address and offset of the datum to be specified.
%  
%  Context is either 'ordinary', 'toplevel', or 'inline', indicating whether
%  inline messages should be open coded.  When an inline message sends another
%  inline message to the same object, the expansion of the sending message can
%  open code the sent message, for greater efficiency.  'Ordinary' means it
%  should not be open coded.  'Toplevel' means it should be open coded.  In
%  both of these cases, we should report any attempt to access a private slot
%  of a superclass.  'Inline' means it should be open coded, and we should not
%  report such a protection violation, because the code we're expanding is
%  itself unfolded code for another method or is an inherited method; any
%  problems trying to access an inherited private slot would have been
%  reported when compiling that method.

expand_method_body(Goal, _, _, _, _, Addr, Addr, _, call(Goal)) :-
	var(Goal),
	!.
expand_method_body((X,Y), Class, Self, Op, Msg, Addr, Addr0, Ctx, (X1,Y1)) :-
	!,
	expand_method_body(X, Class, Self, Op, Msg, Addr, Addr1, Ctx, X1),
	expand_method_body(Y, Class, Self, Op, Msg, Addr1, Addr0, Ctx, Y1).
expand_method_body((X;Y), Class, Self, Op, Msg, Addr, Addr0, Ctx, (X1;Y1)) :-
	!,
	expand_method_body(X, Class, Self, Op, Msg, Addr, Addr1, Ctx, X1),
	expand_method_body(Y, Class, Self, Op, Msg, Addr1, Addr0, Ctx, Y1).
expand_method_body((X->Y), Class, Self, Op, Msg, Addr, Addr0, Ctx, (X1->Y1)) :-
	!,
	expand_method_body(X, Class, Self, Op, Msg, Addr, Addr1, Ctx, X1),
	expand_method_body(Y, Class, Self, Op, Msg, Addr1, Addr0, Ctx, Y1).
expand_method_body(\+(X), Class, Self, Op, Msg, Addr, Addr0, Ctx, \+(X1)) :-
	!,
	expand_method_body(X, Class, Self, Op, Msg, Addr, Addr0, Ctx, X1).
expand_method_body(Mod:X, Class, Self, Op, Msg, Addr, Addr0, Ctx, Mod:X1) :-
	!,
	expand_method_body(X, Class, Self, Op, Msg, Addr, Addr0, Ctx, X1).
expand_method_body(Goal, Class, Self, Op, Msg, Addr, Addr0, Ctx, Goal1) :-
	expand_method_goal(Goal, Class, Self, Op, Msg, Addr, Addr0, Ctx,
			   Goal1),
	!.
expand_method_body(Goal, _, _, _, _, Addr, Addr, _, Goal).


%  expand_method_goal(+Goal0, +Class, +Self, +Op, +Message, -Addr, +Addr0,
%		+Context, -Goal)
%  Goal is Goal0 expanded as appropriate for a method for Op message
%  Message for Class.  Self can be used in Goal as the object
%  receiving the message.
%  indicating whether or not inline messages should be open coded.
%  When an inline message sends another inline message, the expansion
%  of the sending message can open code the sent message, for greater
%  efficiency.  Addr is a list of Offset-Address pairs, followed by
%  Addr0.  In each pair, Offset is bound to an offset from the start
%  address of the Class part of Self.  Each Address is either a term
%  expression(E) where E is a variable to (later) be bound to an
%  expression that evaluates to the address of the specified datum; or
%  Address is bound to a term sum(B,O) where B and O are (later) to be
%  bound to the base address and offset of the datum to be specified.

:- multifile expand_method_goal/9.

expand_method_goal(fetch_slot(Slot,Value), Class, Self, Op, Msg, Addr, Addr0,
		   Ctxt, Accessor) :-
	!,
	accessor(Class, Slot, Self, Op, Msg, Addr, Addr0, Value, fetch_slot,
		 Ctxt, Accessor, _).
expand_method_goal(store_slot(Slot,Value), Class, Self, Op, Msg, Addr, Addr0,
		   Ctxt, Setter) :-
	!,
	accessor(Class, Slot, Self, Op, Msg, Addr, Addr0, Value, store_slot,
		 Ctxt, _, Setter).
expand_method_goal(Goal0, Class, Self, _, _, Addr, Addr0, Context, Goal) :-
	functor(Goal0, Goal_op, 2),
	message_operator(Goal_op),
	arg(1, Goal0, Obj),
	arg(2, Goal0, Goal_msg),
	nonvar(Goal_msg),
	!,
	(   Obj == Self,		% sending to self, and
	    Context \== ordinary,	% we can expand inline Self messages
	    functor(Goal_msg, Name, Arity),
	    unique_inheritance(Class, Name, Arity, Goal_op, 0, _, Defclass),
	    x_inline_method(Name, Arity, Defclass, Self, Goal_op, Goal_msg,
			    Body),
	    inline_method_pos(Name, Arity, Goal_op, Defclass, Pos) ->

/**
This is a nice little hack: if we find an inline message sending an
inline message to Self, we expand one inside the other, saving another
message send.  The problem is that we really need to determine which
messages are inline before generating any of them.  For now, we're not
doing that.  This means that inherited inline messages called from a
new method are handled nicely, but two inline messages defined by the
same class, where one calls the other, are not always handled
properly.  Since we use setof in processing, the situation is that if
an inline message calls an alphabetically earlier inline message, it
will be open coded, if it's alphabetically later, it won't.
**/
		with_effective_position(
		    expand_method_body(Body, Class, Self, Goal_op, Goal_msg, 
				       Addr, Addr0, inline, Goal),
			Pos)
	;   Obj == super ->
		expand_send_right_super(Class, Self, Goal_op, Goal_msg, Addr,
					Addr0, inline, Goal)
	;   nonvar(Obj),
	    Obj = super(Super) ->
		expand_send_super(Class, Super, Self, Goal_op, Goal_msg, Addr,
				  Addr0, inline, Goal)
	;   Addr = Addr0,
	    expand_message_goal(Goal_msg, Obj, Goal_op, Goal)
	).
expand_method_goal(Goal0, _, _, _, _, Addr, Addr, _, Goal) :-
	expand_clause_goal(Goal0, Goal),
	!.


%  expand_send_right_super(+Class, +Self, +Op, +Msg, -Addr, +Addr0, +Context,
%			   -Goal)
%  Goal is the appropriate expansion of Self Op Msg if the class of Self
%  is the unique ancestor of Class from which it inherits this message.
%  This is used to expand send-super goals.  When an inline message
%  sends another inline message, the expansion of the sending message can open
%  code the sent message, for greater efficiency.

expand_send_right_super(Class, Self, Op, Msg, Addr, Addr0, Context,
			Goal) :-
	functor(Msg, Name, Arity),
	(   setof(Super, inherit_from(Class,Name,Arity,Op,0,_,Super),
		  Supers) ->
		(   Supers = [Super] ->
			expand_send_super(Class, Super, Self, Op, Msg,
					  Addr, Addr0, Context, Goal)
		;   Supers = [Super1,Super2|_] -> % too many superclasses
			Goal0 =.. [Op,super,Msg],
			identified_message(warning,
				      ambiguous_send_super(Goal0,Class,
							   Super1,Super2)),
			fail
		)
	;   % no superclasses
	    Goal0 =.. [Op,super,Msg],
	    identified_message(warning, orphan_send_super(Goal0,Class)),
	    fail
	).


%  expand_send_super(+Class, +Super, +Self, +Op, +Msg, -Addr, +Addr0,
%		     +Context, -Goal)
%  Goal is the appropriate expansion of Self Op Msg if the class of Self
%  is Super.  This is used to expand send-super goals.  Super must be an
%  ancestor class of Class.  When
%  an inline message sends another inline message, the expansion of the
%  sending message can open code the sent message, for greater
%  efficiency.

expand_send_super(Class, Super, Self, Op, Msg, Addr, Addr0, Context, Goal) :-
	(   class_ancestor(Class, Super) ->
		true
	;   Goal1 =.. [Op,super(Super),Msg],
	    identified_message(warning,
			  domain_error(Goal1,1,ancestor_of(Class),Super
				       /*,in_method(Class,Self,Op,Msg)*/)),
	    fail
	),
	functor(Msg, Name, Arity),
	unique_inheritance(Super, Name, Arity, Op, 0, Offset, Defclass),
	(   Context \== ordinary,	% we can expand inline Self messages
	    x_inline_method(Name, Arity, Defclass, Self, Op, Msg, Body),
	    inline_method_pos(Name, Arity, Op, Defclass, Pos) ->	
		% Again, the hack to expand inline methods inline
		    with_effective_position(
			expand_method_body(Body, Class, Self, Op, Msg,
					   Addr, Addr0, inline, Goal),
			Pos)
	;   Addr = Addr0,
	    method_goal(Defclass, Msg, Self, Op, Offset, Goal)
	).

%  accessor(+Class, +Slot, +Self, +Op, +Msg, -Addr, +Addr0, +-Value, +Pred,
%	    +Context, -Accessor, -Setter)

accessor(Class, Slot, Self, Op, Msg, Addr, Addr0, Value, Pred, Ctxt,
		Accessor, Setter) :-
	(   defined_class(Class),
	    get_slot(Class, Slot, Realclass, Type, Attribs, 0, Offset) ->
		(   Class \== Realclass,
		    Ctxt \== inline,
		    % This is an inherited message, and not a recursively
		    % unfolded message expansion
		    (   member(visibility(Vis), Attribs) -> true
		    ;	default_slot_attrib(visibility(Vis))
		    ),
		    Vis == (private) ->
			Goal =.. [Pred,Slot,Value],
			IdMsg = permission_error(Goal,Pred,private_slot,Slot,
					     in_method(Class,Self,Op,Msg)),
		        Error = permission(Pred,private_slot,in_method(Class,Self,Op,Msg)),
			identified_message(warning, IdMsg),
			% Only the right one of these will be used
			Accessor = types:illarg(Error,Goal,1,Slot),
			Setter = Accessor
		;   type_accessor(Type, Slot, Offset, Addr, Addr0, Value,
				  Accessor, Setter)
		)
	;   x_term_class(Class, Term, _, Essence),
	    member(Slot:_=Val, Essence) ->	
		Addr = [Term|Addr0],
		Accessor = (Val=Value),
		Setter = error
	;   Goal =.. [Pred,Slot,Value],
	    (	var(Slot) ->
		    IdMsg = instantiation_error(Goal,1),
		    Error = var
	    ;	IdMsg = domain_error(Goal,1,valid_class_slot,Slot
				     /*,in_method(Class,Self,Op,Msg)*/),
		Error = domain(term,valid_class_slot)
	    ),
	    identified_message(warning, IdMsg),
	    % Only the right one of these will be used
	    Accessor = types:illarg(Error,Goal,1,Slot),
	    Setter = Accessor
	).

%  type_accessor(+Type, +Slotname, +Offset, -Addr, +Addr0, +-Value, -Get, -Put)
%  Get and Put are goals which will bind Value to the value of a slot,
%  or store Value as the new value of that slot.  Type is the type of
%  the slot, and Slotname is its name.  Addr is a list of
%  Offset-Address pairs, followed by Addr0.  In each pair, Offset is
%  bound to an offset from the start address of the Class part of
%  Self.  Each Address is either a term expression(E) where E is a
%  variable to (later) be bound to an expression that evaluates to the
%  address of the specified datum; or Address is bound to a term
%  sum(B,O) where B and O are (later) to be bound to the base address
%  and offset of the datum to be specified.
%
%  This predicate is made multifile so that other sorts of slot may
%  later be supported by this package.

:- multifile type_accessor/8.

type_accessor(term_object(_,Term,Slots), Slot, Offset, Addr, Addr0, Term,
		Get, Put) :-
	!,
	term_accessors(Slots, Slot, Offset, Addr, Addr0, Get, Put).
type_accessor(Type, Slot, Offset, [Offset-Address|Addr], Addr, Value, Get, Put) :-
	single_accessor(Type, Slot, Address, Value, Get, Put),
	!.


%  single_accessor(+Type, +Slotname, -Address, +Value, -Get, -Put)
%  Get and Put are goals which will bind Value to the value of a slot,
%  or store Value as the new value of that slot.  Type is the type of
%  the slot, and Slotname is its name.  Address is a term indicating
%  how the address of the slot should be specified.  expression(A)
%  means that A should be bound to an arithmetic expression which
%  evaluates to the address of the slot; sum(Addr,Offset) means that
%  Addr and Offset are integers whose sum is the address of the slot.
%  This predicate is to be used for slots that require only one memory
%  access to get or put the slot's value (the usual case).
%
%  This predicate is made multifile so that other sorts of slot may
%  later be supported by this package.

:- multifile single_accessor/6.

single_accessor(object_ptr(Class), Slot, sum(Address,Offset), Value,
		Get, Put) :-
	!,
	Get = objects:get_object(Address, Offset, Value),
	(   debug_mode ->
		Put = objects:put_object_debug(Value,Address,Offset,Slot,
					       Class)
	;   Put = objects:put_object(Value,Address,Offset)
	).
single_accessor(object_ptr, _, sum(Address,Offset), Value, Get, Put) :-
	!,
	Get = objects:get_object(Address, Offset, Value),
	Put = objects:put_object(Value,Address,Offset).
single_accessor(struct_ptr(Type), Slot, Address, Value, Get, Put) :-
	!,
	single_accessor(address, Slot, Address, Contents, Get, Put),
	Value =.. [Type,Contents].
single_accessor(term, Slot, sum(Addressvar,_), Value, Access, (Clean,Add)) :-
	!,
	term_slot_maintenance(Slot, Addressvar, Value, Access, Add, Clean).
single_accessor(Type, _, sum(Address,Offset), Value, structs:Get, structs:Put) :-
	primitive_type_accessor_names(Type, GetF, PutF), !,
	Get =.. [GetF,Address,Offset,Value],
	Put =.. [PutF,Address,Offset,Value].


%  copied from str_decl.pl

primitive_type_accessor_names(integer,          get_integer,     put_integer    ).
primitive_type_accessor_names(integer_64,       get_integer_64,  put_integer_64 ).
primitive_type_accessor_names(integer_32,       get_integer_32,  put_integer_32 ).
primitive_type_accessor_names(integer_16,       get_integer_16,  put_integer_16 ).
primitive_type_accessor_names(integer_8,        get_integer_8,   put_integer_8  ).
primitive_type_accessor_names(unsigned,         get_unsigned,    put_unsigned   ).
primitive_type_accessor_names(unsigned_64,      get_unsigned_64, put_unsigned_64).
primitive_type_accessor_names(unsigned_32,      get_unsigned_32, put_unsigned_32).
primitive_type_accessor_names(unsigned_16,      get_unsigned_16, put_unsigned_16).
primitive_type_accessor_names(unsigned_8,       get_unsigned_8,  put_unsigned_8 ).
primitive_type_accessor_names(float,            get_float,       put_float      ).
primitive_type_accessor_names(float_32,         get_float_32,    put_float_32   ).
primitive_type_accessor_names(address,          get_integer,     put_integer    ).
primitive_type_accessor_names(atom,             get_atom,        put_atom       ).
primitive_type_accessor_names(string,           get_string,      put_string     ).


%  term_slot_maintenance(+Slot, +Addressvar, +Value, -Access, -Add, -Clean)
%  Access is the goal to call to bind Value to the value of the term slot
%  Slot of the object whose address is Addressvar.  Add is the goal to add
%  a new value to the slot, and Clean is the goal to clean out the old
%  value.  Typically, you must call Clean before calling Add.

term_slot_maintenance(Slot, Addressvar, Value, class_term_slots:Accessor,
		      assert(class_term_slots:Accessor),
		      retractall(class_term_slots:Dummy)):-
	slot_name(Slot, Name),
	Accessor =.. [Name,Addressvar,Value],
	Dummy =.. [Name,Addressvar,_].


%  term_accessors(+Slots, +Slot, +Base, -Addr, +Addr0, -Get, -Put)
%  Get and Put are goals to get and put the essence of a term class
%  whose essence has been distilled to Slots, a list of
%  term_slot(Name,Var,Type,Offset) terms.  Get and Put should each be a
%  conjunction of the goals necessary to get and put each Var, of type
%  Type, at Offset bytes from the beginning of the instance.  The
%  catch is that Address must be a single address term as returned by
%  accessor/6.  Address0 is the address term we're using so far.  Slot
%  is the name of the slot we're creating accessors for, and N is the
%  number of essence slots already generated for this real slot.

term_accessors([], _, _, Addr, Addr, true, true).
term_accessors([term_slot(Name,Var,Type,Offset)|Slots], Slot, Base,
		 Addr, Addr0, (Get0,Get), (Put0,Put)) :-
	subslot(Slot, Name, Subslot),
	Offset1 is Base + Offset,
	type_accessor(Type, Subslot, Offset1, Addr, Addr1, Var, Get0, Put0),
	term_accessors(Slots, Slot, Base, Addr1, Addr0, Get, Put).



subslot(Slot, Part, Subslot) :-
	(   atom(Slot) ->
		Subslot = [Slot,Part]
	;   append(Slot, [Part], Subslot)
	).

%  slot_name(+Slot, -Name)
%  Name is an atom describing Slot, which is a slot spec.
slot_name(Slot, Name) :-
	(   atom(Slot) ->
		Name = Slot
	;   Slot = [First|Rest],
	    (	Rest == [] ->
		    Name = Slot
	    ;   (   foreach(I,Rest),
		    fromto(First,Atom1,Atom3,Name)
		do  atom_concat(Atom1,'#',Atom2),
		    atom_concat(Atom2,I,Atom3)
		)
	    )
	).

%  get_slot(+Class, +Slot, -Realclass, -Type, -Attribs, +Offset0, -Offset)
%  Slot is a slot of type Type with attributes Attribs in instances of class
%  Class.  Realclass is the class that actually defines that slot (the class
%  it is inherited from).  Offset is the byte offset of slot Slot in Class,
%  plus Offset0.

get_slot(Class, Slot, Realclass, Type, Attribs, Offset0, Offset) :-
	(   atom(Slot) ->
		MajorSlot = Slot,
		Rest = []
	;   nonvar(Slot),
	    Slot = [MajorSlot|Rest]
	),
	get_slot1(Class, MajorSlot, Realclass, Type, Attribs,
		  Offset0, Offset, Rest).

%  get_slot1(+Class, +Slot, -Realclass, -Type, -Attribs, +Offset0, -Offset,
%	     +Rest)

get_slot1(Class, Slot, Realclass, Type, Attribs, Offset0, Offset, Rest) :-
	(   x_direct_slot(Class, Slot, Type0, Offset1, Attribs) ->
		Offset2 is Offset0+Offset1,
		get_subslot(Rest, Type0, Type, Offset2, Offset),
		Realclass = Class
	;   x_superclass(Class, Parent, Offset1),
	    Offset2 is Offset0+Offset1,
	    get_slot1(Parent, Slot, Realclass, Type, Attribs,
		      Offset2, Offset, Rest) ->
		true
	).


%  get_subslot(+Subslots, +Type0, -Type, +Offset0, -Offset)
%  Subslots is a list of subslot names, Type0 is the type of the slot which 
%  has these subslots, and Offset1 is the offset from the beginning of the
%  object at which the subslots begin.  Type is the type of the named subslot,
%  and Offset is Offset0 + the offset of this subslot.

get_subslot([], Type, Type, Offset, Offset).
get_subslot([Part|Parts], Type0, Type, Offset0, Offset) :-
	(   Type0 = term_object(_,_,Subslots) ->
		(   member(term_slot(Part,_,Type1,Offset1), Subslots) ->
			true
		;   illarg(system(objects), 0, 0)
			% XXX TODO fix this exception
		)
	;   illarg(system(objects), 0, 0)
		% XXX TODO fix this exception
	),
	Offset2 is Offset0 + Offset1,
	get_subslot(Parts, Type1, Type, Offset2, Offset).



%  expand_message_goal(+Msg, +Self, +Op, -Goal)
%  Goal is the goal to compile when expanding a message invocation
%  sending message Msg, a message of type Op, to Self.

expand_message_goal(Msg, Obj, Op, Goal) :-
	(   debug_mode ->
		(   nonvar(Msg) ->
			functor(Msg, Name, Arity),
			Goal = (objects:ensure_can_send(Obj,Op,Name,Arity),
				Goal0)
		;   Goal =.. [Op, Obj, Msg]
		)
	;   Goal = Goal0
	),
	(   callable(Msg) ->
		message_goal(Msg, Obj, Obj, Op, Goal0)
	;   var(Msg) ->
		meta_message_goal(Msg, Obj, Op, Goal0)
	;   % atomic(Msg), \+ atom(Msg) ->
	        Goal0 =.. [Op,Obj,Msg]
	).

%  message_goal(+Msg, +Obj1, +Obj2, +Op, -Goal)
%  Goal is the goal or clause head that implements sending Msg, a
%  message of type Op, to Obj2.  Obj1 is also the recipient of the
%  message, but is just used for indexing.

message_goal(Msg, Obj1, Obj2, Op, Goal) :-
	nonvar(Op),
	message_operator(Op),
	Msg =.. [Msgname|Args],
	atom_concat(Op, Msgname, Predname),
	Goal0 =.. [Predname,Obj1,Obj2|Args],
	Goal = class_message_clauses:Goal0.


%  meta_message_goal(+Msg, +Obj, +Op, -Goal)
%  Goal is the goal or clause head that implements sending Msg, a
%  message of type Op, to Obj2.  Obj1 is also the recipient of the
%  message, but is just used for indexing.  This differs from
%  message_goal/5 in that Msg can be a variable.  The generated goal
%  will not be as efficient, but will be correct.  Op must still be
%  bound.

meta_message_goal(Msg, Obj, Op, Goal) :-
	nonvar(Op),
	message_operator(Op),
	Goal0 =.. [Op,Msg,Obj],
	Goal = class_message_clauses:Goal0.


%  define_message(+Name, +Arity, +Op, -Clauses, +Clauses0)
%  Clauses is a list of clauses needed to define Name/Arity as a new
%  Op message.
%  Oct 93: added the cut in the generated clause because duplicates
%  can occur due to files with the same method name being separately
%  compiled.  This led to spurious backtracking. - DLB

define_message(Name, Arity, Op, Clauses, Clauses0) :-
	functor(Msg, Name, Arity),
	meta_message_goal(Msg, Obj, Op, Head),
	message_goal(Msg, Obj, Obj, Op, Body),
	add_locked_clause((Head:-!,Body), Clauses, Clauses0).


% [PM] 4.2.1 SPIDER will complain that message_operator/1 ought to use cut. I think the complaint is valid but I dare not modify the source.
:- multifile message_operator/1.

%  message_operator(+Op)
%  Op is a message-sending operator.  This
%  predicate is multifile, so that addon packages may add other
%  message ops.  Of course, Op doesn't need to be a real operator.
message_operator(<-).
message_operator(>>).
message_operator(<<).

/*==============================================================

		       Expanding Other Clauses

The following code is used to optimize messages sent from ordinary Prolog
clauses.

===============================================================*/


%  expand_clause_body(+Goal0, -Goal)
%  Goal is the expansion of Goal0, after generation more efficient
%  code for class package facilities.

expand_clause_body(Goal, Goal) :-
	var(Goal),
	!.
expand_clause_body((X,Y), (X1,Y1)) :-
	!,
	expand_clause_body(X, X1),
	expand_clause_body(Y, Y1).
expand_clause_body((X ; Y), (X1 ; Y1)) :-
	!,
	expand_clause_body(X, X1),
	expand_clause_body(Y, Y1).
expand_clause_body((X->Y), (X1->Y1)) :-
	!,
	expand_clause_body(X, X1),
	expand_clause_body(Y, Y1).
expand_clause_body(\+(X), \+(X1)) :-
	!,
	expand_clause_body(X, X1).
expand_clause_body(Goal0, Goal) :-
	expand_clause_goal(Goal0, Goal),
	!.
expand_clause_body(Goal, Goal).


%  expand_clause_goal(+Goal0, -Goal)
%  Goal is the expansion of goal that can occur either in or out of a
%  method body.

expand_clause_goal(create(Descriptor,Obj),
		   objects:fast_create(Class,Msg,Header,Size,Obj)) :-
	var(Obj),
	nonvar(Descriptor),
	functor(Descriptor, Class, Arity),
	x_class_size(Class, Base, Header),
	!,
	Size is Base+Header,
	(   x_message(Class, create, Arity, <-) ->
		Descriptor =.. [_|Args],
		Msg =.. [create|Args]
	;   /*
                The following warning used to occur if a method tried
	        to create an object of its own class, e.g. a method for
	        class list tried to create a list, IF the appropriate
	        create method had not yet been processed.  Since the
	        methods are processed in alphabetic order (bizarre!)
	        this meant that you would get the warning if either 
	        there is no such create method (in which case a warning
	        is good) or if the name of the method calling create 
	        was alphabetically before "create".  Since the latter
	        case is bogus, I am commenting out the warning.  However,
		I'm leaving this as a fail, so that fast_create does 
	        not get used in this case, because the comment on 
	        fast_create says that the create method must be known.

                identified_message(warning,
			  existence_error(create(Descriptor,Obj),1,
					    initialization_method,
					    create/Arity,0)),
	    */
	    fail
	).
expand_clause_goal(Goal0, Goal) :-
	functor(Goal0, Op, 2),
	message_operator(Op),
	!,
	arg(1, Goal0, Ob),
	arg(2, Goal0, Msg),
	expand_message_goal(Msg, Ob, Op, Goal).





/*================================================================

		      Generating Linkage Clauses

The strategy we use in generating linkage clauses is to collect a list
of all the messages recognized by the specified class.  We know we'll
need one linkage clause for each message, but what kind of linkage
clause depends on the message itself.

================================================================*/



%  generate_linkages(+Class, -Clauses, +Clauses0)
%  Clauses is a list of all the linkage clauses for class Class,
%  followed by Clauses0.

generate_linkages(Class, Clauses, Clauses0) :-
	(   setof(msg(Name,Arity,Op),
		  x_message(Class,Name,Arity,Op), Msgs) ->
		generate_msg_linkages(Msgs, Class, Clauses, Clauses0)
	;   Clauses = Clauses0
	).

generate_msg_linkages([], _, Clauses, Clauses).
generate_msg_linkages([msg(Name,Arity,Op)|Msgs], Class, Clauses, Clauses0) :-
	(   generate_linkage_hook(Name, Arity, Op, Class, Clauses, Clauses0) ->
		true
	;   unique_inheritance(Class, Name, Arity, Op, 0, Offset, Defclass),
	    (   x_inline_method(Name,Arity,Defclass,Self,Op,Message,Body) ->
		    inline_method_pos(Name, Arity, Op, Defclass, Pos),
		    with_effective_position(
			generate_inline(Class, Defclass, Self, Op, Message,
					Body, Clauses, Clauses1),
			Pos)
	    ;   Op == (<-),
	        is_instance_method(Name,Arity,Defclass,Predname) ->
		    generate_instance_linkage(Name, Arity, Class, Predname,
					     Clauses, Clauses1)
	    ;   generate_ordinary_linkage(Name, Arity, Class, Defclass, Op,
					  Offset, Clauses, Clauses1)
	    )
	),
	generate_msg_linkages(Msgs, Class, Clauses1, Clauses0).


%  unique_inheritance(+Class, +Name, +Arity, +Op, +Offset0, -Offset,
%		      -Defclass)
%  Defclass is the class that defines message Name/Arity of type Op,
%  as seen from class Class.  Offset is the byte offset of the slots
%  of Defclass from the start of class Class, plus Offset0.  Defclass
%  will be Class itself, if Class defines this message.
%
%  This package insists that Defclass be unique.  If there are
%  two or more parents that define or inherit this message, then a
%  warning message is printed.  This can be avoided by making an
%  :- 'inherit' declaration.

unique_inheritance(Class, Name, Arity, Op, Offset0, Offset, Defclass) :-
	setof(O-D, method_defined_by(Class,Name,Arity,Op,Offset0,O,D), List),
	(   List = [Offset-Defclass] ->
		true
	;   List = [Offset-Defclass,_-Otherclass|_] ->
	    identified_message(warning,
			  ambiguous_inheritance(Class,Defclass,Otherclass,
						Name,Arity,Op)),
	    assert(dyn_inherit_from(Class,Name,Arity,Op,Defclass))
						% make sure we get no other
						% complaints about this clash
	).

%  inherit_from(+Class, +Name, +Arity, +Op, +Offset0, -Offset, -Defclass)
%  Defclass is a class from which class Class inherits message Name/Arity
%  of type Op.  Offset is the byte offset of the slots of Defclass from
%  the start of class Class, plus Offset0.  Defclass will not be Class
%  itself, even if Class defines this message.

inherit_from(Class, Name, Arity, Op, Offset0, Offset, Defclass) :-
	x_superclass(Class, Parent, Offset1),
	Offset2 is Offset0+Offset1,
	method_defined_by(Parent, Name, Arity, Op, Offset2, Offset, Defclass).


%  method_defined_by(+Class, +Name, +Arity, +Op, +Offset0, *Offset,
%		     *Defclass)
%  Defclass is a class that defines message Name/Arity of type Op,
%  as seen from class Class.  Offset is the byte offset of the slots
%  of Defclass from the start of class Class, plus Offset0.  Defclass
%  will be Class itself, if Class defines this message.

method_defined_by(Class, Name, Arity, Op, Offset, Offset, Class) :-
	x_direct_message(Class, Name, Arity, Op),
	!.					% this cut implements shadowing
method_defined_by(Class, Name, Arity, Op, Offset0, Offset, Defclass) :-
	x_inherit_from(Class, Name, Arity, Op, Parent),
	!,
	x_superclass(Class,Parent,Offset1),
	Offset2 is Offset1+Offset0,
	method_defined_by(Parent, Name, Arity, Op, Offset2, Offset,
			    Defclass),
	!.				% x_superclass not determinate
method_defined_by(Class, Name, Arity, Op, Offset0, Offset, Defclass) :-
	x_superclass(Class, Parent, Offset1),
	\+ x_uninherit_from(Class, Name, Arity, Op, Parent),
	Offset2 is Offset0+Offset1,
	method_defined_by(Parent, Name, Arity, Op, Offset2, Offset, Defclass).


%  declare_inheritance(+Spec, -Clauses, +Clauses0)
%  Record an inheritance specification.  This package warns you when
%  the same message is inherited from two or more parents.  In order
%  to avoid this, you must include an :- inherit directive in the body
%  of the class that redundantly inherits a message.  The form of the
%  directive is
%
%  :- inherit Parent1 <- Name1/Arity1, Parent2 <- Name2/Arity2, ....
%
%  This declares that the class we are defining should inherit message
%  Name1/Arity1 from class Parent1, which must be a parent class (not
%  just an ancestor) of the class we are defining, and must define the
%  Name1/Arity1 message.  Similarly for Parent2, Name2, and Arity2.

declare_inheritance(Spec, Clauses, Clauses0) :-
	(   pending_class(Class) ->
		declare_inheritance1(Spec, Class, Clauses, Clauses0)
	;   illarg(context(not_in_class_defn,0), inherit(Spec), 0)
	).

declare_inheritance1((Spec1,Spec2), Class, Clauses, Clauses0) :-
	!,
	declare_inheritance1(Spec1, Class, Clauses, Clauses1),
	declare_inheritance1(Spec2, Class, Clauses1, Clauses0).
declare_inheritance1(Spec, Class, Clauses, Clauses0) :-
	(   Spec =.. [Op, Parent, Name/Arity],
	    message_operator(Op),
	    atom(Parent),
	    atom(Name),
	    integer(Arity) ->
		declare_inheritance2(Parent, Class, Op, Name, Arity, Spec,
				     Clauses, Clauses0)
	;   identified_message(error, domain_error(inherit(Spec), 1, valid_inherit_spec, Spec))
	).


declare_inheritance2(Parent, Class, Op, Name, Arity, Spec, Clauses,
		     Clauses0) :-
	(   \+ (x_superclass(Class, Parent, _),
		x_message(Parent, Name, Arity, Op)) ->
		identified_message(warning, context_error(:-(inherit(Spec)),
						     not_inherited,
						     inherit_directive))
	;   x_uninherit_from(Class, Name, Arity, Op, Parent) ->
		identified_message(error, consistency_error(:-(uninherit(Spec)),
						       :-(inherit(Spec)),
						       ''))
	;   add_bookkeeping_fact(inherit_from(Class,Name,Arity,Op,
					      Parent),
				 Clauses, Clauses0)
	).



%  suppress_inheritance(+Spec, -Clauses, +Clauses0)
%  Record an "uninheritance" specification.  By default, every method
%  defined by a class's parents is inherited by that class (except
%  create of any arity).  This inheritance may be suppressed on a
%  message by message basis with an 'uninherit' directive:
%
%  :- uninherit Parent1 <- Name1/Arity1, Parent2 <- Name2/Arity2, ....
%
%  This declares that the class we are defining should not inherit message
%  Name1/Arity1 from class Parent1, which must be a parent class (not
%  just an ancestor) of the class we are defining, or else an unbound
%  variable.  If Parent1 is unbound, then Name1/Arity1 is not
%  inherited from any parent.  Similarly for Parent2, Name2, and Arity2.

suppress_inheritance(Spec, Clauses, Clauses0) :-
	(   pending_class(Class) ->
		suppress_inheritance1(Spec, Class, Clauses, Clauses0)
	;   illarg(context(not_in_class_defn,0), uninherit(Spec), 0)
	).

suppress_inheritance1((Spec1,Spec2), Class, Clauses, Clauses0) :-
	!,
	suppress_inheritance1(Spec1, Class, Clauses, Clauses1),
	suppress_inheritance1(Spec2, Class, Clauses1, Clauses0).
suppress_inheritance1(Spec, Class, Clauses, Clauses0) :-
	(   Spec =.. [Op, Parent, Name/Arity],
	    message_operator(Op) ->
		(   nonvar(Parent) ->
			Parent1 = Parent
		;   true			% leave Parent1 unbound
		),
		suppress_inheritance2(Parent, Parent1, Class, Op, Name, Arity,
				      Spec, Clauses, Clauses0)
	;   identified_message(error, domain_error(uninherit(Spec), 1, valid_inherit_spec, Spec))
	).

suppress_inheritance2(Parent, Parent1, Class, Op, Name, Arity, Spec, Clauses,
		      Clauses0) :-
	(   \+ (x_superclass(Class, Parent1, _),
		x_message(Parent1, Name, Arity, Op)) ->
		identified_message(warning, context_error(:-(uninherit(Spec)),
						     not_inherited,
						     uninherit_directive))
	;   x_inherit_from(Class, Name, Arity, Op, Parent) ->
		identified_message(error, consistency_error(:-(inherit(Spec)),
						       :-(uninherit(Spec)),
						       ''))
	;   add_bookkeeping_fact(uninherit_from(Class,Name,Arity,Op,Parent),
				 Clauses, Clauses0)
	).


/*****************************************************************

			   Instance Methods

One unusual feature of this package is the instance method.  An instance
method allows a class to have a different method for a particular
message for each instance.  That is, you can define the method for an
instance at runtime.  This is particularly useful for callbacks.  For
example, you might define a pushbutton class that will be sent a
pressed/0 message when the pushbutton is pressed.  If you make this an
instance method, you can have each individual pushbutton do something
different when the button is pressed.

You may also define a default method for this message, which will be
invoked if no method is defined for this message for this instance.
This is done simply by including some clauses for this method after
declaring it to be an instance method.  If no method is defined for an
instance, and no default method is defined, sending this method to that
instance simply fails.  Note that default methods are static procedures,
and so are quite efficient.  Also, they are not copied over as each
instance is created; instance methods do not make creating new instances
any less time or space efficient, though they do slow down destroying
instances a little.

A method for a particular class is declared to be an instance method
with a declaration within the definition of that class (i.e., between
the ':-class' declaration and the following ':-end_class' or the next
':-class' declaration).  The 'instance_method' declaration looks much
like a dynamic declaration:

++verbatim
	:- instance_method foo/4, bar/3, baz/2.
--verbatim

In the context of a declaration of class C, this declares that the
foo/4, bar/3 and baz/2 methods of each instance of class C are separate
dynamic procedures.  Any method defintions for these messages appearing
later in the definition of class C will be the default methods for each
newly created instance of class C.  That method can be replaced on a
per-instance basis.

Descendant classes of this class will inherit the fact that these
methods are instance_methods, as well as inheriting the default methods.
If a descendant class defines a different method for this message, it
will override (shadow) the default method of the ancestor class, but it
will still be an instance method.  If you wish a descendant class to
have this message be an ordinary class method, you must declare it to be
so:

++verbatim
	:- class_method foo/4, bar/3, baz/2.
--verbatim

Naturally, class methods are not as efficient as static methods, but the
performance penalty is not prohibitive.

****************************************************************/

/*================================================================

				Implementation

Instance methods are implemented a bit differently than ordinary methods.
A instance method fred/2 invoked by
++verbatim
	X <- fred(a,b)
--verbatim
is turned into the call
++verbatim
	class_message_clauses:'<-fred'(X,X,a,b)
--verbatim
by the usual expansion code.  This is just what would happen if fred/2
were not an instance method; it must be the same since we don't know
what the class of X is, so we don't know if fred/2 is an instance
method for that class or not.

We do generate a different linkage clause for instance methods.  For
fred/2 and class cl, we would generate
++verbatim
	class_message_clauses:'<-fred'(cl(Addr),Self,X,Y) :-
		class_instance_method:'<-fred'(Addr,Self,X,Y).
--verbatim
For a descendant class des, we would generate another identical
clause, except that cl would be replaced with des in both places.

Once for each declaration of fred/2 to be an instance method, we also
generate the clauses
++verbatim
	class_instance_method:'<-fred'(_,Self,X,Y) :-
		!,
		class_instance_default:'<-fred'(Self,Self,X,Y).

	class_instance_default:'<-fred'(0_,_,_) :- !, fail.
--verbatim
The latter clause is there just so that class_instance_default:fred/4
is sure to be defined, so there will be no undefined warnings or
errors.  Note that the 0 argument falls where an object must
appear, so this clause will never be reached.

When clauses for fred/2 in class cl are defined, they will be turned
into clauses for class_instance_default:'<-fred'/4 where the first
argument is cl(_).

When a method for fred/2 is defined at runtime for a particular
instance, we asserta a clause for class_instance_method:'<-fred'/4:
++verbatim
	class_instance_method:fred(Address,_,X,Y) :- !, Body
--verbatim
where Body is the processed body of the instance method being
established and Address is the address of the object whose instance
method for foo/2 is Body.  The body is processed to expand message
goals and calls to fetch_slot/2 and store_slot/2.

================================================================*/

%  declare_instance_methods(+Spec, -Clauses, +Clauses0)
%  Clauses is a list of the clauses needed to record the fact that the
%  methods specified by Spec are instance methods, followed by
%  Clauses0.  Spec is a Name/Arity term, or a comma term whose args
%  are instance specs.

declare_instance_methods((Pred1,Pred2), Clauses, Clauses0) :-
	!,
	declare_instance_methods(Pred1, Clauses, Clauses1),
	declare_instance_methods(Pred2, Clauses1, Clauses0).
declare_instance_methods(Name/Arity, Clauses, Clauses0) :-
	pending_class(Class),
	atom_concat(<-, Name, Predname),
	add_bookkeeping_fact(instance_method(Name,Arity,Class,Predname),
			     Clauses, Clauses2),
	length(Args, Arity),
	Head =.. [Predname,_,Self|Args],
	Body =.. [Predname,Self,Self|Args],
	% The cut in the following generated clause is there in case there are
	% multiple instance declarations for the same message, in which case,
	% there will be multiple copies of this clause.  The cut will
	% effectively render the extra clauses unreachable.
	add_locked_dynamic_clause(
		(class_instance_method:Head:-!,class_instance_default:Body),
		Clauses2, Clauses3),
	Dummyhead =.. [Predname,0,_|Args],
	add_locked_clause((class_instance_default:Dummyhead :- !, fail),
		   Clauses3, Clauses0).

%  declare_class_methods(+Spec, -Clauses, +Clauses0)
%  Clauses is a list of the clauses needed to record the fact that the
%  methods specified by Spec are class methods, followed by
%  Clauses0.  Spec is a Name/Arity term, or a comma term whose args
%  are class specs.

declare_class_methods((Pred1,Pred2), Clauses, Clauses0) :-
	!,
	declare_class_methods(Pred1, Clauses, Clauses1),
	declare_class_methods(Pred2, Clauses1, Clauses0).
declare_class_methods(Name/Arity, Clauses, Clauses0) :-
	pending_class(Class),
	add_bookkeeping_fact(class_method(Name,Arity,Class), Clauses,
			     Clauses0).


%  generate_instance_linkage(+Name, +Arity, +Class, +Predname,
%		-Clauses, +Clauses0)
%  Clauses is a linkage clause to link the <- Name/Arity method for
%  class Class to the implementation of that method, followed by
%  Clauses0.  Predname is the name of the predicate that implements
%  this instance method.

generate_instance_linkage(Name, Arity, Class, Predname, Clauses, Clauses1) :-
	instance_term(Class, Object, Test, Key, Goal),
	length(Args, Arity),
	Message =.. [Name|Args],
	message_goal(Message, Object, Obj2, <-, Head),
	Body0 =.. [Predname,Key,Obj2|Args],
	conjoin(Goal, class_instance_method:Body0, Body1),
	% conjoin(!, Body1, Body2), strictly superfluous cut (?) [MC] 4.0.0
	Body1 = Body2,
	conjoin(Test, Body2, Body),
	add_locked_clause((Head:-Body), Clauses, Clauses1).


%  expand_instance_method(+Methods, +Class, +Predname, -Clauses, +Clauses0)
%  Clauses is a list of clauses implementing the instance methods
%  described by Methods for class Class, followed by Clauses0.
%  Methods is a list of method(Message,Self,Body) terms, where Body is
%  the code to implement sending Message to Self.  Predname is the
%  name of the predicate that implements this instance method.  This
%  implementation isn't very sophisticated, since the clauses it
%  generates don't benefit from indexing.

expand_instance_method([], _, _, Clauses, Clauses).
expand_instance_method([method(Msg,Self,Body,Pos)|Methods], Class, Predname,
		Clauses, Clauses0) :-
	Msg =.. [_|Args],
	instance_term(Class, Object, Test),
	Head =.. [Predname,Object,Self|Args],
	with_effective_position(
	    generate_method_clause(class_instance_default:Head, Test, Body,
				   Class, Self, Self, <-, Msg, ordinary,
				   Clauses, Clauses1),
	    Pos),
	expand_instance_method(Methods, Class, Predname, Clauses1, Clauses0).




%  instance_method_cleanup(+Class, +Self, +Cleanup0, -Cleanup)
%  Cleanup is the conjunction of Cleanup0 and whatever code is necessary to
%  cleanup object Self, an instance of Class.

instance_method_cleanup(Class, Self, Cleanup0, Cleanup) :-
	(   setof(Pred/Arity,
		  instance_method_pred(Class, Pred, Arity),
		  Preds) ->
		arg(1, Self, Addressvar),
		instance_methods_retract_goal(Preds, Addressvar, Cleanup0,
					      Cleanup)
	;   Cleanup = Cleanup0
	).


instance_method_pred(Class, Pred, Arity) :-
	x_message(Class, Name, Arity0, (<-)),
	is_instance_method(Name, Arity0, Class, Pred),
	Arity is Arity0 + 2.


instance_methods_retract_goal([], _, Cleanup, Cleanup).
instance_methods_retract_goal([Pred/Arity|Preds], Addressvar,
		Cleanup0, Cleanup) :-
	Goal = objects:remove_instance_method(Pred, Arity, Addressvar),
	conjoin(Goal, Cleanup0, Cleanup1),
	instance_methods_retract_goal(Preds, Addressvar, Cleanup1, Cleanup).


/***************************************************************

			      Miscellany

****************************************************************/


%  add_clause(+Clause, -Clauses, +Clauses0)
%  Clauses is Clause followed by Clauses0.

add_clause(Cl, [Cl|Tail], Tail).


%  add_multifile_clause(+Clause, -Clauses, +Clauses0)
%  Clauses is a multifile and discontiguous declaration for the
%  predicate specified by Clause, followed by Clause itself, followed
%  by Clauses0.

% add_multifile_clause(Cl, [:-(multifile(Spec)),:-(discontiguous(Spec)),Cl|Tail],
% 		Tail) :-
% 	predspec(Cl, Spec).

/*
   Changed this to be the same as add_locked_clause.  classes.pl is
   compiled with -H anyway, so some (most? all?) of these end up
   locked in classes.qof, and due to a Prolog bug it depends on the
   order of files in the qld command line whether the predicate ends
   up locked or not.  I think that we prefer to have all these locked
   all the time anyway. - DB Jan 24, 94
*/
add_multifile_clause(Clause, Clauses, Clauses0) :-
	add_locked_clause(Clause, Clauses, Clauses0).


%  add_locked_clause(+Clause, -Clauses, +Clauses0)
%  Clauses is a multifile, discontiguous, and locked declaration for the
%  predicate specified by Clause, followed by Clause itself, followed
%  by Clauses0.

add_locked_clause(Cl, Tail0, Tail) :-
	predspec(Cl, Spec),
	(   declared_multifile_discontiguous(Spec)
	->  Tail0 = [Cl|Tail]
	;   assertz(declared_multifile_discontiguous(Spec)),
	    Tail0 = [:-(multifile(Spec)),:-(discontiguous(Spec)),Cl|Tail1],
	    (   supports_locking ->
		Tail1 = [:-(locked(Spec))|Tail]
	    ;   Tail1 = Tail
	    )
	).

add_locked_dynamic_clause(Cl, Tail0, Tail) :-
	predspec(Cl, Spec),
	(   declared_multifile_discontiguous(Spec)
	->  Tail0 = [Cl|Tail]
	;   assertz(declared_multifile_discontiguous(Spec)),
	    Tail0 = [:-(multifile(Spec)),:-(discontiguous(Spec)),:-(dynamic(Spec)),Cl|Tail1],
	    (   supports_locking ->
		Tail1 = [:-(locked(Spec))|Tail]
	    ;   Tail1 = Tail
	    )
	).

supports_locking :- fail.	% Not in SP


%  add_bookkeeping_fact(+Clause, -Clauses, +Clauses0)
%  Clauses is Clause followed by Clauses0.  Furthermore, Clause is a
%  bookkeeping fact, so a dynamic version of it must be asserted for
%  processing the rest of the current file.  And while we're at it, we
%  declare the predicate to be multifile and discontiguous.

add_bookkeeping_fact(Cl, Clauses, Clauses0) :-
	(   dyn_version(Cl, Dyn) ->
		assert(Dyn)
	;   true
	),
	add_multifile_clause(obj_decl:Cl, Clauses, Clauses0).


%  conjoin(+Goal1, +Goal2, -Conjunction)
%  Conjunction is the conjunction Goal1 and Goal2.  This takes care to
%  leave out 'true' goals, and to handle 'fail' goals, for the sake of
%  tidyness.

conjoin(true, X, X) :- !.
conjoin(fail, _, fail) :- !.
conjoin(X, true, X) :- !.
conjoin(X, Y, (X,Y)).


%  predspec(+Clausse, -Spec)
%  Spec is a predicate specification for the predicate defined by
%  Clause.

predspec(Mod:Clause, Mod:Spec) :-
	!,
	predspec(Clause, Spec).
predspec((Head:-_), Spec) :-
	!,
	predspec(Head, Spec).
predspec(Head, Name/Arity) :-
	functor(Head, Name, Arity).

%  align(+Offset0, +Size, +Alignment, -Offset, -Next)
%  align_pointer(+Offset0, -Offset, -Next)
%  Offset is the smallest offset for the start of a slot that must be
%  aligned on an Alignment byte boundry and must be at least Offset0.
%  Next is the offset of the next available byte, given that the
%  specified slot occupies Size bytes.  For align_pointer/3, we use
%  pointer size and alignment requirements.

align(Offset0, Size, Alignment, Offset, Next) :-
	Offset is ((Offset0+Alignment-1)//Alignment)*Alignment,
	Next is Offset+Size.

align_pointer(Offset0, Offset, Next) :-
	pointer_alignment(Size, Alignment),
	align(Offset0, Size, Alignment, Offset, Next).


:- mode dyn_version(?,?). % [PM] 4.2.1 (Also) used as generator
:- multifile dyn_version/2.
:- discontiguous dyn_version/2.

%  dyn_version(+Static, -Dynamic)
%  Dynamic is the dynamic version of bookkeeping predicate Static.
%  Fails quietly if Static is not a bookkeeping predicate.  This
%  predicate is declared multifile so that extensions to this package
%  can use add_bookkeeping_fact/3.
dyn_version( class_size(A,B,C),		dyn_class_size(A,B,C)).
dyn_version( class_file(A,B),		dyn_class_file(A,B)).
dyn_version( term_class(A,B,C,D),	dyn_term_class(A,B,C,D)).
dyn_version( subclass(A,B),		dyn_subclass(A,B)).
dyn_version( superclass(A,B,C),		dyn_superclass(A,B,C)).
dyn_version( direct_slot(A,B,C,D,E),	dyn_direct_slot(A,B,C,D,E)).
dyn_version( direct_message(A,B,C,D),	dyn_direct_message(A,B,C,D)).
dyn_version( instance_method(A,B,C,D),	dyn_instance_method(A,B,C,D)).
dyn_version( inherit_from(A,B,C,D,E),	dyn_inherit_from(A,B,C,D,E)).
dyn_version( uninherit_from(A,B,C,D,E),	dyn_uninherit_from(A,B,C,D,E)).
dyn_version( inline_method(A,B,C,D,E,F,G),
					dyn_inline_method(A,B,C,D,E,F,G)).



dyn_fact(F) :-
	dyn_version(_, F).
dyn_fact(debug_mode).
dyn_fact(declared_multifile_discontiguous(_)).

%  cleanup_at_eof
%  get rid of all dynamic versions of bookkeeping facts.
cleanup_at_eof :-
	(   dyn_fact(Fa),
	    retractall(Fa),
	    fail
	;   true
	).

post_compile_checks :-
	retract(forward_def_slot_type(Type,UsedInClass)),
	( defined_class(Type) ->
	    true
	;   identified_message(warning, bad_slot_type(Type,UsedInClass))
	),
	fail.
post_compile_checks.




%  get_current_file(-File)
%  We are currently loading File.

get_current_file(File) :-
	(   prolog_load_context(file, File) -> true
	;   File = user				% hack for debugging
	).


%  source_term_position(-Position)
%  The position of the term we are processing in its source file

source_term_position(Pos) :-
	(   prolog_load_context(term_position, Pos) ->
		true
	;   Pos = '$stream_position'(0,/*BC 0,*/ 0,0,0)
	).

/*================================================================
			    x_ Predicates

These are versions of the bookkeeping predicates that use both the
static and dynamic versions of the predicates.  Note that they are not
determinate. 
================================================================*/

x_class_size(A, B, C) :-
	(   class_size(A, B, C)
	;   dyn_class_size(A, B, C)
	).
x_class_file(A, B) :-
	(   class_file(A, B)
	;   dyn_class_file(A, B)
	).
x_term_class(A, B, C, D) :-
	(   term_class(A, B, C, D)
	;   dyn_term_class(A, B, C, D)
	).
:- public((x_subclass/2)). % [PM] 4.1.3 suppress unreachable warning
x_subclass(A, B) :-				% can backtrack, currently dead code
	(   subclass(A, B)
	;   dyn_subclass(A, B)
	).
x_superclass(A, B, C) :-			% can backtrack
	(   superclass(A, B, C)
	;   dyn_superclass(A, B, C)
	).
x_direct_slot(A, B, C, D, E) :-
	(   direct_slot(A, B, C, D, E)
	;   dyn_direct_slot(A, B, C, D, E)
	).
x_direct_message(A, B, C, D) :-
	(   direct_message(A, B, C, D)
	;   dyn_direct_message(A, B, C, D)
	).
x_instance_method(A, B, C, D) :-
	(   instance_method(A, B, C, D)
	;   dyn_instance_method(A, B, C, D)
	).
x_class_method(A, B, C) :-
	(   class_method(A, B, C)
	;   dyn_class_method(A, B, C)
	).
x_inherit_from(A, B, C, D, E) :-
	(   inherit_from(A, B, C, D, E)
	;   dyn_inherit_from(A, B, C, D, E)
	).
x_uninherit_from(A, B, C, D, E) :-
	(   uninherit_from(A, B, C, D, E)
	;   dyn_uninherit_from(A, B, C, D, E)
	).
x_inline_method(A, B, C, D, E, F, G) :-
	(   inline_method(A, B, C, D, E, F, G)
	;   dyn_inline_method(A, B, C, D, E, F, G)
	).


%  defined_class(+Class)
%  Class is a known "memory" class.  I.e., it is a standard class, represented
%  by a C-style structure with slots, not a term class or any other kind of
%  class that isn't kept as a pointer to a memory address.

:- multifile defined_class/1.

defined_class(Class) :-
	x_class_size(Class, _, _),
	!.
defined_class(Class) :-
	pending_class(Class),
	pending_ordinary_class,
	!.


forward_defined_slot_type(SlotType, UsedInClass) :-
	retractall(forward_def_slot_type(SlotType, UsedInClass)),
	assert(forward_def_slot_type(SlotType, UsedInClass)).


%  class_ancestor(+Class, *Ancestor)
%  Ancestor is Class or an ancestor class of Class.

class_ancestor(Class, Class).
class_ancestor(Class, Ancestor) :-
	x_superclass(Class, Parent, _),
	class_ancestor(Parent, Ancestor).


%  x_message(*Class, *Name, *Arity, *Op)
%  Class defines or inherits a message Name/Arity of type Op.

x_message(Class, Name, Arity, Op) :-
	x_direct_message(Class, Name, Arity, Op).
x_message(Class, Name, Arity, Op) :-
	x_superclass(Class, Super, _),
	x_message(Super, Name, Arity, Op),
	\+ x_uninherit_from(Class, Name, Arity, Op, Super).


%  x_slot(*Class, *Slot)
%  Slot is a slot of Class.  Can backtrack though all slots of all classes,
%  though some slots may be repeated, and all the slots for the same class
%  won't necessarily come out together.

x_slot(Class, Slot) :-
	x_direct_slot(Class, Slot, _, _, _).
x_slot(Class, Slot) :-
	x_superclass(Class, Super, _),
	x_slot(Super, Slot).


%  x_slot(+Class, +Slot, -Type, -Offset, -Attribs)
%  Class defines or inherits a slot Slot of type Type found at Offset,
%  with attributes Attribs.  Class and Slot must be supplied.

x_slot(Class, Slot, Type, Offset, Attribs) :-
	x_slot(Class, Slot, 0, Type, Offset, Attribs).


x_slot(Class, Slot, Offset0, Type, Offset, Attribs) :-
	x_direct_slot(Class, Slot, Type, Offset1, Attribs),
	!,					% implements slot shadowing
	Offset is Offset0+Offset1.
x_slot(Class, Slot, Offset0, Type, Offset, Attribs) :-
	x_superclass(Class, Super, Offset1),
	Offset2 is Offset0+Offset1,
	x_slot(Super, Slot, Offset2, Type, Offset, Attribs).


%  inherited_slot(+Class, +Slot, -Type, -Offset, -Attribs)
%  Same as x_slot, except that Slot is always an inherited slot of Class.

inherited_slot(Class, Slot, Type, Offset, Attribs) :-
	x_superclass(Class, Super, Offset1),
	x_slot(Super, Slot, Offset1, Type, Offset, Attribs).


%  is_instance_method(+Name, +Arity, +Class, -Predname)
%  Class's <- Name/Arity method is an instance method.  Predname is
%  the name of the predicate that implements this instance method.

is_instance_method(Name, Arity, Class, Predname) :-
	(   x_instance_method(Name, Arity, Class, Predname) ->
		true
	;   x_class_method(Name, Arity, Class) ->
		fail
	;   method_defined_by(Class, Name, Arity, <-, 0, _, Defclass),
	    Defclass \== Class,
	    % XXX TODO:  bug?  Does this mean that an instance method declared 
	    %		 to be a class method in a subclass S will still be an 
	    %		 instance method in descendants of S?  I think so.
	    is_instance_method(Name, Arity, Defclass, Predname)
	).


/****************************************************************

			      Debugging

The code generated by this package for sending messages is very
efficient, but this price of this is that sending an unknown message
to an object will just quietly fail.  This can make it very difficult
to track down errors in your code.  Unfortunately, getting around this
problem slows down message sending considerably.

The compromise adopted by this package is to allow users to indicate
whether or not they want to debug the messages sent in this file, or
in part of it, but including a
++verbatim
	:- debug_message.
--verbatim
directive in their file to indicate that messages appearing below in
the file should be debuggable (i.e., they should raise an exception if
they are not defined for the object to which they are sent, rather
than just failing), until the next
++verbatim
	:- nodebug_message.
--verbatim
directive.  These directives may be nested, however, debug mode is
turned off at the end of a file.

****************************************************************/


%  debug_message
%  Start generating code to debug message sending.

debug_message :-
	assert(debug_mode).

%  nodebug_message
%  Stop generating code to debug message sending.

nodebug_message :-
	(   retract(debug_mode) -> true
	;   identified_message(warn,
			  context_error(nodebug_message,
					no_debug_message, 0))
	).



/****************************************************************

			    Extensibility

This package allows itself to be extended in numerous ways.  This
makes it possible for users to go beyond defining new classes with
this package by defining new *kinds* of objects, new attributes for
slots, etc.  You can extend the syntax of class declarations.

These sorts of extensions are made by supplying new clauses for
certain multifile predicates defined and called by this package.  The
following predicates, all defined in module 'obj_decl', may be
extended:

++description
    default_slot_attrib/1
    handle_slot_attrib/8
    expand_method_goal/9
    compile_slot/9
    accessor/4
    type_accessor/8
    analyze_class_defn/5
--description

****************************************************************/

%  analyze_class_defn_hook(+Classdef, +Classname, -Clauses)
%  Clauses is a list of the clauses that need to be generated to
%  define Classname as a class defined by Classdef.
%
%  This user-supplied predicate is declared multifile, so that extensions to
%  this package may define other kinds of class definition.  This hook
%  should be used to define whole new kinds of classes, that don't
%  have ordinary slots and superclasses.

:- multifile analyze_class_defn_hook/3.

%  analyze_class_defn_hook(+Classdef, -Parents, +Parents0, -Direct_slots,
%		+Direct_Slots0, -Clauses, +Clauses0)
%  Parents is a list of superclasses of a class defined by Classdef,
%  followed by Parents0, and Direct_Slots is a list of the slots present in
%  such a class in addition to the slots inherited from Parents, followed by
%  Direct_Slots0.  Clauses is a list of clauses needed to record any
%  extra information contained in Classdef that is not contained in
%  Parents and Direct_Slots, followed by Clauses0.
%
%  This user-supplied predicate is declared multifile, so that extensions to
%  this package may define other kinds of class definition.  This hook
%  should be used in preference to analyze_class_defn_hook/3 when what
%  is being defined can be used in conjunction with ordinary slots and
%  inheritance.

:- multifile analyze_class_defn_hook/7.

%  compile_slot(+Slotspec, +Class, +Offset0, -Offset, -Name, -Type,
%		+Attribs, -Clauses, +Clauses0)
%  Clauses is the list of clauses needed to specify Slotspec as the slot of
%  Class that falls Offset0 bytes from the beginning of Class.  Clauses0
%  is the tail of Clauses after these clauses.  Offset is the offset of the
%  next slot in Class (i.e., the address one byte after this slot).  Name
%  and Type are the name and type of this slot.



%  compile_slot_type_hook(+Slottype, +Class, +Offset0, +Attribs0,
%		-Type, -Offset, -Next, -Attribs)
%  Type and Name are the type and name of a slot in class Class
%  described with name Slotname and of type Slottype.  Type and Name
%  may be different than Slotname and Slottype, but generally Name
%  will be the same as Slotname.  Offset is the offset of the slot,
%  and Next is the offset of the next slot in this class (i.e., the
%  address one byte after this slot).  Offset0 is the offset of the
%  first byte available for this slot (the previous slot's Next).
%  Attribs is a list of the slot's attributes; Attribs0 is the list of
%  attributes previously known.

:- multifile compile_slot_type_hook/8.

%  handle_slot_attrib_hook(+Attrib, +Class, +Name, +Type,-Init,
%			   -Clauses, +Clauses0) 
%  Clauses is the list of clauses defined by Attrib, an attribute of slot
%  Name of class Class, of type Type.  Clauses0 is the tail of Clauses after
%  these clauses.

:- multifile handle_slot_attrib_hook/6.

%  generate_linkage_hook(+Name, +Arity, +Op, +Class, -Clauses, +Clauses0)
%  Clauses is a list of linkage clauses for message Name/Arity, an Op
%  message for class Class.  Clauses0 follows these clauses as the tail of
%  Clauses.

:- multifile generate_linkage_hook/6.

/***************************************************************

			    Error Messages

****************************************************************/


:- multifile 'SU_messages':generate_message/3.

% automatic(?) in SICStus
% 'SU_messages':generate_message(message_at(Message, Position)) -->
% 	'SU_messages':generate_message(Message),
% 	'SU_messages':position(Position).
'SU_messages':generate_message(class_decl_error(Decl,Error)) --> !,
	['In class declaration'-[]],
	[nl],
	['    ~q'-[Decl]],
	[nl],
	'SU_messages':generate_message(Error).
'SU_messages':generate_message(bad_parent(Parent)) --> !,
	['Invalid parent class ~q:  no such class'-[Parent]],
	[nl].
'SU_messages':generate_message(bad_defn(Spec)) --> !,
	['Invalid class definition component ~q'-[Spec]],
	[nl].
'SU_messages':generate_message(bad_slot_type(Type)) --> !,
	['Invalid slot type ~q'-[Type]],
	[nl].
'SU_messages':generate_message(bad_slot_type(Type,InClass)) --> !,
	['Invalid slot type ~q in class ~q: assuming pointer to ~q'-
	[Type,InClass,Type]],
	[nl].
'SU_messages':generate_message(bad_term_class_essence(Essence)) --> !,
	['Invalid term class essence specification term:  ~q'-[Essence]],
	[nl].
'SU_messages':generate_message(ambiguous_inheritance(Class,P1,P2,N,A,Op)) --> !,
	['Ambiguous inheritance in class ~q'-[Class]],
	[nl],
	['method for ~q ~q/~w inherited from ~q and ~q'-[Op,N,A,P1,P2]],
	[nl].
'SU_messages':generate_message(ambiguous_send_super(Goal,Class,P1,P2)) --> !,
	['Ambiguous send super:  class ~q inherits message from'-[Class]],
	[nl],
	['classes ~q and ~q'-[P1,P2]],
	[nl],
	['Goal:  ~q'-[Goal]],
	[nl].
'SU_messages':generate_message(orphan_send_super(Goal,Class)) --> !,
	['Invalid send super:  ~q has no superclass'-[Class]],
	[nl],
	['Goal:  ~q'-[Goal]],
	[nl].
'SU_messages':generate_message(invalid_slot_shadow(Name,Type,Type0)) --> !,
	['Invalid shadowing of inherited slot ~q of type ~q'-[Name,Type0]],
	[nl],
	['with shadowing type ~q'-[Type]],
	[nl].

:- multifile 'SU_messages':contexttype/3.

'SU_messages':contexttype(not_in_class_defn) --> !,
	['when not in a class definition'-[]].
'SU_messages':contexttype(no_debug_message) --> !,
	['when debug mode is off'-[]].
'SU_messages':contexttype(not_inherited) --> !,
	['for message that is not inherited'-[]].


:- multifile 'SU_messages':commandtype/3.

'SU_messages':commandtype(uninherit_directive) --> !,
	[':- uninherit directive'-[]].
'SU_messages':commandtype(inherit_directive) --> !,
	[':- inherit directive'-[]].


:- multifile 'SU_messages':typename/3.

'SU_messages':typename(variable) --> !,
	['unbound variable'-[]].
'SU_messages':typename(valid_class_slot) --> !,
	['class slot'-[]].
'SU_messages':typename(valid_inherit_spec) --> !,
	['inheritance specification term'-[]].
'SU_messages':typename(class) --> !,
	[('class')-[]].
'SU_messages':typename(ancestor_of(Class)) --> !,
	['ancestor of ~w class'-[Class]].
'SU_messages':typename(private_slot) --> !,
	['private slot'-[]].


:- multifile 'SU_messages':operation/3.

'SU_messages':operation(fetch_slot) --> !,
	['fetch'-[]].
'SU_messages':operation(store_slot) --> !,
	['store into'-[]].



:- multifile 'SU_messages':message/3.

'SU_messages':message(in_method(Class,_,Op,Message)) --> !,
	{functor(Message,Name,Arity)},
	['in definition of ~q ~q method for ~q class'-[Op,Name/Arity,Class]].


%  identified_message(+Severity, +Message)
%  Print message Message at severity level Severity, identifying the file and
%  position where the error occurred.

identified_message(Severity, Message) :-
	print_message(Severity, Message).
% QP version:
% identified_message(Severity, Message) :-
% 	get_current_file(File),
% 	effective_source_term_position(Pos),
% 	print_message(Severity, message_at(Message, pos_and_file(Pos,File))).


%  effective_source_term_position(-Pos)
%  Pos is the stream position of the clause currently being processed.
%  Because we collect clauses for later processing, this will sometimes
%  be different than the current stream position.

effective_source_term_position(Pos) :-
	(   current_clause_position(Pos) ->
		true
	;   source_term_position(Pos)
	).


%  with_effective_position(+-Goal, +Pos)
%  Execute Goal with the understanding that if any error message should need
%  to supply a stream position, it should supply the position Pos.  When Goal
%  is completed, or fails or raises an exception, Pos will no longer be
%  understood to be the current position.  NB:  this predicate is always
%  deterministic, even if Goal is not; that is, it commits to the first
%  solution to Goal.
with_effective_position(Goal, Pos) :-
        retractall(current_clause_position(_)),
        assert(current_clause_position(Pos)),
        call_cleanup(Goal, retractall(current_clause_position(_))),
        !.
% [PM] 4.1.3 was:
%with_effective_position(Goal, Pos) :-
%	retractall(current_clause_position(_)),
%	assert(current_clause_position(Pos)),
%	(   on_exception(E, Goal,
%			 (retractall(current_clause_position(_)),
%			  raise_exception(E))) ->
%		retractall(current_clause_position(_))
%	;   retractall(current_clause_position(_)),
%	    fail
%	).


/***************************************************************

			   term_expansion/6

This must come last.

****************************************************************/



expand_directive(class(Classname=Classdef), Expansion) :-
	!,
	(   expand_class_begin(Classname, Classdef, Expansion) -> true
	;   identified_message(error, bad_defn(Classdef))
	).
expand_directive(class(Classname), Expansion) :-
	atom(Classname),
	expand_directive(class(Classname=[]), Expansion).
expand_directive(end_class(Classname), Expansion) :-
	expand_class_end(Classname, Expansion).
expand_directive(end_class, Expansion) :-
	expand_class_end(_, Expansion).
expand_directive(debug_message, []) :-
	debug_message.
expand_directive(nodebug_message, []) :-
	nodebug_message.
expand_directive(instance_method(Methods), Expansion) :-
	declare_instance_methods(Methods, Expansion, []).
expand_directive(class_method(Methods), Expansion) :-
	declare_class_methods(Methods, Expansion, []).
expand_directive(inherit(Specs), Expansion) :-
	declare_inheritance(Specs, Expansion, []).
expand_directive(uninherit(Specs), Expansion) :-
	suppress_inheritance(Specs, Expansion, []).
% undocumented feature -- needs proper error reporting if documented
expand_directive(inlineable_parameters(N,M), []) :-
        integer(N), integer(M),
        N >= 0, M >= 0,
	assert(user_inlineable_parameters(N,M)).


expand_clause(Head, Body, Expansion) :-
	functor(Head, Op, 2),
	message_operator(Op),
	!,
	arg(1, Head, Obj),
	arg(2, Head, Msg),
	expand_method(Obj, Op, Msg, Body, Expansion, []).
expand_clause(Head, Body, (Head :- Body1)) :-
	!,
	expand_clause_body(Body, Body1).


class_decl_expansion((:- Directive), Expansion) :-
	expand_directive(Directive, Expansion),
	!.			% class_decl_expansion/2 is multifile,
				% and you never know what clauses may
				% be added later.  So every term_expansion
				% clause should have a cut.
class_decl_expansion((Head :- Body), Expansion) :- !,
	expand_clause(Head, Body, Expansion).
class_decl_expansion(Clause, Expansion) :-
	functor(Clause, Op, 2),
	message_operator(Op), !,
	arg(1, Clause, Obj),
	arg(2, Clause, Msg),
	expand_method(Obj, Op, Msg, true, Expansion, []).
class_decl_expansion(end_of_file, Expansion) :- !,
	finish_pending_class(_, Expansion, [end_of_file]),
	post_compile_checks,
	cleanup_at_eof.


%%  Ripped off from library(sets).

add_element(Element, Set1, Set2) :-
	member(Element, Set1), !,
	Set2 = Set1.
add_element(Element, Set1, [Element|Set1]).

del_element(Element, Set1, Set2) :-
	select(Element, Set1, Result), !,
	Set2 = Result.
del_element(_, Set1, Set1).

% ----------------------------------------------------------------------
% Install the term expansion
% Use library(expansion) to allow co-existence with other term_expansions.
% ----------------------------------------------------------------------

% Return the line number of the first line of the term layout
condense_layout([FL0|_], FL) :- !, FL=FL0.
condense_layout(FL, FL).

:- multifile user:term_expansion/6.

user:term_expansion(Clauses0, Lay0, Tags, Clauses, Lay, [obj_decl|Tags]) :-
	nonmember(obj_decl, Tags),
	catch(enabled, error(_,_), fail),
	class_decl_expansion(Clauses0, Clauses), !,
	% debugging
	% (   Clauses0==Clauses -> true
	% ;   format('% ORIGINAL : ~q\n', [Clauses0]),
	%     format('% EXPANSION: ~q\n', [Clauses])
	% ),
	condense_layout(Lay0, Lay).

enabled.
