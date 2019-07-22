%  SCCS   : @(#)objects.pl	20.3 12/23/94
%  Package: objects
%  Authors: Peter Schachte
%  Purpose: An Object Oriented Programming Package For Prolog
%  Origin : 04 Nov 92
%
%	+---------------------------------------------------------------+
%	| Copyright (C) 1992 Quintus Corporation.  All rights reserved.	|
%	+---------------------------------------------------------------+
%
%			       Abstract
%
%  This module handles object creation and destruction, as well as
%  other operations, for the objects package.  The goal of this
%  package is to provide a powerful object oriented extension to
%  Prolog that is very efficient.  Most object oriented Prolog
%  extensions use the Prolog database to store information, rendering
%  these packages less useful for large applications due to the
%  performance problems of the dynamic database.  This package uses
%  C-style structures to represent objects where possible, providing
%  faster access and much faster modification of objects.
%
%  This package also improves efficiency by using Prolog's
%  'term_expansion/6' facility.  This allows us to generate efficient
%  code for sending messages and for accessing and setting slots.


:- module(objects, [
	create/2,
	destroy/1,
	(<-)/2,
	(>>)/2,
	(<<)/2,
	class_of/2,
	descendant_of/2,
	pointer_object/2,
	define_method/3,
	undefine_method/3,
	current_class/1,
	class_superclass/2,
	class_ancestor/2,
	direct_message/4,
	message/4
   ]).

% obj_decl generates direct calls to these.  We don't make them public, but
% they are part of the interface of this module to obj_decl.
% 
%	remove_instance_method/3
%
% XXX TODO:  I think there are more!


:- meta_predicate
	define_method(+, +, 0).

:- use_module(library(types)).

:- use_module(library(structs), []). % need the foreign funs

:- public
	fast_create/5,
	get_object/3,
	put_object/3,
	put_object_debug/5.

:- op( 700, xfx, (<-)).

/*****************************Internal**************************

			 Work Left To Be Done

The following things still need to be done for this package:
++enumerate
     o  define_method/3 needs to expand message sends and slot
	accesses in the *Body* argument.

     o  Need to support private and protected methods.

     o  Need to clean up and document the package's extensibility
	features.

     o	Should support some sort of array type.

     o  Provide a simple mechanism for defining new message operators.

     o  Save code space by not having a direct_message/4 clause for
	the get and put messages that access and modify a public slot.
	obj_decl:x_message/? and objects:direct_message/4 will have
	to be changed to also check for public slots so they know
	there are such methods defined.  Maybe other ways to save
	code space?	

     o  Move error messages in this file and in obj_decl.pl into a
	separate file loaded as language(class_msg).
--enumerate


***************************************************************/



/*****************************External**************************

			     Introduction

This section is not intended as a primer on Object Oriented
Programming; I assume familiarity with Object Oriented concepts and
the basics of Object Oriented Programming.  This is just an
introduction to the terminology used in this document and the basic
features of this package.

First a brief glossary:
++description
    object	a manipulable data structure that holds information
		and responds to messages.

    slot	a part of an instance that holds an individual datum.
		Much like a member of a C struct or a field of a
		Pascal record.

    class	a description of what information an instance
		contains, and what messages it responds to.  Every
		instance is an instance of one and only one class.

    instance	another word for object.  The word instance draws
		attention to the class of which it is an instance.

    message	a command to an object to perform some operation, to
		modify itself, or an inquiry into some aspect of the
		object.

    method	an individual class's implementation of a particular
		message.  You send a message to an object, but you
		write methods for a class.

    superclass	a class which is a more general case of a particular
		class.  Each class lists its superclasses.

    parent class
		a synonym for superclass.

    subclass	a class which is a more specific case of a particular
		class.  This is this oposite of superclass.  A class
		does not name its subclasses, they are inferred.

    child class
		a synonym for subclass.

    ancestor	one of a class's superclasses, or one of its
		superclasses's superclasses, etc.  Sometimes in this
		package we use the word ancestor to mean the class
		itself or one of its real ancestors.

    descendant	one of a class's subclasses, or one of its
		subclasses's subclasses, etc.  Sometimes in this
		package we use the word descendant to mean the class
		itself or one of its real descendants.

    inheritance	the process by which a class derives slots and methods
		from an ancestor.

    shadowing	when a class defines its own method for a message one
		of its ancestor also defines, the class's descendants
		will inherit its method for that message, rather than
		its ancestors.  That is, a class always inherits the
		"closer" of two conflicting methods.  This is called
		shadowing.

    multiple inheritance
		by allowing a class to name more than one superclass,
		we allow it to inherit multiple methods for the
		same message from classes not related to each other by
		ancestry.  Of course, this doesn't make any sense.
		Since it's very error-prone for the system to choose a
		particular method to inherit, it forces you to declare
		which method you wish to inherit.

    send super	when the method for a class forces a superclass's
		method for that message to be executed.  This allows a
		class to put a kind of "wrapper" around an inherited
		method, so it doesn't need to duplicate most of the
		method just to make a small extension to it.
--description

And here is a list of the features provided by this package:
++description
    Typed slots	In order to achieve the best performance in slot
		access and update, each slot is given a type.
    Multiple inheritance
		Each class may have zero, one, or more superclasses.
    Automatic generation of accessor/setter methods
		By default, each slot automatically has methods to get
		and set its contents.  These automatic methods can be
		supressed.
    All external slot references go through methods
		Only the methods of an object may directly access and
		update its slots.  Methods for other objects, and
		ordinary Prolog procedures, must send messages to the
		object to access or update its slots.  This
		restriction allows a class to control how its slots
		are used.
    Term-valued slots
		A slot may be of type term.  This means any Prolog
		term can be stored in an object.
    Can store instance of descendant class in slot
		If a slot is declared to be of class x, and y is a
		descendant class of x, this allows you to put in
		instance of class y in the slot.  Also, you can have a
		slot of type object which can hold an object of any
		class.
    Instance methods
		allow different instance of the same class to have
		different methods for the same message.  This is a
		powerful way to implement callbacks: the object sends
		itself a message, which can be different for each
		instance.
    Extensibility
		This package is designed with the realization that it
		will not provide all the features users will want.  An
		effort has been made to allow users to define new
		kinds of objects, slots, and methods, and to extend
		the way current kinds of objects, slots, and methods
		are handled.  This capability is for advanced users.
    Debugging	A simple declaration in your source file informs the
		system that you wish to debug (some of) your code.
		This causes the system to generate slower, more
		careful code.  Messages sent to objects that don't
		understand them will raise an exception in debug mode,
		instead of quietly failing.  An attempt to put an
		object of the wrong class in a slot will be caught and
		an exception will be raised.  See section
		\ref{Debugging Messages}, page \pageref{Debugging
		Messages}.
    Efficiency	The system is quite efficient.  With debugging turned
		off, sending a message usually requires only one extra
		procedure call (beyond what would be required for an
		ordinary procedure call).  Inheritance is precomputed.
		Simple methods, including most setting and accessing
		of slots, avoid even the one intervening procedure
		call.  Sending these messages costs no more than an
		ordinary Prolog predicate call.  Access and update of
		simple number slots use Prolog's low-level memory
		access facilities, and so are very efficient.
    Memory usage
		Static memory usage is kept fairly low.  Each class
		generates two small clauses, plus two clauses for each
		superclass.  Each slot requires a single clause.  Each
		method defined (not inherited) by a class defines a
		separate Prolog predicate, plus a single binary clause
		for each method defined or inherited by the class.

		Dynamic memory usage is similarly frugal.  One extra
		32 bit word is added to each object's memory
		allocation to hold its class.  Term slots currently
		require one dynamic Prolog clause per slot.  Other
		than that, each slot occupies only the needed size (8
		to 64 bits, depending on type).  Instance methods
		require one extra dynamic clause at runtime if you
		define your own method; if you use the default method,
		there is no extra cost
--description


Following sections of this document describe predicates for creating,
destroying, and inquiring about objects; how to send messages to
objects; how to define new classes; how to define methods for your
classes; and how to use the package to inquire about known classes.

****************************************************************/

/***************************************************************

			Working With Instances

****************************************************************/

%  create(+Descriptor, -Object)
%  Object is a newly created and initialized object.  Descriptor is a
%  term describing the object to create.  The functor of Descriptor
%  indicates the class to create.  After the memory is allocated and
%  any slot initializations have been performed, a create message is
%  sent to the object, where the arguments of 'create' are taken from
%  the descriptor term.  For example, create(foo(a,b),Object) will
%  allocate memory for an instance of class foo, bind Object to that
%  object, and then do Object <- create(a,b).  If that message is not
%  defined, an exception is raised.  This means that you must have a
%  create/N method for every arity N you want to be able to use in
%  creating an instance of that class, including arity 0.

create(Descriptor, Object) :-
	must_be(Descriptor, callable, create(Descriptor,Object), 1),
	allocate_instance(Descriptor, Object),
	send_new_message(Descriptor, Object).

%  allocate_instance(+Descriptor, -Object)
%  Object is a newly allocated object whose class is the functor name
%  of Descriptor.  Object has not been initialized at all, except that
%  it is filled with zeros.

allocate_instance(Descriptor, Object) :-
	functor(Descriptor, Class, _),
	(   obj_decl:class_size(Class, Size, Header) ->
		Fullsize is Size+Header,
		structs:calloc(Fullsize, 1, Address0),
		(   Address0 =:= 0 ->
		        illarg(resource(memory), create(Descriptor,Object), 0)
		;   true
		),
		structs:put_atom(Address0, 0, Class),
		Address is Address0+Header,
		functor(Object, Class, 1),
		arg(1, Object, Address)
	;   class_of_hook(Object, _) -> % term object
	        true
	;   illarg(domain(term,create_spec), create(Descriptor,Object), 1)
	).


%  send_new_message(+Descriptor, +Object)
%  Send Object, and instance of Class, a 'create' message whose arguments
%  are taken from the term Descriptor.  But first, call
%  class_message_clauses:initialize_slots(Object).  If anything goes wrong,
%  free Object.

send_new_message(Descriptor, Object) :-
	(   on_exception(Error,
			 send_new_message1(Descriptor,Object),
			 (free_object(Object),
                          % [PM] 4.3 plain re-throw, so throw/1.
                          throw(Error))) ->
		true
	;   free_object(Object),
	    fail
	).


send_new_message1(Descriptor, Object) :-
	(   class_message_clauses:initialize_slots(Object) ->
		true
	;   true				% not every class has
						% slots to initialize
	),
	functor(Descriptor, Class, Arity),
	(   message(Class, <-, create, Arity) ->
		functor(Msg, create, Arity),
		same_args(Arity, 1, Descriptor, 1, Msg),
		class_message_clauses:(<-(Msg, Object))
	;   illarg(existence(initialization_method,create/Arity,0),
		   create(Descriptor,Object), 1)
	).


%  fast_create(+Class, +Createmsg, +Header, +Fullsize, -Object)
%  A faster version of create/2.  Calls to this are generated at compile
%  time when a create goal is seen to create in instance of a known type
%  with a known create method.  This saves a bit of bookkeeping work.
%
%  Object is an instance of Class, which is Fullsize bytes large, including
%  a Header byte header.  Finally send <- Createmsg to it.  If anything
%  goes wrong, free the allocated memory.

fast_create(Class, Createmsg, Header, Fullsize, Object) :-
	structs:calloc(Fullsize, 1, Address0),
	(   Address0 =:= 0 ->
	        functor(Createmsg, _, Arity),
		functor(Descriptor, Class, Arity),
		same_args(Arity, 1, Descriptor, 1, Createmsg),
		illarg(resource(memory), create(Descriptor,Object), 0)
	;   structs:put_atom(Address0, 0, Class),
	    Address is Address0+Header,
	    functor(Object, Class, 1),
	    arg(1, Object, Address),
	    on_exception(Error,
			 fast_create1(Createmsg,Object),
			 (free_object(Object,Address0),
			  throw(Error))) ->
		true
	;   free_object(Object, Address0),
	    fail
	).

fast_create1(Createmsg, Object) :-
	(   class_message_clauses:initialize_slots(Object) ->
		true
	;   true
	),
	class_message_clauses:(<-(Createmsg, Object)).



%  class_message_clauses:initialize_slots(+Object)
%  Initialize the slots of Object.  A clause for this predicate is defined
%  for each class that specifies initial slot values, so this predicate
%  is multifile.

:- multifile class_message_clauses:initialize_slots/1.

%  destroy(+Object)
%  Dispose of Object.  First send a destroy message to Object (i.e.,
%  Object <- destroy) if such a message is defined for Object.

destroy(Object) :-
	(   var(Object) ->
		illarg(var, destroy(Object), 1)
	;   functor(Object, Class, 1),
	    obj_decl:class_size(Class, _, Header),
	    arg(1, Object, Addr0),
	    integer(Addr0) ->
		Address is Addr0 - Header
	;   illarg(domain(term,object), destroy(Object), 1)
	),
	(   message(Class, <-, destroy, 0) ->
		class_message_clauses:'<-'(destroy, Object)
	;   true
	),
	!,  % for safety; in particular destroy method might be non-determinate
	free_object(Object, Address).


%  free_object(+Object)
%  free_object(+Object, +Address)
%  Free Object's memory, but first cleanup any slots that need it.
%  Address is the address of Object's memory.
%  
%  free_object/1 now allows its argument to be a term-class object, and
%  does nothing in that case.  This is needed because an exception in the
%  creation of a term-class object causes a clean-up call to free_object.

free_object(Object) :-
	(   functor(Object, Class, 1),
	    obj_decl:class_size(Class, _, Header),
	    arg(1, Object, Addr0),
	    integer(Addr0) ->
		Address is Addr0 - Header,
		free_object(Object, Address)
	;   class_of_hook(Object, _) ->
	    true                       % don't try to free term class object
	;   illarg(domain(term,object), destroy(Object), 1)
	).

free_object(Object, Address) :-
	(   class_message_clauses:cleanup_slots(Object) -> true
	;   true				% not every class needs cleanup
	),
	structs:free(Address).


%  class_message_clauses:cleanup_slots(+Object)
%  Initialize the slots of Object.  A clause for this predicate is defined
%  for each class that specifies initial slot values, so this predicate
%  is multifile.  

:- multifile class_message_clauses:cleanup_slots/1.


%  class_of(+Object, -Class)
%  Class is the class of Object.  Raises an exception if Object is not really
%  an object or is not bound.

class_of(Object, Class) :-
	(   var(Object) ->
		illarg(var, class_of(Object,Class), 1)
	;   functor(Object, Class0, 1),
	    obj_decl:class_size(Class0, _, _) ->
		Class0 = Class
	;   class_of_hook(Object, Class1) ->
		Class = Class1
	;   illarg(type(object), class_of(Object,Class), 1)
	).

%  descendant_of(+Object, *Class)
%  Object is an instance of Class or of a descendant of Class.

descendant_of(Object, Class) :-
	(   var(Object) ->
		illarg(var, descendant_of(Object,Class), 1)
	;   functor(Object, Class0, 1),
	    obj_decl:class_size(Class0, _, _) ->
		true
	;   class_of_hook(Object, Class0) ->
		true
	;   illarg(type(object), descendant_of(Object,Class), 1)
	),
	class_ancestor(Class0, Class).


%  pointer_object(+Addr, -Obj)
%  pointer_object(-Addr, +Obj)
%  Addr is the address of object Obj.  This can be used to get the address of
%  an object, or to get an object given its address.
%  This is a low level operation, passing an invalid address may crash the system.

pointer_object(Addr, Obj) :-
	(   integer(Addr) ->
		(   Addr =:= 0 ->
			Obj = null
		;   structs:get_functor(Addr, Functor), % [MC] 4.0.5
		    functor(Obj, Functor, 1),
		    arg(1, Obj, Addr)
		)
	;   nonvar(Addr) ->
		illarg(type(integer), pointer_object(Addr,Obj), 1)
	;   compound(Obj),
	    functor(Obj, _, 1) ->
		arg(1, Obj, Addr)
	;   Obj == null ->
		Addr = 0
	;   var(Obj) ->
		illarg(var, pointer_object(Addr,Obj), 0)
	;   illarg(type(object), pointer_object(Addr,Obj), 2)
	).

/***************************************************************

			   Sending Messages

This package defines three kinds of messages: send messages, put
messages and get messages.  *Send* messages are the usual sort of
message, used for performing operations on an object, or for
performing operations that depend on the object.  *Get* messages
merely inquire about some aspect of the object without effectively
changing it\footnote{I say without "effectively" changing it because a
get message may do some destructive bookkeeping operations, or may
compute and cache values, etc.  It is a matter of judgement whether
some subtle side effects on an object suggests that that message
should not be a get message}.  *Put* messages are executed for the
effect of modifying the object.  It is advisable to always have a get
message for each put message, with the same name and arity.  It is
also highly desirable to have a put message for as many of the get
messages as possible.

The reason for the complication of distinguishing put, get, and send
messages is ironically that they make things simpler for the
programmer.  For example, if you have a window class, and each window
may be open or closed, you could have a message to close a window and
another to open it.  This would work fine, but soon you would find
that you need a message to find out whether a window is open or
closed, so you'd add a message to get the window's state.  But now you
need to remember three messages to deal with a window's state.  And
soon you'll find that you need an iconified state, so you'd have to
add an iconify message.

Using get and put messages, you'd have a 'window_state'(*State*) get
and put message, where *State* is one of {'open', 'closed',
'iconified'}.  This is two distinct messages, but very closely
related.  These are much easier to remember and understand than
separate 'open', 'close', 'iconify' and 'get_state' messages.

Many aspects of an object's state are both readable and writable;
separating get and put messages makes explicit this duality.

\label{message operator}
The syntax used for sending a message is
++quote
*Object* *Operator* *Message*
--quote
where *Object* is the object to which we're sending the message,
*Message* is a term determining the message we're sending, and
*Operator* is the message operator determining the kind of message
we're sending.  The message operators defined by this package are
++description
    <-	send message
    >>	get message
    <<	put message
--description

It is possible for users to introduce new message operators.  See
section \ref{Extensibility}, page \pageref{Extensibility}.

****************************************************************/


:- multifile
        class_message_clauses:(<-)/2,
        class_message_clauses:(<<)/2,
        class_message_clauses:(>>)/2.

%  '<-'(+Object, +Message)
%  '>>'(+Object, +Attribute)
%  '<<'(+Object, +Attribute)
%  Send a message to Object.  Object <- Message sends message Message
%  to Object.  Object >> Attribute fetches the value of Attribute of
%  Object.  Object << Attribute stores the value of Attribute of
%  Object.  In all cases, Message or Attribute must be either an atom
%  or a compound term describing the message to send or attribute to
%  fetch or store.

<-(Object, Message) :-
	ensure_can_send(Object, <-, Message),
	class_message_clauses:(<-(Message, Object)).

:- (>>)/2 is documented_as((<-)/2).

>>(Object, Message) :-
	ensure_can_send(Object, >>, Message),
	class_message_clauses:(>>(Message, Object)).

:- (<<)/2 is documented_as((<-)/2).

<<(Object, Message) :-
	ensure_can_send(Object, <<, Message),
	class_message_clauses:(<<(Message, Object)).



%  ensure_can_send(+Object, +Op, +Msg)
%  Raise an exception if Object cannot respond to message Msg of type
%  Op, otherwise succeed quietly.  Does not actually send the message.
%  Assumes that Op is bound to a valid message op.

ensure_can_send(Object, Op, Msg) :-
	(   var(Msg) ->
		Goal =.. [Op,Object,Msg],
		illarg(var, Goal, 2)
	;   \+ callable(Msg) ->
		Goal =.. [Op,Object,Msg],
		illarg(domain(term,valid_message), Goal, 2)
	;   functor(Msg, Name, Arity),
	    ensure_can_send(Object, Op, Name, Arity)
	).


/*****************************External**************************

			   Defining Classes

Defining a new class with this package is pretty simple, at least for
most cases.  Each class must be defined in a Prolog source file, which
may then be loaded into the Prolog development environment or compiled
with qpc.  The one unpleasant complication is that you must include
the line
++verbatim
	:- load_files(library(obj_decl), [when(compile_time),if(changed)]).
--verbatim
in any file that defines classes or sends messages.  This should
appear near the top of the source file, before the first class
definition or clause that sends a message.

The builtin predicates for accessing and modifying slot contents are
documented in section \ref{Direct Slot Access}, page
\pageref{Direct Slot Access}.

****************************************************************/

/*============================External============================

		       Simple Class Definition

A simple class is defined by the following declaration in a Prolog
source file:
++quote
:- 'class' *Classname* '=' [*Slotname* : *Slottype*, ...].
--quote
You may provide as many *Slotnames* and corresponding *Slottypes* as
you like.  This defines *Classname* as a class with the specified
slots.

For example,
++verbatim
    :- class point = [x:integer, y:integer].
--verbatim
defines 'point' to be new class with integer slots 'x' and 'y'.

Note that every class for which you want to be able to create
instances must define at least one 'create' method.  See section
\ref{Creation and Destruction Methods}, page \pageref{Creation and
Destruction Methods} for more on this topic.

================================================================*/

/*============================External============================

			      Slot Types

Each slot in a class definition must be given a type.  The legal types
are as follows:
++description
    integer		a long signed integer
    integer_64		a 64 bit signed integer
    integer_32		a 32 bit signed integer
    ingeter_16		a 16 bit signed integer
    integer_8		an 8 bit signed integer
    unsigned        	a long unsigned integer
    unsigned_64     	a 64 bit unsigned integer
    unsigned_32     	a 32 bit unsigned integer
    unsigned_16   	a 16 bit unsigned integer
    unsigned_8   	an 8 bit unsigned integer
    float 		a double precision float
    float_32		a 32 bit float
    atom		a Prolog atom (32 bits)
    address		a 32 bit address
    term		a Prolog term.  Currently the term is not
			stored in the object itself, but kept in a
			separate dynamic Prolog predicate.
    *Class*		*Class* the name of a defined class; a 32 bit
			pointer to an instance of *Class* or a
			descendant of *Class*.
    pointer(*Type*)	*Type* an atom; just like address, except that
			access to this slot will yield, and update of
			this slot will expect, a term of arity 1 whose
			functor is *Type* and whose argument is the
			address.  This will be useful to store structs
			package objects in object slots.
--description

================================================================*/

/*============================External============================

			   Slot Attributes

In addition to having a type, a slot may be declared to be private,
and may be given an initial value.  These are called slot attributes.
The following subsections describe these attributes.  In addition to
these attributes, advanced users may create new attributes that may be
associated with slots at compile time.  This is described in section
\ref{Extensibility}, page \pageref{Extensibility}.

================================================================*/

/*----------------------------External----------------------------

			    Private Slots

If a slot is declared to be private, it does not get access and
modification methods created for it automatically.  This means that if
you don't provide a way to access or modify the slot's value, users
will have no way to do that.  This is what you want for slots that are
used to maintain internal values that users should mess with.  Or you
may want to write your own access or motification methods that do some
kind of record keeping or something, so the default methods don't suit
you.  There is currently no way to suppress only the access or
modification method, but not the other.

A slot is declared private by preceding the slot name with the word
'private'.  Note that 'private' is an operator, so you don't need
parentheses.  For example, our point class may be define with both of
its slots private as follows:
++verbatim
    :- class point = [private x:integer, private y:integer].
--verbatim

----------------------------------------------------------------*/

/*----------------------------External----------------------------

			 Initial Slot Values
\label{Initial Slot Values}

By default, all number slots are initialized to 0, atoms
are initialized to the empty atom (\verb+''+), addresses and pointers are
initialized to null pointers, and object pointers are initialized to
null objects.  If you wish to provide a different initial value,
you may do that by following the *Slotname* : *Slottype* with '='
*InitialValue*.  For example, we could make the default values of the
x and y slots of our point class be 100 with the following
declaration:
++verbatim
    :- class point = [x:integer=100, y:integer=100].
--verbatim

For object slots (slots that hold pointers to objects), the
initialization may be a term you would pass to create to create an
instance of this class to store in this slot.  In this case, the
object will automatically be created as specified, and a pointer to it
stored in the slot in question.  Furthermore, when an instance of the
object you are specifying is destroyed, the object (whose address is)
stored in that slot will also be destroyed.

----------------------------------------------------------------*/

/*============================External============================

			     Inheritance

A superclass for a new class can be specified as follows:
++quote
:- 'class' *Classname* '=' [*Slotname* : *Slottype*, ...] + *Superclass*.
--quote
or
++quote
:- 'class' *Classname* '=' *Superclass* + [*Slotname* : *Slottype*, ...].
--quote
both are equivalent.  If the new class needs no slots other than
those inherited from *Superclass*, then you may write simply:
++quote
:- 'class' *Classname* '=' *Superclass*.
--quote
or
++quote
:- 'class' *Classname* '=' *Superclass* + [].
--quote
which again are equivalent.

For multiple inheritance, you need only specify all superclasses
separated with plus operators ('+'):
++quote
:- 'class' *Classname* '=' [*Slotname* : *Slottype*, ...] + *S1* + ... + *Sn*.
--quote
where *S1* through *Sn* are the superclasses of *Classname*.  The
list of slots could come first, last, or anywhere in the middle.  It
is even permitted to include several lists of slots, in which case all
of the listed slots will be slots of *Classname*.

Every class inherits all of the slots of its superclasses, and by
default inherits all of its superclasses' methods, though that can be
overridden (see \ref{uninherit}, page \pageref{uninherit}).  This
means that instances of the class will have all the slots that all of
its superclasses have, and by default will have all the methods of all
of its superclasses.

A class may redefine a slot that is defined by one of its superclasses,
but only if the definition specified is *compatible* with the
superclass's definition.  For all but slots that hold (pointers to)
other objects, compatible means identical.  For object slots, the
specified class of the slot must be the same as the superclass's
definition, or a subclass of that specified.

Subclasses may change the attributes of a slot; for example, a
subclass may specify a different default value without changing the
type at all.

================================================================*/

/*============================External============================

		   The Scope of a Class Definition
\label{The Scope of a Class Definition}

The definition of the class continues until a
++quote
    :- 'end_class' *ClassName*.
--quote
or
++quote
    :- 'end_class'.
--quote
directive, or until the next :- 'class' directive, or until the end
of the file, whichever comes first.  It is not possible to nest one
class definition within another.  All clauses that look like a method
definition (see section \ref{Defining Methods}, page \pageref{Defining
Methods}) are considered to be methods for the specified class.

================================================================*/

/*****************************External**************************

			   Defining Methods
\label{Defining Methods}
As mentioned in the introduction, a method defines how instances of a
particular class respond to a certain message.  A method for a class
may be defined anywhere within the class's definition (see \ref{The
Scope of a Class Definition}, page \pageref{The Scope of a Class
Definition}).  Methods may never be defined outside the scope of a
class definition.

****************************************************************/

/*============================External============================

			    Simple Methods

A method is defined by an ordinary Prolog clause whose head has a
message operator as principle functor (see \ref{message operator},
page \pageref{message operator}).  It is permissible to define a
method with more than one clause.  Prolog will index on the first
argument of the message\footnote{Currently first argument indexing is
not available for instance methods (see \ref{Instance Methods}, page
\pageref{Instance Methods}).}, so writing recursive methods is quite a
reasonable thing to do.

For example, a method to print a point object might be coded:
++verbatim
	Self <- print(Stream) :-
		Self >> x(X),
		Self >> y(Y),
		format(Stream, '(~w,~w)', [X,Y]).
--verbatim
This clause must appear within the definition of the point class.
That is, it must appear after the :- 'class' directive and before
the next :- 'class' or :- 'end_class' directive.

Given this definition, you can write
++verbatim
	...,
	Point <- print(Stream),
	....
--verbatim
where 'Point' is a point object and 'Stream' is a Prolog stream.

This example method sends two get messages to the object to get its x
and y coordinates.  These two messages will often be sent together, so
it may be worth defining a separate message to get both together.
This will be more efficient, and easier to read and write, since the
source code will be shorter.

Get and put messages may be defined similarly.  For example, to
provide a polar coordinate interface to the point object, you may have
the following method clauses:
++verbatim
	Self >> r(R) :-
		Self >> x(X),
		Self >> y(Y),
		Rsquare is X*X + Y*Y,
		sqrt(Rsquare, R).

	Self << r(R) :-
		Self >> x(X),
		Self >> y(Y),
		OldRsquare is X*X + Y*Y,
		NewRsquare is R*R,
		Scale is NewRsquare/OldRsquare,
		NewX is X*Scale,
		NewY is Y*Scale,
		Self << x(NewX),
		Self << y(NewY).
--verbatim
Implementation of put and get methods for theta are left as an
exercise for the interested.

================================================================*/

/*============================External============================

		   Creation and Destruction Methods

\label{Creation and Destruction Methods}

You must specify all the ways instances of each class you define may
be created.  This is done by defining a send method or methods with the
name 'create' and any arity.  The simplest creation method would be:
++verbatim
	Self <- create.
--verbatim
If this method were defined for the point class, then one could call
++verbatim
	create(point, Object)
--verbatim
to create an instance of class point.  This creation method doesn't do
extra initialization, but if you wished to perform any particular
initialization, this would be the place to do it.

It may sometimes be convenient (or even essential) to pass parameters
to the creation method.  This is done by defining a create method with
parameters:
++verbatim
	Self <- create(X,Y) :-
		Self << x(X),
		Self << y(Y).
--verbatim
Given this method, one can call
++verbatim
	create(point(100,200), Point)
--verbatim
to create a new point object with x = 100 and y = 200.

Sometimes one must have some code executed when each object of a
certain class is destroyed.  This is often necessary when objects
point to other objects or to separately allocated memory.  This can be
accomplished by defining a 'destroy'/0 send method for the class.
When an object Object is destroyed, the message
++verbatim
	Object <- destroy
--verbatim
is sent, if it is defined.  It is permissible for no such method to be
defined.

================================================================*/

/*============================External============================

			   Abstract Classes

Sometimes you may want to define a class of which no instances should
be made.  Such a class is called an *abstract* *class*, as it defines
an abstraction for other classes, but is not really a full fledged
class itself, since it can have no instances.  This will often be the
case when you want to define a number of classes that all share some
slots and methods, but there is no least common denominator class.  In
this case, you may define a class that has the slots and methods you
want to have in common, and simply not give it an create methods.  If
the class is a subclass of a concrete (non-abstract) class, then you
should *uninherit* all create methods (see section \ref{uninherit},
page \pageref{uninherit}).

================================================================*/

/*============================External============================

		   Multiple Inheritance of Methods

Multiple inheritance leads to the possibility of conflicts.  If two
classes both define or inherit a particular message, and a new class
is defined that specifies both of them as superclasses, then from
which of the two superclasses should the new class inherit this
message?

Different Object Oriented Programming packages approach the resolution
of this conflict differently.  Some packages choose the method of the "first"
of the two superclasses.  Others choose the method inherited through
the fewest generations.  Others are even more complex.

This package takes a simple approach to the problem:  force the user
to choose.  Experience shows that clever attempts to resolve this
conflict automatically too often lead to confusing behavior that can
be difficult to debug.  If there's a conflict, only the user can
really decide what she really meant, so she should tell the system.

This is done with the :- 'inherit' directive:
++quote
:- 'inherit' *Classname* *Operator* *Name* / *Arity*, ....
--quote
where *Classname* names the class from which the message should be
inherited, *Operator* indicates which kind of message it is, and
*Name* and *Arity* indicate the name and arity of the message to be
inherited.  You may include multiple inheritance specifications in one
directive by separating them with commas, much like :- 'dynamic'
declarations.

I provide the classic toy truck example:
++verbatim
	:- class toy=[name:atom].
	Self <- create.
	Self >> size(small).
	Self >> rolls(false).

	:- class truck=[name:atom].
	Self <- create.
	Self >> size(large).
	Self >> rolls(true).

	:- class toy_truck = toy+truck.
	:- inherit toy>>size/1, truck>>rolls/1.
	Self <- create.
--verbatim

================================================================*/

/*============================External============================

		Preventing the Inheritance of Methods

\label{uninherit}

Sometimes you do not wish to inherit certain methods defined by a
class's superclasses.  A superclass's methods can indivicually be
hidden from a class with a :- 'uninherit' declaration, e.g.,:
++verbatim
	:- uninherit super1<-method1/3, super2<<method2/1.
--verbatim
This prevents the class within whose scope this line appers from
inheriting the method1/3 send method from class super1 and the
method2/1 put method from class super2.  You may leave the class part
of one of these method specifications unbound to uninherit the
specified message from all superclasses that define it:
++verbatim
	:- uninherit _<-method3/0.
--verbatim
This prevents the method3/0 send message from being inherited at all.

Note that if you define a message for your class, you do not need to
uninherit that message from its superclasses:  it will automatically
be shadowed.

================================================================*/

/*============================External============================

			      Send Super

\label{Send Super}

Occasionally you will find that you want to write a method that is
just like a superclass's method, but it needs to do just a little
more.  If you write your own method, it shadows the method that does
most of what you want.  You could define a new message name that
invokes the inherited message, but that means your class understands a
message that wouldn't be understood by the superclass, and your class
won't behave properly for the message you wanted to modify.  Or you
could alway copy over the superclass's code for that message (if you
have access to it), but then you bloat your code and introduce
maintenance problems.

Send super solves this problem by allowing you to invoke the superclass's
method from the method that shadows it.  The syntax for send super is
much like the syntax for ordinary message sending, except that instead
of sending the message to your object, you send it to the atom 'self'.
For example, if class a defines a method 'print'/0, and its subclass,
b, wants to print the same thing, except with parenthesis around it,
it might contain the method:
++verbatim
	Self <- print :-
		put_code(0'(),
		super <- print,
		put_code(0')).
--verbatim

Sometimes a class may inherit the same message from multiple
superclasses, and may need to specify which superclass's method to
invoke.  This is done by specifying the term 'super'(*Class*) as the
object to send the message to.  This will often happen with create
messages.  For example, if class 'c' has 'a' and 'b' as superclasses,
it may want to define a create method as follows:

++verbatim
	Self <- create :-
		super(a) <- create,
		super(b) <- create.
--verbatim

================================================================*/

/*============================External============================

			More on 'create' Methods

Create methods can be particulary tricky in cases of multiple
inheritance.  Since almost every class will have at least one create
method, create methods are very likely to clash with multiple
inheritance.  In some ways, it's even worse when they don't clash,
which happens when each superclass defines a create method with a
different arity.  In this case the compiler won't even warn you about
the conflict, and you may not notice the problem.

For example, if if class c has a and b as superclasses, and a defines
create/1 and b defines create/2, class c will inherit two create
methods.  If you specify c of arity 1 when create an instance of c,
you will execute b's create method, but not c's; if you create c with
arity 2, you'll only execute c's create method.  You probably need to
execute both.  In fact, you may need to define a create method of
arity 3 containing the arguments to pass to each superclass's create
method.  And you should probably not support the arity 1 and 2 create
messages, so code won't accidentally create an instance of c that is
only half initialized.  This can be done as follows:
++verbatim
	:- uninherit a<-create/1, b<-create/2.

	Self <- create(A, B1, B2) :-
		super(a) <- create(A),
		super(b) <- create(B1, B2).
--verbatim
Note that send super allows you to send messages that you have
explicitly uninherited.

================================================================*/

/*============================External============================

			Direct Slot Access
\label{Direct Slot Access}

Sometimes one needs a more efficient way to access slots.  Similarly,
sometimes one wants to implement a get or put method oneself, rather
than accept the default method.  In these cases, one needs a way to
directly access or modify a slot's value.  This is provided by the
'fetch_slot'/2 and 'store_slot'/2 predicates:

++quote
{\bf fetch_slot}(*Slotname*, *Value*) \%\
{\bf store_slot}(*Slotname*, *NewValue*)
--quote

These predicates may only appear in the body of a method clause, and
they always operate on the object to which the message is sent.  That
is, it is not possible to directly access or modify the slots of an
object except from a method for that object.

These predicates should be used sparingly.  If you define a method
using get messages to get slot values, then this method will work
properly for descendant classes which redefine the get messages to
calculate the values.  If you use direct slot access to get slot
values, then the method will not behave correctly when inherited by
classes which redefine get messages.

In our point class examples, we often found that we wanted to get or
set both x and y values at the same time.  We might wish to redefine
our point class as follows to make it easier to work with:

++verbatim
	:- class point = [private x:integer, private y:integer].

	Self <- create.				% create point at 0,0
	Self <- create(X,Y) :-			% specify initial position
		Self << point(X,Y).

	Self >> point(X,Y) :-
		fetch_slot(x, X),
		fetch_slot(y, Y).
	Self << point(X,Y) :-
		store_slot(x, X),
		store_slot(y, Y).

	Self <- print(Stream) :-
		Self >> point(X, Y),
		format(Stream, '(~w,~w)', [X,Y]).

	Self >> r(R) :-
		Self >> point(X,Y),
		Rsquare is X*X + Y*Y,
		sqrt(Rsquare, R).
	Self << r(R) :-
		Self >> point(X,Y),
		Scale is R*R/X*X + Y*Y,
		NewX is X*Scale,
		NewY is Y*Scale,
		Self << point(NewX,NewY).
--verbatim

Note that the << point(X,Y) method serves as an initialization
method, as well as providing a more concise and efficient way to set
both x and y values simultaneously.  Also note that the x and y slots
are declared private, so we do not even allow the x and y values to be
accessed or modified separately.  This may or may not be a good idea,
but it is done here to show that it is an option.  Some classes
may have sets of slots that should not be set separately; by making
these slots private and writing your own put and get methods, you can
force users to set slots in groups.

When accessing a slot containing a pointer to an object, if that
pointer happens to be null, fetch_slot/2 returns the atom 'null'.
Similarly, you may pass the atom 'null' to store_slot/2 to store a
null pointer in an object pointer slot.

================================================================*/

/*============================External============================

			   Instance Methods
\label{Instance Methods}

Sometimes you may wish to have different methods for different
instances of a particular class.  For example, each pushbutton in an
application is likely to want to respond differently to being pressed.
If the pushbutton object is sent a '<-' 'pushed'/0 message when the
pushbutton is pressed, then each pushbutton really needs to have a
different method for '<-' 'pushed'/0.  One solution to this would be
to define a different subclass for pushbutton for each use of a
pushbutton.  Another solution would be to store each pushbutton's
method in a term slot, and have the '<-' 'pushed'/0 method fetch that
slot and call it.

Both of these approaches are clumsy and inefficient (the former wastes
space, while the latter wastes runtime).  This package supplies
another solution: instance methods.  *Instance methods* are methods
that can be defined at runtime for individual instances.  A message is
declared to support instance methods with the following declaration:
++quote
:- 'instance_method' *Name* / *Arity*, ....
--quote
in the context of a class definition.  This declares that instances of
this class, and descendants of this class, may each define its own
method for this message.

Note that no message operator is allowed in the 'instance_method'
declaration.  The current implementation does not support instance
methods for get and put messages, only for send messages.

Any method defined for this message by the class will be considered
the default method for this message.  Each instance that doesn't
define its own method for this message will have this method.
Defining a new method overrides this default method; there is no need
to explicitly remove it.  Instance methods may be defined for a
particular instance with the 'define_method'/3 predicate.  An
instance's instance method may be removed, reverting to the default
method for its class, with the 'undefine_method'/3 predicate.  These
predicates are described below.

If a class defines an instance method, its descendants inherit that
class's default method as well as the fact that that method is an
instance method.  A descendant class may wish to define an ordinary
method for that message, and not have it be an instance method.  This
is done with a :- 'class_method' declaration:
++quote
:- 'class_method' *Name* / *Arity*, ....
--quote
declares that this class's method for the send message *Name* /
*Arity* is an ordinary method, not an instance method.  A descendant
class may still declare this to be an instance method, so the same
message may be an instance method for some classes and an ordinary
class method for others, wherever these classes appear in the class
hierarchy.

================================================================*/

%  define_method(+Object, +Message, +Body)
%  Install Body as the method for the send message Message for object
%  Object.  Message must have been declared to be an instance method
%  for the class of Object.  Following the execution of this goal,
%  sending Message to Object will execute Body, rather than any method
%  previously defined with 'define_method'/3 or the default method.

define_method(Object, Message, Body) :-
	Goal = define_method(Object,Message,Body),
        % [PM] 4.2.1 FIXME: The type-tests on Body do not take the implied ':'/2 meta wrapper into account.
	(   var(Object) ->
		illarg(var, Goal, 1)
	;   var(Message) ->
		illarg( var, Goal, 2)
	;   var(Body) ->
		illarg(var, Goal, 3)
	;   \+ compound(Object) ->
		illarg(type(object), Goal, 1)
	;   \+ callable(Message) ->
		illarg(type(valid_message), Goal, 2)
	;   \+ callable(Body) ->
		illarg(type(callable), Goal, 3)
	;   otherwise ->
            %% [PM] 4.1 Will never fail, will raise exception on non-object.
            class_of(Object, Class),
            (   functor(Message, Name, Arity),
                obj_decl:instance_method(Name,Arity,Class,Predname) ->
                    Predarity is Arity+2,
                    arg(1, Object, Address),
                    remove_instance_method(Predname, Predarity, Address),
                    functor(Head, Predname, Predarity),
                    arg(1, Head, Address),
                    same_args(Arity, 3, Head, 1, Message),
                    simple_expand_body(Body, user, Class, Object, Address,
                                       Message, Body1),
                    asserta(class_instance_method:(Head :- !,Body1))
            ;   illarg(domain(term,instance_method(Class)), Goal, 2)
            )
        ).


%  undefine_method(+Object, +Messagename, +Arity)
%  Remove Object's currently defined instance method for
%  Messagename/Arity, if it has one.  Messagename/Arity must have been
%  declared to be an instance method for the class of Object.
%  Following the execution of this goal, sending this message to Object
%  will execute the default method for this message for instances of
%  the class of Object.

undefine_method(Object, Messagename, Arity) :-
	Goal = undefine_method(Object,Messagename,Arity),
	(   var(Object) ->
		illarg(var, Goal, 1)
	;   var(Messagename) ->
		illarg(var, Goal, 2)
	;   var(Arity) ->
		illarg(var, Goal, 3)
	;   \+ compound(Object) ->
		illarg(type(object), Goal, 1)
	;   \+ atom(Messagename) ->
		illarg(type(object), Goal, 2)
	;   \+ integer(Arity) ->
		illarg(type(object), Goal, 3)
	;   class_of(Object, Class), % [PM] 4.1.1+ never fails (throws on non-object)
	    ( obj_decl:instance_method(Messagename,Arity,Class,Predname) ->
		Predarity is Arity+2,
		arg(1, Object, Address),
		remove_instance_method(Predname, Predarity, Address)
            ;   illarg(domain(term,instance_method(Class)), Goal, 0, Messagename/Arity)
            )
	).


%  remove_instance_method(+Predname, +Predarity, +Address)
%  Retract all clauses for class_instance_method:Predname/Predarity
%  where the first argument is Address.  There should be at most one
%  such clause, but we'll be conservative about this and just make
%  sure we get all of them.  We must be careful not to remove the
%  default clause which will have an unbound first argument.

remove_instance_method(Predname, Predarity, Address) :-
	functor(Head, Predname, Predarity),
	arg(1, Head, Addrarg),
	(   clause(class_instance_method:Head, _, Ref),
	    Addrarg == Address,
	    erase(Ref),
	    fail
	;   true
	).


/*============================Internal============================

		       Expanding Instance Method Bodies

Instance methods have clauses added at runtime, and these will need to be
expanded, in particular for fetch_slot/2 and store_slot/2 goals.  At the
moment, in order to keep this code simple (and to keep runtime support as
small as possible), we don't worry about expanding inline-able methods.  Since
instance methods are dynamic code, in many cases interpretation overhead would
tend to eat up much of the gain anyway.  We do however make use of the fact
that we know that address of the object whose slots we are accessing, and
compute exact addresses during the expansion.

Note that this code, while quite different from the expansion code in
obj_decl for the reasons mentioned above, still has many similarities.
Therefore, any changes made here *may* need to be mirrored in obj_decl.pl.

================================================================*/

%  simple_expand_body(+Body0, +Module, +Class, +Object, +Address, +Msg, -Body)

simple_expand_body(Goal0, Module, _, _, _, _, Goal) :-
	var(Goal0),
	!,
	Goal = call(Module:Goal0).
simple_expand_body((X,Y), Module, Class, Object, Address, Msg, (X1,Y1)) :-
	!,
	simple_expand_body(X, Module, Class, Object, Address, Msg, X1),
	simple_expand_body(Y, Module, Class, Object, Address, Msg, Y1).
simple_expand_body((X;Y), Module, Class, Object, Address, Msg, (X1;Y1)) :-
	!,
	simple_expand_body(X, Module, Class, Object, Address, Msg, X1),
	simple_expand_body(Y, Module, Class, Object, Address, Msg, Y1).
simple_expand_body((X->Y), Module, Class, Object, Address, Msg, (X1->Y1)) :-
	!,
	simple_expand_body(X, Module, Class, Object, Address, Msg, X1),
	simple_expand_body(Y, Module, Class, Object, Address, Msg, Y1).
simple_expand_body(\+(X), Module, Class, Object, Address, Msg, \+(X1)) :-
	!,
	simple_expand_body(X, Module, Class, Object, Address, Msg, X1).
simple_expand_body(Module:X, _, Class, Object, Address, Msg, X1) :-
	!,
	simple_expand_body(X, Module, Class, Object, Address, Msg, X1).
simple_expand_body(fetch_slot(Slot,Value), _, Class, _Object, Address, _Msg,
		Goal) :-
	!,
	(   accessor(Class, Slot, Address, Value, Goal, _) ->
		true
	;   illarg(domain(term,valid_class_slot), fetch_slot(Slot,Value), 1)
	).
simple_expand_body(store_slot(Slot,Value), _, Class, _Object, Address, _Msg,
		Goal) :-
	!,
	(   accessor(Class, Slot, Address, Value, _, Goal) ->
		true
	;   illarg(domain(term,valid_class_slot), store_slot(Slot,Value), 1)
	).
simple_expand_body(Goal, Module, _, _, _, _, Module:Goal).


%  accessor(+Class, +Slotspec, +Address, +Value, -Accessor, -Setter)

accessor(Class, Slotspec, Address, Value, Accessor, Setter) :-
	get_slot(Class, Slotspec, Type, Address, Slotaddr),
	type_accessor(Type, Slotspec, Slotaddr, Value, Accessor, Setter).


%  get_slot(+Class, +Slot, -Type, +Offset0, -Offset)
%  Slot is a slot of type Type in instances of class Class.  Offset is
%  the byte offset of slot Slot in Class, plus Offset0.

get_slot(Class, Slot, Type, Offset0, Offset) :-
	(   atom(Slot) ->
		MajorSlot = Slot,
		Rest = []
	;   nonvar(Slot),
	    Slot = [MajorSlot|Rest]
	),
	get_slot1(Class, MajorSlot, Type, Offset0, Offset, Rest).

%  get_slot1(+Class, +Slot, -Type, +Offset0, -Offset, +Rest)

get_slot1(Class, Slot, Type, Offset0, Offset, Rest) :-
	(   obj_decl:direct_slot(Class, Slot, Type0, Offset1, _) ->
		Offset2 is Offset0+Offset1,
		get_subslot(Rest, Type0, Type, Offset2, Offset)
	;   obj_decl:superclass(Class, Parent, Offset1),
	    Offset2 is Offset0+Offset1,
	    get_slot1(Parent, Slot, Type, Offset2, Offset, Rest) ->
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



%  type_accessor(+Type, +Slotname, +Slotaddr, +-Value, -Get, -Put)
%  Get and Put are goals which will bind Value to the value of a slot,
%  or store Value as the new value of that slot.  Type is the type of
%  the slot, and Slotname is its name.

type_accessor(term_object(_,Term,Slots), Slot, Address, Term, Get, Put) :-
	!,
	term_accessors(Slots, [Slot], Address, Get, Put).
type_accessor(atom, _, Address, Value,
		structs:get_atom(Address,0,Value),
		structs:put_atom(Address,0,Value)) :-
	!.
type_accessor(object_ptr(Class), Slot, Address, Value, Get, Put) :-
	!,
	Get = get_object(Address, 0, Value),
	Put = put_object_debug(Value,Address,0,Slot, Class).
type_accessor(object_ptr, _, Address, Value, Get, Put) :-
	!,
	Get = objects:get_object(Address, 0, Value),
	Put = objects:put_object(Value,Address,0).
type_accessor(struct_ptr(Type), Slot, Address, Value, Get, Put) :-
	!,
	type_accessor(address, Slot, Address, Contents, Get, Put),
	Value =.. [Type,Contents].
type_accessor(term, Slot, Address, Value, Access, (Clean,Add)) :-
	!,
	term_slot_maintenance(Slot, Address, Value, Access, Add, Clean).
type_accessor(Type, _, Address, Value, structs:Get, structs:Put) :-
	primitive_type_accessor_names(Type, GetF, PutF),
	Get =.. [GetF,Address,0,Value],
	Put =.. [PutF,Address,0,Value].

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


%  term_slot_maintenance(+Slot, +Address, +Value, -Access, -Add, -Clean)
%  Access is the goal to call to bind Value to the value of the term slot
%  Slot of the object whose address is Address.  Add is the goal to add
%  a new value to the slot, and Clean is the goal to clean out the old
%  value.  Typically, you must call Clean before calling Add.

term_slot_maintenance(Slot, Address, Value, class_term_slots:Accessor,
		      assert(class_term_slots:Accessor),
		      retractall(class_term_slots:Dummy)):-
	slot_name(Slot, Name),
	Accessor =.. [Name,Address,Value],
	Dummy =.. [Name,Address,_].

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

%  term_accessors(+Slots, +Slot, +Base, -Get, -Put)
%  Get and Put are goals to get and put the essence of a term class
%  whose essence has been distilled to Slots, a list of
%  term_slot(Name,Var,Type,Offset) terms.  Get and Put should each be a
%  conjunction of the goals necessary to get and put each Var, of type
%  Type, at Offset bytes from the beginning of the instance.  The
%  catch is that Address must be a single address term as returned by
%  accessor/6.  Address0 is the address term we're using so far.  Slot
%  is the name of the slot we're creating accessors for, and N is the
%  number of essence slots already generated for this real slot.

term_accessors([], _, _, true, true).
term_accessors([term_slot(Name,Var,Type,Offset)|Slots], Slot, Base,
		(Get0,Get), (Put0,Put)) :-
	subslot(Slot, Name, Subslot),
	Address is Base + Offset,
	type_accessor(Type, Subslot, Address, Var, Get0, Put0),
	term_accessors(Slots, Slot, Base, Get, Put).


subslot(Slot, Part, Subslot) :-
	(   atom(Slot) ->
		Subslot = [Slot,Part]
	;   append(Slot, [Part], Subslot)
	).




/*============================External============================

			  Debugging Messages
\label{Debugging Messages}

Ideally, sending a message to an object that doesn't understand that
message would elicit an existence exception.  Unfortunately, this
degree of robustness would cost a great deal in terms of efficiency.
Therefore, as a compromise, this package allows you to declare which
sections of code should be more careful in sending messages.

Any methods or ordinary Prolog clauses following a
++verbatim
	:- debug_message.
--verbatim
directive, up to the following
++verbatim
	:- nodebug_message.
--verbatim
or until the end of the file will be compiled so that any messages
they send will be sent "carefully."  That is, any messages sent by
this code will raise an exception describing the message sent and the
object receiving the message.  This debugging feature also will catch
attempts to send an unbound message or to send to an unbound object,
and similar errors.

================================================================*/

/*----------------------------Internal----------------------------

		    Undefined Handler For Messages

This handler will only catch attempts to send a message that isn't
defined for any class.  But we might as well print a useful message
when it happens.

----------------------------------------------------------------*/

:- multifile user:unknown_predicate_handler/3.

user:unknown_predicate_handler(Goal, class_message_clauses,
			       types:illarg(Err, ErrGoal, 2)) :-
	Goal =.. [Predicate,Object,_|Args],
	message_operator(Op),
	atom_concat(Op, Msgname, Predicate), !,
	Message =.. [Msgname|Args],
	ErrGoal =.. [Op,Object,Message],
	undefined_msg_exception(Op, Object, Message, Err).

:- mode message_operator(?). % [PM] 4.2.1 (Also) used as generator.
:- multifile message_operator/1.
message_operator(<-).
message_operator(>>).
message_operator(<<).


/***************************************************************

		  Finding Out About Defined Classes

****************************************************************/

%  current_class(*Class)
%  Class is the name of a currently defined class.

current_class(Class_name) :-
	(   nonvar(Class_name) ->
		% force determinism
		(   obj_decl:class_size(Class_name, _, _) ->
			true
		;   obj_decl:term_class(Class_name, _, _, _) ->
			true
		)
	;   obj_decl:class_size(Class_name, _, _)
	;   obj_decl:term_class(Class_name, _, _, _)
	).


%  class_superclass(+Class, *Superclass)
%  class_superclass(*Class, +Superclass)
%  class_superclass(*Class, *Superclass)
%  Class is an immediate subclass of Superclass.

class_superclass(Class, Superclass) :-
	(   nonvar(Class)->
		obj_decl:superclass(Class, Superclass, _)
	;   obj_decl:subclass(Superclass, Class)
	).


%  class_ancestor(+Descendant, *Ancestor)
%  class_ancestor(*Descendant, +Ancestor)
%  class_ancestor(*Descendant, *Ancestor)
%  class_ancestor(+Descendant, +Ancestor)
%  Ancestor is Descendant or an ancestor class of Descendant.  Note
%  that this will efficiently produce either ancestors or descendents,
%  or backtrack through all classes and all their descendants, or
%  deterministically check the relationship of two specified classes.
%  It will now only respond to a query "?- class_ancestor(X,Y)" with
%  ground answers.

class_ancestor(Descendant, Ancestor) :-
	(   nonvar(Ancestor) ->
		(   nonvar(Descendant) ->
			descendant_class1(Ancestor, Descendant),
			!
		;   descendant_class1(Ancestor, Descendant)
		)
	;   class_ancestor1(Descendant, Ancestor)
	).

class_ancestor1(Class, Class) :-
	current_class(Class).
class_ancestor1(Class0, Class) :-
	obj_decl:superclass(Class0, Class1, _),
	class_ancestor1(Class1, Class).

descendant_class1(Class, Class) :-
	current_class(Class).
descendant_class1(Class0, Class) :-
	obj_decl:subclass(Class0, Class1),
	descendant_class1(Class1, Class).


%  direct_message(*Class, *Op, *Name, *Arity)
%  message(*Class, *Op, *Name, *Arity)
%  Name/Arity is an Op message understood by instances of
%  Class.  Op is either '<-', '>>', or '<<', specifying the kind of message.
%  For direct_message/4, the message is directly defined for that
%  class; for message/4, it may be either directly defined or
%  inherited.

direct_message(Class, Op, Name, Arity) :-
	obj_decl:direct_message(Class, Name, Arity, Op).

:- message/4 is documented_as(direct_message/4).

message(Class, Op, Name, Arity) :-
	class_ancestor(Class, Ancestor),
	direct_message(Ancestor, Op, Name, Arity),
	% Filter out messages not actually inherited
	\+ obj_decl:uninherit_from(Class, Name, Arity, Op, Ancestor),
	% Filter out redundant solutions for a single message
	\+ (obj_decl:inherit_from(Class, Name, Arity, Op, Ancestor2),
	    Ancestor2 \== Ancestor).


/****************************Internal***************************

		   Runtime Support For obj_decls

****************************************************************/

/*============================Internal============================

		    Making Sure A Message Is Valid

================================================================*/

%  ensure_can_send(+Object, +Op, +Name, +Arity)
%  Raise an exception if Object cannot respond to message Name/Arity
%  of type Op, otherwise succeed quietly.  Does not actually send the
%  message.  Assumes that Op is bound to a valid message op and Name
%  and Arity are bound to an atom and integer, respectively.

ensure_can_send(Object, Op, Name, Arity) :-
	(   nonvar(Object),
	    % should be class_of(Object, Class), but functor/3 is faster
	    class_of(Object, Class),
	    message(Class, Op, Name, Arity) ->
		true
	;   Goal =.. [Op,Object,Msg],
	    % this isn't right, because we don't get message args.
	    functor(Msg, Name, Arity),
	    (   var(Object)
	    ->      illarg(var, Goal, 1)
	    ;   class_of(Object, _Class) ->
		    undefined_msg_exception(Op, Object, Msg, Err),
		    illarg(Err, Goal, 2)
	    ;       illarg(domain(term,object), Goal, 1)
	    )
	).


%  undefined_msg_exception(+Op, +Object, +Message, -Err)
%  Err is the error term appropriate for an attempt to send Message of
%  type Op to Object, which doesn't respond to that message.  We make
%  an attempt to find other defined methods that would be appropriate
%  to mention in the error message.

undefined_msg_exception(Op, Object, Message, Err) :-
	functor(Message, Name, Arity),
	class_of(Object, Class),
	_G =.. [Op,Object,Message],
	Err = existence(method(Class,Op),Name/Arity,M),
	(   setof(Other, other_message_spec(Class,Name,Arity,Op,Other),
		  Others) ->
		M = however_list_are_defined(Others)
	;   M = ''
	).

%  other_message_spec(+Class, +Name, +Arity, +Op, *Other)
%  Other is a message spec term that describes a message named
%  Name with an arity other than Arity of type other than Op that
%  instances of Class respond to.

other_message_spec(Class, Name, Arity, Op, Other) :-
	message(Class, Op0, Name, Arity0),
	( Op \== Op0 -> true ; Arity =\= Arity0 ),
	Other =.. [Op,Class,Name/Arity].


/*============================Internal============================

		   Accessing An Object Pointer Slot

================================================================*/

%  get_object(+Address, +Offset, -Value)
%  Value is the contents of the cell of memory at address
%  Address+Offset, which must be the address of an object, or 0.
%  If it is 0, then Value is the atom 'null', otherwise, we
%  look in the object to determine its class.

get_object(Address, Offset, Value) :-
	structs:get_integer(Address, Offset, Vaddr),
	(   Vaddr =:= 0 ->
		Value = null
	;   structs:get_functor(Vaddr, Functor), % [MC] 4.0.5
	    functor(Value, Functor, 1),
	    arg(1, Value, Vaddr)
	).

%  put_object(+Value, +Address, +Offset)
%  put_object_debug(+Value, +Address, +Offset, +Slot, +Class)
%  Put Value, an object, at Address+Offset.  put_object_debug/5 checks
%  that this is an instance of Class or a descendent of Class.  For
%  error-reporting purposes, this is the address of slot Slot.  If Value
%  is the atom 'null', put 0 at that address.

put_object(null, Address, Offset) :- !,
	structs:put_integer(Address, Offset, 0).
put_object(Value, Address, Offset) :-
	arg(1,Value,Vaddr),
	structs:put_integer(Address, Offset, Vaddr).


put_object_debug(null, Address, Offset, _, _) :-
	!,
	structs:put_integer(Address, Offset, 0).
put_object_debug(Value, Address, Offset, Slot, Class) :-
	functor(Value, Functor, 1),
	(   descendant_class1(Class, Functor) -> true
	;   illarg(domain(term,descendant_of(Class)), store_slot(Slot,Value), 2)
	),
	arg(1, Value, Vaddr),
	structs:put_integer(Address, Offset, Vaddr).



/****************************Internal***************************

			    Miscellaneous

****************************************************************/

%  same_args(+Count, +Start_a, +-Term_a, +Start_b, +-Term_b)
%  The Count arguments of Term_a starting at argument Start_a are the
%  same as the Count arguments of Term_b starting at argument Start_b.

same_args(Count, Start_a, Term_a, Start_b, Term_b) :-
	(   Count =:= 0 ->
		true
	;   arg(Start_a, Term_a, Value),
	    arg(Start_b, Term_b, Value),
	    Start_a1 is Start_a+1,
	    Start_b1 is Start_b+1,
	    Count1 is Count-1,
	    same_args(Count1, Start_a1, Term_a, Start_b1, Term_b)
	).

/*===========================Internal===========================

		      Unknown Predicate Handling

===============================================================*/

%  library(obj_decl) generates clauses for this predicate for
%  each term class, if any.

:- multifile 
        class_of_hook/2.

%  library(obj_decl) generates clauses for these as necessary.  We
%  need the declarations here to prevent undefined warnings when obj_decl
%  is not available at run-time.

:- multifile
	obj_decl:class_size/3,
	% obj_decl:class_file/2,
	obj_decl:term_class/4,
	obj_decl:subclass/2,
	obj_decl:superclass/3,
	obj_decl:direct_slot/5,
	obj_decl:direct_message/4,
	obj_decl:(instance_method)/4,
	% obj_decl:(class_method)/3,
	obj_decl:inherit_from/5,
	obj_decl:uninherit_from/5
	% , obj_decl:inline_method/7
        .

:- if(\+ current_prolog_flag(dialect, spider)).
%% [PM] 4.1.3 SPIDER thinks these are unreferenced (and it may be right).
:- multifile
        obj_decl:class_file/2,
        obj_decl:(class_method)/3,
        obj_decl:inline_method/7.
:- endif. % not spider

/*===========================Internal===========================

			    Error Messages

===============================================================*/

:- multifile 'SU_messages':typename/3.

'SU_messages':typename(create_spec) --> !,
	['object creation specification'-[]].
'SU_messages':typename(initialization_method) --> !,
	['initialization method'-[]].
'SU_messages':typename(object) --> !,
	['object'-[]].
'SU_messages':typename(valid_message) --> !,
	['valid message'-[]].
'SU_messages':typename(method(Class,Op)) --> !,
	['for ~q class, message ~w'-[Class,Op]].
'SU_messages':typename(instance_method(Class)) --> !,
	['instance method for ~q class'-[Class]].
'SU_messages':typename(descendant_of(Class)) --> !,
	['descendent of ~q class'-[Class]].


