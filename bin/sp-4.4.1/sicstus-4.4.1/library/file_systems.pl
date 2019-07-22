/* -*- Mode:Prolog -*- */
%   Module : files
%   Author : Richard A. O'Keefe
%   Updated: 31 Jan 1994
%   Purpose: file-handling commands and predicates.
%   SeeAlso: files.c, library(directory)

%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

% [PM] extensive rewrite. Now uses builtin (SPIO) functionality.

:- module(file_systems, [
        make_directory/1,                       % [PM] 4.0 OK
        %% Later: make_directory/2,

%% FIXME FIXME: I ([PM]) have reinstated some of the access-control preds
%% [PM] 4.0 It is not possible to reliably and portably mimic the OS
%%      access control checks (e.g., due to UNIX ACLs, Win32
%%      permissions, other processes accessing the file system).
%%      See for instance the discussion in SUSv3/POSIX on why
%%      eaccess() is not part of the standard (and why access(2) does
%%      not help, in general).
%%
%%      For this reason all predicates that does access control
%%      (except simple existence checks) have been removed until
%%      further notice.
%%
%%	can_open_file/2,
%%	can_open_file/3,

	close_all_streams/0,                    % [PM] 4.0 OK

	delete_file/1,                          % [PM] 4.0 OK
	delete_directory/1,                     % [PM] 4.0 OK
	delete_directory/2,                     % [PM] 4.0 OK

	directory_exists/1,                     % [PM] 4.0 OK
	directory_exists/2,                     % [PM] 4.0 OK
	directory_must_exist/1,                 % [PM] 4.0 OK
	directory_must_exist/2,                 % [PM] 4.0 OK

	file_exists/1,                          % [PM] 4.0 OK
	file_exists/2,                          % [PM] 4.0 OK
	file_must_exist/1,                      % [PM] 4.0 OK
	file_must_exist/2,                       % [PM] 4.0 OK

%% [PM] 4.0 Gone. If open/[3,4] does not suffice it should be fixed,
%%      not replaced by a library predicate.
%%	open_file/3,

	rename_file/2,                          % [PM] 4.0 OK
	rename_directory/2,                     % [PM] 4.0 OK (new)
	current_directory/1,                    % [PM] 4.0 OK
	current_directory/2,                    % [PM] 4.0 OK
	directory_member_of_directory/2,        % [PM] 4.0 OK
	directory_member_of_directory/3,        % [PM] 4.0 OK
	directory_member_of_directory/4,        % [PM] 4.0 OK
	directory_members_of_directory/1,       % [PM] 4.0 OK
	directory_members_of_directory/2,       % [PM] 4.0 OK
	directory_members_of_directory/3,       % [PM] 4.0 OK
	directory_property/2,                   % [PM] 4.0 OK
	directory_property/3,                   % [PM] 4.0 OK
	file_member_of_directory/2,             % [PM] 4.0 OK
	file_member_of_directory/3,             % [PM] 4.0 OK
	file_member_of_directory/4,             % [PM] 4.0 OK
	file_members_of_directory/1,            % [PM] 4.0 OK
	file_members_of_directory/2,            % [PM] 4.0 OK
	file_members_of_directory/3,            % [PM] 4.0 OK
	file_property/2,                        % [PM] 4.0 OK
	file_property/3                         % [PM] 4.0 OK

	% library(filename) has NOT been merged in
   ]).

:- use_module(library(types), [
	illarg/3,
	illarg/4,
	must_be/4
   ]).
:- use_module(library(system), [
	datime/2
   ]).
:- use_module(library(lists), [
	reverse/2
   ]).


%@  @cindex files
%@  @cindex directories
%@  This module provides operations on files and directories, such as renaming,
%@  deleting, opening, checking permissions, accessing members of.
%@  
%@  The following principles have been observed:
%@  
%@  @itemize @bullet
%@  @item 
%@  An absolute distinction is drawn between files and directories.
%@  The set of operations
%@  one can usefully perform on a directory is different from the set
%@  one can perform on a file:  for example, having
%@  write permission to a directory allows the user to create new files in
%@  it, not to rewrite the entire directory!  If any routine in this package
%@  @c [PM] 4.0 Liar liar, this was never true. Now fixed so that file_exists is not true for directories
%@  tells you that a ``file'' exists, you can be sure that it means a
%@  @c [PM] 4.0 We do not distinguish between ``regular'' and other (non-directory-)files.
%@  @c ``regular'' file.
%@  file and not a directory (and vice versa for ``directory'' exists).
%@  @item 
%@  The directory scanning routines do not actually open the files they
%@  find.  Thus finer discriminations, such as that between
%@  source and object code, are not made.
%@  @item 
%@  @c [PM] 4.0 new
%@  All paths are expanded as if by @code{absolute_file_name/3}.

%@  @c [PM] 4.0 who cares
%@  @c @item 
%@  @c The predicate names
%@  @c are made up of complete English words in lowercase, separated 
%@  @c by underscores, with no abbreviations.
%@  @item 
%@  Every predicate acts like a genuine logical relation insofar as it
%@  possibly can.
%@  @item 
%@  If anything goes wrong, the 
%@  predicates
%@  raise an error exception.  Any time that a predicate
%@  fails quietly, it should mean ``this question is meaningful, but the
%@  answer is no''.
%@  @item 
%@  The directory scanning routines insist that the directory argument
%@  name a searchable directory.
% [PM] 4.1 SPRM 11591 Clarify behavior
%@  @item 

%@  On Unix-like systems, symbolic links are followed by default and
%@  symbolic links that can not be followed are treated as
%@  non-existing. This means @code{file_exists/1} will fail if passed
%@  such a ``broken'' link and that neither
%@  @code{file_members_of_directory/1} nor
%@  @code{directory_members_of_directory/1} et al.@: will return such
%@  a link.
%@  
%@  On Windows, symbolic links (and other @use{reparse points}) are
%@  @emph{not} followed when enumerating directory contents with
%@  @code{file_members_of_directory/1} nor
%@  @code{directory_members_of_directory/1} et al.@: and are not
%@  returned for these predicates.
%@  
%@  The behavior for symbolic links (and reparse points) may change on
%@  all platforms in the future to ensure a well defined and
%@  consistent behavior on all platforms.

%@  
%@  To see @emph{all} members of a directory you can use
%@  @code{absolute_file_name/3} with a @code{glob('*')} option.

%% %@ [PM] 4.0 this was never true (it always, and still, barfs if taking property of a non-existing object)
%% %@  But the ``property'' routines
%% %@  are to be read as ``there exists a thing of such a type with such
%% %@  a property'', and quietly fail if there is no such file or
%% %@  directory.

%@  @end itemize
%@  
%@  The ``property'' routines use the same simplistic access control
%@  model as that used by the @code{absolute_file_name/3}
%@  @code{access/1}-option. @xref{mpg-ref-absolute_file_name}, for details.
%@  
%@  Exported predicates:
%@  
%@  @table @code

%@  @item rename_file(@var{+OldName}, @var{+NewName})
%@  @PLXindex {rename_file/2 (file_systems)}
%% %@  @var{OldName} and @var{NewName} must be atoms which are well formed
%% %@  file names.
%@  @var{OldName} must identify an existing file, which will
%@  be renamed to @var{NewName}.  The details of just when this can be
%@  done are operating-system dependent.
%@  Typically it is only possible to rename within the same file system.

rename_file(OldName, NewName) :-
	Goal = rename_file(OldName, NewName),
        OptionsArgNo = 0,
        prolog:rename_file(OldName, NewName, [], Goal, OptionsArgNo).

% [PM] 4.0 new
%@  @item rename_directory(@var{+OldName}, @var{+NewName})
%@  @PLXindex {rename_directory/2 (file_systems)}
%@  @var{OldName} must identify an existing directory, which will
%@  be renamed to @var{NewName}.  The details of just when this can be
%@  done are operating-system dependent.
%@  Typically it is only possible to rename empty directories within
%@  the same file system.

rename_directory(OldName, NewName) :-
	Goal = rename_directory(OldName, NewName),
        OptionsArgNo = 0,
        prolog:rename_file(OldName, NewName, [directory(true)], Goal, OptionsArgNo).


%% [PM] 4.0 We really need delete_file/2 (system:delete_file/2 used to
%% take options directory,recursive,ignore, we should only support
%% 'ignore' for delete_file/2 but we should support 'recursive' and
%% 'ignore' for delete_directory/2 (but probably give these better
%% names, like force(true/false), recurse(true/false).
%%
%@  @item delete_file(@var{+OldName})
%@  @PLXindex {delete_file/1 (file_systems)}
%@  @var{OldName} must identify an existing file, which will be deleted.

delete_file(OldName) :-
	Goal = delete_file(OldName),
	do_delete_file(OldName, Goal).

do_delete_file(OldName, Goal) :-
        OptionsArgNo = 0, % [PM] 4.3 no options in Goal
        prolog:delete_file(OldName, [], Goal, OptionsArgNo).


%@  @item delete_directory(@var{+Directory})
%@  @itemx delete_directory(@var{+Directory}, @var{+Options})
%@  @PLXindex {delete_directory/[1,2] (file_systems)}
%@  @var{Directory} 
%@  must identify an existing directory, which will be deleted, if empty.
%@  @var{Options} should be a list of at most one term of the form:
%@  @table @code
%@  @item if_nonempty(@var{Value})
%@  @findex if_nonempty/1 (delete_directory/2 option)
%@  Defines what to do if the directory is nonempty.  One of:
%@  @table @code
%@  @item ignore
%@  @findex ignore (delete_directory/2 if_nonempty option value)
%@  The predicate simply succeeds, deleting nothing.
%@  @item fail
%@  @findex fail (delete_directory/2 if_nonempty option value)
%@  The predicate simply fails, deleting nothing.
%@  @item error
%@  @findex error (delete_directory/2 if_nonempty option value)
%@  The predicate raises a permisison error.
%@  @item delete
%@  @findex delete (delete_directory/2 if_nonempty option value)
%@  The predicate recursively deletes the directory and its contents.
%@  @end table
%@  @end table

delete_directory(Directory) :-
	Goal = delete_directory(Directory),
	do_delete_directory(Directory, error, Goal).

delete_directory(Directory, Options) :-
	Goal = delete_directory(Directory,Options),
	must_be(Options, proper_list, Goal, 2),
	(   foreach(O,Options),
	    fromto(error,Flag1,Flag2,IfNonempty),
	    param(Goal)
	do  delete_directory_option(O, Goal, Flag1, Flag2)
	),
	do_delete_directory(Directory, IfNonempty, Goal).

delete_directory_option(if_nonempty(Flag), _, _, Flag) :-
	nonvar(Flag),
	member(Flag, [delete,error,ignore,fail]), !.
delete_directory_option(Option, Goal, ArgNo, _) :-
	illarg(domain(term,delete_directory_option), Goal, ArgNo, Option).


do_delete_directory(Directory, IfNonempty, Goal) :-
        ( IfNonempty == error ->
           DFIfNonempty = error
        ; DFIfNonempty = fail
        ),
        OptionsArgNo = 0, % [PM] 4.3 no options in Goal
        ( prolog:delete_file(Directory, [directory(true), if_nonempty(DFIfNonempty)], Goal, OptionsArgNo) ->
           true
        ;  do_delete_directory_nonempty(IfNonempty, Directory, Goal)             % directory was not empty
        ).


do_delete_directory_nonempty(ignore, _, _).
do_delete_directory_nonempty(delete, Directory, Goal) :-
	file_members_of_directory(Directory, FilePairs),
	(   foreach(_-File,FilePairs),
	    param(Goal)
	do  do_delete_file(File, Goal)
	),
        directory_members_of_directory1(Directory, '*', DirectoryPairs, Goal),
	(   foreach(_-Subdir,DirectoryPairs),
	    param(Goal)
	do  do_delete_directory(Subdir, delete, Goal)
	),
	do_delete_directory(Directory, error, Goal).

%@  @item directory_exists(@var{+Directory})
%@  @itemx directory_exists(@var{+Directory}, @var{+Mode})
%@  @PLXindex {directory_exists/1 (file_systems)}
%@  @PLXindex {directory_exists/2 (file_systems)}
%@  is true when @var{Directory} is an existing directory that is accessible
%@  according to @var{Mode}. @var{Mode} defaults to @code{exist}.
%@  
%@  This is more or less equivalent to
%@  @code{absolute_file_name(@var{File}, _, [file_type(directory),access([exist|@var{Mode}]),file_errors(fail)])}.

directory_exists(File) :-
        Goal = directory_exists(File),
        Mode = exist,
        prolog:absolute_file_name(File, _AFile, [file_type(directory),access(Mode),file_errors(fail)], Goal, 1).

directory_exists(File, Mode) :-
        Goal = directory_exists(File, Mode),
        add_exist_mode(Mode, Mode1),
        prolog:absolute_file_name(File, _AFile, [file_type(directory),access(Mode1),file_errors(fail)], Goal, 1).

%@  @item make_directory(@var{+Directory})
%@  @c @itemx make_directory(@var{+Directory}, @var{+Options})
%@  @PLXindex {make_directory/1 (file_systems)}
%@  @c @PLXindex {make_directory/2 (file_systems)}
%@  @var{Directory} is expanded, as if by @code{absolute_file_name/3},
%@  and the resulting directory is created.

make_directory(Directory) :-
        Goal = make_directory(Directory),
        OptionsArgNo = 0,
        prolog:create_directory(Directory, [], Goal, OptionsArgNo).

:- if(false). % not yet exported
make_directory(Directory, Options) :-
        Goal = make_directory(Directory, Options),
        OptionsArgNo = 2,
        prolog:create_directory(Directory, Options, Goal, OptionsArgNo).
:- endif.

%@  @item file_exists(@var{+File})
%@  @itemx file_exists(@var{+File}, @var{+Mode})
%@  @PLXindex {file_exists/1 (file_systems)}
%@  @PLXindex {file_exists/2 (file_systems)}
%@  is true when @var{File} is an existing file that is accessible
%@  according to @var{Mode}. @var{Mode} defaults to @code{exist}.
%@  
%@  This is more or less equivalent to
%@  @code{absolute_file_name(@var{File}, _, [access([exist|@var{Mode}]),file_errors(fail)])}.

%% [PM] 4.0 In contrast to the original code this will only succeed if
%%          the file is a non-directory.
%% xref the claim above:
%% "If any routine in this package tells you that a ``file'' exists,
%%  you can be sure that it means a ``regular'' file."

file_exists(File) :-
        Goal = file_exists(File),
        Mode = exist,
        prolog:absolute_file_name(File, _AFile, [access(Mode),file_errors(fail)], Goal, 1).

file_exists(File, Mode) :-
        Goal = file_exists(File, Mode),
        add_exist_mode(Mode, Mode1),
        prolog:absolute_file_name(File, _AFile, [access(Mode1),file_errors(fail)], Goal, 1).


add_exist_mode(Mode, ExistMode) :- var(Mode), !,
        ExistMode = Mode.                       % let absolute_file_name catch error
add_exist_mode([], ExistMode) :- !,
        ExistMode = [exist].
add_exist_mode(Mode, ExistMode) :- Mode = [_|_], !,
        ExistMode = [exist|Mode].
add_exist_mode(Mode, [exist, Mode]).


%@  @item file_must_exist(@var{+File})
%@  @itemx file_must_exist(@var{+File}, @var{+Mode})
%@  @PLXindex {file_must_exist/1 (file_systems)}
%@  @PLXindex {file_must_exist/2 (file_systems)}
%@  is like @code{file_exists(@var{File}[, @var{Mode}])} except that if the file is @emph{not}
%@  accessible it reports an error.
%@  
%@  This is more or less equivalent to
%@  @code{absolute_file_name(@var{File}, _, [access([exist|@var{Mode}]),file_errors(error)])}.
file_must_exist(File) :-
        Goal = file_must_exists(File),
        Mode = exist,
        prolog:absolute_file_name(File, _AFile, [access(Mode),file_errors(error)], Goal, 1).

file_must_exist(File, Mode) :-
        Goal = file_must_exists(File, Mode),
        add_exist_mode(Mode, Mode1),
        prolog:absolute_file_name(File, _AFile, [access(Mode1),file_errors(error)], Goal, 1).

% [MC] dead
% file_must_exist1(File, Goal) :-
%         Mode = exist,
%         prolog:absolute_file_name(File, _AFile, [access(Mode),file_errors(error)], Goal, 1).

%@  @item directory_must_exist(@var{+File})
%@  @itemx directory_must_exist(@var{+File}, @var{+Mode})
%@  @PLXindex {directory_must_exist/1 (file_systems)}
%@  @PLXindex {directory_must_exist/2 (file_systems)}
%@  is like @code{file_must_exists(@var{File}[, @var{Mode}])}, but for directories.
%@  
%@  This is more or less equivalent to
%@  @code{absolute_file_name(@var{File}, _, [file_type(directory),access([exists|@var{Mode}]),file_errors(error)])}.

directory_must_exist(File) :-
        Goal = directory_must_exist(File),
        Mode = exist,
        prolog:absolute_file_name(File, _AFile, [file_type(directory),access(Mode),file_errors(error)], Goal, 1).

directory_must_exist(File, Mode) :-
        Goal = directory_must_exist(File),
        add_exist_mode(Mode, Mode1),
        prolog:absolute_file_name(File, _AFile, [file_type(directory),access(Mode1),file_errors(error)], Goal, 1).

% [MC] dead
% directory_must_exist1(File, Goal) :-
%         Mode = exists,
%         prolog:absolute_file_name(File, _AFile, [file_type(directory),access(Mode),file_errors(error)], Goal, 1).


%% %@  @item can_open_file(@var{+FileName}, @var{+Mode})
%% %@  @itemx can_open_file(@var{+FileName}, @var{+Mode}, @var{+Quiet})
%% %@  @PLXindex {can_open_file/[2,3] (file_systems)}
%% %@  checks whether it is possible to open the file called @var{FileName} in @var{Mode}.
%% %@  @var{Mode} is one of:
%% %@  @table @code
%% %@  @item read
%% %@  @var{FileName} must exist and be readable
%% %@  @item write
%% %@  @itemx append
%% %@  @itemx backup
%% %@  @var{FileName} must exist and be writable
%% %@  or else be non-existent in a writable directory
%% %@  @end table
%% %@  
%% %@  The possible values for @var{Quiet} are:
%% %@  @table @code
%% %@  @item fail
%% %@  just quietly fail if the file is not openable (the default)
%% %@  @item warn
%% %@  raise an error if the file won't open.
%% %@  @end table
%% %@  
%% %@  @c All things considered, the simplest way to do this is to just try to
%% %@  @c open the file.  This has a defect: it might indeed be possible to open
%% %@  @c the file, but if too many files are open already it won't work.  But
%% %@  @c the error message for that is intelligible, and if we really want to
%% %@  @c open the file that is something we'd like to know about.
%% %@  
%% %@  @c @code{open_file(@var{File}, @var{Mode}, @var{Stream})} does the same checks, and goes ahead
%% %@  @c and does open the file.  This does better error reporting than the
%% %@  @c existing @code{open/3} (which it uses).
%% %@  
%% %@  @c The "standard" modes are
%% %@  @c     read	- input
%% %@  @c     write	- output, creates a new file
%% %@  @c     append	- output, extends an existing file or creates a new one
%% %@  @c These two predicates understand a fourth mode
%% %@  @c     backup	- output, any existing file F is renamed to F.BAK
%% %@  @c There is no way of overwriting an existing file.
%% 
%% 
%% 
%% %@  @item open_file(@var{+FileName}, @var{+Mode}, @var{-Stream})
%% %@  @PLXindex {open_file/3 (file_systems)}
%% %@  Opens the file called @var{FileName} in @var{Mode}, giving @var{Stream}.
%% %@  @var{Mode} is one of:
%% %@  @table @code
%% %@  @item read
%% %@  @var{FileName} is opened for reading.
%% %@  @item write
%% %@  @var{FileName} is opened for writing, deleting any pre-existing file.
%% %@  @item append
%% %@  @var{FileName} is opened for writing, appending to any pre-existing file.
%% %@  @item backup
%% %@  @var{FileName} is opened for writing, renaming any pre-existing file
%% %@  to @var{FileName}.BAK.
%% %@  @end table



%@  @item close_all_streams 
%@  @PLXindex {close_all_streams/0 (file_systems)}
%@  closes all the streams (other than the standard streams)
%@  which are currently open.  The time to call this is after
%@  an @code{abort/0}.  Note that @code{current_stream/3} does not notice the standard
%@  streams.

close_all_streams :-
	current_stream(_, _, Stream),
	close(Stream),
	fail
    ;	true.

/* Extensions to SU_messages to enable proper printing of error messages */

:- multifile 'SU_messages':operation/3.
:- multifile 'SU_messages':typename/3.

%% [PM] 4.0 used with permission_error
'SU_messages':operation(delete) --> !, [delete-[]].
'SU_messages':operation(rename) --> !, [rename-[]].
'SU_messages':operation(access) --> !, [access-[]].

'SU_messages':typename('access mode') --> !, ['access mode'-[]].
'SU_messages':typename(property) --> !, ['file or directory property'-[]].

%   Module : directory
%   Author : Richard A. O'Keefe
%   Updated: 18 Aug 1994
%   Defines: directory scanning routines
%   SeeAlso: directory.c, unix.{c,pl}

%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

/*  file_member_of_directory([Directory, [Pattern, ]]Name, Full)
    enumerates file Names in the given Directory which match the
    given Pattern.  The Directory defaults to '.', the current
    directory.  The Pattern defaults to '*', which matches anything.
    It is impossible to supply a Pattern without also supplying a
    Directory.  The Name argument will be unified with a simple
    file name, and the Full argument will be unified with a full
    path name, including the Directory and the Name both.  As a
    special case, a leading ./ will be stripped.  Note that Full
    will be absolute if Directory is absolute, relative if relative.
    This routine only matches visible *file* names.  Even names
    which start with a dot are considered to be visible.

    directory_member_of_directory([Directory, [Pattern, ]]Name, Full)
    is exactly like file_member_of_directory except that it only
    matches visible *directory* names.  Even names which start with
    a dot a considered to be visible, but as a special case to stop
    looping, the names '.' and '..' are NOT matched.
*/

:- directory_member_of_directory(?,?) is nondet.
%@  @item directory_member_of_directory(@var{?BaseName}, @var{?FullName})
%@  @PLXindex {directory_member_of_directory/2 (file_systems)}
%@  is true when @var{BaseName} is the name of a subdirectory of the current
%@  directory (other than '.' or '..') and @var{FullName} is its absolute name.
%@  
%@  This uses @code{absolute_file_name/3} with the @code{glob/1} option.


directory_member_of_directory(BaseName, FullName) :-
        Goal = directory_member_of_directory(BaseName, FullName),
        directory_member_of_directory1('.', '*', BaseName, FullName, Goal).


:- directory_member_of_directory(+,?,?) is nondet.
%@  @item directory_member_of_directory(@var{+Directory}, @var{?BaseName}, @var{?FullName})
%@  @PLXindex {directory_member_of_directory/3 (file_systems)}
%@  is true when @var{Directory} is a name (not necessarily the absolute name)
%@  of a directory, @var{BaseName} is the name of a subdirectory of that
%@  directory (other than '.' or '..') and @var{FullName} is its absolute name.
%@  
%@  This uses @code{absolute_file_name/3} with the @code{glob/1} option.

directory_member_of_directory(Directory, BaseName, FullName) :-
        Goal = directory_member_of_directory(Directory, BaseName, FullName),
        directory_member_of_directory1(Directory, '*', BaseName, FullName, Goal).


:- directory_member_of_directory(+,+,?,?) is nondet.
%@  @item directory_member_of_directory(@var{+Directory}, @var{+Pattern}, @var{?BaseName}, @var{?FullName})
%@  @PLXindex {directory_member_of_directory/4 (file_systems)}
%@  is true when @var{Directory} is a name (not necessarily the absolute name)
%@  of a directory, @var{BaseName} is the name of a directory of that
%@  directory (other than '.' or '..') which matches the given @var{Pattern},
%@  and @var{FullName} is the absolute name of the subdirectory.
%@  
%@  This uses @code{absolute_file_name/3} with a @code{glob(@var{Pattern})} option.

directory_member_of_directory(Directory, Pattern, BaseName, FullName) :-
        Goal = directory_member_of_directory(Directory, Pattern, BaseName, FullName),
        directory_member_of_directory1(Directory, Pattern, BaseName, FullName, Goal).


:- directory_member_of_directory1(+, +, ?, ?, +) is nondet.
%% [PM] 4.0
directory_member_of_directory1(Directory, Pattern, BaseName, FullName, Goal) :-
        prolog:absolute_file_name(Directory, FullName, [solutions(all), glob(Pattern),file_errors(fail),file_type(directory)], Goal, 1),
        directory_basename(FullName, BaseName).

%% [PM] 4.0
file_basename(File, BaseName) :-
        atom_codes(File, FullNameCodes),
        strrch(FullNameCodes, 0'/ , [_Slash|BaseNameCodes]),
        atom_codes(BaseName, BaseNameCodes).

%% Or would it be more useful if Suffix is output?
%%%% [PM] 4.0 
%%file_basename(File, BaseName, StripSuffix) :-
%%        atom_codes(File, FullNameCodes),
%%        atom_codes(StripSuffix, SuffixCodes),
%%        strrch(FullNameCodes, 0'/ , [_Slash|BaseNameCodes1]),
%%        ( append(BaseNameCodes, SuffixCodes, BaseNameCodes1) ->
%%           true
%%        ; BaseNameCodes = BaseNameCodes1
%%        ),
%%        atom_codes(BaseName, BaseNameCodes).

%% [PM] 4.0
directory_basename(Directory, BaseName) :-
        atom_codes(Directory, FullNameCodes0),
        %% need to remove the terminating slash implied by file_type(directory).
        reverse(FullNameCodes0, RFullNameCodes0),
        ( RFullNameCodes0 = [0'/ |RFullNameCodes] ->
           reverse(RFullNameCodes, FullNameCodes)
        ; FullNameCodes = FullNameCodes0
        ),
        strrch(FullNameCodes, 0'/ , [_Slash|BaseNameCodes]),
        atom_codes(BaseName, BaseNameCodes).

%% [PM] 4.0 Find the last suffix [G|_] of the ground Xs
strrch(Xs, G, Ys) :- Xs = [X|Xs1],
        ( X == G ->
           strrch1(Xs1, G, Xs, Ys)
        ; strrch(Xs1, G, Ys)
        ).

strrch1(Xs, _G, _Prev, _Ys) :- var(Xs), !, fail.
strrch1([], _G, Prev, Ys) :-
        Ys = Prev.
strrch1(Xs, G, Prev, Ys) :- Xs = [X|Xs1],
        ( X == G ->
           strrch1(Xs1, G, Xs, Ys)
        ; strrch1(Xs1, G, Prev, Ys)
        ).


%@  @item directory_members_of_directory(@var{-Set})
%@  @PLXindex {directory_members_of_directory/[1,2,3] (file_systems)}
%@  is true when @var{Set} is a set of @var{BaseName-FullName} pairs being the
%@  relative and absolute names of subdirectories of the current directory.
%@  
%@  This uses @code{absolute_file_name/3} with the @code{glob/1} option.

directory_members_of_directory(Set) :-
        Goal = directory_members_of_directory(Set),
        directory_members_of_directory1('.', '*', Set, Goal).



%@  @item directory_members_of_directory(@var{+Directory}, @var{-Set})
%@  is true when @var{Set} is a set of @var{BaseName-FullName} pairs being the
%@  relative and absolute names of subdirectories of the given @var{Directory}.
%@  @var{Directory} need not be absolute; the @var{FullNames} will be regardless.
%@  
%@  This uses @code{absolute_file_name/3} with the @code{glob/1} option.

directory_members_of_directory(Directory, Set) :-
        Goal = directory_members_of_directory(Directory, Set),
        directory_members_of_directory1(Directory, '*', Set, Goal).


%@  @item directory_members_of_directory(@var{+Directory}, @var{+Pattern}, @var{-Set})
%@  is true when @var{Set} is a set of @var{BaseName-FullName} pairs being the
%@  relative and absolute names of subdirectories of the given @var{Directory},
%@  such that each @var{BaseName} matches the given Pattern.
%@  
%@  This uses @code{absolute_file_name/3} with a @code{glob(@var{Pattern})} option.

directory_members_of_directory(Directory, Pattern, Set) :-
        Goal = directory_members_of_directory(Directory, Pattern, Set),
        directory_members_of_directory1(Directory, Pattern, Set, Goal).


directory_members_of_directory1(Directory, Pattern, Set, Goal) :-
        findall(BaseName-FullName, directory_member_of_directory1(Directory, Pattern, BaseName, FullName, Goal), List),
        sort(List, Set).


:- file_member_of_directory(?, ?) is nondet.
%@  @item file_member_of_directory(@var{?BaseName}, @var{?FullName})
%@  @PLXindex {file_member_of_directory/[2,3,4] (file_systems)}
%@  is true when @var{BaseName} is the name of a file in the current
%@  directory and @var{FullName} is its absolute name.
%@  
%@  This uses @code{absolute_file_name/3} with the @code{glob/1} option.

file_member_of_directory(BaseName, FullName) :-
        Goal = file_member_of_directory(BaseName, FullName),
        file_member_of_directory1('.', '*', BaseName, FullName, Goal).


:- file_member_of_directory(+, ?, ?) is nondet.
%@  @item file_member_of_directory(@var{+Directory}, @var{?BaseName}, @var{?FullName})
%@  is true when @var{Directory} is a name (not necessarily the absolute name)
%@  of a directory, @var{BaseName} is the name of a file in that directory,
%@  and @var{FullName} is its absolute name.
%@  
%@  This uses @code{absolute_file_name/3} with the @code{glob/1} option.

file_member_of_directory(Directory, BaseName, FullName) :-
        Goal = file_member_of_directory(Directory, BaseName, FullName),
        file_member_of_directory1(Directory, '*', BaseName, FullName, Goal).


:- file_member_of_directory(+, +, ?, ?) is nondet.
%@  @item file_member_of_directory(@var{+Directory}, @var{+Pattern}, @var{?BaseName}, @var{?FullName})
%@  is true when @var{Directory} is a name (not necessarily the absolute name)
%@  of a directory, @var{BaseName} is the name of a file in that directory
%@  which matches the given @var{Pattern},
%@  and @var{FullName} is its absolute name.
%@  
%@  This uses @code{absolute_file_name/3} with a @code{glob(@var{Pattern})} option.

file_member_of_directory(Directory, Pattern, BaseName, FullName) :-
        Goal = file_member_of_directory(Directory, Pattern, BaseName, FullName),
        file_member_of_directory1(Directory, Pattern, BaseName, FullName, Goal).

:- file_member_of_directory1(+, +, ?, ?, +) is nondet.
%% [PM] 4.0 
file_member_of_directory1(Directory, Pattern, BaseName, FullName, Goal) :-
        prolog:absolute_file_name(Directory, FullName,
        [solutions(all), glob(Pattern),
         access(exist),                         % do not include directories
         file_errors(fail)], Goal, 1),
        file_basename(FullName, BaseName).


%@  @item file_members_of_directory(@var{-Set})
%@  @PLXindex {file_members_of_directory/[1,2,3] (file_systems)}
%@  is true when @var{Set} is a set of @var{BaseName-FullName} pairs being the
%@  relative and absolute names of the files in the current directory.
%@  
%@  This uses @code{absolute_file_name/3} with the @code{glob/1} option.

file_members_of_directory(Set) :-
        Goal = file_members_of_directory(Set),
        file_members_of_directory1('.', '*', Set, Goal).



%@  @item file_members_of_directory(@var{+Directory}, @var{-Set})
%@  is true when @var{Set} is a set of @var{BaseName-FullName} pairs being the
%@  relative and absolute names of the files in the given @var{Directory}.
%@  @var{Directory} need not be absolute; the @var{FullNames} will be regardless.
%@  
%@  This uses @code{absolute_file_name/3} with the @code{glob/1} option.

file_members_of_directory(Directory, Set) :-
        Goal = file_members_of_directory(Directory, Set),
        file_members_of_directory1(Directory, '*', Set, Goal).



%@  @item file_members_of_directory(@var{+Directory}, @var{+Pattern}, @var{-Set})
%@  is true when @var{Set} is a set of @var{BaseName-FullName} pairs being the
%@  relative and absolute names of subdirectories of the given @var{Directory},
%@  such that each @var{BaseName} matches the given @var{Pattern}.
%@  
%@  This uses @code{absolute_file_name/3} with a @code{glob(@var{Pattern})} option.

file_members_of_directory(Directory, Pattern, Set) :-
        Goal = file_members_of_directory(Directory, Pattern, Set),
        file_members_of_directory1(Directory, Pattern, Set, Goal).


file_members_of_directory1(Directory, Pattern, Set, Goal) :-
        findall(BaseName-FullName, file_member_of_directory1(Directory, Pattern, BaseName, FullName, Goal), List),
        sort(List, Set).

:- directory_property(+, ?) is nondet.
:- directory_property(+, +) is semidet.
%@  @item directory_property(@var{+Directory}, @var{?Property})
%@  @PLXindex {directory_property/[2,3] (file_systems)}
%@  is true when @var{Directory} is a name of a directory, and @var{Property} is
%@  a boolean property which that directory possesses, e.g.
%@  @example
%@  @group
%@      directory_property(., searchable).
%@  @end group
%@  @end example
%@  
%@  The current set of file and directory properties include:
%@  @table @code
%@  @item readable
%@  @itemx writable
%@  @itemx executable
%@  @itemx searchable
%@  
%@  Tries to determine whether the process has permission to read, write,
%@  execute (only for files) or search (only for directories) the
%@  file.
%@  
%@  @item size_in_bytes
%@  The size, in bytes, of the file. Not available for directories.
%@  
%@  @item create_timestamp
%@  @itemx modify_timestamp
%@  @itemx access_timestamp
%@  
%@  The time of creation, last modification or last access expressed as a
%@  timestamp.
%@  @cindex timestamp
%@  @cindex Epoch
%@  @cindex seconds since the Epoch
%@  @cindex UTC
%@  A @dfn{timestamp} is an integer expressing the time interval, in
%@  seconds, since the ``Epoch''. The @dfn{Epoch} is
%@  the time zero hours, zero minutes, zero seconds, on January 1, 1970
%@  Coordinated Universal Time (UTC).
%@  
%@  The timestamp is what should be used when comparing information
%@  between files since it is independent of locale issues like time zone and daylight
%@  savings time etc.
%@  
%@  @item create_localtime
%@  @itemx modify_localtime
%@  @itemx access_localtime
%@  
%@  The same as the corresponding @code{@dots{}_timestamp} values passed
%@  through @code{system:datime/2}, i.e.@: expressed as local time and
%@  split up in the components year, month, day, hour, minute, seconds.
%@  
%@  @item set_user_id
%@  @itemx set_group_id
%@  @itemx save_text
%@  
%@  True if the set-uid, set-group-id, save-text bits, respectively, are
%@  set for the file. Always false on Windows.
%@  
%@  @item who_can_read
%@  @itemx who_can_write
%@  @itemx who_can_execute
%@  @itemx who_can_search
%@  
%@  A list containing the subset of @code{[user,group,other]} for the
%@  process classes that can, respectively, read, write, execute (only for
%@  files) or search (only for directories.
%@  
%@  @item owner_user_id
%@  @itemx owner_group_id
%@  
%@  The id of the owner and group of the file. The id is an integer on
%@  UNIX and an atom (expressed as a string security identifier) on
%@  Windows.
%@  
%@  @item owner_user_name
%@  @itemx owner_group_group
%@  
%@  The atom containing the name of the files owner and group respectively. On Windows a name
%@  like @code{'@var{DOMAIN}\@var{NAME}'} will be used.
%@  
%@  If for some reason the name cannot be found it will fall back to using
%@  the same value as @code{owner_user_id} and @code{owner_group_id}.
%@  
%@  @end table
%@  
%@  Other properties may be added in the future. You can backtrack through
%@  the available properties by calling @code{file_property/3} or
%@  @code{directory_property/3} with an uninstantiated @var{Property}
%@  argument.
%@  

directory_property(Directory, Property) :-
    	Goal = directory_property(Directory, Property),
        file_property2(Directory, Property, directory, boolean, true, Goal).


:- directory_property(+, ?, ?) is nondet.
:- directory_property(+, +, -) is semidet.
%@  @item directory_property(@var{+Directory}, @var{?Property}, @var{?Value})
%@  is true when @var{Directory} is a name of a directory, @var{Property} is a
%@  property of directories, and @var{Value} is @var{Directory}'s @var{Property} @var{Value}.
%@  See @code{directory_property/2}, above, for a list of properties.

directory_property(Directory, Property, Value) :-
    	Goal = directory_property(Directory, Property, Value),
        file_property2(Directory, Property, directory, _, Value, Goal).




:- file_property(+, ?) is nondet.
:- file_property(+, +) is semidet.
%@  @item file_property(@var{+File}, @var{?Property})
%@  @PLXindex {file_property/[2,3] (file_systems)}
%@  is true when @var{File} is a name of a file, and @var{Property} is
%@  a boolean property which that file possesses, e.g.
%@  @example
%@  @group
%@      file_property('foo.txt', readable).
%@  @end group
%@  @end example
%@  See @code{directory_property/2}, above, for a list of properties.

file_property(File, Property) :-
        Goal = file_property(File, Property),
        file_property2(File, Property, file, boolean, true, Goal).


:- file_property(+, ?, ?) is nondet.
:- file_property(+, +, -) is semidet.
%@  @item file_property(@var{+File}, @var{?Property}, @var{?Value})
%@  is true when @var{File} is a name of a file, @var{Property} is a
%@  property of files, and @var{Value} is @var{File}'s @var{Property} @var{Value}.
%@  See @code{directory_property/2}, above, for a list of properties.

file_property(File, Property, Value) :-
        Goal = file_property(File, Property, Value),
        file_property2(File, Property, file, _, Value, Goal).


:- file_property2(+, ?, +, -, -, +) is nondet.
file_property2(Thing, Property, ThingType, PropertyType, Value, Goal) :-
        ( ThingType == directory ->
           prolog:absolute_file_name(Thing, AbsPath, [file_type(directory),access(exist),file_errors(error)], Goal, 1)
        ; ThingType == file ->
           prolog:absolute_file_name(Thing, AbsPath, [access(exist),file_errors(error)], Goal, 1)
        ; prolog:absolute_file_name(Thing, AbsPath, [file_type(directory),access(exist),file_errors(fail)], Goal, 1) -> % is it a directory?
           ThingType = directory
        ; prolog:absolute_file_name(Thing, AbsPath, [access(exist)], Goal, 1) -> % barf if it is not a file
           ThingType = file
        ),
	(   var(Property) ->
            file_property_adapter(Property, ThingType, PropertyType, Selector, Val, Decoder) % BT over properties
	;   file_property_adapter(Property, ThingType, PropertyType, Selector, Val, Decoder) -> true
	;   illarg(domain(term,property), Goal, 2)
	),
        prolog:file_property(AbsPath, Selector, Goal), % det
	'UNIX decode2'(Decoder, Val, Value).


/*****************************************************************************/
:- discontiguous file_property_adapter/6.
:- discontiguous 'UNIX decode2'/3.

%% [PM] 4.0 xref prolog:file_property_selector/3 in Bips/intrins2.pl
%%          In QP _localtime was called just _time but I want to make explicit what time-reference is used
%%          QP had _date which was like _time but only had the Year,Mont,Day information. Removed as pointless.
%%          QP had no way to get the raw UTC _timestamp values.
%%          QP had size_in_blocks and block_size but those are poorly defined in POSIX. Removed as pointless.
%%          QP had number_of_links and only_one_link. We could re-introduce these if requested.
%%
file_property_adapter(readable,	_,		boolean, can(What), What, bit(PROLOG_FILE_PROPERTY_VALUE_CAN_R)) :-
        PROLOG_FILE_PROPERTY_VALUE_CAN_R = 0x00000001.
file_property_adapter(writable,	_,		boolean, can(What), What, bit(PROLOG_FILE_PROPERTY_VALUE_CAN_W)) :-
        PROLOG_FILE_PROPERTY_VALUE_CAN_W = 0x00000002.
file_property_adapter(executable,	file,		boolean, can(What), What, bit(PROLOG_FILE_PROPERTY_VALUE_CAN_X)) :-
        PROLOG_FILE_PROPERTY_VALUE_CAN_X = 0x00000004.
file_property_adapter(searchable,	directory,	boolean, can(What), What, bit(PROLOG_FILE_PROPERTY_VALUE_CAN_SEARCH)) :-
        PROLOG_FILE_PROPERTY_VALUE_CAN_SEARCH = 0x00000008.
'UNIX decode2'(bit(Mask), Bits, Value) :-
        ( Bits /\ Mask =:= 0 -> Value = false; Value = true).


file_property_adapter(access_localtime,	_,		time, access_time(T), T, local_datime).
file_property_adapter(access_timestamp,	_,		time, access_time(T), T, timestamp).
file_property_adapter(modify_localtime,	_,		time, modify_time(T), T, local_datime).
file_property_adapter(modify_timestamp,	_,		time, modify_time(T), T, timestamp).
file_property_adapter(create_localtime,	_,		time, create_time(T), T, local_datime).
file_property_adapter(create_timestamp,	_,		time, create_time(T), T, timestamp).
'UNIX decode2'(local_datime, T, Value) :-
        datime(T, Value).
'UNIX decode2'(timestamp, T, Value) :-
        Value = T.

file_property_adapter(set_user_id,	 file,		boolean, mode(Mode), Mode, bit(PROLOG_FILE_PROPERTY_VALUE_S_ISUID)) :-
        PROLOG_FILE_PROPERTY_VALUE_S_ISUID = 0x00010000.
file_property_adapter(set_group_id,	 file,		boolean, mode(Mode), Mode, bit(PROLOG_FILE_PROPERTY_VALUE_S_ISGID)) :-
        PROLOG_FILE_PROPERTY_VALUE_S_ISGID = 0x00020000.
file_property_adapter(save_text,	 file,		boolean, mode(Mode), Mode, bit(PROLOG_FILE_PROPERTY_VALUE_S_ISVTX)) :-
        PROLOG_FILE_PROPERTY_VALUE_S_ISVTX = 0x00040000.

file_property_adapter(who_can_read,	 _,		who,	 mode(Mode), Mode, who(read)).
file_property_adapter(who_can_write,	 _,		who,	 mode(Mode), Mode, who(write)).
file_property_adapter(who_can_execute, file,		who,	 mode(Mode), Mode, who(execute)).
file_property_adapter(who_can_search,  directory,	who,	 mode(Mode), Mode, who(search)).
'UNIX decode2'(who(What), ModeBits, Value) :-
        who_bits(What, USR, GRP, OTH),
        ( ModeBits /\ USR =\= 0 -> Usr = [user  | Grp]; Usr = Grp ),
        ( ModeBits /\ GRP =\= 0 -> Grp = [group | Oth]; Grp = Oth ),
        ( ModeBits /\ OTH =\= 0 -> Oth = [other | [] ]; Oth = []  ),
        Value = Usr.

file_property_adapter(owner_user_id,	 _,		atomic, owner_user_id(UID), UID, asis).
file_property_adapter(owner_group_id,  _,		atomic, owner_group_id(GID), GID, asis).
'UNIX decode2'(asis, X, Value) :-
        Value = X.

file_property_adapter(owner_user_name, _,		user,	 owner_user_name(UNAME), UNAME, asis).

file_property_adapter(owner_group_name, _,		group,   owner_group_name(GNAME), GNAME, asis).
%% file_property_adapter(only_one_link,	 file,		boolean, number_of_links(NLinks), NLinks, =:=(1)).
'UNIX decode2'(=:=(Y), X, Value) :-
        ( X =:= Y -> Value = true; Value = false ).

%% 
%% file_property_adapter(number_of_links, file,		integer, number_of_links(NLinks), NLinks, asis).
file_property_adapter(size_in_bytes,   file,		integer, size(S), S, asis).


who_bits(read, PROLOG_FILE_PROPERTY_VALUE_R_USR, PROLOG_FILE_PROPERTY_VALUE_R_GRP, PROLOG_FILE_PROPERTY_VALUE_R_OTH) :-
        PROLOG_FILE_PROPERTY_VALUE_R_USR=0x00000010,
        PROLOG_FILE_PROPERTY_VALUE_R_GRP=0x00000100,
        PROLOG_FILE_PROPERTY_VALUE_R_OTH=0x00001000.
who_bits(write, PROLOG_FILE_PROPERTY_VALUE_W_USR, PROLOG_FILE_PROPERTY_VALUE_W_GRP, PROLOG_FILE_PROPERTY_VALUE_W_OTH) :-
        PROLOG_FILE_PROPERTY_VALUE_W_USR=0x00000020,
        PROLOG_FILE_PROPERTY_VALUE_W_GRP=0x00000200,
        PROLOG_FILE_PROPERTY_VALUE_W_OTH=0x00002000.
who_bits(execute, PROLOG_FILE_PROPERTY_VALUE_X_USR, PROLOG_FILE_PROPERTY_VALUE_X_GRP, PROLOG_FILE_PROPERTY_VALUE_X_OTH) :-
        PROLOG_FILE_PROPERTY_VALUE_X_USR=0x00000010,
        PROLOG_FILE_PROPERTY_VALUE_X_GRP=0x00000100,
        PROLOG_FILE_PROPERTY_VALUE_X_OTH=0x00001000.
who_bits(search, PROLOG_FILE_PROPERTY_VALUE_SEARCH_USR, PROLOG_FILE_PROPERTY_VALUE_SEARCH_GRP, PROLOG_FILE_PROPERTY_VALUE_SEARCH_OTH) :-
        PROLOG_FILE_PROPERTY_VALUE_SEARCH_USR=0x00000080,
        PROLOG_FILE_PROPERTY_VALUE_SEARCH_GRP=0x00000800,
        PROLOG_FILE_PROPERTY_VALUE_SEARCH_OTH=0x00008000.

/*****************************************************************************/

%@  @item current_directory(@var{-Directory})
%@  @itemx current_directory(@var{-Directory}, @var{+NewDirectory})
%@  @PLXindex {current_directory/[1,2] (file_systems)}
%@  @var{Directory} is unified with the current working directory and
%@  the working directory is set to @var{NewDirectory}.

current_directory(Dir) :-
	prolog:'$unix_cd'(Dir, Dir).

/* something like this would be sensitive to load contexts etc.
current_directory(OldDirectory, NewDirectory) :-
	Goal = current_directory(OldDirectory,NewDirectory),
	absolute_file_name(., OldDirectory), 
	prolog:absolute_file_name(NewDirectory, Abs, [], Goal, 2),
	(   prolog:'$unix_cd'(_, Abs) -> true
	;   illarg(existence(directory,Abs,0), Goal, 2, Abs)
	).
*/
% current_directory(OldDirectory, NewDirectory) :-
% 	prolog:'$unix_cd'(Old, Old),
% 	OldDirectory = Old,
%         Goal = current_directory(OldDirectory, NewDirectory),
% 	( OldDirectory == NewDirectory ->       % special case
%            %% [PM] 4.0 This special case ensures that a current
%            %% directory with 'funny' chars does not get expanded again
%            %% causing the directory to change
%            true
%         ; prolog:absolute_file_name(NewDirectory, NewDirectoryAbs, [file_type(directory),access(exist)], Goal, 2),
%           prolog:'$unix_cd'(_, NewDirectoryAbs) -> % do not expect this to fail if absolute_file_name succeeded
%             true
%         ; illarg(existence(directory,NewDirectory,0), Goal, 2)
% 	).
current_directory(OldDirectory, NewDirectory) :-
        Goal = current_directory(OldDirectory, NewDirectory),
        ArgNo = 2,                              % NewDirectory
        prolog:current_directory(OldDirectory, NewDirectory, Goal, ArgNo).


%@  @end table
