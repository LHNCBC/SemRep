/* Copyright (c) 1995-2008, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : system.pl                                                       %
%   Purpose: Operating system utilities SP3 compatibility                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(system3, [
        now/1,
        datime/1,
        datime/2,
        environ/2,
        sleep/1,
        host_name/1,
        host_id/1,
        file_exists/1,
        file_exists/2,
        file_property/2,
        rename_file/2,
        delete_file/1,
        delete_file/2,
        make_directory/1,
        working_directory/2,
        directory_files/2,
        mktemp/2,
        tmpnam/1,
        system/0,
        system/1,
        system/2,
        shell/0,
        shell/1,
        shell/2,
        exec/3,
        popen/3,
        pid/1,
        kill/2,
        wait/2
        ]).

:- use_module(library(types), [
        must_be/4
        ]).

:- use_module(library(system), [
        now/1,
        datime/1,
        datime/2,
        environ/2,
        sleep/1
        ]).

:- use_module(library(file_systems), [
        rename_file/2,
        make_directory/1,
        delete_directory/2,
        directory_members_of_directory/2,
        file_members_of_directory/2
        ]).

:- use_module(library(sockets), [
        current_host/1
        ]).

:- use_module(library(lists), [
        keys_and_values/3
        ]).

:- use_module(library(process)).

host_name(HostName) :-
        current_host(HostName).

host_id(HostName) :-
        current_host(HostName).

file_exists(FileName) :-
        file_exists(FileName, []).

file_exists(FileName, Permissions) :-
        file_systems:file_exists(FileName, Permissions), !.
file_exists(FileName, Permissions) :-
        file_systems:directory_exists(FileName, Permissions), !.

file_property(FileName, Property) :-
        existing_file_or_directory(FileName, What),
        file_property(What, FileName, Property).

file_property(file, FileName, Property) :-
        file_property_equivalent(Property, FileName).
file_property(directory, FileName, Property) :-
        directory_property_equivalent(Property, FileName).

file_property_equivalent(type(Prop), _FileName) :-
        % This may be a lie. Not all non-directories are regular files.
        Prop = regular.
file_property_equivalent(size(Prop), FileName) :-
        file_systems:file_property(FileName, size_in_bytes, Prop).
file_property_equivalent(mod_time(Prop), FileName) :-
        file_systems:file_property(FileName, modify_timestamp, Prop).


directory_property_equivalent(type(Prop), _FileName) :-
        Prop = directory.
directory_property_equivalent(size(Prop), FileName) :-
        file_systems:directory_property(FileName, size_in_bytes, Prop).
directory_property_equivalent(mod_time(Prop), FileName) :-
        file_systems:directory_property(FileName, modify_timestamp, Prop).



%% delete_file(+FileName)
%%     Equivalent to delete_file(FileName,[recursive]).
delete_file(Filename) :-
        delete_file(Filename, [recursive]).

%% delete_file(+FileName,+Options)
%%     FileName is the name of an existing file or directory. Options is a list of options.
%%     Possible options are directory, recursive or ignore. If FileName is not a directory it is deleted; otherwise,
%%     if the option directory is specified but not recursive, the directory will be deleted if it is empty. If recursive
%%     is specified and FileName is a directory, the directory and all its subdirectories and files will be deleted.
%%     If the operation fails, an exception is raised unless the ignore option is specified.
delete_file(Filename, Options) :-
        existing_file_or_directory(Filename, What),
        delete_file(What, Filename, Options).

delete_file(directory, Filename, Options) :-
        memberchk(recursive, Options), !,
        delete_directory(Filename, [if_nonempty(delete)]).
delete_file(directory, Filename, _Options) :-
        delete_directory(Filename, [if_nonempty(ignore)]).
delete_file(file, Filename, _) :-
        file_systems:delete_file(Filename).
delete_file(neither, Filename, Options) :-
        nonmember(ignore, Options), !,
        % trigger error if non-existing.
        absolute_file_name(Filename, _, [access(exist),file_errors(error)]).
delete_file(neither, _Filename, _Options) :-
        % [PM] 4.3.6 quietly succeed if attempting to delelete non-existing file when 'ignore' is in Options.
        true.

working_directory(Old, New) :-
        prolog:'$unix_cd'(OldD, OldD),
        % [PM] 4.3.6 SP3 working_directory/2 does not include the terminating "/", so strip it.
        absolute_file_name(OldD, Old),
        prolog:'$unix_cd'(_, New).


directory_files(Directory, RelFiles) :-
        directory_members_of_directory(Directory, Set1),
        file_members_of_directory(Directory, Set2),
        append(Set1, Set2, KL),
        keys_and_values(KL, RelFiles, _).


existing_file_or_directory(Filename, What) :-
        (   absolute_file_name(Filename, _Abs, [access(exist),file_type(directory),file_errors(fail)]) ->
            What = directory
        ;   absolute_file_name(Filename, _Abs, [access(exist),file_errors(fail)]) ->
            What = file
        ;   What = neither
        ).

proc_call(Binary) :-
        process_create(Binary, [], [wait(_)]).

mktemp(Template, FileName) :-
        atom_codes(Template, FileCodes),
        basename(FileCodes, BaseCodes),
        atom_codes(BaseName, BaseCodes),
        open(temp(BaseName), write, S, [if_exists(generate_unique_name)]),
        current_stream(FileName, _, S),
        close(S).

basename(File, Base) :-
        basename(File, S, S, Base).

basename([], Base, [], Base).
basename([0'/|File], _, _, Base) :- !,
        basename(File, S, S, Base).
basename([C|File], S0, [C|S], Base) :- !,
        basename(File, S0, S, Base).

tmpnam(FileName) :-
        open(temp(sp), write, S, [if_exists(generate_unique_name)]),
        current_stream(FileName, _, S),
        close(S).

system :-
        interactive_shell_binary(Binary),
        proc_call(Binary).

% Note: This code is not secure. See the section Quoting and Security
% in the manual entry for the process library, for details.
system(Cmd) :-
        system(Cmd, 0).

% Note: This code is not secure. See the section Quoting and Security
% in the manual entry for the process library, for details.
system(Cmd, Status) :-
        system_exec(Cmd, [], exit(Status)).

shell :-
        system.

% Note: This code is not secure. See the section Quoting and Security
% in the manual entry for the process library, for details.
shell(Cmd) :-
        system(Cmd).

% Note: This code is not secure. See the section Quoting and Security
% in the manual entry for the process library, for details.
shell(Cmd, Status) :-
        system(Cmd, Status).

interactive_shell_binary(Binary) :-
        % [PM] 4.2.1+ Not clear whether platform_data makes more sense here.
        prolog_flag(os_data, os(windows,_,_)),
        !,
	environ('COMSPEC', Binary).
interactive_shell_binary(Binary) :- % UNIX
	environ('SHELL', Binary1), Binary1 \== '', !,
	Binary = Binary1.
interactive_shell_binary('/bin/sh').


%% [PM] 4.0.3+ The shell used by system and popen.
system_shell_binary(Binary, DashC) :-
        % [PM] 4.2.1+ Not clear whether platform_data makes more sense here.
        prolog_flag(os_data, os(windows,_,_)),
	DashC = '/C',
	environ('COMSPEC', Binary).
system_shell_binary(Binary, DashC) :- % UNIX
	environ('SHELL', Binary1), Binary1 \== '', !,
	DashC = '-c',
	Binary = Binary1.
system_shell_binary('/bin/sh', '-c').



exec(Cmd, [Stdin,Stdout,Stderr], Pid) :-
        shell_exec(Cmd, [stdin(Stdin),stdout(Stdout),stderr(Stderr),process(Process)]),
        %% [PM] 4.0.2+ unfortunately waiting on Pid instead of Process does not work on some UNIX platforms.
        %% xref prolog_process_wait@Emulator/file.c
        %%
        %% process_id(Process, Pid1), % return numeric process id
        %% process_release(Process),
        %% Pid = Pid1.
        Pid = Process.

% Note: This code is not secure. See the section Quoting and Security
% in the manual entry for the process library, for details.
popen(Cmd, Mode, Stream) :-
        Goal = popen(Cmd,Mode,Stream),
        must_be(Mode, oneof([read,write]), Goal, 2),
        (   Mode==read -> Opt = stdout(pipe(Stream))
        ;   Opt = stdin(pipe(Stream))
        ),
        system_exec(Cmd, [Opt]).

shell_exec(Cmd, Opts) :-
        process_create(Cmd, [], [commandline(true)|Opts]).

% [MC] dead
% shell_exec(Cmd, Opts, Status) :-
%         shell_exec(Cmd, [process(Process)|Opts]),
%         process_wait(Process, Status).

% Return the module context of the caller.
:- mode my_module(-).
:- meta_predicate
        my_module(:).
my_module(M:M).

%% [PM] 4.0.3+ We need to invoke the shell explicitly to ensure things
%% like system('dir > foo.txt') works (SPRM 10682)
%% Win32: this version, like SP3 and SP <= 4.0.2 will open a command
%%        window when running a command. This is an artifact of how
%%        CMD invokes sub-processes.
system_exec(Cmd, Opts) :-
	system_shell_binary(Shell, DashC),
        % [PM] 4.2.1 SPIDER correctly noticed that the non-meta argument Opts is passed
        % on as a meta argument to process:process_create/3. Unfortunately we can not make
        % system3:system_exec/2 into a meta predicate since that would affect backwards compatibility.
        % Instead, make the choice of module context explicit.
        my_module(M),
        process_create(Shell, [DashC, Cmd], M:Opts).

system_exec(Cmd, Opts, Status) :-
        system_exec(Cmd, [wait(Status)|Opts]).

pid(Pid) :-
        process_id(Pid).

kill(Pid, Sig) :-
        process_kill(Pid, Sig).

wait(Pid, Status) :-
        process_wait(Pid, exit(Status)).

