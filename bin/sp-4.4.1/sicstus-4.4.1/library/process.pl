/* -*- Mode:Prolog -*- */
/* Copyright(C) 2006, Swedish Institute of Computer Science */
%@  This package contains utilities for process creation.
%@  
%@  
%@  A process is represented by a @dfn{process reference}, a ground
%@  compound term. Both SICStus and the operating system maintain a
%@  state for each such process reference and they must therefore be
%@  released, either explicitly with @code{process_release/1} or
%@  implicitly by @code{process_wait/[2,3]}. Process references are
%@  created with @code{process_create/[2,3]} if explicitly requested with
%@  the @code{process/1} option. Process references are required in
%@  order to obtain the exit status of a process
%@  after @code{process_create/[2,3]} has returned.
%@  
%@  
%@  Many of the
%@  predicates can accept a numeric operating system process id
%@  (``PID'') but since process ids are subject to re-use by the OS
%@  this is less reliable and does not work if the process has already
%@  exited.
%@  
%@ 
%@  @subsection Examples
%@ 
%@  The following illustrates some common tasks.
%@  The process library is portable and works on all supported platforms,
%@  including UNIX, Linux and Windows.
%@  However, the examples are by necessity platform dependent.
%@  Unless otherwise noted, the examples will work on UNIX and similar systems only.
%@
%@  (If you are looking for something like the old SICStus 3 @code{system:system/1}
%@  and @code{system:popen/3}, @xref{unsafe_system}.)
%@  
%@  @enumerate
%@  @item
%@  Run the @command{date} command in the standard shell @samp{sh}.
%@  The output of the command is sent to the terminal:
%@  @example
%@  @group
%@  | ?- @kbd{process_create(path(sh),}
%@       @kbd{['-c', date]).}
%@  @end group
%@  @end example
%@
%@  @item
%@  Run the @command{date} command in the standard shell @samp{sh}.
%@  Wait for the command to terminate before returning to Prolog.
%@  Fail if the process gets an error.
%@  The output of the command is sent to the terminal:
%@  @example
%@  @group
%@  | ?- @kbd{process_create(path(sh),}
%@       @kbd{['-c', date], [wait(exit(0))]).}
%@  @end group
%@  @end example
%@  @noindent Using @code{wait/1} option in this way is a convenient way to
%@  ensure that the command has finished before Prolog continues.
%@  
%@  @item
%@  Run the @command{date} command in the standard shell @samp{sh}.
%@  The output of the command is received by Prolog:
%@  @example
%@  @group
%@  | ?- @kbd{process_create(path(sh),}
%@       @kbd{['-c', date], [stdout(pipe(S))]),}
%@       @kbd{read_line(S,L), close(S), atom_codes(Date,L).}
%@  @dots{},
%@  Date = 'Fri Jan 24 12:59:26 CET 2014' ? 
%@  @end group
%@  @end example
%@  
%@  @item
%@  Pipe the output of the @command{date} command to a file:
%@  @example
%@  @group
%@  | ?- @kbd{process_create(path(sh),}
%@       @kbd{['-c', [date, '>', file('/tmp/foo.txt')]]).}
%@  @end group
%@  @end example
%@  
%@  @item
%@  Count the number of words in an atom, using the @command{wc} command:
%@  @example
%@  @group
%@  | ?- @kbd{process_create(path(wc), ['-w'],}
%@       @kbd{[stdin(pipe(In)), stdout(pipe(Out))]),}
%@       @kbd{write(In, 'a b c\n'), close(In),}
%@       @kbd{read_line(Out, L), close(Out), number_codes(N, L).}
%@  @dots{}
%@  N = 3
%@  @end group
%@  @end example
%@  @noindent It may be preferable to let the input or output go via a file.
%@  This avoids deadlock in case the stream buffers fill up.
%@  
%@  @item
%@  Count the number of unique words in a file, piping the output of
%@  the @command{uniq} command to the @command{wc} command:
%@  @example
%@  @group
%@  | ?- @kbd{process_create(path(sh),}
%@       @kbd{['-c', ['uniq ', file('/tmp/foo.txt'), ' | wc -w']],}
%@       @kbd{[stdout(pipe(Out))]),}
%@       @kbd{read_line(Out, L), close(Out), number_codes(N, L).}
%@  @dots{}
%@  N = 6
%@  @end group
%@  @end example
%@  @noindent Note that quoting is a problem (and potential security issue),
%@  so @strong{never} pass untrusted data,
%@  like file names, to the shell using @option{-c} (@pxref{Quoting and Security}).
%@
%@  @item
%@  Run the @command{make} command with the @option{-n} (dry run) option,
%@  discarding output, fail if it does not succeed:
%@  @example
%@  @group
%@  | ?- @kbd{process_create(path(make), ['-n'],}
%@       @kbd{[stdout(null), wait(Exit)]),}
%@       @kbd{Exit = exit(0).}
%@  @end group
%@  @end example
%@  @noindent By using the @code{wait/1} option, @code{process_create/3}
%@  will not return until the subprocess has exited and its exit status is available.
%@
%@  @item
%@  Run @command{ls} on a home directory in a subshell using the user's preferred shell:
%@  @example
%@  @group
%@  | ?- @kbd{process_create('$SHELL', ['-c', [ls, ' ', file('~/') ]]).}
%@  @end group
%@  @end example
%@
%@  @item
%@  Run a command with output piped from a file and input provided by Prolog.
%@  This is similar to @code{popen('cat > ./myscript.sh',write,S)} in SICStus 3.
%@  This example also shows one way to create a shell script which is useful when
%@  more advanced shell interaction is needed.
%@  (The created script outputs the most common line in its input.
%@  It is used in the next example.)
%@  @example
%@  @group
%@  | ?- @kbd{process_create(path(sh),}
%@       @kbd{['-c',}
%@       @kbd{'cat > ./myscript.sh && chmod a+x ./myscript.sh'],}
%@       @kbd{[stdin(pipe(S))]),}
%@       @kbd{write(S, '#! /bin/sh\n'),}
%@       @kbd{write(S, 'sort | uniq -c | sort -nr | head -n 1\n'),}
%@       @kbd{close(S).}
%@  @end group
%@  @end example
%@  @noindent Please read @ref{Quoting and Security} for problems with this approach.
%@
%@  @item
%@  Run a shell script with input piped from a file and output read by Prolog.
%@  This is similar to @code{popen('./myscript.sh < ./somefile.txt',read,S)} in SICStus 3.
%@  @example
%@  @group
%@  | ?- @kbd{open('somefile.txt',write,OF),}
%@       @kbd{write(OF,'hello\nworld\nhello\nhello\n'),close(OF),}
%@       @kbd{process_create(path(sh),}
%@       @kbd{['-c', './myscript.sh < ./somefile.txt'],}
%#       @kbd{[stdout(pipe(S))]),}
%@       @kbd{read_line(S, L), atom_codes(Line, L), close(S).}
%@  @dots{},
%@  Line = '   3 hello' ? 
%@  @end group
%@  @end example
%@  @noindent Please read @ref{Quoting and Security} for problems with this approach.
%@
%@  @item
%@  Run a goal in a SICStus subprocess (UNIX and Windows):
%@  @example
%@  @group
%@  | ?- @kbd{process_create(application(sicstus),}
%@       @kbd{['-f', '--noinfo', '--nologo',}
%@       @kbd{'--goal', 'read(X), call(X), halt.'],}
%@       @kbd{[stdin(pipe(In)), stdout(pipe(Out))]),}
%@       @kbd{format(In,'~q .~n', [(length([h,e,l,l,o], Len),}
%@       @kbd{                      format('~q .~n', [Len]))]),}
%@       @kbd{close(In), read(Out,Answer), close(Out).}
%@  @dots{},
%@  Answer = 5
%@  @end group
%@  @end example
%@  
%@  @item
%@  Run @command{notepad.exe} on a file @file{C:/foo.txt} under Windows:
%@  @example
%@  @group
%@  | ?- @kbd{process_create('$SYSTEMROOT/notepad.exe',}
%@       @kbd{[file('C:/foo.txt')]).}
%@  @end group
%@  @end example

%@  @item
%@  Open a command shell in a separate window under Windows:
%@  @example
%@  @group
%@  | ?- @kbd{process_create('$COMSPEC',[],[window(true)]).}
%@  @end group
%@  @end example

%@  @end enumerate
%@
%@ @subsubsection Microsoft Windows Shell
%@
%@ On Windows, it is not possible to pass multiple parameters to a subprocess.
%@ When a subprocess is started, it receives exactly one argument and
%@ a quoting convention must be used to encode the parameters as the single argument
%@ actually passed to the process.
%@
%@ Unfortunately, there is no such universal quoting convention, every program can
%@ interpret its (single) argument in any way it sees fit.
%@
%@ Most programs use a convention established by the Microsoft C library.
%@ This is the convention used by @code{process_create/[2,3]} and it usually works well.
%@
%@ However, the command processor on Windows (@command{cmd.exe}) does not use the common convention and,
%@ except for very simple cases, passing arguments to @command{cmd.exe} will not work reliably.
%@
%@ @strong{Please note}: Passing arguments to @command{cmd.exe} suffers from the same
%@ security vulnerabilities as those described in @ref{Quoting and Security}, below.
%@ 
%@ If you want to run commands using @command{cmd.exe}, it is best to create a batch (@samp{.bat})
%@ file with your commands and then tell @command{cmd.exe} to run the batch file.
%@
%@  The following example illustrates how to create a Windows batch file that pipes some output to a file
%@  (@env{COMSPEC} is an environment variable containing the path to @command{cmd.exe}):
%@  @example
%@  @group
%@  | ?- @kbd{BatFileName='test.bat',}
%@       @kbd{open(BatFileName, write, S),}
%@       @kbd{write(S, 'date /T > "result.txt"\n'), close(S),}
%@       @kbd{process_create('$COMSPEC', ['/Q', '/C', file(BatFileName)],}
%@       @kbd{[wait(exit(0))]),}
%@       @kbd{open('result.txt', read, R),}
%@       @kbd{read_line(R,L),close(R),atom_codes(Date,L).}
%@  @dots{},
%@  Date = '2014-01-27 ',
%@  @dots{} ? 
%@  @end group
%@  @end example
%@
%@ More recent versions of Windows come with a redesigned command line processor, @samp{PowerShell},
%@ which solves the problems associated with the traditional @command{cmd.exe} command line processor.
%@ In particular, it has a very general way to encode command line arguments, using @samp{base-64} encoding.
%@ Currently, there is no direct support for PowerShell in this library, but the following example shows
%@ how to get the current week day both using a plain text command and with a base-64-encoded command
%@  
%@  @example
%@  @group
%@  | ?- @kbd{Command = '(get-date).DayOfWeek',}
%@       @kbd{process_create(path(powershell),}
%@       @kbd{['-Command', Command],}
%@       @kbd{[stdout(pipe(S))]),}
%@       @kbd{read_line(S,L),atom_codes(Day,L).}
%@  @dots{},
%@  Day = 'Monday',
%@  @dots{} ?
%@  @end group
%@  @end example
%@  
%@  @example
%@  @group
%@  | ?- @kbd{EncodedCommand =}
%@       @kbd{  'KABnAGUAdAAtAGQAYQB0AGUAKQAuAEQAYQB5AE8AZgBXAGUAZQBrAA==',}
%@       @kbd{process_create(path(powershell),}
%@       @kbd{['-encodedCommand', EncodedCommand],}
%@       @kbd{[stdout(pipe(S))]),}
%@       @kbd{read_line(S,L),atom_codes(Day,L).}
%@  @dots{},
%@  Day = 'Monday',
%@  @dots{} ? 
%@  @end group
%@  @end example
%@  @noindent where the @var{EncodedCommand} value was created by encoding the string
%@  @code{'(get-date).DayOfWeek'} using Base 64. See the PowerShell documentation for details.
%@ 
%@  @subsection Quoting and Security
%@  @anchor{Quoting and Security}
%@ 
%@  It easy to get undesired, and possibly harmful, effects if arbitrary data is passed without proper quoting to a shell.
%@  For instance, accepting arbitrary file names and passing them as part of a command line to a subshell can cause the shell
%@  to execute arbitrary, possibly malicious, code.
%@
%@  The following, vulnerable, predicates suffer from this problem.
%@  They are similar to predicates that existed in SICStus 3, and their fragility is
%@  one of the reasons process interaction was redesigned in SICStus 4.
%@  @anchor{unsafe_system}@anchor{unsafe_popen}
%@  @example
%@  @group
%@  % DO NOT USE. This code is vulnerable.
%@  % Similar to system:system/1 in SICStus 3.
%@  unsafe_system(Cmd) :-
%@     % pass Cmd to shell, wait for exit, fail on error.
%@     process_create(path(sh), ['-c', Cmd], [wait(exit(0))]).
%@  
%@  % DO NOT USE. This code is vulnerable.
%@  % Similar to system:popen/3 in SICStus 3.
%@  unsafe_popen(Cmd, Direction, Pipe) :-
%@     % pass Cmd to shell, do not wait for exit,
%@     % connect to stdin or stdout of subprocess.
%@     ( Direction == read ->
%@       process_create(path(sh), ['-c', Cmd], [stdout(pipe(Pipe))])
%@     ; Direction == write ->
%@       process_create(path(sh), ['-c', Cmd], [stdin(pipe(Pipe))])
%@     ).
%@  @end group
%@  @end example
%@
%@  Now consider the task of passing the contents of some file @var{File} to a command @command{mycommand}.
%@  You may think the following is a good idea (it is not!):
%@   
%@  @example
%@  @group
%@  % DO NOT USE. This code is vulnerable.
%@  unsafe_command(File, S) :-
%@     atom_concat('./mycommand < ', File, Cmd),
%@     unsafe_popen(Cmd, read, S).
%@  @end group
%@  @end example
%@
%@  That works as expected if the the @code{File} argument is a plain
%@  file with no characters that has special meaning to the shell, e.g.@:
%@  @example
%@  @group
%@  @kbd{File = './somefile.txt',}
%@  @kbd{unsafe_command(File, S), read_line(S,L),close(S).}
%@  @end group
%@  @end example
%@
%@  However, assume that the file name was obtained from some untrusted source and consider the following example:
%@  @example
%@  @group
%@  @kbd{File = '$(say bohoo)',}
%@  @kbd{unsafe_command(File, S), read_line(S,L),close(S).}
%@  @end group
%@  @end example
%@  @noindent depending on the system this can have a quite scary effect,
%@  and illustrates how shell meta characters in the constructed command line can lead to potentially dangerous results.
%@
%@  The safest way to interact with the shell is to create shell scripts and
%@  pass arguments to the scripts as separate arguments to the shell. E.g.@:
%@  @example
%@  @group
%@  % A safer version
%@  safer_command(File, S) :-
%@     % pass the file as the first argument to mycommand.
%@     process_create(path(sh),
%@                    ['-c', file('./mycommand'), file(File)],
%@                    [stdout(pipe(S))]).
%@  @end group
%@  @end example
%@ @c %@  The following, unsecure code, suffers from this problem
%@ @c   (it is similar to what could be done with @code{popen/3} in some earlier releases of SICStus):
%@ @c   @example
%@ @c   @group
%@ @c   % DO NOT USE. This code is insecure.
%@ @c   unsafe_popen(File, S) :-
%@ @c           atom_concat('./mycommand < ', File, Cmd),
%@ @c           process_create(path(sh), ['-c', Cmd], [stdout(pipe(S))]).
%@ @c   @end group
%@ @c   @end example
%@ @c 
%@ @c   This works as expected if the the @code{File} argument is a plain
%@ @c   file with no characters that has special meaning to the shell, e.g.@:
%@ @c   @example
%@ @c   @group
%@ @c   @kbd{File = './somefile.txt',}
%@ @c   @kbd{unsafe_popen(File, S), read_line(S,L),close(S).}
%@ @c   @end group
%@ @c   @end example
%@ @c 
%@ @c   However, assume the file name is obtained from some untrusted source, consider the following example:
%@ @c   @example
%@ @c   @group
%@ @c   @kbd{File = '$(say bohoo)',}
%@ @c   @kbd{unsafe_popen(File, S), read_line(S,L),close(S).}
%@ @c   @end group
%@ @c   @end example
%@ @c   @noindent depending on the system this can have a quite scary effect,
%@ @c   and illustrates how shell meta characters in the constructed command line can lead to potentially dangerous results.
%@ @c 
%@ @c   The safest way to interact with the shell is to create shell scripts and
%@ @c   pass arguments to the scripts as separate arguments to the shell. E.g.@:
%@ @c   @example
%@ @c   @group
%@ @c   % A safer version
%@ @c   safer_popen(File, S) :-
%@ @c           % pass the file as the first argument to mycommand.
%@ @c           process_create(path(sh), ['-c', ./mycommand, file(File)], [stdout(pipe(S))]).
%@ @c   @end group
%@ @c   @end example

:- module(process,
          [
           process_create/2,
           process_create/3,
           process_wait/2,
           process_wait/3,
           process_id/1,
           process_id/2,
           is_process/1,
           process_release/1,
           process_kill/1,
           process_kill/2
          ]).

:- meta_predicate process_create(+, +, :).
:- meta_predicate process_create1(+, +, :, +, +).

:- meta_predicate shell_create(+, :).
:- meta_predicate shell_create1(+, :, +, +).



%@  Exported predicates:
%@  
%@  @table @code

%@  @item process_create(@var{+File}, @var{+Args})
%@  @itemx process_create(@var{+File}, @var{+Args}, @var{:Options})
%@  @PLXindex {process_create/[2,3] (process)}

%@  Start a new process running the program identified by @var{File}
%@  and the arguments specified in @var{Args}. The standard streams of
%@  the new process can be redirected to prolog streams. The exit
%@  status of the process can be obtained with @code{process_wait/[2,3]}.

%@

%@  @var{File}, is expanded as if by @code{absolute_file_name/2}
%@  (with arguments @code{access(execute)} and @code{file_type(executable)}) and
%@  is used to locate the file to execute.
%@  

%@  The predefined file search path @code{path/1} (@pxref{ref-fdi})
%@  is especially useful here since it makes it easy to look up the
%@  names of an executable in the directories mentioned by the
%@  @code{PATH} environment variable. To run the Windows command shell
%@  @command{cmd} you would simply specify @code{path('cmd.exe')} (or @code{path(cmd)}), to
%@  start the UNIX Bash shell you would specify @code{path(bash)}.

%@

%@  @var{Args} is a list of argument specifications. Each argument
%@  specification is either a simple argument specification, see
%@  below, or a non-empty list of simple argument specifications. The
%@  expanded value of each element of @var{Args} is concatenated to
%@  produce a single argument to the new process. A @dfn{simple
%@  argument specification} can be one of:

%@

%@  @table @asis
%@  @item an atom
%@  The atom name is used as the expanded value. Some operating
%@  systems only support 7-bit ASCII characters here. Even when some
%@  larger subset of Unicode is used it may not work correctly
%@  with all programs.
%@
%@  @item @code{file(@var{File})}
%@  @var{File}, an atom, is treated as a file name and subject to
%@  an operating system specific transformation to ensure file name
%@  syntax and character set is appropriate for the new process. This
%@  is especially important under Windows where it ensures that the full
%@  Windows Unicode character set can be used.

%@  
%@  @strong{Please note}: The @var{File} part of
%@  @code{file(@var{File})} is not subject to syntactic rewriting, the
%@  argument specification @code{file/1} only adjusts for differences
%@  in file name syntax and character
%@  encoding between SICStus and the operating system. You
%@  must explicitly call
%@  @code{absolute_file_name/[2,3]} if you want to expand file search
%@  paths etc.
%@  

%@  @end table

%@  @var{Options} is a list of options:
%@

%@  @table @code
%@  @item stdin(@var{Spec})
%@  @itemx stdout(@var{Spec})
%@  @itemx stderr(@var{Spec})

%@  Each @var{Spec} specifies how the corresponding standard stream of
%@  the new process should be created. @var{Spec} can be one of:
%@  @table @code

%@  @item std
%@  The new process shares the (OS level) standard stream with the
%@  Prolog process. This is the default.
%@  Note that, especially under Windows, the Prolog process may not have
%@  any OS level standard streams, or the OS streams may not be
%@  connected to a console or terminal. In such a case you need to use
%@  @code{pipe/[1,2]} spec, see below, and explicitly read (write) data
%@  from (to) the process.

%@  @item null
%@  The stream is redirected to a null stream, i.e.@: a stream that
%@  discards written data and that is always at end of file when read.

%@  @item pipe(@var{Stream}) @since{release 4.0}
%@  @itemx pipe(@var{Stream}, @var{StreamOptions}) @since{release 4.3.2}
%@
%@  A new Prolog stream is created and connected to the corresponding stream
%@  of the new process. @var{StreamOptions} is a list of options affecting the
%@  created stream. The supported stream options are:
%@  @code{type/1}, @code{eol/1}, and @code{encoding/1},
%@  with the same meaning as for @code{open/4} (@pxref{mpg-ref-open}).
%@
%@  The default, if no stream options are specified, is to use a text stream with the OS default character encoding.
%@
%@  This stream must be closed using @code{close/[1,2]}, it is not
%@  closed automatically when the new process exits.
%@

%@  @end table
%@  
%@  @item wait(@var{-ExitStatus}) @since{release 4.3}
%@  The call will not return until the sub-process has terminated.
%@  @var{ExitStatus} will be bound to the exit status of the process,
%@  as described for @code{process_wait/2}.

%@  @item process(@var{Proc})
%@  @var{Proc} will be bound to a process reference that can be used
%@  in calls to @code{process_wait/[2,3]} etc.. This process reference
%@  must be released, either explicitly with @code{process_release/1} or
%@  implicitly by @code{process_wait/[2,3]}.
%@  It is often easier to use the @code{wait/1} option if you just want to
%@  wait for the process to terminate.
%@
%@  @item detached(@var{Bool})
%@  @var{Bool} is either @code{true} or @code{false}. Specifies
%@  whether the new process should be ``detached'', i.e.@: whether it
%@  should be notified of terminal events such as @kbd{^C}
%@  interrupts. By default a new process is created detached if none
%@  of the standard streams are specified, explicitly or implicitly,
%@  as @code{std}.
%@
%@  @item cwd(@var{CWD})
%@  
%@  @var{CWD} is expanded as if by @code{absolute_file_name/2} and
%@  is used as the working directory for the new process.
%@
%@  By default, the working directory is the same as the Prolog
%@  working directory.
%@  
%@  @item window(@var{Bool})
%@  @var{Bool} is either @code{true} or
%@  @code{false} (the default). Specifies whether the process should
%@  open in its own window.
%@  
%@  Specifying @code{window(true)} may give unexpected results if the
%@  standard stream options @code{stdin/1}, @code{stdout/1} and
%@  @code{stderr/1} are specified with anything but their default
%@  value @code{std}.
%@  
%@  Currently only implemented on Windows.
%@  
%@  @item environment(@var{Env}) @since{release 4.1}
%@  
%@  @var{Env} is a list of @code{@var{VAR}=@var{VALUE}} for extra
%@  environment variables to pass to the sub-process in addition to the
%@  default process environment.
%@  @var{VAR} should be an atom.
%@  @var{VALUE} should be an argument specification, as described above. The @var{VALUE}
%@  is typically an atom but, especially on the Windows platform, it may be necessary to
%@  wrap file names in @code{file/1} to ensure file paths are converted to the native format.
%@  @xref{System Properties and
%@  Environment Variables, System Properties and Environment
%@  Variables, System Properties and Environment Variables, sicstus,
%@  the SICStus Prolog Manual}, for more information.

%@  


%@  @c  @item daemon(@var{Bool})
%@  @c  not documented

%@  @end table

process_create(File, Args) :-
        Goal = process_create(File, Args),
        Options = [],
        OptionsArgNo = 0,
        process_create1(File, Args, Options, Goal, OptionsArgNo).


process_create(File, Args, Options) :-
        Goal = process_create(File, Args, Options),
        OptionsArgNo = 3,
        process_create1(File, Args, Options, Goal, OptionsArgNo).

process_create1(File, Args, Options, Goal, OptionsArgNo) :-
        prolog:process_create(File, Args, Options, Goal, OptionsArgNo).

shell_create1(Arg, Options, Goal, OptionsArgNo) :-
        prolog:shell_create(Arg, Options, Goal, OptionsArgNo).

% [PM] 4.3 Work in progress, fate not decided.
:- public shell_create/1.
shell_create(Arg) :-
        Goal = shell_create(Arg, Options),
        Options = [],
        OptionsArgNo = 0,
        shell_create1(Arg, Options, Goal, OptionsArgNo).

% [PM] 4.3 Work in progress, fate not decided.
:- public shell_create/2.
shell_create(Arg, Options) :-
        Goal = shell_create(Arg, Options),
        OptionsArgNo = 2,
        shell_create1(Arg, Options, Goal, OptionsArgNo).

%@  @item process_wait(@var{+Process}, @var{-ExitStatus})
%@  @itemx process_wait(@var{+Process}, @var{-ExitStatus}, @var{+Options})
%@  @PLXindex {process_wait/[2,3] (process)}

%@  Wait for a process to exit and obtain the exit status.
%@

%@  @var{Process} is either a process reference obtained from
%@  @code{process_create/3} or an OS process identifier. Specifying a
%@  process identifier is not reliable. The process identifier may
%@  have been re-used by the operating system. Under Windows, it is not
%@  possible to obtain the exit status using a process identifier if
%@  the process has already exited.
%@

%@  @var{ExitStatus} is one of:
%@  @table @code
%@  @item exit(@var{ExitCode})
%@  The process has exited with exit code @var{ExitCode}. By
%@  convention processes use exit code zero to signify success and a
%@  (positive) non-zero value to specify failure.

%@  @item killed(@var{SignalNumber})
%@  UNIX only, the process was killed by signal @code{SignalNumber} (a
%@  positive integer).
%@
%@  @item timeout
%@  The @code{timeout/1} option was specified and the process did not
%@  exit within the specified interval. In this case the process
%@  reference is not released, even if the @code{release/1} option is
%@  specified.
%@  @end table

%@  @var{Options} is a list of options:

%@  @table @code

%@  @item timeout(@var{Seconds})
%@  Specify a maximum time, in seconds, to wait for the process to
%@  terminate. @var{Seconds} should be an integer or floating point
%@  number or the atom @code{infinite} (the default) to specify 
%@  infinite wait. If the specified timeout interval passes before the
%@  process exits, @code{process_wait/3} exits with @var{ExitStatus}
%@  set to @code{timeout} and the process reference is not released.
%@
%@  Currently the UNIX implementation supports only timeout values
%@  0 (zero) and @code{infinite}.
%@

%@  @item release(@var{Bool})
%@  @var{Bool} is either @code{true} (the default) or
%@  @code{false}. Specifies whether the process reference should be
%@  released when @code{process_wait/3} exits successfully.

%@  @end table

process_wait(Process, ExitStatus) :-
        Goal = process_wait(Process, ExitStatus),
        Options = [],
        OptionsArgNo = 0,
        prolog:process_wait(Process, ExitStatus, Options, Goal, OptionsArgNo).

process_wait(Process, ExitStatus, Options) :-
        Goal = process_wait(Process, ExitStatus, Options),
        OptionsArgNo = 3,
        prolog:process_wait(Process, ExitStatus, Options, Goal, OptionsArgNo).

%@  @item process_id(@var{-PID})
%@  @PLXindex {process_id/1 (process)}

%@  Obtain the process identifier of the current (i.e.@: Prolog)
%@  process.

process_id(PID) :-
        prolog:process_self_id(PID).

%@  @item process_id(@var{+Process}, @var{-PID})
%@  @PLXindex {process_id/2 (process)}
%@  Obtain the process identifier of the process reference
%@  @var{Process}.

process_id(Process, PID) :-
        Goal = process_id(Process, PID),
        prolog:process_id(Process, PID, Goal).

%@  @item is_process(@var{+Thing})
%@  @PLXindex {is_process/1 (process)}
%@  Returns true if @var{Thing} is a process reference that has not
%@  been released.
is_process(Thing) :-
	Goal = is_process(Thing),
        prolog:is_process(Thing, Goal).

%@  @item process_release(@var{+Process})
%@  @PLXindex {process_release/1 (process)}
%@  Release a process reference @var{Process} that has previously been
%@  obtained from @code{process_create/3}. This ensures that Prolog
%@  and the operating system can reclaim any resources associated with
%@  the process reference.
%@
%@  Usually you would not call this. Either do not request the process
%@  reference when calling @code{process_create/3} or let
%@  @code{process_wait/[2,3]} reclaim the process reference when the
%@  process terminates.

process_release(Process) :-
        Goal = process_release(Process),
        prolog:process_release(Process, Goal).

%@  @item process_kill(@var{+Process})
%@  @itemx process_kill(@var{+Process}, @var{+SignalSpec})
%@  @PLXindex {process_kill/[1,2] (process)}

%@  Send a signal to the process designated by @var{Process}. The
%@  signal can either be a non-negative integer or a signal name as an
%@  (all uppercase) atom.
%@

%@  The following signal names are accepted under UNIX if the platform
%@  defines them: @code{SIGABRT}, @code{SIGALRM}, @code{SIGBUS},
%@  @code{SIGCHLD}, @code{SIGCONT}, @code{SIGFPE}, @code{SIGHUP},
%@  @code{SIGILL}, @code{SIGINT}, @code{SIGKILL} (the default),
%@  @code{SIGPIPE}, @code{SIGPOLL}, @code{SIGPROF}, @code{SIGQUIT},
%@  @code{SIGSEGV}, @code{SIGSTOP}, @code{SIGSYS}, @code{SIGTERM},
%@  @code{SIGTRAP}, @code{SIGTSTP}, @code{SIGTTIN}, @code{SIGTTOU},
%@  @code{SIGURG}, @code{SIGUSR1}, @code{SIGUSR2}, @code{SIGVTALRM},
%@  @code{SIGXCPU} and @code{SIGXFSZ}. However, many of these do not
%@  make sense to send as signals.
%@

%@  Under Windows, which does not have the signal
%@  concept, the signal name @code{SIGKILL} (the default) is treated
%@  specially and terminates the process with
%@  @code{TerminateProcess(Process, -1)}. 

%@  @strong{Please note}: Using @code{process_kill/[2,3]} on Windows
%@  is not recommended. Also, on Windows, the call may throw an error
%@  if the process has already exited.

%@

process_kill(Process, SignalSpec) :-
	Goal = process_kill(Process, SignalSpec),
	prolog:process_kill(Process, SignalSpec, Goal).

process_kill(Process) :-
	Goal = process_kill(Process),
	prolog:process_kill(Process, 'SIGKILL', Goal).



%@  @end table
