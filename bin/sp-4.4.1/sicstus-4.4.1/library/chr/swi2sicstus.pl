/*
Bart Demoen K.U.Leuven
Fri Dec  2 04:37:24 CET 2005
Transforms SWI files into SICStus files - during the port from
the K.U.Leuven CHR system.
Some files have code of the form:


%% SWI begin
<some SWI specific code>
%% SWI end

and/or

%% SICStus begin
%% <some SICStus specific code>
%% SICStus end

The code below throws away the SWI code and removes the %% in front of
the SICStus specific code.

*/

transform(In,Out) :- transform(In,Out,sicstus).

transform(In,Out,Keep) :-
	In \== Out,
	see(In),
	tell(Out),
	header,
	transform1(Keep),
	close(In),
	close(Out).

header :- write('%% File generated from original SWI file\n\n\n').


begin_line_which("%% SICStus begin",sicstus).
begin_line_which("%% SWI begin",swi).

end_line_which("%% SICStus end",sicstus).
end_line_which("%% SWI end",swi).


begin_line(Line,Which) :-
	begin_line_which(B,Which),
	append(B,_,Line).
	
end_line(Line,Which) :-
	end_line_which(B,Which),
	append(B,_,Line).
	

transform1(Keep) :-
	read_line(Line),
	(Line == end_of_file ->
	    true
	;
	    (begin_line(Line,Which) ->
		(Which == Keep ->
		    keep_strip(Keep)
		;
		    reject(Which,Keep)
		)
	    ;
		write_line(Line),
		transform1(Keep)
	    )
	).

keep_strip(Keep) :-
	read_line(Line),
	(Line == end_of_file ->
	    true
	;
	    (end_line(Line,Keep) ->
		transform1(Keep)
	    ;
		strip(Line,Line1),
		write_line(Line1),
		keep_strip(Keep)
	    )
	).

reject(Which,Keep) :-
	read_line(Line),
	(Line == end_of_file ->
	    true
	;
	    (end_line(Line,Which) ->
		transform1(Keep)
	    ;
		reject(Which,Keep)
	    )
	).

strip([0'%,32|Line1],Line2) :-
        % [PM] 4.3.2 "%% foo(...) ..." should become "foo(...) ...", not " foo(...) ...".
        !,
        Line2 = Line1.
strip([0'%|Line1],Line2) :- !, strip(Line1,Line2).
strip(L,L).

write_line([]) :- nl.
write_line([X|R]) :-
	put_code(X),
	write_line(R).
