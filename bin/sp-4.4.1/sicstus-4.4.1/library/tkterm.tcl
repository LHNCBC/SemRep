# [PM] 4.3 Note: Tcl/Tk 8.5 has a text widget that would be a better choice, I think.

set sptc_tw 0
set sptc_out 0
set sptc_hin 0
set sptc_hout 0
set sptc_hlist {}

# Enhances text widget with bindings etc for tk_terminal
#
proc sptc_start {text_widget} {
    global sptc_tw

    set sptc_tw $text_widget
    tkConsoleBind $text_widget

    $text_widget tag configure stderr -foreground brown
    $text_widget tag configure stdout -foreground black
    $text_widget tag configure stdin -foreground blue

    focus $text_widget
}

# Returns a command line
#
proc sptcin {} {
    global sptc_tw

    set ranges [$sptc_tw tag ranges input]
    set cmd ""
    if {$ranges != ""} {
	set pos 0
	while {[lindex $ranges $pos] != ""} {
	    set start [lindex $ranges $pos]
	    set end [lindex $ranges [incr pos]]
	    append cmd [$sptc_tw get $start $end]
	    incr pos
	}
    }

    sptchin $sptc_tw [string trim $cmd "\n"]
    $sptc_tw mark set output end
    $sptc_tw tag delete input
    $sptc_tw yview -pickplace insert
    return $cmd
}

# tkConsoleBind --
# This procedure first ensures that the default bindings for the Text
# class have been defined.  Then certain bindings are overridden for
# the class.
#
# Arguments:
# None.
#
proc tkConsoleBind {win} {
    catch {tkTextBind dummy_arg}
    
    bindtags $win "$win Text . all"

    # Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
    # Otherwise, if a widget binding for one of these is defined, the
    # <KeyPress> class binding will also fire and insert the character,
    # which is wrong.  Ditto for <Escape>.

    bind $win <Alt-KeyPress> {# nothing }
    bind $win <Meta-KeyPress> {# nothing}
    bind $win <Control-KeyPress> {# nothing}
    bind $win <Escape> {# nothing}
    bind $win <KP_Enter> {# nothing}

    bind $win <Tab> {
	tkTextInsert %W \t
	focus %W
	break
    }
    bind $win <Return> {
	%W mark set insert {end - 1c}
	tkTextInsert %W "\n"
	sptc_set_ready %W
	break
    }
    bind $win <Delete> {
	if {[%W tag nextrange sel 1.0 end] != ""} {
	    %W tag remove sel sel.first promptEnd
	} else {
	    if [%W compare insert < promptEnd] {
		break
	    }
	}
    }
    bind $win <BackSpace> {
	if {[%W tag nextrange sel 1.0 end] != ""} {
	    %W tag remove sel sel.first promptEnd
	} else {
	    if [%W compare insert <= promptEnd] {
		break
	    }
	}
    }
    foreach left {Control-a Home} {
	bind $win <$left> {
	    if [%W compare insert < promptEnd] {
		tk::TextSetCursor %W {insert linestart}
	    } else {
		tk::TextSetCursor %W promptEnd
            }
	    break
	}
    }
    foreach right {Control-e End} {
	bind $win <$right> {
	    tk::TextSetCursor %W {insert lineend}
	    break
	}
    }
    bind $win <Control-c> {
	sptc_interrupt
	break
    }
    bind $win <Control-d> {
	%W mark set insert {end - 1c}
	tkTextInsert %W \004
	sptc_set_ready %W
	break
    }
    bind $win <Control-k> {
	if [%W compare insert < promptEnd] {
	    %W mark set insert promptEnd
	}
    }
    bind $win <Control-t> {
	if [%W compare insert < promptEnd] {
	    break
	}
    }
    bind $win <Meta-d> {
	if [%W compare insert < promptEnd] {
	    break
	}
    }
    bind $win <Meta-BackSpace> {
	if [%W compare insert <= promptEnd] {
	    break
	}
    }
    bind $win <Control-h> {
	if [%W compare insert <= promptEnd] {
	    break
	}
    }
    foreach prev {Control-p Up} {
	bind $win <$prev> {
	    sptchout %W prev
	    break
	}
    }
    foreach prev {Control-n Down} {
	bind $win <$prev> {
	    sptchout %W next
	    break
	}
    }
    bind $win <Control-v> {
	if [%W compare insert > promptEnd] {
	    catch {
		%W insert insert [selection get -displayof %W] {input stdin}
		%W see insert
	    }
	}
	break
    }
    bind $win <Insert> {
	catch {tkTextInsert %W [selection get -displayof %W]}
	break
    }
    bind $win <KeyPress> {
	tkTextInsert %W %A
	break
    }
    foreach left {Control-b Left} {
	bind $win <$left> {
	    if [%W compare insert == promptEnd] {
		break
	    }
	    tk::TextSetCursor %W insert-1c
	    break
	}
    }
    foreach right {Control-f Right} {
	bind $win <$right> {
	    tk::TextSetCursor %W insert+1c
	    break
	}
    }
    foreach copy {F16 Meta-w Control-i} {
	bind $win <$copy> {
	    if {[selection own -displayof %W] == "%W"} {
		clipboard clear -displayof %W
		catch {
		    clipboard append -displayof %W [selection get -displayof %W]
		}
	    }
	    break
	}
    }
    foreach paste {F18 Control-y} {
	bind $win <$paste> {
	    catch {
	        set clip [selection get -displayof %W -selection CLIPBOARD]
		set list [split $clip \n\r]
		tkTextInsert %W [lindex $list 0]
		foreach x [lrange $list 1 end] {
		    %W mark set insert {end - 1c}
		    tkTextInsert %W "\n"
		    sptc_set_ready %W
		    tkTextInsert %W $x
		}
	    }
	    break
	}
    }
}

# Replace the default implementation of tkTextInsert so that we can
# attach tags to user input

proc tkTextInsert {w s} {
    if {$s == ""} {
	return
    }
    catch {
	if {[$w compare sel.first <= insert]
		&& [$w compare sel.last >= insert]} {
	    $w tag remove sel sel.first promptEnd
	    $w delete sel.first sel.last
	}
    }
    if {[$w compare insert < promptEnd]} {
	$w mark set insert end	
    }
    $w insert insert $s {input stdin}
    $w see insert
}

# Called from Prolog to print something on stdout
#
proc sptcout {} {
    global sptc_tw
    global sptc_out
#    puts "out: $sptc_out"
    $sptc_tw mark set output end
    $sptc_tw insert output $sptc_out stdout
    $sptc_tw see insert
    $sptc_tw mark set promptEnd insert
    $sptc_tw mark gravity promptEnd left
}

# Called from Prolog to print something on stderr
#
proc sptcerr {} {
    global sptc_tw
    global sptc_out
#    puts "err: $string"
    $sptc_tw mark set output end
    $sptc_tw insert output $sptc_out stderr
    $sptc_tw see insert
    $sptc_tw mark set promptEnd insert
    $sptc_tw mark gravity promptEnd left
}

# A simple history mechanism
#
proc sptchin {w cmd} {
    global sptc_hin
    global sptc_hout
    global sptc_hlist

    lappend sptc_hlist $cmd
    set sptc_hout [incr sptc_hin 1]
}

proc sptchout {w cmd} {
    global sptc_hin
    global sptc_hout
    global sptc_hlist

    if {$sptc_hin == 0} {
	return
    }
    switch $cmd {
    	prev {
	    incr sptc_hout -1
	    if {$sptc_hout < 0} {
		set sptc_hout [expr $sptc_hin - 1]
	    }
    	}
    	next {
	    incr sptc_hout 1
	    if {$sptc_hout >= $sptc_hin} {
		set sptc_hout 0
	    }
	}
    }
    set cmd [lindex $sptc_hlist $sptc_hout]
    $w delete promptEnd end
    $w insert promptEnd $cmd {input stdin}
}

# Execute a command
#
proc sptcexe {cmd} {
	global sptc_tw

	$sptc_tw delete promptEnd end
	$sptc_tw insert promptEnd $cmd {input stdin}
	$sptc_tw mark set insert {end - 1c}
	tkTextInsert $sptc_tw "\n"
	sptc_set_ready $sptc_tw
}
