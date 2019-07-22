if { [info tclversion] < 7.5 } {
    set tcl_platform(platform) unix
}

proc tk_console_window {} {
    global tcl_platform

    if {$tcl_platform(platform) == "macintosh"} {
	# Use the native scrollbar for the console
	rename scrollbar ""
	rename macscrollbar scrollbar
    }
    text .console  -yscrollcommand ".sb set" -setgrid true
    scrollbar .sb -command ".console yview"
    pack .sb -side right -fill both
    pack .console -fill both -expand 1 -side left
    if {$tcl_platform(platform) == "macintosh"} {
        after idle {.console configure -font {Monaco 9 normal}}
	.sb configure -bg white
        .console configure -bg white -bd 0 -highlightthickness 0 \
	  -selectbackground black -selectforeground white \
	  -selectborderwidth 0 -insertwidth 1
	.console tag configure sel -relief ridge
	bind .console <FocusIn> {  .console tag configure sel -borderwidth 0
	  .console configure -selectbackground black -selectforeground white }
	bind .console <FocusOut> { .console tag configure sel -borderwidth 2
	  .console configure -selectbackground white -selectforeground black }
    }
    
    wm protocol . WM_DELETE_WINDOW { wm withdraw . }
    return .console
}
