#! /usr/bin/wish

proc setup_board { } {
    # create container for the board
    frame .queens

    # loop of rows and columns
    for {set row 1} {$row <= 8} {incr row} {
	for {set column 1} {$column <= 8} {incr column} {

            # create label with a queen displayed in it
	    label .queens.$column-$row -bitmap @bitmaps/q64s.bm -relief flat

            # choose a background color depending on the position of the
            # square; make the queen invisible by setting the foreground
            # to the same color as the background
	    if { [expr ($column + $row) % 2] } {
		.queens.$column-$row config -background #ffff99
		.queens.$column-$row config -foreground #ffff99
	    } else {
		.queens.$column-$row config -background #66ff99
		.queens.$column-$row config -foreground #66ff99
	    }

            # place the square in a chess board grid
	    grid .queens.$column-$row -row $row -column $column -padx 1 -pady 1
	}
    }
    pack .queens
}

proc show_solution { solution } {
    clear_board
    set column 1
    foreach row $solution {
        place_queen $column $row
        incr column
    }
}

proc clear_board { } {
    for { set column 1 } {$column <= 8} {incr column} {
        reset_column $column
    }
}

proc reset_column { column } {
    for {set row 1 } { $row <= 8 } {incr row} {
        set_queens $column $row off
    }
}

proc set_queens { column row state } {
    if { $state == "on" } { 
        .queens.$column-$row config -foreground black
    } else { 
        .queens.$column-$row config -foreground [.queens.$column-$row cget -background] 
    }
}

proc place_queen { column row } {
    reset_column $column
    set_queens $column $row on
}

setup_board

show_solution "1 2 3 4 5 6 7 8"

