#! /usr/bin/wish

# make all the widgets
# worry about packing them later


frame .mbf
menubutton .mbf.mb1 -text menubutton1 -menu .mbf.mb1.m
menubutton .mbf.mb2 -text menubutton2
menubutton .mbf.mb3 -text menubutton3
menubutton .mbf.mb4 -text menubutton4
menubutton .mbf.mb5 -text menubutton5


menu .mbf.mb1.m
.mbf.mb1.m add command -label hello
.mbf.mb1.m add command -label goodbye


pack .mbf.mb1 .mbf.mb2 .mbf.mb3 .mbf.mb4 .mbf.mb5 -side left
pack .mbf -side top -fill x

frame .f2

frame .f2.cf -relief groove  -borderwidth 4
label .f2.cf.label -text "Canvas"
canvas .f2.cf.canvas -bg bisque
.f2.cf.canvas create oval 1c 3c 3c 5c -fill blue -outline {}
.f2.cf.canvas create rectangle 5c 1c 9c 4c -width 2m -outline yellow \
	-fill darkgreen

pack .f2.cf.label -anchor nw
pack .f2.cf.canvas -side top

frame .f2.lbf -relief groove -borderwidth 4
label .f2.lbf.label -text "Listbox and Scrollbar"
listbox .f2.lbf.listbox
scrollbar .f2.lbf.scrollbar -orient vertical
pack .f2.lbf.label -side top -fill x
pack .f2.lbf.listbox -side left
pack .f2.lbf.scrollbar -side left -fill y

pack .f2.cf .f2.lbf -side left -anchor nw -padx 5

frame .f1 

frame .f1.bf -relief groove -borderwidth 4
label .f1.bf.label -text "Button"
button .f1.bf.button -text "I am a button" -command {puts "hello"}
pack .f1.bf.label -anchor nw
pack .f1.bf.button -side top

frame .f1.cbf -relief groove -borderwidth 4
label .f1.cbf.label -text Checkboxes
checkbutton .f1.cbf.check1   -text "pizza"
checkbutton .f1.cbf.check2   -text "pasta"
checkbutton .f1.cbf.check3   -text "toast"
pack .f1.cbf.label -anchor nw
pack .f1.cbf.check1 .f1.cbf.check2 .f1.cbf.check3 -side top

frame .f1.ef -relief groove -borderwidth 4
label .f1.ef.label -text Entry
entry .f1.ef.entry -textvariable evar
set evar "I am an entry widget"
pack .f1.ef.label -anchor nw
pack .f1.ef.entry -side top

frame .f1.lf -relief groove -borderwidth 4
label .f1.lf.flabel -text "Label"
label .f1.lf.label -text "label, label, label"
pack .f1.lf.flabel -anchor nw
pack .f1.lf.label -side top


pack .f1.bf .f1.cbf .f1.ef .f1.lf -side left -padx 5 -anchor nw


frame .f4

frame .f4.mf -relief groove -borderwidth 4
label .f4.mf.label -text Message
message .f4.mf.message -text "I am a multiline non-editable message box. So long and thanks for all the fish!" -width 100 -justify center
pack .f4.mf.label -anchor w -padx 5 -side top
pack .f4.mf.message -anchor w -padx 5 -side top

frame .f4.f3

frame .f4.f3.rf -relief groove -borderwidth 4
label .f4.f3.rf.label -text "Radiobuttons"
radiobutton .f4.f3.rf.radio1 -variable v -value 1 -text "pop" 
radiobutton .f4.f3.rf.radio2 -variable v -value 2 -text "oldies"
radiobutton .f4.f3.rf.radio3 -variable v -value 3 -text "classical"
radiobutton .f4.f3.rf.radio4 -variable v -value 4 -text "intellectual"

set v 2

pack .f4.f3.rf.label -side top -fill x -anchor w
pack .f4.f3.rf.radio1 -side left 
pack .f4.f3.rf.radio2 -side left
pack .f4.f3.rf.radio3 -side left
pack .f4.f3.rf.radio4 -side left

frame .f4.f3.sf -relief groove -borderwidth 4
label .f4.f3.sf.label -text Scale -justify left
scale .f4.f3.sf.scale -orient horizontal -length 200
pack .f4.f3.sf.label -side top -anchor nw -padx 5
pack .f4.f3.sf.scale -side top -fill x -padx 5 -anchor w

pack .f4.f3.rf -side top -pady 5
pack .f4.f3.sf -side top -pady 5

pack .f4.mf -side left -padx 5
pack .f4.f3 -side left -padx 5


frame .tf -relief groove -borderwidth 4
label .tf.label -text "Text"
text .tf.text -width 40 -height 5

pack .tf.label -side top -anchor nw -padx 5
pack .tf.text -side top -anchor nw -pady 5

.tf.text insert end "I am a text widget!
You can edit my multiline text!

The quick brown fox jumped over the lazy dog 1234567890"

pack .f2 -side top -anchor nw -padx 5 -pady 5 -ipadx 5 -ipady 5
pack .f1 -side top -anchor nw -padx 5 -pady 5 -ipadx 5 -ipady 5
pack .f4 -side top -anchor nw -padx 5 -pady 5 -ipadx 5 -ipady 5
 

.f2.lbf.listbox insert end listitem0
.f2.lbf.listbox insert end listitem1
.f2.lbf.listbox insert end listitem2
.f2.lbf.listbox insert end listitem3
.f2.lbf.listbox insert end listitem4
.f2.lbf.listbox insert end listitem5
.f2.lbf.listbox insert end listitem6
.f2.lbf.listbox insert end listitem7
.f2.lbf.listbox insert end listitem8
.f2.lbf.listbox insert end listitem9

pack .tf -side top -pady 5

tk_bisque

.mbf.mb1.m post 0 0

