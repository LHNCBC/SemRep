/* Copyright (C) 1995, 1998, Swedish Institute of Computer Science. */

%   File       : gauge.pl
%   Author     : Stefan Andersson
%   Updated    : 10 May 2000
%   Purpose    : Visualizer for the profiler

:- module(gauge, [view/0]).

:- dynamic tcl_variable/2.

:- discontiguous tcl_proc/1.

:- use_module(library(lists), [
        nth1/3
        ]).
:- use_module(library(tcltk), [
        tcl_delete/1,
        tcl_eval/3,     
        tk_new/2,
        tk_main_window/2,
        tk_destroy_window/1,
        tk_next_event/2
        ]).

% [PM] 4.1.3+ This is only a compile-time dependency
% :- use_module(library(context)).
:- load_files(library(context), [when(compile_time), imports([ctxt/2])]).

%% Defines context expansion keys
%% [PM] 4.1.3+ no longer needs to be dynamic
ctxt_items([interp,header,canvas,value_info,help]).

%% :- dynamic ctxt_items/1. % [PM] 4.1.3
%% %% SPRM 11871
%% %% :- ctxt_items([interp,header,canvas,value_info,help]).
%% :- assertz(ctxt_items([interp,header,canvas,value_info,help])).

%% These are the default settings for some TCL variables. We want to
%% store them as Prolog facts since we use the same settings as default
%% the next time view/0 is called. To add persistency to a Tcl variable,
%% just add a tcl_variable/2 fact for it. Note that these facts are
%% updated when the Gauge window is closed.
%% tcl_variable(TclVarName,Default).

%% Settings
tcl_variable('specvar','calls').
tcl_variable('sortvar','descending_values').
tcl_variable('scalevar','lin').
tcl_variable('showvar','nonzero').
tcl_variable('viewfont','Helvetica 10').
tcl_variable('PrintCmd','lpr -Pprinter').
tcl_variable('SaveFile','0').
tcl_variable('SaveFileName','histogram.ps').
tcl_variable('ColorMode','color').


%%-----------------------------------------------------------------------

view :-
        ctxt(Ctxt, [interp-Interp]),
        tk_new([name('Gauge')], Interp),
        set_tcl_vars(Ctxt),             % Update Tcl variables
        create_window(Ctxt),
        create_procs(Ctxt),
        tk_next_event(Interp, Event),
        on_event(Event, Ctxt, barval([],0)).

%%-----------------------------------------------------------------------

create_window(Ctxt) :-
        LabelLook = [min(relief),flat],
        FrameLook = [min(relief),sunken,min(borderwidth),2],
        %% This is to do proper event handling when the user closes
        %% the window using window manager functions.
        my_eval([wm,protocol,.,'WM_DELETE_WINDOW',br([prolog_event,quit])],Ctxt),
        
        %% [PM] 4.3 Whatever ancient (1999) "Tk bug" this was supposed to work around,
        %%          it breaks OS X (Gauge menus gets white text on white background).
        %% %% Needed under some Unix environments since Tk has a bug that
        %% %% makes the background color of some widgets (notably labels)
        %% %% to be wrong
        %% % my_eval([option,add,dq('*background'),sqb([.,cget,min(bg)])],Ctxt),
        
        Panel = +panel,
        frame(Panel, [], Ctxt),
        Settings = +settings,
        create_settings_panel(Settings, Ctxt, FrameLook, LabelLook),
        ValueInfo = Panel+value_info,
        create_value_info(ValueInfo, Ctxt, FrameLook, LabelLook),
        Buttons = Panel+button,
        create_button_panel(Buttons, Ctxt, FrameLook),
        pack([ValueInfo,Buttons], [min(side),top], Ctxt),
        MainView = +main_view,
        create_main_view(MainView, Ctxt, FrameLook, LabelLook),
        pack([MainView],
             [min(padx),2,min(pady),2,min(side),left,min(fill),both,min(expand),true],
             Ctxt),
        pack([Settings,ValueInfo,Panel],
             [min(padx),2,min(pady),2,min(side),top,min(fill),x],
             Ctxt).

create_settings_panel(Path, Ctxt, _, _) :-
        PackOptions = [min(side),top,min(anchor),w,min(fill),x,min(padx),2,min(pady),2],
        frame(Path, [min(relief),groove,min(borderwidth),4], Ctxt),
        conc_path(Path, heading, Heading),
        label(Heading,'Settings',[],Ctxt),        
        %% Specifications
        SpecVar = specvar,
        conc_path(Path, spec, Spec),
        optionmenu(Spec,'Specification',SpecVar,
                   ['Calls', 'Instructions','Choice Points'],
                   [calls,instructions,choice_points],
                   Ctxt),        
        %% Sort Order
        SortVar = sortvar,
        conc_path(Path, sort, Sort),
        optionmenu(Sort,'Sort Order',SortVar,
                   ['Alphabetic','Descending Values','Ascending Values',
                    'Top 40'],
                   [alphabetic,descending_values,ascending_values,
                    desc40],
                   Ctxt),
        %% Scale
        ScaleVar= scalevar,
        conc_path(Path, scale, Scale),
        optionmenu(Scale,'Scale',ScaleVar,
                   ['Linear','Logarithmic'],
                   [lin,log],
                   Ctxt),
        %% Show
        ShowVar= showvar,
        conc_path(Path, show, Show),
        optionmenu(Show,'Show',ShowVar,
                   ['All','Nonzero only'],
                   [all,nonzero],
                   Ctxt),
        %% Font
        FontVar= viewfont,
        conc_path(Path, font, Font),
        optionmenu(Font,'Font',FontVar,
                   ['Helvetica 8',
                    'Helvetica 10',
                    'Helvetica 12',
                    'Helvetica 14',
                    'Times 8',
                    'Times 10',
                    'Times 12',
                    'Times 14'],
                   ['Helvetica 8',
                    'Helvetica 10',
                    'Helvetica 12',
                    'Helvetica 14',
                    'Times 8',
                    'Times 10',
                    'Times 12',
                    'Times 14'],
                   Ctxt),
        pack([Heading,Spec,Sort,Scale,Show,Font],PackOptions,Ctxt).

create_value_info(Path, Ctxt, FrameLook, _) :-
        frame(Path, [FrameLook], Ctxt),
        conc_path(Path, name, ValueInfoName),
        conc_path(Path, value, ValueInfoValue),
        conc_path(Path, relative, ValueInfoRel),
        path_to_widget(ValueInfoName, ValueInfoNameW),
        path_to_widget(ValueInfoValue, ValueInfoValueW),
        path_to_widget(ValueInfoRel, ValueInfoRelW),
        ctxt(Ctxt, [value_info-vi(ValueInfoNameW,ValueInfoValueW,
                                  ValueInfoRelW)]),
        label(ValueInfoName, '', [min(width),24], Ctxt),
        label(ValueInfoValue, '', [min(width),24], Ctxt),
        label(ValueInfoRel, '', [min(width),24], Ctxt),
        pack([ValueInfoName,ValueInfoValue,ValueInfoRel],[], Ctxt).

create_button_panel(Path, Ctxt, _FrameLook) :-
        ButtonLook = [],
        frame(Path, [], Ctxt),
        conc_path(Path, button_left, ButtonBox),
        frame(ButtonBox, [], Ctxt),
        conc_path(ButtonBox, calc, Calc),
        conc_path(ButtonBox, reset, Reset),
        conc_path(ButtonBox, print, Print),
        conc_path(ButtonBox, help, Help),
        conc_path(ButtonBox, quit, Quit),
        button(Calc, 'Calculate',
               br([prolog_event,'calc($specvar,$sortvar,$scalevar)']),
               ButtonLook, Ctxt),
        button(Reset, 'Reset', br([prolog_event,reset]), ButtonLook, Ctxt),
        button(Print, 'Print', br([prolog_event,print]), ButtonLook, Ctxt),
        button(Help, 'Help', br([prolog_event,help]), ButtonLook, Ctxt),
        button(Quit, 'Quit', br([prolog_event,quit]), ButtonLook, Ctxt),
        pack([ButtonBox], [min(side),left,min(fill),x], Ctxt),
        pack([Calc,Reset,Print,Help,Quit],
             [min(side),top,min(anchor),w,min(fill),x], Ctxt).


create_main_view(Path, Ctxt, FrameLook, LabelLook) :-
        frame(Path, [], Ctxt),
        conc_path(Path, scview, ScrollView),
        frame(ScrollView, [], Ctxt),
        conc_path(ScrollView, view, View),
        conc_path(ScrollView, scrollbar, ScrollBar),
        my_eval([set,xsize,450], Ctxt),
        my_eval([set,ysize,'15c'], Ctxt),
        path_to_widget(View, ViewW),
        path_to_widget(ScrollBar, ScrollBarW),
        my_eval([canvas,ViewW,
                 min(width),'$xsize',
                 min(height),'$ysize',
                 min(yscrollcommand),dq([ScrollBarW,set]),
                 min(background),white,
                 min(relief),sunken,
                 min(borderwidth),2],
                Ctxt),
        my_eval([scrollbar,ScrollBarW,min(command),dq([ViewW,yview])], Ctxt),
        pack([View], [min(side),left,min(fill),both,min(expand),true], Ctxt),
        pack([ScrollBar], [min(side),right,min(fill),y], Ctxt),
        conc_path(Path, header, Header),
        conc_path(Header, header_label, HeaderLabel),
        frame(Header, FrameLook, Ctxt),
        label(HeaderLabel, '', LabelLook, Ctxt),
        pack([Header], [min(side),top,min(fill),x], Ctxt),
        pack([ScrollView], [min(side),top,min(fill),both,min(expand),true], Ctxt),
        pack([HeaderLabel], [], Ctxt),
        path_to_widget(HeaderLabel, HeaderLabelW),        
        ctxt(Ctxt, [canvas-ViewW, header-HeaderLabelW]).



%%-----------------------------------------------------------------------
%% Collection of widgets

button(Path, Text, Command, Look, Ctxt) :-
        path_to_widget(Path, Widget),
        my_eval([button,Widget,min(command),Command,min(text),dq([Text])|Look],
                Ctxt).

:- public radiobutton/5. % callback

radiobutton(Path, Text, Var, Look, Ctxt) :-
        last_item(Path, Value),
        path_to_widget(Path, Widget),
        my_eval([radiobutton,Widget,
                 min(variable),Var,
                 min(value),Value,
                 min(text),dq([Text])|Look],
                Ctxt).

label(Path, Text, Look, Ctxt) :-
        path_to_widget(Path, Widget),
        my_eval([label,Widget,min(text),dq([Text])|Look], Ctxt).

%% Shows the contents of a variable

% [MC] dead
% label_var(Path, Var, Ctxt) :-
%         path_to_widget(Path, Widget),
%         my_eval([label,Widget,min(textvariable),Var], Ctxt).

frame(Path, Look, Ctxt) :-
        path_to_widget(Path, Widget),
        my_eval([frame,Widget|Look], Ctxt).

optionmenu(Path,Title,Var,Labels,Values,Ctxt) :-
        path_to_widget(Path,Widget),
        my_eval([frame,Widget,min(relief),groove,min(borderwidth),2],Ctxt),
        conc_path(Path,menubutton,MenuButtonPath),
        conc_path(MenuButtonPath,menu,MenuPath),
        conc_path(Path,label, LabelPath),
        path_to_widget(MenuButtonPath,MenuButton),
        path_to_widget(MenuPath,Menu),
        path_to_widget(LabelPath,Label),
        my_eval([menubutton,MenuButton,min(text),dq(Title),
                 min(indicatoron),true,
                 min(menu),Menu,
                 min(relief),raised,
                 min(borderwidth),1],
                Ctxt),
        my_eval([menu,Menu],Ctxt),
        atom_codes(Var, VarCodes),
        append(VarCodes, "_label", LabelVarCodes),
        my_eval([label,Label,min(textvariable),codes(LabelVarCodes)], Ctxt),
        pack([MenuButtonPath],[min(side),top,min(anchor),w],Ctxt),
        pack([LabelPath],[min(side),top,min(anchor),e],Ctxt),
        optionmenu_addentries(Menu,VarCodes,LabelVarCodes,Labels,Values,Ctxt).

%% [MC] 3.8.6: made determinate
optionmenu_addentries(_,_,_,[],[],_) :- !.
optionmenu_addentries(Menu,VarCodes,LabelVarCodes,
                      [Label|Labels],[Value|Values],Ctxt) :-
        my_eval([Menu,add,radiobutton,
                 min(variable),codes(VarCodes),
                 min(label),dq(Label),
                 min(value),dq(Value),
                 min(command),br([set,codes(LabelVarCodes),dq(Label)])],
                Ctxt),
        %% Set the initial value in the label
        my_eval([if,br([codes([0'$|VarCodes]),==,br(Value)]),br([set,codes(LabelVarCodes),dq(Label)])],
                Ctxt),        
        optionmenu_addentries(Menu,VarCodes,LabelVarCodes,Labels,Values,Ctxt).

%%-----------------------------------------------------------------------
%% Widget id's (paths) are represented as +a+b+c (for .a.b.c). 

conc_path((+), Item, Path) :- !, Path = +Item.
conc_path(Path, Item, Path+Item).

pack(Paths, Options, Ctxt) :-
	(   foreach(P,Paths),
	    fromto(Widgets,[Widget|S],S,Options)
	do  path_to_widget(P, Widget)
	),
        my_eval([pack|Widgets], Ctxt).

path_to_widget(+, .).
path_to_widget(+Item, dot([Item])).
path_to_widget(Path+Item, dot(List)) :-
        path_to_widget(Path, List, [Item]).

path_to_widget(+Item) --> [Item].
path_to_widget(Path+Item) --> path_to_widget(Path), [Item].


last_item(+Item, Item).
last_item(_+Item, Item).

my_eval(Msg, Ctxt) :-
        ctxt(Ctxt, [interp-Interp]),
        tcl_eval(Interp, Msg, _).

%%-----------------------------------------------------------------------
%% Event handling

on_event(quit, Ctxt, _) :- !,
        ctxt(Ctxt, [interp-Interp]),
        get_tcl_vars(Ctxt),             % Store the new values of Tcl variables
        tk_main_window(Interp, Window),
        tk_destroy_window(Window),
        tcl_delete(Interp).     
on_event(Event, Ctxt, BarVal0) :-
        on_event(Event, Ctxt, BarVal0, BarVal),
        ctxt(Ctxt, [interp-Interp]),
        tk_next_event(Interp, NextEvent),
        on_event(NextEvent, Ctxt, BarVal).

on_event(calc(Spec,SortOrder,Scale), Ctxt, _, BarVal) :- !,
        ctxt(Ctxt, [header-HeaderW, interp-Interp]),
	view_data(Spec, UnsortedBars),
        %% If the user has chosen so, remove zero-values
        (   tcl_eval(Interp,[set,showvar],ShowValue),
            ShowValue \== "nonzero"
        ->  FilteredBars = UnsortedBars
        ;   (   foreach(K-V,UnsortedBars),
		fromto(FilteredBars,S0,S,[])
	    do  (V=\=0 -> S0 = [K-V|S] ; S0 = S)
	    )
        ),
        sort_bars(SortOrder, FilteredBars, Bars),
        Txt = format('~a/~a. ~a.', [Spec,SortOrder,Scale]),
        my_eval([HeaderW,configure,min(text),br(Txt)], Ctxt),
        draw(Bars, Scale, TotalValue, Ctxt),
        BarVal = barval(Bars,TotalValue).
on_event(reset, _, BarVal0, BarVal) :- !,
        BarVal0 = BarVal,
        profile_reset.
on_event(show_value(BarNr), Ctxt, BarVal0, BarVal) :- !,
        BarVal0 = BarVal,
        show_value(BarNr, BarVal, Ctxt).
on_event(help, Ctxt, BarVal0, BarVal) :- !,
        BarVal0 = BarVal,
        help(Ctxt).
on_event(print, Ctxt, BarVal0, BarVal) :- !,
        BarVal0 = BarVal,
        print_chart(Ctxt).
on_event(_, _, BarVal, BarVal).

view_data(instructions, UnsortedBars) :-
	profile_data(Raw),
	(   foreach(Key-Value,UnsortedBars),
	    foreach(Key-counter(_,Value,_,_),Raw)
	do  true
	).
view_data(choice_points, UnsortedBars) :-
	profile_data(Raw),
	(   foreach(Key-Value,UnsortedBars),
	    foreach(Key-counter(_,_,Value,_),Raw)
	do  true
	).
view_data(calls, UnsortedBars) :-
	profile_data(Raw),
	(   foreach(Key-Value,UnsortedBars),
	    foreach(Key-counter(_,_,_,TValue),Raw)
	do  arg(1, TValue, Value)
	).

show_value(0, _, Ctxt) :- !,
        my_eval(bell, Ctxt).
show_value(BarNr, BarVal, Ctxt) :-
        BarVal = barval(Bars,TotalValue),
        ctxt(Ctxt, [value_info-vi(NameW,ValW,RelW)]),
        nth1(BarNr, Bars, Pred-Value),
        (   TotalValue=:=0 -> RelValue=1.0
        ;   RelValue is Value/TotalValue
        ),
        my_eval([NameW,configure,min(text), br(write(Pred))], Ctxt),
        my_eval([ValW,configure,min(text), dq(['Value: ',Value])], Ctxt),
        my_eval([RelW,configure,min(text),
                 dq(format('Relative Value: ~2g',[RelValue]))], Ctxt).

draw(Bars, Scale, TotalValue, Ctxt) :-
        length(Bars, NoBars),
        open_null_stream(Stream),
        max_values(Bars, 0, 0, 0, MaxValue, MaxLabelWidth, TotalValue,
                   Stream, Ctxt),
        close(Stream),
        ctxt(Ctxt, [canvas-CanvasW]),
        my_eval([draw_init,Scale,CanvasW,MaxValue,MaxLabelWidth,NoBars], Ctxt),
        draw_bars(Scale, Bars, Ctxt).

%% [PM] 3.10.2 SPRM 7251
%%     A note on syntax of quoted atoms. ISO does not allow unbackslashed newline in quoted atoms.
%%     \n is a newline in ISO and SICStus mode
%%     \<newline> is ignored in ISO and SICStus mode
%%     \n\<newline> embeds a single newline in the atom in a portable way
tcl_proc(
'proc draw_init {scale bar_view max_val max_labelw no_bars} { \n\
        global view \n\
        global viewfont \n\
        global bar_start \n\
        global y \n\
        global yinc \n\
        global ymax \n\
        global xsize \n\
        global xscale \n\
        global tk_version \n\
        if {$scale == "log" && $max_val > 0} { \n\
                set max_val [expr log($max_val)] \n\
        } \n\
        set view $bar_view \n\
        bind $bar_view <Button-1> {show_bar_value %y} \n\
        set bar_start [expr $max_labelw + 10] \n\
        set y 0 \n\
        set yinc 24 \n\
        set ymax [expr $no_bars*$yinc] \n\
        $bar_view configure -scrollregion "0 0 $xsize $ymax" \n\
        set xsize [winfo width $view] \n\
        if {$max_val == 0} { \n\
                set xscale 0 \n\
        } else { \n\
                set xscale [expr double($xsize-10-$bar_start)/$max_val] \n\
        } \n\
        $view addtag dead all \n\
        $view delete dead \n\
}').

draw_bars(lin, Bars, Ctxt) :-
	(   foreach(Pred-Val,Bars),
	    param(Ctxt)
	do  my_eval([draw_bar_lin,br(write(Pred)),Val], Ctxt)
	).
draw_bars(log, Bars, Ctxt) :-
	(   foreach(Pred-Val,Bars),
	    param(Ctxt)
	do  my_eval([draw_bar_log,br(write(Pred)),Val], Ctxt)
	).

tcl_proc(
'proc draw_bar_lin {label value} { \n\
        global view \n\
        global viewfont \n\
        global bar_start \n\
        global y \n\
        global yinc \n\
        global xscale \n\
        set y [expr $y+$yinc] \n\
        $view create text 5 [expr $y-10] -text $label -anchor w -font $viewfont \n\
        $view create rectangle \
                $bar_start [expr $y-20] \
                [expr $bar_start+$xscale*$value] $y \
                -outline black -fill yellow \n\
}').

tcl_proc(
'proc draw_bar_log {label value} { \n\
        global view \n\
        global viewfont \n\
        global bar_start \n\
        global y \n\
        global yinc \n\
        global xscale \n\
        set y [expr $y+$yinc] \n\
        if {$value > 0} { \n\
                set value [expr log($value)] \n\
        } \n\
        $view create text 5 [expr $y-10] -text $label -anchor w -font $viewfont \n\
        $view create rectangle \
                $bar_start [expr $y-20] \
                [expr $bar_start+$xscale*$value] $y \
                -outline black -fill pink \n\
}').

%% Determine max value and max length of predicate/clause label
max_values([], MaxVal, MaxLab, Total, MaxVal, MaxLab, Total, _, _).
max_values([Pred-Val|Rest], MaxVal0, MaxLab0, Total0,
           MaxVal, MaxLab, Total, Stream, Ctxt) :-
        MaxVal1 is max(Val, MaxVal0),
        Total1 is Total0 + Val,
        labelwidth(Pred, Width, Ctxt),
        MaxLab1 is max(Width, MaxLab0),
        max_values(Rest, MaxVal1, MaxLab1, Total1,
                   MaxVal, MaxLab, Total, Stream, Ctxt).

labelwidth(Term,Width,Ctxt) :-
        ctxt(Ctxt, [interp-Interp]),
        tcl_eval(Interp,[font,measure,dq('$viewfont'),br(write(Term))],
                 WidthString),
        number_codes(Width,WidthString).

%% Print the histogram on a postscript printer or on file.
print_chart(Ctxt) :-
        %% Open the dialog
        ctxt(Ctxt, [canvas-CanvasW]),
        my_eval(['PrintDialog',CanvasW],Ctxt).

%% Update the Tcl variables with the stored settings
set_tcl_vars(Ctxt) :-
        tcl_variable(TclVar,TclValue),
        my_eval([set,TclVar,dq(TclValue)],Ctxt),
        fail.
set_tcl_vars(_).

%% Update the tcl_variable/2 facts
get_tcl_vars(Ctxt) :-
        ctxt(Ctxt, [interp-Interp]),
        findall(V,tcl_variable(V,_),VarNames),
        member(VarName,VarNames),
        tcl_eval(Interp, [set,VarName], Value),
        atom_codes(ValueAtom,Value),
        retract(tcl_variable(VarName,_)),
        assert(tcl_variable(VarName,ValueAtom)),
        fail.
get_tcl_vars(_).


%% Variables
tcl_proc(
' \n\
# Set the temporary directory. \n\
set tmplist "[lindex [array get env TMP] 1] [lindex [array get env TEMP] 1] [lindex [array get env TMPPATH] 1]" \n\
if {$tcl_platform(platform) == "unix"} {  \n\
    # Unix \n\
    set dflt {/tmp} \n\
} else {  \n\
    # Windows \n\
    set dflt {c:/temp} \n\
    # Backslash to slash, needed under Windows \n\
    regsub -all {\\\\} $tmplist "/" tmplist; # We have to use \\\\ instead \n\
       #of \\ because of SICStus quoting and Tcl each wants quoting  \n\
} \n\
set TempPrintFile [format "%s/gaugetmp.ps" [lindex "$tmplist $dflt" 0]] \n\
#puts $TempPrintFile \n\
 ').

%% The main printing dialog
tcl_proc(
' \n\
# The main printing command \n\
proc PrintDialog {Canvas} { \n\
    global PrintCmd ColorMode SaveFile SaveFileName \n\
 \n\
    toplevel .print \n\
    wm title .print "Postscript print:" \n\
 \n\
    # Printing command \n\
    frame .print.cmd \n\
    label .print.cmd.printl -text "Print command:" \n\
    if {$SaveFile} { \n\
        entry .print.cmd.printe -textvariable PrintCmd \
                -state disabled -bg gray \n\
    } else { \n\
        entry .print.cmd.printe -textvariable PrintCmd \
                -state normal -bg white \n\
    } \n\
    pack .print.cmd.printl .print.cmd.printe -side left -padx 4 -pady 4 \n\
 \n\
    # Save to file \n\
    frame .print.savefile \n\
    checkbutton .print.savefile.sfcb -text "Save to file" \
            -variable SaveFile \
            -command {if {$SaveFile} \
            {.print.savefile.filee configure -state normal -bg white;\
            .print.cmd.printe configure -state disabled -bg gray;\
            .print.buttons.print configure -text Save} \
            else {.print.savefile.filee configure -state disabled -bg gray; \
            .print.cmd.printe configure -state normal -bg white;\
            .print.buttons.print configure -text Print}} \n\
    label .print.savefile.filel -text "File name:" \n\
    if {$SaveFile} { \n\
        entry .print.savefile.filee -bg white -textvariable SaveFileName \
                -state normal \n\
    } else { \n\
        entry .print.savefile.filee -textvariable SaveFileName \
                -state disabled -bg gray \n\
    } \n\
    pack .print.savefile.sfcb .print.savefile.filel \
            .print.savefile.filee -side left -padx 4 -pady 4 \n\
 \n\
    # Color mode \n\
    frame .print.col \n\
    radiobutton .print.col.colorrb -variable ColorMode -value color -text "Color" \n\
    radiobutton .print.col.grayrb -variable ColorMode -value gray -text "Gray" \n\
    radiobutton .print.col.monorb -variable ColorMode -value mono -text "Mono" \n\
    pack .print.col.colorrb .print.col.grayrb .print.col.monorb \
            -side left -padx 4 -pady 4 \n\
 \n\
    # Buttons \n\
    frame .print.buttons \n\
    button .print.buttons.print -text "Print" -width 8 \
                          -command "PrintChart $Canvas;destroy .print" \n\
    button .print.buttons.cancel -text "Cancel" -width 8 \
                          -command "destroy .print" \n\
    pack .print.buttons.print .print.buttons.cancel \
            -side left -anchor c -padx 4 -pady 4 -expand true \n\
 \n\
    # Pack all frames \n\
    pack .print.cmd .print.savefile .print.col .print.buttons \
            -side top -anchor w -pady 4 -padx 4 -fill x \n\
}').

%% The actual printing procedure
tcl_proc(
' \n\
proc PrintChart { Canvas } { \n\
    global PrintCmd ColorMode SaveFile SaveFileName TempPrintFile \n\
 \n\
    if {$SaveFile} { \n\
        $Canvas postscript -colormode $ColorMode -file $SaveFileName \n\
    } else { \n\
        $Canvas postscript -colormode $ColorMode -file $TempPrintFile \n\
        eval exec $PrintCmd $TempPrintFile \n\
        file delete $TempPrintFile \n\
    } \n\
}').


%%-----------------------------------------------------------------------

tcl_proc(
'proc show_bar_value {yhit} { \n\
        global yinc \n\
        global view \n\
        global ymax \n\
        set viewy [expr round([$view canvasy $yhit])] \n\
        if {$viewy < 0} { \n\
                set viewy 0 \n\
        } else {if {$viewy >= $ymax}  { \n\
                set viewy [expr $ymax-1] \n\
        }} \n\
        set bar_nr [expr $viewy/$yinc + 1] \n\
        prolog_event "show_value($bar_nr)" \n\
}').


%%-----------------------------------------------------------------------
%% Install the Tcl procedures

create_procs(Ctxt) :-
        tcl_proc(ProcAtom),
        ctxt(Ctxt, [interp-Interp]),
        tcl_eval(Interp, ProcAtom, _),
        fail.
create_procs(_).

%%-----------------------------------------------------------------------
%% Sort and select using built-in keysort
%% TODO: nocer with do/2

sort_bars(alphabetic, Bars0, Bars) :- sort(Bars0, Bars).
sort_bars(descending_values, Bars0, Bars) :-
        swap(Bars0, Bars1),
        keysort(Bars1, Bars2),
        swap_rev(Bars2, [], Bars).
sort_bars(ascending_values, Bars0, Bars) :-
        swap(Bars0, Bars1),
        keysort(Bars1, Bars2),
        swap(Bars2, Bars).
sort_bars(desc40, Bars0, Bars) :-
        swap(Bars0, Bars1),
        keysort(Bars1, Bars2),
        swap_rev(Bars2, [], Bars3),
        firstn(40, Bars3, Bars).

swap([], []).
swap([K-V|T], [V-K|ST]) :-
	swap(T, ST).

swap_rev([], L, L).
swap_rev([K-V|T], T0, ST) :-
	swap_rev(T, [V-K|T0], ST).

firstn(0, _, []) :- !.
firstn(N, L, L1) :-
	firstn1(L, N, L1).

firstn1([], _, []).
firstn1([H|T], N, [H|T1]) :-
	N1 is N-1,
	firstn(N1, T, T1).

%%-----------------------------------------------------------------------
%% Simplified help, puts up a window with a text widget and reads a
%% file in it. No hyper text yet.

tcl_proc(
'proc help_window {} { \n\
        toplevel .help \n\
        text .help.text -yscrollcommand ".help.scroll set" \n\
        scrollbar .help.scroll -command ".help.text yview" \n\
        button .help.close -text "Close" -command "destroy .help" \n\
        pack .help.scroll -side right -fill y \n\
        pack .help.text -side top \n\
        pack .help.close -side bottom -fill x \n\
}').

tcl_proc(
'proc help_file file { \n\
        .help.text delete 1.0 end \n\
        set f [open $file] \n\
        while {![eof $f]} { \n\
                .help.text insert end [read $f 1024] \n\
        } \n\
        close $f \n\
}').

help(Ctxt) :-
        ctxt(Ctxt, [interp-Interp]),
        tcl_eval(Interp,'winfo exists .help',Exists),
        (   Exists == "0"
        ->  tcl_eval(Interp, help_window, _),
            absolute_file_name(library('gauge.txt'), File),
            tcl_eval(Interp, list([help_file,File]), _)
        ;   tcl_eval(Interp, 'raise .help', _)
        ).

