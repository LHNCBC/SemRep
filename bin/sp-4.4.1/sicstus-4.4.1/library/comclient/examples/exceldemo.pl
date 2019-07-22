%%% -*- Mode: Prolog; Module: user; -*-
%% Excel Demo


/*
' Start Excel...
dim app as object
set app = createobject("Excel.Application")

' Make it visible
app.visible = true

' Add a new workbook
app.workbooks.add

MsgBox "Slow fill example..."

' Fill cells with values...
dim i as long
dim j as long

with app.activesheet
   for i = 1 to 15
      for j = 1 to 15
         .cells(i,j).value = i
         DoEvents
      next j
   next i

   msgbox "Click me to clear range"
   .range("A1:O15").Clear
end with

msgbox "Now the fast way!"

CANNOT DO ARRAYS YET!


' Declare an array!
dim arr(1 to 15, 1 to 15) as long

' Fill array with values
for i = 1 to 15
   for j = 1 to 15
      arr(i,j) = i
   next j
next i

' Set all values in one shot!
app.activesheet.range("A1:O15").value = arr

msgbox "All done."

' Clean up...
app.activeworkbook.saved = true
app.quit
set app = nothing

*/

:- use_module(library(comclient)).
:- use_module(library(lists)).

test :-
   test('Excel.Application').

test(ProgID) :-
   comclient_create_instance(ProgID, App),

   %% app.visible = 1 
   comclient_invoke_put(App, visible, 1),

   %% app.workbooks.add
   comclient_invoke_method_proc(App, [workbooks, add]),

   %% with app.activesheet
   comclient_invoke_method_fun(App, activesheet, ActiveSheet),

   Rows = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15],
   Cols = Rows,

   (
     member(I, Rows),
     member(J, Cols),
     ValIJ is I+J/100,

     %% .cells i,j . value = i+j/100 
     comclient_invoke_put(ActiveSheet, [cells(I,J),value], ValIJ),
     fail
   ; true
   ),

   (
     member(I, Rows),
     member(J, Cols),
     comclient_invoke_method_fun(ActiveSheet, [cells(I,J), value], CellValue),
        format(user_error, '~nCell(~w,~w) = ~w', [I,J,CellValue]), flush_output(user_error),
     fail
   ; true
   ),

   Range = 'A1:O15',

   format(user_error, '~Npress return to clear range (~w)', [Range]), flush_output(user_error),
   get_code(_),

   %% .range A1:O15 .Clear 
   comclient_invoke_method_proc(ActiveSheet, [range(Range),clear]),
   
   %%  app.activeworkbook.saved = 1 
   comclient_invoke_put(App, [activeworkbook,saved], 1),

   format(user_error, '~Npress return to quit \'~w\'', [ProgID]), flush_output(user_error),
   get_code(_),
   
   %% app.quit
   comclient_invoke_method_proc(App, quit),

   comclient_release(ActiveSheet),
   comclient_release(App).
   
