Template all Selection
:begin
program:
constant:
typedecl:
type:
variables:
procedure:
function:
stmt:
:end

Template program Sequence
:begin
program <textenter:name> (input, ouput);
const
   <constants>
type
   <types>
var
   <variables>

<procedures>

   begin { main program }
      <stmtlist>
   end.
:end

Template constants Repetition
:begin

<constant>
:end

Template constant Sequence
:begin
<textenter:name> = <text:value>;
:end

Template types Repetition
:begin

<typedecl>
:end

Template typedecl Sequence
:begin
<textenter:name> = <type>;
:end

Template type Selection
:begin
array:
boolean
char
file:
integer
pointer:
range:
real
record:
scalar:
set:
other:
:end

Template array Sequence
:begin
array [<indexlist>] of <type>
:end

Template indexlist Repetition
:begin
, <indextype>
:end

Template indextype Selection
:begin
range:
scalar:
other:
:end

Template file Sequence
:begin
file of <type>
:end

Template pointer Sequence
:begin
^<type>
:end

Template range Sequence
:begin
<text:start> .. <text:stop>
:end

Template record Sequence
:begin
record
   <fieldlist>
end
:end

Template fieldlist Repetition
:begin
;
<field>
:end

Template field Sequence
:begin
<textenter:name>: <type>
:end

Template scalar Sequence
:begin
(<itemlist>)
:end

Template itemlist Repetition
:begin
, <textenter:item>
:end

Template set Sequence
:begin
set of <indextype>
:end

Template other Sequence
:begin
<text>
:end

Template variables Repetition
:begin

<variable>
:end

Template variable Sequence
:begin
<textenter:name>: <type>;
:end

Template procedures Repetition
:begin

<procfunc>
:end

Template procfunc Selection
:begin
procedure:
function:
:end

Template procedure Sequence
:begin
{************************************************************}

procedure <textenter:name> (<args>);
var
   <variables>
begin
   <stmtlist>
end; { <textenter:name> }

:end

Template function Sequence
:begin
{************************************************************}

function <textenter:name> (<args>): <type>;
var
   <variables>
begin
   <stmtlist>
end; { <textenter:name> }

:end

Template args Repetition
:begin
; <argdecl>
:end

Template argdecl Sequence
:begin
var <textenter:id>: <type>
:end

Template stmtlist Repetition
:begin

<stmt>
:end

Template stmt Selection
:begin
assignment:
case:
compound:
for:
if:
repeat:
while:
with:
:end

Template assignment Sequence
:begin
<text:variable> := <text:expression>;
:end

Template case Sequence
:begin
case <text:expression> of
   <caselist>
end;
:end

Template  caselist Repetition
:begin
;
<caseclause>
:end

Template caseclause Sequence
:begin
<text:constant>: <stmt>
:end

Template compound Sequence
:begin
begin
   <stmtlist>
end;
:end

Template for Sequence
:begin
for <text:index> := <text:start> to <text:stop> do begin
   <stmtlist>
end; { for <text:index> }
:end

Template if Sequence
:begin
if <text:ifcondition> then begin
   <stmtlist>
end else begin
   <stmtlist>
end; { if <text:ifcondition> }
:end

Template repeat Sequence
:begin
repeat
   <stmtlist>
until <text:stop>;
:end

Template while Sequence
:begin
while <text:whilecondition> do begin
   <stmtlist>
end; { while <text:whilecondition> }
:end

Template with Sequence
:begin
with <text:idlist> do begin
   <stmtlist>
end; { with <text:idlist> }
:end


Local Variables:
tpl-begin-template-definition:"^Template"
tpl-begin-template-body:"^:begin"
tpl-end-template-body:"^:end"
end:
