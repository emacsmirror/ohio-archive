Template program Sequence
:begin
/* <buffer-name>
   by <user-full-name>, <today> */

#include <stdio.h>
<#vardecllist>

<#functionlist>

/******************** main program ********************/
<main>

:end

Template includelist Repetition
:begin

<include>
:end

Template definelist Repetition
:begin

<define>
:end

Template vardecllist Repetition
:begin

<vardecl>
:end

Template argdecllist Repetition
:begin

<vardecl>
:end

Template functionlist Repetition
:begin


<function>
:end

Template main Sequence
:begin
void main ()
{
     <#vardecllist>
  <stmtlist>
}
:end

Template include Sequence
:begin
#include <<text>>
:end

Template define Sequence
:begin
#define <textenter:name> <textenter:constant>
:end

Template vardecl Sequence
:begin
<type> <textenter:name>;
:end

Template type Selection
:begin
char
double
float
int
long
short
unsigned
void
other:
:end

Template other Sequence
:begin
<text>
:end

Template function Sequence
:begin
/****************************************/
/* <textenter:name>:
     <textlong:description>
*/
<type> <textenter:name> (<#arglist>)
     <#argdecllist>
{
  <#vardecllist>
  <stmtlist>
} /* <textenter:name> */
:end

Template arglist Repetition
:begin
, <arg>
:end

Template arg Sequence
:begin
<text:arg>
:end

Template stmtlist Repetition
:begin

<stmt>
:end

Template caselist Repetition
:begin

<case>
:end

Template stmt Selection
:begin
assignment:
break:
case:
do:
expression:
for:
if:
ife:
return:
switch:
while:
:end

Template assignment Sequence
:begin
<text:lhs> = <text:expression>;
:end

Template break Sequence
:begin
break;
:end

Template case Sequence
:begin
case <text:constant>:
  <stmtlist>
  <#break>
:end

Template do Sequence
:begin
do
  {
    <stmtlist>
    }
while (<text:expression>);
:end

Template expression Sequence
:begin
<text>
:end

Template for Sequence
:begin
for (<text:initially>; <text:test>; <text:increment>)
  {
    <stmtlist>
    } /* for <text:test> */
:end

Template if Sequence
:begin
if (<text:expression>)
  {
    <stmtlist:then>
    } /* if (<text:expression>) */
:end

Template ife Sequence
:begin
if (<text:expression>)
  {
    <stmtlist:then>
    }
else
  {
    <stmtlist:else>
    } /* if (<text:expression>) */
:end

Template return Sequence
:begin
return <#text:expression>;
:end

Template switch Sequence
:begin
switch (<text:expression>)
  {
     <caselist>
     } /* switch (<text:expression>) */
:end

Template while Sequence
:begin
while (<text:expression>)
  {
    <stmtlist>
    } /* while (<text:expression>) */
:end

Template comment Sequence
:begin
/*
  <textlong>
*/
:end


Local Variables:
tpl-begin-template-definition:"^Template"
tpl-begin-template-body:"^:begin"
tpl-end-template-body:"^:end"
end:
