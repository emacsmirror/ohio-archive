Template all Selection
:begin
package:
constant:
variable:
function:
stmt:
:end

Template package Sequence
:begin
;;; <buffer-name> -- <text:documentation>
;;; <user-full-name>, <today>

<#variables>
<functions>
:end

Template variables Repetition
:begin

<variable>
:end

Template variable Sequence
:begin
(defvar <textenter:variable-name> <text:value>
  "<textlong:documentation>"
) ; <textenter:variable-name>
:end

Template functions Repetition
:begin

<function>
:end

Template function Sequence
:begin

(defun <textenter:function-name> (<text:arguments>)
  "<textlong:documentation>"
  (interactive "<arg-type><textenter:function-name>: <text:arguments>? ")
					; Local Variables
  (let (<text:local-variables>)
					; Body
    <stmt:body>
  ) ; let
) ; defun <textenter:function-name>

:end

Template arg-type Selection
:begin
a	; Function name
b	; Buffer name (must exist)
B	; Buffer name (possibly nonexistent)
c	; Single character
C	; Command name
d	; Point as a number (no prompt)
D	; Directory name
f	; File name (must exist)
F	; File name (possibly nonexistent)
k	; Keystroke sequence (string)
m	; Mark as a number (no prompt)
n	; Number (reads a string and converts)
p	; Prefix arg converted to number
P	; Prefix arg in raw form
r	; Region (no prompt)
s	; String
S	; Symbol
v	; Variable name (must be user-variable-p)
x	; Lisp expression unevaluated
X	; Lisp expression evaluated
:end

Template stmt Selection
:begin
progn:
cond:
debug:
ife:
if:
while:
whilelist:
simple:
:end

Template progn Sequence
:begin
(progn
  <POINT>
) ; progn
:end

Template simple Sequence
:begin
(<text:fn> <arglist>)
:end

Template arglist Repetition
:begin
 <arg>
:end

Template arg Selection
:begin
expression:
stmt:
:end

Template expression Sequence
:begin
<text:expression>
:end

Template if Sequence
:begin
(if <text:condition>
  <POINT>
) ; if <text:condition>
:end

Template ife Sequence
:begin
(if <text:condition>
  <POINT>
; else
  <POINT>
) ; if <text:condition>
:end

Template cond Sequence
:begin
(cond
  <clauselist>
) ; cond
:end

Template clauselist Repetition
:begin

<clause>
:end

Template clause Sequence
:begin
(<text:condition>
  <POINT>
) ; <text:condition>
:end

Template while Sequence
:begin
(while <text:condition>
  <POINT>
) ; while <text:condition>
:end

Template whilelist Sequence
:begin
(while <text:list>
  (setq <text:item> (car <text:list>))
  (setq <text:list> (cdr <text:list>))
  <POINT>
) ; while <text:list>
:end

Template unrolledwhile Sequence
:begin
(if <text:condition>
  <stmt:body>
) ; if <text:condition>
(while <text:condition>
  <stmt:body>
) ; while <text:condition>
:end

Template debug Sequence
:begin
(debug nil "<POINT>")
:end


Local Variables:
tpl-begin-template-definition:"^Template"
tpl-begin-template-body:"^:begin"
tpl-end-template-body:"^:end"
end:
