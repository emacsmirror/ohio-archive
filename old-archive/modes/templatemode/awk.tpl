Template whole Sequence
:begin
# <text:name>: an Awk program

BEGIN <#action>

<linelist>

END <#action>
:end

Template action Selection
:begin
for:
if:
ignore:
print:
setvars:
while:
other:
:end

Template for Sequence
:begin
for (<text:initially>; <text:test>; <text:increment>)
  {
    <action>
    }
:end

Template if Sequence
:begin
if (<text:expression>)
  {
    <action>
    }
else
  {
    <action>
    }
:end

Template while Sequence
:begin
while (<text:expression>)
  {
    <action>
    }
:end

Template ignore Sequence
:begin
{}
:end

Template print Sequence
:begin
printf <text:format> <text:args>
:end

Template setvars Sequence
:begin
{ FS = "<text:fieldseparator>"; RS = "<text:recordseparator>"}
:end

Template other Sequence
:begin
{ <text> }
:end

Template pattern Selection
:begin
match:
miss:
:end

Template match Repetition
:begin
&& <fieldmatch>
:end

Template fieldmatch Sequence
:begin
$<text:field> ~ /<text:regexpr>/
:end

Template miss Sequence
:begin
<#firstmatch><fieldmismatch>
:end

Template firstmatch Sequence
:begin
<match> && 
:end

Template fieldmismatch Sequence
:begin
$<text:field> !~ /<text:regexpr>/
:end

Template linelist Repetition
:begin

<line>
:end

Template line Sequence
:begin
<#pattern> <#action>
:end


Local Variables:
tpl-begin-template-definition:"^Template"
tpl-begin-template-body:"^:begin"
tpl-end-template-body:"^:end"
end:
