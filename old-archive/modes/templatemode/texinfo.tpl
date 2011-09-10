Template all Selection
:begin
boilerplate:	; Large blocks of text
breaks:		; Stop filling text
conditionals:	; TeX-only or Info-only
formatted:	; @xyz ... @end xyz
lists:		; enumerate, itemize, table
references:	; cross references
sectioning:	; chapters, sections, etc.
strings:	; special characters and strings
surround:	; @xyz{...}
:end

Template appendix Sequence
:begin
@appendix <POINT>
:end

Template appendixsection Sequence
:begin
@appendixsection <POINT>
:end

Template b Sequence
:begin
@b{<POINT>}
:end

Template boilerplate Selection
:begin
infonodes:	; Use this for on-line info nodes
manual:		; Use this for printed manuals
whole:		; Either of the above
:end

Template breaks Selection
:begin
pagebreak:	; Start a new page
paragraphbreak:	; Start a new paragraph
space:		; Leave some blank vertical space
:end

Template bs Sequence
:begin
@key{BS}
:end

Template bullet Sequence
:begin
@bullet{}
:end

Template center Sequence
:begin
@center <POINT>
:end

Template chapter Sequence
:begin
@chapter <POINT>
:end

Template cindex Sequence
:begin
@cindex <POINT>
:end

Template code Sequence
:begin
@code{<POINT>}
:end

Template commands Selection
:begin
comment:	; Ignore rest of line
headings:	; Turn them on or off
include:	; Include a file
noindent:	; Do not start a new paragraph
refill:		; For info---refill after substitution of strings
:end

Template comment Sequence
:begin
@c <POINT>
:end

Template conditionals Selection
:begin
ifinfo:		; Transparent to TeX
iftex:		; Transparent to Info
:end

Template copyright Sequence
:begin
@copyright{}
:end

Template ctl Sequence
:begin
@key{CTL}
:end

Template ctrl Sequence
:begin
@ctrl{<POINT>}
:end

Template del Sequence
:begin
@key{DEL}
:end

Template dfn Sequence
:begin
@dfn{<POINT>}
:end

Template display Sequence
:begin
@display
<POINT>
@end display
:end

Template dots Sequence
:begin
@dots{}
:end

Template eitem Sequence
:begin
@item
<POINT>
:end

Template enumerate Sequence
:begin
@enumerate
@item
<POINT>
@end enumerate
:end

Template esc Sequence
:begin
@key{ESC}
:end

Template example Sequence
:begin
@example
<POINT>
@end example
:end

Template file Sequence
:begin
@file{<POINT>}
:end

Template findex Sequence
:begin
@findex <POINT>
:end

Template formatted Selection
:begin
center:		; center a line
display:	; like example, but variable-width font
example:	; verbatim, with fixed-width font
group:		; keep together on a page
ifinfo:		; Transparent to TeX
iftex:		; Transparent to Info
ignore:		; Transparent to both
quotation:	; Indented display
:end

Template group Sequence
:begin
@group
<POINT>
@end group
:end

Template headings Sequence
:begin
@headings <text:onoroff>
:end

Template i Sequence
:begin
@i{<POINT>}
:end

Template ifinfo Sequence
:begin
@ifinfo
<POINT>
@end ifinfo
:end

Template iftex Sequence
:begin
@iftex
<POINT>
@end iftex
:end

Template ignore Sequence
:begin
@ignore
<POINT>
@end ignore
:end

Template iitem Sequence
:begin
@item @bullet
<POINT>
:end

Template include Sequence
:begin
@include <text:file>
:end

Template indexcmds Selection
:begin
cindex:		; Concepts
findex:		; Functions
kindex:		; Keys
pindex:		; Programs
tindex:		; Data types
vindex:		; Variables
printindex:	; Command for printing an index
:end

Template indexcode Selection
:begin
cp		; Concepts
fn		; Functions
ky		; Keys
pg		; Programs
tp		; Data types
vr		; Variables
:end

Template infonodes Sequence
:begin
@setfilename <text:filename>

@node top, <textenter:next>, (dir), (dir)
@chapter <POINT>

<node>
:end

Template inforef Sequence
:begin
@xref{<text:node>,<text:name>,<text:file>}
:end

Template itemize Sequence
:begin
@itemize
@item @bullet
<POINT>
@end itemize
:end

Template items Selection
:begin
eitem:		; enumeration item
iitem:		; itemization item
mitem:		; menu item
titem:		; table item
txitem:		; table itemx
:end

Template kbd Sequence
:begin
@kbd{<POINT>}
:end

Template key Sequence
:begin
<specialchars>
:end

Template kindex Sequence
:begin
@kindex <POINT>
:end

Template lfd Sequence
:begin
@key{LFD}
:end

Template lists Selection
:begin
enumerate:	; 1. foo
itemize:	; -- foo
table:		; "foo"   text about foo
:end

Template manual Sequence
:begin
\input texinfo @c -*-texinfo-*-
@settitle <text:title>

@titlepage
@sp 10
@center @titlefont{<text:title>}
@sp 3
@center <user-full-name>
@sp 1
@center <text:address>
@center <text:moreaddress>
@sp 1
@center <today>
@sp 3
@center @b{Abstract}
<POINT>
@end titlepage

@c Include files with actual text
@include <text:infonodesfile>

@c Print indices
<#printindex>

@c Print table of contents
@summarycontents
@contents

@bye
:end

Template menu Sequence
:begin
@menu
* <text:node>:: <POINT>
@end menu
:end

Template meta Sequence
:begin
@key{META}
:end

Template mitem Sequence
:begin
* <text:node>:: <POINT>
:end

Template ncsection Sequence
:begin
@ncsection <POINT>
:end

Template node Sequence
:begin
@node <textenter:here>, <textenter:nxt>, <textenter:prv>, <textenter:up>
@chapter <POINT>
:end

Template noindent Sequence
:begin
@noindent
:end

Template pagebreak Sequence
:begin
@page
:end

Template paragraphbreak Sequence
:begin
@br
:end

Template pindex Sequence
:begin
@pindex <POINT>
:end

Template printindex Sequence
:begin
@node <textenter:indextype> Index, <textenter:nxt>, <textenter:prv>, <text:up>
@unnumbered <textenter:indextype> Index
@printindex <indexcode>
:end

Template pxref Sequence
:begin
@pxref{<text:node>}
:end

Template pxreflong Sequence
:begin
@pxref{<text:node>,<text:name>,<text:citation>,<text:file>,<text:title>}
:end

Template quotation Sequence
:begin
@quotation
<POINT>
@end quotation
:end

Template references Selection
:begin
inforef:	; reference to a file without a manual
pxref:		; like xref, but inside a parenthesized sentence.
pxreflong:	; like xreflong, ...
xref:		; short form of cross reference
xreflong:	; long form with all the fields
:end

Template refill Sequence
:begin
@refill
:end

Template ret Sequence
:begin
@key{RET}
:end

Template samp Sequence
:begin
@samp{<POINT>}
:end

Template section Sequence
:begin
@section <POINT>
:end

Template sectioning Selection
:begin
appendix:
appendixsection:
chapter:
menu:			; Build a menu
ncsection:		; Section when there are no chapters
node:			; Build a node
section:
subsection:
subsubsection:
unnumbered:
unnumberedsec:
:end

Template sft Sequence
:begin
@key{SFT}
:end

Template space Sequence
:begin
@sp <POINT>
:end

Template spc Sequence
:begin
@key{SPC}
:end

Template specialchars Selection
:begin
bs:		; Backspace
ctl:		; Control
del:		; Delete key
esc:		; Escape
lfd:		; Linefeed
meta:		; Meta key (Escape on most keyboards)
ret:		; Carraige return
sft:		; Shift
spc:		; Space
tab:		; Tab
:end

Template strings Selection
:begin
bullet:		; Bullet character for itemization
copyright:	; Copyright symbol
dots:		; Ellipsis
key:		; Choose a special character
tex:		; The TeX logo
:end

Template subsection Sequence
:begin
@subsection <POINT>
:end

Template subsubsection Sequence
:begin
@subsubsection <POINT>
:end

Template surround Selection
:begin
b:		; Bold font
code:		; Literal code
ctrl:		; For control characters
dfn:		; Definition of a term
file:		; File name
i:		; Italic font
kbd:		; Like code, but for names of keys
samp:		; Literal text
t:		; Typewriter font (fixed-width)
var:		; Metasyntactic variables (e.g., the file @var{file}...)
w:		; Prohibit line break within
:end

Template t Sequence
:begin
@t{<POINT>}
:end

Template tab Sequence
:begin
@key{TAB}
:end

Template table Sequence
:begin
@table <surround>
@item <text:label>
<POINT>
@end table
:end

Template tex Sequence
:begin
@TeX{}
:end

Template tindex Sequence
:begin
@tindex <POINT>
:end

Template titem Sequence
:begin
@item <text:label>
<POINT>
:end

Template txitem Sequence
:begin
@itemx <text:label>
:end

Template unnumbered Sequence
:begin
@unnumbered <POINT>
:end

Template unnumberedsec Sequence
:begin
@unnumberedsec <POINT>
:end

Template var Sequence
:begin
@var{<POINT>}
:end

Template vindex Sequence
:begin
@vindex <POINT>
:end

Template w Sequence
:begin
@w{<POINT>}
:end

Template whole Selection
:begin
infonodes:	; Use this for on-line info nodes.
manual:		; Use this for a printed manual.
:end

Template xref Sequence
:begin
@xref{<text:node>}
:end

Template xreflong Sequence
:begin
@xref{<text:node>,<text:name>,<text:citation>,<text:file>,<text:title>}
:end


Local Variables:
tpl-begin-template-definition:"^Template"
tpl-begin-template-body:"^:begin"
tpl-end-template-body:"^:end"
end:
