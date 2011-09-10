Template all Selection
:begin
command:	; spacing, include, ...
environment:	; bold, center, description, ...
part:		; appendix, chapter, whole, ...
surround:	; b, i, u, ux
value:		; date, page, section, ...
:end

Template appendix Sequence
:begin
@Appendix{<POINT>}
:end

Template appendixsection Sequence
:begin
@AppendixSection{<POINT>}
:end

Template arabic Sequence
:begin
@1
:end

Template b Sequence
:begin
@B{<POINT>}
:end

Template bold Sequence
:begin
@Begin{B}
<POINT>
@End{B}
:end

Template blankspace Sequence
:begin
@Blankspace{<POINT>}
:end

Template chapter Sequence
:begin
@Chapter{<POINT>}
:end

Template cite Sequence
:begin
@Cite{<POINT>}
:end

Template center Sequence
:begin
@Begin{Center}
<POINT>
@End{Center}
:end

Template command Selection
:begin
blankspace:
cite:
environment:
foot:
include:
modify:
newpage:
part:
set:
tabclear:
tabdivide:
tabset:
value:
:end

Template counter Selection
:begin
appendix
chapter
enumeration
page
subsection
:end

Template description Sequence
:begin
@Begin{Description}
<POINT>
@End{Description}
:end

Template device Selection
:begin
file:		; ASCII file for viewing on terminal
imagen300:	; laser printer
pagedfile:	; like file, but with formfeeds and headers, etc.
:end

Template document Selection
:begin
article
manual
report
slides
:end

Template enumerate Sequence
:begin
@Begin{Enumerate}
<POINT>
@End{Enumerate}
:end

Template environment Selection
:begin
bold:
center:
description:
enumerate:
format:
heading:
italic:
itemize:
majorheading:
underlined:
verbatim:
:end

Template file Sequence
:begin
@Device{File}
@Style{Justification off}
:end

Template foot Sequence
:begin
@Foot{}
:end

Template format Sequence
:begin
@Begin{Format}
<POINT>
@End{Format}
:end

Template heading Sequence
:begin
@Begin{Heading}
<POINT>
@End{Heading}
:end

Template i Sequence
:begin
@I{<POINT>}
:end

Template italic Sequence
:begin
@Begin{I}
<POINT>
@End{I}
:end

Template imagen300 Sequence
:begin
@Device{Imagen300}
@Style{FontFamily "ComputerModernRoman12"}
:end

Template include Sequence
:begin
@Include{<POINT>}
:end

Template itemize Sequence
:begin
@Begin{Itemize}
<POINT>
@End{Itemize}
:end

Template keyword Selection
:begin
indent
justification
:end

Template letterlower Sequence
:begin
@a
:end

Template letterupper Sequence
:begin
@A
:end

Template majorheading Sequence
:begin
@Begin{Majorheading}
<POINT>
@End{Majorheading}
:end

Template memo Sequence
:begin
@Center{M E M O R A N D U M}
@Blankspace{0.25 inches}
@Begin{Format}
@Tabclear
@Tabset{1 inch}
From:@\<user-full-name>
To:@\<text:recipients>
Cc:@\<text:cclist>
Date:@\@Value{Month} @Value{Day}, @Value{Year}
Subject:@\<text:topic>
@Blankspace{0.25 inches}
@Tabclear
@&-
@End{Format}
@Blankspace{0.25 inches}
@Set{Page=1}
@Comment{******************** END OF MEMO BOILERPLATE ********************}

:end

Template modify Sequence
:begin
@Modify{<counter>, Numbered{<template>}}
:end

Template newpage Sequence
:begin
@Newpage
:end

Template pagedfile Sequence
:begin
@Device{Pagedfile}
@Style{Justification off}
:end

Template paragraph Sequence
:begin
@Paragraph{<POINT>}
:end

Template part Selection
:begin
appendix:
appendixsection:
chapter:
memo:
paragraph:
section:
slidepage:
subsection:
titlepage:
whole:
:end

Template romanlower Sequence
:begin
@i
:end

Template romanupper Sequence
:begin
@I
:end

Template section Sequence
:begin
@Section{<POINT>}
:end

Template set Sequence
:begin
@Set{<counter>=<POINT>}
:end

Template slidepage Sequence
:begin
@Comment{*********************************************************************}
@Newpage
@Comment{Page <text:page>}

@Begin{Center}
@Begin{B}
<text:title>
@End{B}
@End{Center}

@Blankspace{1 inch}

@Tabclear
@Tabset{<text:settings>}
@Begin{Format}

@End{Format}
:end

Template style Sequence
:begin
@Style{<keyword> <POINT>}
:end

Template subsection Sequence
:begin
@SubSection{<POINT>}
:end

Template surround Selection
:begin
b:
i:
u:
ux:
:end

Template tabclear Sequence
:begin
@Tabclear
:end

Template tabdivide Sequence
:begin
@Tabdivide{<POINT>}
:end

Template tabset Sequence
:begin
@Tabset{<POINT>}
:end

Template template Selection
:begin
arabic:		; 1, 2, 3, ...
letterlower:	; a, b, c, ...
letterupper:	; A, B, C, ...
romanlower:	; i, ii, iii, ...
romanupper:	; I, II, III, ...
:end

Template titlepage Sequence
:begin
@Begin{TitlePage}
@Begin{TitleBox}
@Begin{MajorHeading}
<text:title>
@End{MajorHeading}

<user-full-name>

<textlong:address>

@Value{Month} @Value{Day}, @Value{Year}

@End{TitleBox}

@CopyrightNotice{<user-full-name>}

@Begin{ResearchCredit}

@End{ResearchCredit}
@End{TitlePage}
@Newpage
@Comment{***************** END OF TITLEPAGE BOILERPLATE *****************}
:end

Template u Sequence
:begin
@U{<POINT>}
:end

Template underlined Sequence
:begin
@Begin{U}
<POINT>
@End{U}
:end

Template ux Sequence
:begin
@UX{<POINT>}
:end

Template value Selection
:begin
vdate:
vday:
vmanuscript:
vmonth:
vpage:
vsectionnumber:
vsectiontitle:
vtime:
vweekday:
vyear:
:end

Template vdate Sequence
:begin
@Value{Date}
:end

Template vday Sequence
:begin
@Value{Day}
:end

Template verbatim Sequence
:begin
@Begin{Verbatim}
<POINT>
@End{Verbatim}
:end

Template vmanuscript Sequence
:begin
@Value{Manuscript}
:end

Template vmonth Sequence
:begin
@Value{Month}
:end

Template vpage Sequence
:begin
@Value{Page}
:end

Template vsectionnumber Sequence
:begin
@Value{SectionNumber}
:end

Template vsectiontitle Sequence
:begin
@Value{SectionTitle}
:end

Template vtime Sequence
:begin
@Value{Time}
:end

Template vweekday Sequence
:begin
@Value{Weekday}
:end

Template vyear Sequence
:begin
@Value{Year}
:end

Template whole Sequence
:begin
@Make{<document>}
<device>
@Style{Linewidth 6.5 inches}
@Style{Indent 0 chars}
@Use{Bibliography = "<text:bibfile>"}
@Style{References = StdAlphabetic}
@Pageheading{left "<text:heading>", right <@Value(Page)>}
@Pagefooting{center <@Value(Month) @Value(Day), @Value(Year)>}

@Comment{****************** END OF STYLISTIC BOILERPLATE *********************}

<titlepage>

:end


Local Variables:
tpl-begin-template-definition:"^Template"
tpl-begin-template-body:"^:begin"
tpl-end-template-body:"^:end"
end:
