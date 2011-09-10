Template all Selection
:begin
boilerplate:
breaks:
displays:
lists:
sectioning:
surround:
:end

Template abstract Sequence
:begin
\begin{abstract}
<POINT>
\end{abstract}
:end

Template appendix Sequence
:begin
\appendix{<POINT>}
:end

Template bf Sequence
:begin
{\bf <POINT>}
:end

Template boilerplate Selection
:begin
abstract:
title:
whole:
:end

Template breaks Selection
:begin
linebreak:
newline:
newpage:
pagebreak:
:end

Template center Sequence
:begin
\begin{center}
<POINT>
\end{center}
:end

Template chapter Sequence
:begin
\chapter{<POINT>}
:end

Template description Sequence
:begin
\begin {description}
\item [<POINT>]
\end {description}
:end

Template displays Selection
:begin
center:
verbatim:
:end

Template em Sequence
:begin
{\em <POINT>}
:end

Template enumerate Sequence
:begin
\begin {enumerate}
\item <POINT>
\end {enumerate}
:end

Template it Sequence
:begin
{\it <POINT>}
:end

Template itemize Sequence
:begin
\begin {itemize}
\item <POINT>
\end {itemize}
:end

Template linebreak Sequence
:begin
\linebreak[4]
:end

Template lists Selection
:begin
description:
enumerate:
itemize:
:end

Template newline Sequence
:begin
\newline
:end

Template newpage Sequence
:begin
\newpage
:end

Template pagebreak Sequence
:begin
\pagebreak[4]
:end

Template paragraph Sequence
:begin
\paragraph{<POINT>}
:end

Template part Sequence
:begin
\part{<POINT>}
:end

Template rm Sequence
:begin
{\rm <POINT>}
:end

Template section Sequence
:begin
\section{<POINT>}
:end

Template sectioning Selection
:begin
appendix:
chapter:
paragraph:
part:
section:
subparagraph:
subsection:
subsubsection:
:end

Template sl Sequence
:begin
{\sl <POINT>}
:end

Template subparagraph Sequence
:begin
\subparagraph{<POINT>}
:end

Template subsection Sequence
:begin
\subsection{<POINT>}
:end

Template subsubsection Sequence
:begin
\subsubsection{<POINT>}
:end

Template surround Selection
:begin
bf:
em:
it:
rm:
sl:
tt:
:end

Template title Sequence
:begin
\title{<POINT>}
\author{<user-full-name>}
\date{<today>}
\maketitle
:end

Template tt Sequence
:begin
{\tt <POINT>}
:end

Template verbatim Sequence
:begin
\begin{verbatim}
<POINT>
\end{verbatim}
:end

Template whole Sequence
:begin
\documentstyle [12pt] {article}

\oddsidemargin 0in
\evensidemargin 0in
\topmargin -0.5in
\textheight 9.0in
\textwidth 6.5in

% END OF STYLISTIC BOILERPLATE

\begin{document}

<#abstract>
<title>

<POINT>

\end{document}

:end


Local Variables:
tpl-begin-template-definition:"^Template"
tpl-begin-template-body:"^:begin"
tpl-end-template-body:"^:end"
end:
