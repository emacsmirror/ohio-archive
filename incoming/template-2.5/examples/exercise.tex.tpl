%%% exercise.tex.tpl --- file with header style 2, (>>>YEAR<<<)

%% Full date in a template specific format: (>>>time<<<)

\documentclass{exercise}

\begin{document}

%%%% The last way of deriving the comment style  Level 4: (>>>single<<<)
%%%  is a bit different if `comment-start' is a  Level 3: (>>>single<<<)
%%   string of length 1: level = #repetitions    Level 2: (>>>single<<<)
                                %                Level 1: (>>>single<<<)

%%  Here, a block comment consists of all neighboring lines which start with
%%% spaces and `comment-start', nearly the same as is TEMPLATE.cc.tpl...
%%-(>>>block<<<)

%%  It is a bit different here where `comment-start' is a string of length 1:
%%% only lines where the number of repetitions of `comment-start' is the same
%%% or larger than in the line where the command is invoked from, is considered
%%%-(>>>block<<<)
%%  to be part of the block.

%% Note that the template commands in this template are safe!

\end{document}
>>>TEMPLATE-DEFINITION-SECTION<<<
nil
normal-mode
nil
("single" template-single-comment)
("block" template-block-comment)
("time" template-insert-time "%a %d %b %Y, %I:%M %p")
