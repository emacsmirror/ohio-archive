#!/bin/csh

# LaTeXinfo
setenv LATEXINFO /usr2/ai/nesc/latexinfo

# Add the format files to the list of directories that LaTeX searches.
if ( $?TEXINPUTS ) then
	setenv TEXINPUTS "$TEXINPUTS"':'"$LATEXINFO"
  else
	setenv TEXINPUTS "$LATEXINFO"
endif
