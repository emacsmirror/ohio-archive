#!/usr/local/bin/perl --  # -*-Perl-*-
'di';
'ig00';
# %f	-- %U
#
# $Log$
$RCSHEADER = '$Header$';	#'
'$Revision$'  =~ /^\$\w+:\s+([.1234567890]+)\s+\$$/;	#'
$VERSION = $1;

########################################################

	# These next few lines are legal in both Perl and Nroff.

.00;		# finish .ig
 
'di		\" finish diversion--previous line must be blank
.nr nl 0-1	\" fake up transition to first page again
.nr %% 0		\" start at page 1
'; __END__	#### From here on it's a standard manual page ####
.TH %B 1 "%d" "%o"
.SH NAME
%b \- %[Brief description: %]
.SH SYNOPSIS
.B %b [options] [files]
.SH DESCRIPTION
.SH OPTIONS
.SH ENVIRONMENT
.SH EXAMPLES
.SH FILES
.SH AUTHOR
%U %a
.SH SEE ALSO
.SH DIAGNOSTICS
.SH BUGS
