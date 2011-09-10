# /a/sundown/export/home/0002/beebe/emacs/profile.awk, Mon Mar  1 06:05:21 1999
# Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
# ========================================================================
# Read an emacs  profile file, and produce a new listing with an extra
# column showing relative performance.
#
# Usage:
#	awk -f profile.awk [-v TIME=nnn] profile-listing >new-profile-listing
#
# The TIME value is used as a divisor for the last column, so that it
# represents the time of the profile line against which other lines should
# be measured.  If it is not specified, then the time of the first line
# is used.
#
# [01-Mar-1999]
# ========================================================================

/^Function/	{ print; next }

/^========/	{ print; next }

		{ if (TIME == 0) TIME = $4; printf("%-75s %15.2f\n", $0, $4/TIME) }

