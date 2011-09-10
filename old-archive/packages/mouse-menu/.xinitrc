#!/bin/csh -f
xconsole -geometry +1+1 &		# Divert any error messages first. 
xset bc &				# Enable bug compatibility for Emacs.
twm					# Exit X when the window manager dies.
kbd_mode -a				# Reset the keyboard to sanity.
clear_colormap				# Reset the screen to sanity.