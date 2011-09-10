;Date: 4 Jan 89 17:00:00 GMT
;From: bob@cis.ohio-state.edu  (Bob Sutterfield)
;Subject: Re: Function keys under X?
;To: info-gnu-emacs@prep.ai.mit.edu
;
;In article <726@jupiter.iis.UUCP> heiser@iis.UUCP (Gernot Heiser) writes:
;>Does anybody know of a way to use function keys in Emacs under X?
;>...
;>I'm currently running Emacs 18.49 and X Version 10 on a Sun 3/50
;>under Sun OS 4.0.
;
;Here's what Chris Lott (a student programmer here) worked up for our
;undergrads when their instructor Really, Really Wanted arrow keys.
;Put it in your .emacs.  It worked under X10, and seems to continue
;working under X11, as well as other various window systems or the lack
;thereof, on most vintages (read: ROM versions) of Sun-3 keyboards.
;
;;
; arrow key remappings
; unmap the escape sequence esc-left_bracket
(global-unset-key       "\e[")
;;
; esc-[-A is up arrow, vt100 mapping
(global-set-key         "\e[A"          'previous-line)
; esc-[-215z is up arrow, sun kbd mapping
(global-set-key         "\e[215z"       'previous-line)
;;
; esc-[-B is down arrow, vt100 mapping
(global-set-key         "\e[B"          'next-line)
; esc-[-221z is up arrow, sun kbd mapping
(global-set-key         "\e[221z"       'next-line)
;;
; esc-[-C is right arrow, vt100 mapping
(global-set-key         "\e[C"          'forward-char)
; esc-[-219z is right arrow, sun kbd mapping
(global-set-key         "\e[219z"       'forward-char)
;;
; esc-[-D is left arrow, vt100 mapping
(global-set-key         "\e[D"          'backward-char)
; esc-[-217z is left arrow, sun kbd mapping
(global-set-key         "\e[217z"       'backward-char)
;;
; map r11 key to set mark - a safe null command for key between arrow keys
(global-set-key         "\e[218z"       'set-mark-command)
;;
; R15  key - move down in file
(global-set-key         "\e[222z"       'scroll-up)
; R9  key  - move up in file
(global-set-key         "\e[216z"       'scroll-down)
; R7 key   - go to top
(global-set-key         "\e[214z"       'beginning-of-buffer)
; R13 key  - go to end
(global-set-key         "\e[220z"       'end-of-buffer)
;;

;Enjoy!

