;; @(#) line-nos.el -- line numbers for emacs

;; @(#) $Id: line-nos.el,v 1.3 1997/03/10 16:06:37 jaalto Released $
;; @(#) $Keywords: line numbers $
;; $KnownCompatibility: 18.57, 19.28, XE19.14 $

;; This file is Not part of GNU emacs

;;{{{ Id

;; Copyright (C)  1989 Wayne Mesard
;; Author:        Wayne Mesard <bbn.com!mesard@bbn.com>
;; Maintainer:    Jari Aalto <jari.aalto@poboxes.com>
;; First Created: Feb 6th 1989
;;
;; To get information on this program use ident(1)

;; LCD Archive Entry:
;; line-nos|Wayne Mesard, Jari Aalto|bbn.com!mesard@bbn.com, jari.aalto@ntc.nokia.com|
;; line numbers for emacs|
;; 10-Mar-1997|1.3|~/misc/line-nos.el.Z|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;}}}
;;{{{ Install

;;; Intallation:
;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;      (global-set-key "\C-c\C-l" 'line-toggle)
;;      (autoload 'line-toggle          "line-nos.el" "" t)
;;      (autoload 'display-line-nums    "line-nos.el" "" t)
;;      (autoload 'hide-line-nums       "line-nos.el" "" t)
;;
;; copy the little script provided and put it somewhere in your PATH
;;
;;      ~/bin/line-nos.sh

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;; To: unix-emacs@bbn.com
;; Date: 6 Feb 89 19:21:44 GMT
;; From: Wayne Mesard <bbn.com!mesard@bbn.com>
;; Subject: Re: line numbers in emacs
;;
;; In article <22@euteal.UUCP> michel@euteal.UUCP (Michel Berkelaar)
;; >writes: I'm looking for an emacs minor mode which will add line
;; >numbers to all lines and still allow editing, scrolling, saving
;; >(without those numbers), etc to go on.  It seems so basic that
;; >someone must have worked on it.
;;
;; One would think so, but Control-X L seems adequate for most
;; people's needs.
;;
;; >If there are any hints, please post, or mail to:
;;
;; I have what may be an answer.  Well, two answers.  And since I'm in
;; M-x verbose-mode today.  You're gonna have to wade through a bunch
;; of prose to get to them.
;;
;; Some time ago, I wrote commands to stuff line numbers into the
;; current region or buffer.  This solution has the benefits of
;; simplicity:
;;

;; LINE-NUMBERS-TAKE-1
;; -------------------------------------------------
;; This is basically useless if you want the line numbers there while
;; you're editing.
;;
;; When I saw your message, it renewed my frustration at the fact that
;; there's no way to write a minor mode in GNU elisp.  The basic
;; mechanism needed to get line numbers displayed is to have an
;; interrupt facility (or something) periodically call an elisp
;; function which checks to see if the window has been scrolled
;; recently, and does the right thing if it has.
;;
;; My first thought was that it would be interesting to create a new
;; keymap, bind every key to the same function, and have that function
;; do its minor-mode-type-thing and then call the function which is
;; normally bound to that key:
;;
;;      (let ((cmd (aref *old-map* (this-command-keys))))
;;        ...
;;        (call-interactively cmd))))
;;
;; Well, that is interesting, except it doesn't work so good.  (Left
;; as an exercise to the reader.)
;;
;; My second thought was, lacking an explicit interrupt or
;; on-redisplay-hook type of thing, I could abuse the procedure
;; facility (used in emacs/lisp/time.el) to do the equivalent.  All
;; that's needed is a program which will periodically dump a character
;; onto its output.  loadst(1) which comes with GNU Emacs and BSD's
;; yes(1) both fit this bill.  But I used the shell script
;; below:
;;
;; ------------- ~/bin/line-nos.sh -------
;; :
;; # line-nos.sh
;; # Shell script required by line-nos.el, M-x display-line-nums
;; #
;;
;; while [ 1 ]
;; do
;;  echo -n x
;;  sleep $1
;; done
;; -------------- end of ~/bin/line-nos.sh --------
;;
;; start-process will invoke this program, and everytime it prints
;; something out (i.e. approxiamtely every LINE-NUM-UPDATE seconds),
;; the process-filter will get called.
;;
;; Put "peep" in your PATH somewhere, then the following will maintain
;; a line number buffer to the left of the buffer you are editing.
;; The biggest disadvantage with this method is that Emacs can't do
;; smart scrolling with horizontally split windows (at least not on
;; vt100s).  This can be a pain at 1200 baud.
;;
;; I'd be interested in any comments on this code.
;;
;;
;; LINE-NUMBERS-TAKE-2
;; ----------------------------------------
;; display-line-numbers:  Show line-numbers for an Emacs buffer.
;; Copyright (C) 1989 Wayne Mesard
;;
;; This file is not officially part of GNU EMACS.  But it is being
;; distributed under the terms of the GNU Emacs General Public License.
;; The copyright notice and this notice must be preserved in all copies.
;;
;; Call M-x display-line-nums  to show 'em.
;;      M-x hide-line-nums     to make 'em go away.
;; Or bind these guys to keys in your .emacs file.
;;
;; [N.B.  I haven't tested this rigorously.  It may be buggy.]
;; I will get around to these things eventually.  Other suggestions
;; are welcome.  Mail to Mesard@BBN.COM.   -wsm
;; To do:  [wsm060289]
;;     o Support for line numbers in multiple windows is not adequate.
;;       Things like this should be possible and fool proof:
;;
;;                          +---------------------+
;;                          | 1| GNU, which stands|
;;                          | 2|for Gnu's not Unix|
;;                          | 3|is the name for   |
;;                          | 4|the complete ...  |
;;                          +---------------------+
;;                          |20|I canna change the|
;;                          |21|laws o physics    |
;;                          |22|Cap'n!            |
;;                          +---------------------+
;;
;;     o  Ctrl-X 1 hides the line number window, but doesn't kill
;;        The line-num process.  It should.  (Have the filter function
;;        check for the case when the line num window isn't on screen.)
;;     o  Ctrl-X o should not go to the num buf.
;;     o  Similarly, The *Help* buffer should NOT be allowed to come
;;        up in the line number window!
;;     o  Some function documentation would be nice.

;;}}}
;;{{{ History

;; ........................................................ &t-history ...
;; [jari]   = jari.aalto@poboxes.com
;;
;; Mar   10     1995    [jari]          19.28   v1.3            Released
;; - XEmacs didn't like transient-mode function call. Added fboundp test.
;;
;; Oct   20     1995    [jari]          19.28   v1.2            Released
;; - Added line-nos-load-hook. Rewrote the other package information
;; - Seems to be ready for ohio now. Deleted the nl(1) function and
;;   moved it my general tinylib.el
;;
;; Apr   28     1995    [jari]          19.28   v1.1            NotReleased
;; - I found this .el from some of our 18.57 directories long time ago and
;;   it immediately drop among to my favourite modules for emacs.
;;   I tried to reach Wayne to ask if he is supporting this el, but I
;;   got no answers.
;; - I just cleaned the layout of this code a bit and added some
;;   minor things.
;; - There is a drawback of using line numbers in separate vertical buffer:
;;   If you use some command that displays data on _other_ buffer,
;;   it will propably pick up that vertical buffer .. not very good for
;;   reading text :-/ So, turn on line numbers on, when you need them,
;;   and off, when you want to display something else.
;; - Several people have been asking in the gnu.emacs.help about the
;;   capability to show line numbers, so here is some info I'm aware
;;   of at the moment. Actually this is my post to g.e.h  07 Apr 1995
;;
;;   --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
;;   There exists several packages for showing the line numbers.
;;
;;
;;   tinylib.el, function  ti::b-number-lines
;;   ========================================
;;   - Adds Unix nl(1) like line numbers to buffer. Permanent.
;;
;;   line-nos           [for any emacs ver]
;;   ========================================
;;   - Shows line-nos in separate vertical window, so you can see whole
;;     buffer's nbrs at once.
;;   - Used as on/off basis. Displays when you need the line number
;;     information, eg. when running the debugger and setting appropriate
;;     breakpoints by looking at the code in source window.
;;
;;   column.el          [19.2x ]
;;   ========================================
;;   - archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/misc/column.el.Z
;;   - Displays column and line number info constantly on the *mode* line, not
;;     in the buffer.
;;
;;   setnu.el           [19.29+]
;;   ========================================
;;   - Ultimately the best line number package, strict vi-style in
;;     buffer.
;;
;;   show-lines.el      [any emacs]
;;   ========================================
;;   - Which adds permanent line numbers to buffer for a while
;;     (kind of "flash numbers" ).
;;
;;   --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

;;}}}
;;{{{ Bugs

;; ............................................................. &bugs ...
;; KNOWN LIMITATIONS
;; - Since this package creates separate _window_ to show the line numbers
;;   you may run into serious window configuration troubles if
;;   help/completion  + few more buffers keep costantly popping up.
;;   And if you use many split windows, changing from one to another
;;   may make the line number buffer associate to wrong buffer!
;; - There is no solution to these problems, just remember to turn off
;;   the line-nums when you need to do things somewhere else and turn
;;   it on when you come back. This may annoy people who like to see
;;   line number information all the time: I advice them to switch to
;;   column.el or setnu.el.
;; - The tarnsient-mark-mode causes little trouble because it makes
;;   the area in the line-num buffer to have 'region hilit sometimes.
;;   There is a bug in T-M-mode, which causes other buffers to show
;;   "extra" hilits sometimes. Just go and click the line-num buffer
;;   to get rid of the extra hilit.

;;}}}


(defvar line-nos-load-hook nil
  "*Hook run when file has been loaded.")


;;; ############################################## LINE-NUMBERS-TAKE-1 ###
;;;

(defun line-number-region (beg end &optional relative)
  "Insert line number labels at the beginning of every line in the region.
Each label will be the same length so that it's trivial to write a function
or keyboard macro to delete them at a later time.
where we are in the buffer.  Otherwise, begin counting from the starting
line number.
"
  (interactive "r\nP")
  (save-excursion
    (let ((num (if relative 1 (1+ (count-lines (point-min) beg))))
          (digitc
           (length (int-to-string (count-lines
                                   (if relative beg (point-min))
                                   end))))
          )
      (goto-char end)
      (end-of-line)
      (setq end (point-marker))
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end)
        (insert (substring "0000000" 0 (- digitc (length (int-to-string num))))
                (int-to-string num)
                ?:)
        (setq num (1+ num));    (forward-line 1)
        )))
)

;;; ----------------------------------------------------------------------
;;;
(defun line-number-buffer ()
  "Put line numbers at the start of every line in the buffer."
  (interactive)
  (line-number-region (point-min) (point-max) t))


(defconst v18 (string< emacs-version "19")
  "Check if we're in emacs 18")


;;; ############################################## LINE-NUMBERS-TAKE-2 ###
;;;
(defvar *win-min-width*         7
  "*The line number buffer width.")

(defvar *line-num-update*       1
  "Process update time in seconds.")

(defvar line-num-process        nil)
(defvar *line-num-buf*          nil)
(defvar prev-top                -99)
(defvar line-nos-sh-prg "line-nos.sh" ) ;;;  this used to "peep" program


;;; ----------------------------------------------------------------------
;;;
(make-variable-buffer-local 'line-toggle-var)
(defvar line-toggle-var nil
  "Used by function line-toggle to store the state.")

;;; ----------------------------------------------------------------------
;;;
(defun line-toggle (arg)
  "toggles display-line-nums hide-line-nums. ARG 1 hides allways
Uses global line-toggle-var/nil-t
"
  (interactive "P")
  (let (flag
        (bn (buffer-name))
        )
    (cond
     ((string-match "line-num" bn)
      (message "Can't toggle in line num buffer!")
      (other-window 1)
      )
     (t
      (if arg
          (setq flag arg)
        (setq line-toggle-var (not line-toggle-var))
        (setq flag line-toggle-var))
      (if flag (hide-line-nums) (display-line-nums)))
    )))


;;; ----------------------------------------------------------------------
;;;
(defun display-line-nums ()
  (interactive)
  (let* ((func 'transient-mark-mode)
         )
    (if (> window-min-width *win-min-width*)
        (setq window-min-width *win-min-width*))
    (split-window-horizontally *win-min-width*)
    (switch-to-buffer " Line-Numbers" t)
    (if (fboundp 'transient-mark-mode)
        ;;  turn off this mode in Emacs; the FUNC shuts up XE
        ;;  byteCompiler warnings.
        ;;
        (funcall func 0))
    (setq *line-num-buf* (current-buffer))
    (other-window 1)

    (if (not (and line-num-process
                  (eq (process-status line-num-process) 'run)))
        (progn
          (setq line-num-process
                (start-process "line-nums" nil line-nos-sh-prg
                               (int-to-string *line-num-update*)))
          (set-process-sentinel line-num-process 'line-num-sentinel)
          (set-process-filter line-num-process 'update-when-scrolled)))
    (process-kill-without-query line-num-process)
    ))

;;; ----------------------------------------------------------------------
;;;
(defun line-num-sentinel (ignore reason)
  (hide-line-nums)
  (message "The line number process died unexpectedly: %s." reason))

;;; ----------------------------------------------------------------------
;;;
(defun hide-line-nums ()
  (interactive)
  (if (and line-num-process
           (eq (process-status line-num-process) 'run))
      (progn
        (set-process-sentinel line-num-process nil)
        (delete-process line-num-process)))
  (let ((first-window (current-buffer))
        (first-time t))
    (while (or (not (eq first-window (current-buffer)))
               first-time)
      (setq first-time nil)
      (if (eq *line-num-buf* (current-buffer))
          (delete-window)
        (other-window 1)))
    ))


;;; ----------------------------------------------------------------------
;;;
(defun update-when-scrolled (ignore ignore)
  "When window has been scrolled, do something."
  (save-excursion
      (move-to-window-line 0)
      (if (and (not (eq (current-buffer) *line-num-buf*))
               (/= (point) prev-top))
          (let* ((cur-line-num  (count-lines (point-min) (point)))
                 (i (+ cur-line-num (window-height))))
            (setq prev-top (point))
            (other-window -1)
            (if (eq (current-buffer) *line-num-buf*)
                (progn
                  (newline (goto-line i))
                  (while (> i cur-line-num)
                    (setq i (1- i))
                    (forward-line -1)
                    (if (looking-at "$")
                        (progn
                          (if (< i 10)
                              (insert "  ")
                            (if (< i 100)
                                (insert ?\040)))
                          (insert (int-to-string i))))
                    )
                  (move-to-window-line 0)
                  (scroll-up (- i (count-lines (point-min) (point))))
                  (move-to-window-line 0)
                  ))
            (other-window 1))
        )))


(provide   'line-nos)
(run-hooks 'line-nos-load-hook)

;;; ............................................................. &eof ...
