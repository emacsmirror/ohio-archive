; Path: dg-rtp!rock!mcnc!stanford.edu!leland.Stanford.EDU!leland!jsegal
; From: jsegal@elaine5.Stanford.EDU (Jonathan Segal)
; Newsgroups: gnu.emacs.sources
; Subject: Improve c-macro-expand
; Date: 22 Jun 91 18:32:03 GMT
; Organization: The antithesis of chaos.
; 
; 
; Hello.
; 
; I have modified the c-macro-expand function (defined in cmacexp.el) so
; that one can specify an include path for header files, instead of
; depending upon the header files to reside in the same directory as the
; .c files you are editing.  One can also configure it to use a
; preprocessor other than /lib/cpp (for instance, if you want to use the
; gnu cpp).  There is more information in the description section of the
; comments below.
; 
; This is a repost, as it didn't seem to make it past my local site last
; time...
; Enjoy!!!
; -----------cut here (and don't forget the .sig at the bottom) --
;;; -*-Emacs-Lisp-*-
;;;%Header
;;; New and Improved c-macro-expand, c-macexp.el
;;; Copyright (C) 1991 Free Software Foundation, Inc.
;;; Modified 1991 Jonathan Segal, jsegal@oracle.com

;;; LCD Archive Entry:
;;; c-macexp|Jonathan Segal|jsegal@oracle.com
;;; |This is a modified version of the c-macro-expand
;;; |91-06-22||~/functions/c-macexp.el.Z

;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.
;;;

;;; DESCRIPTION:  This is a modified version of the c-macro-expand
;;; initially distributed with the standard GNU Emacs distribution.  I
;;; have modified it to allow you to specify include file search
;;; paths, if you have relevant .h files which are not in the same
;;; directory as the c files you are editing.  To use, in your .emacs
;;; file set the variable c-macexp-include-dirs to be a list of
;;; directories to be searched for your include files.  If you wish,
;;; you can also set the variable c-macexp-cpp if you do not want to
;;; use the default cpp in /lib/cpp, and if you use a really funky
;;; preprocessor which uses a flag other than -I to specify
;;; directories to be searched for include files, you can set that in
;;; c-macexp-include-prefix.  All these variables have standard
;;; default settings.   To invoke, select a region of code for which
;;; you want to see the preprocessed output, and call the function
;;; c-macro-expand  (In my c-mode hook I bind C-c C-m to this
;;; function).  It will spawn off a cpp process, and create a new
;;; buffer containing the preprocessed version of the code you have
;;; selected.  This can be useful for debugging various pre-compiler
;;; tricks, as well as ensuring that you have your macros correct.

;;; This file could replace the current cmacexp.el in the standard
;;; distribution (upon which this is based).

;;; If you have any problems, I can be reached at jsegal@oracle.com

(defvar c-macexp-include-dirs 
  '("/usr/include" "/usr/include/sys" ".")
  "*A list of directories to be searched for include files by c-macro-expand"
)

(defvar c-macexp-include-prefix "-I" 
"*The prefix for cpp to know the include files.  Unless you are using a weird
cpp, this should probably be \"-I\""
)

(defvar c-macexp-cpp "/lib/cpp" "*Command to use for preprocessor.  Should probably be /lib/cpp or some variant")

(defun c-macro-expand (beg end)
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor.
It will use the preprocessor specified in the variable c-macexp-cpp,
and will search for include files in the path specified in
c-macexp-include-dirs.  c-macexp-include-prefix will be prepended to
each directory specified, and is probably \"-I\""
  (interactive "r")
  (let ((outbuf (get-buffer-create "*Macroexpansion*"))
	(tempfile "%%macroexpand%%")
        proc-arg-list
	process
	last-needed)
    (save-excursion
      (set-buffer outbuf)
      (erase-buffer))
    (setq proc-arg-list (append (list "macros" outbuf c-macexp-cpp)
	(c-macexp-add-prefixes c-macexp-include-dirs c-macexp-include-prefix)
    ))
    (setq process (apply 'start-process proc-arg-list
    ))
    (set-process-sentinel process '(lambda (&rest x)))
    (save-restriction
      (widen)
      (save-excursion
	(goto-char beg)
	(beginning-of-line)
	(setq last-needed (point))
	(if (re-search-backward "^[ \t]*#" nil t)
	    (progn
	      ;; Skip continued lines.
	      (while (progn (end-of-line) (= (preceding-char) ?\\))
		(forward-line 1))
	      ;; Skip the last line of the macro definition we found.
	      (forward-line 1)
	      (setq last-needed (point)))))
      (write-region (point-min) last-needed tempfile nil 'nomsg)
      (process-send-string process (concat "#include \"" tempfile "\"\n"))
      (process-send-string process "\n")
      (process-send-region process beg end)
      (process-send-string process "\n")
      (process-send-eof process))
    (while (eq (process-status process) 'run)
      (accept-process-output))
    (delete-file tempfile)
    (save-excursion
      (set-buffer outbuf)
      (goto-char (point-max))
      (re-search-backward "\n# [12] \"\"")
      (forward-line 2)
      (while (eolp) (delete-char 1))
      (delete-region (point-min) (point)))
    (display-buffer outbuf)))

(defun c-macexp-add-prefixes (list pref) 
  "*Passed a list of strings, will return a list with each of the strings
prefixed by the string pref"
  (if (null list)
    nil
    (cons (concat pref (car list)) (c-macexp-add-prefixes (cdr list) pref))
))

(provide 'c-macexp)
