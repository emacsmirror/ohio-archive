;;; filemenu.el --- Mode for buffers that present menus of files to visit

;; Copyright (C) 1998 Will Mengarini

;; Author: Will Mengarini <seldon@eskimo.com>
;; URL: <http://www.eskimo.com/~seldon>
;; Created: Sa 26 Jul 97
;; Version: 0.53
;; Keywords: abbrev, files, filemenu, menu

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package lets you set up one or more menus of names of files you visit
;; frequently, so you can select from that menu.  A mouse is supported but
;; not required.  The menus are stored in text files; they can have several
;; columns of file names, & can apply different default directories to
;; different groups of lines in the same file.  File names that contain
;; spaces, such as occur on Macs & on Windows {95,NT}, are supported.

;; To use this package, you need to hack 2 files: a file you'll create
;; containing a menu of names of files you access often, & your .emacs file,
;; where you'll autoload filemenu-mode & map loading your menu file to a key.

;; By default, the name of your menu file is "~/filemenu".  I suggest that to
;; get started, you copy this text to that file:
;;   ~/
;;   my-novel.txt   "My Invention.txt"    my-checklist.chl
;;   c:\emacs\emacs-19.34\lisp\simple.el
;; Delete the leading semicolons by putting the cursor in the top left corner
;; of the file, pressing C-SPC to set the mark, moving it to line 3 column 5
;; over the "c" in "c:", & pressing C-x r k to run (kill-rectangle).  Then
;; modify the file names to name files you really have on your system.  You
;; might as well leave the quotes around the middle-column file name just to
;; remind yourself that that format is available (you may need it on
;; Microsoft systems).  On a Microsoft OS, you can use either a virgule or a
;; bash as a directory delimiter; the bash need not be doubled or quadrupled
;; (as it is to cope with multiple levels of quoting in some contexts).  On a
;; Unix system, use a virgule.  "~/" works with all systems.  Note that
;; because it's alone on a line, it sets the default directory for all file
;; names on all following lines until another alone-on-a-line path ending in
;; a directory separator occurs; however, files that have their own absolute
;; directory specifiers are unaffected.  To edit the third line of that file
;; to reflect where your ELisp library actually is, type C-h v load-path; the
;; correct value is in there someplace; copy it.  That'll be enough to let
;; you experiment with filemenu; later you can edit ~/filemenu to add all the
;; files you visit frequently.  (The simple.el library file defines the most
;; frequently used Lisp functions that are standard in GNU Emacs, such as
;; `newline-and-indent'.  If you're a Lisp hacker you'll want to read these
;; occasionally just to understand how Emacs works.  This is also a
;; convenient way of accessing other library files, since once you've loaded
;; this one, its directory becomes the default directory for find-file, and
;; you can use completion to name other files.  (Using dired on the ELisp
;; library can be a bad idea except on fast machines because the library is
;; so big; I've seen building the dired buffer take > 30 s.)  Sparse package
;; documentation often makes it necessary to "Use the Source, Luke" to figure
;; out how to use Emacs features effectively.)

;; Next, put these lines in your .emacs (_emacs on Microsoft systems) file:
;;   (autoload 'filemenu "filemenu"
;;     "Load the file menu named by the variable filemenu-file-name."
;;     t)
;;   (autoload 'filemenu-mode "filemenu"
;;     "Major mode for picking a file to edit from a buffer offering a menu."
;;     t)
;;   (add-to-list 'auto-mode-alist '("[:/\\]filemenu\\'" . filemenu-mode))
;;   (global-set-key (read-kbd-macro "C-c f") 'filemenu)
;; You can modify the argument to (read-kbd-macro) to whatever is convenient
;; for you.

;; Next, move the cursor to the first of those (autoload) lines, type C-SPC
;; to set the mark, move it to the beginning of the line after the
;; `global-set-key', & type M-x eval-region.  You should then see "nil" in
;; the echo area; that's the value of the final form evaluated.  If you see
;; an error message, there's a typo.  You won't need to do this `eval-region'
;; again; in the future, when you load Emacs, that region will be evaluated
;; along with the rest of your .emacs at load time.  This is just for adding
;; lines to .emacs during a running session.

;; Now you're ready to try filemenu.  Type C-c f (or whatever you modified
;; the `read-kbd-macro' arg to).  You should find yourself in ~/filemenu,
;; which is the buffer into which you typed those 3 sample lines; the mode
;; line should show "Filemenu" ("T:Filemenu" on Microsoft systems) as the
;; major mode.  If anything else happens, review the instructions to be sure
;; you followed them correctly; if it still doesn't work, send me hate mail.

;; Now you're ready to select from the menu.  Try it first just using the
;; keyboard: use the usual motion keys to put point before or inside any of
;; those file names, and press RET.  The effect will be to visit the file.
;; Type C-x k to kill the buffer you just visited, & C-c f again (or
;; whatever) to get back to the filemenu buffer; then type v instead of RET,
;; & the effect will be to visit the file in view-mode.  Kill it again.  If
;; you're familiar with dired, you can try positioning point on the first
;; line (the "~/" line) & pressing RET; the effect will be to run dired on
;; your home directory & put you in the dired buffer.

;; If you're on a system that has a mouse, now try it.  Type C-c f or
;; whatever to get back to the filemenu buffer, move the mouse pointer to
;; some file, & press mouse-1 (which is usually the left button).  The effect
;; will be to visit the file for editing.  Kill it, C-c f again, & press
;; mouse-2 (which is usually the middle button, or a click on the mouse wheel
;; if you have an IntelliMouse, or the left & right buttons simultaneously if
;; you only have a 2-button mouse).  You'll visit the file in view-mode.

;; That's it; filemenu is installed.  Hack ~/filemenu to add the names of
;; files you visit frequently.  Watch out for a gotcha there: when you're
;; visiting that file, you're not in text-mode, you're in filemenu-mode, so
;; RET & v don't insert characters, they visit files.  Get around this by
;; preceding them with C-q.  If you're typing in a large # of file names at
;; once this could be a nuisance; in that case, you could type them into the
;; *scratch* buffer & cut-&-paste them to ~/filemenu.

;; Some customizations are possible; most people won't need these.

;; If you don't like the name "~/filemenu", put a line like
;;   (setq filemenu-file-name "c:/My Emacs Stuff/My Filemenu File.fmn")
;; or whatever pleases you in your .emacs or _emacs file.  Note that if you
;; used bashes instead of virgules as your directory separators there (that's
;; Lisp code, not filemenu buffer text) you'd need to double them:
;;   (setq filemenu-file-name "c:\\My Emacs Stuff\\My Filemenu File.fmm")
;; Virgules work with Emacs on Microsoft systems.

;; If you don't like the keymappings of filemenu-mode, put code like
;;   (defun 'my-filemenu-mode-hook ()
;;     (local-set-key (read-kbd-macro "H-M-f") 'filemenu-find-file-at-point)
;;     (local-set-key [down-mouse-3] 'mouse-set-point)
;;     (local-set-key [mouse-3] 'filemenu-view-file-at-point))
;;   (add-hook 'filemenu-mode-hook 'my-filemenu-mode-hook)
;; in your .emacs file.

;; Finally, if you want multiple filemenu files, take a deep breath, then
;; copy all of this code literally to the top of your .emacs file:
;;
;;   (require 'cl)
;;   
;;   (defmacro interactive-lambda (argstring &rest list)
;;     (if (equal argstring "")
;;         `(lambda () (interactive) ,@list)
;;       `(lambda ,(car list) (interactive ,argstring) ,@(cdr list))))
;;   
;;   (defmacro global-set-key-to-interactive-lambda (key argstring &rest list)
;;     `(global-set-key ,key (interactive-lambda ,argstring ,@list)))
;;   
;;   (fset 'K 'global-set-key-to-interactive-lambda)
;;   
;;   (defun multiple-actions-for-multiple-taps-on (key-sequence actions)
;;     (if (stringp key-sequence) (callf read-kbd-macro key-sequence))
;;     (let (keys)
;;       (while
;;           (progn
;;             (eval (car actions))
;;             (callf cdr actions)
;;             (setq keys (read-key-sequence nil))
;;             (and (equal keys key-sequence)
;;                  actions)))
;;       (setq unread-command-events (listify-key-sequence keys))))
;;   
;;   (put 'multiple-actions-for-multiple-taps-on
;;        'lisp-indent-function
;;        'defun)
;;
;; Then do an `eval-region' as before on the code you just inserted.
;;
;; Next choose a way of recognizing files that are filemenu files.  On
;; Microsoft systems, this'll be the "extension" of the file, the chars that
;; follow its final dot; I'll assume that's your situation, since if you're
;; on a Unix system you must be smart enough to figure out for yourself how
;; to modify these instructions appropriately.  Let's say you chose ".fmn"
;; as the extension.  Add this line to your .emacs after the other
;; filemenu-related add-to-list line that you already put in:
;;   (add-to-list 'auto-mode-alist '("\\.fmn\\'" . filemenu-mode))
;; Then replace the global-set-key line you already put in with lines
;; equivalent to these (modify the find-file arguments to name your files):
;;   (K "\C-cf" "" (multiple-actions-for-multiple-taps-on "f"
;;                   '((filemenu)
;;                     (find-file "~/WorkMenu.fmm")
;;                     (find-file "~/PlayMenu.fmm")
;;                     (find-file "~/HomeMenu.fmm"))))
;; Do an `eval-region' on the `add-to-list' line & the `K' lines.
;; The effect will be that pressing C-c f will invoke (filemenu) as before,
;; but each subsequent press of f will move to one of the files in
;; "~/{Work,Play,Home}Menu.fmm".  Pressing any other key than f returns f to
;; its normal meaning; f also regains its normal meaning after the final
;; file in the list, "~/HomeMenu.fmm", has been visited.
;; As Ross Perot would say, "it's that simple".
                                
;;; Code:

;; I know about ffap.el, but it has a different feature set.

(defvar filemenu-file-name "~/filemenu"
  "*Path to default file menu used by the filemenu command.")

(defvar filemenu-mode-hook nil
  "Normal hook run after loading a buffer in filemenu mode.")

(eval-when-compile (require 'cl))

;;;###autoload
(defun filemenu-mode ()
  "Major mode for picking a file to edit from a buffer offering a menu.
The intended use is on a file you will create listing the files you visit
so frequently you want them on a menu.  The command 'filemenu visits that
file in filemenu-mode.  An example of the format:
   /etc/passwd
   /home/myself/
      diary  \"File with spaces in its name.Doc\"  c:\\DOS\\Nibbles.Bas
      foo.spoo  woo.hoo  /usr/local/bin/ballyhoo.u  oops.oof
   /etc/hosts\\<filemenu-map>
Any # of file names may appear on a line.  If the name contains no
directory separator characters (virgules or backslashes), any preceding
line that ended with such a character is used as the directory.
Indentation is optional & ignored.  You pick a file to visit by
positioning point on its name & typing \\[filemenu-find-file-at-point],
or by mouse-clicking it.  Turning on filemenu mode calls the value of the
variable `filemenu-mode-hook', if that value is non-nil.

\\{filemenu-map}"
;;Note that the above documentation is an ELisp string; the bashes are
;;doubled there only because of that; use single bashes in ~/filemenu.
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'filemenu-mode
	mode-name "Filemenu")
  (use-local-map filemenu-map)
  (run-hooks 'filemenu-mode-hook))

;;;###autoload
(defun filemenu ()
  "Load the file named by the variable filemenu-file-name."
  (interactive)
  (find-file filemenu-file-name)
  (filemenu-mode))

(defvar filemenu-map nil
  "Keymap for filemenu mode.")

(if filemenu-map
    ()
  (setq filemenu-map (make-sparse-keymap))
  ;; Remember that M-o whatever can still be used for orthodox functions,
  ;; if you're using my orthodox.el package.
  (define-key filemenu-map "\C-m"         'filemenu-find-file-at-point)
  (define-key filemenu-map [mouse-1]      'filemenu-find-file-at-point)
  ;; That alone isn't good enough because the global [down-mouse-1] mapping
  ;; can eat the [mouse-1] event, so it must be modally overridden:
  (define-key filemenu-map [down-mouse-1] 'mouse-set-point)
  ;; I find that I'm accustomed to using [mouse-2] as in *Apropos* & dired,
  ;; but I'd also like the option of view-mode, so:
  (define-key filemenu-map "v"            'filemenu-view-file-at-point)
  ;; Use C-q v to enter a literal "v"
  (define-key filemenu-map [down-mouse-2] 'mouse-set-point)
  (define-key filemenu-map [mouse-2]      'filemenu-view-file-at-point)
  )

(defun filemenu-find-file-at-point ()
  (interactive)
  (filemenu-visit-file-at-point-using 'find-file))

(defun filemenu-view-file-at-point ()
  (interactive)
  (filemenu-visit-file-at-point-using 'view-file))

(defun filemenu-visit-file-at-point-using (function)
  (or (symbolp function) (error "Internal failure 3322"))
  (if (and (bolp) (eolp)) (error "Empty line"))
  (let (point-bol point-start point-end file)
    (save-excursion
      (beginning-of-line)
      (if (looking-at "[ \t]+$") (error "Line is all whitespace"))
      (setq point-bol (point)))
    (save-excursion
      ;; Now we must get inside any quotes.  Because the quote search starts
      ;; with a search-backward, it's OK to be on the trailing quote of
      ;; "file", but not on the leading one; so if we're on a quote followed
      ;; by nonwhitespace we must move forward.  Also, we could be invoked
      ;; after the last quoted string on the line; in that case we must move
      ;; backward to the closing quote, or the search-backward intended
      ;; to find the opening quote will find the closing one instead.
      (when (< (current-column) (current-indentation))
        (back-to-indentation))
      (if (looking-at "\"[^ \t\n]")
          (forward-char)
        (while (looking-at "[ \t\]*$") (backward-char)))
      ;; Buglettino not worth fixing: in a line like
      ;;   "file1"  "file2"  "file3"
      ;; this works if dot is before or after the nonwhitespace on the line,
      ;; but if it's invoked *between* files, it'll interpret the surrounding
      ;; quotes as quoting a file whose name is whitespace.  It could avoid
      ;; that by counting quotes & realizing an even # preceded dot, but hey.
      (if (search-backward "\"" point-bol t)
          (progn
            ;; There was one quote, so require the other
            (forward-char)
            (setq point-start (point))
            (search-forward "\"")
            (backward-char)
            (setq point-end (point)))
        (re-search-backward "^\\|[ \t]")
        (if (looking-at "[ \t]")
            (forward-char))
        (setq point-start (point))
        (re-search-forward "\\([ \t]\\)\\|$")
        (if (match-beginning 1)
            (backward-char))
        (setq point-end (point)))
      (setq file (buffer-substring point-start point-end))
      (if (= (length file) 0) (error "No file name specified"))
      ;; If this file name included no directory, search backward in the
      ;; menu buffer for a line defining the directory & prepend it.
      (if (string-match "[/\\]" file)
          ()
        (when (re-search-backward "[/\\]$" nil t)
          (beginning-of-line)
          (setq point-bol (point))
          (end-of-line)
          (setq file (concat (buffer-substring point-bol (point)) file)))))
    (if (string-match "[*?]" file)
        (progn
          (message "(dired %S)" file)
          (dired file))
      (message "(%S %S)" function file)
      (let ((jiggle-enabled nil));see my jiggle.el
        (bury-buffer))
      (funcall function file))))

(provide 'filemenu)

;;; filemenu.el ends here
