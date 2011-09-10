;;; $Id: foldingo.el,v 1.29 1998/09/02 11:40:05 queinnec Exp $
;;; Copyright (C) 1995-97 by C.Queinnec (University Paris 6 & INRIA)

;;; LCD Archive Entry:
;;; fold|Christian Queinnec|Christian.Queinnec@inria.fr|
;;; A minor folding mode with mouse/menu/face support.|
;;; $Date: 1998/09/02 11:40:05 $|$Revision: 1.29 $|
;;; ~/misc/foldingo|

;;; PORTED to Xemacs by Christoph Ley <ley@physik.rwth-aachen.de>.

;;; PLUS ADD ON by Georg Greve (greve@fusebox.hanse.de)
;;; (June 4th 1996) --- See "Commentaries/About ADD ON" section below.

;;; MODIFIED by Dan Nicolaescu <done@ece.arizona.edu> (Feb 16th 1997)
;;; to use overlays instead of selective-display. Unfortunately this
;;; breaks Xemacs compatibility (lattest foldingo compatible version 
;;; is 1.24 which you may find on:
;;;   ftp://ftp.inria.fr/INRIA/Projects/icsla/WWW/foldingo-1.24.el

;;; This file is not part of GNU Emacs.

;;;{{{ Commentaries

;;; This package was inspired by outline mode, folding.el (from Jamie
;;; Lokier), tinyfold.el (Jari Aalto), foldout.el (Kevin Broadey).
;;; Advantages with respect to other packages:
;;;  -1- Menu support
;;;  -2- Simple mouse behavior to open/close folds
;;;  -3- Programmable faces to identify open or closed folds
;;;  -4- Customizable identification of folds
;;;        You may use explicit folds bounded by {{{ and }}}
;;;        You may also have implicit folds depending on the mode (for 
;;;        instance in Lisp- or C-based mode, long definitions are folded).

;;; The best way to understand how this package works is to use
;;; Fold-mode itself.  First, load or evaluate this file, then in the
;;; Tools menu, choose "Fold" then "Enter Fold mode". Now choose "Fold
;;; whole buffer". You should see this file now folded and, in
;;; particular, this comment will be folded and no longer readable! To
;;; open or hide particular folds, set the dot where you want and call
;;; the appropriate menu items. Alternatively, you can use
;;; Shift-mouse-1 to toggle the state of a fold: click on the headline
;;; of the fold. You may also try this on the }}} closing a fold.

;;;{{{ Remarks
;;; This code was tested with Emacs 19.29. It also works on read-only 
;;; buffers as well as on Lisp, Scheme, C, Caml files.
 
;;; I also chose not to disturb dot position whenever possible but this
;;; will warp your cursor when folds are closed/opened.

;;; If {{{ and }}} do not match well, a message will be shown for 2
;;; seconds but the cursor will not altered.

;;; Observe that the status of subfolds is remanent even if hidden.

;;; If you make new folds appear whether explicitly or implicitly, 
;;; reenter Fold mode (from the tools menu) to take them into account.

;;; ``fol dingo'' means in French "crazy mad". No relation to what this 
;;; file does, I only had to choose a free name!

;;; After folds are found, there is a curious noticeable delay. It may
;;; be due to the installation of the overlays, but it also seems to
;;; be due to the numerous messages that are piped into the *Messages*
;;; buffer. How to get rid of them ???

;;;}}}

;;;{{{ Installation
;;; When you're convinced, byte-compile this file (you can even byte-compile
;;; it while folded), and just add to your .emacs:
;;;       (require 'foldingo)
;;;
;;; To automatically activate the fold mode when a file contains these
;;; special {{{ and }}} markers you can add a hook to find-file-hooks 
;;; as in:
;;;    (add-hook 'find-file-hooks 'install-fold-mode-if-needed t)
;;; Caution: this hook must be appended and not prepended since fold-mode
;;; uses ^M characters and these characters may trigger some other hooks 
;;; trying to convert files from DOS.
;;;}}}

;;;{{{ About ADD ON by Georg Greve
;;; ADD ON contains:
;;;        - Folding marks for "c++-mode" and "c++-c-mode" 
;;;           (located at the "c-mode" folding marks)
;;;        - Commands for inserting C(++) folds (accessible via menu)
;;;        - Keybindings for the important functions
;;;
;;; Keybindings are:
;;; C-c f e : (Re)Enter fold mode
;;; C-c f a : (Re)Enter fold mode and close all folds
;;; C-c f o : Open current fold
;;; C-c f h : Hide current fold
;;; C-c f f : Close all folds
;;; C-c f i ; insert new C(++) folding
;;; C-c f c ; end/close new C(++) folding
;;;
;;; The keybindings are global, but the "C-c f ?" weren't defined before,
;;; so there shouldn't be any problems.
;;;
;;; For TeX users:
;;; I made my "tex-mode.el" override the keybindings for C-c f i and
;;; C-c f c with the according definitions for TeX. This way I usually
;;; got C(++) foldings and TeX foldings when in (La)TeX mode
;;; accessible via the keyboard.
;;;
;;; Here is what you got to do if you want the same:
;;;
;;; At the LaTeX keymap definition (in file "tex-mode.el")
;;;                      (defun tex-define-common-keys (keymap)...
;;; you insert the following two lines:
;;;            (define-key keymap "\C-cfi"   'insert-new-latex-fold)
;;;            (define-key keymap "\C-cfc"   'close-latex-fold)
;;;
;;; then you append the following to the "tex-mode.el" (I put it just before 
;;; the (run hooks...) statement:
;;;
;;;    (defun close-latex-fold ()
;;;      "Closing a fold for LaTeX."
;;;      (interactive "*")
;;;      (if (not (bolp))
;;;          (insert ?\n))
;;;      (insert "%%}}}%%\n")
;;;    )
;;;
;;;    (defun insert-new-latex-fold (foldname)
;;;      "Insert a new fold for LaTeX."
;;;      (interactive "*sTitle of the new fold: ")
;;;      (if (not (bolp))
;;;          (insert ?\n))
;;;      (insert (format "%%%%{{{  %s  %%%%\n"
;;;                      foldname))
;;;    )
;;;
;;; Now you byte-compile the "tex-mode.el" again and everything should
;;; work fine.
;;;
;;; I think that should be enough for now, have fun,
;;;                                              Georg (greve@fusebox.hanse.de)
;;;}}}

;;;{{{ Repository
;;; Bugs, remarks etc should be sent to 
;;;     Christian.Queinnec@inria.fr
;;; Newer versions will be sent to the LCD Archive but may appear earlier on:
;;;     ftp.inria.fr:INRIA/Projects/icsla/Miscellaneous/foldingo.el
;;; Other Emacs packages can be found with World Wide Web with URL:
;;;     ftp://ftp.inria.fr/INRIA/Projects/icsla/WWW/elisp.html
;;;}}}
;;;}}}

;;;{{{ Code

;;;    {{{ Compatibility with XEmacs (part I)
;;; By Christoph Ley <ley@physik.rwth-aachen.de>

(defun check-for-XEmacs ()
      (string-match "XEmacs" emacs-version) )

;;;}}}
;;;   {{{ Variables
;;;      {{{ Customizable variables

(defvar fold-begin-regexp nil
  "Regexp identifying where an explicit fold begins.
If not specified it will be computed when entering `fold-mode' as follows:
  ^ comment-start+ space* [fold-end space*] fold-begin
This is a buffer-specific variable." )
(make-variable-buffer-local 'fold-begin-regexp)

(defvar fold-end-regexp nil
  "Regexp identifying where an explicit fold ends.
If not specified it will be computed when entering `fold-mode' as follows:
  ^ comment-start+ space* fold-end
This is a buffer-specific variable." )
(make-variable-buffer-local 'fold-end-regexp)

(defvar fold-open-face nil
  "Face for open folds. 
Usually nil not to make them apparent but it is also possible to set 
the background color to identify fold regions. See also fold-closed-face." )

(defvar fold-closed-face (and window-system 'highlight)
  "Face for closed folds ie the headline of a fold. 
With default binding, you just click on it to open it.
See also fold-open-face." )

;(defvar fold-end-face (and window-system 'highlight)
;  "Face for end of folds markers ie }}}.
;This is mainly to experiment with mouse-face property." )

(defvar fold-compute-folds 'fold-compute-explicit-folds
  "The function that computes the folds present in a buffer. 
These folds may be explicit ie delimited between fold-begin and 
fold-end or implicit for instance in Lisp mode, all definitions of 
more than 9 lines constitute a fold.

The default value of this variable is:
      fold-compute-explicit-folds
that looks for all the explicit folds of a file. To these folds
may be added more implicit folds (for long definitions for instance).
This is done in Lisp based modes by function:
      fold-compute-implicit-defun-folds
While in C mode, use rather:
      fold-compute-implicit-c-folds

The best way to automatically set this variable in a major mode is to put it
on the plist of the symbol of the mode under property fold-compute-folds.
" )
(make-variable-buffer-local 'fold-compute-folds)
  
(defvar fold-implicit-fold-threshold 15
  "When a definition is that big it is implicitly turned into a fold.
This is used in Lisp- or C-based modes.
This variable is buffer-specific." )
(make-variable-buffer-local 'fold-implicit-fold-threshold)

(defvar fold-comment-start nil
  "This regexp identifies the beginning of a comment.
All paragraphs not starting with this regexp are considered as definitions.
This variable may be customized for other modes. This may be used in C
mode but also in Caml mode." )
(make-variable-buffer-local 'fold-comment-start)

(defvar fold-implicit-closed-fold-height 1
  "This is the height (in lines) of an implicit fold when closed.
1 is good for Lisp, 1 or 2 may be appropriate for C." )
(make-variable-buffer-local 'fold-implicit-closed-fold-height)

(defvar fold-closed-text-properties 
  (list 'read-only t)
  "These are the text properties of a closed fold." )
(make-variable-buffer-local 'fold-closed-text-properties)

(defvar fold-mode-hook nil
  "Hooks to be run when `fold-mode' is activated in a buffer." )

;;;   }}}{{{ Internal variables

(defvar fold-mode nil
  "Boolean value telling if Fold-mode is active or not.
This is a buffer-specific variable." )
(make-variable-buffer-local 'fold-mode)

(defvar fold-mark nil
  "Marks the dot.
This marker is used to record the position of the dot when clicking
so the dot may be left at its current position." )
(make-variable-buffer-local 'fold-mark)

(defvar fold-depth-description " Fold"
  "String telling that Fold mode is active.
In some future it may describe more precisely the current fold.
This is a buffer-specific variable." )
(make-variable-buffer-local 'fold-depth-description)

(defvar fold-end-or-begin-regexp nil
  "Regexp to identify the beginning or the end of a fold.
It is automatically computed from `fold-begin-regexp' and `fold-end-regexp.'
This is a buffer specific variable." )
(make-variable-buffer-local 'fold-end-or-begin-regexp)

(defvar fold-folds-list nil
  "List of all the current folds of the current buffer.
This list has the following structure:
     (   (overlay closed faceC faceO subfolds) ... )
overlay marks the region delimiting the fold (start and end may be
obtained from the overlay). The overlay should start on a beginning
of line and finish on an end of line.
closed is a boolean indicating whether the fold is closed or not.
faceC is the face to use to represent the closed fold.
faceO is the face to use to represent the open fold.
subfolds is the list of subfolds." )
(make-variable-buffer-local 'fold-folds-list)

(defvar fold-begin "{{{"
  "String marking the beginning of an explicit fold.
This string generally appears at the beginning of a comment or after a 
FOLD-END string. There may be additional whitespaces or comment signs before.
In Lisp mode, for instance ;;;{{{ or ;}}}{{{ both indicate that an
explicit fold is starting.

Not advised currently to change it." )

(defvar fold-end "}}}"
  "String marking the end of an explicit fold.
This string generally appears at the beginning of a comment with possibly
some whitespaces or comment signs before. 
In Lisp mode, for instance ;;;}}} is an explicit fold end.

Not advised currently to change it." )

(defvar fold-keymap 
  (if (check-for-XEmacs)
      (make-sparse-keymap)
    (make-sparse-keymap "Fold") )
  "Keymap of the minor mode `Fold-mode.'
It currently only contains mouse binding to detect clicks on headlines 
to open/close folds. See fold-mode documentation." )

(defvar fold-menu-map 
  (if (check-for-XEmacs)
      (make-sparse-keymap)
    (make-sparse-keymap "Fold") )
  "Menu map of the minor mode `Fold-mode'. 
It currently contains some menu items to enter/exit fold mode,
hide/open folds, fold/unfold the whole buffer.
See fold-mode documentation." )

;;;    }}}
;;;}}}
;;;    {{{ Compatibility with XEmacs (part II)

;;; from ediff mode ...
(if (check-for-XEmacs)
    (progn
      (fset 'foldin-read-event (symbol-function 'next-command-event))
      (fset 'foldin-overlayp (symbol-function 'extentp))
      (fset 'foldin-make-overlay (symbol-function 'make-extent))
      (fset 'foldin-delete-overlay (symbol-function 'delete-extent))
      (fset 'foldin-overlay-buffer (symbol-function 'extent-buffer))
      (fset 'foldin-overlay-get (symbol-function 'extent-property)))
  (fset 'foldin-read-event (symbol-function 'read-event))
  (fset 'foldin-overlayp (symbol-function 'overlayp))
  (fset 'foldin-overlayp (symbol-function 'overlayp))
  (fset 'foldin-make-overlay (symbol-function 'make-overlay))
  (fset 'foldin-delete-overlay (symbol-function 'delete-overlay))
  (fset 'foldin-overlay-buffer (symbol-function 'overlay-buffer))
  (fset 'foldin-overlay-get (symbol-function 'overlay-get)))

(defsubst foldin-overlay-start (overl)
  (if (foldin-overlayp overl)
      (if (check-for-XEmacs)
	(extent-start-position overl)
	(overlay-start overl))))
	
(defsubst foldin-overlay-end  (overl)
  (if (foldin-overlayp overl)
      (if (check-for-XEmacs)
	(extent-end-position overl)
	(overlay-end overl))))

(defmacro foldin-buffer-live-p (buf)
  (` (and (, buf) (get-buffer (, buf)) (buffer-name (get-buffer (, buf))))))

(defun foldin-overlay-put (overlay prop value)
  "Calls `overlay-put' or `set-extent-property' depending on Emacs version.
Checks if overlay's buffer exists."
  (if (foldin-buffer-live-p (foldin-overlay-buffer overlay))
      (if (check-for-XEmacs)
	  (set-extent-property overlay prop value)
	(overlay-put overlay prop value))
    (foldin-delete-overlay overlay)))

;;;}}}
;;;   {{{ Installation

;;; Install fold-mode as a pervasive minor mode (as a keymap, a menu
;;; and a short string in the modeline).

(let ((v (assoc 'fold-mode minor-mode-alist)))
  (if (not v)
      (progn
        (setq minor-mode-alist
              (cons (list 'fold-mode 'fold-depth-description)
                    minor-mode-alist ) ) ) ) )

(let ((v (assoc 'fold-mode minor-mode-map-alist)))
  (if (not v)
      (progn
        (setq minor-mode-map-alist
              (cons (cons 'fold-mode fold-keymap)
                    minor-mode-map-alist ) ) ) ) )

;;;}}}{{{ Data structure

(defun fold-make (beg end closed closed-face open-face subfolds height)
  "Build a fold"
      (vector "Fold" (foldin-make-overlay beg end)
	      closed closed-face open-face subfolds height) )


(defmacro fold-overlay (fold)
  (list 'aref fold 1) )

(defmacro fold-closed-p (fold)
  (list 'aref fold 2) )

(defmacro fold-set-closed-p (fold bool)
  (list 'aset fold 2 bool) )

(defmacro fold-closed-face (fold)
  (list 'aref fold 3) )

(defmacro fold-open-face (fold)
  (list 'aref fold 4) )

(defmacro fold-subfolds (fold)
  (list 'aref fold 5) )

(defmacro fold-set-subfolds (fold bool)
  (list 'aset fold 5 bool) )

(defmacro fold-height (fold)
  (list 'aref fold 6) )

;;;}}}{{{ Functions
;;;      {{{ Enter/Leave Fold-mode

(defun fold-mode (&optional arg)
  "Toggle fold-mode (or enter it if ARG is true).

Fold mode adds a new menu item under the Tools menu of the menubar.
In this menu:
  You may (re)enter or leave the Fold mode.
  You can hide or show all the folds of a buffer.
  You can hide or show the current fold containing the dot.

You can also click the mouse (more precisely \\[fold-mouse-handle])
on either }}} or {{{ to show/hide the associated fold.

Fold mode can be customized with the following variables (whose default
value for a given mode are stored in its Plist):
      fold-begin-regexp
      fold-end-regexp
      fold-open-face
      fold-closed-face
      fold-compute-folds
      fold-implicit-fold-threshold
      fold-comment-start
      fold-implicit-closed-fold-height
      fold-mode-hook.
"
  (interactive "P")
  (condition-case error-object
      (progn
        (setq fold-mode (if (null arg)
                            (not fold-mode)
                          (> (prefix-numeric-value arg) 0) ))
        ;; release all overlays.
        (fold-release-all-folds)
        (if fold-mode
            (progn
              (setq 
	       ;;selective-display          t
	       line-move-ignore-invisible t 
	       buffer-invisibility-spec   '((t . t))
	       fold-mark                  (make-marker) )
              ;; let the user customize mode
              (run-hooks 'fold-mode-hook)
              ;; compute if absent the regexps that identify fold boundaries.
              (fold-guess-parameters)
              ;; compute folds
              (message "Looking for folds...")
              (funcall fold-compute-folds)
              (let ((n (length fold-folds-list)))
                (if (= 0 n)
                    (progn (message "Found 0 folds: Fold-mode exited")
                           (fold-mode nil) )
                  ;;(message "Found %d folds" n)
		  ) ) )
          (progn
            (setq 
	     ;;selective-display          nil
             line-move-ignore-invisible nil 
             ;;buffer-invisibility-spec   t 
             )
            t ) ) )
    (error (setq fold-mode nil)
           ;; Leave time to see the error message.
           (message "Error while folding %s" error-object)
           ;(sleep-for 2)
           ;(message "Cannot activate Fold-mode!")
           ) )
  ;; refresh modeline.
  (force-mode-line-update) )

(defun fold-guess-parameters ()
  "Setup the regexps needed by Fold-mode to identify the folds.
Currently, it sets up if absent the following regexps:
     fold-begin-regexp to identify where folds start,
     fold-end-regexp   to identify where folds end,
     fold-end-or-begin-regexp the union of the two previous.

Setup also the function to discover folds stored in the fold-compute-folds 
variable and other variables is overriden in the major-mode Plist.

If the major mode has a comment-start of one character then everything
works automatically otherwise there is somewhere something that does not
work (I don't remember where). In that latter case, you must explicitly
set up fold-{end,begin}-regexp. Look at the examples immediately after
this function.
"
  (interactive)
  (let (cs)
    ;; Discovered by Nadim Saeed <Nadim@metaflow.com>:
    ;; Perl now has "# " as comment-start rather than "#"
    ;; so remove trailing spaces.
    (or (stringp comment-start)
        (error "This mode badly set comment-start") )
    (setq cs
          (if (string-match "\\(.*[^ ]+\\) *$" comment-start)
              (substring comment-start (match-beginning 1) (match-end 1))
            comment-start ) )
    ;; compute regexps if needed
    (or fold-end-regexp
        (setq fold-end-regexp
              (or (get major-mode 'fold-end-regexp)
                  (concat "^" (regexp-quote cs)
                          "+\\s-*" (regexp-quote fold-end) ) ) ) )
    (or fold-end-regexp
        (error "Cannot determine fold-end-regexp") )
    (or fold-begin-regexp
        (setq fold-begin-regexp
              (or (get major-mode 'fold-begin-regexp)
                  (concat "^" (regexp-quote cs) "+\\s-*\\("
                          (regexp-quote fold-end) "\\|\\)\\s-*" 
                          (regexp-quote fold-begin) ) ) ) )
    (or fold-begin-regexp
        (error "Cannot determine fold-begin-regexp") )
    (or fold-end-or-begin-regexp
        (setq fold-end-or-begin-regexp
              (or (get major-mode 'fold-end-or-begin-regexp)
                  (concat "\\(" fold-begin-regexp "\\|"
                          fold-end-regexp "\\)" ) ) ) )
    (or fold-end-or-begin-regexp
        (error "Cannot determine fold-end-or-begin-regexp") )
    ;; override global value if there is an appropriate property
    (let ((value (get major-mode 'fold-open-face)))
      (if value (setq fold-open-face value)) )
    (let ((value (get major-mode 'fold-closed-face)))
      (if value (setq fold-closed-face value)) )
    (let ((fn (get major-mode 'fold-compute-folds)))
      (if fn (setq fold-compute-folds fn)) )
    (let ((value (get major-mode 'fold-comment-start)))
      (if value (setq fold-comment-start value)) )
    (let ((value (get major-mode 'fold-implicit-closed-fold-height)))
      (if value (setq fold-implicit-closed-fold-height value)) )
    (let ((value (get major-mode 'fold-closed-text-properties)))
      (if value (setq fold-closed-text-properties value)) )
    t ) )

;;;   {{{ Customize for some major modes

;;; Set up explicitely the emacs-lisp-mode customization to allow
;;; people to fold the current file even if they have a weird
;;; comment-start value.  (Suggested by <ascott@sedona.intel.com>).
;;; The value I use for comment-start is ";".

(put 'emacs-lisp-mode 'fold-end-regexp "^;+[ \t]*}}}")
(put 'emacs-lisp-mode 'fold-begin-regexp "^;+[ \t]*\\(}}}\\|\\)[ \t]*{{{")

(put 'c-mode 'fold-end-regexp "^\\(/\\|[ \t]*\\)\\*[ \t]*}}}")
(put 'c-mode 'fold-begin-regexp "^\\(/\\|[ \t]*\\)\\*[ \t]*{{{")
(put 'c-mode 'fold-end-or-begin-regexp 
     "^\\(/\\|[ \t]*\\)\\*[ \t]*\\(}}}[ \t]*\\|{{{\\)" )

(put 'c++-mode 'fold-end-regexp "^\\(/\\|[ \t]*\\)\\*[ \t]*}}}")
(put 'c++-mode 'fold-begin-regexp "^\\(/\\|[ \t]*\\)\\*[ \t]*{{{")
(put 'c++-mode 'fold-end-or-begin-regexp 
     "^\\(/\\|[ \t]*\\)\\*[ \t]*\\(}}}[ \t]*\\|{{{\\)" )

(put 'c++-c-mode 'fold-end-regexp "^\\(/\\|[ \t]*\\)\\*[ \t]*}}}")
(put 'c++-c-mode 'fold-begin-regexp "^\\(/\\|[ \t]*\\)\\*[ \t]*{{{")
(put 'c++-c-mode 'fold-end-or-begin-regexp 
     "^\\(/\\|[ \t]*\\)\\*[ \t]*\\(}}}[ \t]*\\|{{{\\)" )

(put 'caml-mode 'fold-end-regexp "^(\\*[ \t]*}}}")
(put 'caml-mode 'fold-begin-regexp "^\\((\\*\\)?[ \t]*{{{")

;;; For TeX-based modes, one may define fold-begin with
;;;(put 'tex-mode 'fold-end-regexp "^\\\\.*section")
;;;(put 'tex-mode 'fold-begin-regexp "^\\\\.*section")
;;; But I prefer to use explicit %{{{ and %}}}. This poses no problems
;;; since TeX-based modes use a single comment-start character so all
;;; these variables are automatically computed.
;;;   }}}

;;; Parse or reparse a buffer, identify folds and install them. 
;;; At the beginning they are all open.

(defun fold-enter-mode ()
  "Enter Fold-mode or reenter it (ie recompute folds).
All folds are initially open."
  (interactive)
  (fold-mode t)
  (if fold-mode 
      (fold-install-all-folds) ) )

(defun fold-exit-mode ()
  "Leave Fold-mode."
  (interactive)
  (if fold-mode
      (progn 
        (fold-open-all-folds)
        (fold-mode nil) )
    (error "Not in Fold-mode") ) )

;;;{{{ Possible hooks

;;; This hook may be inserted into find-file-hooks.

(defun install-fold-mode-if-needed ()
  "Install `Fold-mode' if the buffer uses these funny triple brackets.
It does not hide all possible folds, you have to require it yourself 
with the menu! This is safer since not all files containing triple 
brackets use Fold-mode."
  (save-excursion
    (goto-char (point-min))
    (if (and (search-forward fold-begin nil t)
             (search-forward fold-end nil t) )
        (condition-case nil
            (progn
              (require 'foldingo)
              (fold-mode t) )
          (error (condition-case nil
                     (fold-mode nil)
                   (error nil) )) ) ) ) )

;;;}}}
;;;   }}}{{{ Parse a buffer to identify folds

(defun fold-compute-explicit-folds ()
  "Compute where are the explicit folds within a buffer.
Non balanced fold marks will be signalled.

This is the default function to identify folds based on the sole
marks {{{ and }}}. More elaborated functions may also identify implicit
folds corresponding to big definitions spanning numerous lines. Implicit
folds depend on the major-mode of course.
"
  (interactive)
  (fold-release-all-folds)
  (let ((stack-begin nil)
        (subfolds    nil) )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward fold-end-or-begin-regexp nil t)
        (let ((dot (match-beginning 0)))
          (goto-char dot)
;;          (message "Looking for explicit folds (%2d%%)"
;;                   (/ (* 100 dot) (point-max)) )
          (if (looking-at fold-end-regexp)
              (if (consp stack-begin)
                  (save-excursion
                    ;this modifies the buffer.
                    ;(if fold-end-face
                    ;    (put-text-property (match-beginning 0)
                    ;                       (match-end 0)
                    ;                       'mouse-face fold-end-face ) )
                    (beginning-of-line)
                    (backward-char 1)
                    (let ((newfold (fold-make 
                                    (car stack-begin)
                                    (point)
                                    nil
                                    fold-closed-face
                                    fold-open-face
                                    (car subfolds)
                                    1 )))
                      (setq fold-folds-list
                            (cons newfold fold-folds-list ) )
                      (setq subfolds (cdr subfolds))
                      (setq subfolds (cons (cons newfold (car subfolds))
                                           (cdr subfolds) ))
                      (setq stack-begin (cdr stack-begin)) ) )
                (error "Unexpected %s at position %d" fold-end dot) ) )
          ;; an end of fold may also start a new fold
          (if (looking-at fold-begin-regexp)
              (progn
                (setq stack-begin (cons dot stack-begin))
                (setq subfolds (cons nil subfolds)) ) )
          (end-of-line) ) ) )
    (if (consp stack-begin)
        (error "Unmatched %s at position %d" 
               fold-begin (car stack-begin) ) ) )
  (setq fold-folds-list (nreverse fold-folds-list)) )

;;;   {{{ Fold identification for modes implementing defuns

;;; Although lisp is mentioned in these names, all modes that
;;; implement beginning-of-defun and end-of-defun may share this
;;; code. Alas this is not the case of the C-based modes I know.

(defun fold-compute-implicit-defun-folds ()
  "This function turns big definitions into implicit folds.
It can be used in Lisp based modes.
It must be run after fold-compute-explicit-folds."
  (let ((l fold-folds-list)
        explicit-fold )
    (save-excursion
      (if (consp l)
          (while (consp l)
            (setq explicit-fold (car l))
            (if (not (fold-subfolds explicit-fold))
                (save-restriction
                  (narrow-to-region
                   (foldin-overlay-start (fold-overlay explicit-fold))
                   (foldin-overlay-end (fold-overlay explicit-fold)) )
                  (fold-compute-implicit-defun-folds-in-region 
                   explicit-fold ) ) )
            (setq l (cdr l)) )
        (fold-compute-implicit-defun-folds-in-region nil) ) ) ) )

(defun fold-compute-implicit-defun-folds-in-region (superfold)
  "Compute implicit folds based on big defuns in the (narrowed) buffer."
  (let (beg end)
    (goto-char (point-max))
;;    (message "Looking for implicit folds (%2d%%)"
;;             (/ (* 100 (point)) (buffer-size)) )
    (while (beginning-of-defun 1)
      (setq beg (point))
      (save-excursion
        (end-of-defun 1)
        (skip-syntax-backward " ")
        (end-of-line)
        (setq end (point))
        (if (>= (count-lines beg end) fold-implicit-fold-threshold)
            (fold-add-implicit-subfold superfold beg end) ) ) ) ) )

(defun fold-add-implicit-subfold (fold beg end)
  "Add as subfold of FOLD, a new fold from BEG to END."
  (let ((subfold (fold-make (save-excursion
                              (goto-char beg)
                              (beginning-of-line)
                              (point) )
                            (save-excursion
                              (goto-char end)
                              (end-of-line)
                              (point) )
                            nil         ; initially open
                            fold-closed-face
                            fold-open-face
                            nil         ; no subfolds
                            fold-implicit-closed-fold-height )))
    (setq fold-folds-list (cons subfold fold-folds-list))
    (if fold (fold-set-subfolds fold (cons subfold (fold-subfolds fold))))
    subfold ) )

;;; Make Lisp-based modes use this new way of computing folds.

(defun fold-compute-folds-for-lisp-based-modes ()
  "Compute explicit folds.
Convert any defun bigger than fold-implicit-fold-threshold lines 
into implicit folds."
  (fold-compute-explicit-folds)
  (fold-compute-implicit-defun-folds) )

;;;}}}{{{ Fold identification for C-based modes

(defun fold-compute-implicit-c-folds ()
  "This function turns big definitions into implicit folds.
It can be used in C, Caml modes and similar modes.
It must be run after fold-compute-explicit-folds.

Definitions are recognized as being paragraphs that do not start with
fold-comment-start regexp.
"
  (if fold-comment-start
      (let ((l fold-folds-list)
            explicit-fold )
        (save-excursion
          (if (not l)
              (fold-compute-implicit-c-folds-in-region nil)
            (while (consp l)
              (setq explicit-fold (car l))
              (if (not (fold-subfolds explicit-fold))
                  (save-excursion
                    (save-restriction
                      (narrow-to-region 
                       (foldin-overlay-start (fold-overlay explicit-fold))
                       (foldin-overlay-end (fold-overlay explicit-fold)) )
                      (fold-compute-implicit-c-folds-in-region
                       explicit-fold ) ) ) )
              (setq l (cdr l)) ) ) ) ) ) )

(defun fold-compute-implicit-c-folds-in-region (superfold)
  "Identify the implicit folds in the current (narrowed) buffer.
Store them as subfolds of superfold."
  (let (beg end dot)
    (goto-char (point-max))
;;    (message "Looking for implicit folds (%2d%%)"
;;             (/ (* 100 (point)) (buffer-size)) )
    (skip-syntax-backward " ")
    (setq end (point))
    (while (not (bobp))
      (backward-paragraph 1)
      (save-excursion
        (skip-syntax-forward " ")
        (if (not (looking-at fold-comment-start))
            (progn
              (beginning-of-line)
              (setq beg (point))
              (if (>= (count-lines beg end) 
                      fold-implicit-fold-threshold )
                  (fold-add-implicit-subfold 
                   superfold beg end ) ) ) ) )
      (skip-syntax-backward " ")
      (setq end (point)) ) ) )

(defun fold-compute-folds-for-c-based-modes ()
  "Compute explicit folds.
Convert any C definition bigger than fold-implicit-fold-threshold 
lines into implicit folds."
  (fold-compute-explicit-folds)
  (fold-compute-implicit-c-folds) )

;;;}}}{{{ Customize for some major modes

(mapcar (function (lambda (mode)
                    (put mode
                         'fold-compute-folds
                         'fold-compute-folds-for-lisp-based-modes) ))
        '( lisp-mode
           scheme-mode
           emacs-lisp-mode
           ) )

(mapcar (function (lambda (mode-and-comment)
                    (put (car mode-and-comment)
                         'fold-compute-folds
                         'fold-compute-folds-for-c-based-modes)
                    (put (car mode-and-comment)
                         'fold-comment-start
                         (car (cdr mode-and-comment)) )
                    (put (car mode-and-comment)
                         'fold-implicit-closed-fold-height
                         (car (cdr (cdr mode-and-comment))) ) ))
        '( ( c-mode    "/\\*" 2 )
           ( caml-mode "(\\*" 1 )
           ) )

;;;}}}
;;;   }}}{{{ Search current fold

(defun fold-current-fold ()
  "Return the fold that currently contains the dot."
  (let ((l   fold-folds-list)
        (dot (point))
        fold )
    (while (consp l)
      (if (and (<= (foldin-overlay-start (fold-overlay (car l))) dot)
               (<= dot (foldin-overlay-end (fold-overlay (car l)))) )
          (if fold
              (if (> (foldin-overlay-start (fold-overlay (car l)))
                     (foldin-overlay-start (fold-overlay fold)) )
                  (setq fold (car l)) )
            (setq fold (car l)) ) )
      (setq l (cdr l)) )
    (or fold (error "No current fold")) ) )

(defun fold-identify-fold (pt &optional end)
  "Returnt the current fold.
The current fold starts at PT or, if END is true, the one that ends at PT."
  (let ((l fold-folds-list)
        fold )
    (while (consp l)
      (if (= pt (if end
                    (foldin-overlay-end (fold-overlay (car l)))
                  (foldin-overlay-start (fold-overlay (car l))) ))
          (progn
            (setq fold (car l))
            (setq l nil) )
        (setq l (cdr l)) ) )
    fold ) )

(defun fold-open-current-fold ()
  "Show the current fold."
  (interactive)
  (if fold-mode
      (fold-open-fold (fold-current-fold))
    (error "Fold-mode inactive") ) )

(defun fold-close-current-fold ()
  "Hide the current fold."
  (interactive)
  (if fold-mode
      (fold-close-fold (fold-current-fold))
    (error "Fold-mode inactive") ) )

;;;   }}}{{{ Mouse support

;;; NOTE: This function should rather use fold-begin and fold-end
;;; instead of looking for {{{ or }}}. 
;;; NOTE2: How to control an implicit fold ???
;;;        Use S-mouse1 to open and S-mouse3 to close ?

(defun fold-mouse-on-fold-end ()
  "Return the fold if the current position is on a fold boundary.
A fold boundary is either }}} or {{{. 
This function is used to control the mouse support."
  (save-excursion
    (backward-char 2)
    (and (looking-at "\\(}}}\\|.}}}\\|..}}}\\)")
         (progn
           (beginning-of-line)
           (and (looking-at fold-end-regexp)
                (fold-identify-fold (progn (backward-char 1) (point))
                                    t ) ) ) ) ) )

(defun fold-mouse-handle (event)
  "Handle a mouse action on a fold.
It is actually bound to \\[fold-mouse-handle].
Leaves the dot where it is."
  (interactive "e")
  (if fold-mode
      (progn
        (mouse-set-point event)
        (let ((fold (fold-mouse-on-fold-end)))
          (if fold
              (fold-toggle fold)
            (let ((fold (fold-current-fold)))
              (if fold
                  (fold-toggle fold)
                (error "No current fold") ) ) ) ) )
    (error "Fold-mode inactive") ) )

;;;   }}}{{{ Toggle fold state

(defun fold-toggle (fold)
  "Close an open fold or open a closed fold."
  (set-marker fold-mark (point))
  (if (fold-closed-p fold)
      (fold-open-fold fold)
    (fold-close-fold fold) )
  (goto-char (marker-position fold-mark)) )

;;;   }}}{{{ Install a fold

;;; The buffer may be readonly.

(defun fold-install-all-folds ()
  "Install all folds (overlays, faces ...).
This function does not alter the buffer."
  (interactive)
  (let ((l fold-folds-list))
    (while (consp l)
      (fold-install-fold (car l))
      (setq l (cdr l)) ) ) )

(defun fold-install-fold (fold)
  "Install a particular fold ie show it or hide it.
When a fold is hidden, so are its subfolds (but their 
status is remanent)."
  (let* ((overlay  (fold-overlay fold))
         (beg      (foldin-overlay-start overlay))
         (end      (foldin-overlay-end overlay))
         (subfolds (fold-subfolds fold))
         ;; snarfed from folding.el (Jamie Lokier)
         (buffer-read-only buffer-read-only)
         (modified (buffer-modified-p))
         (ask1 (symbol-function 'ask-user-about-supersession-threat))
         (ask2 (symbol-function 'ask-user-about-lock)) )
    (unwind-protect
        (progn
          (setq buffer-read-only nil)
          (or modified
              (progn
                (fset 'ask-user-supersession-threat
                      '(lambda (&rest x) nil) )
                (fset 'ask-user-about-lock
                      '(lambda (&rest x) nil) )
                (set-buffer-modified-p t) ) )
          ;; Nilify some hooks (from done@nexus.sorostm.ro and
          ;; Simon.Marshall@esrin.esa.it).
          (let ((inhibit-read-only t)
                (buffer-undo-list t)
                before-change-functions
                after-change-functions
                )
            (remove-text-properties beg end fold-closed-text-properties) )
          (if (fold-closed-p fold)
              (save-excursion
                (goto-char beg)
                (forward-line (- (fold-height fold) 1))
;;                (subst-char-in-region (point) end ?\n ?\r t) 
		(foldingo-flag-region  (point) end t)
		)
	    (foldingo-flag-region beg end nil)
	    ;;            (subst-char-in-region beg end ?\r ?\n t) 
	    )
          (if (fold-closed-p fold)
              (foldin-overlay-put overlay 'face (fold-closed-face fold))
            (foldin-overlay-put overlay 'face (fold-open-face fold)) )
          ;; Make readonly a closed fold. Suggestion from
          ;; Christopher Kline <ckline@texas.mitre.org>
          (if (fold-closed-p fold)
              ;; Nilify some hooks (from done@nexus.sorostm.ro and
              ;; Simon.Marshall@esrin.esa.it).
              (let ((inhibit-read-only t) 
                    (buffer-undo-list t)
                    before-change-functions 
                    after-change-functions
                    )
                (add-text-properties beg end fold-closed-text-properties) ) )
          ;; restaure inner subfolds to their former status
          (if (not (fold-closed-p fold))
              (while (consp subfolds)
                (fold-install-fold (car subfolds))
                (setq subfolds (cdr subfolds)) ) ) )
      (or modified
          (unwind-protect
              (set-buffer-modified-p nil)
            (fset 'ask-user-about-supersession-threat ask1)
            (fset 'ask-user-about-lock ask2) ) ) ) )
  fold )

(defun fold-no-command ()
  (interactive)
  (error "No command allowed on a closed fold") )

;;;   }}}{{{ Release a fold

;;; Release folds, this is necessary since a fold contains an overlay
;;; which has to be explicitly released. Open folds as they are released
;;; to convert back ^M into NL.

(defun fold-release-all-folds ()
  "Release all folds."
  (interactive)
  (let ((l     fold-folds-list)
        (whole (fold-make (point-min) (point-max)
                          nil nil nil nil 1 )) )
    (setq fold-folds-list nil)
    ;; reopen the whole buffer in one go.
    (fold-install-fold whole)
    (foldin-delete-overlay (fold-overlay whole))
    (while (consp l)
      (foldin-delete-overlay (fold-overlay (car l)))
      (setq l (cdr l)) ) ) )

;;;   }}}{{{ Close fold

(defun fold-close-all-folds ()
  "Hide all folds."
  (interactive)
  (if fold-mode
      (let ((l fold-folds-list))
        (while (consp l)
          (fold-close-fold (car l))
          (setq l (cdr l)) ) )
    (error "Fold-mode inactive") ) )

(defun fold-close-fold (fold)
  "Hide a fold."
  (fold-set-closed-p fold 't)
  (fold-install-fold fold) )

;;;   }}}{{{ Open fold

(defun fold-open-all-folds ()
  "Show all folds."
  (interactive)
  (if fold-mode
      (let ((l fold-folds-list))
        (while (consp l)
          (fold-open-fold (car l))
          (setq l (cdr l)) ) )
    (error "Fold-mode inactive") ) )

(defun fold-open-fold (fold)
  "Show a fold."
  (fold-set-closed-p fold 'nil)
  (fold-install-fold fold) )

;;;   }}}{{{ ADD ON by Georg Greve

(defun insert-new-empty-c++-fold ()
  "Insert a new fold for C++."
  (interactive "*")
  (if (not (bolp))
      (insert ?\n))
  (insert "/*{{{ ")
  (let ((old (point)))
    (insert "  */\n")
  (goto-char old))
)

(defun close-c++-fold ()
  "Closing a fold for C++."
  (interactive "*")
  (if (not (bolp))
      (insert ?\n))
  (insert "/*}}}*/\n")
)

(defun insert-new-c++-fold (foldname)
  "Insert a new fold for C++."
  (interactive "*sTitle of the new fold: ")
  (if (not (bolp))
      (insert ?\n))
  (insert (format "/*{{{  %s  */\n"
		  foldname))
)

(defun fold-enter-fold-mode-close-all-folds ()
  "(Re)Enter Fold mode and close all folds."
  (interactive)
  (fold-enter-mode)
  (fold-close-all-folds)
)

(global-set-key "\C-cfi"   'insert-new-c++-fold)
(global-set-key "\C-cfc"   'close-c++-fold)
(global-set-key "\C-cfe"   'fold-enter-mode)
(global-set-key "\C-cff"   'fold-close-all-folds)
(global-set-key "\C-cfo"   'fold-open-current-fold)
(global-set-key "\C-cfh"   'fold-close-current-fold)
(global-set-key "\C-cfa"   'fold-enter-fold-mode-close-all-folds)

;;;   }}}

;;; <done@ece.arizona.edu> Support functions.

(defun foldingo-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char from)
      (end-of-line)
      (foldingo-discard-overlays (point) to 'invisible 'foldingo)
      (if flag
	  (let ((overlay (make-overlay (point) to)))
	    (foldingo-make-overlay-hidden overlay))))))

(defun foldingo-make-overlay-hidden (overlay)
  ;; Make overlay hidden and intangible.
;;  (overlay-put overlay 'invisible t)
  (overlay-put overlay 'invisible t)
  (overlay-put overlay 'foldingo t)
;;  (overlay-put overlay 'intangible t)
)

(defun foldingo-discard-overlays (beg end prop value)
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let ((overlays (overlays-at (point))))
	(while overlays
	  (let ((o (car overlays)))
	    (if (overlay-get o prop)
		;; Either push this overlay outside beg...end
		;; or split it to exclude beg...end
		;; or delete it entirely (if it is contained in beg...end).
		(if (< (overlay-start o) beg)
		    (if (> (overlay-end o) end)
			(let ((o1 (outline-copy-overlay o)))
			  (move-overlay o1 (overlay-start o1) beg)
		      (move-overlay o (overlay-start o) beg)))
		  (if (> (overlay-end o) end)
		      (move-overlay o end (overlay-end o))
		    (delete-overlay o)))))
	  (setq overlays (cdr overlays))))
      (goto-char (next-overlay-change (point))))))

;;;}}}{{{ Fold menu

(if (check-for-XEmacs)
    (progn
      (defconst foldin-menu
        '("Fold"
	  ["(Re)Enter fold mode"  fold-enter-mode                 t]
	  ["Exit fold mode"  fold-exit-mode               fold-mode]
	  "----"
	  ["Insert C++ fold" insert-new-c++-fold                  t]
	  ["Close C++ fold" close-c++-fold                        t]
          "----"
	  ["Fold whole buffer"   fold-close-all-folds     fold-mode]
	  ["Unfold whole buffer" fold-open-all-folds      fold-mode]
          "----"
	  ["Open current fold"   fold-open-current-fold   fold-mode]
	  ["Hide current fold"   fold-close-current-fold  fold-mode]
	  ))

      (or (condition-case error-object
              (progn (add-submenu '("Tools") foldin-menu "VC")
                     t )
            (error nil) )
          ;; From Gareth J McAleese <G.McAleese@ulst.ac.uk>: if you do
          ;; not have a tools menu, you may alternatively use:
          (condition-case error-object
              (progn (add-menu nil "Foldingo" foldin-menu)
                     t )
            (error nil) ) )
      )
  
  (progn
    (require 'menu-bar)
    
    (define-key menu-bar-tools-menu [fold]
      (cons "Fold" fold-menu-map) )
    
;;; Don't forget to define them in reverse order.
    
    (define-key fold-menu-map [fold-close-fold]
      '("Hide current fold" . fold-close-current-fold) )
    (define-key fold-menu-map [fold-open-fold]
      '("Open current fold" . fold-open-current-fold) )
    (define-key fold-menu-map [fold-separator2]
      '("--") )
    (define-key fold-menu-map [unfold-buffer]
      '("Unfold whole buffer" . fold-open-all-folds) )
    (define-key fold-menu-map [fold-buffer] 
      '("Fold whole buffer" . fold-close-all-folds) )
    (define-key fold-menu-map [fold-separator1]
      '("--") )
    (define-key fold-menu-map [close-c++-fold]
      '("Close C++ fold" . close-c++-fold) )
    (define-key fold-menu-map [insert-new-c++-fold]
      '("Insert C++ fold" . insert-new-c++-fold) )
    (define-key fold-menu-map [fold-separator1]
      '("--") )
    (define-key fold-menu-map [fold-mode-off]
      '("Exit fold mode" . fold-exit-mode) )
    (define-key fold-menu-map [fold-mode-on]
      '("(Re)Enter fold mode" . fold-enter-mode) )
    )
  )

;;; Fold mode items only appear if fold mode is active.

;;;(put 'close-c++-fold          'menu-enable 'fold-mode)
;;;(put 'insert-new-c++-fold     'menu-enable 'fold-mode)
(put 'fold-close-current-fold 'menu-enable 'fold-mode)
(put 'fold-open-current-fold  'menu-enable 'fold-mode)
(put 'fold-open-all-folds     'menu-enable 'fold-mode)
(put 'fold-close-all-folds    'menu-enable 'fold-mode)
(put 'fold-exit-mode          'menu-enable 'fold-mode)

;;; As of Emacs 19.30, S-down-mouse-1 is dedicated to mouse-set-font so
;;; this binding is not appropriate if you want to diddle with fonts.

(if (check-for-XEmacs)
    (progn
      (define-key fold-keymap [(shift button3)]      'fold-mouse-handle)
      )
  (progn
    ;; Avoid an ugly beep, thanks to shields@crosslink.net (Michael Shields)
    (define-key fold-keymap [S-down-mouse-1] 'fold-noop)
    (defun fold-noop () (interactive))
    (define-key fold-keymap [S-mouse-1]      'fold-mouse-handle)
    ))

;;;}}}
;;;}}}

(provide 'foldingo)

;;; end of foldingo.el
