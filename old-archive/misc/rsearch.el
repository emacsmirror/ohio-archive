;;; rsearch.el -- Repeatable non-incremental search commands and menu bar entry 
;;; Copyright (C) 1993, 1994 James H. Berry

;;; Author:      James H. Berry <jberry@frb.gov>
;;;              Inspired by ideas and code found in the
;;;              GNU Emacs-19 distribution and in various articles
;;;              posted to the gnu.* newsgroups.

;;; Version:     $Revision: 1.11 $
;;; Keywords:    search, replace, menu

;;; This file is not part of GNU Emacs. but is made available under
;;; the same conditions.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; LCD Archive Entry:
;;; rsearch|James Berry|jberry@frb.gov|
;;; Repeatable non-incremental search commands and search/replace menubar.|
;;; 15-Jul-1994|1.11|~/misc/rsearch.el.Z|

;;; This file supplies a repeatable non-incremental search command
;;; and a menu bar item containing several search, replace and related commands.
;;; As powerful as isearch is, sometimes it is not quite right for the
;;; task at hand.  Although not as sophisticated as isearch, rsearch
;;; can be useful when the incremental feature is not wanted, when a
;;; repeatable word-search is needed, when a menubar is desired, or
;;; for novice users who want a simpler search command.  Features:

;;;   o  Executing the command twice or more in a row will repeat the
;;;      previous search, no questions asked. 
;;;   o  String, RE, and Word searches are supported.
;;;   o  Some feedback is provided in the minibuffer area.
;;;   o  Successful searches may be highlighted.
;;;   o  A search/replace menubar menu is provided.
;;;      Contains a few interesting non-rsearch commands, such as 
;;;      replace in region, occur, and delete matching/nonmatching lines.

;;;   GNU Emacs 19 is required for highlighting and the menubuar.
;;;   It should not be too much trouble to adjust for Lucid GNU Emacs.
        

;;; INSTALLATION:
;;;
;;; 1) To use this package,  put it in a file called "rsearch.el" in a Lisp
;;;    directory known to Emacs and byte-compile it.
;;;
;;; 2) Place the following in your ~/.emacs file or in the file default.el 
;;;    in the site-lisp directory of the Emacs distribution. 
;;;
;;;      (require 'rsearch)
;;;
;;; 3) The command 'rsearch-define-key should then be used to bind two keys
;;;    for rsearching forwards and backwards.  For example:
;;;
;;;      (rsearch-define-key [f7] [S-f7])      ;   binds F7 and Shift-F7
;;;      (rsearch-define-key "\C-cs" "\C-cr")  ;   binds C-c s and C-c r
;;; 
;;;    The two keys will be bound in both the global and in a local
;;;    minibuffer map.
;;;
;;; 4) By default, successful searches are highlighted with the highlight face.
;;;
;;;      (setq rsearch-highlight nil)          ; do not highlight
;;;      (setq rsearch-highlight-face 'region) ; use the 'region face 
;;;
;;;  5) A search/replace menubar is installed by default.  To remove it:
;;;
;;;       (rsearch-install-menubar 1)  ;  Remove search menubar.
;;;
;;;   rsearch-define-key and rsearch-install-menubar may be used interactively.
;;; 
;;;  6) A few other variables may be set to change the default behaviour.
;;;     See the User Variables section below.
;;;
;;; Change log;
;;;
;;;  1.2 - original public version posted to gnu.emacs.sources
;;;  1.3   Do not do menubar bindings if V18, Epoch or Lucid.
;;;        Fixed wrapping when repeating-last from menubar.
;;;  1.4   Added push-mark at beginning of search.         
;;;  1.5   Fixed typo in rsearch-repeat-search 
;;;  1.6   Added wrapping control.  Default is not wrap.
;;;        Added count-matches-in-region.
;;;  1.7   Fixed message problem when search string contains `%'.
;;;        Thanks to Paul Raines <raines@slac.stanford.edu>
;;;  1.8   Redo of highlighting scheme.  More functions added to menubar.
;;;  1.9   Redo of (rsearch-highlight) for backwards compatibility.
;;;  1.10  Minor changes to menubar.
;;;  1.11  replace-*-region commands use perform-replace.
;;; =====================================================================
;;; Code:

;;; User Variables

(defvar rsearch-highlight t
  "*Non-nil means rsearch highlights the current match.")

(defvar rsearch-highlight-face 'highlight
  "*The face used for rsearch highlighting. 
Typical values are 'highlight, 'modeline, 'region, and 'underline.")

(defvar rsearch-wrapp nil
  "*If non-nil, wrap to begin (or end) if previous command was an unsuccessful
rsearch. If nil, then request a new search argument if previous command 
was an unsuccessful rsearch.")

(defvar rsearch-display-lastp nil
  "*If non-nil, display last rsearch argument when prompting for a new one.
The Emacs Lisp Reference Manual implies that this is not a good idea.")

(defvar rsearch-historyp t
  "*If non-nil, save rsearch commands in the command history.
This allows retrieval via repeat-complex-command.")

(defvar rsearch-set-mark t
  "*If non-nil, set the mark at the start of a successful rsearch.") 

;;; Internal Variables.
;;; These are all global variables.
;;; Perhaps some should be buffer local.

(defvar rsearch-arg nil 
  "Current string, word or regexp to be searched for.")

(defvar rsearch-type 'string
  "Current type of search.  One of 'string, 'word, 'regexp")

(defvar rsearch-direction nil
  "If non-nil, rsearch is searching backward.") 

(defvar rsearch-previous-function  nil
  "Previous search function invoked. 
One of 'search-forward,  'search-backward, 're-search-forward ...")

(defvar rsearch-wrapped nil 
  "If non-nil, current rsearch was restarted at top (bottom).")

(defvar rsearch-mark-msg nil
  "If non-nil, display a  Mark set  message from rsearch-message-2.")

(defvar rsearch-success nil
  "Did last rsearch succeed? If non-nil, location of point.")

(defvar rsearch-overlay nil 
  "Overlay used for highlighting a successful rsearch.")

;;; ========================================================================
;;; Define the rsearch minibuffer map

;;; Typically, the user will use the command rsearch-define-key to bind
;;; rsearch-forward and -backward in the global map and to use the same keys
;;; in the minibuffer map.

(defvar minibuffer-local-rsearch-map nil
  "Minibuffer keymap used while prompting in repeated search.")

(or minibuffer-local-rsearch-map 
        (let ((map (copy-keymap minibuffer-local-map)))
          (define-key map "\C-t" 'rsearch-minibuffer-toggle-type)
          (define-key map "\C-s" 'rsearch-minibuffer-forward)
          (define-key map "\C-r" 'rsearch-minibuffer-backward)
          (define-key map "\C-h" 'rsearch-minibuffer-help-msg)
          (setq minibuffer-local-rsearch-map map)))

(defun rsearch-define-key (keyf keyb)
  "Define the key KEYF to perform an rsearch and KEYB to do it backwards."
  (interactive "kEnter key to be used for rsearch forward: \nkEnter another key to be used for rsearch backward: ")
  (define-key global-map         keyf 'rsearch-forward)
  (define-key global-map         keyb 'rsearch-backward)
  (define-key minibuffer-local-rsearch-map keyf 'rsearch-minibuffer-forward)
  (define-key minibuffer-local-rsearch-map keyb 'rsearch-minibuffer-backward)
  (if (interactive-p)
      (message "%s and %s are now bound to rsearch-forward and -backward."
	       keyf keyb)))

;;; ========================================================================
;;; Entry points to rsearch: rsearch-forward, rsearch-backward, etc.
;;; Prefix arg means: Do a search without asking any questions, using
;;; last known search argumnt, even if most recent command was not an
;;; rsearch.

(defun rsearch-forward (arg)
  "Start (or repeat) a string, word, or regexp search forward. 
Consecutive rsearches repeat with no questions asked.
With prefix arg, use last search argument.
\\<minibuffer-local-rsearch-map>
When prompted for the initial search string, you may also:
Type \\[rsearch-minibuffer-toggle-type] to toggle string, word, or regexp search.
Type \\[previous-history-element] and \\[next-history-element] to use previously entered strings.
An initial empty string means use the previous search argument.

The variable `rsearch-highlight-face' selects the face to use for highlighting.
The variable `rsearch-wrapp' controls whether repeating a failed search
will wrap to the beginning of the buffer or request a new search argument."
  (interactive "P")
  (rsearch-command arg nil nil))

(defun rsearch-backward (arg)
  "Start (or repeat) a string, word, or regexp search backwards. 
With prefix arg, use last search argument.
See \\<global-map>\\[rsearch-forward] for more information."
  (interactive "P")
  (rsearch-command arg t nil))

;;; The following 6 commands are for the benefit of a menubar, but could
;;; be bound to a key sequence.

(defun rsearch-string-forward (arg)
  "Do a repeatable string-search forwards. 
With prefix arg, use last search argument.
See \\<global-map>\\[rsearch-forward] for more information."
  (interactive "P")
  (rsearch-command arg nil 'string))

(defun rsearch-string-backward (arg)
  "Do a repeatable string-search backwards. 
With prefix arg, use last search argument.
See \\<global-map>\\[rsearch-forward] for more information."
  (interactive "P")
  (rsearch-command arg t 'string))

(defun rsearch-word-forward (arg)
  "Do a repeatable word-search forwards. 
With prefix arg, use last search argument.
See \\<global-map>\\[rsearch-forward] for more information."
  (interactive "P")
  (rsearch-command arg nil 'word))

(defun rsearch-word-backward (arg)
  "Do a repeatable word-search backwards. 
With prefix arg, use last search argument.
See \\<global-map>\\[rsearch-forward] for more information."
  (interactive "P")
  (rsearch-command arg t 'word))

(defun rsearch-regexp-forward (arg)
  "Do a repeatable regexp-search forwards.
With prefix arg, use last search argument. 
See \\<global-map>\\[rsearch-forward] for more information."
  (interactive "P")
  (rsearch-command arg nil 'regexp))

(defun rsearch-regexp-backward (arg)
  "Do a repeatable regexp-search backwards.
With prefix arg, use last search argument.  
See \\<global-map>\\[rsearch-forward] for more information."
  (interactive "P")
  (rsearch-command arg t 'regexp))

;;; =========================================================================
;;; The generic rsearch command, which is called from the entry points above.
;;; Note the (setq this-command 'rsearch-command) which provides the
;;; repeatability.

(defun rsearch-command (arg &optional direction type)
  "Either start a search or repeat the previous rsearch command.
With prefix arg, repeat last known rsearch.
Optional DIRECTION, if non-nil, means search backwards.
TYPE may be 'string, 'regexp, or 'word to use a new type of search."
  (interactive "P")
  (or type (setq type rsearch-type)) 
  (if (or arg (eq last-command 'rsearch-command))
      (rsearch-repeat-search arg direction type)         
    (rsearch-begin-new-search direction type))

  ;; Do a few post-search things.
  (if (and rsearch-success rsearch-highlight window-system)
      (rsearch-highlightit (match-beginning 0) (match-end 0)))
  (message "%s" (rsearch-message-2))
  (if (not rsearch-success) (beep))
  (setq this-command 
	(if (or rsearch-success rsearch-wrapp) 'rsearch-command nil)))


;;; ========================================================================
;;;           Lower level functions.
  
;; Begin a new search.  Prompt user for parameters.  
;; Sometimes suggest the last used search string as initial string.
;; C-t will toggle whether a string, word or RE search.
;; These global variables are set:
;;
;;     rsearch-direction -   t means going backward.
;;     rsearch-type      -  'string 'word or 'regexp 
;;     rsearch-arg       -   what to search for
;;
;;     rsearch-success   -   non-nil is location of point after search

;; If rsearch-display-lastp is t
;; then use the last search arg as initial string when prompting.
;; If 1st arg is non-nil, then search backwards.
;; 2nd arg is one of 'string 'word or regexp

(defun rsearch-begin-new-search (direction type)
  (setq rsearch-wrapped nil
        rsearch-direction direction
	rsearch-type type)
  (let ((opoint (point))
	(more-input t)
	(newarg (and rsearch-display-lastp rsearch-arg)))
    (while more-input
      (setq more-input nil)
      (setq newarg (read-from-minibuffer
		    (rsearch-message-1)
		    newarg
		    minibuffer-local-rsearch-map
		    nil)))
    ;; If no search arg given, use last known
    (if (eq (length newarg) 0) (setq newarg rsearch-arg)) 
    (if (eq (length newarg) 0) (error "Empty search string."))
    (setq  rsearch-previous-function (rsearch-function)
	   rsearch-arg newarg
	   rsearch-success  
	   (funcall rsearch-previous-function rsearch-arg nil t nil))
    ;; Set mark at beginning of a successful search
    (if (and rsearch-success rsearch-set-mark)
	(progn 
	  (push-mark opoint t)
	  (setq rsearch-mark-msg t)))
      
    ;; This allows use of  C-x Esc Esc to repeat-complex-command
    (if rsearch-historyp
	(setq command-history
	      (cons
	       (list rsearch-previous-function rsearch-arg) 
	       command-history)))))


;; Repeat some search, no questions asked.
;;   If immediately previous search failed, and if using
;;   same direction and search type, and if wrapping is allowed,
;;   then wrap to top (bottom).
 
(defun rsearch-repeat-search (new-search new-direction new-type)
  "Repeat a search without asking any questions.
Non-nil NEW-SEARCH means this is a new search.
NEW-DIRECTION if t means search backwards.
NEW-TYPE if non-nil means do this search type"
    
  (if (and (not new-search) 
	   (eq rsearch-direction new-direction)
	   (eq rsearch-type new-type))
      ;;  same direction and search type
      (progn				
	(if rsearch-success             ; Was last rsearch successful?
	    ;; Yes, so repeat it.
	    (setq rsearch-success 
		  (funcall rsearch-previous-function rsearch-arg nil t nil))

	  ;; No. Wrap to beginning (end) and limit search up to current point
	  (let ((current (point)))	
	    (goto-char (if rsearch-direction (point-max) (point-min)))
	    (setq rsearch-wrapped t
	          rsearch-success 
		  (funcall rsearch-previous-function 
			   rsearch-arg current 'limit nil)))))

    ;; Reversing Direction or new search.
    (setq rsearch-direction new-direction
          rsearch-type new-type
          rsearch-wrapped nil
          rsearch-previous-function (rsearch-function)
          rsearch-success 
	  (funcall rsearch-previous-function rsearch-arg nil t nil))))     
;;;
;;;  Minibuffer functions.
;;;
(defun rsearch-minibuffer-toggle-type ()
  "Toggle type of search (string, word, regexp)."
  (interactive)
  (setq rsearch-type
	(cond ((eq rsearch-type 'word)     'regexp)
	      ((eq rsearch-type 'regexp)   'string)
	      (t                           'word)))
  (setq more-input t)
  (exit-minibuffer))
 
(defun rsearch-minibuffer-forward ()
  "Set rsearch direction forward and exit minibuffer."
  (interactive)
  (setq rsearch-direction nil)
  (exit-minibuffer))

(defun rsearch-minibuffer-backward ()
  "Set rsearch direction backward and exit minibuffer."
  (interactive)
  (setq rsearch-direction t)
  (exit-minibuffer))

(defvar rsearch-help-message 
"Beginning a repeatable non-incremental search.
\\<minibuffer-local-rsearch-map> 	
Enter \\[rsearch-minibuffer-toggle-type] to switch whether searching for a string, word, or regexp.
Enter \\[previous-history-element] to fetch an earlier minibuffer input 
Enter \\[next-history-element] to fetch a later minibuffer input 
Enter RET to begin searching.
      (an empty string means use the previous search argument).

Subsequently, \\<global-map>
Enter \\[rsearch-forward] to repeat the search forward.
Enter \\[rsearch-backward] to repeat the search backward."
  "Help message for rsearch.")

(defun rsearch-minibuffer-help-msg ()
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ 
     (substitute-command-keys rsearch-help-message))))


(defun rsearch-function ()
  "Determine the search function based on direction and type."
  (cond ((eq rsearch-type 'string) 
	 (if rsearch-direction 'search-backward 'search-forward))
	((eq rsearch-type 'word) 
	 (if rsearch-direction 'word-search-backward 'word-search-forward))
	((eq rsearch-type 'regexp) 
	 (if rsearch-direction 're-search-backward 're-search-forward))
	(t (if rsearch-direction 'search-backward 'search-forward))))


;; Form a string to be displayed when prompting for initial search arg.
(defun rsearch-message-1 ()
  (let ((str
	 (concat
	  "[C-h for help] "
	  (cond ((eq rsearch-type 'word) "Word ")
		((eq rsearch-type 'regexp)   "RE ")
		(t ""))
	  "Search"
	  (if rsearch-direction  " Backward" " Forward")
	  ": ")))
  str))

;; Form a string to be displayed after an rsearch.
(defun rsearch-message-2 ()
  (let ((str
	 (concat
	  (if rsearch-mark-msg "[Mark set] ")
	  (if rsearch-success "[Found] " "[Failed] ")
	  (if rsearch-wrapped "Wrapped ")
	  (cond ((eq rsearch-type 'word) "Word ")
		((eq rsearch-type 'regexp)   "RE ")
		(t ""))
	  "Search"
	  (if rsearch-direction  " Backward" " Forward")
	  ": "
	  rsearch-arg)))
    (setq rsearch-mark-msg nil)		; Print "Mark set" only once.
    str))

;;; ========================================================================
;;;        Highlighting Using an Overlay
;;;
;;;  This overlay code requires GNU Emacs 19.  
;;;
(defun rsearch-highlightit (beg end)
  (if (overlayp rsearch-overlay)
      (move-overlay rsearch-overlay beg end (current-buffer))
    (setq rsearch-overlay (make-overlay beg end))
    (overlay-put rsearch-overlay 'face rsearch-highlight-face))
  (add-hook  'pre-command-hook 'rsearch-remove-overlay))
	     
(defun rsearch-remove-overlay  ()
  "Pre-command hook function to delete the rsearch overlay."
  (if (overlayp rsearch-overlay)
       (delete-overlay rsearch-overlay))
  ;; And remove thyself from the pre-command-hook
  (setq pre-command-hook (delq 'rsearch-remove-overlay pre-command-hook)))

;;This command is included for backward compatibility,
;;The advertised thing is to (setq rsearch-highlight-face face-name)

(defun rsearch-highlight (face)
  "[Obsolete] Use the face FACE to hilight a successful rsearch. 
Nil means do not hilight. t means use 'highlight"
  (interactive "SFace name or nil or t: ")
  (if (eq face 't)
      (setq face 'highlight))
  (if (overlayp rsearch-overlay)
      (delete-overlay rsearch-overlay))
  (setq rsearch-overlay nil)
  (if (not face)
      (setq rsearch-highlight nil)
    (setq rsearch-highlight t)
    (setq rsearch-highlight-face face)))

;;; ========================================================================
;;; Menu Bar Support - Search, Occur, Replace, Delete lines

;;; This is similar to other Search Menu Bar entries seen on the net,
;;; but the rsearch functions are called for repeatability. 
;;; Perhaps a second menu is required for all of this. 

;;; Skip most of this for V18, Epoch, Lucid.

(if (or (not (string-match "^19" emacs-version))
	(string-match "Lucid" emacs-version))
    (setq rsearch-highlight nil)                 ; Turn it off

  (defvar menu-bar-search-menu (make-sparse-keymap "Search"))

  (define-key menu-bar-search-menu [keep-lines]
    '("Delete non-matching lines" . keep-lines))

  (define-key menu-bar-search-menu [flush-lines]
    '("Delete matching lines" . flush-lines))
 
  (define-key menu-bar-search-menu [search-sep3]
    '("-------------------------"))
 
  (define-key menu-bar-search-menu [replace-regexp-region]
    '("Replace regexp in region" . replace-regexp-region))
  
  (define-key menu-bar-search-menu [replace-word-region]
    '("Replace word in region" . replace-word-region))
  
  (define-key menu-bar-search-menu [replace-string-region]
    '("Replace string in region" . replace-string-region))
  
  (define-key menu-bar-search-menu [query-replace-regexp]
    '("Query replace regexp" . query-replace-regexp))
   
  (define-key menu-bar-search-menu [query-replace-word]
    '("Query replace word" . query-replace-word))
  
  (define-key menu-bar-search-menu [query-replace]
    '("Query replace string " . query-replace))
  
  (define-key menu-bar-search-menu [search-sep2]
    '("-------------------------"))

  (define-key menu-bar-search-menu [count-matches-region]
    '("Count matches in region" . count-matches-region))
  
  (define-key menu-bar-search-menu [occur]
    '("List matching lines (occur)" . occur))

  (define-key menu-bar-search-menu [search-sep1]
    '("-------------------------"))

   (define-key menu-bar-search-menu [rsearch-wrap-toggle]
    '("Wrap / Don\'t Wrap" . rsearch-wrap-toggle))
  
  (define-key menu-bar-search-menu [search-word-b]
    '("Word Search backward" . rsearch-word-backward))
  
  (define-key menu-bar-search-menu [search-word-f]
    '("Word Search forward" . rsearch-word-forward))
  
  (define-key menu-bar-search-menu [search-re-b]
    '("Regexp Search backward ". rsearch-regexp-backward))
  
  (define-key menu-bar-search-menu [search-re-f]
    '("Regexp Search forward ". rsearch-regexp-forward))
  
  (define-key menu-bar-search-menu [search-b]
    '("Search backward " . rsearch-string-backward))
  
  (define-key menu-bar-search-menu [search-f]
    '("Search forward " . rsearch-string-forward))
  
  (define-key menu-bar-search-menu [search-repeat]
    '("Repeat last search" . rsearch-menu-repeat-search))

  (defun rsearch-wrap-toggle ()
     "Toggle whether repeating a failed rsearch will wrap or start anew."
     (interactive)
     (setq rsearch-wrapp (not rsearch-wrapp))
     (if (interactive-p)
	 (message 
	  "Future rsearch commands %s wrap around the end of the buffer."
	  (if rsearch-wrapp "will" "will not"))))

  (defun rsearch-menu-repeat-search ()
    "Menubar command used to repeat last rsearch command."
    (interactive)
    (rsearch-command
     ;; this allows wrapping when appropriate
     (not (eq last-command 'rsearch-command))
     rsearch-direction))
  
  (defun rsearch-install-menubar (&optional arg)
    "Install a menu bar item for searching and replacing.
With prefix arg, de-install the search menu item."
    (interactive "P")
    (if (lookup-key global-map [menu-bar])
	(if arg
	    (define-key global-map [menu-bar search] 'undefined)
	  (define-key-after (lookup-key global-map [menu-bar])
	    [search] (cons "Search" menu-bar-search-menu) 'edit))))

  (rsearch-install-menubar nil) ; install the menubar

  ) ; End of code for menubar
;;;
;;; Some useful little commands which have been put on menubar.
;;;
(defun count-matches-region (beg end regexp)
  "Counts number of matches in region of REGEXP." 
  (interactive "r\nsCount matches in region of regexp: ")
  (if (or (not (stringp regexp))
	  (string= regexp ""))
      (setq regexp "\\>")) ; word end.  Perhaps a silly default.
  (let (mess1)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(setq mess1 (how-many regexp))))
    (message "%s of %s found in region." mess1 regexp)))

(defun query-replace-word ()
  "Query-replace only those matches surrounded by word boundaries."
  (interactive)
  (setq current-prefix-arg 1)	
  (call-interactively 'query-replace))

;;; These 3 replace-in-region commands use perform-replace 
;;; to conform with the standard replace commands conventions regarding
;;; case folding and word delimitation.

(defun replace-string-region (from-string to-string &optional delimited)
  "Replace occurrences in region of FROM-STRING with TO-STRING.  
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and FROM-STRING has no uppercase letters. 
Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries."
  (interactive "*sReplace string in region: \nsReplace string in region %s with: \nP")
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (perform-replace from-string to-string nil nil delimited)))
  (if (interactive-p) (message "Done")))

(defun replace-word-region (word to-string &optional ignored)
  "Replace word-bounded occurrences in region of WORD with TO-STRING. 
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and WORD has no uppercase letters."
  (interactive "*sReplace word in region: \nsReplace word in region %s with: \nP")
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (perform-replace word to-string nil nil t)))
  (if (interactive-p) (message "Done")))

(defun replace-regexp-region (regexp to-string &optional delimited)
  "Replace things in region matching REGEXP with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters. 
Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
In TO-STRING, `\&' stands for whatever matched the whole of REGEXP,
and `\N' (where N is a digit) stands for
whatever what matched the Nth `\(...\)' in REGEXP."
  (interactive "*sReplace regexp in region: \nsReplace regexp in region %s with: \nP")
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (perform-replace regexp to-string nil t delimited)))
  (if (interactive-p) (message "Done")))


(provide 'rsearch)

;;; rsearch.el ends here
