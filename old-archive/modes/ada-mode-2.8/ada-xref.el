;;; ada-xref.el --- use Gnatf for lookup and completion in Ada Mode
;;;                 works with ada-mode.el v2.1 or higher and GNAT
;;; Copyright (C) 1994 by Markus Heritsch

;;; Author:  Markus Heritsch <Markus.Heritsch@studbox.uni-stuttgart.de>
;;; $Revision: 1.4 $

;;; $Date: 1994/11/11 19:53:52 $

;;; This file is not part of GNU Emacs.

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

;;; This Package provides a set of functions to use the output of the
;;; cross reference tool Gnatf that comes with the GNAT Ada compiler
;;; for lookup and completion in ada-mode. To use it in ada-mode, simply
;;; put the following line at the end of ada-mode.el or in your .emacs .
;;;
;;;   (require 'ada-xref)
;;;

;;; Code:

;; ----- Requirements -----------------------------------------------------

(require 'ada-mode)
(require 'compile)


;; ----- USER-OPTIONS -----------------------------------------------------

(defvar ada-xref-complete-casing 'capitalize
  "*Defines casing function for completion of identifiers.
Must be one of 'capitalize-word, 'ada-loose-case-word, 'downcase-word or
'upcase-word.")

(defvar ada-xref-always-gnatf nil
  "*If non-nil, Gnatf is called automatically if necessary
when user executes an Xref command.")

(defvar ada-xref-prog "gnatf"
  "*Name of the crossreferencing program.")

;; ----- Keybindings ------------------------------------------------------

(define-key ada-mode-map [S-mouse-1] 'ada-point-and-xref)
(define-key ada-mode-map "\C-c\C-d" 'ada-goto-declaration)
(define-key ada-mode-map [M-tab] 'ada-complete-identifier)
(define-key ada-mode-map "\C-c\C-x" 'ada-gnatf-current)


;; ----- Main Functions ---------------------------------------------------

(defun ada-point-and-xref ()
  "Calls mouse-set-point and then ada-goto-declaration."
  (interactive)
  (mouse-set-point last-input-event)
  (ada-goto-declaration))


(defun ada-add-xref-menu ()
  "Adds the menu 'Xref' to the menu-bar in Ada-Mode."
  (easy-menu-define t ada-mode-map t
		    '("Xref"
		      ["complete identifier" ada-complete-identifier t]
		      ["goto declaration" ada-goto-declaration t]
		      ["------" nil nil]
		      ["Gnatf current" ada-gnatf-current t]
		      ["next error" next-error (fboundp 'next-error)]	      
		      )))


(defun ada-goto-declaration ()
  "Displays the declaration of the identifier around point."
  (interactive)
  (ada-find-in-gnatf (ada-read-identifier)))


(defun ada-complete-identifier ()
  "Tries to complete the identifier around point."
  (interactive)
  (let ((sofar (downcase (car (ada-read-identifier))))
	(completed nil)
	(symalist nil)
	(insertpos nil)
	(curbuf (current-buffer)))

    ;;
    ;; delete the incomplete identifier in the buffer
    ;;
    (looking-at "[a-zA-Z0-9_]+")
    (delete-region (setq insertpos (match-beginning 0))
		   (match-end 0))

    ;; select Xref buffer
    (set-buffer (ada-get-gnatf-buffer))
    (goto-char (point-max))
    
    (while (search-backward sofar nil t) 
      ;;
      ;; build an alist of possible completions
      ;;
      (or
       (if (looking-at (concat "^\\(" sofar "[a-zA-Z0-9_]*\\)"))
	   (setq symalist (cons (cons (buffer-substring (match-beginning 1)
							(match-end 1))
				      nil)
				symalist)))
       (backward-char 1)
       (if (looking-at (concat "\\.\\(" sofar "[a-zA-Z0-9_]*\\)"))
	   (setq symalist (cons (cons (buffer-substring (match-beginning 1)
							(match-end 1))
				      nil)
				symalist)))))
    
    (or
     ;;
     ;; symbol gets completed as far as possible
     ;;
     (stringp (setq completed (try-completion sofar symalist)))
     ;;
     ;; or is already complete
     ;;
     (setq completed sofar))
    
    ;;
    ;; insert the completed symbol
    ;;
    (set-buffer curbuf)
    (goto-char insertpos)
    (insert completed)
    
    ;;
    ;; casing according to user option
    ;;
    (funcall ada-xref-complete-casing -1)
    ))

	      
(defun ada-gnatf-current (&optional nocheck)
  "Creates a new Xref file from the current source file.
If NOCHECK is non-nil, generates without checking if necessary.
Uses the function compile to execute Gnatf."
  (interactive)
  (let ((xrefname nil)
	(xrefbuf nil)
	(mesgbuf nil)
	(msgflag t)
	(comp-comm compile-command))
    ;;
    ;; get name of Xref file by changing extension to xr?
    ;;
    ;; it only works with the gnat name convention .ad[bs].
    ;; must be generalized (RE) ???
    ;; 
    (setq xrefname (concat (substring (buffer-file-name) 0 -3)
			   "xr"
			   (substring (buffer-file-name) -1)))
    
    ;;
    ;; is Xref file up to date ?
    ;;
    (if (or
	 nocheck
	 (not (ada-check-gnatf xrefname t)))
	;;
	;; no => create a new one
	;;
	(progn
	  ;; kill old existing Xref buffer
	  (if (setq xrefbuf (get-file-buffer xrefname))
	      (kill-buffer xrefbuf))
	  ;; prompt for command to execute Gnatf
	  (setq compile-command (concat "-x5 " (buffer-name)))
	  (setq compile-command (read-from-minibuffer
				 (concat "enter command to execute Gnatf: "
					 ada-xref-prog)
				 compile-command))
	  (setq compile-command (concat ada-xref-prog " " compile-command))
	  ;; use compile to execute gnatf
	  (compile compile-command)
	  ;; restore old compile-command
	  (setq compile-command comp-comm)))
    
    ;; return name of Xref file if it exists
    xrefname))


;; ----- Support Functions ------------------------------------------------
       
(defun ada-check-gnatf (xrefname &optional nomessage)
  "Checks if the Xref file XREFNAME is up to date.
Returns t if it is."
  (if (not (file-exists-p xrefname))
      (if (y-or-n-p "No Xref file - generate ? ")
	  (ada-gnatf-current t)
	(error "no Xref command available")))
  (if (file-newer-than-file-p (buffer-file-name) xrefname)
      (if ada-xref-always-gnatf
	  (progn
	    (ada-gnatf-current t)
	    t)
	(progn
	  (if (not nomessage)
	      (progn
		(message "ATTENTION: Xref information not up to date !")
		(sit-for 0)))
	  nil))
    (progn
      (message "Xref file is up to date")
      t)))


(defun ada-read-identifier ()
  "Reads the indentifier around point.
Returns a list of three elements: 1) the indentifier itself as a string
                                  2) the line # of the identifier
                                  3) the column # of its first character"

  ;; goto first character of the identifier
  (skip-chars-backward "a-zA-Z0-9_")

  ;;
  ;; check if it really is an identifier
  ;;
  (if (or
       (not (looking-at "[a-zA-Z0-9_]+"))
       (and (eq ada-language-version 'ada94)
	    (looking-at (concat "[^_]" ada-94-keywords "[^_]" )))
       (and (eq ada-language-version 'ada83)
	    (looking-at (concat "[^_]" ada-94-keywords "[^_]" ))))
      (error "no identifier around point"))
  (if (ada-in-string-or-comment-p)
      (error "inside string or comment"))

  ;;
  ;; build a list of name, line and column
  ;;
  (looking-at "[a-zA-Z0-9_]+")
  (list	(buffer-substring (match-beginning 0)
			  (match-end 0))
	(count-lines (point-min) (point))
	(1+ (current-column))))


(defun ada-get-gnatf-buffer ()
  "Returns the buffer containing the Xref file."
  (let ((name nil))
    ;;
    ;; get name of Xref file by changing extension to xr?
    ;; knows too much about naming convetion ???
    ;; 
    (setq name (concat (substring (buffer-file-name) 0 -3)
		       "xr"
		       (substring (buffer-file-name) -1)))
    (ada-check-gnatf name)
    (find-file-noselect name)))


(defun ada-in-braces-p ()
  "Returns t if point is inside { ... } ."
  (save-excursion
    (re-search-backward"{\\|}" nil 1)
    (looking-at "{")))

    
(defun ada-find-in-gnatf (identlist)    
  "Looks in gnatf-output of current buffer for identifier in INDENTLIST.
Finds the source file containing the declaration, displays the buffer and 
moves point to this declaration."
  (let ((line nil)        ; line where identifier is in source code
	(column nil)      ; column ''
	(decline nil)     ; line where declaration should be
	(deccol nil)      ; column ''
	(name nil)        ; name of identifier in the source code
	(curfile (file-name-nondirectory (buffer-file-name)))
	(curbuf (current-buffer))
	(xrefname nil)    ; name of the file containing the gnatf output
	(xrefbuf nil)     ; buffer              ''
	(filename nil)    ; name of the file that should contain the declaration
	(declname nil)    ; name of the identifier in the gnatf-output
	(identype nil)    ; type ''
	(dispbuffer nil)  ; buffer containing the declaration
	(found nil)
	(curwin (selected-window))
	(winpoint (window-point (selected-window)))
	(modified nil)
	(opoint (point))
	(over nil))       ; overlay for highlighting the identifier
    
    ;;
    ;; highlight the identifier
    ;;
;    (cond (window-system
;	   (setq over (make-overlay (1+ (point))
;				    (save-excursion
;				      (skip-chars-forward "a-zA-Z0-9_")
;				      (point))))
;	   (copy-face 'default 'identifier)
;	   (invert-face 'identifier)
;	   (overlay-put over 'face 'identifier)
;	   (sit-for 0)
;	   ))

    ;; read the values in identlist
    (setq name (car identlist))
    (setq line (car (cdr identlist)))
    (setq column (car (cdr (cdr identlist))))

    (condition-case err
	;;
	;; on error jump to handler to remove overlay
	;;
	
	(progn
	  
	  ;;
	  ;; the buffer containing the Xref file becomes current
	  ;; but not displayed
	  ;;
	  (setq xrefbuf (set-buffer (ada-get-gnatf-buffer)))
	  (setq xrefname (buffer-file-name))
	  (beginning-of-buffer)
	  
	  ;;
	  ;; search for line:column in Xref file
	  ;; 
	  (while (and (not found)
		      (re-search-forward (concat "[{\\| ]\\("
						 (int-to-string line)
						 "[\n]? *:"
						 (int-to-string column)
						 "\\)[}\\| ]") nil t))
	    ;;
	    ;; get the filename
	    ;;
	    (setq filename (ada-xref-get-filename))    
	    ;;
	    ;; check if it is a declaration
	    ;;
	    (if (save-excursion
		  (forward-char -1)
		  (ada-in-braces-p))
		(setq found t)
	      (progn
		;;
		;; point is already on a declaration => error message
		;;
		(if (string= filename curfile)
		    (progn
		      ;; yes
		      (error "already on declaration")))))
	    ) ; end of loop

	  (catch 'modified
	    ;;
	    ;; on error due to modified buffer jump to handler
	    ;;
	    
	    (progn
	      (if (not found)
		  (if (or (buffer-modified-p curbuf)
			  (file-newer-than-file-p curfile xrefname))
		      ;;
		      ;; position not found due to modified buffer =>
		      ;; try other algorithm.
		      ;;
		      (progn
			(setq modified t)
			(throw 'modified nil))
		    
		    (error (concat "position "
				   line
				   ":"
				   column
				   " not found in "
				   (file-name-nondirectory xrefname)))))
	
	      ;;
	      ;; ok, we found it, now skip back to the column # and read it
	      ;;
	      (re-search-backward "{")
	      (skip-chars-backward " \n\t0-9")
	      (looking-at "[0-9]+")
	      (setq deccol (1- (string-to-int
				(buffer-substring (match-beginning 0)
						  (match-end 0)))))
	      ;;
	      ;; skip back to the line # and read it
	      ;;
	      (skip-chars-backward ": \n")
	      (skip-chars-backward "0-9")
	      (looking-at "[0-9]+")
	      (setq decline (string-to-int
			     (buffer-substring (match-beginning 0)
					       (match-end 0))))
	      ;;
	      ;; skip back to the identifier and read it
	      ;;
	      (skip-chars-backward " ")
	      (skip-chars-backward " a-zA-Z_")
	      (looking-at "[a-zA-Z_]+")
	      ;;
	      ;; check if it is the right one
	      ;;
	      (if (not (string= (downcase name)
				(setq declname
				      (buffer-substring (match-beginning 0)
							(match-end 0)))))
		  (if (or (buffer-modified-p curbuf)
			  (file-newer-than-file-p curfile xrefname))
		      ;;
		      ;; wrong identifier due to modified buffer =>
		      ;; try other algorithm.
		      ;;
		      (progn
			(setq modified t)
			(throw 'modified nil))
		    
		    (error (concat "different identifier "
				   declname
				   " recorded at this position in "
				   (file-name-nondirectory xrefname))))))
	    ) ; end of catch

	  (if modified
	      ;;
	      ;; call the other search-algorithm for modified buffer
	      ;;
	      (setq dispbuffer (ada-find-in-modified name curfile
						     xrefname
						     xrefbuf))
	    
	    (progn		  
	      ;;
	      ;; find declaration in source file
	      ;;
	      (message (concat "finding declaration of " name))
	      ;;
	      ;; check if source file exists
	      ;;
	      (if (not (file-exists-p filename))
		  (error (concat "file " filename " doesn't exist")))
	      ;;
	      ;; make buffer containing the declaration current
	      ;;
	      (set-buffer (setq dispbuffer (find-file-noselect filename)))
	      (beginning-of-buffer)
	      ;;
	      ;; move point to the position of the declaration
	      ;;
	      (goto-line decline)
	      (move-to-column deccol)
	      
	      ;;
	      ;; check if it is the right one
	      ;;
	      (if (not (looking-at (concat name "[^_]")))
		  ;;
		  ;; declaration not found due to modified buffer or file ?
		  ;;
		  (if (or (buffer-modified-p dispbuffer)
			  (file-newer-than-file-p filename xrefname))
		      ;; try the other search-algorithm for modified buffers
		      (if (not (ada-xref-search-nearest name))
			  (error (concat "no declaration of "
					 name
					 " anywhere in file "
					 filename)))
		    
		    (error (concat
			    "no declaration of "
			    name
			    " in "
			    filename
			    " at expected position")))
		(message "found declaration exactly as recorded in Xref file"))
	      ))
	  ) ; end of condition-case
      ;;
      ;; the error handler (needed to delete the overlay)
      ;;
      (error
       (progn
;	 (delete-overlay over)
	 (error "%s" (substring (format "%s" err) 7 -1)))
       nil))

    ;; check if point is now back on original position => error
    (set-buffer dispbuffer)
    (if (= (point) opoint)
	(error "already on declaration"))

    ;; display the buffer containing the declaration in another window
    ; (delete-overlay over)
    (switch-to-buffer-other-window dispbuffer)
    (set-window-point curwin winpoint)))


(defun ada-find-in-modified (name curfile xrefname xrefbuf)
  "Tries to find the declaration of NAME in CURFILE according to XREFNAME
in XREFBUF if source files were modified after running gnatf."
  (let ((declist nil)
	(decline nil)
	(deccol nil)
	(declist nil)
	(len nil)
	(choice nil))

    ;; make Xref buffer current
    (set-buffer xrefbuf)
    (goto-char (point-max))

    ;;
    ;; loop, search for all identifiers 'name'
    ;;
    (while (search-backward name nil t)
      
      ;; check if it is not anything else
      (if (save-excursion
	    (backward-char 1)
	    (or
	     (looking-at (concat "\n" name "\\>[^_]"))
	     (looking-at (concat "\\." name "\\>[^_]"))))

	  (progn
	    ;;
	    ;; get the filename
	    ;;
	    (setq filename (ada-xref-get-filename))
	    
	    (save-excursion
	      ;;
	      ;; get line/column # of the declaration
	      ;;
	      (skip-chars-forward "a-zA-Z_0-9")
	      (skip-chars-forward " \n\t")
	      (looking-at "[0-9]+")
	      (setq decline (string-to-int
			     (buffer-substring (match-beginning 0)
					       (match-end 0))))
	      (goto-char (1+ (match-end 0)))
	      (looking-at "[0-9]+")
	      (setq deccol (1- (string-to-int
				(buffer-substring (match-beginning 0)
						  (match-end 0))))))
	    ;;
	    ;; build a list of lists (name identype decline deccol filename)
	    ;; 
	    (setq declist (cons (list name decline deccol filename) declist)))
	) ; end of if
      ) ; end of loop
    
    ;;
    ;; how many possible declarations ?
    ;;
    (setq len (length declist))
    (cond
     ;;
     ;; none => error
     ;;
     ((= len 0)
      (error (concat "no declaration of "
		     name
		     " recorded in Xref file")))
     ;;
     ;; one => should be the right one
     ;;
     ((= len 1)
      (ada-xref-search-in-source (car declist)))     
     ;;
     ;; more than one => display choice list
     ;;
     (t
      (with-output-to-temp-buffer "*choice list*"
	(princ "Identifier is overloaded and Xref information is not up to date.\n")
	(princ "Possible declarations are:\n\n")
	(princ "  no. in file                           at line  col\n")
	(princ "  ---    ------------------------------    ----  ---\n")
	(let ((counter 1))
	  (while (<= counter len)
	    (princ (format "  %2d)    %-30s    %4d  %3d\n"
			    counter
			    (nth 3 (nth (1- counter) declist))
			    (nth 1 (nth (1- counter) declist))
			    (nth 2 (nth (1- counter) declist))
			    ))
	    (setq counter (1+ counter))
	    ) ; end of while
	  ) ; end of let
	) ; end of with-output-to ...
      (while (or
	      (not choice)
	      (not (integerp choice))
	      (< choice 1)
	      (> choice len))
	(setq choice (string-to-int
		      (read-from-minibuffer "Enter No. of your choice: "))))
      (ada-xref-search-in-source (nth (1- choice) declist))
      ))))


(defun ada-xref-search-in-source (decinf)
  "Searches for declaration of identifier described in DECINF.
DECINF is a list containing name, type, line, column # and filename of
the identifier."
  (let ((name (car decinf))
	(line (car (cdr decinf)))
	(col (nth 2 decinf))
	(filename  (nth 3 decinf))
	(dispbuffer nil)
	(orgpos nil)
	(newpos nil)
	(diff nil))

    ;;
    ;; move to position recorded in Xref file
    ;;
    (set-buffer (setq dispbuffer (find-file-noselect filename)))
    (goto-line line)
    (move-to-column col)
    (if (not (looking-at (concat name "[^_]")))
	;;
	;; if the declaration of name is not exactly there
	;; => look for the nearest declaration of name
	;;
	(progn
	  (if (not (setq newpos (ada-xref-search-nearest name)))
	      (error (concat "no declaration of "
			     name
			     " anywhere in file "
			     filename))))
      (message "found declaration as recorded in Xref file"))
    dispbuffer))
	    
    
(defun ada-xref-search-nearest (name)
  "Searches for NAME nearest to the position recorded in the Xref file.
It returns the position of the declaration in the buffer or nil if not found."
  (let ((orgpos (point))
	(newpos nil)
	(diff nil))
    
    (goto-char (point-max))
    ;;
    ;; loop - look for all declarations of name in this file
    ;;
    (while (search-backward name nil t)

      ;;
      ;; check if it really is a declaration
      ;;
      (if (and
	   (not (save-excursion
		  (goto-char (match-end 0))
		  (looking-at "_")))
	   (not (ada-in-string-or-comment-p))
	   (or
	    ;; variable declaration ?
	    (save-excursion
	      (skip-chars-forward "a-zA-Z_0-9" )
	      (ada-search-ignore-string-comment "[^ \n\t]")
	      (backward-char 1)
	      (looking-at ":[^=]"))
	    ;; procedure, function, task or package declaration ?
	    (save-excursion
	      (ada-goto-previous-word)
	      (looking-at "\\<procedure\\>\\|\\<function\\>\\|\\<type\\>\\|\\<task\\>\\|\\<package\\>\\|\\<body\\>"))))
	  ;;
	  ;; check if it is nearer than the ones before if any
	  ;;
	  (if (or (not diff)
		  (< (abs (- (point) orgpos)) diff))
	      (progn
		(setq newpos (point))
		(setq diff (abs (- newpos orgpos))))))
      ) ; end of loop
    
    (if newpos
	(progn
	  (message "ATTENTION: this declaration is only a (good) guess ...")
	(goto-char newpos))
      nil)))


(defun ada-xref-get-filename ()
  "Returns the name of the file where the declaration should be.
Assumes point to be on the name of the identifier in the Xref file."
  (save-excursion
    (re-search-backward "^\\(%%\\|--\\) \\([a-zA-Z0-9\\._/]+\\)" nil t)
    (file-name-nondirectory
     (buffer-substring (match-beginning 2)
		       (match-end 2)))))
  

;; ----- Add To ada-mode-hook ---------------------------------------------

(add-hook 'ada-mode-hook 'ada-add-xref-menu)


;; ------------------------------------------------------------------------

(provide 'ada-xref)

;;; package ada-xref.el ends here
