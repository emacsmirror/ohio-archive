;From ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!tut.cis.ohio-state.edu!indetech.com!lrs Mon Nov  6 13:10:43 1989
;Article 646 of gnu.emacs
;Path: ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!tut.cis.ohio-state.edu!indetech.com!lrs
;>From lrs@indetech.com (Lynn Slater)
;Newsgroups: gnu.emacs
;Subject: SQL mode
;Message-ID: <m0gNBfg-0000FAC@fire.indetech.com>
;Date: 3 Nov 89 21:42:00 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 644
;
;Enclosed is SQL mode. This mode provides both for editing of sql files
;and for the transmittal to another process of sql statements.  This later
;ability provides a significant functionality improvement over either 'isql'
;(from Sybase) or 'sqlplus' (from Oracle).
;
;In either case, you start the vendor supplied sql interpretor in its own
;buffer (a shell buffer will do), position the cursor on or near the sql you
;wish transmitted, and use one of the transmittal bindings listed below.
;Unlike the vendor supplied sql interpretors, you will have a complete set
;of editing capabilities and can work on more than the most recient
;expression.  All capabilities of the vendor supplied interpretors are
;retained as they are doing the actual work in the subshell.
;
;This mode is very useful for incrimental development of sql scripts. Once
;your expressions are developed with this mode, the file can be saved and
;run in a batch manner.
;
;===============================================================
;Lynn Slater -- lrs@indetech.com or {sun, ames, pacbell}!indetech!lrs
;42075 Lawrence Place, Fremont Ca 94538
;Office (415) 438-2048; Home (415) 796-4149; Fax (415) 438-2034
;===============================================================
;
;-*- File: ~/local/sql.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql.el --- sql mode and support to send statements to sql interpretors.
;; Author          : Lynn R Slater
;; Created On      : Thu Dec 10 15:20:26 1987
;; Last Modified By: Lynn Slater
;; Last Modified On: Fri Nov  3 13:40:22 1989
;; Update Count    : 97
;; Status          : Beta areleased
;;
;; History 		
;; 3-Nov-1989		Lynn Slater	
;;    Last Modified: Fri Nov  3 13:32:34 1989 #93 (Lynn Slater)
;;    Cleaned up for distribution.
;; 15-Jun-1989		Lynn Slater	
;;    Last Modified: Thu Jun 15 10:18:23 1989 #69 (Lynn Slater)
;;    added oracle support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copyright 1989 Lynn Randolph Slater, Jr. (lrs@indetech.com)

;; This file may become part of GNU Emacs.
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(provide 'sql)
(require 'shell)

;; See the sql mode help for instructions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sql-buffer-name "*shell*"
  "Name of the buffer to which the sql commands send data.
   Is typically '*shell*' or '*<host>-telnet'")

(defvar sql-seperator-regexp
  "^[ \t]*go[ \t\n]"  			;; For Sybase "isql" program
  ;; ";[ \t]*$"   				;; For Oracle "sqlplus" program
  "Regexp used to seperate sql forms. It's value should differ from vendor
   to vendor.")

(defun sql-set-buffer ()
  "Sets the buffer to which sql statements will be sent."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (progn
	(setq sql-buffer-name (buffer-name (current-buffer)))
	(message "SQL statements will now sent to buffer %s" sql-buffer-name))
    (error "This buffer is not talking to anybody!")
    ))

;;;;;;;

(defun echo-in-buffer (buffer-name string &optional force-display)
  "Displays string in the named buffer, creating the buffer if needed.
   If force-display is true, the buffer will appear if not already shown."
  ;; lrs April 23, 1987
  (let ((buffer (get-buffer-create buffer-name)))
    (if force-display (display-buffer buffer))
    ;; there is probably a better way to do this
    (set-buffer buffer)
    (end-of-buffer)
    (insert string)
    (if force-display
	(set-window-point (get-buffer-window buffer-name) (point-max)))))

(defmacro sit-pause ()
  (if (fboundp 'sit-for-millisecs)
      (sit-for-millisecs 400)
    (sit-for 1)))

(defun flash-region (min max)
  "Temporarely moves the curser to the endpoints of a region."
  ; should probably be added to emacs, is usefull all over.
  ; April 23, 1987. lrs of Silvar-Lisco @sun!silvlis!lrs
  (interactive "r")
  (save-excursion
    (if (not (eq (point) min))
	(progn
	  (goto-char min)
	  (sit-pause)))
    (if (not (eq (point) max))
	(progn
	  (goto-char max)
	  (sit-pause)))
    ))

;;;;;;;
(defun sql-verify-buffer ()
  "Generates reasonable error messages abouut the sql connection"
  (if (not (get-buffer sql-buffer-name))
      (error "The buffer %s does not exist! Try M-x sql-set-buffer" sql-buffer-name))
  (if (not (get-buffer-process sql-buffer-name))
      (error "The buffer %s is not talking to anybody!" sql-buffer-name)))
  
(defun sql-send-strings (strings)
  ;; To help avoid losing track of what sql is doing, echo.
  ;; Cause the buffer to be always displayed and a hint given.
  (sql-verify-buffer)
  (echo-in-buffer sql-buffer-name (apply 'concat strings))
  ;;(lisp-goto-output-point)
  (let ((string (apply 'concat strings))
	(process  (get-buffer-process sql-buffer-name)))
    (send-string process (concat string "\n"))
    (if (eq (current-buffer) (process-buffer process))
	(set-marker (process-mark process) (point)))
    ))

(defun sql-send-region (start end)
  "Send the region to the SQL process."
  (interactive "r")
  (save-excursion
    (sql-send-strings (list (buffer-substring start end))))
  (flash-region start end)		; April 23, 1987. @sun!silvlis!lrs
  )

(defun sql-left ()
  "Send the expression on this line and to the left of `point' to the SQL
   process." 
  (interactive)
    (sql-send-region
      (progn (beginning-of-line) (point)) (progn (end-of-line)
						 (point))))

(defun sql-mark-current ()
  "Marks as the region the current sql form found by matching pairs of the items
   listed in 'sql-seperater-regexp. Moves to end of region."
  (interactive)
  (beginning-of-line)
  (or (and (re-search-backward sql-seperator-regexp nil t)
	   (goto-char (match-end 0)))
      (and (re-search-backward "^[ \t]*$" nil t) (goto-char (match-end 0)))
      (goto-char (point-min)))
  (skip-chars-forward " \t\n")
  (set-mark (point))
  (re-search-forward sql-seperator-regexp)
  (goto-char (match-end 0))
  )

(defun sql-send-current (arg)
  "Send the current sql form to the sql process named by sql-buffer-name.
   Marks what was sent.
   If given a numeric argument, sends that many expressions."
  (interactive "p")
  (sql-mark-current)
  (call-interactively 'sql-send-region)
  (forward-line 1)
  (if (> arg 1) (ss (1- (prefix-numeric-value arg)))))

(defun sql-goto-error (n)
  "moves to the n'th line in the region"
  (interactive "NLine number of error: ")
  (goto-char (region-beginning))
  (forward-line (1- n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Format Conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sql-convert-sybase-to-oracle ()
  "Converts sql seperated by 'go' statements sql seperated by semi-colons.
   Also converts some other sybase vendor specific stuff into oracle vendor
   specific stuff. I.E.
      datetime -> date
      money -> number
      tinyint -> number
      etc."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (not (eq major-mode 'sql-mode)) (sql-mode))
    (replace-regexp "^[ \t]*go[ \t]*$" ";")

    (goto-char (point-min))
    (replace-string "datetime" "date")

    (goto-char (point-min))
    (replace-string "money" "number /* money */")
   
    (goto-char (point-min))
    (replace-string "tinyint" "number /* tinyintes */")
   
    (goto-char (point-min))
    (replace-string "clustered" "/* clustered */")

    (goto-char (point-min))
    (replace-string "nonclustered" " /* nonclustered */")

    (goto-char (point-min))
    (while (and (re-search-forward "^[ \t]*$" nil t)
		(not (eobp)))
      (forward-line -1)
      (back-to-indentation)
      (let ((column (current-column)))
	(forward-line 1)
	(if (not (= column 0))
	    (progn
	      (kill-region (point) (progn (end-of-line) (point)))
	      (insert (make-string column ? ) "/* Blank Line */")
	      )
	  (forward-line 1)
	  )))
    )
  (message "Oracle conversion complete"))
	     

(defun sql-insert-gos ()
  "Inserts 'go' statements between each apparent block of sql code.
   Is good for making a isql script program out of plain sql."
  (interactive)
  (while (not (eobp))
    (forward-line 1)
    (if (and (looking-at "[a-z]") (not (looking-at "go")))
	(progn (insert "go\n")
	       ;;(forward-line 1)
	       ))
    ))

(defun sql-insert-semi-colons ()
  "Inserts ';'s between each apparent block of sql code.
   Is good for making a isql script program out of plain sql."
  (interactive)
  (while (not (eobp))
    (forward-line 1)
    (if (and (looking-at "[a-z]") (not (looking-at ";")))
	(progn (insert ";\n")
	       ;;(forward-line 1)
	       ))
    ))

(defun sql-make-deinstall-script
  "Turns an install script into a deinstall script by converting create
   commands to drop commands.  Is particularly usefukl with stored
   procedures in Sybase."
  (interactive)
  (keep-lines "^[ \\t]*create")
  (query-replace "create" "drop" nil)
  (delete-matching-lines "clustered")
  (query-replace " as" "" nil)
  (make-header)
  (write-file "/flan/ada/scripts/de-installflan")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mass Actions not possiblew in SQL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sql-drop-tables (query)
  "Drops all the tables named by QUERY.  Is handy as sql does not let the
   table name be passed as an argument."
  (interactive (list (read-string "query: "
				  (format "select TABLE_NAME from all_tables where OWNER = '%s';" (user-login-name)))))
  (sql-verify-buffer)
  (switch-to-buffer sql-buffer-name)
  (goto-char (point-max))
  (let ((here (point))
	(max)
	table
	)
    ;; Send the query
    (insert query)
    (shell-send-input)
    (message "am awaiting sql to list tables")
    (sit-for 5)
    (goto-char (point-max))
    (setq max (point))
    (re-search-backward
     "^\\(\\-+\\|no records selected\\) $") ;; Regexp may be oracle specific
    (forward-line 1)

    (while (re-search-forward "^\\([a-zA-Z][a-zA-Z0-9_]+\\) $" nil t)
      (save-excursion
	(goto-char (point-max))
	(setq table (buffer-substring (match-beginning 1) (match-end 1)))
	(insert "drop table " table)
	(shell-send-input)
	(insert ";")
	(shell-send-input)
	(sit-for 4)
	(message "am giving sql time to drop table '%s'" table)))	
    )
  (message "done"))

(defun sybase-drop-non-system-procs ()
  "Sybase specific drop of all stored procedures not owned by system.
   Is useful to clear out a database.
   (Oracle has no stored procedures and this function is N/A.)"
  (interactive)
  (sql-verify-buffer)
  (switch-to-buffer sql-buffer-name)
  (goto-char (point-max))
  (let ((here (point))
	(max)
	)
    (insert "select sysobjects.name, sysusers.name, sysobjects.uid from sysobjects, sysusers where not sysobjects.uid = 1 and type = \"P \" and sysobjects.uid = sysusers.uid")
    (shell-send-input)
    (insert "go")
    (shell-send-input)
    (sit-for 5)
    (goto-char (point-max))
    (setq max (point))

    (goto-char here)
    (while (re-search-forward "^ \\([^ \t]+\\)[ \t]+\\([^ \t]+\\)" nil t)
      (save-excursion
	(goto-char (point-max))
	(insert "drop proc "
		(buffer-substring (match-beginning 2) (match-end 2))
		"."
		(buffer-substring (match-beginning 1) (match-end 1))
		)
	(shell-send-input)
	(insert "go")
	(shell-send-input)
	(sit-for 1)))	
    ))

(defun sybase-drop-non-system-types ()
  "Sybase specific drop of all types not owned by system.
   Is useful to clear out a database."
  (interactive)
  (sql-verify-buffer)
  (switch-to-buffer sql-buffer-name)
  (goto-char (point-max))
  (let ((here (point))
	(max)
	)
    (insert "select uid, name from systypes where uid != 1")
    (shell-send-input)
    (insert "go")
    (shell-send-input)
    (sit-for 5)
    (goto-char (point-max))
    (setq max (point))

    (goto-char here)
    (while (re-search-forward "^ +\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)" nil t)
      (save-excursion
	(goto-char (point-max))
	(insert "sp_droptype "
		(buffer-substring (match-beginning 2) (match-end 2))
		)
	(shell-send-input)
	(insert "go")
	(shell-send-input)
	(sit-for 1)))	
    ))

(defun sybase-drop-non-system-tables ()
  "Sybase specific drop of all tables not owned by system.
   Is useful to clear out a database."
  (interactive)
  (sql-verify-buffer)
  (switch-to-buffer sql-buffer-name)
  (goto-char (point-max))
  (let ((here (point))
	(max)
	)
    (insert "select sysobjects.name, sysusers.name, sysobjects.uid from sysobjects, sysusers where not sysobjects.uid = 1 and type = \"U \" and sysobjects.uid = sysusers.uid")
    (shell-send-input)
    (insert "go")
    (shell-send-input)
    (sit-for 5)
    (goto-char (point-max))
    (setq max (point))

    (goto-char here)
    (while (re-search-forward "^ \\([^ \t]+\\)[ \t]+\\([^ \t]+\\)" nil t)
      (save-excursion
	(goto-char (point-max))
	(insert "drop table "
		(buffer-substring (match-beginning 2) (match-end 2))
		"."
		(buffer-substring (match-beginning 1) (match-end 1))
		)
	(shell-send-input)
	(insert "go")
	(shell-send-input)
	(sit-for 1)))	
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sql-mode-abbrev-table nil
  "Abbrev table in use in Sql-mode buffers.")

(defvar sql-mode-map nil
  "Keymap used in Sql mode.")

(defun sql-mode ()
  "This mode provides both for editing of sql files and for the transmittal to
another process of sql statements.  This later ability provides a
significant functionality improvement over either 'isql' (from Sybase) or
'sqlplus' (from Oracle).

In either case, you start the vendor supplied sql interpretor in its own
buffer (a shell buffer will do), position the cursor on or near the sql you
wish transmitted, and use one of the transmittal bindings listed below.
Unlike the vendor supplied sql interpretors, you will have a complete set
of editing capabilities and can work on more than the most recient
expression.  All capabilities of the vendor supplied interpretors are
retained as they are doing the actual work in the subshell.

This mode is very useful for incrimental development of sql scripts. Once
your expressions are developed with this mode, the file can be saved and
run in a batch manner.

Control-C ? will give you useage help in much the same manner as
help-for-help. 

Interesting functions probably not bound to anything:
  sql-set-buffer  		-- defines which buffer will be sent to.
  sql-send-region

  sql-make-deinstall-script       
  sql-insert-gos
  sql-convert-sybase-to-oracle
  sql-insert-semi-colons
  sql-drop-tables
  sybase-drop-non-system-procs
  sybase-drop-non-system-types
  sybase-drop-non-system-tables

Command apropos on sql- or sybase- will show all the interesting functions.

Mode Specific Bindings:
     \\{sql-mode-map} "
  (interactive)
  (c-mode)
  (use-local-map sql-mode-map)
  (setq major-mode 'sql-mode)
  (setq mode-name "sql")
  (setq local-abbrev-table sql-mode-abbrev-table)
  (run-hooks 'sql-mode-hook)
  )

(defun show-sql-shell (&optional fcn)
  "Makes the isql shell visible in the other window"
  (interactive)
  (sql-verify-buffer)
  (let ((window  (selected-window)))
    (if (not (eq (window-buffer window) (get-buffer sql-buffer-name)))
	(switch-to-buffer-other-window sql-buffer-name))
    (if fcn (apply fcn nil))
    (select-window window)))

(defun sql-kill-output-from-shell ()
  "In sql shell, Kill shell buffer above current prompt."
  (interactive)
  (show-sql-shell 'kill-output-from-shell))

(defun sql-kill-all-output-from-shell ()
  "In sql shell, Kill shell buffer above current prompt."
  (interactive)
  (show-sql-shell 'kill-all-output-from-shell))

(defun sql-shell-next-command ()
  "Search for the next command in the sql shell."
  (interactive)
  (show-sql-shell 'shell-next-command))

(defun sql-shell-prev-command ()
  "Search for the previous command in a shell."
  (interactive)
  (show-sql-shell 'shell-prev-command))

(defun sql-shell-bottom-command ()
  "Search for the previous command in a shell."
  (interactive)
  (show-sql-shell 'end-of-buffer))

(defun sql-shell-scroll-up ()
  "Scroll-up in the sql shell"
  (interactive)
  (show-sql-shell)
  (scroll-other-window))

(defun sql-shell-scroll-down ()
  "Scroll-down in the sql shell"
  (interactive)
  (sql-verify-buffer)
  (let ((window  (selected-window))
	h)
    (switch-to-buffer-other-window sql-buffer-name)
    (setq h (-  (window-height) 3))
    (select-window window)
    (scroll-other-window  (* -1 h))))  

(defun sql-shell-kill-input ()
  "Do a kill-shell-input in the sql shell"
  (interactive)
  (show-sql-shell 'kill-shell-input))

(defun sql-show-output-from-shell ()
  "In sql shell, Display start of this batch of shell output at top of window.
Also put cursor there."
  (interactive)
  (show-sql-shell 'show-output-from-shell))

(defun sql-help ()
  "You have typed \\[sql-help], the help character.  Type a Sql option:
s  Send current sql expression
e  Same as S
g  Goto error line

B  Move the sql window to the bottom of all input/output
V  Make the sql window become visible in the other window
T  Move the sql window to the top of the last input
+  Move the sql window to the top of the next input
-  Move the sql window to the top of the previous input
N  Do a scroll-up (next-page) in the sql window
P  Do a scroll-down (previous-page) in the sql window
I  Kill all pending unprocessed input not yet sent to sql
U  Kill the results of the last input
K  Kill all results so far

C  Convert to oracle format
"
  (interactive)
  (let ((line-prompt "s e g B V T + - N P  I U K C. Type ? for more help: "))
    (message (substitute-command-keys line-prompt))
    (let ((char (read-char)))
      (if (or (= char ??) (= char help-ch))
	  (save-window-excursion
	    (switch-to-buffer "*Help*")
	    (erase-buffer)
	    (insert (documentation 'sql-help))
	    (goto-char (point-min))
	    (while (memq char (cons help-ch '(?? ?\C-v ?\ ?\177 ?\M-v)))
	      (if (memq char '(?\C-v ?\ ))
		  (scroll-up))
	      (if (memq char '(?\177 ?\M-v))
		  (scroll-down))
	      (message "%s%s: "
		       line-prompt
		       (if (pos-visible-in-window-p (point-max))
			   "" " or Space to scroll"))
	      (let ((cursor-in-echo-area t))
		(setq char (read-char))))))
      (let ((defn (cdr (assq (downcase char)
			     (cdr (cdr (assq 3 sql-mode-map)))))))
	(if defn (call-interactively defn) (ding))))))


(if (not sql-mode-map)
    (progn
      (setq sql-mode-map (copy-keymap c-mode-map))
      ;; take out some C things
      (define-key sql-mode-map ";"     'self-insert-command)
      (define-key sql-mode-map ":"     'self-insert-command)
      ;; Add in SQL things
      (define-key sql-mode-map "\C-ck" 'sql-kill-all-output-from-shell)
      (define-key sql-mode-map "\C-cu" 'sql-kill-output-from-shell)
      (define-key sql-mode-map "\C-ci" 'sql-shell-kill-input)
      (define-key sql-mode-map "\C-c-" 'sql-shell-prev-command)
      (define-key sql-mode-map "\C-c+" 'sql-shell-next-command)
      (define-key sql-mode-map "\C-cp" 'sql-shell-scroll-down)
      (define-key sql-mode-map "\C-cn" 'sql-shell-scroll-up)
      (define-key sql-mode-map "\C-ct" 'sql-show-output-from-shell)
      (define-key sql-mode-map "\C-cv" 'show-sql-shell)
      (define-key sql-mode-map "\C-cb" 'sql-shell-bottom-command)
      (define-key sql-mode-map "\C-cc"  'sql-convert-sybase-to-oracle)
      (define-key sql-mode-map "\C-c\C-@" 'sql-mark-current)
      (define-key sql-mode-map "\C-c\?" 'sql-help)
      (define-key sql-mode-map "\C-cg"  'sql-goto-error)
      (define-key sql-mode-map "\C-x\C-e" 'sql-send-current)
      (define-key sql-mode-map "\C-cs" 'sql-send-current)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL abbreviations. To enable 
;;    (defun on-abbrev () (abbrev-mode 1))
;;    (setq sql-mode-hook 'on-abbrev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (define-abbrev-table 'sql-mode-abbrev-table ())
  (define-abbrev sql-mode-abbrev-table "arc" "archivelog" nil)

  (define-abbrev sql-mode-abbrev-table "s" "select" nil)
  (define-abbrev sql-mode-abbrev-table "f" "from" nil)
  (define-abbrev sql-mode-abbrev-table "fr" "from" nil)
  (define-abbrev sql-mode-abbrev-table "w" "where" nil)
  (define-abbrev sql-mode-abbrev-table "o" "order by" nil)
  (define-abbrev sql-mode-abbrev-table "nu" "number" nil)
  (define-abbrev sql-mode-abbrev-table "da" "date" nil)
  (define-abbrev sql-mode-abbrev-table "co" "connect" nil)
  (define-abbrev sql-mode-abbrev-table "be" "between" nil)
  (define-abbrev sql-mode-abbrev-table "sy" "synonym" nil)
  (define-abbrev sql-mode-abbrev-table "tr" "trigger" nil)
  (define-abbrev sql-mode-abbrev-table "up" "update" nil)
  (define-abbrev sql-mode-abbrev-table "ins" "insert" nil)
  (define-abbrev sql-mode-abbrev-table "gr" "grant" nil)
  (define-abbrev sql-mode-abbrev-table "gra" "grant all to" nil)
  (define-abbrev sql-mode-abbrev-table "pu" "public" nil)
  (define-abbrev sql-mode-abbrev-table "un" "unique" nil)
  (define-abbrev sql-mode-abbrev-table "cl" "cluster" nil)
  (define-abbrev sql-mode-abbrev-table "we" "whenever" nil)
  (define-abbrev sql-mode-abbrev-table "ta" "table" nil)
  (define-abbrev sql-mode-abbrev-table "pr" "priviledges" nil)
  (define-abbrev sql-mode-abbrev-table "dr" "drop" nil)
  (define-abbrev sql-mode-abbrev-table "ro" "rollback" nil)
  (define-abbrev sql-mode-abbrev-table "rb" "rollback" nil)
  (define-abbrev sql-mode-abbrev-table "tr" "transaction" nil)
  (define-abbrev sql-mode-abbrev-table "us" "using" nil)
  (define-abbrev sql-mode-abbrev-table "u" "using" nil)
  )

;-*- End File: ~/local/sql.el


