; -*- Mode: Emacs-Lisp; -*- 

;  Eric Beuscher
;  Tulane University
;  Department of Computer Science
;  simple-bookmarks.el

;;; utility code for adding bookmarks


;; *************** TODO ***************

;; work on the interactive for s-bookmark-set

;; clean up the s-bookmark-alist-name->alist function since i think that it is
;; now obsolete

;; clean up s-bookmark-alist-from-buffer and s-bookmark-alist-from-file
;; there is no need for both of them to do save-excursions and to do
;; (point-min), ...

;; xx finish documenting EACH and EVERY function/variable

;; fix tight-coupling of bookmark-alist's name with bookmark-alist's file

;; a function to test for bookmark-alist multiplicity

;; do i need a close function for bookmark-alists?????
;; for efficiency of maintaining alist-list, maybe


;; *************** NOTES ***************

;; a directory can be a bookmark-alist's file right now     this is not
;; good, but i dont know how to detect if a file is a dir or not
;; this situation arises either from using s-bookmark-read-file-name or by
;; directly editing a bookmark-alist file

;; *************** documentation ***************

;; to be gotten from SIMPLE-BOOKMARKS.NOTES after this file's completion

;; *************** utilities ***************

(defun strip-dir-path-h (filepath length max)
  (cond ((= 0 length) filepath)
	((string= "/" (substring filepath (- length 1) length))
	 (substring filepath length max))
	(t
	 (strip-dir-path-h filepath (- length 1) max))))

(defun strip-dir-path (filepath)
  (let ((len (length filepath)))
    (strip-dir-path-h filepath len len)))

(defun must-read-string (prompt &optional initial-input history)
  "extension of read-string, 
see read-string documentation for info on the arguments

read in a string, if the user enters the empty-string, then reprompt until
the user enters a non-empty string"
  (let ((result (read-string prompt initial-input history)))
    (if (string= result "")
	(must-read-string prompt initial-input history)
      result)))

(defun s-bookmark--read-file-name ()
  "read in a filename
if the file exists, then ask if the user wants this file anyway"
  (let* ((tmpfile (read-file-name "File to save this bookmark set in: "))
	 (file (expand-file-name tmpfile))
	 (res (if (and (file-readable-p file) (file-writable-p file))
		  (if (y-or-n-p
		       (concat "Do you really wish to overwrite \""
			       file "\" "))
		      file
		    nil)
		(if (file-writable-p file)
		    file
		  nil))))
    (or res (s-bookmark--read-file-name))))

;; *************** variables ***************

(defvar s-bookmark--open-alists '()
  "list of known open bookmark alists, as specified by alist-names")

(defvar s-bookmark--alist-prefix-str "s-bookmark-alist-"
  "should be obsolete as i have discovered that it is extraneous")

(defvar s-bookmark-default-dir nil
  "The default place to start looking for a user's bookmarks.
if nil, then current-directory is used")


;; the initial string should be the empty string so that when null string is
;; given for a function like s-bookmark-load, the answer will yield "" as
;; opposed to the default dir that is gotten when nil is used
;; a string
(defvar s-bookmark--last-save-file ""
  "the last file bookmarks were saved to.
of the form <absolute-path>/<name>")

(defvar s-bookmark--last-save-dir ""
  "the last dir that a bookmark was saved in")

;; a symbol
(defvar s-bookmark--current-alist nil
  "the current default alist, associated with s-bookmark-last-save-file.
has the active bookmark alist,
i.e. bookmark-alist := '(<alist-name> '(<bookmark>*))")

(defvar s-bookmark--file-info
  ";;; simple bookmarks bookmark file
;;; this is an auto-generated file
;;; in general, this file should not be directly edited as it will be 
;;; callously overwritten by saves, etc ...         but if you know what you
;;; are doing ...

;;; be careful when renaming bookmark files from the shell since there is
;;; a tight coupling between the name of the file the bookmarks are saved in
;;; and the name associated with the bookmark alist
"
  "the header to be put at the top of bookmark files")

(defvar s-bookmark--alist-list '()
  "the list of open bookmark alists")


;; *************** bookmark record helpers ***************

(defun s-bookmark--rec-properties (bookmark)
  "given a BOOKMARK record, return the BOOKMARK's properties"
  (cadr (cddr bookmark)))

(defun s-bookmark--rec-position (bookmark)
  "given a BOOKMARK record, return the BOOKMARK's position within the
associated file"
  (car (cddr bookmark)))

(defun s-bookmark--rec-file (bookmark)
  "given a BOOKMARK record, return the BOOKMARK's associated file"
  (cadr bookmark))

(defun s-bookmark--rec-name (bookmark)
  "given a BOOKMARK record, return the BOOKMARK's name"
  (car bookmark))

(defun s-bookmark--set-bookmark-name (bookmark name)
  "given a BOOKMARK record, set the name to NAME"
  (setcar bookmark name)
  bookmark)

;; *************** updaters ***************

(defun s-bookmark--add-bmark-to-alist! (bmark alist)
  "add a new bookmark record BMARK to the bookmarks of ALIST"
  (let ((bmks (s-bookmark--alist-bookmarks alist)))
    (if bmks
	(let ((head (car bmks)))
	  (setcdr bmks (cons head (cdr bmks)))
	  (setcar bmks bmark)
	  alist)
      (s-bookmark--set-alist-bookmarks alist (list bmark)))
      ))

(defun s-bookmark--add-to-open! (alist-name)
  "add alist-name to the list of open alists
possibly check if ALIST-NAME isnt already in list"
  (setq s-bookmark--open-alists (cons alist-name s-bookmark--open-alists)))

(defun s-bookmark--add-alist! (alist)
  "add a new alist to the s-bookmark-alist-list,
possibly do some checks to prevent duplicity"
  (setq s-bookmark--alist-list (cons alist s-bookmark--alist-list)))

(defun s-bookmark--change-current-alist! (alist)
  "change s-bookmark-current-alist to ALIST
possibly check if s-bookmark-current-alist isnt ALIST
possibly save the current s-bookmark-current-alist"
  (setq s-bookmark--current-alist alist))

(defun s-bookmark--change-last-save-file! (file)
  "change s-bookmark--last-save-file to FILE"
  (setq s-bookmark--last-save-file file))

;; *************** bookmark-file helpers ***************

(defun s-bookmark--file-data (data)
  "given an s-bookmark file-record, return the bookmark-alist"
  (cadr data))

;; *************** bookmark-alist helpers ***************

(defun s-bookmark--alist-file (alist)
  "given a bookmark-alist record, return the associated file"
  (car (cdr alist)))

(defun s-bookmark--alist-bookmarks (alist)
  "given a bookmark-alist record, return the list of bookmarks"
  (car (cddr alist)))

(defun s-bookmark--set-alist-file (alist file)
  "given a bookmark-alist record, set the file to save in to FILE"
  (let ((list (cdr alist)))
    (setcar list file)
    alist))

(defun s-bookmark--set-alist-bookmarks (alist bmarks)
  "given a bookmark-alist record, set the bookmark list to BMARKS"
  (let ((bmks (cdr (cdr alist))))
    (setcar bmks bmarks)
    alist))

(defun s-bookmark--remove-bmark-from-alist (name alist)
  "remove the bookmark denoted by NAME from the bookmark alist ALIST"
  (s-bookmark--remove-bmark-from-alist-h 
   name
   (s-bookmark--alist-bookmarks alist)))

(defun s-bookmark--remove-bmark-from-alist-h (name bmk-list)  
  (cond ((null bmk-list) (error "the bookmark denoted by: " name
				"is not in the alist: " bmk-list))
	((equal (caar bmk-list) name)
	 (cdr bmk-list))
	(t
	 (cons (car bmk-list)
	       (s-bookmark--remove-bmark-from-alist-h name (cdr bmk-list))))))

;; *************** bookmark-alist-list helpers ***************

(defun s-bookmark--get-alist-list-files ()
  (mapcar 's-bookmark--alist-file s-bookmark--alist-list))

;; *************** get data from records ***************

(defun s-bookmark--get-bookmark (name bookmark-alist)
  "get a bookmark record from ALIST given the bookmark's NAME"
  (assoc name bookmark-alist))

(defun s-bookmark--get-alist (name alist-list)
  "get a bookmark ALIST from the ALIST-LIST, based an ALIST's NAME"
  (assoc name alist-list))

;; *************** helpers ***************

;; obsolete
(defun s-bookmark--alist-name->alist (alist-name)
  "obsolete"
  'obsolete)
;  (make-symbol (concat s-bookmark-alist-prefix-str alist-name)))
  

(defun s-bookmark--file-recordify-alist (alist)
  "create an s-bookmark file record from a bookmark-alist"
  (list 's-bookmark alist))

(defun s-bookmark--file-record-p (item)
  "return ITEM if it is a s-bookmark record, else nil"
  (and item				;; not nil
       (listp item)			;; is a list
       (eq 's-bookmark (car item))	;; has the record flag
       (listp (car (cddr item)))	;; second item is a list
					;; presumable of bookmarks
       item				;; if all good, return item
       ))

;; blatantly stolen from bookmark-buffer-file-name in bookmark.el

(defun s-bookmark--buffer-file-name ()
  "Return the current buffer's file in a way useful for bookmarks.
For example, if this is a Info buffer, return the Info file's name."
  (if (eq major-mode 'Info-mode)
        Info-current-file
    (or
     buffer-file-name
     (if (and (boundp 'dired-directory) dired-directory)
         (if (stringp dired-directory)
             dired-directory
           (car dired-directory))))))

;; most of s-bookmark-alist-from-buffer is blatantly stolen from
;; bookmark-alist-from-buffer in bookmark.el

(defun s-bookmark--alist-from-buffer ()
  "in a buffer, read in data, and return that data if it is a valid
s-bookmark record, else signal an error"
  (let ((result (read (current-buffer))))
    (or (s-bookmark--file-record-p result)
	(error "Record not of s-bookmark format: %S" result))))

;; oldie
;(defun s-bookmark-alist-from-buffer-old ()

;; most of this function was blatantly stolen from bookmark-load in
;; bookmark.el 

(defun s-bookmark--alist-from-file (file)
  "return an s-bookmark alist from FILE.
assumes given a valid file"
  (save-excursion
    (save-window-excursion
      (set-buffer (let ((enable-local-variables nil))
		    (find-file-noselect file)))
      (goto-char (point-min))
      (let ((blist (s-bookmark--alist-from-buffer)))
	(s-bookmark--file-data blist)
	))))

;; most of this function was blatantly stolen from bookmark-write-file in
;; bookmark.el

(defun s-bookmark--write-file (alist file)
  "write a bookmark-alist ALIST to a a FILE,
deleting previous contents of FILE"
  (save-excursion
    (save-window-excursion
      (set-buffer (let ((enable-local-variables nil))
		    (find-file-noselect file)))
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (insert s-bookmark--file-info)
      (pp (s-bookmark--file-recordify-alist alist) (current-buffer))
      (write-file file)
      (kill-buffer (current-buffer))
      )))

;; *************** record creators ***************

(defun s-bookmark--make-bookmark (name &optional file)
  "record maker for bookmarks"
  (list name				;; name of bookmark
	(or file			;; file associated with bookmark
	    (s-bookmark--buffer-file-name))
	nil				;; position within file
	'()				;; property-list
  ))

(defun s-bookmark--make-bookmark-alist (alist-name file)
  "record maker for bookmark-alists"
  (list alist-name			;; name of bookmark-alist
	file				;; file where bookmark-alist is stored
	'()				;; list of bookmarks
	))

;; *************** minibuffer reads ***************

(defun s-bookmark--completing-read-bookmark-alists ()
  "choose one of the bookmark-alist sets from the list of open bookmark-alists"
  (completing-read "Which bookmark set? "	;; prompt
		   (mapcar '(lambda (alist-name);; table
			      (list alist-name))
			   s-bookmark--open-alists)
		   nil				;; predicate
		   t				;; require-match
		   ))

(defun s-bookmark--force-completing-read-bookmark-alists ()
  "choose one of the bookmark-alist sets from the list of open bookmark-alists"
  (let ((val (s-bookmark--completing-read-bookmark-alists)))
    (if (string= val "")
	(s-bookmark--completing-read-bookmark-alists)
      val)))

(defun s-bookmark--completing-read-bookmarks (alist)
  "choose one of the bookmarks from a bookmark-alist"
  (completing-read "Which bookmark? "	;; prompt
		   (mapcar '(lambda (bmark)
			      (list (s-bookmark--rec-name bmark)))
			   (s-bookmark--alist-bookmarks alist))
		   nil			;; predicate
		   t			;; require-match
		   ))

(defun s-bookmark--force-completing-read-bookmarks (alist)
  "choose one of the bookmarks from a bookmark-alist"
  (let ((val (s-bookmark--completing-read-bookmarks alist)))
    (if (string= val "")
	(s-bookmark--completing-read-bookmarks alist)
      val)))

;; *************** file interactions ***************

;; a FILE is <path>/<name>
;; a ALIST is <s-bookmark-alist-prefix-str><name>
;; a NAME is <name>

;(old-alist-name (substring s-bookmark-current-alist
;			   (length s-bookmark-alist-prefix-str)
;			   (length s-bookmark-current-alist)))


(defun s-bookmark-start-new-set (alist-name file)
  "Start a fresh set of bookmark groupings, by giving a NAME for the set,
and a file to save these bookmarks in"
  (interactive
   (let* ((name (must-read-string "Name for bookmark set: "))
	  (file (s-bookmark--read-file-name)))
     (list name file)))
  (let ((new-alist (s-bookmark--make-bookmark-alist alist-name file)))
    (s-bookmark--add-alist! new-alist)
    (s-bookmark--add-to-open! alist-name)
    (s-bookmark--change-last-save-file! file)
    (s-bookmark--change-current-alist! new-alist)
    )
  (message "New bookmark set begun")
  )

;; LEFT OFF

	     
;(defun s-bookmark-load (file msg)
(defun s-bookmark-load (file)
  "Load in a bookmark-file, and place it in its own alist of the form:
s-bookmark-alist-<file>
if MSG is non-nil, then print a message of some sort"
  (interactive
   (list (read-file-name
	  (format "Load bookmarks from: (%s) "
		  s-bookmark--last-save-file)
	  ;; default-directory
	  s-bookmark-default-dir
	  ;nil
	  s-bookmark--last-save-file
	  nil)))
	  ;(strip-dir-path s-bookmark-last-save-file))))

;(message file)

  (let* ((file-sans-path (strip-dir-path file)) ;; NEEDED???
	 (file (expand-file-name file));; mirror input var FILE
	 )
    
    (if s-bookmark--current-alist	;; if there are loaded bookmarks
	(s-bookmark-save-current))

    (let* ((new-alist (if (file-exists-p file);; is file name new?
			  (s-bookmark--alist-from-file file)
			(s-bookmark--make-bookmark-alist
			 file-sans-path
			 file)))
	   (key (car new-alist)))

      (if (not (member key s-bookmark--open-alists))
	  (progn
	    (s-bookmark--add-to-open! key)
	    (s-bookmark--add-alist! new-alist)
	    ))

      (s-bookmark--change-last-save-file! file)
      (s-bookmark--change-current-alist! new-alist)
      ;;(setq s-bookmark--alist-list (cons new-alist s-bookmark--alist-list))

      (message (concat
		"s-bookmark-load of \""
		key
		"\" is complete"))
      )
    ))

(defun s-bookmark-save-as (alist file)
  "save the bookmark-alist ALIST to the file associated with ALIST"
  (interactive
   (let* ((alist-name (s-bookmark--force-completing-read-bookmark-alists))
	  (alist (s-bookmark--get-alist alist-name s-bookmark--alist-list))
	  (file
	   (read-file-name "Save to file: "
			   s-bookmark-default-dir
			   s-bookmark--last-save-file)))
     (list alist file)))

  (if (s-bookmark--file-record-p (s-bookmark--file-recordify-alist alist))
      (s-bookmark--write-file alist file)
    (error "Corrupt s-bookmark-current-alist: %S"
		 s-bookmark--current-alist)))

(defun s-bookmark-save ()
  "save s-bookmark--current-alist"
  (interactive)
  (s-bookmark-save-as s-bookmark--current-alist
		      (s-bookmark--alist-file s-bookmark--current-alist)))

(defalias 's-bookmark-save-current 's-bookmark-save)

;; *************** manipulate bmarks in alists ***************

;; oldie
;(defun s-bookmark-set (name alist-name multiples-p)

(defun s-bookmark-set (name alist-name multiples-p &optional file)
  "Save a bookmark named NAME, in alist specified by ALIST-NAME.
if MULTIPLES-P is non-nil, allow bookmarks of the same name in a single
alist

does not currently allow for creation of a new bookmark alist
see oldie
"
  (interactive
   (let ((name
	  ;; let the user name the bookmark, else default to the buffer-name
	  (let ((res (read-string
		      (format "Bookmark name for buffer: (%s): "
			      (buffer-name)))))
	    (if (string= res "")
		(buffer-name)
	      res)))
	 ;; completing read from one of the valid, open alists
	 (alist-name (s-bookmark--force-completing-read-bookmark-alists))
	 ;; should probably be placed in front of name, make let let*, and
	 ;; used it if defining name
	 ;;(multiples-p (y-or-n-p "Allow duplicate bookmark names? "))
	 )
     (list name alist-name nil)))

  (let ((alist (s-bookmark--get-alist alist-name s-bookmark--alist-list)))

    (let ((sb--ab (s-bookmark--alist-bookmarks alist)))
      (if sb--ab
	  (if (member name sb--ab)
	      (s-bookmark-update name alist)
	    (s-bookmark--add-bmark-to-alist!
	     (s-bookmark--make-bookmark name file)
	     alist))
	(s-bookmark--add-bmark-to-alist!
	     (s-bookmark--make-bookmark name file)
	     alist)))

    ;; if not the current-alist, then make the current alist
    (if (not (equal alist s-bookmark--current-alist))
	(progn
	  (s-bookmark-save-current)
	  (s-bookmark--change-current-alist! alist)))
    t)
  )


(defun s-bookmark-delete (name alist)
  "delete a bookmark NAME from ALIST"
  (interactive
   (let* ((alist (s-bookmark--completing-read-bookmark-alists))
	  (name (s-bookmark--completing-read-bookmarks alist)))
     (list name alist)))

  (s-bookmark--set-alist-bookmarks
   alist
   (s-bookmark--remove-bmark-from-alist name alist))
  )

(defun s-bookmark-jump (name alist)
  "go to bookmark NAME in ALIST"
  (interactive
   (let* ((alist-name (s-bookmark--force-completing-read-bookmark-alists))
	  (alist (s-bookmark--get-alist alist-name s-bookmark--alist-list))
	  (name (s-bookmark--force-completing-read-bookmarks alist))
	  )
     (list name alist)))

  (let ((cell (s-bookmark--jump-noselect
	       (s-bookmark--get-bookmark
		name
		(s-bookmark--alist-bookmarks alist)))))
    (and cell
	 (switch-to-buffer (car cell))
	 ;; jump to point in cell (cdr cell)
	 )))

;; oldie
;(defun s-bookmark-jump-old (name alist)

;; this function is almost entirely blatantly stolen from
;; bookmark-jump-noselect in bookmark.el
(defun s-bookmark--jump-noselect (bmark)
  "returns '(BUFFER . POINT)"
;  (message "entering s-b-jump-noselect")
  (let* ((file (expand-file-name (s-bookmark--rec-file bmark)))
	 ;; get other bmark record data here
	 )
    (if (or
	 (file-exists-p file)
	 ;; other possibilities
	 )
	(save-excursion
	  (set-buffer (find-file-noselect file))
	  ;; jump to position
	  (list (current-buffer) (point))
	  )
      ;; else, if a bookmarks file doesnt exist, try to relocate it, else
      ;; delete it
      )	
    ))

(defun s-bookmark-relocate-alist (alist file)
  "relocate the bookmark-alist ALIST, to be associated with the file FILE"
  (interactive
   (list (s-bookmark--force-completing-read-bookmark-alists)
	 (s-bookmark--read-file-name)))
  (s-bookmark--set-alist-file file alist))

(defun s-bookmark-relocate (name alist)
  "relocate the bookmark denoted by NAME in ALIST to a new file"
  )

(defun s-bookmark-rename (name alist new)
  "rename the bookmark NAME in ALIST
used on s-bookmark-current-alist
"
  (interactive
   (let* ((alist-name (s-bookmark--force-completing-read-bookmark-alists))
	  (alist (s-bookmark--get-alist alist-name s-bookmark--alist-list))
	  (name (s-bookmark--force-completing-read-bookmarks alist))
	  (new (must-read-string "New name for bookmark: ")))
     (list name alist new)))
	 
  (let ((old-bmk (s-bookmark--get-bookmark
		  name
		  (s-bookmark--alist-bookmarks alist))))
    (s-bookmark--set-bookmark-name old-bmk name))

  (message (concat "Bookmark renamed to " new))
  )

(defun s-bookmark-update (name alist)
  "update the position of bookmark NAME in ALIST
used on s-bookmark-current-alist
"
  'update
  )

;; *************** manipulate whole bookmark alists ***************

(defun s-bookmark-open-files-from-alist (alist-name)
  (interactive
   (list
    (s-bookmark--force-completing-read-bookmark-alists)))
  ;; get the bookmark-alist from the list of open bmark alists
  (let ((alist (assoc alist-name s-bookmark--alist-list)))
    (mapcar '(lambda (bmrk)
	       (s-bookmark-jump (car bmrk) alist))
	    (s-bookmark--alist-bookmarks alist))))

;; *************** hyper operations ***************


;; *************** finish ups ***************

(provide 's-bookmarks)

;; *************** the end ***************

;; *************** oldies ***************

;(defun s-bookmark-alist-from-buffer-old ()
;  "in a buffer, read in data, and return that data if it is a valid
;s-bookmark record, else signal an error"
;  (save-excursion
;    (goto-char (point-min))
;    (let ((result (read (current-buffer))))
;      (or (s-bookmark-file-record-p result)
;	  (error "Record not of s-bookmark format: %S" result)))))
      
;(defun s-bookmark-jump-old (name alist)
;  "go to bookmark NAME in ALIST"
;  (interactive
;   (let* ((alist-name (s-bookmark-completing-read-bookmark-alists))
;	  (alist (s-bookmark-get-alist alist-name s-bookmark-alist-list))
;	  (name
;	   (completing-read "Which bookmark? " ;; prompt
;			    (mapcar '(lambda (bmark)
;				       (list (s-bookmark-rec-name bmark)))
;				    (s-bookmark-alist-bookmarks alist))
;			    nil		;; predicate
;			    t		;; require-match
;			    )))
;     (list name alist)))

;  (message "entering s-b-jump")
;  (let ((cell (s-bookmark-jump-noselect
;	       (s-bookmark-get-bookmark name
;					(s-bookmark-alist-bookmarks alist)))))
;    (and cell
;	 (switch-to-buffer (car cell))
;	 ;; jump to point in cell (cdr cell)
;	 )))

;(defun s-bookmark-set (name alist-name multiples-p)
;  "save a bookmark named NAME, in alist specified by ALIST-NAME
;if MULTIPLES-P is non-nil, allow bookmarks of the same name in a single
;alist

;does not currently allow for creation of a new bookmark alist
;see oldie
;"
;  (interactive
;   (let ((name
;	  ;; let the user name the bookmark, else default to the buffer-name
;	  (let ((res (read-string
;		      (format "Bookmark name for buffer: (%s): "
;			      (buffer-name)))))
;	    (if (string= res "")
;		(buffer-name)
;	      res)))
;	 ;; completing read from one of the valid, open alists
;	 (alist-name (s-bookmark--completing-read-bookmark-alists))
;	 ;; should probably be placed in front of name, make let let*, and
;	 ;; used it if defining name
;	 (multiples-p (y-or-n-p "Allow duplicate bookmark names? "))
;	 )
;     (list name alist-name multiples-p)))
;  (let ((open-p (member alist-name s-bookmark--open-alists)))
;    (if open-p
;	(let ((alist (assoc alist-name s-bookmark--alist-list)))
;	  (if (member name (s-bookmark--alist-bookmarks alist))
;	      ;(s-bookmark-update name alist)
;	      'update
;	    (s-bookmark--add-bmark-to-alist!
;	     (s-bookmark--make-bookmark name)
;	     alist))
;	  ;; if not the current-alist, then make the current alist
;	  (if (not (equal alist s-bookmark--current-alist))
;	      (progn
;		(s-bookmark-save-as
;		 s-bookmark--current-alist
;		 (s-bookmark--alist-file s-bookmark--current-alist))
;		(s-bookmark--change-current-alist! alist)))
;	  )
;      (call-interactively 's-bookmark-start-new-set)))
;  (message "Bookmark set")
;  )
