;;; mew-func.el --- Basic functions for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997
;; Revised: Sep  1, 1999

;;; Code:

(defconst mew-func-version "mew-func.el version 0.13")

(require 'mew)

;;
;; List functions
;;

(defun mew-folder-member (a list)
  (catch 'member
    (while list
      (if (or (equal (car list) a)
	      (equal (car list) (file-name-as-directory a)))
	  (throw 'member t))
      (setq list (cdr list)))))

(defmacro mew-case-equal (str1 str2)
  (` (equal (downcase (, str1)) (downcase (, str2)))))

(defun mew-member-case-equal (str list)
  "Return the position matched to STR in LIST"
  (let ((n 0))
    (catch 'member
      (while list
	(if (equal (downcase (car list)) (downcase str))
	    (throw 'member n))
	(setq list (cdr list))
	(setq n (1+ n))))))

(defun mew-member-match (str list &optional case-ignore)
  (let ((n 0) (case-fold-search case-ignore))
    (catch 'member
      (while list
	(if (string-match (car list) str)
	    (throw 'member n))
	(setq list (cdr list))
	(setq n (1+ n))))))

(defun mew-uniq-list (lst)
  "Distractively uniqfy elements of LST."
  (let ((tmp lst))
    (while tmp (setq tmp (setcdr tmp (delete (car tmp) (cdr tmp))))))
  lst)

(defun mew-uniq-alist (alst)
  "Distractively uniqfy elements of ALST."
  (let ((tmp alst))
    (while tmp (setq tmp (setcdr tmp (mew-delete (car (car tmp)) (cdr tmp))))))
  alst)

(defun mew-delete (key alist)
  "Destructively delete elements whose first member is equal to key"
  (if (null key)
      alist
    (let (ret)
      (while (and (listp (nth 0 alist)) (equal (car (nth 0 alist)) key))
	(setq alist (cdr alist)))
      (setq ret alist)
      (while alist
	(if (and (listp (nth 1 alist)) (equal (car (nth 1 alist)) key))
	    (setcdr alist (cdr (cdr alist)))
	  (setq alist (cdr alist))))
      ret)))

;;
;; Associative list functions
;;

(defun mew-assoc-equal (key alist nth)
  (let (a n)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(setq n (nth nth a))
	(if (or (equal n key) (eq n t))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun mew-assoc-case-equal (key alist nth)
  (let ((skey (downcase key)) a n)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(setq n (nth nth a))
	(if (or (and (stringp n) (equal (downcase n) skey))
		(eq n t))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun mew-assoc-match (key alist nth)
  (let ((case-fold-search t) a n)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(setq n (nth nth a))
	(if (or (and (stringp n) (string-match key n))
		(equal n key) (eq n t))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun mew-assoc-match2 (key alist nth)
  (let ((case-fold-search t) a n)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(setq n (nth nth a))
	(if (or (and (stringp n) (string-match n key))
		(equal n key) (eq n t))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun mew-assoc-match3 (key alist nth)
  (let ((case-fold-search t) (i 0) a n )
    (catch 'loop
      (while alist
	(setq a (car alist))
	(setq n (nth nth a))
	(if (or (and (stringp n) (string-match n key))
		(equal n key) (eq n t))
	    (throw 'loop (cons i a)))
	(setq i (1+ i))
	(setq alist (cdr alist))))))

(defmacro mew-assoc-member (key lol nth)
  (` (mew-assoc-member-base (, key) (, lol) (, nth) (function member))))

(defmacro mew-assoc-member-case-equal (key lol nth)
  (` (mew-assoc-member-base (, key) (, lol) (, nth)
			    (function mew-member-case-equal))))

(defun mew-assoc-member-base (key lol nth func)
  (let (l)
    (catch 'loop
      (while lol
	(setq l (car lol))
	(setq lol (cdr lol))
	(if (and (listp (nth nth l)) (funcall func key (nth nth l)))
	    (throw 'loop l))))))

;;
;; getting next
;;

(defun mew-get-next (LIST MEM)
  (let (frst next crnt)
    (setq frst (car LIST))
    (setq LIST (cdr LIST))
    (setq next (car LIST))
    (if (equal frst MEM)
	(if next next frst)
    (catch 'loop
      (while LIST
	(setq crnt next)
	(setq LIST (cdr LIST))
	(setq next (car LIST))
	(if (equal crnt MEM)
	    (throw 'loop (if next next frst))))))))


;;
;; Generic extract functions
;;

(defun mew-subsequence (seq beg &optional end)
  (cond
   ((vectorp seq)
    (mew-subvector seq beg end))
   ((stringp seq)
    (substring  seq beg end))
   ((listp seq)
    (mew-sublist seq beg end))
   (t nil)))

(defun mew-sublist (list beg &optional end)
  (let (i ret)
    (if (null end) (setq end (length list)))
    (setq end (1- end))
    (setq i end)
    (while (<= beg i)
      (setq ret (cons (nth i list) ret))
      (setq i (1- i)))
    ret))

(defun mew-subvector (vec beg &optional end)
  (let (i j ret)
    (if (null end) (setq end (length vec)))
    (setq ret (make-vector (- end beg) nil))
    (setq i beg j 0)
    (while (< i end)
      (aset ret j (aref vec i))
      (setq i (1+ i) j (1+ j)))
    ret))

;;
;; String
;;

(defun mew-replace-character (string from to)
  (let ((len (length string))
	(cnt 0))
    (while (< cnt len)
      (if (char-equal (aref string cnt) from)
	  (aset string cnt to))
      (setq cnt (1+ cnt)))
    string))

(defun mew-match (pos &optional string)
  "Substring POSth matched from STRING or the current buffer"
  (cond 
   ((stringp string)
    (substring string (match-beginning pos) (match-end pos)))
   (t 
    (mew-buffer-substring (match-beginning pos) (match-end pos)))))

(defun mew-substring (str width)
  "Return multi-lingual-safe substring. The length of results
are less than or equal to width."
  ;; This code assumes that "str" is short enough.
  ;; The algorithm is not effective. O(N).
  (if (fboundp 'mew-string-to-list)
      (let ((char-list (mew-string-to-list str))
	    (i 0) (w 0))
	(catch 'loop
	  (while char-list
	    (setq w (+ w (mew-char-width (car char-list))))
	    (if (> w width) (throw 'loop nil))
	    (setq i (+ i (length (char-to-string (car char-list)))))
	    (setq char-list (cdr char-list))))
	(substring str 0 i))
    (substring str 0 width)))

;;
;; Insertion
;;

(defun mew-insert (form str)
  (if str
      (if form
	  (insert (format form str))
	(insert str))))

(defmacro mew-insert-message (fld msg rcs size)
  (` (if (or (mew-folder-newsp (, fld)) (mew-folder-imapp (, fld)))
	 (mew-piolet
	  (, rcs) mew-cs-dummy
	  (mew-summary-im-start mew-prog-imcat (, fld) nil (, msg)
				nil nil (, rcs)))
       (mew-frwlet
	(, rcs) mew-cs-dummy
	(insert-file-contents (mew-expand-folder (, fld) (, msg))
			      nil 0 (, size))))))

;;
;; Stolen form Perl
;;

(defmacro mew-join (separator string-list)
  (` (mapconcat (function identity) (, string-list) (, separator))))

(defun mew-split (str sepchar &optional qopen qclose)
  "Return a list of strings splitting STR with SEPCHAR.
If QUOTEDCHAR is provided, SEPCHARs between QOPEN and QCLOSE are
ignored."
 (let ((qlevel 0) (len (length str)) (start 0) (n 0) ret c)
   (if (and qopen (not qclose)) (setq qclose qopen))
   (while (< n len)
     (setq c (aref str n))
     (cond 
      ((and qopen (char-equal c qopen))
       (setq qlevel (1+ qlevel)))
      ((and qclose (char-equal c qclose))
       (setq qlevel (1- qlevel)))
      ((char-equal c sepchar)
       (if (>= qlevel 1)
	   ()
	 (setq ret (cons (substring str start n) ret))
	 (setq start (1+ n)))))
     (setq n (1+ n)))
   (setq ret (cons (substring str start n) ret))
   (nreverse ret)))

(defun mew-chop (str)
  "Split off preceding and following white spaces."
  (let ((i 0) (j (length str)) c)
    (catch 'loop
      (while (< i j)
	(setq c (aref str i))
	(if (or (char-equal c 32) (char-equal c ?\t))
	    (setq i (1+ i))
	  (throw 'loop nil))))
    (setq j (1- j))
    (catch 'loop
      (while (< i j)
	(setq c (aref str j))
	(if (or (char-equal c 32) (char-equal c ?\t))
	    (setq j (1- j))
	  (throw 'loop nil))))
    (substring str i (1+ j))))
	
;;
;; Folder
;;

(defmacro mew-folder-mailp (folder)
  (` (string-match "^+" (, folder))))

(defmacro mew-folder-newsp (folder)
  (` (string-match "^-" (, folder))))

(defmacro mew-folder-local-newsp (folder)
  (` (string-match "^=" (, folder))))

(defmacro mew-folder-imapp (folder)
  (` (string-match "^%" (, folder))))

(defmacro mew-folder-virtualp (folder)
  (` (string-match "^\\+\\+" (, folder))))

(defmacro mew-folder-to-dir (folder)
  (` (if (string-match "^[+=]" (, folder))
	 (substring (, folder) 1 nil)
       (, folder))))

(defmacro mew-folder-localp (folder)
  (` (or (mew-folder-mailp (, folder)) (mew-folder-local-newsp (, folder)))))

(defmacro mew-folder-remotep (folder)
  (` (or (mew-folder-newsp (, folder)) (mew-folder-imapp (, folder)))))

(defun mew-expand-folder (folder &optional message)
  (let ((subdir (substring folder 1 nil))
	dir)
    (cond
     ((mew-folder-mailp folder)
      (setq dir (expand-file-name subdir mew-mail-path)))
     ((mew-folder-local-newsp folder)
      (setq dir (expand-file-name subdir mew-news-path)))
     ((mew-folder-imapp folder)
      (setq dir (mew-imap-folder-dir folder mew-mail-path)))
     ((file-name-absolute-p folder)
      (setq dir (expand-file-name folder))))
    (if message
	(expand-file-name message dir)
      dir)))

(defun mew-expand-folder-get-msg (folder message)
  (let ((subdir (substring folder 1 nil))
	dir file)
    (cond
     ((mew-folder-remotep folder)
      (if (mew-folder-newsp folder)
	  (setq dir (expand-file-name subdir mew-temp-dir))
	(setq dir (mew-imap-folder-dir folder mew-temp-dir)))
      (if (file-directory-p dir)
	  ()
	(mew-make-directory dir))
      (setq file (expand-file-name message dir))
      (if (not (file-readable-p file))
	  (mew-summary-im-start
	   mew-prog-imcat folder nil message nil nil mew-cs-text-for-read 'noinsert))
      file)
     (t
      (mew-expand-folder folder message)))))

(defun mew-imap-folder-dir (folder dir)
  (let ((spec (mew-imap-folder-fullspec folder)))
    (string-match "^%\\(.*\\)//\\([^:]+\\):\\([^@]+\\)@\\(.*\\)$" spec)
    (expand-file-name
     (concat "@" (mew-match 4 spec)
	     "#" (mew-match 3 spec)
	     mew-path-separator (mew-match 1 spec))
     dir)))

(defun mew-imap-folder-fullspec (folder)
  "Make full-spec IMAP folder name of FOLDER."
  (let ((fold nil)
	(user (user-login-name))
	(auth "//AUTH")
	(host "@localhost"))
    (if mew-imap-account
	(progn
	  (string-match
	   "^\\(/[^@:]+\\)?:?\\([^/@]+\\)?\\(@.+\\)?$" mew-imap-account)
	  (if (match-beginning 1) (setq auth (concat "/" (mew-match 1 mew-imap-account))))
	  (if (match-beginning 2) (setq user (mew-match 2 mew-imap-account)))
	  (if (match-beginning 3) (setq host (mew-match 3 mew-imap-account)))))
    (string-match "^\\([^:@]+\\)\\(:\\([^@]+\\)\\)?\\(@.+\\)?$" folder)
    (setq fold (mew-match 1 folder))
    (if (match-beginning 3) (setq user (mew-match 3 folder)))
    (if (match-beginning 4) (setq host (mew-match 4 folder)))
    (if (string-match "\\(.*\\)\\(//.+\\)" fold)
	(progn
	  (setq auth (concat (mew-match 2 fold)))
	  (setq fold (mew-match 1 fold))))
    ;; %folder//auth:user@server
    (concat fold auth ":" user host)))

(defun mew-remote-folder-cache-delete (folder)
  "Delete cached messages for remote FOLDER."
  (let (dir files file)
    (if (mew-folder-newsp folder)
	(setq dir (expand-file-name (substring folder 1) mew-temp-dir))
      (setq dir (mew-imap-folder-dir folder mew-temp-dir)))
    (if (and (file-exists-p dir) (file-directory-p dir))
	(progn
	  (setq files (file-name-all-completions "" dir))
	  (mapcar (function (lambda (x)
			      (and (string-match "[0-9]+" x)
				   (setq file (expand-file-name x dir))
				   (file-readable-p file)
				   (file-writable-p file)
				   (delete-file file))))
		  files)))))

(defun mew-folder-check (folder &optional force-to-create)
  "A function to see if FOLDER exists.
Return t if exists or created. Otherwise, return nil."
  (if (not (stringp folder))
      nil ;; wrong
    (let ((absdir (mew-expand-folder folder))  ;; /home/Mail/foo
	  (folders-file (expand-file-name mew-folders-file mew-mail-path))
	  (create-it force-to-create)
	  append-it)
      (if (file-exists-p absdir)
	  (if (file-directory-p absdir)
	      (progn
		;; The folder doesn't exist in mew-folder-list if
		;; a user creates it by hand...
		(mew-folder-setup folder) ;; just in case
		t) ;; exists
	    (message "%s is a file" folder)
	    nil) ;; exists but a file
	(if (null create-it)
	    (if (mew-folder-imapp folder)
		(if (y-or-n-p (format "Maybe %s doesn't exist (cache-dir not found). Create it? " folder))
		    (setq create-it t))
	      (if (y-or-n-p (format "%s doesn't exist. Create it? " folder))
		  (setq create-it t))))
	(if (not create-it)
	    nil ;; not created
	  (mew-make-directory absdir)
	  (setq append-it (mew-folder-setup folder))
	  (if (and append-it (file-writable-p folders-file))
	      (save-excursion
		(mew-set-buffer-tmp)
		(insert folder)
		(insert "\n")
		(write-region (point-min) (point-max)
			      folders-file 'append 'no-msg)))
	  t))))) ;; created

(defun mew-folder-new-message (folder &optional num-only)
  (let ((files (directory-files (mew-expand-folder folder)))
	(max 0) cur file maxfile)
    (while files
      (setq file (car files))
      (setq files (cdr files))
      (if (not (string-match "^[0-9]+$" file))
	  ()
	(setq cur (string-to-int file))
	(if (> cur max)
	    (setq max cur))))
    (setq max (int-to-string (1+ max)))
    (setq maxfile (mew-expand-folder folder max))
    (if (not (file-exists-p maxfile))
	() ;; OK
	;; due to NFS?
      (mew-touch-folder folder) ;; can clear cache?
      (setq files (directory-files (mew-expand-folder folder)))
      (setq max 0)
      (while files
	(setq file (car files))
	(setq files (cdr files))
	(if (not (string-match "^[0-9]+$" file))
	    ()
	  (setq cur (string-to-int file))
	  (if (> cur max)
	      (setq max cur))))
      (setq max (int-to-string (1+ max)))
      (setq maxfile (mew-expand-folder folder max)))
    (while (file-exists-p maxfile)
      (setq max (read-string (format "%s/%s exists. Input a message number : " max folder)))
      (while (not (string-match "^[0-9]+$" max))
	(setq max (read-string "Input NUMBER : ")))
      (setq maxfile (mew-expand-folder folder max)))
    (if num-only
	max
      maxfile)))

(defun mew-folder-list (folder)
  "List up subfolders of FOLDER."
  (let (folders)
    (condition-case nil
	(let ((case-fold-search t)
	      curdir dirent relpath abspath attr
	      subprefix subfolder)
	  (setq curdir (mew-expand-folder folder))
	  (if (string-match "^[+=]$" folder)
	      (setq subprefix folder)
	    (setq subprefix (concat folder mew-path-separator)))
	  (setq dirent (directory-files curdir))
	  (while dirent
	    (setq relpath (car dirent))
	    (setq dirent (cdr dirent))
	    (setq abspath (expand-file-name relpath curdir))
	    (setq attr (mew-file-attributes abspath))
	    (if (or (equal relpath ".")
		    (equal relpath "..")
		    (not (nth 0 attr))) ;; this is a file
		() 
	      (setq subfolder (concat subprefix relpath))
	      (setq folders (nconc folders (list subfolder)))
	      (if (or (and mew-folder-list-skip-pattern
			   (not (string-match mew-folder-list-skip-pattern
					      relpath)))
		      (/= (nth 1 attr) 2)) ;; # of links
		  (setq folders (nconc folders (mew-folder-list subfolder))))))
	  folders) ;; return value
      (file-error folders))))

(defun mew-touch-folder (fld)
  (if (and mew-touch-folder-p (stringp mew-summary-touch-file))
      (let ((file (expand-file-name mew-summary-touch-file
 				    (mew-expand-folder fld))))
 	(if (file-writable-p file)
 	    (write-region (point-min) (point-min) file nil 'no-msg)))))

;;
;; Directory
;;

(defmacro mew-directory-empty-p (dir)
  "See if DIR never contains files"
  (` (null (nth 2 (directory-files (, dir))))))

(defun mew-make-directory (path)
  (let ((parent (directory-file-name (file-name-directory path))))
    (if (null (file-directory-p parent))
	(mew-make-directory parent))
    (if (and (file-exists-p path) (not (file-directory-p path)))
	(delete-file path))
    (make-directory path)
    (if (/= mew-folder-mode (mew-file-get-mode path))
	(set-file-modes path mew-folder-mode))))

(defun mew-delete-directory-recursively (dir)
  (if (not (file-directory-p dir))
      ()
    (let ((files (directory-files dir t "^[^.]\\|^.[^.]")))
      (while files
	(cond
	 ((file-symlink-p (car files))
	  ;; never chase symlink which points a directory
	  (delete-file (car files)))
	 ((file-directory-p (car files))
	  (mew-delete-directory-recursively (car files)))
	 (t
	  (delete-file (car files))))
	(setq files (cdr files))))
    (if (null (directory-files dir t "^[^.]\\|^.[^.]"))
	(delete-directory dir))))

;;
;; File existence
;;

(defun mew-which (file path)
  (catch 'loop
    (while path
      (if (file-exists-p (expand-file-name file (car path)))
	  (throw 'loop (expand-file-name file (car path)))
	(setq path (cdr path))))))

(defun mew-which-el (elfile path)
  (or (mew-which (concat elfile ".el") path)
      (mew-which (concat elfile ".elc") path)))

;;
;; File
;;

(defun mew-file-attributes (filename)
  "Same as file-attributes, but chase it if FILENAME is a symbolic-link."
  (let ((w32-get-true-file-link-count t) ; for Meadow
	attr symlnk)
    (while (stringp (setq symlnk (car (setq attr (file-attributes filename)))))
      (setq filename
	    (expand-file-name symlnk (file-name-directory filename))))
    attr))

;; file-regular-p is not available in Emacs 19.
;; This allows symbolic link now.
;; Should be writable, anyway.
(defmacro mew-file-regular-p (file)
  (` (and (file-exists-p (, file))
	  (file-writable-p (, file))
	  (not (file-directory-p (, file))))))

(defmacro mew-file-get-time (file)
  (` (nth 5 (file-attributes (, file)))))

(defmacro mew-file-get-size (file)
  (` (nth 7 (file-attributes (, file)))))

(defun mew-file-get-mode (file)
  (let* ((mode (nth 8 (file-attributes file)))
	 (len (length mode))
	 (i 1)
	 (dec 0))
    (while (< i len)
      (setq dec (* dec 2))
      (if (not (char-equal (aref mode i) ?-))
	  (setq dec (1+ dec)))
      (setq i (1+ i)))
    dec))

(defun mew-file-from-home (str)
  (if (string-match (expand-file-name mew-home) str)
      (concat mew-home (substring str (match-end 0)))
    str))

;;
;; temp name
;;

(defun mew-make-temp-name (&optional fname)
  (if (not (file-exists-p mew-temp-dir))
      (mew-make-directory mew-temp-dir)) ;; just in case
  (if fname
      (expand-file-name fname mew-temp-dir)
    (make-temp-name mew-temp-file)))

;;
;; Random
;;

(defvar mew-random-last nil)

(defun mew-random-string ()
  (let ((num (% (abs (random)) 1000)))
    (while (equal mew-random-last num)
      (setq num (% (abs (random)) 1000)))
    (setq mew-random-last num)
    (int-to-string num)))

(defvar mew-random-base "0123456789abcdefghijklmnopqrstuvwxwz")

(defun mew-random-char ()
  (let* ((len (length mew-random-base))
	 (idx (% (abs (random)) len)))
    (substring mew-random-base idx (1+ idx))))

(defun mew-random-filename (dir &optional suffix)
  (let ((cnt 0) (max (length mew-random-base))
	file filepath)
    (setq file (concat (mew-random-char) suffix))
    (setq filepath (expand-file-name file dir))
    (while (and (file-exists-p filepath) (< cnt max))
      (setq file (concat (mew-random-char) suffix))
      (setq filepath (expand-file-name file dir))
      (setq cnt (1+ cnt)))
    (if (file-exists-p filepath) 
	nil
      filepath)))

;;
;; Buffer
;;

(defun mew-set-buffer-tmp (&optional dir)
  (set-buffer (get-buffer-create mew-buffer-tmp))
  (setq buffer-read-only nil)
  (mew-erase-buffer)
  (if dir (cd dir) (cd mew-home)))

(defmacro mew-elet (&rest body)
  (` (let ((buffer-read-only nil)
	   (inhibit-read-only t))
       (,@ body))))

(defmacro mew-erase-buffer ()  
  '(mew-elet
    (widen)
    (erase-buffer)))

(defmacro mew-region-bytes (beg end buf)
  (` (if (fboundp 'string-bytes)
	 (save-excursion
	   (set-buffer (, buf))
	   (string-bytes (buffer-substring (, beg) (, end))))
       (- (, end) (, beg)))))

;;
;; process
;;

(defun mew-im-call-process (msg prog &rest arg)
  (let (ret)
    (and msg (message "%s ..." msg))
    (setq ret (apply (function call-process)
		     prog nil t nil (append arg mew-prog-im-arg)))
    (if (equal ret 0)
	(progn
	  (and msg (message "%s ... done" msg))
	  t)
      (if msg (message "%s ... failed" msg) (message "failed"))
      nil)))

(defun mew-im-call-process-no-output (msg prog &rest arg)
  (let (ret)
    (and msg (message "%s ..." msg))
    (setq ret (apply (function call-process)
		     prog nil nil nil (append arg mew-prog-im-arg)))
    (if (equal ret 0)
	(progn
	  (and msg (message "%s ... done" msg))
	  t)
      (if msg (message "%s ... failed" msg) (message "failed"))
      nil)))

(defun mew-im-start-process (prog name &rest arg)
  (apply (function start-process)
	 name (current-buffer) prog
	 (append arg mew-prog-im-arg)))

(defconst mew-im-boundary-begin "^---BEGIN-IMGET-MESSAGE---\n")

(defun mew-summary-im-start (imprg src dsts msgs discard &optional arg rcs noinsert)
  (let (dir file)
    (if (mew-folder-newsp src)
	(setq dir (expand-file-name (substring src 1 nil) mew-temp-dir))
      (if (mew-folder-imapp src)
	  (setq dir (mew-imap-folder-dir src mew-temp-dir))
	(setq dir nil)))
    (cond
     (discard
      (mew-summary-im-start2 imprg src dsts msgs discard arg rcs)
      (if (not dir)
	  ()
	(or (listp msgs) (setq msgs (list msgs)))
	(mapcar (function (lambda (x)
			    (setq file (expand-file-name x dir))
			    (if (file-readable-p file)
				(delete-file file))))
		msgs)))
     (t  ;; imcat
      (setq file (expand-file-name msgs dir))
      (if (not (file-readable-p file))
	  (save-excursion
	    (mew-set-buffer-tmp)
	    (mew-summary-im-start2 imprg src dsts msgs discard arg mew-cs-text-for-read)
	    (if (file-directory-p dir)
		()
	      (mew-make-directory dir))
	    (mew-frwlet
	     mew-cs-text-for-read mew-cs-dummy
	     (write-region (point-min) (point-max) file nil 'no-msg))
	    (if (/= mew-file-mode (mew-file-get-mode file))
		(set-file-modes file mew-file-mode))
	    (erase-buffer)))
      (or noinsert
	  (mew-frwlet
	   (or rcs mew-cs-text-for-read) mew-cs-dummy
	   (insert-file-contents file)))))))

(defun mew-summary-im-start2 (imprg src dsts msgs discard arg rcs)
  (let ((process-connection-type mew-connection-type2)
	(imarg (if arg (cons arg mew-prog-im-arg) mew-prog-im-arg)))
    ;; dsts can be nil. disappears thanks to append
    (or (listp dsts) (setq dsts (list dsts)))
    (or (listp msgs) (setq msgs (list msgs)))
    (if (not discard) (setq imarg (cons "--boundary=yes" imarg)))
    (setq mew-summary-buffer-string nil)
    (setq mew-summary-buffer-config mew-config-imget)
    (setq mew-summary-buffer-begin nil)
    (setq mew-summary-buffer-process (apply (function start-process)
					    imprg (current-buffer) imprg
					    (format "--src=%s" src)
					    (append imarg dsts msgs)))
    (mew-set-process-cs mew-summary-buffer-process
			(or rcs mew-cs-text-for-read)
			mew-cs-dummy)
    (if discard
	(set-process-filter mew-summary-buffer-process 'mew-summary-im-filter1)
      (set-process-filter mew-summary-buffer-process 'mew-summary-im-filter2))
    (set-process-sentinel mew-summary-buffer-process 'mew-summary-im-sentinel)
    (if (not discard) (message "Caching a message ... "))
    (while mew-summary-buffer-process
      (if mew-xemacs-p
	  (accept-process-output)
	(sit-for 1)
	;; accept-process-output or sleep-for is not enough
	(discard-input)))
    (if (not discard) (message "Caching a message ... done"))))

;; discard
(defun mew-summary-im-filter1 (process string)
  (save-excursion
    (set-buffer (process-buffer process)) ;; necessary
    (setq mew-summary-buffer-string (concat mew-summary-buffer-string string))
    (if (string-match "^Password (\\([^\)]+\\))" mew-summary-buffer-string)
	(progn
	  (process-send-string
	   process
	   (format "%s\n" (mew-summary-scan-passwd
			   (mew-match 1 mew-summary-buffer-string))))
	  (setq mew-summary-buffer-string "")))))

;; accept
(defun mew-summary-im-filter2 (process string)
  ;; can't use save-excursion
  (let ((obuf (current-buffer)))
    (set-buffer (process-buffer process)) ;; necessary
    (cond
     (mew-summary-buffer-begin
      (insert string))
     (t
      (setq mew-summary-buffer-string
	    (concat mew-summary-buffer-string string))
      (if (string-match "^Password (\\([^\)]+\\))" mew-summary-buffer-string)
	  (progn
	    (process-send-string
	     process
	     (format "%s\n" (mew-summary-scan-passwd
			     (mew-match 1 mew-summary-buffer-string))))
	    (setq mew-summary-buffer-string "")))
      (if (string-match mew-im-boundary-begin mew-summary-buffer-string)
	  (progn
	    (insert (substring mew-summary-buffer-string (match-end 0)))
	    (setq mew-summary-buffer-string "")
	    (setq mew-summary-buffer-begin t)))))
    (set-buffer obuf)))

(defun mew-summary-im-sentinel (process event)
  (save-excursion
    (set-buffer (process-buffer process)) ;; necessary
    (if (and mew-summary-buffer-string
	     (string-match "ERROR: invalid password (\\([^\)]+\\))"
			   mew-summary-buffer-string))
	(let ((src (mew-match 1 mew-summary-buffer-string)))
	  (mew-passwd-set-passwd src nil)
	  (if (equal mew-summary-buffer-config mew-config-default)
	      (message "Password is wrong!")
	    (message "Password is wrong (%s)!" mew-summary-buffer-config))))
    (setq mew-summary-buffer-process nil)
    (setq mew-summary-buffer-string nil)
    (setq mew-summary-buffer-config nil)
    (setq mew-summary-buffer-begin nil)))

;;
;;
;; Current status
;;

(defmacro mew-frame-id (&optional frame)
  (` (if window-system
	 (let ((fram (or (, frame) (selected-frame))))
	   (cdr (assq 'window-id (frame-parameters fram))))
       "no-window")))

(defun mew-current-get (key)
  (let ((frame-sym (intern (format "mew-frame-%s" (mew-frame-id)))))
    (if (symbol-plist frame-sym)
	(get frame-sym key))))

(defun mew-current-set (key value)
  (let ((frame-sym (intern (format "mew-frame-%s" (mew-frame-id)))))
    (if (null (symbol-plist frame-sym))
	(setplist frame-sym
		  (list
		   'message nil 
		   'cache   nil
		   'part    nil
		   'window  nil
		   )))
    (put frame-sym key value)))

(defun mew-current-clean-up ()
  (let ((frams (frame-list)))
    (while frams
      (setplist (intern (format "mew-frame-%s" (mew-frame-id (car frams))))
		(list
		 'message nil 
		 'cache   nil
		 'part    nil
		 'window  nil
		 ))
      (setq frams (cdr frams)))))

;;
;; Address
;;

(defmacro mew-get-my-address ()
  '(or (mew-header-parse-address mew-from:) mew-mail-address))

(defmacro mew-get-my-address-regex-list ()
  '(cons (concat "^" (regexp-quote (user-login-name)) "$")
	 (cons (concat "^" mew-mail-address "$")
	       mew-mail-address-list)))
  
(defmacro mew-is-my-address (addrs from)
  (` (and (, from)
	  (let ((case-fold-search t))
	    (catch (quote match)
	      (car (mapcar (function (lambda (arg)
				       (and (string-match arg (, from))
					    (throw (quote match) t))))
			   (, addrs))))))))

;;
;; Lisp load/save
;;

(defun mew-lisp-load (filename)
  "Load lisp from FILENAME"
  (let ((lisp nil)
	(fullname (expand-file-name filename mew-mail-path)))
    (save-excursion
      (if (not (file-readable-p fullname))
	  ()
	(mew-set-buffer-tmp)
	(insert-file-contents fullname)
	(setq lisp 
	      (condition-case nil
		  (read (current-buffer))
		(error ())))
	lisp))))

(defun mew-lisp-save (filename lisp)
  "Save LISP to FILENAME. LISP is truncated to mew-lisp-max-length
by side-effect."
  (save-excursion
    (let* ((fullname (expand-file-name filename mew-mail-path))
	   (tmp-buf  (create-file-buffer fullname)))
      (set-buffer tmp-buf)
      (mew-erase-buffer)
      (if (> (length lisp) mew-lisp-max-length)
	  (setcdr (nthcdr (1- mew-lisp-max-length) lisp) nil))
      (prin1 lisp tmp-buf)
      (princ "\n" tmp-buf)
      (write-region (point-min) (point-max) fullname nil 'no-msg)
      (kill-buffer tmp-buf))))

(provide 'mew-func)

;;; Copyright Notice:

;; Copyright (C) 1997, 1998, 1999 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-func.el ends here
