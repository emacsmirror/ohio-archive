(in-package :elispdb)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *archive-root* "/usr/src/emacs-lisp/")
  (defparameter *incoming-words-database-filename*
    (concatenate 'string *archive-root* "incoming-words.db"))
  (defparameter *incoming-elae-database-filename*
    (concatenate 'string *archive-root* "incoming-elae.db"))
  (defparameter *archive-words-database-filename*
    (concatenate 'string *archive-root* "archive-words.db"))
  (defparameter *archive-elae-database-filename*
    (concatenate 'string *archive-root* "archive-elae.db")))

(defparameter *maximum-search-results* 15)

(defvar *databases-open* nil)

(defvar *incoming-words-database* nil)
(defvar *incoming-elae-database* nil)
(defvar *archive-words-database* nil)
(defvar *archive-elae-database* nil)

(defun open-databases ()
  (setq *incoming-words-database* (brkdb:db-open *incoming-words-database-filename*)
	*incoming-elae-database* (brkdb:db-open *incoming-elae-database-filename*)
	*archive-words-database* (brkdb:db-open *archive-words-database-filename*)
	*archive-elae-database* (brkdb:db-open *archive-elae-database-filename*)))

(defun close-databases ()
  (macrolet ((close-and-nil! (db) `(progn (brkdb:db-close ,db) (setq ,db nil))))
    (close-and-nil! *incoming-words-database*)
    (close-and-nil! *incoming-elae-database*)
    (close-and-nil! *archive-words-database*)  
    (close-and-nil! *archive-elae-database*)))

(declaim (inline format-html-escaped))
(defun format-html-escaped (str format &rest args)
  (princ (araneida:html-escape (apply #'format nil format args)) str))

(declaim (inline princ-to-string-html-escaped))
(defun princ-to-string-html-escaped (obj)
  (araneida:html-escape (princ-to-string obj)))

(defun format-args-html-escaped (str format &rest args)
  (princ (apply #'format nil format
		(mapcar #'princ-to-string-html-escaped args)) str))

(defun map-hash-sorted (mapfun sortfun hash &key (start 0) (range -1))
  (declare (function mapfun sortfun)
	   (hash-table hash)
	   (integer start range)
	   (optimize (speed 3)))
  (let* ((i -1)
	 (count (hash-table-count hash))
	 (vec (make-array count :adjustable nil :initial-element nil)))
    (declare (integer i)
	     (simple-vector vec))
    (maphash #'(lambda (k v)
		 (setf (svref vec (incf i)) (cons k v))) hash)
    (sort vec sortfun)
    (if (eql range -1)
	(setf range count)
      (setf range (min count (+ start range))))
    (when (> range 0)
      (do ((i start (incf i)))
	  ((>= i range) nil)
	(declare (integer i))
	(let ((e (svref vec i)))
	  (declare (cons e))
	  (funcall mapfun (car e) (cdr e)))))))

(defun merge-hashes (hashes)
  (reduce #'(lambda (result h)
	      (maphash #'(lambda (k v)
			   (let ((ovalue (gethash k result 0)))
			     (setf (gethash k result) (+ ovalue v))))
		       h)
	      result) hashes))

(defun get-score-hashes (db search)
  (mapcar #'(lambda (x) (let ((h (elispdb:get-word-hash db x)))
			  (log-debug "word ~A got ~A matches~%" x (hash-table-count h))
			  h))
;	  (remove-if-not #'(lambda (x) (regexec *valid-word-re* x))
			 search)) ;)

(defun get-score-hash (db search)
  (merge-hashes (get-score-hashes db search)))

(defun write-search-results (search archive start stream)
  (declare (type list search)
	   (type (member :incoming :archive) archive)
	   (type integer start)
	   (stream stream)
	   (optimize (speed 2)))
  (let* ((db (or
	      (ecase archive
		(:incoming
		 *incoming-words-database*)
		(:archive
		 *archive-words-database*))
	      (error "Database ~A not open." archive)))
	 (elaedb (or
		  (ecase archive
		    (:incoming
		     *incoming-elae-database*)
		    (:archive
		     *archive-elae-database*))
		  (error "Database ~A not open." archive)))
	 (matches (get-score-hash db search)))
    (flet ((write-search-result (name score link db)
		(flet ((write-elae-entry (elae)
			 (when (elispdb:archive-entry-auto-generated elae)
			   (princ "<FONT SIZE=-1 COLOR=#F00000>(automatically generated ELAE)</FONT>" stream))
			 (princ "<BR>" stream)
			 (flet ((write-elae-entry-entry (k v)
					(when (consp v)
					  (setf v (elispdb::join ", " v)))
					(format-args-html-escaped stream "<I>~A</I>: ~A<BR>
"
								  k v)))
			   (maphash #'write-elae-entry-entry
				    (elispdb:archive-entry-entries elae)))))
		  (let ((elae (elispdb:get-elae-entry db name)))
		    (format stream "<P><A HREF=\"~A/~A\">~A</A> (score ~A)<BR>
" link name name score)
		    (if elae
			(write-elae-entry elae)
		      (princ "(No ELAE available)<BR>" stream))
		    (princ "</P>" stream)))))
      (flet ((write-results (hash link db)
		    (map-hash-sorted
		     #'(lambda (k v)
			 (write-search-result k v link elaedb))
		     #'(lambda (a b)
			 (> (cdr a) (cdr b)))
		     hash :start start :range *maximum-search-results*)
		    nil))
	(let ((match-count (hash-table-count matches))
	      (end (+ start *maximum-search-results*)))
	  (multiple-value-bind (link name)
	      (ecase archive
		(:incoming
		 (values "http://www.cis.ohio-state.edu/archive/incoming/"
			 "Incoming"))
		(:archive
		 (values "http://www.cis.ohio-state.edu/archive/archive/"
			 "Main")))
	    (log-debug "Count: ~A ~%" match-count)
	    (cond ((zerop match-count)
		   (format stream "<P>Your search produced no matches. (Try <A HREF=\"~A\">browsing directly</A>)</P>" link)
		   (princ "Please note that the search engine doesn't perform any word stemming.  For example, to find packages dealing with buffers, you would want to search for both \"buffer\" and \"buffers\"." stream))
		  (t
		   (format-args-html-escaped stream "<P><H2>Matches ~A-~A in the <A HREF=\"~A\">~A archive</A>:</H2></P>" start (min end match-count) link name)))
	    (write-results matches link db)))))))

(defun search-handler (request rest-of-url)
  (handler-case
   (let ((alist (araneida:url-query-alist (araneida:request-url request)))
	 (stream (araneida:request-stream request)))
    (flet ((param-or-lose (param) (let ((v (assoc param alist :test #'string-equal)))
				    (if v
					(cadr v)
				      (error "Required parameter ~A not given." param)))))
      (let ((archive (intern (nstring-upcase (param-or-lose "archive")) :keyword))
	    (search (param-or-lose "q"))
	    (start (parse-integer (or (cdr (assoc "start" alist :test #'string-equal))
				      "0"))))
	(let ((file (search-output-to-tmpfile search archive start)))
	  (araneida:request-send-headers request)
	  (with-open-file (tmpstream file :direction :input :element-type 'character)
			  (araneida::copy-stream tmpstream stream))))))
   (error (e)
	  (araneida:request-send-error request 500 "Error: ~A" e)
	  (format *error-output* "Error: ~A" e)
	  (return-from search-handler nil))))

(defun canonicalize-search (search)
  (split-re *word-boundary-re* search))

(defun search-output-to-tmpfile (search archive start)
  (log-debug "Request: ~A Archive: ~A start: ~A~%" search archive start)
  (let ((file (make-temporary-file "/var/elispdb/" :prefix "elispdb-"))
	(searches (canonicalize-search search)))
    (with-open-file (stream file :direction :output :if-exists :append)
      (princ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">
<HTML><HEAD><TITLE>Emacs Lisp Archive Search</TITLE></HEAD>
<BODY bgcolor = \"#FFFFFF\" text = \"#000000\" link = \"#0000FF\" vlink = \"#0000C8\">
<H1>The Ohio State Emacs Lisp Archive</H1><HR>" stream)
      (format stream "<A HREF=\"http://www.cis.ohio-state.edu/emacs-lisp/\">Back to the main page</a>.<BR><P><B>Search Request:</B> ~A</P>"
	      (princ-to-string-html-escaped search))
      (princ (concatenate 'string "<HR>Performance Information:<BR>" 
			  (araneida:html-escape
			   (with-output-to-string (*trace-output*)
			     (time
			      (write-search-results searches archive start stream))))) stream)
      (format stream "<HR>Last database update at: ~A<BR>" (princ-to-string-html-escaped (araneida::universal-time-to-rfc-date (file-write-date *archive-words-database-filename*))))
      (princ "<HR>You can view the <A HREF=\"http://www.cis.ohio-state.edu/archive/WWW/source/\">source code</A> to this search engine." stream)
      (princ "</BODY></HTML>" stream))
    file))
      
    
    
    