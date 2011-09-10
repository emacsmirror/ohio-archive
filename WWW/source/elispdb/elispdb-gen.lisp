(in-package :elispdb)

(use-package "EXT")
(use-package "PCRE")
(use-package "BRKDB")
(use-package "ELISPDB")

(declaim (optimize (speed 2) (compilation-speed 0)))

; (declaim (optimize (speed 3)))


(defvar *header-regexp* (regcompile "^\\s{0,3};+\\s*((?:\\w|[-.])+)\\s*:\\s*(.+?)\\s*$")
  "A regexp to match stuff in the form of the GNU Elisp coding standard.")

(defvar *cont-header-regexp* (regcompile "^\\s*;+\\s{5,10}(.+?)\\s*$")
  "A regexp to match multiple line stuff.")

(defvar *bof-regexp* (regcompile "^\\s*;+\\s*((?:\\w|[-.])+)\\s{0,2}-{1,5}\s{0,2}(.+)$")
  "A regexp to match beginning of file declarations.")

(defvar *file-prefix-re* (regcompile "^((?:\\w|(?:-|\\w))+)"))

(defvar *word-boundary-re* (regcompile "\\W"))

(defvar *valid-word-re* (regcompile "^(\\w|-)+$"))

(defvar *tar-file-re* (regcompile "\\.tar\\.gz$"))

(defvar *zip-file-re* (regcompile "\\.zip$"))

(defvar *el-file-re* (regcompile "\\.el$"))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     (progn ,@body)))

(declaim (inline inchash))
(defun inchash (key val hash)
  (declare (type simple-base-string key) (integer val))
  (setf (gethash key hash) (+ (the integer (gethash key hash 0)) val))
  nil)

(defun join (sep list)
  (declare (string sep)
	   (cons list))
  (let ((result "")
	(cur list))
    (while (cdr cur)
      (setq result (concatenate 'string
				result
				(car cur)
				sep))
      (setf cur (cdr cur)))
    (setq result (concatenate 'string result (car cur)))
    result))

(declaim (inline nmap))
(defun nmap (fun list)
  (map-into list fun list))

(defun file-prefix (filename)
  (regexec-with-bindings (*file-prefix-re* (file-name-nondirectory filename)) (whole prefix)
    (if whole
	prefix
      filename)))

(defun score-string (hash string score)
  (declare (type hash-table hash)
	   (type simple-base-string string)
	   (type integer score)
	   (optimize (speed 3) (safety 0)))
  (let ((count 0))
    (declare (fixnum count))
    (dolist (word (split-re *word-boundary-re* string))
      (declare (type simple-base-string word))
      (let ((len (length word)))
	(when (and (> len 2)
		   (< len 12)
		   (regexec *valid-word-re* word))
	  (incf count)
	  (inchash (nstring-downcase word) score hash))))
    (dolist (substr (split-re *whitespace-re* string))
      (declare (type simple-base-string substr))
      (when (and (> (length substr) 1)
		 (some #'alphanumericp substr))
	(incf count)
	(inchash (nstring-downcase substr) score hash)))
    count))
  
(defun generate-elae-entry-1 (filename &optional package)
  (let ((elae (make-archive-entry :filename (file-name-nondirectory filename)
				  :package package))
	oline)
    (with-open-file (stream filename :direction :input)
      (dotimes (i 10)
	(let ((line (read-line stream nil)))
	  (when line
	    (regexec-with-bindings (*bof-regexp* line) (whole name description)
	      (when whole
		(setf (gethash "description" (archive-entry-entries elae))
		      (string-trim " " description))
		(return-from nil)))))))
    (with-open-file (stream filename :direction :input)
      (let (line)
	(let ((entryhash (archive-entry-entries elae)))
	  (while (setf line (or oline (read-line stream nil)))
	    (setf oline nil)
	    (when (search "emacs lisp archive entry" (string-downcase line))
	      (setf (archive-entry-auto-generated elae) nil))
	    (regexec-with-bindings (*header-regexp* line) (whole key value)
	      (when (and whole (archive-entry-keyword-p
				(canonicalize-archive-entry-keyword key)))
		(setf key (nstring-downcase key))
		(setf (gethash key entryhash)
		      (if (multi-line-entry-p key)
			  (let ((cur (list value)))
			    (block read-more
			      (do ()
				  (nil)
				(setf oline (read-line stream nil))
				(regexec-with-bindings
				  (*cont-header-regexp* oline) (whole value)
				  (if whole
				      (push value cur)
				    (return-from read-more cur))))))
			value))))))))
    elae))

(defun maybe-generate-elae-entry (filename tmpdir)
  (unwind-protect
      (let* ((prefix (file-prefix filename))
	     (mainfile (block try
			 (progn
			   (map-directory
			    #'(lambda (file)
				(declare (type simple-base-string file))
				(when (and (regexec *el-file-re* file)
					   (string-equal (file-prefix file)
							 prefix))
				  (return-from try file)))
			    tmpdir)
			   nil))))
	(when mainfile
	  (return-from maybe-generate-elae-entry
	    (generate-elae-entry-1 mainfile prefix)))
	(let ((entries (make-array 0 :fill-pointer t)))
	  (map-directory
	   #'(lambda (file)
	       (when (regexec *el-file-re* file)
		 (vector-push-extend (generate-elae-entry-1 file prefix) entries)))
	   tmpdir)
	  (sort entries #'(lambda (a b)
			    (declare (archive-entry a b)
				     (optimize (speed 3) (debug 0)))
			    (> (hash-table-count (archive-entry-entries a))
			       (hash-table-count (archive-entry-entries b)))))
	  (if (> (fill-pointer entries) 0)
	      (aref entries 0)
	    nil)))
    (rm-rf tmpdir)))

(defun generate-elae-entry (filename)
  (cond ((regexec *tar-file-re* filename)
	 (let ((tmpdir (extract-tar-file filename)))
	   (maybe-generate-elae-entry filename tmpdir)))
	((regexec *zip-file-re* filename)
	 (let ((tmpdir (extract-zip-file filename)))
	   (maybe-generate-elae-entry filename tmpdir)))
	((regexec *el-file-re* filename)
	 (generate-elae-entry-1 filename))
	(t
	 nil)))

(macrolet ((with-character-iterator ((stream &optional (predicate '(null c))) &rest body)
	     (let ((strm (gensym)))
	       `(let ((,strm ,stream))
		  (block character-iterator
		    (do ((c (read-char ,strm nil) (read-char ,strm nil)))
			(,predicate)
		      (progn
			,@body)))))))
(defun score-comments (wordhash stream initial-score)
  (declare (hash-table wordhash) (stream stream) (integer initial-score))
  (let ((count 0))
    (declare (integer count))    
    (with-character-iterator (stream)
      (when (eql c #\;)
	(incf count (score-string wordhash (read-line stream nil) (if (> initial-score 1)
								      (decf initial-score)
								    initial-score)))))
    count))

(defun score-docstrings (wordhash stream)
  (declare (hash-table wordhash) (stream stream))
  (let ((count 0))
    (declare (integer count))
    (with-character-iterator (stream)
      (when (eql c #\")
	(incf count
	      (score-string wordhash
			    (with-output-to-string (str)
			      (with-character-iterator (stream (or (eql c nil) (eql c #\")))
				(princ c str)
				(when (and (eql c #\\)
					   (eql (peek-char nil stream nil) #\"))
				  (princ (read-char stream nil) str))))
			    1))))
    count))
)

(defun add-word-scores (wordhash filename)
  (declare (type hash-table wordhash)
	   (type simple-base-string filename))
  (let ((size 0)
	(count 0))
    (declare (integer size count))
    (log-info "processing file ~A..." filename)
    (with-open-file (stream filename :direction :input)
      (incf count (score-comments wordhash stream 20))
      (setf size (with-open-file (stream filename)
		   (file-length stream))))
    (with-open-file (stream filename :direction :input)
      (incf count (score-docstrings wordhash stream)))
    (log-info "done. (~A word entries)~%" count)
    size))

(defun generate-word-scores (filename)
  (declare (type simple-base-string filename))
  (let ((total-size 0)
	(wordhash (make-hash-table :test #'equal)))
    (declare (integer total-size)
	     (hash-table wordhash))
    (flet ((add-word-scores-recursive (directory)
	      (map-directory #'(lambda (file)
				 (when (regexec *el-file-re* file)
				   (incf total-size (add-word-scores wordhash file)))
				 nil)
			     directory)))
    (cond ((regexec *tar-file-re* filename)
	   (let ((tmpdir (extract-tar-file filename :prefix "elispdb-gen")))
	     (add-word-scores-recursive tmpdir)))
	  ((regexec *zip-file-re* filename)
	   (let ((tmpdir (extract-zip-file filename :prefix "elispdb-gen")))
	     (add-word-scores-recursive tmpdir)))
	  ((regexec *el-file-re* filename)
	   (incf total-size (add-word-scores wordhash filename))))
    (inchash (file-prefix filename) 100 wordhash)
    (let ((factor (floor (/ total-size 16384))))
      (when (zerop factor)
	(setf factor 1))
      (maphash #'(lambda (k v)
		   (setf (gethash k wordhash) (ceiling (/ v factor))))
	       wordhash))
    wordhash)))
  
(defun make-word-database (directory dbfilename)
  (with-open-database (db dbfilename)
    (log-info "Generating word database ~A for directory ~A...~%" dbfilename directory)
    (map-directory
     #'(lambda (filename)
	 (handler-case 
	  (let ((hash (generate-word-scores filename)))
	    (maphash #'(lambda (k v)
			 (declare (type simple-base-string k)
				  (integer v)
				  (optimize (speed 3)))
			 (when (> v 1)
			   (add-word-entry db (file-name-nondirectory filename) k v)))
		     hash))
	  (error (e) (log-error "Processing of ~A failed: ~A~%" filename e))))
     directory)
    (log-info "Generation of ~A finished.~%" dbfilename)))

(defun make-elae-database (directory dbfilename)
  (with-open-database (db dbfilename)
    (log-info "Generating ELAE database ~A for directory ~A...~%" dbfilename directory)
    (map-directory
     #'(lambda (filename)
	 (handler-case
	  (let ((entry (generate-elae-entry filename)))
	    (when entry
	      (setf (get-elae-entry db (file-name-nondirectory filename)) entry)))
	  (error (e) (log-error "Processing of ~A failed: ~A~%" filename e))))
     directory)
    (log-info "Generation of ~A finished.~%" dbfilename)))

(defun make-databases ()
  (flet ((unprobe-file (file) (when (probe-file file) (delete-file file)) file))
    (make-word-database "/usr/src/emacs-lisp/incoming/"
			(unprobe-file "/usr/src/emacs-lisp/incoming-words.db"))
    (make-word-database "/usr/src/emacs-lisp/archive/"
			(unprobe-file "/usr/src/emacs-lisp/archive-words.db"))
    (make-elae-database "/usr/src/emacs-lisp/incoming/"
			(unprobe-file "/usr/src/emacs-lisp/incoming-elae.db"))
    (make-elae-database "/usr/src/emacs-lisp/archive/"
			(unprobe-file "/usr/src/emacs-lisp/archive-elae.db"))))
