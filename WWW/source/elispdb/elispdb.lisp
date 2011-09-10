(in-package :elispdb)

(defparameter *verbose* nil "Babble about our progress.")

(defparameter *debug* nil)

(defconstant *archive-entry-keywords*
  '("filename" "package" "version" "keywords" "author"
    "maintainer" "created" "description"
    "url" "compatibility" "incompatibility" "last-updated"))

(defvar *word-boundary-re* (regcompile "\\W"))

(defvar *whitespace-re* (regcompile "\\s"))

(defvar *valid-word-re* (regcompile "^(\\w|-)+$"))

(declaim (inline log-info))
(defun log-info (msg &rest args)
  (when *verbose*
    (apply #'format t msg args)))

(declaim (inline log-debug))
(defun log-debug (msg &rest args)
  (when *debug*
    (apply #'format t msg args)))

(declaim (inline log-error))
(defun log-error (msg &rest args)
  (apply #'format *error-output* msg args)
  (finish-output *error-output*))

(defstruct archive-entry 
  filename
  package
  (auto-generated t :type (member nil t))
  (entries (make-hash-table :test #'equal) :type hash-table))

(defun archive-entry-keyword-p (str)
  (member str *archive-entry-keywords* :test #'string-equal))

(defun write-archive-entry (entry &optional (stream *standard-output*))
  (let ((*print-readably* t))
    (format stream "~S ~S ~S~%"
	    (archive-entry-filename entry)
	    (archive-entry-package entry)
	    (archive-entry-auto-generated entry))
    (maphash #'(lambda (k v)
		 (print k stream)
		 (print v stream))
	     (archive-entry-entries entry))))

(defun read-archive-entry (stream)
  (let* ((fname (read stream))
	 (pkg (read stream))
	 (auto-generated (read stream))
	 (e (make-archive-entry :filename fname
				:package pkg
				:auto-generated auto-generated))
	 (smokeit (archive-entry-entries e)))
    (do ((k (read stream nil) (read stream nil)))
	((null k) smokeit)
      (setf (gethash k smokeit) (read stream nil)))
    e))

(defun canonicalize-archive-entry-keyword (key)
  (let ((v (find-if #'(lambda (l) (member key l :test #'string-equal))
		    '(("author" "authors") ("maintainer" "maintainers")
		      ("url" "x-url")))))
    (if v
	(car v)
      key)))

(defun multi-line-entry-p (str)
  (member str '("maintainer" "author") :test #'equal))

(defun add-word-entry (db filename word score)
  (let ((s (db-get db word)))
    (when (null s)
      (setf s ""))
    (let ((*print-readably* t))
      (db-put db word (concatenate 'string s
				   (prin1-to-string filename)
				   (prin1-to-string score))))))

(defun get-word-hash (db word)
  (deserialize-hash (db-get db word)))

(defun add-elae-entry (db filename elae)
  (db-put db filename
	  (with-output-to-string (stream)
	    (write-archive-entry elae stream))))

(defun get-elae-entry (db filename)
  (let ((e (db-get db filename)))
    (unless (eql (length e) 0)
      (with-input-from-string (stream e)
	(read-archive-entry stream)))))

(defun (setf get-elae-entry) (value db filename)
  (db-put db filename (with-output-to-string (stream)
			(write-archive-entry value stream))))

(defun serialize-hash (hash)
  (declare (optimize (speed 3)))
  (with-output-to-string (stream)
    (let ((*print-readably* t))
      (maphash #'(lambda (k v)
		   (print k stream)
		   (print v stream))
	       hash))))

(defun deserialize-hash (str)
  (declare (optimize (speed 3)))
  (with-input-from-string (stream str)
    (let ((hash (make-hash-table :test #'equal)))
      (do ((k (read stream nil) (read stream nil)))
	  ((null k) hash)
	(setf (gethash k hash) (read stream nil))))))



