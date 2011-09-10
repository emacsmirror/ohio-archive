(in-package :elispdb)

(defun file-name-nondirectory (filename)
  (declare (type (or simple-base-string pathname) filename)
	   (optimize (speed 3)))
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (let ((start 0))
    (declare (fixnum start))
    (loop
     (let ((p (position #\/ filename :start start)))
       (if (null p)
	   (if (>= start (length filename))
	       (error "Invalid file name: ~A" filename)
	     (return (subseq filename start)))
	 (setf start (1+ p)))))))

(defmacro with-current-directory (dir &body body)
  (let ((cur (gensym)))
    `(let ((,cur (namestring (ext:default-directory))))
       (unwind-protect
	   (progn (unix:unix-chdir ,dir)
		  ,@body)
	 (unix:unix-chdir ,cur)))))

(defun in-directory (dir file)
  (declare (simple-base-string dir file))
  (when (eql (length dir) 0)
    (setf dir "/"))
  (if (eql (char dir (1- (length dir))) #\/)
      (concatenate 'string dir file)
    (concatenate 'string dir "/" file)))

(defmacro do-directory-entries ((name dir &optional result &key all full-names)
				&body body)
 (let ((dirname (gensym))
       (dirstream (gensym)))
   `(let ((,dirname ,dir))
      (when (pathnamep ,dirname)
	(setf ,dirname (namestring ,dirname)))
      (let ((,dirstream (unix:open-dir ,dirname)))
	(when (null ,dirstream)
	  (error 'file-error "Unable to open directory ~A" ,dirname))
	(unwind-protect
	    (with-current-directory ,dirname
	      (do ((,name (unix:read-dir ,dirstream) (unix:read-dir ,dirstream)))
		  ((null ,name) ,@(if result `(,result)))
		,(let ((inner-bodyform
			`(progn
			   ,@(when full-names
			       `((setf ,name (concatenate 'string
							  (namestring (ext:default-directory))
							  ,name))))
			   ,@body)))
		   (if all
		       inner-bodyform
		     `(when (not (or (string-equal ,name ".")
				     (string-equal ,name "..")))
			,inner-bodyform)))))
	  (unix:close-dir ,dirstream))))))

(defun map-directory (fun directory)
  (let ((result nil))
    (do-directory-entries (entry directory result :full-names t)
      (case (unix:unix-file-kind entry)
	(:directory
	 (map-directory fun entry))
	(:file
	 (push (funcall fun entry) result))
	(t
	 nil)))))

(defun fill-temporary-name (str start len)
  (declare (simple-base-string str)
	   (fixnum start len)
	   (optimize (speed 3)))
  (dotimes (j len)
    (setf (char str (+ j start))
	  (svref #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
		   #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
		   #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
		   #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d
		   #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n
		   #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x
		   #\y #\z) (random 62)))))

(defun make-temporary-file (dir &key (prefix "temp-") (mode #o600))
  (declare (simple-base-string dir prefix)
	   (type (unsigned-byte 32) mode))
  (let ((prefix-len (+ (length dir) (length prefix)))
	(filename (in-directory dir (concatenate 'string prefix "          "))))
    (dotimes (i 15)
      (fill-temporary-name filename prefix-len 10)
      (multiple-value-bind (fd errno)
	  (unix:unix-open filename (logior unix:o_rdwr unix:o_creat unix:o_excl)
			  mode)
	(cond ((not (null fd))
	       (unix:unix-close fd)
	       (return-from make-temporary-file filename))
	      ((not (= errno unix:eexist))
	       (error "Failed to create temporary file ~S: ~A"
		      filename (unix:get-unix-error-msg errno))))))
    (error "Could not create temporary file in ~A" dir)))

(defmacro with-open-temporary-file ((binding dir &rest keys) &body body)
  `(let ((,binding (make-temporary-file ,dir ,@keys)))
     (unwind-protect
	 (progn ,@body)
       (progn
	 (close ,binding)
	 (unix:unix-unlink (lisp::fd-stream-file ,binding))))))

(defun make-temporary-directory (dir &key (prefix "temp-") (mode #o700))
  (declare (simple-base-string dir prefix)
	   (type (unsigned-byte 32) mode))  
  (let ((prefix-len (+ (length dir) (length prefix)))
	(dirname (concatenate 'string dir prefix "          ")))
    (dotimes (i 15)
      (fill-temporary-name dirname prefix-len 10)
      (multiple-value-bind (created errno)
	  (unix:unix-mkdir dirname mode)
	(cond ((not (null created))
	       (return-from make-temporary-directory dirname))
	      ((not (= errno unix:eexist))
	       (error "Failed to create temporary directory ~A: ~A"
		      dirname (unix:get-unix-error-msg errno))))))
    (error "Could not create temporary directory in ~A" dir)))

(defun rm-rf (dir)
  (declare (simple-base-string dir))  
  (do-directory-entries (file dir)
    (let ((type (unix:unix-file-kind file)))
      (cond ((eq type :directory)
	     (rm-rf file))
	    ((null type))
	    (t
	     (unix:unix-unlink file)))))
  (unix:unix-rmdir dir))

(defmacro with-temporary-directory ((binding tmpdir name &key force-cleanup) &body body)
  `(let ((,binding (make-temporary-directory ,tmpdir :prefix ,name)))
     (unwind-protect
	 (with-current-directory ,binding
	   (progn
	     ,@body))
       ,(if force-cleanup
	    `(rm-rf ,binding)
	  `(unix:unix-rmdir ,binding)))))


(declaim (inline process-success-or-lose))
(defun process-success-or-lose (name proc)
  (unless (zerop (process-exit-code proc))
    (error "Process ~A (pid ~A) failed with exit code ~A~%"
	   name
	   (process-pid proc)
	   (process-exit-code proc))))

(defun extract-tar-file (filename &key (dir "/tmp/") (prefix "tmp"))
  (with-temporary-directory (tmpdir dir prefix)
    (let* ((gzipproc (run-program "gzip" `("-dc" ,filename)
				  :error *error-output*
				  :output :stream :wait nil))
	   (tarproc (run-program "tar" `("-x" "-C" ,tmpdir)
				 :input (process-output gzipproc)
				 :output :stream
				 :error *error-output*
				 :wait t)))
      (ext:process-close gzipproc)
      (ext:process-close tarproc)
      (process-success-or-lose "tar" tarproc)
      (namestring (ext:default-directory)))))

(defun extract-zip-file (filename &key (dir "/tmp/") (prefix "tmp"))
  (with-temporary-directory (tmpdir dir prefix)
    (let* ((unzipproc (run-program "unzip" `(,filename "-d" ,tmpdir)
				   :error *error-output*)))
      (ext:process-close unzipproc)
      (process-success-or-lose "unzip" unzipproc)      
      (namestring (ext:default-directory)))))