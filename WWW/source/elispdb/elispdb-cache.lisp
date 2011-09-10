(in-package :elispdb)

(defvar *cache-directory* "/var/elispdb/")

(defstruct cache-entry
  (creation-time (get-universal-time) :type integer)
  (hits 0 :type integer)
  (search "" :type simple-base-string)
  (file "" :type simple-base-string))

(defvar *search-cache* (make-hash-table :test #'equal))

(defconstant +maximum-cache-size+ 60)

(defun maybe-reduce-cache ()
  (when (> (hash-table-count *search-cache*) +maximum-cache-size+)
    (let ((i 0))
      (map-hash-sorted #'(lambda (k v)
			   (when (> (incf i) (/ +maximum-cache-size+ 2))
			     (return-from maybe-cleanup-cache nil))
			   (delete-file (cache-entry-file v))
			   (remhash k *search-cache*))
		       #'(lambda (a b)
			   (< (cache-entry-hits (car a))
			      (cache-entry-hits (car b))))
		       *search-cache*))))

(defun make-broadcast-cache-stream (stream)
  (let ((filename (sys::pick-temporary-file-name
		   (concatenate 'string *cache-directory* "~D~C")))
	(cachestream (open filename :direction :output :if-exists :supersede)))
    (make-broadcast-stream cachestream stream)))

(declaim (inline search-cached-p))
(defun search-cached-p (search)
  (when (gethash search *search-cache* nil)
    t))

(defun 

(defmacro with-search-cached ((searchstr stream) &body body)
  (let ((search (gensym))
	(broadcast (gensym)))
    `(let ((,search ,searchstr))
       (maybe-reduce-cache)
       (when (not (gethash ,search *search-cache* nil))
	 (let ((,stream (make-broadcast-cache-stream ,stream)))
	   (handler-case
	    (unwind-protect
		(progn
		  ,@body
		  (setf (gethash search *search-cache*)
			(make-cache-entry :file ,filename
					:search ,search)))
	      (prog
	       (map nil #'close (broadcast-stream-streams ,stream))
	       (close ,stream)))
	    (error (e)
		   (progn (close ,cachestream)
			  (close ,broadcast)
			  (delete-file ,filename)))))))))
	  
	 
							    
    
    
    



