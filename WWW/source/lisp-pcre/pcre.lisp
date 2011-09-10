;;; Partial CMUCL bindings for Philip Hazel's PCRE library.  These
;;; tested with PCRE 2.05 and the CMUCL 2.4.9 Debian package

;;; (c) 1999 Daniel Barlow <dan@telent.net>, may be freely used and
;;; redistributed.  I'd even make it public domain, except that
;;; apparently public domain software doesn't let me SHOUT about how
;;; it's supplied WITHOUT ANY WARRANTY.  NO.  NOT EVEN A LITTLE ONE.
;;; And I want to be clear about that.  Not even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; If you want to be sure that you have the latest version of this,
;;; check http://www.telent.net/lisp/pcre.lisp

;;; How to use it: see the load-foreign invocation below, then do
;;; something similar.  It doesn't matter what file you load, as long
;;; as it's an object (".o") file intelligible to ld.  Note that
;;; foreign code doesn't get dumped if you dump an image, so you'll
;;; need to evaluate it again when you restart.  See the CMUCL manual
;;; chapter 8 for more information


(defpackage "PCRE"
  (:use "COMMON-LISP" "ALIEN" "EXT" "C-CALL")
  (:export regcompile regexec regexec-with-bindings with-regexps split-re))
(in-package :pcre)

; (unless (ignore-errors (extern-alien "pcre_malloc" (* t)))
;   ;; load the foreign stuff if not already loaded
;   (load-foreign "pgrep.o" :libraries (list "libpcre.a")))

(declaim (optimize (speed 2) (safety 1) (compilation-speed 0)))

(defvar *constants*
  '((:CASELESS       . #x0001)
    (:MULTILINE      . #x0002)
    (:DOTALL         . #x0004)
    (:EXTENDED       . #x0008)
    (:ANCHORED       . #x0010)
    (:DOLLAR-ENDONLY . #x0020)
    (:EXTRA          . #x0040)
    (:NOTBOL         . #x0080)
    (:NOTEOL         . #x0100)
    (:UNGREEDY       . #x0200)))

#| Exec-time and get-time error codes |#

(defvar *errors*
  '((-1 . :NOMATCH     )
    (-2 . :NULL        )
    (-3 . :BADOPTION   )
    (-4 . :BADMAGIC    )
    (-5 . :UNKNOWN_NODE)
    (-6 . :NOMEMORY    )
    (-7 . :NOSUBSTRING )))

#| Types |#

;;; note that the C-language bindings say "typedef void pcre" then
;;; use *pcre everywhere, whereas we say (effectively) "typedef void *pcre"
;;; and don't.  This way fits the cmucl alien convention better

(def-alien-type pcre (* t))
(def-alien-type pcre-extra (* t))

#| Store get and free functions. These can be set to alternative malloc/free
functions if required. 

;;; eventually we might make these allocate memory in such a way that
;;; the GC can collect it.  In the meantime, we're not exposing them

extern void *(*pcre_malloc)(size_t);
extern void  (*pcre_free)(void *);

|#

(defun add-up-options (b)
  (let ((result 0))
    (declare (type fixnum result))
    (dolist (e b result)
      (let ((val (assq e *constants*)))
	(when val
	  (setf result (the fixnum (logior result (the fixnum (cdr val))))))))))

#| Functions |#

;;;       pcre *pcre_compile(const char *pattern, int options,
;;;                          const char **errptr, int *erroffset,
;;;                          const unsigned char *tableptr);

(def-alien-routine ("pcre_compile" %compile) pcre
  (pattern c-string) (options int)
  (errptr system-area-pointer :out) (erroffset int :out) (tableptr c-string))

;;;       int pcre_info(const   pcre   *code,   int  *optptr,
;;;                     int *firstcharptr);

(def-alien-routine ("pcre_info" %info) int
  (code pcre) (options int :out) (firstchar int :out))

(defun regcompile (pattern &key tableptr options)
  "Compiles PATTERN into an internal form, using the OPTIONS provided.
TABLEPTR does nothing useful at present.  The returned PCRE must be
discarded with FREE-ALIEN when it is no longer needed.  "
  (multiple-value-bind (pcre errptr erroffset)
      (%compile pattern (add-up-options options) tableptr)
    (when (zerop (sys:sap-int (alien:alien-sap pcre)))
      (error "Failed to compile regexp ~A." pattern))
    pcre))
(export 'regcompile)

;;;       pcre_extra *pcre_study(const pcre *code, int options,
;;;                              const char **errptr);

(def-alien-routine ("pcre_study" %study) pcre-extra
  (code pcre) (options int) (errptr system-area-pointer :out))

;;; int pcre_exec(const pcre *code, const pcre_extra *extra,
;;;             const char *subject, int length, int startoffset,
;;;             int options, int *ovector, int ovecsize);

(def-alien-routine ("pcre_exec2" %exec) int
  (code pcre) (study-result pcre-extra) (subject c-string) (length int)
  (startoffset int) (options int) (ovector (* int)) (ovecsize int))

(defun regexec (pcre subject &key studied length options)
  "Attempt to match the string SUBJECT against the compiled expression
PCRE.  Returns a list of ( start . end ) pairs giving the positions of
what was matched - whole match first then bracketed subexpressions if any."
  (declare (simple-base-string subject)
	   (type (alien pcre) pcre))
  (let* ((osize (* 3 (1+ (%info pcre))))
         (length (or length (length subject)))
	 (ovec (make-alien int osize)))
    (unwind-protect
	(progn
	  (let ((num-matches (%exec pcre studied subject length
				    0 (add-up-options options)
				    ovec osize)))
	    (cond ((eql num-matches -1)
		   ;; XXX cast num-matches to something of type error 
		   nil)
		  ((< num-matches -1)
		   (error "PCRE_ERROR (~A) pcre: ~S subject: ~S osize: ~S ovec: ~S"
			  num-matches pcre subject osize ovec))
		  (t
		   (loop for i from 0 to (1- (* 2 num-matches)) by 2
			 collect (cons (deref ovec i) (deref ovec (1+ i))))
		   ))))
      (free-alien ovec))))
(export 'regexec)

#| (put 'regexec-with-bindings 'lisp-indent-function 2) |#
(defmacro regexec-with-bindings ((regexp string) (&rest names) &body body)
  (let ((str (gensym)))
    `(let ((,str ,string))
       (multiple-value-bind (,@names)
	   (values-list
	    (mapcar #'(lambda (m)
			(subseq ,str (car m) (cdr m)))
		    (regexec ,regexp ,str)))
	 (declare (ignorable ,@names))
	 (declare (type (or null simple-base-string) ,@names))
	 ,@body))))
(export 'regexec-with-bindings)

(defmacro with-regexps (regexps &body body)
  "REGEXPS is a list of assignments of the form (var (regcomp \"foo\")) ,
which are evaluated before evaluating BODY.  The variables involved
will be freed after (successful or otherwise) completion of BODY.
Returns the values returned by BODY"
  (let ((names (mapcar #'(lambda (e) (list (car e) 'nil)) regexps)))
    `(let ,names
       (unwind-protect
           (progn  ,@(mapcar (lambda (x) (cons 'setq x)) regexps)
                   ,@body)
         ,@(mapcar (lambda (x) `(if ,(car x) (free-alien ,(car x))))
                   (reverse names))))))
(export 'with-regexps)

(defun split-re (regexp string)
  (declare (string string))
  (labels
      ((split (string result)
	      (let ((match (regexec regexp string)))
		(declare (type simple-base-string string))		
		(if match
		    (split (subseq string (cdar match))
			   (cons (subseq string 0 (caar match))
				 result))
		  (cons string result)))))
    (nreverse (split string '()))))
(export 'split-re)

#|
Emacs users may want to do

 (put 'with-regexps 'lisp-indent-function 'defun)

(in elisp, not in this buffer...) to get sane indentation of constructs
like the one following

(with-regexps ((a (regcompile "foo"))
	       (b (regcompile "bar")))
  (cond ((regexec a "foobar")
	 "Matched the first")
	((regexec b "bar food")
	 "First failed, second matched")
	(t "Incompentent fool")))
|#

