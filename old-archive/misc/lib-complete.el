; Path: dg-rtp!rock.concert.net!mcnc!stanford.edu!agate!spool.mu.edu!olivea!uunet!comp.vuw.ac.nz!waikato.ac.nz!aukuni.ac.nz!mike-w
; From: mike-w@cs.aukuni.ac.nz (Mike Williams)
; Newsgroups: gnu.emacs.sources
; Subject: UPDATE: lib-complete.el
; Date: 20 Jun 91 16:41:53 GMT
; Organization: University of Auckland, New Zealand.
; Nntp-Posting-Host: cs17.cs.aukuni.ac.nz
; X-News-Software: GNUS 3.13
; 
; 
;   Here's an update of the recently posted lib-complete.el, with improved
;   caching, several bug-fixes, and a generally better outlook on life :-)
;   It incorporates several ideas from Hallvard Furuseth's load-library.el.
; 
;; ========================================================================
;; lib-complete.el --  Completion on a search path
;; Author          : Mike Williams <mike-w@cs.aukuni.ac.nz>
;; Created On      : Sat Apr 20 17:47:21 1991
;; Last Modified By: Mike Williams
;; Last Modified On: Tue Jun 18 12:53:08 1991
;; RCS Info        : $Revision: 1.7 $ $Locker:  $
;; ========================================================================
;; NOTE: this file must be recompiled if changed.
;;
;; Copyright (C) Mike Williams <mike-w@cs.aukuni.ac.nz> 1991
;;
;; LCD Archive Entry:
;; lib-complete|Mike Williams|mike-w@cs.aukuni.ac.nz
;; |Completion on a search path
;; |91-06-20|1.7|~/misc/lib-complete.el.Z
;;
;; This file is not part of GNU Emacs, but is made available under the
;; same conditions.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Many thanks to Hallvard Furuseth <hallvard@ifi.uio.no> for his
;; helpful suggestions.

(provide 'lib-complete)

;; LCD Archive Entry:
;; lib-complete|Mike Williams|mike-w@cs.aukuni.ac.nz
;; |Completion on a search path
;; |91-04-20|$Revision: 1.7 $|

;;=== Usage ===============================================================
;; 
;; (autoload (fmakunbound 'load-library) "lib-complete" nil t)
;; (autoload 'locate-file "lib-complete" nil t)
;; (autoload 'library-all-completions "lib-complete")
;; (autoload 'read-library "lib-complete")

;;=== Locate a file in a search path ======================================

(defun locate-file (FILE SEARCH-PATH &optional SUFFIX-LIST PRED)
  "Search for FILE on SEARCH-PATH (list).  If optional SUFFIX-LIST is
provided, allow file to be followed by one of the suffixes.
Optional second argument PRED restricts the number of files which
may match.  The default is file-exists-p."
  (if (not SUFFIX-LIST) (setq SUFFIX-LIST '("")))
  (if (not PRED) (setq PRED 'file-exists-p))
  (if (file-name-absolute-p FILE) (setq SEARCH-PATH '(nil)))
  (if (equal FILE "") (error "Empty filename"))
  (let ((filelist 
	 (mapcar 
	  (function (lambda (ext) (concat FILE ext)))
	  SUFFIX-LIST)))
    ;; Search SEARCH-PATH for a readable file in filelist
    (catch 'found
      (while SEARCH-PATH
	(let ((filelist filelist))
	  (while filelist
	    (let ((filepath (expand-file-name (car filelist) 
					      (car SEARCH-PATH))))
	      (if (funcall PRED filepath)
		  (throw 'found filepath)))
	    (setq filelist (cdr filelist))))
	(setq SEARCH-PATH (cdr SEARCH-PATH))))
    ))

;;=== Determine completions for filename in search path ===================

(defun library-all-completions (FILE SEARCH-PATH &optional FULL FAST)
  "Return all completions for FILE in any directory on SEARCH-PATH.
If optional third argument FULL is non-nil, returned pathnames should be 
  absolute rather than relative to some directory on the SEARCH-PATH.
If optional fourth argument FAST is non-nil, don't sort the completions,
  or remove duplicates."
  (setq FILE (or FILE ""))
  (if (file-name-absolute-p FILE)
      ;; It's an absolute file name, so don't need SEARCH-PATH
      (progn
	(setq FILE (expand-file-name FILE))
	(file-name-all-completions 
	 (file-name-nondirectory FILE) (file-name-directory FILE)))
    (let ((subdir (file-name-directory FILE))
	  (file (file-name-nondirectory FILE))
	  all-completions)
      ;; Make list of completions in each directory on SEARCH-PATH
      (while SEARCH-PATH
	(let* ((dir (concat (file-name-as-directory 
			     (expand-file-name (car SEARCH-PATH)))
			    subdir))
	       (dir-prefix (if FULL dir subdir)))
	  (if (file-directory-p dir)
	      (let ((subdir-completions 
		     (file-name-all-completions file dir)))
		(while subdir-completions
		  (setq all-completions 
			(cons (concat dir-prefix (car subdir-completions))
			      all-completions))
		  (setq subdir-completions (cdr subdir-completions))))))
	(setq SEARCH-PATH (cdr SEARCH-PATH)))   
      (if FAST all-completions
	(let ((sorted (nreverse (sort all-completions 'string<)))
	      compressed)
	  (while sorted
	    (if (equal (car sorted) (car compressed)) nil
	      (setq compressed (cons (car sorted) compressed)))
	    (setq sorted (cdr sorted)))
	  compressed)))))

;;=== Utilities ===========================================================

(defmacro progn-with-message (MESSAGE &rest FORMS)
  "(progn-with-message MESSAGE FORMS ...)
Display MESSAGE and evaluate FORMS, returning value of the last one."
  ;; based on Hallvard Furuseth's funcall-with-message
  (` 
   (if (eq (selected-window) (minibuffer-window))
       (save-excursion
	 (goto-char (point-max))
	 (let ((orig-pmax (point-max)))
	   (unwind-protect
	       (progn
		 (insert " " (, MESSAGE)) (goto-char orig-pmax)
		 (sit-for 0)		; Redisplay
		 (,@ FORMS))
	     (delete-region orig-pmax (point-max)))))
     (prog2
      (message "%s" (, MESSAGE))
      (progn (,@ FORMS))
      (message "")))))

(put 'progn-with-message 'lisp-indent-hook 1)

;;=== Completion caching ==================================================

(defconst lib-complete:cache nil
  "Used within read-library and read-library-internal to prevent 
costly repeated calls to library-all-completions.
Format is a list of lists of the form

    ([<path> <subdir>] <cache-record> <cache-record> ...)

where each <cache-record> has the form

   (<root> <modtimes> <completion-table>)")

(defun lib-complete:better-root (ROOT1 ROOT2)
  "Return non-nil if ROOT1 is a superset of ROOT2."
  (and (equal (file-name-directory ROOT1) (file-name-directory ROOT2))
       (string-match
	(concat "^" (regexp-quote (file-name-nondirectory ROOT1)))
	ROOT2)))

(defun lib-complete:get-completion-table (FILE PATH FILTER)
  (let* ((subdir (file-name-directory FILE))
	 (root (file-name-nondirectory FILE))
	 (PATH 
	  (mapcar 
	   (function (lambda (dir) (file-name-as-directory
				    (expand-file-name (or dir "")))))
	   PATH))
	 (key (vector PATH subdir FILTER))
	 (real-dirs 
	  (if subdir
	      (mapcar (function (lambda (dir) (concat dir subdir))) PATH)
	    PATH))
	 (path-modtimes
	  (mapcar 
	   (function (lambda (fn) (if fn (nth 5 (file-attributes fn))))) 
	   real-dirs))
	 (cache-entry (assoc key lib-complete:cache))
	 (cache-records (cdr cache-entry)))
    ;; Look for cached entry
    (catch 'table
      (while cache-records
	(if (and 
	     (lib-complete:better-root (nth 0 (car cache-records)) root)
	     (equal (nth 1 (car cache-records)) path-modtimes))
	    (throw 'table (nth 2 (car cache-records))))
	(setq cache-records (cdr cache-records)))
      ;; Otherwise build completions
      (let ((completion-list 
	     (progn-with-message "(building completion table...)"
	       (library-all-completions FILE PATH nil 'fast)))
	    (completion-table (make-vector 127 0)))
	(while completion-list
	  (let ((completion
		 (if (or (not FILTER) 
			 (file-directory-p (car completion-list))) 
		     (car completion-list)
		   (funcall FILTER (car completion-list)))))
	    (if completion
		(intern completion completion-table)))
	  (setq completion-list (cdr completion-list)))
	;; Cache the completions
	(lib-complete:cache-completions key root 
					path-modtimes completion-table)
	completion-table))))

(defvar lib-complete:max-cache-size 20 
  "*Maximum number of search paths which are cached.")

(defun lib-complete:cache-completions (KEY ROOT MODTIMES TABLE)
  (let ((cache-entry (assoc key lib-complete:cache))
	(cache-records (cdr cache-entry))
	(new-cache-records (list (list ROOT MODTIMES TABLE))))
    (if (not cache-entry) nil
      ;; Remove old cache entry
      (setq lib-complete:cache (delq cache-entry lib-complete:cache))
      ;; Copy non-redundant entries from old cache entry
      (while cache-records
	(if (or (equal ROOT (nth 0 (car cache-records)))
		(lib-complete:better-root ROOT (nth 0 (car cache-records))))
	    nil
	  (setq new-cache-records 
		(cons (car cache-records) new-cache-records)))
	(setq cache-records (cdr cache-records))))
    ;; Add entry to front of cache
    (setq lib-complete:cache
	  (cons (cons KEY (nreverse new-cache-records)) lib-complete:cache))
    ;; Trim cache
    (let ((tail (nthcdr lib-complete:max-cache-size lib-complete:cache)))
      (if tail (setcdr tail nil)))))

;;=== Read a filename, with completion in a search path ===================

(defun read-library-internal (FILE FILTER FLAG)
  "Don't call this."
  ;; Relies on read-library-internal-search-path being let-bound
  (let ((completion-table
	 (lib-complete:get-completion-table
	  FILE read-library-internal-search-path FILTER)))
    (cond
     ((not completion-table) nil)
     ;; Completion table is filtered before use, so the PREDICATE
     ;; argument is redundant.
     ((eq FLAG nil) (try-completion FILE completion-table nil))
     ((eq FLAG t) (all-completions FILE completion-table nil))
     ((eq FLAG 'lambda) (and (intern-soft FILE completion-table) t))
     )))

(defun read-library (PROMPT SEARCH-PATH &optional DEFAULT MUST-MATCH 
			    FULL FILTER)
  "Read library name, prompting with PROMPT and completing in directories
from SEARCH-PATH.  A nil in the search path represents the current
directory.  Completions for a given search-path are cached, with the
cache being invalidated whenever one of the directories on the path changes.
Default to DEFAULT if user enters a null string.
Optional fourth arg MUST-MATCH non-nil means require existing file's name.
  Non-nil and non-t means also require confirmation after completion.
Optional fifth argument FULL non-nil causes a full pathname, rather than a 
  relative pathname, to be returned.  Note that FULL implies MUST-MATCH.
Optional sixth argument FILTER can be used to provide a function to
  filter the completions.  This function is passed the filename, and should
  return a transformed filename (possibly a null transformation) or nil, 
  indicating that the filename should not be included in the completions."
  (let* ((read-library-internal-search-path SEARCH-PATH)
	 (library (completing-read PROMPT 'read-library-internal 
				   FILTER (or MUST-MATCH FULL) nil)))
    (cond 
     ((equal library "") DEFAULT)
     (FULL (locate-file library read-library-internal-search-path))
     (t library))))

;; NOTE: as a special case, read-library may be used to read a filename
;; relative to the current directory, returning a *relative* pathname
;; (read-file-name returns a full pathname).
;;
;; eg. (read-library "Local header: " '(nil) nil)

;;=== Replacement for load-library with completion ========================

(defun load-library (LIBRARY)
  "Load the library named LIBRARY."
  (interactive 
   (list 
    (read-library "Load Library: " load-path nil nil nil
		  (function (lambda (fn) 
			      (cond 
			       ((string-match "\\.elc?$" fn)
				(substring fn 0 (match-beginning 0))))))
		  )))
  (load LIBRARY))

;;=== END of lib-complete.el ==============================================
--
    /-------------------- mike-w@cs.aukuni.ac.nz ---------------------\
    | Mike Williams, Computer Science, Auckland University, Aotearoa. |
    \-------------- I have a lovely bunch of coconuts. ---------------/
    Disclaimer: I wasn't even there.
-- 
New administrater uofa.
