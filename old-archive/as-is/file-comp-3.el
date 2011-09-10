;To: unix-emacs@bbn.com
;Date: 21 Mar 89 16:36:47 GMT
;From: Ashwin Ram <Ram-Ashwin@yale.ARPA>
;Subject: FILE-COMPLETE.EL -- Display file name completions in mod-time order
;
;GNU Emacs 18.52.26 of Mon Mar 20 1989 on leo.ring.cs.yale.edu (Domain/OS)
;
;Some time ago, I had asked if there was a way to display file name
;completions in order of modification time instead of alphabetically.  Since
;no-one seemed to have anything on hand, I wrote some code to do this with
;considerable help from Joe Wells.
;
;If you load file-complete.el, you can use
;        ?       to display completions alphabetically, as always
;        |       to display completions in mod-time order
;        =       to display completions without sorting
;If you're completing something other than file names, | behaves like ?, so it
;is safe to rebind ? to this function if you so choose.
;
;I find this package useful in several situations, e.g., (i) if I'm trying to
;find a file in someone else's directory -- I know it's a recent file but I
;don't know the exact file name, or (ii) I'm trying to find a file of mine
;that I haven't used for a long time, whose exact name I've forgotten.  Etc...
;
;One caveat -- for some reason, file-attributes uses lstat(), while
;file-newer-than-file-p uses stat().  I don't know why, but this means that
;Joe Wells's fast sort using file-attributes may produce slightly different
;results with links than my initial attempt using file-newer-than-file-p.
;
;Feedback is welcome.
;
;------------------------------------------------------------------------------
;; FILE-COMPLETE.EL -- Display file name completions in mod-time order.
;; Copyright (c) 1989 Free Software Foundation, Inc.
;;
;; This file is not part of the GNU Emacs distribution (yet).
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;
;; Comments, corrections, and improvements should be sent to:
;;
;;     Ashwin Ram
;;
;;     ARPA:   Ram-Ashwin@cs.yale.edu
;;     UUCP:   {decvax,ucbvax,harvard,cmcl2,...}!yale!Ram-Ashwin
;;     BITNET: Ram@yalecs
;;
;;
;; MODIFICATION HISTORY:
;;
;; 03/13/89 Ashwin Ram <Ram-Ashwin@cs.yale.edu>
;;          Initial release.
;;
;; 03/18/89 Joe Wells <jbw%bucsf.bu.edu@bu-it.bu.edu>
;;          Optimized sort, did temp-minibuf-message, etc.
;;
;;
;; DOCUMENTATION:
;;
;; Display file name completions in order of modification date instead
;; of alphabetically.
;;
;; After loading or requiring this package, you can use:
;;        ?       to display completions alphabetically, as always
;;        |       to display completions in mod-time order
;;        =       to display completions without sorting
;; If you're completing something other than file names, | behaves like ?,
;; so it is safe to rebind ? to this function if you so choose.

;; For the curious, when doing a completing read for a filename,
;; minibuffer-completion-table is read-file-name-internal and
;; minibuffer-completion-predicate is the current-buffer's directory.

;; Suggested key bindings:
(define-key minibuffer-local-completion-map "|" 'file-name-completion-help)
(define-key minibuffer-local-completion-map "=" 'minibuffer-completion-help-unsorted)


(defun file-name-completion-help ()
  "Display a list of possible completions of the current minibuffer contents.
If the minibuffer is completing filenames, print the list in file modification
time order.  Should only be called from inside a completing read in the
minibuffer."
  (interactive)
  (message "Making completion list...")
  (let* ((buffer-string (buffer-string))
         (completions
          (all-completions buffer-string
                           minibuffer-completion-table
                           minibuffer-completion-predicate)))
    (cond (completions
           (with-output-to-temp-buffer " *Completions*"
             (display-completion-list
              ;; If completing filenames, sort filenames by mod-time
              ;; else sort strings in lexicographic order.
              (if (eq minibuffer-completion-table 'read-file-name-internal)
                  (let* ((string-dir    ;directory part of minibuf contents
                          (file-name-directory
                           (substitute-in-file-name buffer-string)))
                         (real-dir      ;directory for completion
                          (if (null string-dir)
                              minibuffer-completion-predicate
                            (expand-file-name
                             string-dir
                             minibuffer-completion-predicate))))
                    (sort-files-by-modtime completions real-dir))
                (sort completions 'string-lessp))))
           (temp-minibuf-message ""))   ;clear message
          (t
           (ding)
           (temp-minibuf-message " [No completions]")))))

(defun minibuffer-completion-help-unsorted ()
  "Display a list of possible completions of the current minibuffer contents.
Prints the list in the order that it is returned from all-completions
(unsorted).  Should only be called from inside a completing read in the
minibuffer."
  (interactive)
  (message "Making completion list...")
  (let ((completions
         (all-completions (buffer-string)
                          minibuffer-completion-table
                          minibuffer-completion-predicate)))
    (cond (completions
           (with-output-to-temp-buffer " *Completions*"
             (display-completion-list completions))
           (temp-minibuf-message ""))   ;clear message
          (t
           (ding)
           (temp-minibuf-message " [No completions]")))))

;; Avoid calling stat() more than once per file, at the expense of some extra
;; consing in file-attributes.  That's ok, because the consing is O(n) and
;; the stats were O(n lg n).  We grab the file modtime and put it in the list
;; with the filename.  Then we call sort with a predicate that compares the
;; modtimes.  The modtimes are in this format: (HIGH LOW) where HIGH and LOW
;; are 16 bit integers.  During the sort, the list is in this format:
;; ((FILENAME HIGH LOW) ...).  This sort occurs in place.

(defun sort-files-by-modtime (files &optional dir)
  "Sort a list of FILES by the files' modification times.
Optional argument DIR is the directory the files are located in, which
defaults to the default-directory of the current buffer.  This is a
destructive in-place sort, but the head of the list may change.  Returns
the new head of the list."
  (let ((p files)
        time1 time2)
    (while (consp p)
      (setcar p (cons (car p)
                      (nth 5 (file-attributes (expand-file-name (car p)
                                                                dir)))))
      (setq p (cdr p)))
    (setq files
          (sort files
                (function
                 (lambda (f1 f2)
                   (setq time1 (cdr f1)
                         time2 (cdr f2))
                   (or (> (car time1) (car time2))
                       (and (= (car time1) (car time2))
                            (> (car (cdr time1)) (car (cdr time2)))))))))
    (setq p files)
    (while (consp p)
      (setcar p (car (car p)))
      (setq p (cdr p))))
  files)

;; Too bad this isn't in src/minibuf.c:
;;
;; DEFUN ("temp-minibuf-message", Ftemp_minibuf_message, Stemp_minibuf_message,1, 1, 0,
;;   "Documentation.")
;;   (s)
;;      Lisp_Object s;
;; {
;;   CHECK_STRING (s);
;;   temp_minibuf_message (XSTRING(s)->data);
;;   return Qnil;
;; }
;;
;; defsubr (&Stemp_minibuf_message);

(defun temp-minibuf-message2 (m)
  "Prints string MESSAGE in the current buffer to the right of all text
in the buffer.  It is used mainly for putting messages in the minibuffer
while also showing the minibuffer text."
  (let ((osize (point-max))
        (inhibit-quit t))
    (save-excursion
      (goto-char osize)
      (insert m)
      (goto-char osize)
      ;; The next statement is a gross hack.
      ;; The purpose is to set minibuf_message = 0, so that the contents
      ;; of the minibuffer will show.
      (let ((unread-command-char ?\C-m))
        (read-from-minibuffer "" nil nil nil)) 
      (sit-for 2)
      (delete-region osize (point-max))
      (if quit-flag
          (setq quit-flag nil
                unread-command-char ?\C-g)))))

;; Check if temp-minibuf-message has been fixed in the C code.
(or (and (fboundp 'temp-minibuf-message)
         (subrp (symbol-function 'temp-minibuf-message)))
    (fset 'temp-minibuf-message
          (symbol-function 'temp-minibuf-message2)))

(provide 'file-complete)
;------------------------------------------------------------------------------
;
;-- Ashwin.
;
;ARPA:    Ram-Ashwin@cs.yale.edu
;UUCP:    {decvax,ucbvax,harvard,cmcl2,...}!yale!Ram-Ashwin
;BITNET:  Ram@yalecs

