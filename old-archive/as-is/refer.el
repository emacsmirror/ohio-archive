;Date: 18 Jan 89 01:35:44 GMT
;From: Ashwin Ram <Ram-Ashwin@yale.ARPA>
;Subject: REFER.EL -- Look up references in bibliography files.
;Reply-To: Ashwin Ram <Ram-Ashwin@cs.yale.edu>
;Organization: Computer Science, Yale University, New Haven, CT 06520-2158

;Some time ago, I posted a request for a GNU Emacs package to search for
;keywords in files containing lists of references.  The TAGS package didn't do
;the job because the TAGS table for .BIB files only contained the citation
;keyword; I wanted to be able to search for one or more keywords occurring
;anywhere inside a bibliography entry.

;Since no-one volunteered a package, I went ahead and wrote my own.  Here's
;what you get:

;    refer-find-entry

;        prompt for a blank separated list of keywords, then search for them
;        in the specified bibliography files.  If called with prefix argument,
;        continue search from previous point.

;        By default, all the .bib files in the current directory are searched;
;        this can be customized to prompt for a file name or to automatically
;        pick up the filename(s) from a \bibliography command in a TeX file.
;        See the included documentation.

;    refer-find-next-entry

;        search for next occurrence of the same set of keywords.  This is the
;        same as calling refer-find-entry with a prefix argument.

;For example, if you wanted to find an article by Knuth dealing with
;semaphores, you'd call refer-find-entry and in response to the Keywords:
;prompt, you'd type in: Knuth semaphores
;(i.e., one or more keywords separated by blanks).  You would then find the
;cursor positioned at the first bibliograpy entry which contained these
;keywords anywhere in the entry.

;Comments, corrections and suggestions are welcome.  If this is useful, you
;are welcome to include it in the GNU Emacs distribution.

;-- Ashwin.

;ARPA:    Ram-Ashwin@cs.yale.edu
;UUCP:    {decvax,ucbvax,harvard,cmcl2,...}!yale!Ram-Ashwin
;BITNET:  Ram@yalecs


;REFER.EL follows after the page break.
;
;; REFER.EL -- Look up references in bibliography files.
;; Copyright (C) 1989 Ashwin Ram.
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
;; 01/08/89 Ashwin Ram <Ram-Ashwin@cs.yale.edu>
;;          Initial release.
;;
;;
;; DOCUMENTATION:
;;
;; Functions to look up references in bibliography files given lists of
;; keywords, similar to refer(1).  I don't use tags since tags on .bib files
;; only picks up the cite key, where as refer-find-entry looks for occurrences
;; of keywords anywhere in the bibliography entry.
;;
;; To use:
;;      (autoload 'refer-find-entry "refer" nil t)
;; or   (require 'refer)
;;
;;      To look for an article by Knuth about semaphores:
;;          Invoke refer-find-entry, then in response to the Keywords: prompt,
;;          say: Knuth semaphores (a blank-separated list of keywords to be used
;;          as search strings).
;;
;;      To continue the previous search, i.e., to search for the next occurrence
;;      of the keywords, use refer-find-next-entry, or invoke refer-find-entry
;;      with a prefix argument.
;;
;;      If the list of bibliography files changes, reinitialize the variable
;;      refer-bib-files.
;;
;; To customize:
;;      See variables refer-bib-files, refer-cache-bib-files and
;;      refer-bib-files-regexp.  By default, these are set up so that refer
;;      looks for the keywords you specify in all the .bib files in the current
;;      directory.
;;
;;      The only assumption I make about bib files is that they contain a bunch
;;      of entries, one to a paragraph.  refer-find-entry searches paragraph by
;;      paragraph, looking for a paragraph containing all the keywords
;;      specified.  So you should be able to use pretty much any bib file with
;;      this code.  If your bib file does not use paragraphs to separate
;;      entries, try setting the paragraph-start/separate variables, or changing
;;      the (forward-paragraph 1) call in refer-find-entry-in-file.


(provide 'refer)

(defvar refer-bib-files 'dir
   "*List of \\.bib files to search for references,
or one of the following special values:
nil  = prompt for \\.bib file (if visiting a \\.bib file, use it as default)
auto = read \\.bib file names from appropriate command in buffer (see refer-bib-files-regexp)
dir  = use all \\.bib files in current directory.

If a specified file doesn't exist and has no extension, a \\.bib extension
is automatically tried.

If refer-bib-files is nil, auto or dir, it is setq'd to the appropriate
list of files when it is first used if refer-cache-bib-files is t.  If
refer-cache-bib-files is nil, the list of \\.bib files to use is re-read
each time it is needed.")

(defvar refer-cache-bib-files t
   "*Variable determining whether the value of refer-bib-files should be cached.
If t, initialize the value of refer-bib-files the first time it is used.  If
nil, re-read the list of \\.bib files depending on the value of refer-bib-files
each time it is needed.")

(defvar refer-bib-files-regexp "\\\\bibliography"
   "*Regexp matching a bibliography file declaration.
The current buffer is expected to contain a line such as
\\bibliography{file1,file2,file3}
which is read to set up refer-bib-files.  The regexp must specify the command
(such as \\bibliography) that is used to specify the list of bib files.  The
command is expected to specify a file name, or a list of comma-separated file
names, within curly braces.
If a specified file doesn't exist and has no extension, a \\.bib extension
is automatically tried.")

(make-variable-buffer-local 'refer-bib-files)
(make-variable-buffer-local 'refer-cache-bib-files)

(defun refer-find-entry (keywords &optional continue)
   "Find entry in refer-bib-files containing KEYWORDS.
If KEYWORDS is nil, prompt user for blank-separated list of keywords.
If CONTINUE is t, or if called interactively with a prefix arg, look for next
entry by continuing search from previous point."
   (interactive (list nil current-prefix-arg))
   (or keywords (setq keywords (if continue
                                   refer-previous-keywords
                                   (read-string "Keywords: "))))
   (setq refer-previous-keywords keywords)
   (refer-find-entry-internal keywords continue))

(defun refer-find-next-entry ()
   "Find next occurrence of entry in refer-bib-files.  See refer-find-entry."
   (interactive)
   (refer-find-entry-internal refer-previous-keywords t))

(defun refer-find-entry-internal (keywords continue)
   (let ((keywords-list (convert-string-to-list-of-strings keywords))
         (files (if continue
                    refer-saved-state
                    (refer-get-bib-files))))
      (catch 'found
         (while files
            (let ((file (cond ((file-exists-p (car files)) (car files))
                              ((file-exists-p (concat (car files) ".bib")) (concat (car files) ".bib")))))
               (setq refer-saved-state files)
               (if file
                   (if (refer-find-entry-in-file keywords-list file continue)
                       (throw 'found (find-file file))
                       (setq files (cdr files)))
                   (progn (message "Scanning %s... No such file" (car files) (ding))
                          (sit-for 1)
                          (setq files (cdr files))))))
         (message "Keywords \"%s\" not found in any \.bib file" keywords (ding)))))

(defun refer-find-entry-in-file (keywords-list file &optional continue)
   (message "Scanning %s..." file) ; (expand-file-name file)
   (set-buffer (find-file-noselect file))
   (if continue
       (forward-paragraph 1)
       (goto-char (point-min)))
   (let ((begin (point))
         (end 0)
         (found nil))
      (while (and (not found)
                  (not (eobp)))
         (forward-paragraph 1)
         (setq end (point))
         (setq found
               (every (function (lambda (keyword)
                                   (goto-char begin)
                                   (re-search-forward keyword end t)))
                      keywords-list))
         (if (not found)
             (progn
                (setq begin end)
                (goto-char begin))))
      (if found
          (progn (goto-char begin)
                 (re-search-forward "\\W" nil t)
                 (message "Scanning %s... found" file))
          (progn (message "Scanning %s... not found" file)
                 nil))))

(defun every (pred l)
   (cond ((null l) nil)
         ((funcall pred (car l))
          (or (null (cdr l))
              (every pred (cdr l))))))

(defun convert-string-to-list-of-strings (s)
   (let ((current (current-buffer))
         (temp-buffer (get-buffer-create "*refer-temp*")))
      (set-buffer temp-buffer)
      (erase-buffer)
      (insert (regexp-quote s))
      (goto-char (point-min))
      (insert "(\"")
      (while (re-search-forward "[ \t]+" nil t)
         (replace-match "\" \"" t t))
      (goto-char (point-max))
      (insert "\")")
      (goto-char (point-min))
      (prog1 (read temp-buffer)
         (set-buffer current))))

(defun refer-get-bib-files ()
   (let ((files
           (cond ((null refer-bib-files) 
                  (list (expand-file-name
                          (if (eq major-mode 'bibtex-mode)
                              (read-file-name (format ".bib file: (default %s) " (file-name-nondirectory (buffer-file-name)))
                                              (file-name-directory (buffer-file-name))
                                              (file-name-nondirectory (buffer-file-name))
                                              t)
                              (read-file-name ".bib file: " nil nil t)))))
                 ((listp refer-bib-files) refer-bib-files)
                 ((eq refer-bib-files 'auto)
                  (save-excursion
                     (if (progn (goto-char (point-min))
                                (re-search-forward (concat refer-bib-files-regexp "\{") nil t))
                         (let ((files (list (buffer-substring (point)
                                                              (progn (re-search-forward "[,\}]" nil t)
                                                                     (backward-char 1)
                                                                     (point))))))
                            (while (not (looking-at "\}"))
                               (setq files (append files
                                                   (list (buffer-substring (progn (forward-char 1)
                                                                                  (point))
                                                                           (progn (re-search-forward "[,\}]" nil t)
                                                                                  (backward-char 1)
                                                                                  (point)))))))
                            files)
                         (error "No \\\\bibliography command in this buffer, can't read refer-bib-files"))))
                 ((eq refer-bib-files 'dir)
                  (directory-files "." t "\\.bib$"))
                 (t (error "Illegal value for refer-bib-files: %s" refer-bib-files)))))
      (if refer-cache-bib-files
          (setq refer-bib-files files))
      files))
