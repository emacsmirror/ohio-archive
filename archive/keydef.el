
;;; keydef.el --- a simpler way to define keys, with kbd syntax

;; Emacs Lisp Archive Entry
;; Filename: keydef.el
;; Author: Michael John Downes <mjd@ams.org>
;; Created: 2001/01/18
;; Keywords: convenience lisp customization keyboard keys
;; Version: 1.9
;; $Revision: 1.9 $ $Date: 2001/02/14 19:03:46 $

;; This program was placed in the public domain on 2001/01/18 by the
;; Author, who in his obtuseness is unable to detect any practical
;; reason for bothering with copyleft for this sort of package (small,
;; plain text, available on the WWW). For more info see
;;
;;   http://www.fsf.org/philosophy/license-list.html
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;;; The macro keydef provides a simplified interface to define-key that
;;; smoothly handles a number of common complications.

;;; The global-set-key command isn't ideal for novices because of its
;;; relatively complex syntax. And I always found it a little
;;; inconvenient to have to quote the name of the command---that is, I
;;; tend to forget the quote every once in a while and then have to go
;;; back and fix it after getting a load error.

;;; One of the best features is that you can give an Emacs lisp form (or
;;; even a series of forms) as the key definition argument, instead of a
;;; command name, and the keydef macro will automatically add an
;;; interactive lambda wrapper. I use this to get, for example, a more
;;; emphatic kill-buffer command (no confirmation query) by writing
;;;
;;;   (keydef "<S-f11>" (kill-buffer nil))
;;;
;;; For keydef the key sequence is expected to be given uniformly in the
;;; form of a string for kbd, with one or two refinements that are
;;; intended to conceal from users certain points of confusion, such as
;;; (for those whose keyboards lack a Meta key) the whole
;;; Meta/ESC/escape muddle.

;;; I have had some trouble in the past regarding the distinction
;;; between ESC and [escape] (in a certain combination of circumstances
;;; using the latter form caused definitions made with the other form to
;;; be masked---most puzzling when I wasn't expecting it). Therefore the
;;; ESC form is actually preprocessed a bit to ensure that the binding
;;; goes into esc-map.

;;; There is one other special feature of the key sequence syntax
;;; expected by the keydef macro: You can designate a key definition for
;;; a particular mode-map by giving the name of the mode together with
;;; the key sequence string in list form, for example
;;;
;;;   (keydef (latex "C-c %") comment-region)
;;;
;;; This means that the key will be defined in latex-mode-map. I
;;; arranged for the mode name to be given in symbol form just because I
;;; didn't want to have to type extra quotes if I could get away with
;;; it. For the same reason this kind of first arg is not written in
;;; dotted pair form.

;;; If the given mode-map is not defined, keydef "does the right thing"
;;; using eval-after-load. In order to determine what library the
;;; mode-map will be loaded from, it uses the following algorithm:
;;;
;;; First check if foo-mode has autoload information. If not, check
;;; whether "foo-mode" is the name of a library that can be found
;;; somewhere in the load-path (using locate-library); otherwise check
;;; whether "foo" is the name of a locatable library. Failing that, give
;;; up and return nil.

;;; The following examples show some typical keydef lines followed by the
;;; results of the macro expansion.
;;;
;;; (keydef "C-x m" gnus-group-mail)
;;;
;;;   -->(define-key global-map (kbd "C-x m") (quote gnus-group-mail))
;;;
;;; (keydef "C-x m" gnus-gruop-mail)
;;;
;;;   -->(error "Command gnus-gruop-mail unknown \(perhaps \
;;;              misspelled, or not loaded yet\)")
;;;
;;; ;; A leading ESC gets special handling to go through esc-map.
;;; (keydef "ESC &" query-replace-regexp)
;;;
;;;   -->(define-key esc-map (kbd "&") (quote query-replace-regexp))
;;;
;;; ;; Undefine a key
;;; (keydef "ESC `")
;;;
;;;   -->(define-key esc-map (kbd "`") nil)
;;;
;;; ;; Define a macro
;;; (keydef "\"" "``''\C-b\C-b")
;;;
;;;   -->(define-key global-map (kbd "\"") "``''\002\002")
;;;
;;; ;; Reset a key to self-insert
;;; (keydef "\"" "\"")
;;;
;;;   -->(define-key global-map (kbd "\"") (quote self-insert-command))
;;;
;;; ;; If the second arg is a list, wrap it in an interactive lambda form.
;;; (keydef "C-z"
;;;   (message "Control-Z key disabled---redefine it if desired."))
;;;
;;;   -->(define-key global-map
;;;       (kbd "C-z")
;;;       (lambda (arg)
;;;         "anonymous keydef function"
;;;         (interactive "p")
;;;         (message "Control-Z key disabled---redefine it if desired.")))
;;;
;;; ;; This shows the notation for F-keys.
;;; (keydef "<C-f17>" (kill-buffer nil))
;;;
;;;   -->(define-key global-map
;;;       (kbd "<C-f17>")
;;;       (lambda (arg)
;;;         "anonymous keydef function"
;;;         (interactive "p")
;;;         (kill-buffer nil)))
;;;
;;; ;; Because of the confusing Meta/Escape complications, I recommend to
;;; ;; the users that I support that they use the ESC notation
;;; ;; consistently if that is what they type from their keyboard, even
;;; ;; for F-key definitions that might normally be written with <M-...>
;;; ;; notation.
;;; (keydef "ESC <f3>" find-file-read-only)
;;;
;;;   -->(define-key esc-map (kbd "<f3>") (quote find-file-read-only))
;;;
;;; ;; The next two definitions go together. The second one shows how to
;;; ;; write a mode-specific definition.
;;; (keydef "<f5>" isearch-forward)
;;;
;;;   -->(define-key global-map (kbd "<f5>") (quote isearch-forward))
;;;
;;; (keydef (isearch "<f5>") isearch-repeat-forward)
;;;
;;;   -->(define-key isearch-mode-map (kbd "<f5>")
;;;                                   (quote isearch-repeat-forward))
;;;
;;; ;; Making a definition for a mode-map that might not be loaded yet.
;;; (keydef (latex "C-c %") comment-region)
;;;
;;;   -->(eval-after-load "tex-mode"
;;;        (quote
;;;         (define-key latex-mode-map
;;;           (kbd "C-c %")
;;;           (quote comment-region))))
;;;
;;; If the mode-map is not loaded yet AND the command being bound to a
;;; key is undefined at the time of the keydef assignment, it presents
;;; further problems. The simplest solution is to assume that after the
;;; package is loaded that defines the mode-map, the given command will
;;; be defined and satisfy commandp. With some extra effort it should be
;;; possible to determine more accurately whether the command will be
;;; defined or not, but I'm not sure I want to go to that extreme, since
;;; as far as I can see it would require opening the package file and
;;; searching through it for a matching defun/defalias/fset statement.
;;; [mjd,2001-02-14]

;;; About that interactive lambda wrapper added by keydef, when the CMD
;;; arg does not satisfy commandp: It always takes a single prefix
;;; argument named "arg", which is read in the usual way with
;;; (interactive "p"); so this could be used in the body of the function
;;; if need be.

;;; Code:

;;; TO DO:
;;; ---If someone wants to do massive alterations or additions to a
;;; mode-map that is not yet loaded, it might be a good idea to
;;; provide another macro that will bundle them into a single
;;; eval-after-load call rather than dozens of separate ones.
;;;
;;; ---More error-checking would probably be a good idea, when SEQ
;;; satisfies listp but the contents of the list are not usable in the
;;; way that we expect.

(defun keydef-lib-lookup (mode)
  "For a not-already-loaded mode function, try to determine what library
it would be loaded from: First check for autoload information, otherwise
check if a library file matching the mode name can be found in the load
path, with or without the -mode suffix. Failing that, give up."
  (let* ((modesym (intern mode))
         (fcar (and (fboundp modesym) (car (symbol-function modesym)))))
    (cond
     ((eq fcar 'autoload)
      (car (cdr (symbol-function modesym))))
     ((locate-library mode)
      mode)
     (t
      (let ((shortmode (substring mode 0 -5))) ; chop "-mode" from the end
        (if (locate-library shortmode)
            shortmode))))))

;;;###autoload
(defmacro keydef (seq &rest cmd)
  "Define the key sequence SEQ, written in kbd form, to run CMD.
CMD is automatically wrapped in an anonymous interactive function if it
is Emacs Lisp code rather than a command name. SEQ may also have the form
\(MODE SEQ\) where the car is a mode name\; for example

  \(keydef \(latex \"C-c %\"\) comment-region\)

means to define the given key in latex-mode-map. To be more precise, the
\"mode name\" that you use here should yield the proper foo-mode-map
symbol when \"-mode-map\" is appended\; although this will normally
match the mode name as given in the mode line, Shell-script is one
example I can think of where it doesn't---the map is named sh-mode-map."
  (let ((map (quote global-map))
        (modestring)
        (loaded t))
    ;; If seq is a list, the car indicates a mode-specific map that we
    ;; should use instead of global-map.
    (if (and (listp seq)
             (symbolp (car seq))
             (stringp (car (cdr seq))))
        (progn
          (setq modestring
                (concat (downcase (symbol-name (car seq))) "-mode"))
          (setq map (intern (concat modestring "-map")))
          (if (not (and (boundp map) (keymapp (symbol-value map))))
              (setq loaded nil))
          (setq seq (car (cdr seq)))))
    (cond
     ((stringp seq)
      (if (string-match "^ESC " seq)
          (progn
            (setq seq (substring seq 4))
            (setq map (quote esc-map)))))
     (t (error "Invalid key sequence for keydef \(kbd syntax is required\).")))
    (if (not (null cmd))
        (let ((token (car cmd)))
          ;; Note that commandp is true for strings. So we have to be a
          ;; little careful about the order of tests here.
          (cond
           ;; This case arises when an explicit second arg of nil is given.
           ((eq token nil)
            (setq cmd nil))
           ;; If someone forgets that keydef does not require you to
           ;; quote the command name, we had better make sure it works
           ;; anyway.
           ((eq (car-safe token) 'quote)
            (setq cmd token))
           ;; If the CMD is a one-character string that matches the SEQ, use
           ;; self-insert-command as the binding. Otherwise it will be a macro
           ;; that will run an infinite loop until specpdl-size is exceeded.
           ((stringp token)
            (if (and (= (length token) 1)
                     (string-equal token seq))
                (setq cmd '(quote self-insert-command))
              (setq cmd token)))        ; kbd macro string
           ;; If the command is a simple command name, use it directly as the
           ;; definition.
           ((and (commandp token)
                 (= (length cmd) 1))
            (setq cmd `(quote ,token)))
           ;; If the command looks like a simple command name but fails the
           ;; commandp test, then probably it was misspelled; if it passes the
           ;; fboundp test, however, make a lambda wrapper similar to the next
           ;; case. Could try to work harder at getting the arguments right in
           ;; that case, but for now just assume it has zero args.
           ((and (= (length cmd) 1)
                 (not (listp token)))
            (cond
             ((and (symbolp token) (fboundp token))
              (setq cmd
                    (append
                     '(lambda (arg) "*Anonymous function created by keydef."
                        (interactive "p"))
                     (list cmd))))
             ((and (symbolp token) (not loaded))
              ; If it's an unknown symbol, assume that it is a command
              ; that will be defined by a not-yet-loaded package.
              (setq cmd `(quote ,token)))
             (t
              (error
               "Command %s unknown \(perhaps misspelled, or not loaded yet\)"
                     (prin1-to-string token)))))
           (t
            (setq cmd
                  (append
                   '(lambda (arg)
                      "*Anonymous function created by keydef."
                      (interactive "p"))
                   cmd))))))
    (if (and (not loaded) modestring)
        (let ((loadfrom (keydef-lib-lookup modestring)))
          (if loadfrom
              `(eval-after-load ,loadfrom
                 (quote (define-key ,map (kbd ,seq) ,cmd)))
            (error "Unknown mode '%s' \(perhaps misspelled?\)" modestring)))
      `(define-key ,map (kbd ,seq) ,cmd))))

(provide 'keydef)
;;; keydef.el ends here