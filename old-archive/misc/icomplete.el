; Here's the spiel, followed by the code.
; 
; Icomplete.el is a significant enhancement for minibuffer completion,
; providing succinct, keystroke-by-keystroke alternatives feedback in
; the minibuffer.  It takes effect when you load the code.  (You can
; inhibit and reenable the extended behavior after the package is loaded
; by toggling the switch-variable 'icomplete-inhibit' between a nil and
; non-nil value.)
; 
; The elisp code is attached below (search for "Cut Here"), after some
; general information about incremental completion and explanation of
; the completion cues.
; 
;                      About Incremental Completion
; 
; The completion cue style is adapted from Michael Cook's 'iswitch'
; package, which provided some of the same sort of functionality, but
; specifically for 'switch-to-buffer'.  (Iswitch, in turn, took after
; things like csh/bash brace expansion expressions.)
; 
; Instead of concentrating on a single completing function, i worked on
; the minibuffer completion maps.  This provides generality, operating
; on all completion functions, and transparency, allowing full
; minibuffer operation, including eg all editing and window-switching
; actions in the minibuffer.
; 
; Icompletion is inhibited for completion functions that involve a lot
; of overhead, like filename completion.  It operates with everything
; else, and i've never noticed any extra lag using it.
; 
; 
;                       How to Read Icomplete Cues
; 
; Incremental completion shows the prospective completions by displaying
; the remaining, untyped portions of the candidates in brackets, just
; after what the user has entered.  As with normal completion, fully
; resolved input is signaled with the message 'Matched', in suitable
; (square or paren - see below) braces, following the complete input.
; 'No matches' is displayed if there are no eligible candidates.
; 
; Also, TAB, SPACE, question-mark, etc., work exactly the same as they
; do in normal completion, popping up a completion buffer when suitable.
; (The advantage of incremental completion is that you almost never need
; to pop up a completion buffer, because the options are usually already
; evident.)
; 
; The kind of brackets around the prospect suffixes indicates the kind
; of completion that is pending.
; 
; Curly `{' `}' braces are used when numerous candidates are pending,
; with all the candidate suffixes (possibly including a null one)
; separated by commas.  With the input '^H-f next-', the minibuffer looks
; like:
; 
; Describe-function: next-{window,error,complex-command,file,line}
; 
; Regular `(' `)' parens are used when a single, incomplete candidate
; remains and matching is enforced.  Function name completion in
; execute-extended-command and describe-function is an example.  Adding
; a 'w', '^H-f next-w', disambiguates the candidates of the previous
; example:
; 
; Describe-function: next-w(indow) [Matched]
; 
; Square `[' `]' brackets are used when displaying single candidates for
; optional completion, that is, completion where input outside the
; candidates is acceptable.  Buffer name completion in switch-to-buffer
; is one example; you are able to select buffers with new names, even
; when their names match the beginnings of existing buffers.  Eg, with
; input '^X-b ico', the minibuffer looks like:
; 
; Switch to buffer: ico[mplete.el] [Matched]
; 
; Sometimes all candidate suffixes have a common root, eg when you
; haven't yet typed in a '-' when entering extended commands.  In that
; case you can get a mixture of bracket cues, distingushing the
; definite and the ambiguous portions of the completions.  '^H-f next':
; 
; Describe-function: next(-){window,error,complex-command,file,line}
; 
; Minibuffer completion feedback does not interfere with minibuffer
; input editing.  The cursor is always correctly positioned to show
; where input is going.  The end of your typed input (not the end of the
; match feedback) is the end-of-line.  And you can remove characters
; >from the middle of the string and continue to get correct, immediate
; completion feedback.
; 
; This is a lot of verbage for such a modest feature.  Moreover, it was
; not a simple hack.  It was, however, fun and instructive, and i think
; the completion style represents an improvement in a fundamental aspect
; of the emacs user interface.  In any case, i really like it, i hope
; you'll give it a try...
; 
; Ken
; klm@nist.gov, 301 975-3539
; 

;;;_* Icomplete.el - minibuffer completion with incremental feedback

;;;_ + Provide
(provide 'icomplete)

;;;_ + Package Identification Stuff
;;;_  - Author: Ken Manheimer <klm@nist.gov>
;;;_  - Maintainer: Ken Manheimer <klm@nist.gov>
;;;_  - Version: icomplete.el,v 2.12 1993/06/02
;;;_  - Created: Mar 1993 klm@nist.gov - first release to usenet
;;;_  - Keywords: extensions
;;;_  - LCD Archive Entry and RCS ID
;;; LCD Archive Entry:
;;; icomplete|Ken  Manheimer|klm@nist.gov|
;;; Completion enhancement, with incremental feedbac in the minibuffer.|
;;; 02-Jun-1993|2.12|~/misc/icomplete.el.Z|
;;;
;;; $Header: /home/coil/klm/lib/emacs/RCS/icomplete.el,v 2.12 1993/06/02 22:23:41 klm Exp klm $

;;;_  - GNU Copyright
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Developed by Ken Manheimer, klm@nist.gov.

;;;_ + User Customization variables

(defconst icomplete-overload t
  "Configure this with t if you want the standard completing-read to be
overloaded by icompleting-read when the source file is loaded.")

(defvar icomplete-inhibit nil
  "*Set this variable to t to inhibit special icomplete behavior.")

(defvar icomplete-dynamic-default t
  "*When true, minibuffer-default value is visible only when for empty input.
Set this variable to nil to keep default option always visible.")

;;;_ + Package Variables and State Functions
;;;_  - Private icompletion state variables
(defvar icomplete-eoinput 0
  "Point where minibuffer input ends and completion info begins.")

(defvar icomplete-ignore-key nil
  "Icomplete-internal variable, to inhibit command action (but allow other
minibuffer processing) on first pending input key.")

;;; Command-loop shadow vars, for internal use by icomplete-process-key:
(setq icomplete-prefix-arg nil)
(setq icomplete-last-command nil)
;;;_  > icomplete-prime-minibuffer ()
(defun icomplete-prime-minibuffer ()
  (setq icomplete-ignore-key t)
  (setq unread-command-char 0))
;;;_  > icomplete-process-esc-key ()
(defun icomplete-process-esc-key ()
  (interactive)
  (icomplete-process-key))

;;;_ + Driver function & incidentals:
;;;_  > icomplete-process-key (&optional key keymap)
(defun icomplete-process-key (&optional key keymap)
  "A wrapper for minibuffer completion keystrokes, providing incremental
feedback about eligible completion candidates within the minibuffer input line.
\(This is the guts of incremental completion, and should properly be integrated
with the minibuffer command loop.  Simply binding to the keys which edit the
buffer is *not* the answer - that's too ambiguous.  It might be reasonable to
include it in a buffer-modified hook - Emacs 19\?.)"

  (interactive)
  (let* ((key (or key (this-command-keys)))
         (prefix-arg icomplete-prefix-arg)
         (last-command icomplete-last-command)
         (curmap (current-local-map))
         (realmap (or keymap
                      (if (eq curmap icomplete-completion-map)
                          icomplete-real-completion-map
                        icomplete-real-must-match-map)))
         (realmap-val (lookup-key realmap key))
         (val (if (and realmap-val
                                        ; lookup-key gives number of chars if
                                        ; key isn't in keymap:
                       (not (numberp realmap-val)))
                  realmap-val
                (lookup-key (current-global-map) key)))
         (default (if (and (boundp 'minibuffer-default)
                           minibuffer-default
                           (not (equal minibuffer-default "")))
                      (format "(default %s) " minibuffer-default)
                    nil))
         (minibuffer-prompt (if (boundp 'minibuffer-prompt)
                                (if (and default
                                         (not icomplete-dynamic-default))
                                    (concat minibuffer-prompt
                                            default)
                                  minibuffer-prompt)
                              ""))
         (do-icomplete (and (not icomplete-inhibit)
                            ;; Inhibit for file-name and other
                            ;; special-func completions:
                            (not (symbolp minibuffer-completion-table))
                            (equal (window-buffer (minibuffer-window))
                                   (current-buffer))))
         )

                                        ; Unravel keymap indirections:
    (if icomplete-ignore-key
        ;; If t, toggle and skip everything but post-key processing:
        (progn (setq unread-command-char -1)
               (setq icomplete-ignore-key nil))
                                        ; ... not if icomplete-ignore-key:
      (while (keymapp val)
        (setq key (read-key-sequence (if (boundp 'minibuffer-prompt)
                                         minibuffer-prompt
                                       "")))
        (setq val (or  (lookup-key val key)
                       (lookup-key (current-global-map) key))))

                                        ; Cleanup from last exhibit, deleting
                                        ; anything after the registered end of
                                        ; the users input (ie, match-status
                                        ; info):
      (if (and do-icomplete
               (< icomplete-eoinput (1- (point-max))))
          (condition-case err
              (delete-region icomplete-eoinput (point-max))
            (error (setq icomplete-eoinput 0))))

                                        ; Zero icomplete-eoinput, in case key
                                        ; command concludes the minibuffer
                                        ; session:
      (setq icomplete-eoinput 0)

      ;; --------------------------------------
                                        ; Process the key, encapsulating
                                        ; icomplete vars within scope of cmd.
                                        ; (Prefix arg carryover set in 'let*')
      (command-execute val)
                                        ; Preserve the prefix arg intended for
                                        ; the current minibuffer:
      (setq icomplete-prefix-arg prefix-arg)
      ;; --------------------------------------

      )                                 ; ... if icomplete-ignore-key.



                                        ; Exhibit prospective completions:
    (if (and do-icomplete
             ;; Ensure that processing the key left us in the minibuffer:
             (equal (window-buffer (minibuffer-window))
                    (current-buffer)))
        (let ((contents (buffer-substring (point-min)(point-max))))
          (save-excursion
            (goto-char (point-max))
                                        ; Register the end of input, so we
                                        ; know where the extra stuff
                                        ; (match-status info) begins:
            (setq icomplete-eoinput (point))
                                        ; Insert the match-status information:
            (insert-string
             (cond ((= (point-max) 1)
                    (if (and icomplete-dynamic-default
                             default)
                        default
                      ""))
                   (t (icomplete-prompt contents
                                        minibuffer-completion-table
                                        minibuffer-completion-predicate
                                        (not
                                         minibuffer-completion-confirm)))
                   )))))
    )
  )
;;;_  > icomplete-prompt (name candidates predicate require-match)
(defun icomplete-prompt (name candidates predicate require-match)
  "Show prospective candidates for completion in the minibuffer prompt line.

Prospects (if any) are displayed, bracketed by one of (), [], or {}
pairs.  The choice of bracket depends on whether a single prospect is
identified and matching is enforced, identified but matching is
optional, or ambiguous, respectively.  Determined, disambiguous
matches are then indicated with the message ' [Matched]' (whether
complete or not), or a ' [No matches]', if no matches are possible."

  (let ((comps (all-completions name candidates predicate))
                                        ; "-determined" - only one candidate
        (open-bracket-determined (if require-match "(" "["))
        (close-bracket-determined (if require-match ")" "]"))
                                        ;"-prospects" - more than one candidate
        (open-bracket-prospects "{")
        (close-bracket-prospects "}")
        )
    (cond ((null comps) (format " %sNo matches%s"
                                open-bracket-determined
                                close-bracket-determined))
          ((null (cdr comps))           ;one match
           (concat (if (and (> (length (car comps))
                               (length name)))
                       (concat open-bracket-determined
                               (substring (car comps) (length name))
                               close-bracket-determined)
                     "")
                   " [Matched]"))
          (t                            ;multiple matches
           (let* ((most (try-completion name candidates predicate))
                  (most-len (length most))
                  most-is-exact
                  (alternatives
                   (apply
                    'concat
                    (cdr (apply 'append
                                (mapcar '(lambda (com)
                                           (if (= (length com) most-len)
                                               ;; Most is one exact match,
                                               ;; note that and leave out
                                               ;; for later indication:
                                               (progn
                                                 (setq most-is-exact t)
                                                 ())
                                             (list ","
                                                   (substring com
                                                              most-len))))
                                        comps))))))
             (concat (and (> most-len (length name))
                          (concat open-bracket-determined
                                  (substring most (length name))
                                  close-bracket-determined))
                     open-bracket-prospects
                     (if most-is-exact
                         (concat "," alternatives)
                       alternatives)
                     close-bracket-prospects)))))
)

;;;_ + Minibuffer prep:
;;;_  > setup-icomplete-keymaps (&optional aside)
(defun setup-icomplete-keymaps (&optional aside)
  (interactive)
                                        ; icomplete maps will be used as
                                        ; completion maps in minibuffer, so
                                        ; icomplete-process-key is called:
  (setq icomplete-completion-map (make-keymap))
  (setq icomplete-must-match-map (make-keymap))
                                        ; lookup of meta-keys would circumvent
                                        ; icomplete completion maps, so we
                                        ; make a special one:
  (setq icomplete-esc-map (make-keymap))
                                        ; Register bona-fide completion maps
                                        ; in a holder var:
  (if (not (boundp 'icomplete-real-completion-map))
      (setq icomplete-real-completion-map
            minibuffer-local-completion-map))
  (if (not (boundp 'icomplete-real-must-match-map))
      (setq icomplete-real-must-match-map
            minibuffer-local-must-match-map))

                                        ; Every key in icomplete maps go to
                                        ; icomplete-process-key:
  (let* ((index (1- (length (if (vectorp icomplete-completion-map)
                                icomplete-completion-map
                              (car (cdr icomplete-completion-map)))))))
    (while (>= index 0)
      (define-key icomplete-completion-map
        (char-to-string index) 'icomplete-process-key)
      (define-key icomplete-esc-map
        (char-to-string index) 'icomplete-process-key)
      (setq index (1- index))))
  (setq icomplete-must-match-map (copy-sequence icomplete-completion-map))
                                        ; And make sure to hook in icomplete
                                        ; meta map:
  (define-key icomplete-completion-map
    (char-to-string meta-prefix-char) icomplete-esc-map)
  (define-key icomplete-must-match-map
    (char-to-string meta-prefix-char) icomplete-esc-map)

                                        ; Install the icompletion maps:
  (setq minibuffer-local-completion-map icomplete-completion-map)
  (setq minibuffer-local-must-match-map icomplete-must-match-map)
  )
(setup-icomplete-keymaps)

;;;_ + Value-added completing function overloads:
;;;_  - Note - you do not need to apply these overloads in order to get the
;            icompletion effects, but they do provide some small additional
;            features, like fancy default-choice display.
;;;_  > icompleting-read (minibuffer-prompt table
(defun icompleting-read (minibuffer-prompt table
                                &optional predicate require-match
                                initial-input minibuffer-default)
  "Like completing read, but with incremental feedback on candidates.

All this really does is act as a wrapper for builtin version, suitably
defining some state vars for active reference by icomplete-process-key"

  (let ((icomplete-inhibit nil))
    (icomplete-prime-minibuffer)
    (builtin-completing-read minibuffer-prompt table
                             predicate require-match initial-input))
  )
;;;_  > iread-buffer-v18 (minibuffer-prompt &optional minibuffer-default)
(defun iread-buffer-v18 (minibuffer-prompt &optional minibuffer-default)
  "Incremental-completion version of read-buffer [for emacs v18].
See description of 'builtin-read-buffer' for operating details.

All this really does is act as a wrapper for builtin version, suitably
defining some state vars for active reference by icomplete-process-key"
  (let ((icomplete-inhibit nil))
    (icomplete-prime-minibuffer)
    (setq target (builtin-read-buffer minibuffer-prompt
                                      (if (not icomplete-dynamic-default)
                                          minibuffer-default)))
    )
  )
;;;_  > iread-buffer-v19 (minibuffer-prompt &optional minibuffer-default strict)
(defun iread-buffer-v19 (minibuffer-prompt &optional minibuffer-default strict)
  "Incremental-completion version of read-buffer [for emacs v19].
See description of 'builtin-read-buffer' for operating details.

All this really does is act as a wrapper for builtin version, suitably
defining some state vars for active reference by icomplete-process-key"
  (let ((icomplete-inhibit nil))
    (icomplete-prime-minibuffer)
    (setq target (builtin-read-buffer minibuffer-prompt
                                      (if (not icomplete-dynamic-default)
                                          minibuffer-default)
                                      strict))
    )
  )
;;;_  - iread-buffer - select v18 or v19, depending on (emacs-version)
(if (string-match "Emacs 18" (emacs-version))
    (fset 'iread-buffer (symbol-function 'iread-buffer-v18))
  (fset 'iread-buffer (symbol-function 'iread-buffer-v19)))
;;;_  > iswitch-to-buffer (&optional target norecord)
(defun iswitch-to-buffer (&optional target norecord)
  "An incremental-completion version of switch-to-buffer.  See description
of 'builtin-switch-to-buffer' for operating details.

All this really does is act as a wrapper for builtin version, suitably
defining some state vars for active reference by icomplete-process-key"
  (interactive)
  (builtin-switch-to-buffer
   (if (not (interactive-p))
       target
     (read-buffer "Switch to buffer: "
                  (buffer-name (other-buffer (current-buffer)))))
   norecord)
  )
;;;_  > iswitch-to-buffer-other-window (&optional target)
(defun iswitch-to-buffer-other-window (&optional target)
  "An incremental-completion version of switch-to-buffer-other-window.
See description of 'builtin-switch-to-buffer-other-window' for operating
details.

All this really does is act as a wrapper for builtin version, suitably
defining some state vars for active reference by icomplete-process-key"
  (interactive)
  (builtin-switch-to-buffer-other-window
   (if (not (interactive-p))
       target
     (read-buffer "Switch to buffer other window: "
                  (buffer-name (other-buffer (current-buffer))))))
  )
;;;_  > ikill-buffer (&optional target)
(defun ikill-buffer (&optional target)
  "An incremental-completion version of kill-buffer.
See description of 'builtin-kill-buffer' for operating details.

All this really does is act as a wrapper for builtin version, suitably
defining some state vars for active reference by icomplete-process-key"
  (interactive)
  (builtin-kill-buffer
   (if (not (interactive-p))
       target
     (read-buffer "Kill buffer: "
                  (buffer-name (current-buffer)))))
  )
;;;_  > idescribe-function (&optional function-symbol)
(defun idescribe-function (&optional function-symbol)
  "An incremental-completion version of describe-function.
See description of 'builtin-describe-function' for operating details.

All this really does is act as a wrapper for builtin version, suitably
defining some state vars for active reference by icomplete-process-key"
  (interactive)
  (if (not (interactive-p))
      (builtin-describe-function function-symbol)
    (let ((minibuffer-prompt "Describe function: "))
      (icomplete-prime-minibuffer)
      (call-interactively 'builtin-describe-function)))
  )
;;;_  > iexecute-extended-command (&optional command prefix-argument)
(defun iexecute-extended-command (&optional command prefix-argument)
  "An incremental-completion version of execute-extended-command.
See description of 'builtin-execute-extended-command' for operating
details.

All this really does is act as a wrapper for builtin version, suitably
defining some state vars for active reference by icomplete-process-key"
  (interactive)
  (if (not (interactive-p))
      (builtin-execute-extended-command prefix-argument)
    (let ((minibuffer-prompt "Esc x "))
      (icomplete-prime-minibuffer)
      (call-interactively 'builtin-execute-extended-command)))
  )
;;;_  > iset-variable (&optional variable value)
(defun iset-variable (&optional variable value)
  "An incremental-completion version of set-variable.
See description of 'builtin-set-variable' for operating details.

All this really does is act as a wrapper for builtin verision, suitably
defining some state vars for active reference by icomplete-process-key"
  (interactive)
  (if (not (interactive-p))
      (builtin-set-variable variable value)
    (let ((minibuffer-prompt "Set variable: "))
      (icomplete-prime-minibuffer)
      (call-interactively 'builtin-set-variable)))
  )

;;;_ + Function overload mechanisms:
;;;_  - icompletion-do-overload (func)
(defun icompletion-do-overload (func)
  (let ((builtin-name (intern-soft (concat "builtin-"
                                           (symbol-name func))))
        (i-name (intern-soft (concat "i"
                                     (symbol-name func)))))
    (if (not (fboundp i-name))
        (error "Nonexistant icompletion-stub %s." func))
    (if (not (fboundp builtin-name))
        (fset builtin-name (symbol-function func)))
    (fset func (symbol-function i-name))
    )
  )
;;;_  - icompletion-overloads (&optional revoke)
(defun icompletion-overloads (&optional revoke)
  "Overload several builting completing functions with stubs that will
invoke the original functions, but with advertised parameters that are
used by the incremental completing-read (icompleting-read) function.

This is only necessary for builtin functions, since even byte-compiled
functions are open coded to most other functions."
  (interactive)
  (let ((func-list '(completing-read
                     read-buffer
                     switch-to-buffer
                     switch-to-buffer-other-window
                     kill-buffer
                     describe-function
                     execute-extended-command
                     set-variable)))
    (while func-list
      (icompletion-do-overload (car func-list))
      (setq func-list (cdr func-list)))
    ))
(if icomplete-overload
    (icompletion-overloads))

;;;_* Local emacs vars.
'(
Local variables:
eval: (save-excursion
        (if (not (condition-case err (outline-mode t)
                   (wrong-number-of-arguments nil)))
            (progn
              (message
               "Allout outline-mode not loaded, not adjusting buffer exposure")
              (sit-for 1))
          (message "Adjusting '%s' visibility" (buffer-name))
          (outline-lead-with-comment-string ";;;_")
          (goto-char 0)
          (outline-exposure -1 0)))
End:)
