;From ark1!uakari.primate.wisc.edu!samsung!uunet!mailrus!cornell!bard Tue Mar 27 09:11:27 1990
;Article 1635 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!samsung!uunet!mailrus!cornell!bard
;>From bard@brigid.cs.cornell.edu (Bard Bloom)
;Newsgroups: comp.emacs
;Subject: minibuffer-memory
;Message-ID: <39027@cornell.UUCP>
;Date: 24 Mar 90 18:24:47 GMT
;References: <120@cvbnetPrime.COM> <640015@hpcvca.CV.HP.COM>
;Sender: nobody@cornell.UUCP
;Reply-To: bard@cs.cornell.edu (Bard Bloom)
;Organization: Cornell Univ. CS Dept, Ithaca NY
;Lines: 175
;
;Various people have been asking for a package which remembers previous
;strings that have been typed in the minibuffer.  Here's mine.  It's a bit
;stale (I wrote it a couple years ago, intending to polish it some before 
;I distributed it, and I never did.) 
;
;Bug reports and enhancements welcome.  In particular, I'd like a better way
;of changing commands in all minibuffer keymaps. Flames will be responded to
;with a large chunk of m-x flame (highly enhanced) output.
;
;Bard Bloom
;bard@cs.cornell.edu
;Asst. Professor
;Department of Computer Science
;Cornell University
;Ithaca, NY 14853
;607/255-9211

;; minibuf-mem.el -- memory for most minibuffer commands
;; Copyright (C) Bard Bloom, March 1990

;; This file is not yet part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; This gives GNU Emacs a simple retyping mechanism.  If you enter a string
;; in a minibuffer command (see caveats below), it will remember it in a list.
;; In minibuffers, the commands c-p and c-n move up and down in that list.
;; (The commands m-p and m-n are bound to previous-line and next-line.)
;; 
;; If you type the same string repeatedly, the minibuffer memory mechanism
;; does not remember it.  That is, if the last three strings you entered
;; were A, B, and C, and you use the memory mechanism to call up the B and use it,
;; the last three strings remembered are still A, B, and C (rather than A, B, C, B).
;; The variable minibuffer-search-depth controls this behavior; it is initially
;; 4, saying that it doesn't duplicate the last four strings.
;;
;; This works by rebinding some keys.  There are a lot of minibuffer maps, and
;; we have to rebind keys in all of them.  They are listed in the 
;; variable known-minibuffers (which is a list of keymaps).   There are
;; functions define-in-minibuffers and substitute-in-minibuffers, which 
;; allow redefinition of commands in all minibuffers at once.
;;
;; The list of remembered strings grows without bounds.  I've never had a problem with
;; it getting too long.   The strings you type in minibuffers are generally pretty
;; short.
;;


(require 'cl)

(defvar minibuffer-strings '()
  "A list of strings that have been entered into minibuffers.
For minibuffer-previous-string, minibuffer-next-string, and friends.")

(defvar known-minibuffers
  (list
    minibuffer-local-completion-map
    minibuffer-local-map
    minibuffer-local-must-match-map
    minibuffer-local-ns-map
;    repeat-complex-command-map   ; seems not to work well
    )
  "List of things I know to be minibuffer maps.  Elements must be keymaps.")

(defun define-in-minibuffers (key value)
  (mapcar (function (lambda (map) (define-key map key value)))
        known-minibuffers))

(defun substitute-in-minibuffers (olddef newdef)
  "Substitutes OLDDEF to NEWDEF to in all the minibuffer maps I can 
think of."
  (mapcar
   (function (lambda (map)
     (substitute-key-definition olddef newdef map)))
   known-minibuffers))

(defvar minibuffer-depth -1
  "(nth minibuffer-depth minibuffer-strings) returns the string to 
insert next.  Setting it to -1 means that the next minibuffer-previous-string
returns the most recent string; that function adds 1.")

(defvar minibuffer-search-depth 4
  "*Don't save a string if it is one of the first N saved strings.")

(defun limited-member (s l &optional n)
  "Return true iff S is an element of L, but one of the first N if
N is present."
  (let ((answer nil) (finished nil))
  (while (and l (> n 0) (not finished))
    (setq n (1- n))
    (cond
     ((equal s (car l))
      (setq answer t finished t))
     (t
      (setq l (cdr l)))))
  answer))

(defun minibuffer-save-string ()
  "Prepend the contents of the current buffer -- usually the minibuffer --
onto the list minibuffer-strings, for later recall.  Zeros the counter 
minibuffer-depth as well."
  (setq minibuffer-depth -1)
  (let ((s (buffer-substring (point-min) (point-max))))
    (when
     (and (not (equal s ""))
          (not (limited-member s minibuffer-strings
                               minibuffer-search-depth)))
     (setq minibuffer-strings (cons s minibuffer-strings))
     )))

(defun exit-minibuffer-save ()
  "Save the contents of the minibuffer, and exit"
  (interactive)
  (minibuffer-save-string)
  (exit-minibuffer))


(defvar minibuffer-original "" 
  "What was in this minibuffer before use of history.")


(defun minibuffer-previous-string (n)
  "Replaces the contents of the current buffer (usually the minibuffer)
by the Nth previous string in minibuffer-strings."
  (interactive "p")
  (cond
   ((<= minibuffer-depth -1)
    (setq minibuffer-original (buffer-string))
    (setq minibuffer-no-history nil)
    )) 
  (setq minibuffer-depth
        (min (length minibuffer-strings)
             (max -1 (+ n minibuffer-depth))))
  (undo-boundary)
  (erase-buffer)
  (cond
   ((<= minibuffer-depth -1)
    (insert minibuffer-original))
   (t 
    (insert (or (nth minibuffer-depth minibuffer-strings) "")))))


(defun minibuffer-next-string (n)
  "Replaces the contents of the current buffer (usually the minibuffer)
by the N'th next string in minibuffer-strings."
   (interactive "p")
   (minibuffer-previous-string (- n)))

(defun get-from-minibuffer-history (n)
  "RTFM.  (Read The Minibuffer, and insert what you read into the real buffer)."
  (interactive "p")
  (setq minibuffer-depth (- n 1))
  (let ((r (read-from-minibuffer "R.T.F.M:"
                (nth minibuffer-depth minibuffer-strings)
                minibuffer-local-map)))
  (insert r)))

(define-in-minibuffers "\C-p" 'minibuffer-previous-string)
(define-in-minibuffers "\ep" 'previous-line)
(define-in-minibuffers "\C-n" 'minibuffer-next-string)
(define-in-minibuffers "\en" 'next-line)
(substitute-in-minibuffers 'exit-minibuffer 'exit-minibuffer-save)


