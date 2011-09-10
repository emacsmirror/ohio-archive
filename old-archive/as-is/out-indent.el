;Date: Tue, 13 Jun 89 20:14:02 EDT
;From: bard@theory.lcs.mit.edu
;To: unix-emacs@BBN.COM
;Subject: out-indent: outline mode enhancement
;
;
;I write heavily-indented outlines that look like this:
;
;* Wombats
; ** From Hell
; ** From the Moons of Neptune
;   *** Highly Insulated
;   *** Methane-breathing
; ** From Australia
;   *** Reported in science fiction stories
;   *** Real
;
;and so I wrote a few commands for inserting the heading points more
;automatically: e.g., c-c c-c gives you another heading point at the current
;depth, c-c c-e gives you one further in, and so on.
;
;-- Bard the emacs gargoyle
;
;------------------------ cut here ------------------------

;; out-indent.el -- indented outline-mode 
;; Copyright (C) Bard Bloom, June 1989
;; bard@theory.lcs.mit.edu

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

;; WHAT THIS DOES:
;; This lets you write outlines which are textually as well as logically
;; indented.   It is best suited to writing outlines and things like that,
;; which is one of my major uses of outline-mode.
;;
;; For example, you can write:
;;
;; * Heading
;;  ** Subheading-1
;;  ** Subheading-2
;;   *** Sub-subheading
;;  ** Subheading-3
;;
;; OK. You could do this before, too, but you had to put in all those 
;; spaces and stars by hand.  Now you can put them in by emacs commands,
;; and it even gets the spaces right.
;;
;;
;; Here's what you do.  
;; Have your outline-mode-hook call set-outline-levels 
;;      (or use the one I have provided).
;;      The outline-levels is a list of the headings you want.
;;      It is actually an association-list, with elements of the form:
;;         (level  outline-string)
;;      For example,
;;         ((1  "*")
;;          (2  " **")
;;          (3  "  ***")
;;          (1  ">"))
;;      This will use 1-3 stars for depth 1-3 points; it will recognize a ">" 
;;      but not insert it.  (Note: outline-regexp is built automatically from 
;;      this alist; you shouldn't set it yourself.  The things are
;;      regexp-quoted, so don't worry about using *'s.)
;;      
;;
;; The commands for making new outline points:
;; c-c c-c: start a new outline point on the next line, 
;;          which is (by default) at the same level as this one.
;;          If you're at a **, this will make a new **.
;;          c-u c-c c-c means make a top-level heading.
;;          c-u c-u c-c c-c means make a second-level heading.
;;          numeric argument makes a heading of that level.
;; c-c c-d: Start a heading which is one (or ARG) levels further out.
;;          Go from "**" to "*"
;; c-c c-e: Start a heading which is one (or ARG) levels deeper.
;;          Go from "**" to "***"
;;
;; This works quite well with Martin Neitzel's gin-mode, a gadget which guesses
;; the proper indentation for the current line.  The variable 
;; gin-left-hang-indent-re should have have something like
;;     \\*+\\s +     
;; (with double-backslashes for use with strings)
;; so that each point fills in a block nicely.
;;
;; I do not consider this perfected yet.  Comments and improvements 
;; to bard@theory.lcs.mit.edu.

(provide 'out-indent)

(defun set-outline-levels (level-list)
  "Sets outline-levels. The argument LEVEL-LIST should be a list looking
like:
  (
   (1 \"*\")
   (2 \" **\")
   (3 \"  ***\"))
The numbers tell what level the outline entry is at.  The string is the 
string which will start entries at that level: things at level two (here) 
will start with a space and two stars."
  (setq outline-levels level-list)
  (setq outline-level-regexp (regexp-quote "*"))
  (dolist (i outline-levels)
    (setq outline-level-regexp
          (concat outline-level-regexp "\\|" (regexp-quote (cadr i))))))

(set-outline-levels
 '((1 "*")
   (2 "  **")
   (3 "    ***")
   (4 "      ****")
   (5 "        *****")
   (6 "          ******")
   (7 "            *******")))


(defun outline-new-heading (arg)
  "Start a new outline point on the next line.
Default level is the same as the last one.  
numeric ARG sets the level of the heading.
c-u means level one,
c-u c-u means level two."
  (interactive "P")
  (outline-maybe-newline)
  (let ((depth
         (cond
          ((null arg)
           (outline-previous-depth))
          ((equal arg '(4)) 1)
          ((equal arg '(16)) 2)
          ((integerp arg)
           arg)
          (t
           (error "huh?")))))
    (insert (cadr (assq depth outline-levels)) " ")))

(defun outline-previous-depth ()
  (catch 'outline-depth-found
    (save-excursion
      (while (not (bobp))
        (previous-line 1)
        (beginning-of-line 1)
        (when (looking-at outline-level-regexp)
          (dolist (l outline-levels)
            (when (looking-at (regexp-quote (cadr l)))
              (throw 'outline-depth-found (car l)))))
      ))
    ;; At this point, we're lost -- assume depth 1.
    (throw 'outline-depth-found 1)
    ))

(define-key outline-mode-map "\C-c\C-c" 'outline-new-heading)
(define-key outline-mode-map "\C-c\C-d" 'outline-new-outer-heading)
(define-key outline-mode-map "\C-c\C-e" 'outline-new-inner-heading)

(defun outline-new-outer-heading (arg)
  "Start a new outline point on the next line, ARG levels farther out."
  (interactive "p")
  (outline-maybe-newline)
  (insert (cadr (assq (- (outline-previous-depth) arg)
                      outline-levels)) " "))

(defun outline-new-inner-heading (arg)
  "Start a new outline point on the next line, ARG levels deeper."
  (interactive "p")
  (outline-maybe-newline)
  (insert (cadr (assq (+ (outline-previous-depth) arg)
                      outline-levels)) " "))

(defun outline-maybe-newline ()
  (let ((shouldnt-newline
         (save-excursion
           (beginning-of-line 1)
           (looking-at "^[ \t]*$"))))
    (cond
     (shouldnt-newline
      (delete-region
       (save-excursion (beginning-of-line 1) (point))
       (save-excursion (end-of-line 1) (point))))
     (t (newline)))))
