;From uakari!indri!ames!apple!rutgers!aramis.rutgers.edu!athos.rutgers.edu!mende Tue Jul 18 05:54:12 PDT 1989
;Article 69 of comp.emacs:
;Path: ark1!uakari!indri!ames!apple!rutgers!aramis.rutgers.edu!athos.rutgers.edu!mende
;>From: mende@athos.rutgers.edu (Bob Mende)
;Newsgroups: gnu.emacs,comp.emacs,alt.sex
;Subject: purity.el (part 1)
;Message-ID: <Jul.17.23.59.43.1989.20914@athos.rutgers.edu>
;Date: 18 Jul 89 04:00:08 GMT
;Organization: Rutgers Univ., New Brunswick, N.J.
;Lines: 603
;Xref: ark1 gnu.emacs:44 comp.emacs:69
;
;Since I have had over 100 requests for this, I am posting it.... enjoy.
;please replace the following three characters
;
;<DEL> with a real delete
;<CTRL-C> with a real ctrl-c
;<CTRL-S> with a real ctrl-s
;
;;
;;  Purity.el    Emacs lisp program to administer the purity test.
;;               Robert Mende (mende@aramis.rutgers.edu)
;;               5/5/89
;;
;; This file is not offically part of GNU Emacs, but can be if FSF wishes
;; it to be so.  Distributed under the GNU copyleft.

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

(setq purity-test-extra-buffer "*Hit Space Bar To Continue*")

(defvar purity-test-save-file
  (concat (getenv "HOME") "/.purity-test.save.el")
  "*file to save purity test saves in")

(defvar purity-test-mode-map (make-sparse-keymap)
  "the keymap used while adminstering the purity test")

(defconst purity-test-backup-size 20
  "*Number of questions that can be backed up")

(defun purity-test-hold-question (question answer section section-text)
  (cond ((>= (length purity-test-backup-ring) 
	    purity-test-backup-size)
	 (setcdr (nthcdr (- purity-test-backup-size 2)
			 purity-test-backup-ring)
		 nil)))
  (setq purity-test-backup-ring (cons (list question answer 
					    section section-text)
				      purity-test-backup-ring))
  purity-test-backup-ring)

(defun purity-test-backup ()
  "Backup up one question (or many times)"
;; this is fun!!!  (pseudo code)
;pop off first element
;is it in the same section
;    yes:
;	cons question onto section-questions
;    no:
;	cons section-text then section name to section question
;	then cons section question onto questions
;	then make a install new section name/text
;	create new section-questions with this question
;was this a yes answer
;    yes:
;	subtract one from the score
;call next question
;;
  (interactive)
  (cond 
   ((not (consp purity-test-backup-ring))
    (message "No more backup information available")
    (ding)
    (sit-for 1))
   (t
    (let ((question-text (car (car purity-test-backup-ring)))
	  (question-answer (car (cdr (car purity-test-backup-ring))))
	  (section-name (car (cdr (cdr (car purity-test-backup-ring)))))
	  (section-text (car (cdr (cdr (cdr (car purity-test-backup-ring)))))))
      (setq purity-test-backup-ring (cdr purity-test-backup-ring))
      (setq purity-test-section-questions
	    (cons purity-test-questions-text purity-test-section-questions))
      (cond 
       ((string-equal section-name purity-test-section-name)
	(setq purity-test-section-questions 
	      (cons question-text purity-test-section-questions)))
       (t
	(message (concat "backing into " section-name "."))
	(sit-for 1)
	(message "")
	(setq purity-test-section-questions 
	      (cons purity-test-section-text purity-test-section-questions))
	(setq purity-test-section-questions 
	      (cons purity-test-section-name purity-test-section-questions))
	(setq purity-test-questions
	      (cons purity-test-section-questions purity-test-questions))
	(setq purity-test-section (1- purity-test-section))
	(setq purity-test-section-name section-name)
	(setq purity-test-section-text section-text)
	(setq purity-test-section-questions 
	      (list question-text))))
      (setq purity-test-question (- purity-test-question 2))
      (setq purity-test-question-text (prin1-to-string purity-test-question))
      (cond (question-answer
	     (setq purity-test-score (1- purity-test-score))))
      (purity-test-next-question)))))

(defun purity-test-yes ()
  "I have done this"
  (interactive)
  (setq purity-test-score (1+ purity-test-score))
  (purity-test-hold-question purity-test-questions-text
			     t
			     purity-test-section-name
			     purity-test-section-text)
  (purity-test-next-question))

(defun purity-test-no ()
  "I have not done this"
  (interactive)
  (purity-test-hold-question purity-test-questions-text
			     nil
			     purity-test-section-name
			     purity-test-section-text)
  (purity-test-next-question))

(defun purity-test-display (header question)
  (setq buffer-read-only nil)
  (erase-buffer)
  (goto-char 0)
  (if (not (null header))
      (insert header "

"))
  (insert question)
  (goto-char 0)
  (setq buffer-read-only t))

(defun purity-test-pop-up (header question)
  (save-excursion
    (save-restriction
      (switch-to-buffer (get-buffer-create purity-test-extra-buffer))
      (purity-test-display header question)
      (view-buffer purity-test-extra-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq buffer-read-only t)
      (discard-input))))

(defun purity-test-next-question ()
  (setq purity-test-question (1+ purity-test-question))
  (setq purity-test-question-text (prin1-to-string purity-test-question))
  (cond ((and (null purity-test-section-questions)
	      (null purity-test-questions))
	 (setq purity-test-question (1- purity-test-question))
	 (setq purity-test-question-text (prin1-to-string purity-test-question))
	 (purity-test-display-score)
	 (purity-test-done))
	(t
	 (cond ((null purity-test-section-questions)
		(setq purity-test-section (1+ purity-test-section))
		(setq purity-test-section-questions (car purity-test-questions))
		(setq purity-test-section-name (car purity-test-section-questions))
		(setq purity-test-section-questions (cdr purity-test-section-questions))
		(setq purity-test-section-text (car purity-test-section-questions))
		(setq purity-test-section-questions (cdr purity-test-section-questions))
		(setq purity-test-questions (cdr purity-test-questions))
		(purity-test-describe-section)
		(discard-input)))
	 (setq purity-test-questions-text (car purity-test-section-questions))

	 (purity-test-display "Have you ever:" purity-test-questions-text)
	 (setq purity-test-section-questions (cdr purity-test-section-questions)))))

(defun purity-test-describe-section ()
  (interactive)
  (purity-test-pop-up (concat "Section "
			      (prin1-to-string purity-test-section)
			      "    "
			      purity-test-section-name)
		      purity-test-section-text))


(defun purity-test-quit ()
  "quit purity test with option to save"
  (interactive)
  (cond ((y-or-n-p "Are you sure you want to quit the purity test? ")
	 (cond ((y-or-n-p "Save current status of test? ")
		(purity-test-save)))
	 (setq purity-test-question (1- purity-test-question))
	 (setq purity-test-question-text (prin1-to-string purity-test-question))
	 (purity-test-display-score)
	 (purity-test-done)))
  (message ""))

(defun purity-test-done ()
  (fundamental-mode)
  (setq buffer-read-only t)
  (kill-buffer purity-test-extra-buffer))

(defun purity-test-current-score ()
  (interactive)
  (save-excursion
    (save-restriction
      (setq purity-test-question (1- purity-test-question))
      (setq purity-test-question-text (prin1-to-string purity-test-question))
      (switch-to-buffer (get-buffer-create purity-test-extra-buffer))
      (purity-test-display-score)
      (view-buffer purity-test-extra-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq buffer-read-only t)
      (setq purity-test-question (1+ purity-test-question))
      (setq purity-test-question-text (prin1-to-string purity-test-question))
      (discard-input))))

(defun purity-test-display-score ()
  (setq buffer-read-only nil)
  (erase-buffer)
  (goto-char 0)
  (insert "You answered yes to ")
  (insert (prin1-to-string purity-test-score))
  (insert " questions out of ")
  (insert purity-test-question-text)
  (insert".

This is a score of: ")
  (cond ((zerop purity-test-question)
	 (insert "---"))
	((= purity-test-score purity-test-question)
	 (insert "0%"))
	(t
	 (insert (prin1-to-string (/ (* (- purity-test-question
					   purity-test-score)
					10000)
				     purity-test-question)))
	 (backward-char 2)
	 (insert ".")
	 (forward-char 2)
	 (insert "%")))
  (goto-char 0)
  (setq buffer-read-only t))

(defun purity-test-save ()
  "Save current status for taking purity test onto disk."
  (interactive)
  (set-buffer (create-file-buffer purity-test-save-file))
  (erase-buffer)
  (insert "(setq purity-test-SAVE-versionID \"" 
	  purity-test-versionID
	  "\")
")
  (insert "(setq purity-test-SAVE-question "
	  purity-test-question-text
	  ")
")
  (insert "(setq purity-test-SAVE-score "
	  (prin1-to-string purity-test-score)
	  ")
")
  (cond ((file-exists-p purity-test-save-file)
	 (set-file-modes purity-test-save-file 128)
	 (delete-file purity-test-save-file)))
  (write-file purity-test-save-file)
  (set-file-modes purity-test-save-file 256) ;; -r--------
  (kill-buffer (buffer-name))
  (set-buffer "*Purity Test*"))

(defun purity-test-restore ()
  "restore a saved purity test session"
  (interactive)
  (cond ((not (file-exists-p purity-test-save-file))
	 (purity-test-pop-up "Error restoring save file"
			     (concat purity-test-save-file
				     " not found.")))
	(t
	 (load-file purity-test-save-file)
	 (cond ((not (string-equal purity-test-versionID
				   purity-test-SAVE-versionID))
		(purity-test-pop-up "Error restoring save file"
				    (concat "The saved file is for questions identified as \""
					    purity-test-SAVE-versionID
					    "\"
while the questions of this test are \""
					    purity-test-versionID
					    ".")))
	       (t
		(message "restoring ... ")
		(cond ((purity-test-skip-to purity-test-SAVE-question)
		       (setq purity-test-question (1- purity-test-question))
		       (setq purity-test-question-text (prin1-to-string purity-test-question))
		       (setq purity-test-score purity-test-SAVE-score)
		       (set-file-modes purity-test-save-file 128)
		       (delete-file purity-test-save-file)
		       (message "restoring ... done")
		       (purity-test-next-question))
		      (t
		       (purity-test-pop-up "Error restoring save file"
					   (concat "Could not skip to question "
						   (prin1-to-sting purity-test-SAVE-question))))))))))
		       

(defun purity-test-skip-to (question-goal)
  (let ((purity-test-R-section 0)
	(purity-test-R-section-name "")
	(purity-test-R-section-text "")
	(purity-test-R-section-questions ())
	(purity-test-R-question 1)
	(purity-test-R-questions-text "")
	(purity-test-R-questions (cdr (purity-test-init-questions)))
	(cant-find nil))
    (while (and (< purity-test-R-question question-goal)
		(not cant-find))
      (setq purity-test-R-question (1+ purity-test-R-question))
      (cond ((and (null purity-test-R-section-questions)
		  (null purity-test-R-questions))
	     (setq cant-find t))
	    (t
	     (cond ((null purity-test-R-section-questions)
		    (setq purity-test-R-section (1+ purity-test-R-section))
		    (setq purity-test-R-section-questions (car purity-test-R-questions))
		    (setq purity-test-R-section-name (car purity-test-R-section-questions))
		    (setq purity-test-R-section-questions (cdr purity-test-R-section-questions))
		    (setq purity-test-R-section-text (car purity-test-R-section-questions))
		    (setq purity-test-R-section-questions (cdr purity-test-R-section-questions))
		    (setq purity-test-R-questions (cdr purity-test-R-questions))))))
      (setq purity-test-R-questions-text (car purity-test-R-section-questions))
      (setq purity-test-R-section-questions (cdr purity-test-R-section-questions)))
    (cond (cant-find
	   nil)
	  (t
	   (setq purity-test-section purity-test-R-section)
	   (setq purity-test-section-name purity-test-R-section-name)
	   (setq purity-test-section-text purity-test-R-section-text)
	   (setq purity-test-section-questions purity-test-R-section-questions)
	   (setq purity-test-question purity-test-R-question)
	   (setq purity-test-questions-text purity-test-R-questions-text)
	   (setq purity-test-questions purity-test-R-questions)
	   t))))

(defun purity-test-mode ()
  "Major mode to administer the purity test.    Keys are:
\\{purity-test-mode-map}"
  (use-local-map purity-test-mode-map)
  (define-key purity-test-mode-map " " 'purity-test-yes)
  (define-key purity-test-mode-map "?" 'describe-mode)
  (define-key purity-test-mode-map "S" 'purity-test-save)
  (define-key purity-test-mode-map "R" 'purity-test-restore)
  (define-key purity-test-mode-map "b" 'purity-test-backup)
  (define-key purity-test-mode-map "c" 'purity-test-current-score)
  (define-key purity-test-mode-map "d" 'purity-test-describe-section)
  (define-key purity-test-mode-map "e" 'purity-test-definitions)
  (define-key purity-test-mode-map "h" 'purity-test-history)
  (define-key purity-test-mode-map "i" 'purity-test-instructions)
  (define-key purity-test-mode-map "l" 'purity-test-liability)
  (define-key purity-test-mode-map "n" 'purity-test-no)
  (define-key purity-test-mode-map "s" 'purity-test-scoring)
  (define-key purity-test-mode-map "t" 'purity-test-title)
  (define-key purity-test-mode-map "q" 'purity-test-quit)
  (define-key purity-test-mode-map "w" 'purity-test-warranty)
  (define-key purity-test-mode-map "y" 'purity-test-yes)
  (define-key purity-test-mode-map "" 'purity-test-no)
  (define-key purity-test-mode-map "\C-x<CTRL-C>" 'purity-test-quit)
  (define-key purity-test-mode-map "\C-x<CTRL-S>" 'purity-test-save)
  (setq mode-name "Purity Test")
  (setq major-mode 'purity-test-mode)
  (setq mode-line-format '(""
			   mode-name
			   " question "
			   purity-test-question-text
			   "    "
			   purity-test-section-name)))

(defun purity-test ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*Purity Test*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (goto-char 0)
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq purity-test-backup-ring ())
  (setq purity-test-score 0)
  (setq purity-test-section 0)
  (setq purity-test-section-name "")
  (setq purity-test-section-text "")
  (setq purity-test-section-questions ())
  (setq purity-test-question 0)
  (setq purity-test-question-text "")
  (setq purity-test-questions-text "")
  (setq purity-test-questions (purity-test-init-questions))
;;  (setq purity-test-questions (purity-test-test-questions))
  (setq purity-test-versionID (car purity-test-questions))
  (setq purity-test-questions (cdr purity-test-questions))
  (purity-test-mode)
  (purity-test-title)
  (purity-test-instructions)
  (cond ((file-exists-p purity-test-save-file)
	 (cond ((y-or-n-p "You have a saved test in progress.  Resume it? ")
		(purity-test-restore))
	       (t
		(purity-test-next-question))))
	(t
	 (purity-test-next-question))))


(defun purity-test-definitions ()
  (interactive)
  (purity-test-pop-up "Definitions"
"All questions in this test pertain to events that have happened to you
subsequent to your weaning and babyhood/infancy.  Anything that may have
happened before that time is considered not standing and void.

The term mutual masturbation refers to someone masturbating you AND/OR you
masturbating someone else, not exclusively both at the same time.

We would also like to define having sex in the homosexual case; homosexual sex
has occurred when both partners are of the same sex and one of the partners has
an orgasm while there is some contact between the genitals of both partners.

We would now like to bring to your attention that there is no passing nor
failing score.  Therefore, one really shouldn't worry too much about getting a
high score...  even if you do get giggled at for the rest of your life.
                       --- ALL TECHNICALITIES COUNT ---
"))

(defun purity-test-history ()
  (interactive)
  (purity-test-pop-up "Purity Test Genesis/History:"
"Version 1 (100) Created at MIT's Baker House.  Two parallel versions; one for
                male, and one for female.  Not much is known about this
                version.  It was ported to CMU by ps in 1982.
Version 2 (247) Spring 1983 - CMU/jb, pd, kr, ps, ts, mt, et al.
                Expanded to 247 questions.  This marked the beginning of
                the unisex versions. The story goes that they intended it to be
                250 questions, but got tired that night and said  'we'll  think
                of three more tomorrow', and tomorrow never got there.
Version 3.3C.1 (400) on 05-Dec-1984
                First  formal  release  general of this test, version 3.xx. All
                former versions were short-lived and tended to be bug-ridden.
                Does not discriminate against gays or bi's. Good correspondence
                of  scores (especially in the higher score ranges) between this
                version and version 2. Added Genesis/History section.
Version 3.4 (400) on 29-Jan-1985
                Internal version;  never  released.  Source  code  accidentally
                destroyed,  much  to  the  consternation of one of the authors.
                Cleaned up many bugs. Added sections: Disclaimer of  Liability,
                Instructions for Use, Scoring, and Warranty Information.
Version 3.5 (400) on 10-Apr-1985
                Rebuilt from the 3.3C.1 source and the 3.4 (only surviving
                copy) Xerox X9700 laser printer hardcopy.  Cleaned up same bugs
                in 3.4; wiped out a duplicate question.  Added in verbose
                history section.
Version 3.5A (400) on 13-Apr-1985:  CMU/da, fa, tc, no, dt, sv, rz, et al
                Found  that  we  had  431  questions  instead  of 400.
Version 3.5B (400) on 18-Mar-1986: Yale (Pierson College)/ as
                Intermediate release, with footnotes integrated into main body
                of text and some grammatical errors cleaned up.  Begun in
                Fall, 1985; finished in April for the benefit of a friend at
                MIT (where it all began), who hadn't seen any versions except
                the antique Version 1.
Version 3.5C (400) on 17-Jan-1988: Yale (Pierson College)/mmd (CLARINET@YALEVM)
                Grammatical errors corrected.  Introduction and history
                cleaned up.
Version 4.0 (500) on 23-April-1988: Yale (Silliman College)/dfc, ad, dcg, mlm,
                and Dartmouth (Alpha Theta)/alb.  Original 400-question version
                expanded to 500 questions.
Version 4.1 (500) in 1989 jkm and gwe from Univ. of Kentucky converted version
                4.0 to LaTeX
Version 4.1A (500) on 5-4-1989: Rutgers/rgm.   Version 4.0 converted into GNU
                emacs lisp."))

(defun purity-test-instructions ()
  (interactive)
  (purity-test-pop-up "Instructions for Use:"
"This is a fairly long test consisting of five hundred questions.  It starts out
tame and gets progressively worse (or better, depending on your viewpoint).
There are many ways of going about taking this test.  You can, of course, as
your right, guaranteed by the Constitution, be anti-social and sequester
yourself in your room and take this test all by yourself; however, we feel that
the funnest way to utilize this test is to hold a Purity Test Party.  All you
need is one copy of the test, and a bunch of friends.  (Lots of writing
implements and paper would be useful too.)  The person with the copy of the
test is the test administrator; s/he readsd the questions out loud and
everybody else writes down their answers.  We have no definite rules as to
whether the participants are required to divulge their answers; that is up to
the group to decide.  However, each person's purity score should be made common
knowledge.  (The person with the highest score gets to be giggled at for the
rest of his/her life.)  This works great at parties and lets everybody know
who's easy and who isn't, so you'll know who to go home with.  Don't leave home
without it."))

(defun purity-test-liability ()
  (interactive)
  (purity-test-pop-up "Disclaimer of Liability"
"The user of this test acknowledges that sex is a hazardous sport; that a person
must copulate in control, and use good judgement at all times; that partners'
conditions vary constantly and are greatly affected by weather changes and
previous use; and that dirty sheets, variations in terrain and bed surfaces,
spouses/pimps/managers, forest growth, rocks and debris, clothed obstacles, and
many other natural and man-made obstacles and hazards, including other users
and customers, exist throughout the bedroom area.  Personal managers
(pimps/spouses) and sado-masochistic operations and equipment are constantly in
use and may be hazardous to those not copulating in control.  Impotence,
collisions, and social diseases resulting in injury can happen at any time,
even to those copulating in control with proper sexual equipment.  Inherent
risks are part of the sport and may exist within your partner.  As a condition
of being permitted to use the facilities of your partner, the user of this test
agrees to copulate in control and within the limits of his/her ability, and
further acknowledges and accepts these hazards, dangers, and risks and assumes
the risk of injury or loss to person or damage to property which might result
from use of the partner's facilities.

As a further condition of being permitted to use the facilities of your
partner, the customer understands and agrees that:  
(1) 	In the event of a transfer of use by another or anything else in the
	management's opinion is misconduct, misuse, kinky, impotence, or
	nuisance, this service may be revoked without refund. 
(2)	the partner is the property of the harem and, upon request,
	s/he must be presented to any authorized representative of the 
	pimp/spouse.
(3)	sexual equipment must be visibly displayed at all times when you
	are in any bedroom and when approaching the bed to copulate.  Your
	sexual partner is not transferable; see Theft of Services, V.S.A.,
	sections 2581 and 2582."))

(defun purity-test-scoring ()
  (interactive)
  (purity-test-pop-up "Scoring"
"In this version of the purity test, emacs will do all the math for you.
But, if you had taken this test the old fashion way, you would have to
follow these directions.

Congratulations!  You are now the proud owner of a sheet of paper containing
lots of itty-bitty answers to the Purity Test.  Sworn to excellence of
workmanship, we now give you directions on how to calculate your Purity score.
There are several methods; the calculator method works best.  Also there is the
a la mainframe method.  (A DECsystem-2060 works great as a PC.)

Scoring method:  Count 'yes' answers.
                 Subtract that number from 500.
                 Divide the result by 5.
                 The result is your percentage purity.

The higher the number, the more pure you are; in the same vein, the lower the
score, the more of a sleaze-bag you are.

For your reference, we include calculator directions:
     For people with real calculators (HP):
               <# of NO answers> [ENTER] 5 /
     For people with other (dinky) calculators:
               <# of NO answers> / 5 ="))

(defun purity-test-title ()
  (interactive)
  (purity-test-pop-up
"                            THE UNISEX, OMNISEXUAL
                            P U R I T Y    T E S T"
"                             Version 4.0A (500)
 			    GNU Emacs Lisp Version
                                 5-May-1989

Public domain; no copyright.  All rights wronged, all wrongs reversed.  Up with
going down.  The risen flesh commands:  let there be love.  Murphy's law on
sex:  Love is a matter of chemistry; sex is a matter of physics.  Chaste makes
waste.  Virginity can be cured.

This document was not sponsored by the Department of Defense Advanced Research
Projects Agency, and was not monitored by the Air Force Avionics Laboratory.
The views and conclusions contained in this document should not be interpreted
as representing the official policies, either expressed or implied, of the
Defense Advanced Projects Agency or the US Government.  Neither should it be
interpreted nor inferred that the authors/contributors have actually performed
any of the actions contained herein.

	     Hit '?' For instructions on how to take the test

                       --- ALL TECHNICALITIES COUNT ---
"))

(defun purity-test-warranty ()
  (interactive)
  (purity-test-pop-up "Warranty Information"
"We  hope that you enjoy this test. It does not come with a warranty, nor
does it guarantee that it will get you laid or make you somehow somewhat better
in bed or the haystack.
The makers of this test are not responsible  for  any  liabilities  or  damages
resulting from this test, including but not limited to paternity suits.  Ask
your doctor or pharmacist.
Do not open back panel; no user serviceable parts inside.
Propagate (this test) at will, even without the written permission of the
publisher; just don't edit or change it.  In reproducing this test, the authors
of this test may exercise droit de seigneur over you, your immediate family, or
fiance(e).  You may or may not have additional rights which may vary from state
to state (i.e. inebriated, ecstacy).
Not recommended for children under twelve.  Parental guidance discouraged and
frowned upon.  Pencils, additional paper, and  batteries not included.  Some
assembly may be required.  Does not come with any other figures.

Drive carefully; 90% of the people in the world are caused by accidents."))
;-- 
;
;
;From uakari!indri!ames!apple!rutgers!aramis.rutgers.edu!athos.rutgers.edu!mende Tue Jul 18 05:53:54 PDT 1989
;Article 68 of comp.emacs:
;Path: ark1!uakari!indri!ames!apple!rutgers!aramis.rutgers.edu!athos.rutgers.edu!mende
;>From: mende@athos.rutgers.edu (Bob Mende)
;Newsgroups: gnu.emacs,comp.emacs,alt.sex
;Subject: purity.el (part 2)
;Message-ID: <Jul.17.23.48.37.1989.20841@athos.rutgers.edu>
;Date: 18 Jul 89 03:48:39 GMT
;Organization: Rutgers Univ., New Brunswick, N.J.
;Lines: 839
;Xref: ark1 gnu.emacs:43 comp.emacs:68
;
;here is part two....  the questions...


(defun purity-test-test-questions ()
  '(
    "Purity Test Test Questions"
    ("section one"
     "This is what Section One is about"
     "question one")
    ("section two"
     "This is what Section two is about"
     "question two"
     "question three")
    ))

(defun purity-test-init-questions ()
  "This function returns a list describing the questions to be used.  The
First Element is a string that is the question ID.  All other elements are
lists representing one section of the purity test.  A section list contains
a number of strings.  The first is the name of the section.  The second is
the discription of the section.  The rest are questions.  Make sure that
any questions do not have a double quote in them (use single quotes)."
  '(
    "Version 4.1A 5/6/1989 mende@athos.rutgers.edu"
("Platonic Relations"
"For this section, if you are mostly a:
         - heterosexual, then your partner in deed, often referred to
                         by the word 'someone' or 'partner', is to be someone
                         of the OPPOSITE gender.
         - homosexual, then your partner in deed, often referred to by the
                         word 'someone' or 'partner', is to be someone of
                         YOUR OWN gender.
         - 50-50 confirmed bisexual, then your partner in deed, often
                         referred to by the word 'someone' or 'partner', is to
                         be someone of the OPPOSITE gender."
"kissed a friend or stranger on their hands or their head/neck region
as a friendly gesture?"
"held hands with someone?"
"had a date?"
"had a date past 1 a.m.?"
"dated someone on a regular basis?"
"picked someone up?"
"been picked up?"
"gone steady?"
"slow danced?"
"had the symptoms of Russian fingers (rushin' fingers)?"
"had the symptoms of Roman hands (roamin' hands)?"
"shared a bed, sleeping bag, or sleeping accommodations with someone
without anything steamy happening?"
"given a back or neck rub or massage with no ulterior motive?"
"used tickling as a pick-up, get-to-know-you-better routine?"
"directly asked someone whom you were not going out with and had
never gone out with if they were a virgin?"
"used physical strength, physical power, or any physical prowess, or
prowesslessness as a factor in pick-up, get-to-know-you-better
routine?"
"secretly lusted after someone without that person knowing?"
"dropped subtle hints to someone whom you liked/loved/wanted, hoping
that they would pick up on it?"
"written anonymous 'love letters' to someone (secret admirer, etc.  That
is, dropped some not-so-subtle hints)."
)
("Auto-erotica and Mono-sexualism"
"     Although this section is termed Auto-erotica and mono-sexualism, the
     events herein still count even if you are with someone else at the time.
     It was so named because these activities, like the harp, (and the
     porcelain goddess) are predominantly solo events."
"had an arousing dream?  (Wet dreams and the like.)"
"been sexually aroused?"
"uttered/muttered/yelled/screamed (or in other words verbally
expressed) obscenities?"
"fantasized about your long-term instructor, mentor, or someone who
is superior to you?  (One day skydiving teachers, two hour ski
instructors, and the like do not count.)"
"fantasized about your lawyer, doctor, nurse, psychiatrist or someone
with whom you are having a professional relationship?"
"fantasized about someone you know personally but not closely?"
"fantasized about anyone and masturbated at the same time?"
"read or bought pornographic periodicals?"
"ever had a subscription to pornographic periodicals?"
"read sexually explicit literature?"
"gone skinny dipping alone?"
"made obscene phone calls?"
"phoned up any recorded phone sex numbers? (e.g.  (212)976-2626,
(212)976-2727, (212)976-2828, etc. Please note that these are fifty
cent calls in addition to toll charges and long distance charges.)"
"phoned up any live phone sex numbers?"
"stuffed your bra if you are female, or stuffed your pants if you are
male?"
"shaved your genital pubic hair?"
"shaved your genital pubic hair on a fairly regular basis?"
"colored or bleached your genital pubic hair?"
"shaved or shaped your genital pubic hair in a particular design?
(moons, hearts, diamonds, clovers, etc.)"
"masturbated?"
"masturbated at least five times in one twenty-four hour period?"
"masturbated on a fairly regular basis of no less than once a
fortnight?"
"masturbated where you could have been discovered?  (In a crowd, in
public, city parks, gym/dorm/barrack showers, movie theater, etc.)"
"masturbated out in the wilds or in nature with no nearby
civilization?"
"masturbated to orgasm?"
"masturbated while reading either pornographic or sexually explicit
materials?"
"masturbated while driving a *moving* land vehicle? (Car, RV, truck,
motorcycle, hearse, etc.)"
"masturbated while on the phone?  (No kinky interpretations of 'on the
phone'. This is legit.)"
"masturbated while in a bathroom of the opposite sex?"
"masturbated while watching an R or X-rated show?"
"seen any burlesque show? (Rocky Horror counts)"
"been to a peep show?"
"been to a private showing of a pornographic movie?"
"seen a pornographic movie in a theater?"
"walked around in your room/apartment/house/habitation in the nude?
(Must be a serious walk; five minutes of going around trying to find
your undies after a shower doesn't count.)"
"walked around in a public or semi-public area with a top (shirt,
T-shirt, etc) but no bottoms (pants, shorts, etc)? (Dorm hallways,
lobby areas, etc. are acceptable; however locker rooms, bathrooms
and such places where this behavior is acceptable do not count.)"
"bought blatant sexual objects? (This means that if you buy a bottle
of Coke and you use it as a dildo, it doesn't count.  Think design and
function.)"
"owned any erotic art pieces?  (Like that phallic symbol in the cat
woman scene in 'A Clockwork Orange.')"
"written down your own stories or fantasies for masturbatory purposes?
(Your own custom-made Penthouse Forum [tm])"
"sculpted erotic/obscene artworks in food (bananas, carrots, apples...)?"
"eaten any erotic food items?  (Chocolate tits, banana dicks, etc.)"
"sculpted erotic/obscene artworks in soap, wood, or any other
carvable material that isn't food?"
"made an X- or R-rated snowman or snowwoman?"
"tasted your own orgasmic liquids?"
"inserted your finger into your rectum?"
"used ben-wa balls or anal beads?  (ben-wa balls: a pair of small, usually
metal, balls which are placed in the vagina or anus and are supposed to
feel good as they move around.  Anal beads: a string of beads inserted
into the anus which is supposed to feel good as it is pulled out.)"
"performed oral sex on yourself?  (Yes, this is possible for most males,
and even for some females.)"
"willingly urinated on the garments that you were wearing at the
time? (In other words: piss in your pants; wet yourself; had an
accident.)"
"willingly urinated on any part of your body?"
"willingly defecated on the garments that you were wearing at the
time?  (In other words: shit in your pants; had a serious accident.)"
"willingly defecated on yourself?"
)
("Legislative Misfits and Other Ethical Matters"
"These questions deal actions that are not socially approved of, or are
commonly frowned upon"
"administered a whole Purity Test or are in the process of
administering a whole Purity Test of any version?  (That's where you
are the test administrator in a Purity Test Party. See 'Instructions
for Use' for further information.)(having emacs do it for you does not
count!)"
"taken Purity Tests of any versions more than 5 times?"
"lied on any previous Purity Tests?"
"exaggerated about any sexual experiences?"
"gone to (or escorted someone to) a Planned Parenthood Clinic?  (for an
exam, to obtain birth control pills, diaphragm, etc.)"
"broken your word, promise, or vow?"
"lied to someone at someone else's request?"
"lied about your sexual preference in order to avoid a date?"
"written graffitti?"
"plagiarized?"
"shoplifted?"
"stolen?"
"made out a check that bounced?"
"used someone else's credit card without their knowledge and/or consent?"
"committed breaking and entering?"
"seen a snuff film?"
"read someone else's diary without their knowledge and/or consent?"
"searched someone's room without their knowledge and/or consent?"
"told someone that you loved them when you did not?"
"told someone that you loved them strictly because you wanted to have
sex with them?"
"fantasized about someone else other than your partner while you were
engaged in sex, oral sex, or mutual masturbation?"
"intentionally listened in on other people having sex, oral sex, or
mutual masturbation without their knowing it?"
"used alcohol to lower someone else's inhibitions for the purpose of
sex or oral sex?"
"used drugs other than alcohol to lower someone else's inhibitions for
the purpose of sex or oral sex?"
"drugged someone without their knowledge and/or consent?  (hash brownies
and the like.)"
"forced or coerced someone into having intimate physical relations
with you?"
"had dates with more than one person in the same night while one or
more of the people involved were NOT aware of your actions?"
"gone steady with two or more people at the same time while one or
more of the people involved were NOT aware of what you were doing?"
"gone steady with two or more people at the same time while all of
them WERE aware you were doing it?"
"urinated on someone else intentionally?  (Piss fights!)"
"urinated in cup, bottle, pitcher or any such receptacle that was not
originally designed for such purpose?  Urinalysis incidents don't count."
"urinated anywhere other than a bathroom, outhouse, tree, bush or any
commonly accepted piss hole?  (in the kitchen sink, in the glove
compartment of the family car...)"
"urinated from higher than the fifth floor, or equivalent height,
above ground level, out of a building or off a bridge or any such
permanent structure (e.g. the Grand Canyon)?"
"defecated anywhere other than a bathroom, outhouse or any commonly
accepted shit hole?  (Camping/hiking trips in which bathroom/outhouse
access are nil do not count.)"
"stored any excretia in a refrigerator, oven, or any food
storage/preparation appliance?"
"stored evacuated excretia anywhere else in your room, apartment,
house, habitation, office, etc?"
"entered a bathroom of the opposite sex, unaccompanied by someone of the
opposite sex, while not involved in a search and/or rescue mission?"
"mooned or flashed someone from the front?"
"farted (audibly) in mixed company?"
"streaked, flashed, or otherwise exposed pretty much totally yourself
in public before an informal, unofficial gathering of people?"
"streaked, flashed, or otherwise exposed pretty much totally yourself
in public before a formal (official) gathering of people?  (Such as a
function, stage performance, charity dinner, etc.)"
"been arrested?  (Picture taken and all that wonderful, free stuff.)"
"received money or some favor to have sex, oral sex, or mutual
masturbation with someone?"
"given money or some favor to have sex, oral sex, or mutual
masturbation with someone?"
"thought you might be, or might have caused someone else to be,
unintentionally pregnant?"
"bought a home pregnancy test?"
"bought condoms?"
"borrowed/stolen/taken birth control devices from your or someone else's
parents?  (Condoms, spermicidal foam, diaphragms, chastity belt)"
"had, or given someone, an unwanted pregnancy?"
"lied about being pregnant or about having made someone pregnant?"
"had sex while either you or your partner, but not both, were under the
legal age of consent of the state in which you were having sex?"
"thought you had VD?"
"had VD of any sort (VD, STD's, i.e. the clap, crabs, herpes, etc.)"
"had an AIDS test due to reasonable suspicion or hyperactive imagination?"
"given a sympathy fuck?"
"initiated sex with someone for the sake of sex only?"
"willingly committed incest?"
"committed adultery?  (you need not have been the one who was married
to get this point.)"
"bought lingerie/undergarments of the opposite sex?"
"stolen the underwear of someone you knew, for a purpose other than
that of a practical joke or to just ire the person? (A bunch of guys
invading the women's dorm on a mission of panty raids does not
count.)"
"intentionally taken someone's underwear and didn't know who it
belonged to?"
)
("Drugs"
"These questions deal with both legal and illegal substances, their
effects, and things you have done while under their influence"
"had an alcoholic drink?"
"been intoxicated?"
"thrown up from having drunk too much alcohol?"
"passed out due to having drunk too much alcohol?"
"forgotten events that occurred while you were drunk?"
"smoked tobacco?  (cigarette, pipe, cigar, hookah)"
"chewed tobacco?  (snuff)"
"bought controlled/illicit drugs, or any compound scheduled by the
DEA, in violation of the law?"
"sold or re-sold controlled/illicit substances, or any compound
scheduled by the DEA, in violation of the law?"
"taken stimulants?"
"taken depressants excluding alcohol?"
"inhaled nitrous oxide while not visiting a dentist?"
"inhaled anything containing butyl nitrate?"
"used a commercial aphrodisiac?  (Spanish Fly, Magic Love Drops, powdered
rhino horn, etc.)"
"taken valium?"
"smoked marijuana/sensemilia?"
"smoked marijuana/sensemilia more than four times?"
"eaten marijuana/sensemilia?"
"eaten marijuana or sensemilia more than four times?"
"taken opiate in any form?"
"taken opiate in any form more than twice?"
"used cocaine?"
"used cocaine more than four times?"
"injected any drugs into your body for any other purpose other than
medical?"
"injected any one drug into your body for any purpose other than
medical more than twice?"
"taken Ecstasy/X?"
"taken PCP?"
"taken PCP more than twice?"
"taken d-lysergic acid diethylamide (LSD-25), peyote, or psilocybin?"
"taken LSD, peyote, or psilocybin more than twice?"
"taken LSD more than six times?"
"deliberately injured yourself so as to obtain medication?"
"played any games in which there was a mystery as to who would be
getting drugs and who wouldn't, yet everybody plays in it? (e.g.
'Who's got the pill', where half the cups (of soda) have LSD in them and
the other half don't, and everybody picks a cup and drinks it and wonders
who gets lucky.)"
"gone to class or work while under the influence of drugs?  (alcohol
counts)"
"mixed drugs? (alcohol counts.  Mixed meaning being under the influence of
two or more drugs at once.)"
"sold possessions in order to obtain drugs?"
"had sex while under the influence of nitrous?"
"had an orgasm while under the influence of nitrous? (difficult because of
timing)"
"had sex while under the influence of cocaine?"
"had sex while under the influence of marijuana/sensemilia?"
"had sex while under the influence of Ecstasy/X?"
"had sex while under the influence of LSD, peyote, or psilocybin?"
"had sex while under the influence of drugs with a partner who did not
realize you were on them?"
)
("Non Platonic Relations"
"For this section, it you are mostly a:
         - heterosexual, then your partner in deed, often referred to by
                         the word 'someone' or 'partner', is someone of the
                         OPPOSITE gender.
         - homosexual, then your partner in deed, often referred to by the
                         word 'someone' or 'partner', is to be someone of
                         your OWN gender.
         - 50-50 confirmed bisexual, then your partner in deed, often referred
                         to by the word 'someone' or 'partner', is to be
                         someone of the OPPOSITE gender."
"propositioned someone for necking or petting?
(petting is defined as any caressing, patting, stroking or fondling.)"
"propositioned someone for sex, oral sex, or mutual masturbation?"
"pinched or patted someone else's buttocks?"
"french kissed?"
"been kissed below the neck but not including arms or hands?"
"kissed someone else below the neck but not including arms or hands?"
"necked?"
"petted above the waist?"
"petted below the waist?"
"kissed on the first date?"
"necked on the first date?"
"petted above the waist on the first date?"
"petted below the waist on the first date?"
"given a hickey?"
"received a hickey?"
"worn specific clothes for the purpose of hiding hickeys?  (i.e.,
turtlenecks)"
"fondled or stroked someone else's clothed legs?"
"fondled or stroked someone else's bare legs?"
"fondled or stroked someone else's frontal chest/torso region?"
"had your frontal chest/torso region fondled or stroked?"
"been involved with pelvic thrusting while fully clothed?"
"had your fingers licked or sucked?"
"had your ear or ear region licked, breathed upon, sucked, or
nibbled?"
"licked, breathed onto, sucked, or nibbled an ear or ear region?"
"licked or sucked someone else's finger(s)?"
"fondled someone who was asleep?"
"given a back massage with ulterior motives?"
"given a back massage that led to something steamier?"
"seen someone else completely nude when that person was under good
lighting conditions?"
"been seen completely nude by someone else under good lighting
conditions?"
"been in someone's company while both of you were completely nude?"
"bathed or showered with someone?"
"let someone else wash you while you were perfectly capable of doing
it yourself.  (This means that if you were in the hospital with two
broken arms and the nurse washed you, it doesn't count, no matter how
cute s/he was.)"
"gone skinny dipping in mixed company?"
"been involved with the stroking or fondling of a woman's clothed
breast (as either possessor or fondler of the breast)?"
"been involved with the stroking or fondling of a woman's bare
breast (as either possessor or fondler of the breast)?"
"licked, sucked, or nibbled on someone else's nipple?"
"had your nipple licked, sucked, or nibbled upon?"
"petted, stroked, fondled, or otherwise handled someone else's
covered genitals?"
"had your covered genitals petted, stroked, fondled, or otherwise
handled?"
"petted, stroked, fondled, or otherwise handled someone else's bare
genitals?"
"had your bare genitals petted, stroked, fondled, or otherwise
handled?"
"had an orgasm while petting?"
"given your partner an orgasm while petting?"
"given finger scratch marks?"
"received finger scratch marks?"
"drawn blood by scratching during sex?"
"drawn blood by biting during sex?  (hickeys do not count as having drawn
blood)"
"given or received *scars* from scratches or bites made during sex?"
"performed oral sex?"
"received oral sex?"
"swallowed semen, or licked female liquids off of fingers?"
"done sixty-nine ?"
"performed mutual masturbation?"
"had sex?  (No need for orgasm; penetration counts.)"
"had sex on the first date?"
"had sex without the use of birth control devices?  (Use of the rhythm,
prayer, and hope methods counts as 'without the use of birth control
devices'.)"
"had sex with a virgin?  (Not yourself.)"
"had sex with someone whose name you did not know?  (and *still* don't
know)"
"had sex with someone whose face you never saw?"
"had sex with someone where there was an age difference of more than 20
years?"
"had sex with someone not of your own race?"
"had sex with a religious officiary?  (Priest, nun, mother-superior,
cardinal, pope, deity, etc.)"
)
("Non Primary Choice Relations"
"      This section of the test deals with whether you have done things with
      people whom you may not be altogether comfortable, therefore in this
      section of the test, if you are mostly a:
         - heterosexual, then your partner in deed, often referred to by the
                         word 'someone' or 'partner', is to be someone of your
                         OWN gender.
         - homosexual, then your partner in deed, often referred to by the
                         word 'someone' or 'partner', is someone of the
                         OPPOSITE gender.
         - 50-50 confirmed bisexual, then your partner in deed, often
                         referred to by the word 'someone' or 'partner', is to
                         be someone of your OWN gender."
"held hands or otherwise displayed public affection?"
"kissed someone on the lips?"
"french kissed someone?"
"necked?"
"petted?"
"received manual sex?"
"given manual sex?"
"received oral sex?"
"given oral sex?"
"had sex?"
"had sex with a virgin?  (not yourself.)"
"had sex with someone when there was an age difference of more than 20
years?"
"had anal sex?  (The use of fingers or any phallic objects in this
case would also count.)"
"been involved in fist-fucking?"
"done 69?  (simultaneous oral sex.)"
"propositioned someone for sex, oral sex, or mutual masturbation?"
"yielded willingly to a proposition from someone for sex, oral sex,
or mutual masturbation?"
"had sex with someone whose name you didn't know?  (and still don't
know.)"
"had sex with someone whose face you never saw?"
"been a participant in a who's-physically-better-equipped
verification contest?  (Contestants must be of same gender;
spectators, judges, umpires, and verifiers may be of different
gender.)"
"been a judge in a who's-physically-better-equipped contest?"
"gave money or favors for sex, oral sex, or mutual masturbation?"
"received money or favors for sex, oral sex, or mutual masturbation?"
"fondled someone who was asleep?"
"attempted to seduce someone?"
"allowed yourself to be seduced?"
"had an orgasm while petting?"
"gave your partner an orgasm while petting?"
"had an orgasm at all?"
"had sex, oral sex, or mutual masturbation with more than 10 people?"
"stroked or fondled the clothed legs of someone?"
"stroked or fondled the bare legs of someone?"
"stroked or fondled the clothed chest/torso region of someone?"
"stroked or fondled the bare chest/torso region of someone?"
"stroked, fondled, or otherwise handled the clothed genitals of
someone else?"
"stroked, fondled, or otherwise handled the bare genitals of someone
else?"
"had sex, oral sex, mutual masturbation with someone not of your own race?"
"been in a menage-a-trois of people of the same sex?"
"been involved in group sex, with all participants of the same sex?  (4
or more people, partners must be exchanged.)"
)
("Alternate Choices"
"      For any of the questions in this section, a yes answer is in order if it
      is something that you do as an alternative to other sexual gratifications
      or as an aid and/or in conjunction with other means of sexual
      gratification.  In other words, have you done it in a serious basis?
      Trying it a few times to see what it's like does not count."
"been decidedly heterosexual?"
"been decidedly homosexual?"
"been decidedly bisexual?"
"practiced bestiality (avec les animaux)?"
"practiced transvesticism?"
"practiced sadism ?"
"practiced masochism ?"
"practiced bondage?"
"practiced domination?"
"practiced submission?"
"practiced sodomy (anal intercourse)?"
"practiced cocrophilia (a marked interest in excrement; esp. the use of
feces or filth for sexual excitement)?"
"practiced frotteurism (masturbation by rubbing against another's person
or the need to rub against another stranger)?"
"practiced infantilism (a dependency on the sight or feeling of diapers
or of being diapered; a dependency on being dressed and treated as a
baby)?"
"practiced klismaphilia (a dependency on being given an enema)?"
"practiced necrophilia (copulation with a corpse)?"
"practiced mysophilia (a dependency on something soiled or filthy, such as
sweaty underwear or used menstrual pads)?"
"practiced scoptophilia (a dependency on looking at sexual organs and
watching sexual activity openly, not surreptitiously, as in voyeurism)?"
"practiced urophilia (being dependently responsive to the smell or taste
of urine or the sight and sound of someone urinating)?"
"practiced role-playing (nurse-patient, teacher-student, border guard,
well endowed co-ed, etc.)?"
"owned an underwear collection of underwear not belonging to you?"
"been a foot fetishist to any degree?"
"been a leather fetishist to any degree?"
"been a rubber/latex fetishist to any degree?"
"been a voyeur?"
"been an exhibitionist?"
)
("Group Sexual Relations"
"      This section relates to what you have or have not done.  Accordingly, the
      group of people of which we are speaking are of both genders.  In
      questions where groups of people are concerned, there must be at least
      one person in the group who is of the opposite gender."
"listened to dirty jokes in mixed company?"
"told dirty jokes in mixed company?"
"discussed masturbation?  (Bringing up the topic of masturbation
during Purity Testing does not cut it as discussing masturbation.)"
"watched a porn movie in mixed company?"
"watched a porn movie with your own or someone else's parents?"
"played a game which may require you or others to disrobe?"
"played a game which may require you or others to perform an
action on another participant?"
"been in intimate contact with more than one person at the same time
while all of you were nude?"
"had oral sex with more than 10 people?  (not necessarily at one time)"
"had sex with more than 10 people?  (not necessarily at one time)"
"had sex with more than 1 person in a 24 hour period?"
"had sex with both genders in a 24 hour period?"
"been in a menage-a-trois with at least one member of the opposite sex?"
"walked in on others having sex (committed an 'oops') and then joined in?"
"had sex or oral sex with a person and his/her parent?  (not necessarily
at the same time.)"
"had sex, oral sex, mutual masturbation, necking, or petting with a
person and his/her sibling?  (not necessarily at the same time.)"
"been involved in a two (or more) in one?  (oral & anal or vaginal & anal
counts.  But no fingers - we're talking the real thing here.)"
"been involved in a gang bang?  (Step right up; come and get it while
it's hot.)"
"been in a circle of fuck?"
"been in a 69 circle?"
"been to a (cooking, baby, Wesson) oil party?"
"played naked Twister [tm] (with or without oil)?"
"participated in a heterosexual orgy or been involved in group sex?
(Four people or more; partners must be exchanged.)"
"participated in a bisexual orgy or group sex? (Four people or more;
partners must be exchanged.)"
"propositioned a person or group of people for group sex?"
"been propositioned for group sex?"
"participated in a fuck-a-thon?  (Where the object is to see how
many times you can do it in a certain amount of time.)"
)
("Non sentient objects"
"      This section measures your kinkiness.  Therefore, the questions apply to
      actions and events which occurred while you were alone, as well as those
      which occurred while you were with someone else."
"used a bowling pin, Coke bottle, or something along those general
shapes for masturbatory or sexual purposes?"
"masturbated using the aid of food?"
"eaten the food used in masturbation after masturbation?"
"eaten a lab dissection?"
"inserted food into your or someone else's anus?"
"eaten food after it was extracted/evacuated from your or someone else's
anus?"
"received an enema for a purpose other than medical?"
"received an enema consisting on a non-normal enema solution (wine, beer,
urine, windex)?"
"actually measured your own or someone else's penis?  (i.e., actually
grabbed a ruler, yardstick, tape measure, etc.)"
"used a mechanical hand-holdable device in aiding or replacing
masturbation?  (Vibrator, massager, Dustbuster(tm), vacuum cleaner, etc.)"
"used a feather or any other tickling device for the purpose of
tickling?"
"used tickling as a form of *arousal*?"
"used ice for sexual purposes?"
"used ice for something frozen as a dildo?  (ice cubes don't count here -
icicles, popsicles, and the like.  Penetration.)"
"used a strap-on dildo or male extension sheath?"
"used whipped cream for sexual purposes?"
"used hot/melted wax for sexual purposes?"
"had sex in front of or under a mirror?"
"put food, gravy, syrup, salad dressing, candy, peanut butter, honey
or anything else edible on your partner's body, and then eaten it?"
"used household syrup, sandwich spreading, oil, salad dressing,
or any like substance for sexual purposes?"
"used ropes, chains, cuffs or any other such device for bondage
purposes?"
"used a whip, chain, cat-o-nine-tails, or something similar for pain?"
"worn edible underwear/lingerie?"
"eaten edible underwear/lingerie off of someone?"
"worn a leather/grore suit?"
"worn diapers for a sexual or masturbatory purpose?"
"been diapered by someone else for a sexual or masturbatory purpose?"
"used a ball gag or other manufactured gag?"
"worn a collar and/or leash?"
"been completely tied down (spreadeagled, hogtied, etc.)?"
"tied someone down completely?"
"had sex while you or your partner was tied up?"
"used nipple clips (clothespins count)?"
"pierced a part of your body other than your ears or nose (nipples,
labia, head of penis)?"
"found a prepubescent child sexually attractive/arousing?"
"had sexual contact of any kind with a prepubescent child?"
"used an inflatable doll?"
"humped an inanimate object like a pillow, (dinner) liver, hole in
the wall, sausage, banana, etc?"
"had sex or oral sex with a dead person?"
"inserted a small animal or creature into your rectum?  (Either alive
or dead.)"
"had sex or oral sex with your (dead) dinner animal?"
"watched animals having sex?"
"been aroused by the sight of animals having sex?"
"attempted to have sex, oral sex, or (mutual) masturbation with a
live animal, but failed?"
"had sex, oral sex, or (mutual) masturbation with a smallish animal?
(Dog, cat, rabbit, lab rat, gerbil, etc.)"
"had sex, oral sex, or (mutual) masturbation with a domesticated farm
animal?  (Cow, pig, chicken, sheep, etc.)"
"mutilated or killed any living animal or creature for your pleasures?"
"had sex with a live animal?  (Any size.)"
"received oral sex from a live animal?  (Any size.)"
"gave oral sex to a live animal?  (Any size.)"
"had sex, oral sex, or (mutual) masturbation with a single type
animal more than once?  (Alive or dead.)"
"had sex, oral sex, or (mutual) masturbation with an animal in the
presence of 1 or more other people?"
"cross dressed in the *undergarments* of the opposite sex for a sexual
purpose?  (This means that school plays, initiations, and the like
don't count.)"
"cross dressed in the *undergarments* of the opposite sex for a sexual
purpose in the presence of 1 or more other people?  (Plays and the like
still don't count - this is for a sexual purpose)."
"stuffed your pants or bra while you were cross-dressed for your
sexual purpose?"
"had your head inserted into a urinal or toilet bowl willingly?
(Doing this in a pristine, show-room model does not count.)"
"worn groinal underwear on your head?  (Panties, jock-strap, etc.)"
"eaten, sucked, licked, nibbled, or otherwise inserted a used, unwashed
piece of groinal underwear into your mouth while said underwear
was not being worn at the time?"
"deliberately sniffed or smelled a used, unwashed piece of groinal
underwear while it was not being worn at the time?"
"been gagged with someone's underwear?  (not your own)"
"played in or with shit?"
"worn or used a condom or any feminine hygiene contraption that has
already been used by someone else?"
)
("Locality"
"This  section  tries  to  figure  out how many places you have done it. It
applies only to those situations in which you were with someone else."
"necked or petted in a place of religion? (Church, temple, altar,
Grand Holy Cabbage Head Patch, etc.)"
"had sex or been involved in oral sex or mutual masturbation in a place
of religion?"
"necked or petted in a place of the dead? (Morgue, mortuary, cemetery,
etc.)"
"had sex or been involved in oral sex or mutual masturbation in a place
of the dead?"
"necked or petted in a contraption of the dead? (Coffin, hearse, body
bag, etc.)"
"had sex or been involved in oral sex or mutual masturbation in a
contraption of the dead?"
"had sex in/on a construction site (house, office, launch platform, etc.)?"
"necked or petted in a moderately sized, land/road-based vehicle of LESS
THAN 30,000 pounds net unladen gross weight?  (car, station
wagon, van, minivan, minibus, fuckmobile, etc.)"
"had sex or been involved in oral sex or mutual masturbation in a
moderately sized, land/road-based vehicle of LESS THAN 30,000 pounds
net unladen gross weight?"
"necked or petted in a land/road-based vehicle of MORE THAN 30,000
pounds net unladen gross weight?  (truck, tank, armored car, steam-
roller, crane, bulldozer, garbage truck, etc.)"
"had sex or been involved in oral sex or mutual masturbation in a
land/road-based vehicle of MORE THAN 30,000 pounds net unladen gross
weight?"
"had sex or been involved in oral sex or mutual masturbation in a land-
based, non road dependant vehicle? (Train, subway, roller-coaster,
monorail, Disneyland ride, etc.)"
"necked, petted, masturbated, been masturbated, or had sex or oral sex
in a water based, manual powered vehicular transport medium? (Rowboat,
surfboard, floating bathtub, etc.)  (For this question it only counts if
the thing was in the water at the time.)"
"necked, petted, masturbated, been masturbated, or had sex or oral sex
in a water based, wind or propeller driven transport medium LESS THAN 80
feet in length?  (Yacht, PT boat, windsurfer, Sunfish, etc.)"
"necked, petted, masturbated, been masturbated, or had sex or oral sex
in a water based, wind or propeller driven transport medium MORE THAN 80
feet in length?  (Cruise ship, battleship, aircraft carrier, nuclear
submarine, etc.)"
"necked, petted, masturbated, been masturbated, or had sex or oral sex
in an aircraft?  (Airplane, helicopter, hovercraft, balloon, zeppelin,
space shuttle, flying carpet, flying saucer, etc.)"
"had sex in a household room other than a bedroom?"
"had sex on the floor (but not the roof)?"
"had sex on any furniture that is indoors but is not a bed, table,
desk, counter-top, a nor anything that is predominantly used for
sitting or as a table/desk?  (Television, washer/dryer, microwave, etc.)"
"had sex, participated in oral sex, or participated in mutual
masturbation in a telephone booth, voting booth, automatic
photograph taker, or any such small, non-moving enclosure that was
not designed for such activities?"
"had sex, participated in oral sex, or participated in mutual
masturbation in an elevator, people-mover, escalator, dumbwaiter, or any
building-internal people moving device?"
"had sex, participated in oral sex, or participated in mutual
masturbation up a tree but not in a tree house or similar structure?"
"had sex, oral sex, mutual masturbation in a suspension device of some
kind (hammock, trampoline, tightrope, safety net, etc.)?"
"had sex, participated in oral sex, or participated in mutual
masturbation on the roof of a building in excess of 5 floors?"
"had sex, participated in oral sex, or participated in mutual
masturbation inside or within the confines of a hedge, bush, other
natural vegetation which can provide a wall effect, cave, rock
overhang, in a well, or any other secluded, outdoor, non-vegetative
shielding structure?"
"had sex, oral sex, or mutual masturbation in the snow? (Spring thaw is
acceptable.)"
"had sex, oral sex, or mutual masturbation in a place where the
prevailing, ambient temperature (of the air immediately surrounding
you) was below the freezing point of water?"
"had sex, participated in oral sex, or participated in mutual
masturbation in a place where you could have been discovered?"
"had sex, oral sex, or mutual masturbation in a water-filled
bathtub, hot tub, or under a shower or other running water (waterfall,
torrential downpour, monsoon, etc.)?"
"had sex, oral sex, or mutual masturbation in a body of water?
(swimming pool, swimming hole, pond, lake, lagoon, sea, ocean, etc.)"
"had sex on the beach (and the resultant sand in uncomfortable places)?"
"had sex, oral sex, or mutual masturbation in a bathroom of the
opposite sex?"
"gone to a motel (however sleazy) for the sole purpose of having sex?"
)
("Sexual Style"
"These questions deal with the 'stylishness' of your sexuality."
"watched while someone else masturbated?"
"been watched while masturbating?"
"orgasmed on somebody?"
"orgasmed in somebody if you are male, or had someone orgasm in you
if you are female?"
"had more than one person orgasm on you at once, or been one of several
people orgasming on someone else?"
"been involved in the use of a penis as a leash or bludgeoning
device?"
"been involved in oxygen deprivation for sexual enhancement?
(Nitrous oxide does not count.)"
"willingly made video tapes or had pictures taken while having sex,
oral sex, or mutual masturbation?"
"physically watched others having sex?"
"watched your partner of choice having sex, oral sex, or mutual
masturbation with someone else?"
"taken pictures or made video tapes of your partner of choice having sex,
oral sex, or mutual masturbation with someone else?"
"willingly made audio recordings while having sex, oral sex, or
mutual masturbation?"
"talked dirty while having sex, oral sex, or mutual masturbation?"
"intentionally made more noise than necessary while having sex, oral
sex, or mutual masturbation so as to put on a good show for whoever
might have been listening in?"
"intentionally made 'animal' noises during sex?"
"had a pet or domesticated animal walk over you or your partner while
you were involved in sex or oral sex?"
"had a general emergency arise while you were steeped in sex?  (House on
fire, flash flood, hurricane, private plane crash lands in your
dwelling, etc.)"
"had your sexual technique/style/skill openly praised by someone?"
"taken nude pictures of someone else?"
"had nude pictures of you taken?"
"placed a personal ad?"
"answered a personal ad?"
"been involved in breast fucking?  (aka 'The Hawaiian Muscle Fuck')"
"participated in fist-fucking?  (see 'Caligula')"
"shaved someone's genital pubic hair (or had yours shaved) as part of a
sexual activity?"
"had sex for more than three hours in a single session of sex?"
"been bruised during sex, oral sex, or mutual masturbation?
(hickies do not count.)"
"bruised someone else during sex, oral sex, or mutual masturbation?"
"been injured during sex, oral sex, or mutual masturbation?"
"orgasmed more than three times in one session of sex?"
"had sex so many times or for so long that one or both people involved
runs dry?"
"disturbed other people by making excessive noise while having sex?"
"had sex or mutual masturbation or received oral sex
while you were driving (the car or boat or whatever vehicle)?"
"had sex doggie fashion?"
"had sex in the female superior position?"
"had sex sitting up?"
"had sex standing up?"
"had sex upside-down?"
"gone through two or more sexual positions without the need for
re-entry?"
"fallen asleep during sex?"
"woken up to someone having sex with you?"
"had sex while one person was passed out or unconscious?"
"given or received a hickey on your upper inner thigh?"
"been on the receiving of anal sex?"
"been on the ramming end of anal sex (a dildo counts)?"
"had sex more than 10 times with 1 person?"
"had sex more than 5 times in a 24 hour period?"
"had sex more than 10 times in a 24 hour period?"
"had sex, participated in oral sex, or participated in masturbation
while someone other than your partner was watching?"
"had sex or oral sex while one or both of you were playing a musical
instrument, hacked, watched television, read, drawn or in other
words were preoccupied with something other than sex and lust?"
"performed oral sex after intercourse without washing or douching?"
"kissed your partner on the lips after oral sex without brushing
teeth, nor washing/gargling/rinsing out mouth?"
"inflicted pain during sex?"
"been involved in cunnilingus during the woman's period?"
"had sex during the woman's period?"
"foot masturbated someone?"
"been foot masturbated?"
"tongue bathed someone?"
"been tongue bathed?"
"licked or sucked on someone else's feet and/or toes?"
"had your feet and/or toes licked or sucked by someone else?"
"licked someone's anus?"
"licked someone's anus while they were defecating?"
"performed oral sex while the person was urinating?"
"drank your own urine?"
"tasted or drank someone else's urine?"
"drank/drained an entire bladder-full of someone else's urine?"
"drank human blood?"
"tasted someone else's nasal mucous?"
"been involved in a golden shower?"
"swallowed your partner's orgasmic secretions?"
"used the Purity Test as a checklist of things you could do?"
"ever done something for the sole purpose of lowering your Purity Test
score?"
"bought/read books to enhance sexual technique?"
"participated in Purity Testing with an ulterior motive?"
"become interested in someone only after hearing about their Purity Test
score?"
)))
;-- 


