;;; animals.el - guess the animal

;;; v1.01 27 Jun 1994
;;; Denis Howe <dbh@doc.ic.ac.uk>
;;; Extensive rewrite.

;;; v1.00 23 Dec 1992
;;; Keith Gabryelski <ag@yoda.omnicron.com>
;;; Original version.

;;; In your ~/.emacs put:
;;;
;;;	(autoload 'animal "animals" "Play the animal game.")

;;; LCD Archive Entry:
;;; animals|Keith Gabryelski|ag@yoda.omnicron.com|
;;; The ancient game of guess the animal.|
;;; 27-Jun-1994|1.01|~/games/animals.el|

(defvar animals-file "~/.animals"
  "Animal game data file.")

(defvar animals "a cat"
  "Default data for animal game.
A tree where each node is either of the form
(QUESTION YES-TREE . NO-TREE) or the name of an animal.")

(defun play-animal ()
  "Play the games of animal until you're sick of it."
  (interactive)
  (animal)
  (if (y-or-n-p "Would you like to play again? ")
      (play-animal)))

(defun animal ()
  "Play the game \"Guess The Animal\".
Data is read from the file named by the variable \"animals-file\" if
that file exists.  The data is saved there after each game."
  (interactive)
  (animal-read-file)
  (switch-to-buffer " Animals dialogue")
  (erase-buffer)
  (insert "Please think of an animal and I will try
to guess which one you are thinking of.\n\n")
  (message "Hit a key when ready")
  (read-event)
  (setq animals (animal-ask animals))
  (animal-write-file))

(defun animal-ask (animal-tree)
  "Ask about known animals until we reach a terminal.
Return a replacement animal tree."
    (if (listp animal-tree)
	;; Non-leaf node - a question to ask
	(let* ((question (car animal-tree))
	       (subtrees (cdr animal-tree))
	       (yes-tree (car subtrees))
	       (no-tree (cdr subtrees)))
	  (cons question
		(if (animal-query-bool question)
		    (cons (animal-ask yes-tree) no-tree)
		  (cons yes-tree (animal-ask no-tree)))))
      ;; Leaf-node - an animal name
      (if (animal-query-bool (concat "Was it " animal-tree))
	  animal-tree			; Got it - no change
	(animal-give-up animal-tree)))) ; No luck - give up

(defun animal-give-up (old-animal)
  "Ask the user for a new animal and question.
Return a new tree for the best guess so far (OLD-ANIMAL), the new
question and the new animal."
  (let* ((new-animal
	  (animal-add-article
	   (animal-query-string "What animal where you thinking of")))
	 (new-question
	  (animal-get-question new-animal old-animal)))
    (cons new-question 
	  (if (animal-query-bool (animal-replace-it new-question new-animal))
	      (cons new-animal old-animal)
	    (cons old-animal new-animal)))))	    

(defun animal-get-question (new old)
  "Ask the user for a question to distingish NEW from OLD."
  (let ((question (animal-query-string
		   (format "What yes/no question distinguishes %s from %s"
			   new old))))
    (if (string-match "\\<it\\>" question)
	question
      (insert "Your question must include the word \"it\".\n\n")
      (animal-get-question new old))))
      
(defun animal-read-file ()
  "Read the animals data.
Return the data read from \"animals-file\" if it exists."
  (setq animals-file (expand-file-name animals-file))
  (if (file-readable-p animals-file)
      (save-excursion
	(find-file animals-file)
	(goto-char (point-min))
	(setq animals (read (current-buffer))))))

(defun animal-write-file ()
  "Write the animals data to \"animals-file\"."
  (let* ((standard-output (get-buffer-create " Animals data")))
    (set-buffer standard-output)
    (print animals)
    (write-file animals-file)
    (kill-buffer standard-output)))

(defun animal-replace-it (question animal)
  "Replace the string \"it\" in QUESTION with ANIMAL."
  (concat (substring question 0 (string-match "it" question))
	  animal
	  (substring question (match-end 0))))

(defun animal-add-article (animal)
  "Make sure ANIMAL starts with `a ' or 'an '."
  (let ((lower (downcase animal)))
    (cond ((string-match "^a " lower) animal)
	  ((string-match "^an " lower) animal)
	  ((string-match "^the " lower) animal)
	  ((string-match "^[A-Z]" animal) animal)
	  ((string-match "^[aeiou]" (downcase animal)) (concat "an " animal))
	  (t (concat "a " animal)))))

(defun animal-query-bool (prompt)
  "Ask the user a yes/no PROMPT.
Insert it and the reply in the current buffer."
  (insert prompt "? ")
  (let ((reply (y-or-n-p (concat prompt "? "))))
    (insert (if reply "Yes." "No.") "\n\n")
    (sit-for 0)
    reply))

(defun animal-query-string (prompt)
  "Insert PROMPT in the current buffer, read a reply and insert that too
and return it."
  (insert prompt "?\n")
  (let ((reply (read-string (concat prompt "? "))))
    (insert reply "\n\n")
    (sit-for 0)
    reply))

;; End of animals.el
