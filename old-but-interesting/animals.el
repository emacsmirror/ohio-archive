;;;
;;;From: ag@yoda.omnicron.com (Keith Gabryelski)
;;;Date: 23 Dec 92 00:59:40 GMT
;;;
;;;Written for GZ (wherever she is these days).
;;;
;;;Various hacks from: Denis Howe <dbh@doc.ic.ac.uk>
;;;

;;; LCD Archive Entry:
;;; animals|Keith Gabryelski|ag@yoda.omnicron.com|
;;; The ancient game of guess the animal.|
;;; 24-Dec-1992||~/games/animals.el.Z|

(defvar animals-file "~/.animals" "Animal game data file")

(defvar animals-default-data '("a cat" nil nil) "Default data for animal game")

(defvar animals nil
  "Animal game data.  A recursive list of the form
(STRING TRUE-LIST FALSE_LIST) where TRUE-LIST and FALSE-LIST are lists of
the this form and STRING is either a question posed to the user or a name
of an animal.")

(defun animal ()
  "Play the game \"Guess The Animal\" using the list \"animals\" as data."
  (interactive)
  (let (animal-buffer)
    (save-window-excursion
      (if (file-readable-p (expand-file-name animals-file))
	  (setq animal-buffer
		(find-file-noselect (expand-file-name animals-file)))
	(progn
	  (setq animal-buffer
		(create-file-buffer (expand-file-name animals-file)))
	  (set-buffer animal-buffer)
	  (erase-buffer)
	  (insert (format "%s" animals-default-data))))
      (set-buffer animal-buffer)
      (goto-char (point-min))
      (setq animals (read animal-buffer)))
    (animal-read-string
     "This is the game called animal where you think of an animal and I try to guess which one you are thinking of.

Please think of an animal and answer the questions I give you." nil)
    (setq animals (parse-animal-tree animals))
    (save-window-excursion
      (set-buffer animal-buffer)
      (erase-buffer)
      (insert (format "%s" animals))
      (write-file (expand-file-name animals-file)))))

(defun play-animal ()
  "Play the games of animal lots of times"
  (interactive)
  (let (done)
    (while (not done)
      (progn
	(animal)
	(setq done (not (y-or-n-p "Would you like to play again? ")))))))

(defun parse-animal-tree (animal-list)
  "Ask questions about a list (STRING TRUE-LIST FALSE-LIST) until we reach a
terminal"
  (if (animal-query-user (car animal-list) (null (car (cdr animal-list))))
      (if (null (car (cdr animal-list)))
	  (progn 
	    (message "Thanks for playing.")
	    animal-list)			
	(list (car animal-list)
	      (parse-animal-tree
	       (car (cdr animal-list))) (car (cdr (cdr animal-list)))))
    (if (null (car (cdr (cdr animal-list))))
	(animal-give-up animal-list)
      (list (car animal-list)
	    (car (cdr animal-list))
	    (parse-animal-tree (car (cdr (cdr animal-list))))))))

(defun animal-query-user (question terminal)
  "Query user about information on a particular animal"
  (y-or-n-p (if terminal
		(format "Is it %s? " question)
	      (format "%s? " question))))

(defun animal-give-up (animal-list)
  "Given an animal list, query the user for information on a new type of
animal"
  (let (new-animal new-question)
    (progn
      (setq new-animal
	    (animal-read-string "I give up.  I need to know a little bit about the animal you are thinking of
so I won't miss this one next time." "What animal where you thinking of? "))
      (setq new-animal (animal-add-article new-animal))
      (setq new-question
	    (animal-read-string
	     (format
	      "I also need to know how to ask a yes or no question that distinguishes \"%s\" from \"%s\".

Please type in such a question (ie, \"Does it have wings\" would be a suitable question to distinguish a bird from a cat)."
	      new-animal (car animal-list))  "Please type in a question: "))
      (if (animal-query-user (animal-replace-it new-question new-animal) nil)
	  (list new-question (list new-animal nil nil) animal-list)
	(list new-question animal-list (list new-animal nil nil))))))

(defun animal-read-string (buffer-string prompt-string)
  "Open a buffer up, spew some helpul advice and accept a string"
  (let (animal-help-buffer)
    (progn
      (setq animal-help-buffer (get-buffer "*Animal-Help*"))
      (if animal-help-buffer
	  nil
	(setq animal-help-buffer (generate-new-buffer "*Animal-Help*")))
      (set-buffer animal-help-buffer)
      (erase-buffer)
      (insert buffer-string)
      (fill-region (point-min) (point-max))
      (goto-char (point-min))
      (display-buffer animal-help-buffer)
      (if (not (null prompt-string))
	  (read-string prompt-string)))))

(defun animal-replace-it (question animal)
  "Replace the string `it' in QUESTION, if any, with ANIMAL."
  (let ((it (string-match "it" question)))
    (if it
	(concat (substring question 0 it)
		animal
		(substring question (+ it 2)))
      question)))

(defun animal-add-article (animal)
  "Make sure ANIMAL starts with `a ' or 'an '."
  (cond ((string-match "^a " (downcase animal)) animal)
	((string-match "^an " (downcase animal)) animal)
	((string-match "^the " (downcase animal)) animal)
	((string-match "^[A-Z]" animal) animal)
	((string-match "^[aeiou]" (downcase animal)) (concat "an " animal))
	(t (concat "a " animal))))
