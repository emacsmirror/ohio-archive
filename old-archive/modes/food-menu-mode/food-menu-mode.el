;;;; -*- Mode: Emacs-Lisp -*-
;;;; File: food-menu-mode.el
;;; 
;;; This code is distributed under the terms of the GNU General Public
;;; License. If you are not sure what that means, contact the author
;;; at kfogel@cs.oberlin.edu.
;;; If you do not have electronic mail access, write to me at
;;;
;;; Karl Fogel
;;; 1123 N. Oak Park Ave.
;;; Oak Park, ILL
;;; USA      60302
;;;
;;; for more information.
;;;
;;; Please let me know what you think of it... I like messages with
;;; complaints almost as much as ones without complaints! :-)
;;;
;;;
;;;    LCD Archive Entry:
;;;    food-menu-mode|Karl Fogel|kfogel@cs.oberlin.edu|
;;;    Major mode for planning restaurant meals|
;;;    14-June-93|$Revision: 1.00 $|~/modes/food-menu-mode.tar.gz|
;;;
;;;
;;; INSTALLATION:
;;; Put this file in a directory that is in Emacs' load-path, and then
;;; add the following to your .emacs (without the semicolons, of course):
;;;
;;; (setq auto-mode-alist (cons '("\\.mnu$" . food-menu-mode) auto-mode-alist))
;;;
;;; (autoload 'food-menu-mode "food-menu-mode" "Major mode for food menus." t)
;;;
;;; Then, whenever you bring up a file ending in ".mnu", it will
;;; automatically be in food-menu-mode from the start.
;;;
;;; MENU FORMAT:
;;; This is a special format that restaurant menus must be in.  Think of
;;; it as source code for a meal, with unselected meals as the never-
;;; executed branches of "if" statements :-)  
;;;
;;; Menu format works like this:
;;; Codes introduced with a backslash (the "item" indicator on a menu):
;;; \a    appetizer
;;; \m    main dish
;;; \b    beverage
;;; \v    vegetarian
;;; \s    liquid
;;; \S    spicy
;;; \d    special deal (usually a luncheon special or something, with
;;;                     details given on the menu.)
;;; 
;;;
;;; Codes can be combined: for example, "\mv" is a vegetarian main
;;; dish (main dish = "entree", for francophiles).  All item codes
;;; come at the beginning of a line (i.e.: when doing regexp searches
;;; for items, use "^\\\\[a-zA-Z]" ).
;;;
;;; There will be new codes in the future.  All will consist of a
;;; single letter of the Roman alphabet, upper or lower case.  So I 
;;; guess we are limited to 52 codes.  Hopefully, this won't be a
;;; problem.  If you have an idea for a new item code, send it to me
;;; at kfogel@cs.oberlin.edu or fogel@antares.mcs.anl.gov.  It is
;;; important that control of new item codes remain centralized so
;;; that food-menu Lisp stuff remains portable and reliable.
;;; 
;;; Menu format introduces extraneous comments with "#" at the
;;; beginning of a line.
;;; 
;;; a "$" indicates the beginning of a price.  Prices usually do not
;;; appear at the beginning of a line, but rather at the end of a
;;; line, following a menu item.  The number follows the "$" without
;;; any spaces, and should have two places following the decimal point
;;; (i.e.: $7.95 and $12.50 are both legal prices, but $21.936 is
;;; not).  Do not use a "$" anywhere in the actual name of a dish, 
;;; since it is a special symbol that indicates price.
;;;
;;; "$" is U.S.-centric, but people will just have to deal with that.
;;; :-) 
;;; Right now, maximum is 3 sizes for a dish.  This may change, but 
;;; don't count on it.  If you are actually eating at restaurants that
;;; offer the same dish in four or more sizes, Emacs is the least of 
;;; your problems.  
;;; 
;;; "*** foo ***" at the beginning of a line indicates a section on the 
;;; menu.  Format is "*** <section name> ***".  For example, 
;;; "*** seafood ***", or "*** fried rice ***".
;;;
;;; So now you know how restaurant menu format works.  Whoopee.
;;;
;;; For an example of a menu-format document, look at mandarin.mnu,
;;; which you should have gotten along with food-menu-mode.el.  Both
;;; mandarin.mnu and food-menu-mode.el are archived for anonymous ftp
;;; from cs.oberlin.edu:/public/emacs-lisp/food-menu-mode.el.tar.gz
;;;
;;; USAGE:
;;; 
;;; Use "C-h m" in the menu buffer to see available commands.  Use "n"
;;; and "p" to browse up and down the menu, and RETURN to select an
;;; item.  It will keep track of the cost for you, including tax (see
;;; variable food-meal-tax) and tip (see variable
;;; food-meal-tip-percentage).  See also food-meal-use-tax and
;;; food-meal-use-tip.
;;;
;;; Here is an excerpt of the help string for food-menu-mode:
;;;
;;; Major mode for planning a meal with a
;;; menu-format buffer.  Normally, it sets up a separate buffer called
;;; the "meal buffer", which displays the meal being planned.
;;; This includes selected dishes and the total cost (see variables
;;; food-meal-tax and food-meal-tip-percentage for local
;;; customizations).  Keybindings in the menu buffer (not the meal
;;; buffer) are: 
;;; key             binding
;;; ---             -------
;;; 
;;; r		re-search-backward
;;; s		re-search-forward
;;; q		food-menu-bury-buffers
;;; j		food-menu-jump-to-section
;;; d		food-meal-delete-item
;;; RET		food-menu-select-this-item
;;; c		food-meal-calculate-cost
;;; P		food-menu-previous-section
;;; p		food-menu-previous-item
;;; N		food-menu-next-section
;;; V		food-menu-previous-item-vegetarian
;;; v		food-menu-next-item-vegetarian
;;; SPC		food-menu-next-item
;;; n		food-menu-next-item
;;; 
;;; The meal buffer can be edited like any other Emacs buffer -- it does
;;; not have its own major mode.  However, typing C-c C-c in the meal
;;; buffer should recalculate the cost for you.  This is useful if you
;;; have been editing the prices or deleting items.
;;;
;;; NOTES ON THE CODE:
;;; Functions that deal mainly with the menu buffer are prefixed
;;; "food-menu-", those that deal mainly with the meal buffer are
;;; prefixed "food-meal-".  Note that "\C-c\C-c" is rebound to
;;; food-meal-calculate-cost just about every time the meal buffer is
;;; switched to.  This is because the user might conceivably kill the
;;; meal buffer after food-menu-mode was called.  Thus, later when a
;;; new meal buffer is created with get-buffer-create (say, in
;;; food-menu-select-this-item), the keybinding would be gone.
;;; Solution is to rebind every time "(set-buffer food-meal-buffer)"
;;; is called.  Also, should food-meal-calculate-cost actually take
;;; buffer as an arg?  It doesn't really matter... I guess the added
;;; flexibility is a plus, so we'll leave it that way...

;;;; menu and meal functions ;;;;

;;; TODO LIST AND GENERAL NOTES:

;;; mmm... need to put real GNU GPL at top, maybe...

;;; food-menu.el will need functions:
;;; jump-to-next-vegetarian/soup/whatever
;;; (that will be one function called with various regexps,
;;; probably...
;;; how to combine conditions? i.e.: go to next spicy & vegetarian
;;; item (!?)
;;; Probably can append to regexp, yeah...
;;; Is it important enough, or do I suffer from feeping creaturism?
;;;
;;; We already have vegetarian, bound to "v" in menu buffer.  was
;;; pretty easy.

(defvar food-menu-item-regexp "^\\\\" "*This is what previous-item and
next-item use to find where they're supposed go.  This regexp changes
when told to search only for vegetarian items, etc.")

(defvar food-menu-line-regexp
  (concat
   "^"
   ;; backslash codes
   "\\\\\\([a-zA-Z]*\\)" ; end first pexp : the backslash codes
   "\\(\\s +\\)"      ; end second pexp : the whitespace
   
   ;; Description of item
   "\\(\\S [^$]*\\)"  ; end third pexp : the item string
   
   ;; List of prices
   "\\(\\$.+\\)"      ; end fourth pexp : the price(s)
   
   "$")
  "*This variable makes use of \"menu format\" to construct a
regexp that will be used to parse the backslash codes, item string, 
and prices out of a menu line entry. You can change this, but it 
might break some or all menu-format documents if you do.")

(defvar food-meal-use-tax t "*If non-nil, use the value of variable
food-meal-tax in calculating the cost of a meal.")

(defvar food-meal-tax 0.055 "*Tax to be used when computing meal cost.")
;;; You may apply the function "clintonize" if you wish to increase
;;; this without *actually* setq'ing...  need emacs 19 for floats

(defvar food-meal-use-tip t "*If non-nil, use the value of variable
food-meal-tip-percentage in calculating the cost of a meal.")

(defvar food-meal-tip-percentage 0.15 "*Keep this high, please -- the author 
of this code works as a waiter.  Note: this is not literally a
percentage: if you want to tip fifty percent, set this to \"0.50\".")
;;; need emacs 19 for floats.  Yay!  Sigh...

(defvar food-meal-initial-cost 0 "*Modify this if you wish a fixed cost to
be added to the price of every meal. The variable food-meal-cost is 
initialized to this value, and reset to it after every calculation of
a meal cost. It should be zero by default.")

(setq food-menu-buffer (buffer-name))

(make-variable-buffer-local 'food-menu-buffer)

(defvar food-menu-buffer-shortname
  (save-match-data
    (let ((bufname (buffer-name)))
      (if (string-match "\\(.*\\)\\(\\.mnu\\)" bufname)
          (substring 
           bufname 
           (match-beginning 1)
           (match-beginning 2))
        (buffer-name))))
  "*Name of the menu buffer, with .mnu extension removed if it was there
\(which it should have been\).")

(make-variable-buffer-local 'food-menu-buffer-shortname)

(defvar food-meal-buffer ; needs to be a local variable
  (concat "*" food-menu-buffer-shortname " Meal*")
  "*Name of the buffer containing meal information \(selected dishes,
total cost, etc.  It is different for every menu file -- in fact, it's
just \"<menu-buffer-name> Meal*\".  In the meal buffer, C-c C-c should
usually recalculate the cost for you.")

(make-variable-buffer-local 'food-meal-buffer)

(defun food-meal-add-item (buf str)
  (save-excursion
    (set-buffer (get-buffer-create buf))
    (goto-char (point-min))
    (save-match-data
      (let ((have-items nil))
	(while (re-search-forward "\\$\\([0-9]*\\.[0-9]*\\)" (point-max) t)
	  (setq have-items t))
	(if have-items 
	    (progn
	      (forward-line 1)
	      (insert (concat str "\n")))
	  (progn 
	    (goto-char (point-min))
	    (insert (concat str "\n"))))))
    (food-meal-calculate-cost food-meal-buffer)
    (display-buffer buf)))

(defun food-meal-delete-item ()
  "Delete this item of the menu from the meal buffer.  If the item
appears multiple times in the meal buffer, perhaps in different sizes,
all instances will be removed.  To remove a single instance of an item
that appears multiple times in the meal buffer, it's easier to just
switch to the meal buffer and kill the line with normal Emacs editing
commands \(i.e.: \\[kill-line]\)."
  (interactive)
  (let ((str (food-menu-parse-item (current-buffer) (point) t)))
    (save-excursion
      (set-buffer (get-buffer-create food-meal-buffer))
      ;; we rebind calculate-cost every time we switch to the meal
      ;; buffer because we want the user to have this keybinding even
      ;; if the killed the original meal buffer and a new one was
      ;; made later.  
      (local-set-key "\C-c\C-c" 'food-meal-calculate-cost)
      (goto-char (point-min))
      (save-match-data
	(while (search-forward str (point-max) t)
	  (progn
	    (beginning-of-line)
	    (let ((kill-whole-line t))
	      (kill-line)))))
      (food-meal-calculate-cost food-meal-buffer)
      (display-buffer food-meal-buffer))))
    
(defun food-menu-select-this-item ()
  "Gets menu item of the current line."
  (interactive)
  (let ((str (food-menu-parse-item (current-buffer) (point))))
    (if str (food-meal-add-item food-meal-buffer str))))

;;; this next function will ask the user what size they want, after
;;; determining what's available. However, eventually, it will first
;;; examine the value of a variable, default-menu-item-size, and only
;;; query if that is not set (and with med will go to small, when
;;; necessary). Then that variable can be set with a keystroke while in
;;; menu-mode, as though its current setting is a kind of "small"-mode
;;; or "large"-mode or something... we will *not* modify meal-cost
;;; here, rather, calculate-cost will do it, so that people can
;;; actually edit the menu buffer manually if they want. Therefore,
;;; when asking the size, be prepared to accept <return> as the answer,
;;; meaning show all sizes in the menu buffer, but calculate-cost will
;;; choose the first number following a $ that it sees (i.e.: smallest
;;; item), unless the buffer is edited manually before calculating the
;;; cost. 

(defun food-menu-parse-item (menu-buffer menu-item-point &optional no-size)
  "Asks user whether they want small or large if there is a choice, then
returns the correct string, with price.

If the user hits <RETURN> instead of a size \(like \"s\", \"m\", or
\"l\"\), then all of the available sizes will be inserted into the meal
buffer.  This will mean that the automatically calculated price will
probably be too high, but the meal buffer can be edited manually and
price recalculated later.

From Lisp, takes MENU-BUFFER and MENU-POINT as args. Optional third arg 
NO-SIZE, if non-nil, means don't query about size \(no default, just no 
size chosen\).  This is not the same as the behavior when a user
selects <RETURN> as the size -- that causes all sizes to be chosen."
  (set-buffer menu-buffer)
  (beginning-of-line)
  (save-match-data
    (if (not (looking-at "\\\\[a-zA-Z]"))
	(error "This line is not a valid restaurant menu item.")
      (let* ((price-index 0)
	     (price-cnt 0)
	     (full-item-str 
	      ;; full-item-str is the whole line, with backslash codes and all
	      (buffer-substring 
	       (point) 
	       (progn (end-of-line) (point))))
	     (item-str 
	      (progn (string-match food-menu-line-regexp full-item-str) 
		     ;; that was so we can use match data later
		     (substring full-item-str 
				(match-beginning 3) 
				(match-end 3))))
	     ;; god i love match-data
	     (price-str 
	      (substring full-item-str 
			 (match-beginning 4) 
			 (match-end 4))))
	;; no, really, money & sex are nothing compared to match-data...
	
	;; find out how many prices (i.e.: sizes) the dish comes in:
	(while (string-match "\\$" price-str price-index)
	  (setq price-index 
		(1+ 
		 (string-match "\\$" price-str price-index)))
	  (setq price-cnt (1+ price-cnt)))
	
	;; This section uses price-cnt to determine what size dishes to 
	;; offer the user. price-cnt was set, hopefully correctly, 
	;; earlier in this function. A lot could be abstracted out here,
	;; but at this point, I am too lazy to do it. Maybe after it all 
	;; works...
	(cond
	 (no-size item-str)
	 ((= 1 price-cnt)
	  (concat
	   item-str
	   (food-nth-str-tok price-str 1 "\\$" "\\s \\|[0-9]$")))
	 ((= 2 price-cnt)
	  (let ((item-size
		 (progn (message "Small size (s) or large (l)? ")
			(read-char))))
	    (cond
	     ((char-equal item-size ?s)
	      (concat
	       item-str
	       (food-nth-str-tok price-str 1 "\\$" "\\s-\\|[0-9]$")))
	     ((char-equal item-size ?l)
	      (concat
	       item-str
	       (food-nth-str-tok price-str 2 "\\$" "\\s-\\|[0-9]$")))
	     ((char-equal item-size ?\C-m)
	      (concat item-str price-str))
	     (t
	      (progn
		(ding)
		(message (concat 
			  "\""
			  (char-to-string item-size) 
			  "\" not a valid size."
			  ))
		nil)))))
	 
	 ;; now next, perhaps we ought to use (<= 3 price-cnt) ??
	 ((= 3 price-cnt) ; oooh, the biggy: three sizes! Can we cope?
	  (let ((item-size
		 (progn (message "Small size (s), medium (m), or large (l)? ")
			(read-char))))
	    (cond
	     ((char-equal item-size ?s)
	      (concat
	       item-str
	       (food-nth-str-tok price-str 1 "\\$" "\\s-\\|[0-9]$")))
	     ((char-equal item-size ?m)
	      (concat
	       item-str
	       (food-nth-str-tok price-str 2 "\\$" "\\s-\\|[0-9]$")))
	     ((char-equal item-size ?l)
	      (concat
	       item-str
	       (food-nth-str-tok price-str 3 "\\$" "\\s-\\|[0-9]$")))
	     ((char-equal item-size ?\C-m)
	      (concat item-str price-str))
	     (t
	      (progn
		(ding)
		(message (concat 
			  "\""
			  (char-to-string item-size)
			  "\" not a valid size."
			  ))
		nil)))))
	 (t
	  (concat
	   item-str 
	   ;; this defaulting to smallest will have to go...?
	   ;; there ought to be a variable to indicate default size!
	   ;; we could just insert price-str and let the user edit the
	   ;; meal buffer, too!!! hmmm.... easy modification to make,
	   ;; so we'll leave it alone for now.  Can do that later if
	   ;; it proves to be the Right Thing.
	   (food-nth-str-tok price-str 1 "\\$" "\\s-\\|[0-9]$"))))))))

(defun food-nth-str-tok (str n start-re end-re)
  "This is currently a hack, and should be much improved as soon as 
possible. The gist is to pass this function STRING, INTEGER, 
START-REGEXP, and END-REGEXP. It will then return the substring of 
STRING that begins with the INTEGERth occurence of START-REGEXP, up
to the next occurrence of END-REGEXP after that (previous occurrences 
of END-REGEXP are ignored)."
  ;; why can't this be done with one regexp? Hmmm... think about this:
  ;; what you use it for, how you use it, and if this is fixable? Sure
  ;; it is, dummy. Do this soon!

  ;; At the moment this will be general enough. It is searching by
  ;; the first re, instead of by both, so that if it finds a match for
  ;; the first that has 0 occurences of the second after it, it will
  ;; die a painful death. Or backtrack, which is the same thing. This
  ;; will have to be fixed, but for the moment it will be okay for
  ;; menu format, since we know what the format of lines is going to
  ;; be and can plan accordingly...
  ;; so, right now, if n is too high, it just returns the last possible
  ;; match it would if n were the highest it could legitimately be.
  ;; This is not really so bad - how should it be improved?
  ;; probably it could be improved by removing verbose and useless
  ;; comments, but without them I cannot insure immortality.  What is 
  ;; the solution to this conflict?
  (save-match-data
    (let ((index1 0)
	  (index2 0))
      (while (< 0 n)
	(setq n (1- n))
	(string-match start-re str index1)
	(setq index1 (1+ (match-beginning 0))))
      (setq index1 (1- index1)) ; to get first char of substr
      (setq index2
	    (1+ (string-match end-re str index1)))
      (substring str
		 index1 index2))))

(defun food-menu-previous-item ()
  "Move to the previous item on the menu, wrapping if necessary."
  (interactive)
  (beginning-of-line)
  (save-match-data
    (if (re-search-backward food-menu-item-regexp (point-min) t)
	(re-search-forward "\\s-")
      (progn
	(goto-char (point-max))
	(re-search-backward food-menu-item-regexp (point-min) t)
	(re-search-forward "\\s-")))))

(defun food-menu-previous-section ()
  "Move to the start of the previous section on the menu, wrapping around
if already at first section.  

See also food-menu-jump-to-section \\[food-menu-jump-to-section]."
  (interactive)
  (beginning-of-line)
  ;; having "***" as the section divider makes for some hilarious
  ;; regexps!
  (save-match-data
    (if (looking-at "^\\*\\*\\*.*\\*\\*\\*")
	(if (re-search-backward "^\\*\\*\\*.*\\*\\*\\*" (point-min) t)
	    nil
	  (progn (goto-char (point-max))
		 (re-search-backward "^\\*\\*\\*.*\\*\\*\\*")))
      (if (re-search-backward "^\\*\\*\\*.*\\*\\*\\*" (point-min) t)
	  (if (re-search-backward "^\\*\\*\\*.*\\*\\*\\*"
				  (point-min)
				  t)
	      nil
	    (progn (goto-char (point-max))
		   (re-search-backward "^\\*\\*\\*.*\\*\\*\\*")))
	(progn (goto-char (point-max))
	       (re-search-backward "^\\*\\*\\*.*\\*\\*\\*")))
      (beginning-of-line))))
  
(defun food-menu-next-item-vegetarian ()
  "Move to the next vegetarian item on the menu, wrapping if necessary."
  (interactive)
  ;; let's hear it for dynamic scoping, YAY!!!
  (let ((food-menu-item-regexp "^\\\\[a-zA-Z]*v"))
    (food-menu-next-item)))

(defun food-menu-previous-item-vegetarian ()
  "Move to the next vegetarian item on the menu, wrapping if necessary."
  (interactive)
  ;; let's hear it for dynamic scoping, YAY!!!
  (let ((food-menu-item-regexp "^\\\\[a-zA-Z]*v"))
    (food-menu-previous-item)))

(defun food-menu-next-item ()
  "Move to the next item on the menu, wrapping around if necessary."
  (interactive)
  (end-of-line)
  (save-match-data
    (if (re-search-forward food-menu-item-regexp (point-max) t)
	(re-search-forward "\\s-")
      (progn
	(goto-char (point-min))
	(re-search-forward food-menu-item-regexp (point-max) t)
	(re-search-forward "\\s-")))))

(defun food-menu-next-section ()
  "Move to start of next section on menu, wrapping around if already
at last section.

See also food-menu-jump-to-section \\[food-menu-jump-to-section]."
  (interactive)
  (end-of-line)
  (save-match-data
    (if (looking-at "^\\*\\*\\*.*\\*\\*\\*")
	(progn (re-search-forward "^\\*\\*\\*.*\\*\\*\\*")))
    (if (re-search-forward "^\\*\\*\\*.*\\*\\*\\*" (point-max) t)
	nil
      (progn (goto-char (point-min))
	     (re-search-forward "^\\*\\*\\*.*\\*\\*\\*")))
    (beginning-of-line)))

;; jump to a section of the menu ;;
(defun food-menu-make-section-list (buf)
  (save-excursion
    (set-buffer buf)
    (beginning-of-buffer)
    (save-match-data
      (let ((lst ()))
	;; regexps give me severe indigestion...
	(while (re-search-forward "^\\*\\*\\*.*\\*\\*\\*" (point-max) t)
	  (setq lst
		(cons 
		 (list (buffer-substring
			(progn 
			  (beginning-of-line)
			  (forward-char 4)
			  (point))
			(progn 
			  (end-of-line)
			  (forward-char -4)
			  (point))))
		 lst)))
	lst))))
      
(defun food-menu-jump-to-section (section)
  "doc string here"
  (interactive (let ((secs (food-menu-make-section-list 
                            food-menu-buffer)))
                     (list
                      (completing-read "Jump to section: "
                                       secs
                                       nil
                                       t))))
  (set-buffer food-menu-buffer)
  (beginning-of-buffer)
  (save-match-data
    (goto-char (progn 
		 (re-search-forward
		  (concat 
		   "^\\*\\*\\* "
		   ;; why does this work?
		   ;; why don't I have to
		   ;; take the car of
		   ;; section???
		   section
		   " \\*\\*\\*"))
		 (point)))
    (beginning-of-line)))

;;; this does the obvious. It will need to read the meal buffer, parse
;;; for inserted prices, add, add tax, delivery, etc. Remember to add a
;;; delivery code to menu format...

(defun food-meal-calculate-cost (meal-buffer)
  "Calculate the cost of a given meal buffer BUFFER (a buffer name), 
using the variable food-meal-tax.  It will automatically use the value
of variable food-meal-buffer when called interactively."
  (interactive (list (get-buffer food-meal-buffer)))
  (save-excursion
    (set-buffer (get-buffer meal-buffer))
    ;; see food-meal-delete-item for rationale for rebinding calc-cost
    ;; below: 
    (local-set-key "\C-c\C-c" 'food-meal-calculate-cost)
    (goto-char (point-min))
    (save-match-data
      (if (re-search-forward "^-------" nil t)
	  (delete-region (progn
			   (beginning-of-line)
			   (prog1 (point)
			     (end-of-line)))
			 (progn
			   (re-search-forward "^-------" nil t)
			   (point))))
      (goto-char (point-min))
      (let ((meal-cost food-meal-initial-cost))
	(while (re-search-forward "\\$\\([0-9]*\\.[0-9]*\\)" (point-max) t)
	  (setq meal-cost
		(+ meal-cost 
		   (string-to-number 
		    (buffer-substring (match-beginning 1)
				      (match-end 1))))))
	(goto-char (point-max))
	(beginning-of-line)
	(let ((float-output-format "%.02f"))
	  (progn
	    (insert (concat "-------"
			    "\nSubtotal without tax: " 
			    (number-to-string meal-cost)
			    "\n"))
	    (if (and food-meal-use-tax 
		     (numberp food-meal-tax) 
		     (< 0 food-meal-tax))
		(progn
		  (insert (concat 
			   "Tax: " 
			   (number-to-string (* meal-cost food-meal-tax))))
		  (setq meal-cost 
			(+ meal-cost (* food-meal-tax meal-cost)))
		  (insert (concat "    (Subtotal with tax: " 
				  (number-to-string meal-cost) 
				  ")\n"))))
	    (if (and food-meal-use-tip 
		     (numberp food-meal-tip-percentage) 
		     (< 0 food-meal-tip-percentage))
		(progn
		  (insert (concat 
			   "Tip: " 
			   (number-to-string 
			    (* meal-cost food-meal-tip-percentage))
			   "    (Using "
			   ;; format str to float-output-format seems
			   ;; to be buggy.  Cannot eliminate a decimal
			   ;; point in the output.  Why not?  I think
			   ;; the problem is with Emacs...
			   (let ((float-output-format "%.0f"))
			     (number-to-string
			      (* 100 food-meal-tip-percentage)))
			   "% tip)"
			   "\n"))
		  (setq meal-cost 
			(+ meal-cost (* food-meal-tip-percentage meal-cost)))
		  (insert (concat "Grand total: "
				  (number-to-string meal-cost) 
				  "\n"))))
	    (insert "-------")
	    (display-buffer food-meal-buffer)))))))
  
;;;; mode defining stuff ;;;;

;;; notes:

(defvar food-menu-mode-map (make-sparse-keymap "Food Menu"))
(define-key food-menu-mode-map "n" 'food-menu-next-item)
(define-key food-menu-mode-map " " 'food-menu-next-item)
(define-key food-menu-mode-map "v" 'food-menu-next-item-vegetarian)
(define-key food-menu-mode-map "V" 'food-menu-previous-item-vegetarian)
(define-key food-menu-mode-map "N" 'food-menu-next-section)
(define-key food-menu-mode-map "p" 'food-menu-previous-item)
(define-key food-menu-mode-map "P" 'food-menu-previous-section)
(define-key food-menu-mode-map "c" 'food-meal-calculate-cost)
(define-key food-menu-mode-map "\r" 'food-menu-select-this-item)
(define-key food-menu-mode-map "d" 'food-meal-delete-item)
(define-key food-menu-mode-map "j" 'food-menu-jump-to-section)
(define-key food-menu-mode-map "q" 'food-menu-bury-buffers)
(define-key food-menu-mode-map "s" 're-search-forward)
(define-key food-menu-mode-map "r" 're-search-backward)

(defun food-menu-mode () "Major mode for planning a meal with a
menu-format buffer.  Normally, it sets up a separate buffer called
the \"meal buffer\", which displays the meal being planned.
This includes selected dishes and the total cost \(see variables
food-meal-tax and food-meal-tip-percentage for local
customizations\).  Keybindings in the menu buffer \(not the meal
buffer\) are: 
\\{food-menu-mode-map}

The meal buffer can be edited like any other Emacs buffer -- it does
not have its own major mode.  However, typing C-c C-c in the meal
buffer should recalculate the cost for you.  This is useful if you
have been editing the prices or deleting items."
  (interactive)
  (kill-all-local-variables)
  (use-local-map food-menu-mode-map)
  (let ((orig-buffer (current-buffer)))
    (set-buffer (get-buffer-create food-meal-buffer))
    (local-set-key "\C-c\C-c" 'food-meal-calculate-cost)
    (set-buffer orig-buffer)) 
  (if (not buffer-read-only) (toggle-read-only))
  (setq major-mode 'food-menu-mode)
  (setq mode-name "Food Menu") ; hmmm, hyphen? one word?  see examples...
  (put 'food-menu-mode 'mode-class 'special)
  (run-hooks 'food-menu-mode-hook))

(defun food-menu-bury-buffers ()
  "This buries both the menu and meal buffers, giving them the very 
lowest priority as default choices of buffer to switch to."
  (interactive)
  (bury-buffer food-meal-buffer)
  (delete-other-windows)
  (bury-buffer food-menu-buffer)
  (bury-buffer))

;;; food-menu-mode ends here.
