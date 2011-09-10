;; balance-mode.el

;; Balance -- Major mode for recording transactions and balancing a bank
;;            account.  Use with Crypt++ for some security.  Developed
;;            on FSF GNU Emacs version 19.22 -- other platforms untested.
;;            Author: Jason Baietto, jason@ssd.csd.harris.com 1/12/94
;;
;; Numerous modifications 1/99 Bob Newell
;; **** please send fixes and improvements to bnewell@alum.mit.edu ****
;; Minor bug fixes
;; Added field for transaction-cleared status
;; Added book-balancing calculations
;; Added deposit transaction type
;; Added LaTeX check-printing mode (HIGHLY specific to my printer and my
;;  check forms)
;; *** The original author, Jason Baietto, is NOT responsible for this
;; hacked-up version!  Any errors and problems, poor coding, etc., is
;; the responsibility of Bob Newell (bnewell@alum.mit.edu).  No warranty
;; or support provided.  Jason did all the good stuff, all I did was mess
;; with it.  Thanks to Jason for writing the original.
;;
;; LCD Archive Entry:
;; balance|Jason Baietto|jason@jade.ssd.csd.harris.com|
;; Major mode for recording transactions and balancing a bank account|
;; 09-Feb-1994||~/modes/balance.el.Z|
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc., 675
;; Mass Ave, Cambridge, MA 02139, USA.
;;
;; Put this file in your emacs load path and add the following lines to your
;; .emacs file:
;;
;;    (autoload 'balance-mode "balance")
;;    (setq auto-mode-alist
;;       (append '(("\\.bal$" . balance-mode)) auto-mode-alist))
;;
;; Then, doing a find-file on "sample.bal" will automatically load the balance
;; elisp and enter Balance major mode for the buffer visiting the file.
;; See the balance-mode mode help for details and a quick interactive tutorial.
;;

(require 'calendar)

;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar balance-check-type "check[ \t]+\\([0-9]+\\)"
   "Type field for check transactions.  This is defined separately from
the other transaction types because it is used by functions that perform
special operations on check transactions.")

(defvar balance-transaction-types
   (list
      balance-check-type   ;; Hand written or printout checks.
      "atm"                ;; Automatic teller machine.
      "bank"               ;; Initiated by the bank (fees, interest).
      "deposit"            ;; Non-direct deposits
      "teller"             ;; Human at teller or drive through.
      "direct"             ;; Direct deposit or direct withdrawl.
      "phone"              ;; Initiated over a touch-tone phone.
      "online"             ;; On-line banking transaction.
      "pos"                ;; Point of sale purchase (debit card).
      "")                  ;; This allows blank type fields.
   "List of valid expressions allowed in the transaction type field.
Anything else is an error and will be flagged during recalculation.")

(defvar balance-date-column 0
   "Column where transaction date begins.")

(defvar balance-type-column 9
   "Column where transaction type begins.")

(defvar balance-clear-column 21
    "Column where status appears.")

(defvar balance-description-column 23 
   "Column where transaction description begins.")

(defvar balance-sign-column 55
   "Column where transaction sign begins.")

(defvar balance-amount-column 57
   "Column where transaction amount begins.")

(defvar balance-current-balance-column 69
   "Column where current balance begins.")

(defvar balance-tab-stop-list
   (list
      balance-date-column
      balance-type-column
      balance-clear-column
      balance-description-column
      balance-sign-column
      balance-amount-column
      balance-current-balance-column)
   "List of tab stops that define the start of all transaction fields.
Redefine this in your .emacs file to add or change fields.")

(defvar balance-mode-map nil
   "Keymap for balance buffer.")

(if balance-mode-map
   ()
   (setq balance-mode-map (make-sparse-keymap))
   (define-key balance-mode-map "\C-c\C-b" 'balance-backward-field)
   (define-key balance-mode-map "\C-c\C-c" 'balance-recalculate-buffer)
   (define-key balance-mode-map "\C-c\C-d" 'balance-clear-current-field)
   (define-key balance-mode-map "\C-c\C-f" 'balance-forward-field)
   (define-key balance-mode-map "\C-c\C-n" 'balance-append-next-check)
   (define-key balance-mode-map "\C-c\C-r" 'balance-summarize-checks-region)
   (define-key balance-mode-map "\C-c\C-s" 'balance-summarize-checks-buffer)
   (define-key balance-mode-map "\C-c\C-t" 'balance-append-transaction)
   (define-key balance-mode-map [tab]      'balance-forward-field)
;;   (define-key balance-mode-map [S-tab]    'balance-backward-field)
)

(defvar balance-largest-balance "999999.99"
   "This is only used for formatting purposes.")

(defvar balance-mode-abbrev-table nil
   "Abbrev table used while in balance mode.")

(define-abbrev-table 'balance-mode-abbrev-table nil)

;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun balance-mode()
"Major mode for editing a buffer containing financial transactions.
The following bindings provide the main functionality of this mode:

   \\[balance-append-transaction]\
	Append transaction to end of buffer.
   \\[balance-append-next-check]\
	Append next sequential check transaction to end of buffer.
   \\[balance-recalculate-buffer]\
	Recalculate balance of all transactions in buffer.
   \\[balance-summarize-checks-buffer]\
	Produce summary of all checks in buffer.
   \\[balance-summarize-checks-region]\
	Produce summary of all checks in region.

In addition, these bindings are defined to simplify editing
transactions:

   \\[balance-forward-field]\
	Move forward to start of next field (same as TAB).
   \\[balance-backward-field]\
	Move backward to start of previous field (same as shift-TAB).
   \\[balance-clear-current-field]\
	Clear the current field.

Transactions occur on a single line and have the following fields (in
order):

   date           The transaction date in MM/DD/YY format.
   type           This field must either be blank or match one of the
                  expressions defined in balance-transaction-types.
   clear          Status of transaction, o is open, x is cleared.
   description    A possibly blank transaction description.
   sign           This field must either be \"+\", \"-\" or \"=\".
                  + means credit, - means debit, and = resets balance.
   amount         The transaction amount.  You must enter this field.
   balance        The balance after this transaction.  This field will be
                  computed upon recalculation
(\\[balance-recalculate-buffer]).

Other fields can be defined by redefining the balance-tab-stop-list. Any line
in the buffer that does not begin with a date will be considered a comment and
ignored.  Among other things, this allows the transaction description to span
several lines.

The following lines provide a simple demonstration.  Type
\\[balance-mode] in
this help buffer to enter Balance major mode and then type \
\\[balance-recalculate-buffer].

01/14/93 x            Starting balance                  = 1000
01/14/93 x check 1     Florist                           - 10
01/15/93 x atm         Dinner with Sandy at              - 50
                     The Royal Bombay Club
01/15/93 o check 2     Southern Bell                     - 40.01
01/15/93 o check 3     Florida Power & Light             - 120
01/18/93 x atm         Birthday gift from Bob            + 20
01/19/93 o check 5     Loan shark                        - 799.99

Changing any amount and recalculating again will update all visible balances.
Transactions may be commented out by putting a semi-colon (or any other
character) at the beginning of the line.  This makes it easy to reconcile your
account with the bank, which is usually several transactions behind you.

Balance mode also provides a check summary feature.  Typing \
\\[balance-summarize-checks-buffer] will
produce a summary of all the checks in the buffer.  The transactions will be
sorted by check number and breaks in check sequence will be flagged by lines
with an asterisk on them.  In addition, the checks will be \"balanced\". Type
\\[balance-summarize-checks-buffer] to see a check \
summary for this help buffer.

Entering balance-mode runs the `balance-mode-hooks' if any exist.  There is
also a balance-mode-abbrev-table for the truly warped.
"
   (interactive)
   (setq major-mode 'balance-mode)
   (setq mode-name "Balance")
   (use-local-map balance-mode-map)
   (make-local-variable 'tab-stop-list)
   (setq tab-stop-list balance-tab-stop-list)
   (make-local-variable 'indent-tabs-mode)
   (setq indent-tabs-mode nil)
   (setq indent-line-function 'balance-forward-field)
   (setq local-abbrev-table balance-mode-abbrev-table)
   (overwrite-mode nil)
   (run-hooks 'balance-mode-hooks))

(defun balance-current-line()
"Return the current buffer line at point.  The first line is 0." 
   (save-excursion
      (beginning-of-line)
      (count-lines (point-min) (point))))

(defun balance-forward-field()
"Move the cursor to the next data entry field for the transaction on the
current line."
   (interactive)
;rjn fixed, move-to-tab-stop always returns nil
;   (if (not 
(move-to-tab-stop))
;      (move-to-column (car tab-stop-list))))

(defun balance-last (list)
"Return last element in a list."
   (cond
      ((null list) '())
      ((null (cdr list)) (car list))
      (t (balance-last (cdr list)))))

(defun balance-find-largest-less-than(list item)
"Search a sorted LIST of numbers, return the largest number that is still less
than ITEM."
   (let ((list-car (car list))
         (list-cdr (cdr list))
         (last nil))
      (while (and list-car (< list-car item))
         (setq last list-car)
         (setq list-car (car list-cdr))
         (setq list-cdr (cdr list-cdr)))
      last))

(defun balance-find-largest-less-than-equal(list item)
"Search a sorted LIST of numbers, return cdr of the list starting with the
largest number that is less than or equal to ITEM."
   (let ((list-car (car list))
         (list-cdr (cdr list))
         (last nil))
      (while (and list-car (<= list-car item))
         (setq last (cons list-car list-cdr))
         (setq list-car (car list-cdr))
         (setq list-cdr (cdr list-cdr)))
      last))

(defun balance-find-field(column)
"Return a list of the start and end of the field around COLUMN.  End may be
nil if column is after the last defined tab stop."
   (let ((field (balance-find-largest-less-than-equal
                     balance-tab-stop-list column)))
      (if (equal 1 (length field))
         (list (car field) nil)
         (list (nth 0 field) (nth 1 field)))))

(defun balance-backward-field()
"Move the cursor to the previous data entry field for the transaction on the
current line."
   (interactive)
   (let* ((col (current-column))
          (prev (balance-find-largest-less-than balance-tab-stop-list col)))
      (if prev
         (move-to-column prev)
         (move-to-column (balance-last balance-tab-stop-list)))))

(defun balance-clear-current-field()
"Reset the field around point to contain only spaces and position point at
the beginning of the field."
   (interactive)
   (let* ((field (balance-find-field (current-column)))
         (line-start (progn (beginning-of-line) (point)))
         (line-end (progn
                      (end-of-line)
                      (untabify line-start (point))
                      (point)))
         (field-start (+ line-start (nth 0 field)))
         (field-end (if (nth 1 field)
                        (+ line-start (nth 1 field))
                        line-end)))
      (clear-rectangle field-start field-end)
      (goto-char field-start)))

(defun balance-append-transaction()
"Add a transaction to the end of current buffer using today's date."
   (interactive)
   (goto-char (point-max))
   (if (not (equal 0 (current-column)))
      (newline))
   (let* ((date (calendar-current-date))
          (month (car date))
          (day (car (cdr date)))
          (year (mod (car (cdr (cdr date))) 100)))
   (insert (format "%02d/%02d/%02d" month day year))
   (move-to-tab-stop)))

(defun balance-append-next-check()
"Add a check transaction to the end of the current buffer using today's date.
Inserts the check number following the last check number written into the
transaction type column.  Loses if you write checks out of order."
   (interactive)
   (balance-append-transaction)
   (let (check check-number)
      (save-excursion
         (if (search-backward-regexp balance-check-type 0 t)
            (setq check (buffer-substring (match-beginning 1) (match-end 1)))
            (error "no previous checks found")))
      (setq check-number (1+ (string-to-number check)))
      (move-to-column balance-type-column)
      (if (< (current-column) balance-type-column)
         (indent-to-column balance-type-column))
      (insert (format "check %d" check-number))
      (move-to-tab-stop)))

(defun balance-build-type-regexp()
"Return a regular expression that will match any valid transaction type.
This is done dynamically so users can redefine the valid transactions in
their .emacs files even after this file has been loaded."
   (concat
      "^\\("
      (mapconcat (lambda(x) x) balance-transaction-types "\\|")
       "\\)[ \t]*$"))

(defun balance-check-transaction-type(line-start)
"Given starting POINT of a transaction line, determine if the transaction
type specified is one of the valid defined transaction types.  An error
is produced if the transaction type is invalid, otherwise returns nil."
   (let* ((type-regexp (balance-build-type-regexp))
         (type-start (+ line-start balance-type-column))
         (type-end (+ line-start balance-clear-column))
         (type-string (buffer-substring type-start type-end)))
      (if (string-match type-regexp type-string)
         nil
         (error "line %d, invalid type: %s"
            (1+ (balance-current-line)) type-string))))

(defun balance-find-next-transaction()
"Find next line that looks like a complete transaction and return a list of
the line start, numeric data start and line end points."
   (let ((found nil)
         (line-regexp "^[0-9]*/[0-9]*/[0-9]*.*$")
         line-start
         line-end
         data-start)
      (while (and
               (not found)
               (search-forward-regexp line-regexp (point-max) t))
         (setq line-start (match-beginning 0))
         (untabify (match-beginning 0) (match-end 0))
         (setq line-end (progn (end-of-line) (point)))
         (setq data-start (+ line-start balance-sign-column))
         (if (> line-end data-start)
               (setq found t)))
      (if found
         (list line-start data-start line-end)
         nil)))

(defun balance-parse-transaction-data(data)
"Given a STRING representing the sign, amount and optionally balance of a
transaction, return a list of the sign and amount and balance as floating
point numbers.  Balance is nil if not present."
   (let ((data-regexp "\\([-+=p]\\)[ \t]*\\([0-9.]+\\)?[ \t]*\\([-]?[0-9.]+\\)?")
         (balance nil)
         (reset nil)
	 (printit nil)
         string sign amount)
      (string-match data-regexp data)
      (if (match-beginning 1)
         (setq sign (substring data (match-beginning 1) (match-end 1)))
         (error "line %d, missing sign" (1+ (balance-current-line))))
      (if (equal sign "p")
	  (progn 
	    (setq printit t)
	    (setq sign "-")))
      (if (equal "=" sign)
         (progn
            (setq sign "+")
            (setq reset t)))
      (if (match-beginning 2)
         (progn
            (setq string (substring data (match-beginning 2) (match-end 2)))
            (setq amount (string-to-number (concat sign string))))
         (error "line %d, missing amount" (1+ (balance-current-line))))
      (if (match-beginning 3)
         (progn
            (setq string (substring data (match-beginning 3) (match-end 3)))
            (setq balance (string-to-number string))))
      (if reset (setq sign "="))
      (if printit (setq sign "p"))
      (list sign amount balance)))

(defun balance-same(amount1 amount2)
"Compare two dollar amounts for equivalence.  This is necessary due to the
imprecision of the float implementation in emacs 19."
(let ((string1 (format "%10.2f" amount1))
      (string2 (format "%10.2f" amount2)))
  (equal string1 string2)))

(defun balance-form-transaction-data(sign amount balance)
"Given SIGN, AMOUNT and a BALANCE, return a string suitable for placing in the
numeric region of a transaction, based on the defined input columns."
; rjn
;   (if (equal sign "p")
;       (setq sign "-"))
   (let* ((amount (abs amount))
         (width1 (- balance-amount-column balance-sign-column))
         (width2 (- balance-current-balance-column balance-amount-column))
         (len (length balance-largest-balance))
         (gap (- width2 len))
         (value (concat "%" (number-to-string len) ".2f"))
         (format-string (concat "%-" (number-to-string width1) 
				"s" value 
				"%" (number-to-string gap) "s" value)))
      (format format-string sign amount "" balance)))

(defun number-it (the_num)
"Returns a string giving words for a number from 1 to 99, recursive!"
  (let* ( (num-string ""))
  (cond
   ( (>=  the_num 90)
     (setq num-string "Ninety ")
     (setq num-string (concat num-string (number-it (% the_num 10)))))
   ( (>=  the_num 80)
     (setq num-string "Eighty ")
     (setq num-string (concat num-string (number-it (% the_num 10)))))
   ( (>=  the_num 70)
     (setq num-string "Seventy ")
     (setq num-string (concat num-string (number-it (% the_num 10)))))
   ( (>=  the_num 60)
     (setq num-string "Sixty ")
     (setq num-string (concat num-string (number-it (% the_num 10)))))
   ( (>=  the_num 50)
     (setq num-string "Fifty ")
     (setq num-string (concat num-string (number-it (% the_num 10)))))
   ( (>=  the_num 40)
     (setq num-string "Forty ")
     (setq num-string (concat num-string (number-it (% the_num 10)))))
   ( (>=  the_num 30)
     (setq num-string "Thirty ")
     (setq num-string (concat num-string (number-it (% the_num 10)))))
   ( (>=  the_num 20)
     (setq num-string "Twenty ")
     (setq num-string (concat num-string (number-it (% the_num 10)))))
   ( (= 19 the_num)
     (setq num-string "Nineteen "))
   ( (= 18 the_num)
     (setq num-string "Eighteen "))
   ( (= 17 the_num)
     (setq num-string "Seventeen "))
   ( (= 16 the_num)
     (setq num-string "Sixteen "))
   ( (= 15 the_num)
     (setq num-string "Fifteen "))
   ( (= 14 the_num)
     (setq num-string "Fourteen "))
   ( (= 13 the_num)
     (setq num-string "Thirteen "))
   ( (= 12 the_num)
     (setq num-string "Twelve "))
   ( (= 11 the_num)
     (setq num-string "Eleven "))
   ( (= 10 the_num)
     (setq num-string "Ten "))
   ( (=  9 the_num)
     (setq num-string "Nine "))
   ( (=  8 the_num)
     (setq num-string "Eight "))
   ( (=  7 the_num)
     (setq num-string "Seven "))
   ( (=  6 the_num)
     (setq num-string "Six "))
   ( (=  5 the_num)
     (setq num-string "Five "))
   ( (=  4 the_num)
     (setq num-string "Four "))
   ( (=  3 the_num)
     (setq num-string "Three "))
   ( (=  2 the_num)
     (setq num-string "Two "))
   ( (=  1 the_num)
     (setq num-string "One "))
  )
   num-string
))

(defun print-make-words (bucks)
"Returns a string giving the amount of bucks in words suitable for 
check writing.  Limited to under a hundred grand!  Code inspired by
original Inform library code of Graham Nelson."
(let* (
      (words "")
      (dollars (truncate bucks))
; avoid floating point errors
      (cents (- (* 100 bucks) (* 100 dollars)))
)
(if (>= dollars 100000)
    (error "Checks over 99999.99 not supported!"))
(if (>= dollars 1000)
    (progn
      (setq words (concat words (number-it (truncate (/ dollars 1000))) 
"Thousand "))
      (setq dollars (% dollars 1000))))
(if (>= dollars 100)
    (progn
      (setq words (concat words (number-it (truncate (/ dollars 100)))
"Hundred "))
      (setq dollars (% dollars 100))))
(if (>= dollars 0)
    (progn
      (setq words (concat words (number-it dollars)))))
(if (> cents 0)   ; still a rounding problem with cents
    (setq words (concat words "and " (format "%02d" cents) "/100"))
    (setq words (concat words "and 00/100")))
 words
)
)

(defun balance-print-check (data-string amount check-count)
"Output this check for printing."
(let* (
  (print-buffer (get-buffer-create "*Print Checks*"))
  (print-date (substring data-string 0 balance-type-column))
  (print-payto (substring data-string balance-description-column
balance-sign-column))
  (print-memo "")   
  (note-pos nil)
  (decimal-amount (format "%.2f" (abs amount)))
  (print-decimal-amount (print-make-words (abs amount)))
)
   (if (setq note-pos (string-match "|" print-payto))
      (progn
	(setq print-memo (substring print-payto (1+ note-pos)))
	(setq print-payto (substring print-payto 0 note-pos))))
 (save-excursion
   (set-buffer print-buffer)
   (end-of-buffer)
   (if (= check-count 1)
    (progn 
     (insert "\\documentclass[12pt]{article}\n")
     (insert "% ********************************************\n")
     (insert "% This is hardwired for Bob Newell's printer and check forms\n")
     (insert "% There are just too many variables to easily parameterize\n")
     (insert "% TO PRINT YOUR CHECKS:\n")
     (insert "% --Save this buffer with a tex extension \(i.e. ctrl-x ctrl-s foo.tex\)\n")
     (insert "% --Run LaTeX \(ctrl-c ctrl-c if you have AucTeX set up, otherwise use a shell\)\n")
     (insert "% --Run dvips or dvilj in a shell, or whatever you normally do to\n")
     (insert "%   make a print file from LaTeX output\n")
     (insert "% --Print on your check forms as usual.\n")
     (insert "% THIS IS SET UP FOR THREE-TO-PAGE CHECKS\n")
     (insert "% It will BOMB OUT if you print non-multiples of 3!\n")
     (insert "% On different printers you only need mess with the picture offset\n")
     (insert "% For different check forms it's a whole new game\n")
     (insert "% It takes about an hour to measure and fine tune everything\n")
     (insert "% ********************************************\n")
     (insert "\\oddsidemargin -1in\n")
     (insert "\\topmargin 0in\n")
     (insert "\\textwidth 7.5in\n")
     (insert "\\textheight = 55\\baselineskip\n")
     (insert "\\begin{document}\n")
     (insert "\\unitlength=1in\n")
    )) 
   (if (= (% check-count 3) 1)
    (progn
     (insert "\\begin{picture}\(8.5,10.5\)\(0.37,-1.15\)\n")
     (insert "%date\n")
     (insert "  \\put(7.0,10.0\){\\makebox\(0,0\)[l]{")
     (insert print-date)
     (insert "}}\n")
     (insert "%payee\n")
     (insert "  \\put\(1.2,9.45\){\\makebox\(0,0\)[l]{")
     (insert print-payto)
     (insert "}}\n")
     (insert "%amount in words\n")
     (insert "  \\put\(0.5,9.15\){\\makebox\(0,0\)[l]{")
     (insert print-decimal-amount)
     (insert "}}\n")
     (insert "%amount in decimal\n")
     (insert "  \\put\(7.0,9.5\){\\makebox\(0,0\)[l]{")
     (insert decimal-amount)
     (insert "}}\n")
     (insert "%memo\n")
     (insert "  \\put\(0.7,8.1\){\\makebox\(0,0\)[l]{")
     (insert print-memo)
     (insert "}}\n")
))
   (if (= (% check-count 3) 2)
    (progn
     (insert "%date\n")
     (insert "  \\put\(7.0,6.5\){\\makebox\(0,0\)[l]{")
     (insert print-date)
     (insert "}}\n")
     (insert "%payee\n")
     (insert "  \\put\(1.2,5.95\){\\makebox\(0,0\)[l]{")
     (insert print-payto)
     (insert "}}\n")
     (insert "%amount in words\n")
     (insert "  \\put\(0.5,5.65\){\\makebox\(0,0\)[l]{")
     (insert print-decimal-amount)
     (insert "}}\n")
     (insert "%amount in decimal\n")
     (insert "  \\put\(7.0,6.0\){\\makebox\(0,0\)[l]{")
     (insert decimal-amount)
     (insert "}}\n")
     (insert "%memo\n")
     (insert "  \\put\(0.7,4.6\){\\makebox\(0,0\)[l]{")
     (insert print-memo)
     (insert "}}\n")
))
   (if (= (% check-count 3) 0)
    (progn
     (insert "%date\n")
     (insert "  \\put\(7.0,3.0\){\\makebox\(0,0\)[l]{")
     (insert print-date)
     (insert "}}\n")
     (insert "%payee\n")
     (insert "  \\put\(1.2,2.45\){\\makebox\(0,0\)[l]{")
     (insert print-payto)
     (insert "}}\n")
     (insert "%amount in words\n")
     (insert "  \\put\(0.5,2.15\){\\makebox\(0,0\)[l]{")
     (insert print-decimal-amount)
     (insert "}}\n")
     (insert "%amount in decimal\n")
     (insert "  \\put\(7.0,2.5\){\\makebox\(0,0\)[l]{")
     (insert decimal-amount)
     (insert "}}\n")
     (insert "%memo\n")
     (insert "  \\put\(0.7,1.1\){\\makebox\(0,0\)[l]{")
     (insert print-memo)
     (insert "}}\n")
     (insert "\\end{picture}\n")
     (insert "\\newpage\n")
))
)
))

(defun balance-recalculate(start end)
"Go through the region specified by START and END and recalculate all
transaction balances.  The final balance, uncleared total, and 
the number of balances that changed, and the transaction count are 
returned in a list."
   (let ((current-balance 0)
         (changes 0)
	 (uncleared 0)
;	 (check-count 0)
         (transactions 0)
         line-points)
      (save-excursion (save-restriction
         (narrow-to-region start end)
         (goto-char (point-min))
         (while (setq line-points (balance-find-next-transaction))
            (setq transactions (1+ transactions))
            (let* ((line-start (nth 0 line-points))
                  (data-start (nth 1 line-points))
                  (data-end (nth 2 line-points))
		  (clear-flag (buffer-substring 
                               (+ line-start balance-clear-column) 
                               (+ 1 line-start balance-clear-column)))
                  (data-string (buffer-substring data-start data-end))
;                  (full-string (buffer-substring line-start data-end))  
                  (data-values (balance-parse-transaction-data data-string))
                  (sign (nth 0 data-values))
; rjn
;		  (printit (if (equal sign "p")
;			     t
;			     nil))
; rjn
                  (amount (nth 1 data-values))
                  (balance (nth 2 data-values))
                  (new-balance (if (equal sign "=")
                                  amount
                                  (+ current-balance amount)))
		  (new-uncleared (if (equal clear-flag "x")
				 uncleared
			          (+ uncleared amount)))
                  (new-string
                     (balance-form-transaction-data sign amount new-balance)))
               (balance-check-transaction-type line-start)
               (setq current-balance new-balance)
	       (setq uncleared new-uncleared)
               (if (or (null balance) (not (balance-same balance new-balance)))
                  (setq changes (1+ changes)))
;	       (if printit
;		   (progn
;		     (setq check-count (1+ check-count))
;		     (balance-print-check full-string amount check-count)
;		     ))
               (if (not (equal data-string new-string))
                  (progn
                     (delete-region data-start data-end)
                     (goto-char data-start)
                     (insert new-string)))))
         (widen)))
;;      (if (> check-count 0)
;;	  (progn
;;	    (set-buffer "*Print Checks*")
;;;	    (end-of-buffer)
;;;	    (insert "\\end{document}\n")
;;	    (beginning-of-buffer)
;;	    (latex-mode)
;;	    (switch-to-buffer "*Print Checks*")
;;	    ))
      (list current-balance uncleared changes transactions)))

(defun balance-recalculate-buffer()
"Recalculate the current buffer.  See balance-recalculate."
   (interactive)
   (let* ((result (balance-recalculate (point-min) (point-max)))
         (balance (nth 0 result))
	 (uncleared (nth 1 result))
         (changes (nth 2 result))
         (total (nth 3 result)))
      (message 
       (format "book bal %.2f unclrd %.2f bank bal %.2f (%d/%d recalcs)"
               balance uncleared (- balance uncleared) changes total))
      (if (> changes 0)
         (end-of-line))))

(defun balance-recalculate-region(start end)
"Recalculate the current region.  See balance-recalculate."
   (interactive "r")
   (let* ((result (balance-recalculate start end))
         (balance (nth 0 result))
	 (uncleared (nth 1 result))
         (changes (nth 2 result))
         (total (nth 3 result)))
     (message (format "region balance %.2f uncleared %.2f (%d/%d recalcs)"
                      balance uncleared changes total))
      (if (> changes 0)
         (end-of-line))))

(defun balance-summarize-checks(start end)
"Create a *checks* buffer that lists only the checks in the specified region.
The list is sorted on check number.  Breaks in sequence are denoted by lines
containing an asterisk between the checks where the break occurs.  The buffer
is also recalculated, thus showing to total of the checks summarized."
   (let ((balance-buffer (current-buffer))
         (summary-buffer (get-buffer-create "*check summary*"))
         (check-count 0)
         (sequence-breaks 0)
         line-points)
      (save-excursion (save-restriction
         (set-buffer summary-buffer)
         (delete-region (point-min) (point-max))
         (set-buffer balance-buffer)
         (narrow-to-region start end)
         (goto-char (point-min))
         (while (setq line-points (balance-find-next-transaction))
            (let* ((line-start (nth 0 line-points))
                  (line-end (nth 2 line-points))
                  (type-start (+ line-start balance-type-column))
                  (type-end (+ line-start balance-description-column))
                  (type-string (buffer-substring type-start type-end)))
               (if (string-match balance-check-type type-string)
                  (progn
                     (append-to-buffer
                        summary-buffer line-start (1+ line-end))
                     (setq check-count (1+ check-count))))))
         (widen)
         (set-buffer summary-buffer)
         (sort-numeric-fields 3 (point-min) (point-max))
         (setq sequence-breaks (balance-find-sequence-breaks))
         (goto-char (point-max))
         (insert (format "\n%d check%s summarized, %d sequence break%s\n"
                     check-count
                     (if (equal 1 check-count) "" "s")
                     sequence-breaks
                     (if (equal 1 sequence-breaks) "" "s")))
         (set-buffer balance-buffer)))
      (pop-to-buffer summary-buffer)
      (goto-char (point-max))))

(defun balance-find-sequence-breaks()
"Find check sequence breaks in the current check summary buffer.  Mark breaks
in sequence by inserting a line with an asterisk between the offending checks.
Return the count of sequence breaks found."
   (let ((last-check nil)
         (sequence-breaks 0))
      (goto-char (point-min))
      (while (search-forward-regexp
               (concat "\\([0-9/]+\\)[ \t]+" balance-check-type)
               (point-max) t)
         (let* ((check-start (match-beginning 2))
               (check-end (match-end 2))
               (check-string (buffer-substring check-start check-end))
               (check-number (string-to-int check-string)))
         (if (not last-check)
            (setq last-check check-number)
            (if (not (equal check-number (1+ last-check)))
               (progn
                  (setq sequence-breaks (1+ sequence-breaks))
                  (beginning-of-line)
                  (open-line 1)
                  (insert "*")
                  (next-line 2)))
            (setq last-check check-number))))
      sequence-breaks))

(defun balance-summarize-checks-buffer()
   (interactive)
   (balance-summarize-checks (point-min) (point-max))
   (balance-recalculate (point-min) (point-max)))

(defun balance-summarize-checks-region(start end)
   (interactive "r")
   (balance-summarize-checks start end)
   (balance-recalculate (point-min) (point-max)))

(provide 'balance)
