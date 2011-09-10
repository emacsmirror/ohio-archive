;From ark1!nems!mimsy!dftsrv!ukma!tut.cis.ohio-state.edu!JULIET.LL.MIT.EDU!neilm Wed Dec  6 12:18:20 1989
;Article 791 of gnu.emacs:
;Path: ark1!nems!mimsy!dftsrv!ukma!tut.cis.ohio-state.edu!JULIET.LL.MIT.EDU!neilm
;>From neilm@JULIET.LL.MIT.EDU
;Newsgroups: gnu.emacs
;Subject: Appointment notification in GNU emacs
;Message-ID: <8911291444.AA02912@horatio>
;Date: 29 Nov 89 14:44:52 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 525
;
;
;        appt.el is an appointment notification system, to be used
;        in conjunction Edward M. Reingold's calendar.el. It
;        will notify users of pending appointments based upon their
;        diary file (see calendar.el if your not sure about a diary
;        file). The header of the file below describes what needs
;        to be in your .emacs file in order to use this.
;
;
;==========================
;Neil Mager    <neilm@juliet.ll.mit.edu>
;Office        (617) 981-4803
;Dumb Quote
;"Necessity is the mother of invention...Laziness is the mother of necessity"
;
;############################## CUT HERE ########################################
;;
;; appt.el - visable and/or audible notification of
;;           appointments from ~/diary file generated from
;;           Edward M. Reingold's calendar.el.
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
;;
;;; This file will alert the user of a pending appointment based on their
;;; diary file.
;;;
;;; ******* It is necessary to invoke 'display-time' ********
;;; *******  and 'diary' for this to work properly.  ********
;;; 
;;; A message will be displayed in the mode line of the emacs buffer
;;; and (if the user desires) the terminal will beep and display a message
;;; from the diary in the mini-buffer, or the user may select to 
;;; have a message displayed in a new buffer.
;;;
;;; The variable 'appt-message-warning-time' allows the
;;; user to specify how much notice they want before the appointment. The 
;;; variable 'appt-issue-message' specifies whether the user wants
;;; to to be notified of a pending appointment.
;;; 
;;; In order to use, the following should be in your .emacs file in addition to
;;; creating a diary file and invoking calendar:
;;;
;;;    Set some options
;;; (setq view-diary-entries-initially t)
;;; (setq issue-appointments-message t)
;;;
;;;   The following three lines are required:
;;; (display-time)
;;; (autoload 'appt-make-list "appt.el" nil t)
;;; (setq list-diary-entries-hook 
;;;     (list 'appt-make-list 'prepare-fancy-diary-buffer))
;;;
;;; 
;;;  This is an example of what can be in your diary file:
;;; Monday
;;;   9:30am Coffee break
;;;  12:00pm Lunch        
;;; 
;;; Based upon the above lines in your .emacs and diary files, 
;;; the calendar and diary will be displayed when you enter
;;; emacs and your appointments list will automatically be created.
;;; You will then be reminded at 9:20am about your coffee break
;;; and at 11:50am to go to lunch. 
;;;
;;; Use describe-function on appt-check for a description of other variables
;;; that can be used to personalize the notification system.
;;;
;;;  In order to add or delete items from todays list, use appt-add
;;;  and appt-delete.
;;;
;;;  Additionally, the appointments list is recreated automatically
;;;  at 12:01am for those who do not logout every day or are programming
;;;  late.
;;;
;;; Brief internal description - Skip this if your not interested!
;;;
;;; The function appt-check is run from the 'loadst' process which is started
;;; by invoking (display-time). A function below modifies display-time-filter 
;;; (from original time.el) to include a hook which will invoke appt-check.
;;;
;;;  NOTE: If this is included in the gnuemacs distribution, the original
;;;        time.el should be modified.
;;;
;;; The function appt-make-list creates the appointments list which appt-check
;;; reads. This is all done automatically.
;;; It is invoked from the function list-diary-entries.
;;;
(defvar appt-issue-message t
  "*If T, the diary buffer is checked for appointments. For an
appointment warning to be made, the time must be the first thing on
the line.")

(defvar appt-message-warning-time 10
  "*The amount of time in minutes before the meeting that the warning
begins.")

(defvar appt-audible t
  "*Variable used to determine if appointment is audible.")

(defvar appt-visable t
  "*Variable used to determine if appointment message should be displayed
in the mini-buffer.")

(defvar appt-display-mode-line t
  "*Variable used to determine if minutes to appointment and time
should be displayed on the mode line.")

(defvar appt-msg-window t
 "*Variable used to determine if appointment message
should temporarily appear in another window.")

(defvar appt-display-duration 5
  "*The number of seconds an appointment message
is displayed in another window.")

(defvar appt-time-msg-list nil
  "The list of appointments for today. Use appt-add and appt-delete
to add and delete appointments from list. The original list is generated
from the today's diary-entries-list. The number before each time/message
is the time in minutes from midnight.")

(defconst max-time 1439
  "11:59pm in minutes - number of minutes in a day minus 1.")

(defun appt-check ()
  "Check for an appointment and update the mode line and minibuffer if
desired. Note: the time must be the first thing in the line in the diary
for a warning to be issued.

The format of the time can be either 24 hour or am/pm.
Example: 

               02/23/89
                 18:00 Dinner
            
              Thursday
                11:45am Lunch meeting.

The following variables control the action of the notification:

appt-issue-message
        If T, the diary buffer is checked for appointments.

appt-message-warning-time
       Variable used to determine if appointment message
        should be displayed.

appt-audible
        Variable used to determine if appointment is audible.
        Default is t.

appt-visable
        Variable used to determine if appointment message should be
        displayed in the mini-buffer. Default is t.

appt-msg-window
       Variable used to determine if appointment message
       should temporarily appear in another window. Mutually exclusive
       to appt-visable.

appt-display-duration
      The number of seconds an appointment message
      is displayed in another window.

This function is run from the loadst process for display time.
Therefore, you need to have (display-time) in your .emacs file."
  
  (if (and appt-issue-message appt-time-msg-list)
      (let ((min-to-app -1)
            (new-time ""))
        (save-excursion
          
          ;; Get the current time and convert it to minutes
          ;; from midnight. ie. 12:01am = 1, midnight = 0.
          
          (let* ((cur-hour(string-to-int 
                           (substring (current-time-string) 11 13)))
                 (cur-min (string-to-int 
                           (substring (current-time-string) 14 16)))
                 (cur-comp-time (+ (* cur-hour 60) cur-min)))
            
            ;; If the time is 12:01am, we should update our 
            ;; appointments to todays list.
            
            (if (= cur-comp-time 1)
                (let* ((tmp-list-diary-entries-hook
                        list-diary-entries-hook))
                  (setq list-diary-entries-hook nil)
                  (setq diary-entries-list (diary))
                  (setq list-diary-entries-hook
                        tmp-list-diary-entries-hook)
                  (appt-make-list)))
            
            ;; Get the first time off of the list
            ;; and calculate the number of minutes until
            ;; the appointment.
            
            (let ((appt-comp-time (car (car (car appt-time-msg-list)))))
              (setq min-to-app (- appt-comp-time cur-comp-time))

              (while (and appt-time-msg-list (< appt-comp-time cur-comp-time))
                (setq appt-time-msg-list (cdr appt-time-msg-list)) 
                (if appt-time-msg-list
                    (setq appt-comp-time (car (car (car appt-time-msg-list))))))
              
              ;; If we have an appointment between midnight and
              ;; 'appt-message-warning-time' minutes after midnight,
              ;; we must begin to issue a message before midnight.
              ;; Midnight is considered 0 minutes and 11:59pm is
              ;; 1439 minutes. Therefore we must recalculate the minutes
              ;; to appointment variable. It is equal to the number of 
              ;; minutes before midnight plus the number of 
              ;; minutes after midnight our appointment is.
              
              (if (and (< appt-comp-time appt-message-warning-time)
                       (> (+ cur-comp-time appt-message-warning-time)
                          max-time))
                  (setq min-to-app (+ (- (1+ max-time) cur-comp-time))
                        appt-comp-time))
              
              ;; issue warning if the appointment time is 
              ;; within appt-message-warning time
              
              (if (and (<= min-to-app appt-message-warning-time)
                       (>= min-to-app 0))
                  (progn
                    (if appt-msg-window
                        (progn
                          (string-match
                           "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?" 
                           display-time-string)
                          
                          (setq new-time (substring display-time-string 
                                                    (match-beginning 0)
                                                    (match-end 0)))
                          (appt-disp-window min-to-app new-time
                                            (car (cdr (car
                                                       appt-time-msg-list)))))
                      ;;; else

                      (if appt-visable
                          (message "%s" (car (cdr (car appt-time-msg-list)))))
                      
                      (if appt-audible
                          (beep 1)))

                      (if appt-display-mode-line
                          (progn
                            (string-match
                             "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?" 
                             display-time-string)
                            
                            (setq new-time (substring display-time-string 
                                                      (match-beginning 0)
                                                      (match-end 0)))
                            (setq display-time-string
                                  (concat  "App't in "
                                           min-to-app " min. " new-time " "))
                          
                            ;; force mode line updates - from time.el
                            
                            (save-excursion (set-buffer (other-buffer)))
                            (set-buffer-modified-p (buffer-modified-p))
                            (sit-for 0)))
                                                            
                    (if (= min-to-app 0)
                        (setq appt-time-msg-list
                              (cdr appt-time-msg-list)))))))))))


(defun appt-disp-window (min-to-app new-time appt-msg)
" Displays appointment message in a 
seperate buffer."
  (require 'electric)
  (save-window-excursion

    ;; Make sure we're not in the minibuffer
    ;; before splitting the window.

    (if (= (screen-height)
           (nth 3 (window-edges (selected-window))))
        nil
      (select-lowest-window)
      (split-window))

    (let* ((this-buffer (current-buffer))
           (appt-disp-buf (set-buffer (get-buffer-create "appt-buf"))))
      (setq mode-line-format 
            (concat "-------------------- Appointment in "
                    min-to-app " minutes. " new-time " %-"))
      (pop-to-buffer appt-disp-buf)
      (insert-string appt-msg)
      (shrink-window-if-larger-than-buffer (get-buffer-window appt-disp-buf))
      (set-buffer-modified-p nil)
      (if appt-audible
          (beep 1))
      (sit-for appt-display-duration)
      (if appt-audible
          (beep 1))
      (kill-buffer appt-disp-buf))))


(defun select-lowest-window ()
  " Determines which window is the lowest one being
displayed and selectes that one."
  (setq lowest-window (selected-window))
  (let* ((bottom-edge (car (cdr (cdr (cdr (window-edges))))))
         (last-window (previous-window))
         (window-search t))
    (while window-search
      (let* ((this-window (next-window))
             (next-bottom-edge (car (cdr (cdr (cdr 
                                               (window-edges this-window)))))))
        (if (< bottom-edge next-bottom-edge)
            (progn
              (setq bottom-edge next-bottom-edge)
              (setq lowest-window this-window)))

        (select-window this-window)
        (if (eq last-window this-window)
            (progn
              (select-window lowest-window)
              (setq window-search nil)))))))


(defun appt-add (new-appt-time new-appt-msg)
  "Adds an appointment to the list of appointments for the day at TIME
and issue MESSAGE. The time should be in either 24 hour format or
am/pm format. "

  (interactive "sTime (hh:mm[am/pm]): \nsMessage: ")
  (if (string-match "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?" new-appt-time)
      nil
    (error "Unacceptable time-string"))
  
  (let* ((appt-time-string (concat new-appt-time " " new-appt-msg))
         (appt-time (list (appt-convert-time new-appt-time)))
         (time-msg (cons appt-time (list appt-time-string))))
    (setq appt-time-msg-list (append appt-time-msg-list
                                     (list time-msg)))
    (setq appt-time-msg-list (appt-sort-list appt-time-msg-list)))) 


(defun appt-delete ()
  "Deletes an appointment from the list of appointments."
  (interactive)
  (let* ((tmp-msg-list appt-time-msg-list))
    (while tmp-msg-list
      (let* ((element (car tmp-msg-list))
             (prompt-string (concat "Delete " 
                                    (prin1-to-string (car (cdr element))) 
                                    " from list? "))
             (test-input (y-or-n-p prompt-string)))
        (setq tmp-msg-list (cdr tmp-msg-list))
        (if test-input
            (setq appt-time-msg-list (delq element appt-time-msg-list)))
        (setq tmp-appt-msg-list nil)))
    (message "")))
                 

(defun appt-make-list ()
  "Create the appointments list from todays diary buffer.
The time must be at the beginning of a line for it to be
put in the appointments list.

               02/23/89
                 12:00pm lunch

                Wednesday
                  10:00am group meeting"

  (setq appt-time-msg-list nil)

  (save-excursion
    (fix-time)
    (if diary-entries-list

        ;; Cycle through the entry-list (diary-entries-list)
        ;; looking for entries beginning with a time. If 
        ;; the entry begins with a time, add it to the
        ;; appt-time-msg-list. Then sort the list.
        
        (let ((entry-list diary-entries-list)
              (new-time-string ""))
          (while (and entry-list 
                      (calendar-date-equal 
                       (calendar-current-date) (car (car entry-list))))
            (let ((time-string (substring (prin1-to-string 
                                           (cdr (car entry-list))) 2 -2)))
              
              (while (string-match
                      "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?.*" 
                      time-string)
                (let* ((appt-time-string (substring time-string
                                                    (match-beginning 0)
                                                    (match-end 0))))

                  (if (< (match-end 0) (length time-string))
                      (setq new-time-string (substring time-string 
                                                       (+ (match-end 0) 1)
                                                       nil))
                    (setq new-time-string ""))
                  
                  (string-match "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?"
                                time-string)
                    
                  (let* ((appt-time (list (appt-convert-time 
                                           (substring time-string
                                                      (match-beginning 0)
                                                      (match-end 0)))))
                         (time-msg (cons appt-time
                                         (list appt-time-string))))
                    (setq time-string new-time-string)
                    (setq appt-time-msg-list (append appt-time-msg-list
                                                     (list time-msg)))))))
            (setq entry-list (cdr entry-list)))))
  (setq appt-time-msg-list (appt-sort-list appt-time-msg-list))

  ;; Get the current time and convert it to minutes
  ;; from midnight. ie. 12:01am = 1, midnight = 0,
  ;; so that the elements in the list
  ;; that are earlier than the present time can
  ;; be removed.
  
  (let* ((cur-hour(string-to-int 
                   (substring (current-time-string) 11 13)))
         (cur-min (string-to-int 
                   (substring (current-time-string) 14 16)))
         (cur-comp-time (+ (* cur-hour 60) cur-min))
         (appt-comp-time (car (car (car appt-time-msg-list)))))

    (while (and appt-time-msg-list (< appt-comp-time cur-comp-time))
      (setq appt-time-msg-list (cdr appt-time-msg-list)) 
      (if appt-time-msg-list
          (setq appt-comp-time (car (car (car appt-time-msg-list)))))))))
  

(defun appt-sort-list (appt-list)
  " Simple sort to put the appointments list in order.
Scan the list for the smallest element left in the list.
Append the smallest element left into the new list, and remove
it from the original list."

  (let ((order-list nil))
    (while appt-list
      (let* ((element (car appt-list))
             (element-time (car (car element)))
             (tmp-list (cdr appt-list)))
        (while tmp-list
          (if (< element-time (car (car (car tmp-list))))
              nil
            (setq element (car tmp-list))
            (setq element-time (car (car element))))
          (setq tmp-list (cdr tmp-list)))
        (setq order-list (append order-list (list element)))
        (setq appt-list (delq element appt-list))))
    order-list))


(defun appt-convert-time (time2conv)
  " Convert hour:min[am/pm] format to minutes from
 midnight."

  (let ((conv-time 0)
        (hr 0)
        (min 0))

    (string-match ":[0-9][0-9]" time2conv)
    (setq min (string-to-int 
               (substring time2conv 
                          (+ (match-beginning 0) 1) (match-end 0))))
  
    (string-match "[0-9]?[0-9]:" time2conv)
    (setq hr (string-to-int 
              (substring time2conv 
                         (match-beginning 0)
                         (match-end 0))))
  
    ;; convert the time appointment time into 24 hour time
  
    (if (and (string-match  "[p][m]" time2conv) (< hr 12))
        (progn
          (string-match "[0-9]?[0-9]:" time2conv)
          (setq hr (+ 12 hr))))
  
    ;; convert the actual time
    ;; into minutes for comparison
    ;; against the actual time.
  
    (setq conv-time (+ (* hr 60) min))
    conv-time))


(defvar display-time-hook nil
  "* List of functions to be called when the time is updated on the 
mode line.")

(setq display-time-hook 'appt-check)

(defvar display-time-filter-initialized nil)

(defun fix-time()
(if display-time-filter-initialized         ;; only do this stuff once!
    nil
  (fset 'old-display-time-filter            ;; we're about to redefine it...
        (symbol-function 'display-time-filter))
  (setq display-time-filter-initialized t)
  (defun display-time-filter (proc string)  ;; ...here's the revised definition
    "Revised version of the original function: this version calls a hook."
      (old-display-time-filter proc string)
      (run-hooks 'display-time-hook))))
