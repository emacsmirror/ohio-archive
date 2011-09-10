;;; weather.el --- quickly grab a temperature from the net

;; Copyright (C) 2001 by Janardan Revuru

;; Emacs Lisp Archive Entry
;; Filename: weather.el
;; Author: Janardan Revuru <janardan@email.com>
;; Created:  31 Mar 2001
;; Version: 0.1
;; Keywords: data, temperature, weather

;; Emacs versions: 
;;  GNU Emacs 20.2.1 (Solaris)
;;  GNU Emacs 20.7.1 (Windows NT)
;;  X Emacs 21.1 patch 9 (Windows NT)
;;
;; W3 Version:
;;  WWW 4.0pre.46, URL p4.0pre.46, MM 1.96

;; This program is based on the code written by John Wiegley's stock-quote.el
;; The ideas are derived from this Emacs extension.

;; Copyright (C) 1999, 2000 John Wiegley
;; Author: John Wiegley <johnw@gnu.org>
;; Created:  3 Dec 1999
;; Version: 2.2
;; Keywords: data
;; X-URL: http://www.gci-net.com/users/j/johnw/emacs.html


;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A simple mode for snarfing weather updates.
;;
;; To obtain temperature, type `M-x weather', and enter the city-code
;; symbol.
;;
;; city-code:
;; Right now code gets temperature from site http://weather.netscape.com
;; and it uses three character city code for all United States cities,
;; just like airlines use. For example: SJC for San Jose, OAK for Oakland,
;; LAX for Los Angles, etc.
;; To get the city-code you stay in, please browse the netscape site for
;; details. I plan to fix this limitation in future or anyone ready to
;; do it is welcome.

;; To watch temperature in the modeline, configure the variable
;; `weather-in-modeline' with the name of the city-code symbol to
;; watch.
;;
;; To change the way that temperatures are obtained, write a new
;; function and add-hook it to weather-data-functions'.  It will be
;; passed the city-code name, and should return the temperature if it
;; can.  The default method uses the Netscape web page, and the W3
;; package.
;;
;; If you want Emacs to do certain things when certain conditions are
;; met, you should configure the triggers and actions variables.  Only
;; a few predefined triggers are given, but the mechanism is
;; extensible.  Just add your trigger function to
;; `weather-trigger-functions'.  It will be given the city-code name
;; and temperature (as a integer value).  If the function returns a
;; string, representing the reason why the trigger is firing, then the
;; trigger is considered as having fired, and an action will be
;; performed.
;;
;; Actions are functions that have been add-hook'd to
;; `weather-trigger-functions'.  If the first action to return a
;; non-nil value is chosen.  They can do anything, and will be passed
;; the city-code name, the temperature, and the reason returned by the
;; trigger function.
;;

;;; Future Enhancements:

;; Enhancement 1:
;;  (defvar weather-unit "F"
;;      "Temperature reading defaults to Fahrheit. Make it C for Celcius.")
;;      if (string= weather-unit "F" ) 
;;          append to netscape url "units=english"
;;      else 
;;          append to netscape url "units=metric"
;;      Note: By default units are "F"
;;      This URL appending will be different for different sites.
;;      Please investigate this option for site you get temperature update.

;; Enhancement 2:
;;  Currently code expects that you have w3 installed. To remove this need,
;; can use "lynx -dump -nolinks" options using shell-command-to-string and
;; parse the string. Assumption is that if you are running on Unix mostly
;; you have lynx installed already.

;; Enhancement 3:
;;  Accept city-code or ZIP code.
;;  http://www.weather.com has ZIP code encoded in URL and seems to be covering
;; more cities.

;;; Other Comments:

;; Named this package 'weather' though it gets only temperature. I
;; would like to extend this package more than just temperature
;; update. But still modeline will only have temperature (and not
;; forecast for the next month!)

;; This program has been a query-replace version of stock-quote.el
;; You may find some variables or documentation talking about quotes.

;;; History:

;;; Code:

(defconst weather-version "0.1"
  "This version of weather.")

(defgroup weather nil
  "quickly grab a temperature reading from the Net."
  :group 'applications)

;;; User Variables:

(defcustom weather-data-functions '(weather-netscape)
  "*A list of functions used to obtain temperature values.
Each function is passed the city-code name as a string.
The first function to return a temperature (as an integer value) is
used."
  :type 'hook
  :group 'weather)

(defcustom weather-trigger-functions nil
  "*A list of functions to call to determine if a trigger fires.
Each function is passed the city-code name as a string, and the 
temperature
as an integer value.
The first function to return a string, representing the reason for the
trigger, is called."
  :type 'hook
  :group 'weather)

(defcustom weather-action-functions '(weather-message-box)
  "*A list of functions to call when a trigger fires.
Each function is passed the city-code name, the temperature as 
an integer
value, and the return value from the trigger.
If any function returns a non-nil value, none else will be called."
  :type 'hook
  :group 'weather)

(defcustom weather-interval 300
  "*Number of seconds to wait between temperature updates."
  :type 'integer
  :group 'weather)

;;; Internal Variables:

(defvar weather-last-value nil
  "The last temperature reading.")

(defvar weather-mode-string ""
  "A string representing the last temperature reading.")


(defvar weather-timer nil)

;;; User Functions:

;;;###autoload
(defun weather (city-code)
  "Get *present* temperature from an Internet site and display it.
*present* temperature means the temperature last update at the site it
is grabbing from.  You can display the *present* temperature by using
weather-in-modeline function.  Update interval can be modified by
setting the weather-interval variable."
  (interactive "sCity-Code: ")
  (setq weather-last-value
	(run-hook-with-args-until-success
	 'weather-data-functions (upcase city-code)))
  (if (numberp weather-last-value)
      (message (format "%s: %dF" city-code weather-last-value))
    (message (format "Error: weather-last-value (%s) should be 
a number" 
		     weather-last-value))))

;;; Internal Functions:


(defun weather-netscape (city-code)
  "Download a temperature using the Netscape web page."
  (require 'w3)
  (require 'url)
  (with-temp-buffer
    (let (url-show-status)
      (url-insert-file-contents
       (concat "http://weather.netscape.com/weather/home.tmpl?city="
	       city-code))
      (set-buffer-modified-p nil))
    (goto-char (point-min))
    (if (and (re-search-forward "Current Temperature: " nil t)
	     (looking-at "-?[0-9]+"))
	  (string-to-int (match-string 0)))))


(custom-add-option 'weather-data-functions
		   'weather-netscape)

(defun weather-update ()
  "Update the last known temperature, and check triggers."
  (when (setq weather-last-value
	      (run-hook-with-args-until-success
	       'weather-data-functions weather-in-modeline))
    (setq weather-mode-string
	  (format " [%dF]" weather-last-value))
    (force-mode-line-update)
    (let ((reason (run-hook-with-args-until-success
		   'weather-trigger-functions
		   weather-in-modeline weather-last-value)))
      (if reason
	  (run-hook-with-args-until-success
	   'weather-action-functions weather-in-modeline
	   weather-last-value reason)))))

(defcustom weather-floor-trigger 10
  "*Value of the floor trigger (see `weather-trigger-functions') ."
  :type 'number
  :group 'weather)

(defun weather-floor-trigger (city-code temperature)
  "Fire a trigger if the temperature goes below 
`weather-floor-trigger'."
  (if (< temperature weather-floor-trigger)
      (format "%s has dropped to %.2f" city-code temperature)))

(custom-add-option 'weather-trigger-functions
		   'weather-floor-trigger)

(defcustom weather-ceiling-trigger 100
  "*Value of the ceiling trigger (see `weather-trigger-functions') ."
  :type 'number
  :group 'weather)

(defun weather-ceiling-trigger (city-code temperature)
  "Fire a trigger if the temperature rises above 
`weather-ceiling-trigger'."
  (if (> temperature weather-ceiling-trigger)
      (format "%s has risen to %.2f" city-code temperature)))

(custom-add-option 'weather-trigger-functions
		   'weather-ceiling-trigger)

(defcustom weather-move-trigger 10
  "*Value of the move trigger (see `weather-trigger-functions') ."
  :type 'number
  :group 'weather)

(defun weather-move-trigger (city-code temperature)
  "Fire a trigger on changes more than `weather-move-trigger' percent."
  (let ((diff (and weather-last-value
		   (* (/ weather-last-value temperature) 100))))
    (if (> (abs diff) weather-ceiling-trigger)
	(format "%s has moved by %.2f%%" city-code diff))))

(custom-add-option 'weather-trigger-functions
		   'weather-move-trigger)

(defun weather-message-box (city-code temperature reason)
  "If a triggers fires, pop up a message box."
  (ignore (message-box "Temperature trigger: %s" reason)))

(custom-add-option 'weather-action-functions
		   'weather-message-box)

(defun weather-in-modeline (city-code)
  "Set the temperature of CITY-CODE to display in the modeline.
If CITY-CODE is nil, disable modeline display."
  (setq weather-in-modeline city-code)
  (if city-code
      (run-with-idle-timer
       1 nil
       (function
	(lambda ()
	  (unless (memq 'weather-mode-string global-mode-string)
	    (if global-mode-string
		(nconc global-mode-string '(weather-mode-string))
	      (setq global-mode-string '(weather-mode-string))))
	  (setq weather-timer
		(run-at-time nil weather-interval
			     'weather-update))
	  (force-mode-line-update))))
    (setq global-mode-string
	  (delq 'weather-mode-string global-mode-string))
    (when weather-timer
      (cancel-timer weather-timer)
      (setq weather-timer nil)))
  city-code)

(defcustom weather-in-modeline nil
  "*If a string, display that temperature in the modeline."
  :set (lambda (symbol value)
	 (weather-in-modeline value))
  :type '(choice string (const :tag "None" nil))
  :require 'weather
  :group 'weather)

(provide 'weather)

;;; weather.el ends here
