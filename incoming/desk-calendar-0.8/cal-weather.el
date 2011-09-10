;; cal-weather.el --- Get weather forecasts for the Emacs Diary

;; Copyright (C) 1995, 1999 D. Dale Gulledge.
;;
;; Author: D. Dale Gulledge <dsplat@rochester.rr.com>
;; Version: 0.8 (1999/08/11)
;; Keywords: calendar
;; Human-Keywords: desk calendar, diary

;; This file is derived from functions in the Calendar/Diary facility
;; of GNU Emacs.  The copyright is currently held by the author,
;; D. Dale Gulledge, pending assignment to the Free Software
;; Foundation.  It may be used under the terms of the GNU General
;; Public License (also known as the GPL or GNU Copyleft).
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; To use this feature add a line like:
;;
;;   &%%(diary-weather "http://weather.unisys.com/foredat.cgi/KROC")
;;
;; to your diary file.  You can substitute a city name for KROC in many cases.
;; You can also go to the Unisys weather site and follow the links for the
;; local forecast.  They do not lead to the foredat.cgi script, but the
;; station names can be found that way in many cases.

(require 'w3)

(setq max-lisp-eval-depth 1024)

(defun diary-desk-calendar-weather-parser (url)
  "This function fetches the page pointed to by URL using w3-fetch.  It assumes
that page is in the format used by the US National Weather Service.  It will
extract from that page several pieces of information:
NOTE: DALE DESCRIBE WHAT IT PULLS OUT."
  (let ((buffer-to-return-to (current-buffer))
	(ast-dawn)
	(nau-dawn)
	(dawn)
	(sunrise)
	(sunset)
	(dusk)
	(nau-dusk)
	(ast-dusk)
	(sol-noon)
	(daylength)
	(twilight)
	(start)
	(now-text)
	(this-afternoon-text)
	(tonight-text)
	(tomorrow-text)
	(tomorrow-night-text)
	(two-days-text)
	(end))
    (setq end (w3-fetch url))
    (set-buffer "Untitled")
    (goto-char (point-min))
    (setq day-of-week (calendar-day-of-week (calendar-current-date))
	  ast-dawn (diary-desk-calendar-time-to-number
		    (diary-desk-calendar-get-weather-time "Ast Dawn:  "))
	  nau-dawn (diary-desk-calendar-time-to-number
		    (diary-desk-calendar-get-weather-time "Nau Dawn:  "))
	  dawn (diary-desk-calendar-time-to-number
		    (diary-desk-calendar-get-weather-time "Dawn:      "))
	  sunrise (diary-desk-calendar-time-to-number
		    (diary-desk-calendar-get-weather-time "Sunrise:   "))
	  sunset (diary-desk-calendar-time-to-number
		    (diary-desk-calendar-get-weather-time "Sunset:    "))
	  dusk (diary-desk-calendar-time-to-number
		    (diary-desk-calendar-get-weather-time "Dusk:      "))
	  nau-dusk (diary-desk-calendar-time-to-number
		    (diary-desk-calendar-get-weather-time "Nau Dusk:  "))
	  ast-dusk (diary-desk-calendar-time-to-number
		    (diary-desk-calendar-get-weather-time "Ast Dusk:  "))
	  sol-noon (diary-desk-calendar-time-to-number
		    (diary-desk-calendar-get-weather-time "Sol Noon:  "))
	  daylength (diary-desk-calendar-get-weather-time "Daylength: ")
	  twilight (diary-desk-calendar-get-weather-time "Twilight:  ")
	  now-text (diary-desk-calendar-get-weather-forecast ".NOW...")
	  this-afternoon-text
	  (diary-desk-calendar-get-weather-forecast ".THIS AFTERNOON...")
	  tonight-text (diary-desk-calendar-get-weather-forecast ".TONIGHT...")
	  tomorrow-text (diary-desk-calendar-get-weather-forecast
			 (concat "."
				 (diary-desk-calendar-day-of-week
				  (1+ day-of-week))
				 "..."))
	  tomorrow-night-text (diary-desk-calendar-get-weather-forecast
			       (concat "."
				       (diary-desk-calendar-day-of-week
					(1+ day-of-week))
				       " NIGHT..."))
	  two-days-text (diary-desk-calendar-get-weather-forecast
			 (concat "."
				 (diary-desk-calendar-day-of-week
				  (+ day-of-week 2))
				 "..."))
	  three-days-text (or (diary-desk-calendar-get-weather-forecast
			       (concat "."
				       (diary-desk-calendar-day-of-week
					(+ day-of-week 3))
				       "..."))
			      (diary-desk-calendar-get-weather-forecast
			       (concat "."
				       (diary-desk-calendar-day-of-week
					(+ day-of-week 3))
				       " AND "
				       (diary-desk-calendar-day-of-week
					(+ day-of-week 4))
				       "...")))
	  four-days-text (diary-desk-calendar-get-weather-forecast
			  (concat (diary-desk-calendar-day-of-week
				   (+ day-of-week 4))
				  "...")))
    (switch-to-buffer buffer-to-return-to)
    (list ast-dawn ast-dawn "Astronomical dawn"
	  nau-dawn nau-dawn "Nautical dawn"
	  dawn (1- sunrise) "Dawn"
	  sunrise sunrise "Sunrise"
	  sunset sunset "Sunset"
	  (1+ sunset) dusk "Dusk"
	  nau-dusk nau-dusk "Nautical dusk"
	  ast-dusk ast-dusk "Astronomical dusk"
	  sol-noon sol-noon "Solar noon"
	  "Day length" daylength
	  "Twilight length" twilight
	  "Current weather" now-text
	  "Forecast for this afternoon" this-afternoon-text
	  "Forecast for tonight" tonight-text
	  "Tomorrow\'s forecast" tomorrow-text
	  "Tomorrow Night\'s forecast" tomorrow-night-text
	  (diary-desk-calendar-day-of-week (+ day-of-week 2)) two-days-text
	  (diary-desk-calendar-day-of-week (+ day-of-week 3)) three-days-text
	  (diary-desk-calendar-day-of-week (+ day-of-week 4)) four-days-text)))

(defun diary-weather (url)
  "Generate a diary entry from the current weather as given at URL.  It will
appear on the calendar at the time specified by TIME."
  (save-excursion
    (let ((buffer-to-return-to "*Calendar*")
	  (days-ahead (- (calendar-absolute-from-gregorian date)
			 (calendar-absolute-from-gregorian
			  (calendar-current-date)))))
      (cond ((= days-ahead 0)
	     (let ((current-weather)
		   (tomorrow-weather)
		   (tomorrow-night-weather))
	       (w3-fetch url)
	       (setq ast-dawn (diary-desk-calendar-time-to-number
			       (diary-desk-calendar-get-weather-time "Ast Dawn:  "))
		     nau-dawn (diary-desk-calendar-time-to-number
			       (diary-desk-calendar-get-weather-time "Nau Dawn:  "))
		     dawn (diary-desk-calendar-time-to-number
			   (diary-desk-calendar-get-weather-time "Dawn:      "))
		     sunrise (diary-desk-calendar-time-to-number
			      (diary-desk-calendar-get-weather-time "Sunrise:   "))
		     sunset (diary-desk-calendar-time-to-number
			     (diary-desk-calendar-get-weather-time "Sunset:    "))
		     dusk (diary-desk-calendar-time-to-number
			   (diary-desk-calendar-get-weather-time "Dusk:      "))
		     nau-dusk (diary-desk-calendar-time-to-number
			       (diary-desk-calendar-get-weather-time "Nau Dusk:  "))
		     ast-dusk (diary-desk-calendar-time-to-number
			       (diary-desk-calendar-get-weather-time "Ast Dusk:  "))
		     sol-noon (diary-desk-calendar-time-to-number
			       (diary-desk-calendar-get-weather-time "Sol Noon:  "))
		     daylength (diary-desk-calendar-get-weather-time "Daylength: ")
		     twilight (diary-desk-calendar-get-weather-time "Twilight:  "))
	       (set-buffer "Untitled")
	       (goto-char (point-min))
	       (setq current-weather
		     (or
		      (diary-desk-calendar-get-weather-forecast ".NOW...")
		      (diary-desk-calendar-get-weather-forecast ".TODAY..."))
		     this-afternoon-weather
		     (diary-desk-calendar-get-weather-forecast
		      ".THIS AFTERNOON...")
		     tonight-weather
		     (diary-desk-calendar-get-weather-forecast ".TONIGHT..."))
	       (switch-to-buffer buffer-to-return-to)
	       (concat
		(if current-weather
		    (concat "\t08:00 Weather: "
			    current-weather
			    "\n"))
		(if this-afternoon-weather
		    (concat "\t12:00 Weather: "
			    this-afternoon-weather
			    "\n"))
		(if tonight-weather
		    (concat "\t18:00 Weather: "
			    tonight-weather
			    "\n"))
		(concat "\t"
			(diary-desk-calendar-number-to-time dawn)
			"-"
			(diary-desk-calendar-number-to-time (1- sunrise))
			" Dawn\n\t"
			(diary-desk-calendar-number-to-time (1+ sunset))
			"-"
			(diary-desk-calendar-number-to-time dusk)
			" Dusk\n\t"
			(diary-desk-calendar-number-to-time sol-noon)
			" Solar noon"))))
	    ((= days-ahead 1)
	     (let ((day-of-week (calendar-day-of-week (calendar-current-date)))
		   (current-weather)
		   (this-afternoon-weather)
		   (tonight-weather))
	       (setq end (w3-fetch url))
	       (set-buffer "Untitled")
	       (goto-char (point-min))
	       (setq tomorrow-weather
		     (diary-desk-calendar-get-weather-forecast
		      (concat "."
			      (diary-desk-calendar-day-of-week
			       (1+ day-of-week))
			      "..."))
		     tomorrow-night-weather
		     (diary-desk-calendar-get-weather-forecast
		      (concat "."
			      (diary-desk-calendar-day-of-week
			       (1+ day-of-week))
			      " NIGHT...")))
	       (switch-to-buffer buffer-to-return-to)
	       (concat
		(if tomorrow-weather
		    (concat "\t08:00 Weather: "
			    tomorrow-weather
			    (if tomorrow-night-weather "\n")))
		(if tomorrow-night-weather
		    (concat "\t18:00 Weather: "
			    tomorrow-night-weather)))))
	    ((= days-ahead 2)
	     (let ((day-of-week (calendar-day-of-week (calendar-current-date)))
		   (two-days-text))
1	       (setq end (w3-fetch url))
	       (set-buffer "Untitled")
	       (goto-char (point-min))
	       (setq two-days-text
		     (diary-desk-calendar-get-weather-forecast
		      (concat "."
			      (diary-desk-calendar-day-of-week
			       (+ day-of-week 2))
			      "...")))
	       (switch-to-buffer buffer-to-return-to)
	       (concat "\t08:00 Weather: " two-days-text)))
	    ((= days-ahead 3)
	     (let ((day-of-week (calendar-day-of-week (calendar-current-date)))
		   (three-days-text))
	       (setq end (w3-fetch url))
	       (set-buffer "Untitled")
	       (goto-char (point-min))
	       (setq three-days-text
		     (diary-desk-calendar-get-weather-forecast
		      (concat "."
			      (diary-desk-calendar-day-of-week
			       (+ day-of-week 3))
			      "...")))
	       (switch-to-buffer buffer-to-return-to)
	       (concat "\t08:00 Weather: " three-days-text)))
	    (t nil)))))

(defconst diary-desk-calendar-weather-days-of-week
  '["SUNDAY" "MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY" "FRIDAY" "SATURDAY"])

(defun diary-desk-calendar-day-of-week (day-of-week)
  (aref diary-desk-calendar-weather-days-of-week
	(% day-of-week 7)))

(defun diary-desk-calendar-strip-newlines (string)
  (if (= (length string) 0)
      ""
    (let ((i 0))
      (while (< i (length string))
	(if (or (string= (substring string i (1+ i)) "\n")
		(string= (substring string i (1+ i)) "\r"))
	    (setq string
		  (concat (substring string 0 i)
			  " "
			  (substring string (1+ i)))))
	(setq i (1+ i)))
      string)))

(defun diary-desk-calendar-get-weather-forecast (search-string)
  (goto-char (point-min))
  (if (search-forward search-string nil t)
      (progn
	(if (looking-at "$")
	    (beginning-of-line 2))
	(let ((begin (point)))
	  (re-search-forward "^[ 	]*$\\|^\\.[A-Z]" (point-max) t)
	  (end-of-line 0)
	  (diary-desk-calendar-strip-newlines
	   (buffer-substring-no-properties begin (point)))))
    nil))

(defun diary-desk-calendar-get-weather-time (search-string)
  (goto-char (point-min))
  (if (search-forward search-string nil t)
      (let ((start (point)))
	(end-of-line 1)
	(buffer-substring-no-properties start (point)))
    nil))

(defun diary-desk-calendar-number-to-time (time-number)
  "Convert a time as an integer into a string in the 24:MM format."
  (format "%02d:%02d"
	  (/ time-number 100)
	  (% time-number 100)))

(defun diary-desk-calendar-time-to-number (time-string)
  "Convert a time string into a time of day as an integer in the internal
format used by the desk calendar."
  (+
   (* 100 (string-to-int (substring time-string 0 2)))
   (string-to-int (substring time-string 3 5))))