;;;
;;;  This is nt-url.el rev. 3a.
;;;
;;;  Glue between browse-url.el and WWW browser on Windows95 & NT.
;;;
;;;  Written by Kim F. Storm <storm@olicom.dk>
;;;  Based on code from Peter Breton <pbreton@i-kinetics.com>
;;;  Win95 support by Rich Pieri <rich.pieri@prescienttech.com>
;;;  
;;;  Usage:
;;;	(load-library "nt-url")
;;;     (setq nt-url-use-shelex t)  [optional; see below]
;;;
;;;  Using the standard settings, clicking S-mouse-2 on an URL will
;;;  write the URL string to a file (see nt-url-work-file) and invoke
;;;  "rundll32.exe url.dll,OpenURL file" to transfer the URL to your
;;;  browser.
;;;
;;;  If the default way of invoking the browser does not work, you 
;;;  need to obtain the program "shelex.exe" which performs
;;;  ShellExecute directly on a given URL, and set the variable: 
;;;	(setq nt-url-use-shelex t)
;;;
;;;  If you have another program which performs ShellExecute, or
;;;  the shelex.exe is not on your PATH, set the variable to a
;;;  string containing the name (and path) of that program, e.g.
;;;	(setq nt-url-use-shelex "c:\emacs\bin\shelex.exe")
;;;

(defvar nt-url-use-shelex nil
  "Use \"shelex.exe\" to invoke browser for a given URL.
If set to a string, use that program instead of shelex.
If nil, use rundll32.exe and url.dll (default).")
 
(defvar nt-url-work-file
  (concat (or (getenv "TMP") (getenv "TEMP") "C:") "/win_url.URL")
  "Name of work file for passing URL to url.dll.")

(defun nt-url-browse-url (url)
  "Browse a URL using Windows WWW browser."
  (interactive
   (progn
     (require 'browse-url)
     (append (browse-url-interactive-arg "Browse URL: "))))
  (if nt-url-use-shelex
      (call-process 
       (if (stringp nt-url-use-shelex) nt-url-use-shelex "shelex.exe")
       nil 0 nil url)
    (write-region (concat "[InternetShortcut]\nURL=" url "\n") nil
		  nt-url-work-file nil 1)
    (call-process "rundll32.exe" nil 0 nil
		  "url.dll,OpenURL" nt-url-work-file)))

(autoload 'browse-url-browser-function "browse-url"
  "Ask a WWW browser to show a URL." t)
(setq browse-url-browser-function 'nt-url-browse-url)
(setq vm-url-browser 'nt-url-browse-url)		; VM
(setq gnus-button-url 'nt-url-browse-url)		; GNUS

(global-set-key [S-mouse-2] 'browse-url-at-mouse)
