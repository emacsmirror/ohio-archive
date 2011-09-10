;;; cdrw.el -- Dired frontend to various commandline
;;; CDROM burning tools.
;;
;; Emacs Lisp Archive Entry
;; Filename: cdrw.el
;; Author: Tony Sideris <tonys1110@home.com>
;; Version: 1.2
;; Created: 15 Jan 2001
;; Keywords: CDROM, CDRW, mp3, dired, burn, write
;; Description: dired front end to various commandline CD writing utilities
;; Compatibility: Emacs20 (only one tested so far)
;;
;; Copyright (C) 2000 Anthony Sideris.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; This file is NOT part of GNU Emacs.

(defconst cdrw-version "1.2"
  "Version of Emacs CD Creator.")

;;; Commentary:
;;
;;	This file is a frontend to various CDROM burning utilities
;;	such as cdrecord, mpg123, and mkisofs (in the future). It is
;;	basically just some extensions to `dired'.
;;
;;	I started writing this because I wanted a way to semi-easily
;;	burn audio CDs that would work on a tty, as well as in X. And
;;	because lisp is fun...
;;
;;	Currently only support for burning MP3 files to audio CDs
;;	is supported, I haven't gotten to data CD creation yet...
;;	this is mainly because data CD creation from the command-
;;	line is trivial and has always worked fine for me without
;;	a front-end, but I will probably implement it in the
;;	future for the hell of it...
;;
;;;	Features:
;;
;;	- Calculate the playing time (in minutes) of the selected
;;	(marked) MP3 files in the `dired' buffer.
;;	- Burn the selected MP3 without intermediate files.
;;	- Save/Restore file-lists (playlists if you will).
;;
;;;	Requirements:
;;
;;	- A machine that can already burn CDs using cdrecord.
;;	- cdrecord (common with most GNU/Linux distros, latest
;;	version also includes many other useful, related tools).
;;	- mpg123 (common with most GNU/Linux distros).
;;	- MP3::Info (perl module available from ftp.cpan.org).
;;	- mp3time (perl script that should be included with
;;	this script, see the end of this file).
;;
;;; Installation:
;;
;;	See the above section (REQUIREMENTS), and the end of this
;;	file if you do not already have the mp3time script. This
;;	script also requires the MP3::Info perl module, available
;;	from ftp://ftp.cpan.org
;;
;;	Place this file somewhere in your load-path (and optionally
;;	byte-compile it with M-x byte-compile-file RET).
;;
;;	Do "M-x customize-group RET cdrw RET" to customize the
;;	various variables.
;;
;;	Add the following code to your ~/.emacs file:
;;
;;	;;	This allows the commands that open CDRW buffers
;;	;;	to be autoloaded when needed.
;;	(autoload 'cdrw "cdrw" "CDRW Creation" t)
;;	(autoload 'cdrw-from-file "cdrw" "Load previously saved mark-file." t)
;;
;;	;;	This binds the commands that open CDRW buffers
;;	;;	to "C-c @ l" and "C-c @ f" respectively. (the
;;	;;	'@' character kinda looks like a CD :).
;;	(global-set-key "\C-c@" nil)
;;	(global-set-key "\C-c@l" 'cdrw)
;;	(global-set-key "\C-c@f" 'cdrw-from-file)
;;
;;	Feel free to change the keybinding to whatever
;;	you feel comfortable with.
;;
;;;	Todo:
;;
;;	- Write support for burning data CDs.
;;	- Write support for wav, au, cdr, and other audio
;;	file types (for audio CD creation).
;;	- Possibly move the mp3 info code to its
;;	own package to allow it to be used elsewhere.
;;	- Display information specfic to burning/mp3s
;;	in the modeline (total minutes, size, etc.)
;;
;;;	Disclaimers:
;;
;;	USE AT YOUR OWN RISK! I AM NOT RESPONSIBLE FOR ANY
;;	COASTERS/FRISBEES THAT YOU MAY INADVERTANTLY CREATE
;;	WHILE USING THIS CODE.
;;
;;; Code:
(require 'dired)

;;;; CONSTANTS
;;
(defconst cdrw-debug nil
  "No real (CDR related/time-consuming) commands
will executed if this is t.")

(defconst cdrw-win32-p
  (string= (symbol-name system-type) "windows-nt")
  "Whether or not we're on Win32.")

; (defconst cdrw-mp3-calc-time-command
;   (if cdrw-win32-p "perl D:/Stuff/Scripts/mp3time" "mp3time")
;   "Command to call to calculate playing times of MP3 files.")

(defconst cdrw-cdrecord-command "cdrecord")
(defconst cdrw-mp3-to-cdr-command "mpg123")
; (defconst cdrw-mkiso-command "mkisofs")

(defconst cdrw-burn-buffer "*cdrw-burn*")

;;;; CUSTOMIZATION
;;
(defgroup cdrw nil
  "Options for configuring CDRW dired interface."
  :group 'tools)

(defcustom cdrw-device "0,0,0"
  "Your CDR device, this can be determined with
the 'cdrecord -scanbus' command."
  :group 'cdrw
  :type 'string)

(defcustom cdrw-speed "2"
  "Speed to burn at."
  :group 'cdrw
  :type 'string)

(defcustom cdrw-save-prompt t
  "Should I prompt you to save your
selection before you begin a burn?"
  :group 'cdrw
  :type 'boolean)

(defcustom cdrw-extra-data-switches
  "-R"
  "Extra commandline switches to pass to cdrecord
when burning data CDs."
  :group 'cdrw
  :type 'string)

(defcustom cdrw-extra-audio-switches
  nil
  "Extra commandline switches to pass
to cdrecord when burning audio CDs."
  :group 'cdrw
  :type 'string)

(defcustom cdrw-fifo-dir "~/"
  "Directory to create (temporary) fifos in, these
are used during the burn process."
  :group 'cdrw
  :type 'directory)

;;;; MP3 FILE INFO CODE - This all may be moved into it's
;;;; own package at some point in the future.
;;
(defconst cdrw-mp3-bitrate-tbl
  [ [0 32 64 96 128 160 192 224 256 288 320 352 384 416 448 0]
	[0 32 48 56 64 80 96 112 128 160 192 224 256 320 384 0]
	[0 32 40 48 56 64 80 96 112 128 160 192 224 256 320 0]
	[0 32 48 56 64 80 96 112 128 144 160 176 192 224 256 0]
	[0 8 16 24 32 40 48 56 64 80 96 112 128 144 160 0]
	]
  "Bitrate table")

(defconst cdrw-mp3-samplerate-tbl
  [ [11025 12000 8000 0]
	[0 0 0 0]
	[22050 24000 16000 0]
	[44100 48000 32000 0]
	]
  "Samplerate table")

(defconst CDRW-FrameSync '(21 . 11))	;	StartBit/BitLength constants
(defconst CDRW-Version '(19 . 2))
(defconst CDRW-Layer '(17 . 2))
(defconst CDRW-NoCRC '(16 . 1))
(defconst CDRW-BitrateIndex '(12 . 4))
(defconst CDRW-SampleIndex '(10 . 2))
(defconst CDRW-Padding '(9 . 1))
(defconst CDRW-Private '(8 . 1))
(defconst CDRW-Channel '(6 . 2))
(defconst CDRW-ModeExt '(4 . 2))
(defconst CDRW-Copyright '(3 . 1))
(defconst CDRW-Original '(2 . 1))
(defconst CDRW-Emphasis '(0 . 2))

(defconst CDRW-V25 0)	;	Version and Layer constants
(defconst CDRW-V0 1)
(defconst CDRW-V2 2)
(defconst CDRW-V1 3)

(defconst CDRW-L0 0)
(defconst CDRW-L3 1)
(defconst CDRW-L2 2)
(defconst CDRW-L1 3)

(defun CDRW-VAL (header start-size)
  ;; (m_data >> bi.bit) & ((1 << bi.size) - 1)
  (logand (lsh header (- (car start-size)))
		  (- (lsh 1 (cdr start-size)) 1)))

(defun cdrw-mp3-get-header ()
  ;;	Create a 4 byte integer value starting at point.
  (let ((advance (lambda () (forward-char 1) (preceding-char))))
	(logior
	 (lsh (funcall advance) 24)
	 (lsh (funcall advance) 16)
	 (lsh (funcall advance) 8)
	 (logand (funcall advance)))))

(defun cdrw-mp3-get-bitrate (header)
  ;; return (getValue(biVersion) == V1)
  ;;   ? bitRateTbl[getValue(biVersion) ^ getValue(biLayer)][getValue(biBitrateIndex)]
  ;;   : bitRateTbl[(getValue(biLayer) == L1) ? 3 : 4][getValue(biBitrateIndex)];
  (aref
   (aref cdrw-mp3-bitrate-tbl (if (= (CDRW-VAL header CDRW-Version) CDRW-V1)
								  (logxor (CDRW-VAL header CDRW-Version)
										  (CDRW-VAL header CDRW-Layer))
								(if (= (CDRW-VAL header CDRW-Layer) CDRW-L1)
									3 4)))
   (CDRW-VAL header CDRW-BitrateIndex)))

(defun cdrw-mp3-get-samplerate (header)
  ;; return sampleRateTbl[getValue(biVersion)][getValue(biSampleIndex)];
  (aref
   (aref cdrw-mp3-samplerate-tbl (CDRW-VAL header CDRW-Version))
   (CDRW-VAL header CDRW-SampleIndex)))

(defun cdrw-mp3-yon (header start-size)
  ;;	Return "yes" or "no" for boolean
  ;;	header fields.
  (if (= (CDRW-VAL header start-size) 1)
	  "yes" "no"))

(defun cdrw-mp3-version (header)
  (aref ["2.5" "(unknown)" "2.0" "1.0"]
		(CDRW-VAL header CDRW-Version)))

(defun cdrw-mp3-layer (header)
  (aref ["(unknown)" "III" "II" "I"]
		(CDRW-VAL header CDRW-Layer)))

(defun cdrw-mp3-channel-mode (header)
  (aref ["Stereo" "Joint Stereo"
		 "Dual channel"
		 "Single channel"]
		(CDRW-VAL header CDRW-Channel)))

(defun cdrw-mp3-emphasis (header)
  (aref ["none" "50/15 ms" "reserved" "CCIT J.17"]
		(CDRW-VAL header CDRW-Emphasis)))

(defun cdrw-mp3-find-first-header ()
  "Find the first frame in the current
buffer. Returns -1 if not found."
  (let ((done nil)
		(last 0))
  	(while (not done)
	  (if (and (char-equal last 255)
			   (= (lsh (char-after) -5) 7))
			(setq done t))
	  (setq last (char-after))
	  (forward-char 1))
	(- (point) 2)))

(defun cdrw-mp3-calc-playing-time (info)
  "Return time in seconds for the given file info."
  (/ (/ (* (assoc-default 'size info) 8)
		1000)
	 (cdrw-mp3-get-bitrate (assoc-default 'hdr info))))

(defun cdrw-mp3-display-info (info)
  (let ((buf (get-buffer-create "*mp3-info*")))
	(with-current-buffer buf
	  (goto-char (point-max))
	  (if (> (point) (point-min))
		  (progn (insert "\n") (beginning-of-line)))
	  (insert
	   (format "%s (%d bytes in file, header at %d)\n"
			   (assoc-default 'file info)
			   (assoc-default 'size info)
			   (assoc-default 'start info))
	   (format "\tVersion %s/Layer %s\n"
			   (cdrw-mp3-version (assoc-default 'hdr info))
			   (cdrw-mp3-layer (assoc-default 'hdr info)))
	   (format "\t%dkbit/%shz %s\n"
			   (cdrw-mp3-get-bitrate (assoc-default 'hdr info))
			   (cdrw-mp3-get-samplerate (assoc-default 'hdr info))
			   (cdrw-mp3-channel-mode (assoc-default 'hdr info)))
	   (format "\tPrivate: %s\n\tMissing CRC: %s\n\tCopyright: %s\n\tOriginal: %s\n\tEmphasis: %s\n"
			   (cdrw-mp3-yon (assoc-default 'hdr info) CDRW-Private)
			   (cdrw-mp3-yon (assoc-default 'hdr info) CDRW-NoCRC)
			   (cdrw-mp3-yon (assoc-default 'hdr info) CDRW-Copyright)
			   (cdrw-mp3-yon (assoc-default 'hdr info) CDRW-Original)
			   (cdrw-mp3-emphasis (assoc-default 'hdr info)))
	   (let ((seconds (cdrw-mp3-calc-playing-time info)))
		 (format "\tPlaying Time: %2d:%02d (%d total seconds)\n"
				 (/ seconds 60) (% seconds 60) seconds))
	   )
	(display-buffer buf))))

(defun cdrw-mp3-get-file-info (file)
  "Return a list of information about the given FILE.
The list contains the following values (access with
`assoc-default').
  file - Filename
  hdr - First frame header
  size - File length in bytes
  start - Byte position of the first frame"
  (let ((file-length (nth 7 (file-attributes file)))
		(first-header 0)
		(header 0))
	(with-temp-buffer
	  ;;	Read the beginning of the file into the temp
	  ;;	buffer, DO NOT READ THE ENTIRE FILE into the
	  ;;	buffer, this is EXTREMELY SLOW! But we must
	  ;;	read in enough of the file so that we have
	  ;;	the first frame header. So far 3kb seems to
	  ;;	work (actually 2k seemed to work, but to be
	  ;;	safe...).
	  (insert-file-contents-literally file nil 0 3072)
	  (setq first-header (cdrw-mp3-find-first-header))
	  (if (not (= first-header -1))
		  (progn
			(goto-char first-header)
			(setq header (cdrw-mp3-get-header)))
		(error (concat "Header not found in " file))))
	(list (cons 'file file)
		  (cons 'hdr header)
		  (cons 'size file-length)
		  (cons 'start (- first-header 1)))))

;;;; CDRW FUNCTIONS
;;
;;;; PERL version
; (defun cdrw-calc-times (files)
;   "Calculate total time in minutes
; in all selected MP3 files."
;   (string-to-int
;    (shell-command-to-string
; 	(concat cdrw-mp3-calc-time-command " \""
; 			(mapconcat 'identity files "\" \"") "\""))))

;;;; ELISP version
(defun cdrw-calc-times (files)
  "Calculate total time in minutes
in all selected MP3 files."
  (let ((total 0)
		(sec 0))
	(mapcar '(lambda (file)
			   (setq sec (cdrw-mp3-calc-playing-time
						  (cdrw-mp3-get-file-info file)))
			   (setq total (+ total (if (> (% sec 60) 30)
										(+ (/ sec 60) 1)
									  (/ sec 60)))))
			files)
	total))

(defun cdrw-save-file (file)
  "Save the marked files to FILE, which can be
loaded later with `cdrw-from-file'."
  (interactive "FSave selection to file: ")
  (let ((files (dired-get-marked-files)))
	(with-temp-file file
	  (insert
	   ";;-*-emacs-lisp-*-\n"
	   ";; File generated by cdrw.el feel free to\n"
	   ";; edit as long as it's valid code.\n;;\n"
	   "(setq __cdrw-mark-file '((files . (\n")
	  (mapcar '(lambda (file) (insert (concat "\"" file "\"\n"))) files)
	  (insert
	   "))\n"
	   (concat "(version . \"" cdrw-version "\")))\n"))
	  ;;	Pretty it up.
	  (emacs-lisp-mode)
	  (mark-whole-buffer)
	  (call-interactively 'indent-region))))

(defun cdrw-load-file (file)
  "Load a file saved by `cdrw-save-file'. This
opens a `dired' buffer at the directory you were
in when you saved FILE, and remarks the files
you had marked."
  (interactive "fLoad selection file: ")
  (load-file file)
  (let* ((file-list (assoc-default 'files __cdrw-mark-file))
		 (dirname (file-name-directory (car file-list))))
	(with-current-buffer (cdrw dirname)
	  (mapcar '(lambda (file)
				 (if (not (string= dirname (file-name-directory file)))
					 (progn
					   (setq dirname (file-name-directory file))
					   (dired-maybe-insert-subdir dirname)))
				 (dired-mark-files-regexp (regexp-quote (file-name-nondirectory file))))
			  file-list)
	  (setq __cdrw-mark-file nil)
	  )))

(defun cdrw-command (cmd args show async)
  "Execute the command CMD with ARGS, if SHOW
is t, create/show *cdrw-burn* buffer, and display
the command's output there. If ASYNC is t then
execute the command and return immediately."
  (if cdrw-debug
	  ;;
	  ;;	Don't actually execute anything,
	  ;;	just log the command line.
	  (with-temp-buffer
		(insert (concat cmd " " (mapconcat 'identity args " ") "\n"))
		(append-to-file (point-min) (point-max) "~/cd-burn-debug.log"))
	;;
	;;	Execute the specified command with arguments.
	(let ((buf (if show cdrw-burn-buffer nil)))
	  (if async 	;;	Execute asyncronously.
		  (let ((process (apply #'start-process cmd buf cmd args)))
			(if show (display-buffer (get-buffer buf)))
			process)
		;;
		;;	Execute command syncronously.
		(apply #'call-process cmd nil buf show args)))))

(defun cdrw-common-args ()
  "Return a list of arguments to cdrecord."
  (list
   "-v"
;   "-dummy"
   (concat "speed=" cdrw-speed)
   (concat "dev=" cdrw-device)))

(defun cdrw-cdrecord-sentinel (process event)
;  (message (concat "From cdrecord: " event))
  ;;
  ;;	cdrecord is now complete. Remove
  ;;	the FIFOs that we created.
  (if (or (string-match "\\(finished\\|exited\\)\n" event)
		  (string-match ".+(core dumped)\n$" event))
	  (let ((cdr-files (file-expand-wildcards
						(concat cdrw-fifo-dir "*.fifo.cdr"))))
		(mapcar 'delete-file cdr-files)
		(cdrw-ordinary-insertion-filter
		 process
		 "\nCDRW: cdrecord is now complete.")
		)
	)
  )

(defun cdrw-ordinary-insertion-filter (proc string)
;;; Yes... this is mostly straight
;;; from the elisp manual...
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        (goto-char (process-mark proc))
		;;	BEGIN: My code.
		(cond ((string-match "^\\(\b+\\)\\(.+\\)" string)
			   (let ((bs (match-string 1 string)))
				 (goto-char (- (point) (length bs)))
				 (setq string (match-string 2 string))
				 (save-excursion
				   (delete-region (point) (+ (point) (length bs))))))

			  ((string-match "^Starting.+" string)
			   (insert "\n"))

			  ((string-match "^\r\\(.+\\)" string)
			   (setq string (match-string 1 string))
			   (beginning-of-line)
			   (save-excursion
				 (delete-region (point)
								(save-excursion (end-of-line) (point))))))
		;;	END: My code.
		(insert string)
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun cdrw-burn-audio (files)
  "Burn all FILES to CDR drive."
  (let ((params (append (cdrw-common-args) '("-audio")))
		(cdr-files ()))
	;;
	;;	Create a FIFO and have mpg123 convert
	;;	the mp3 file into it for each mp3.
	(mapcar '(lambda (file)
			   (let ((fifo (expand-file-name (concat cdrw-fifo-dir (file-name-nondirectory file) ".fifo.cdr"))))
				 (if (and (= (cdrw-command "mkfifo" (list fifo) nil nil) 0)
						  (cdrw-command cdrw-mp3-to-cdr-command (list "--cdr" fifo file) nil t))
					 ;;
					 ;;	Queue up the CDR for burning if successful.
					 (setq cdr-files (append cdr-files (list fifo)))
				   ;;
				   ;;	Oops... something failed.
				   (unless (yes-or-no-p "Error encountered during conversion, continue?")
					 (error (concat "Conversion error: " file))))
				 )
			   )
			files)
	(if cdr-files
		(progn
		  (if cdrw-extra-audio-switches
			  (setq params (append param cdrw-extra-audio-switches)))
		  ;;
		  ;;	Run cdrecord asyncronously, and request
		  ;;	notification of completion.
		  (let ((process (cdrw-command cdrw-cdrecord-command
									   (append params cdr-files) t t)))
			(progn
			  (set-process-filter process 'cdrw-ordinary-insertion-filter)
			  (set-process-sentinel process 'cdrw-cdrecord-sentinel)
			  process))
		  ))
  ))

;;;; INTERFACE FUNCTIONS
;;
;;;###autoload
(defun cdrw-mp3-display-times ()
  "Display the total playing time of
all selected mp3 files in the current
`dired' buffer."
  (interactive)
  (let ((minutes (cdrw-calc-times (dired-get-marked-files))))
	(message (format "%d minutes in %d files." minutes (length (dired-get-marked-files))))))

;;;###autoload
(defun cdrw (dir)
  "Open a `dired' buffer at DIR. Add CDRW-specific
key bindings and menu items."
  (interactive "DTop Level Directory: ")
  (let ((buf (dired dir)))
	;;
	;;	Add stuff to the menubar and keymap.
	;;
	(define-key dired-mode-map "\C-c@" nil)
	(define-key dired-mode-map "\C-c@i" 'cdrw-mp3-display-this-file-info)
	(define-key dired-mode-map "\C-c@m" 'cdrw-mp3-display-times)
	(define-key dired-mode-map "\C-c@a" 'cdrw-create-audio-cd)
	(define-key dired-mode-map "\C-c@s" 'cdrw-save-file)
	(define-key dired-mode-map "\C-c@d" 'cdrw-create-data-cd)

	(if (require 'easymenu nil t)
		;;	Add some menu items to
		;;	the menubar.
		(progn
		  (easy-menu-define
		   cdrw-menu dired-mode-map "CDRW menu"
		   '("CDRW"
			 ["Save mark file" cdrw-save-file (dired-get-marked-files)]
			 "-"
			 ["Create Data CD (not yet implemented)" cdrw-create-data-cd nil]
			 ["Create Audio CD" cdrw-create-audio-cd (dired-get-marked-files)]
			 "-"
			 ["Minutes in marked MP3s" cdrw-mp3-display-times (dired-get-marked-files)]
			 ["Display file info" cdrw-mp3-display-this-file-info t]
			 "-"
			 ["Customize" (customize-group 'cdrw) t]
			 ))))

	buf)
  )

;;;###autoload
(defun cdrw-from-file (file)
  "Like `cdrw', but marks the files listed in 'file'. This
file should have been created with `cdrw-save-files'."
  (interactive "fLoad selection file: ")
  (cdrw-load-file file))

;;;###autoload
(defun cdrw-create-audio-cd ()
  "Burns the marked MP3 files to the CDR."
  (interactive)
  (if (yes-or-no-p (format "%d minutes in marked files, continue?"
						   (cdrw-calc-times (dired-get-marked-files))))
	  (progn
		(if (and cdrw-save-prompt (yes-or-no-p "Save selection?"))
			(call-interactively 'cdrw-save-file))
		(cdrw-burn-audio (dired-get-marked-files)))))

;;;###autoload
(defun cdrw-create-data-cd ()
  (interactive)
  (error "Data CD creation not yet implemented!")
  )

;;;###autoload
(defun cdrw-mp3-display-file-info (file)
  "Display information about the MP3 FILE."
  (interactive "fMP3 File: ")
  (cdrw-mp3-display-info (cdrw-mp3-get-file-info file)))

;;;###autoload
(defun cdrw-mp3-display-this-file-info ()
  "Display information about the
MP3 file at `point'."
  (interactive)
  (mapcar 'cdrw-mp3-display-file-info
		  (dired-get-marked-files)))

(provide 'cd-burn)
;;; cdrw.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;	This is the perl script 'mp3time' that this script
;;	requires. Place the following code in its own file
;;	remove the (elisp) comments, save it to a directory
;;	in your PATH, and make it executable.
;;
;;	THIS IS NO LONGER NEEDED, I AM LEAVING IT HERE
;;	JUST IN CASE...
;;
;;-------- >8 -------------------- >8 ------------
; #!/usr/bin/perl
; ################################################
; ##	Calculate the playing time of mp3 files
; ##	and/or directories full of them.
; ##
; ##	Usage:
; ##		mp3time <directory|file> ...
; ##
; ##	by Tony Sideris
; ################################################
; use strict;
; use MP3::Info;

; my $grandtotal = 0;

; while (@ARGV)
; {
; 	if (-f $ARGV[0])
; 	{
; 		my $info = get_mp3info($ARGV[0]) || die("Failed to get info on $ARGV[0]\n");
; 		my $min = $info->{MM};
; 		$min++ if ($info->{SS} > 30 || $min == 0);
; 		$grandtotal += $min;
; 	}
; 	elsif (-d $ARGV[0])
; 	{
; 		opendir(DIR, $ARGV[0]) || die("Failed to read contents of $ARGV[0]\n");
; 		my @files = grep { /mp\d$/i && -f "$ARGV[0]$_" } readdir(DIR);
; 		closedir(DIR);
; 		my ($sec) = (0);

; 		foreach (@files)
; 		{
; 			my $info = get_mp3info("$ARGV[0]$_") || print STDERR $_, ": Failed\n" && next;
; 			print format_file($_, $info);

; 			$sec += (($info->{MM} * 60) + $info->{SS});
; 		}

; 		printf("\nThere is %d seconds (%d hours) of music in %d files.\n",
; 			   $sec, (($sec / 60) / 60), length(@files));
; 		$grandtotal += ($sec / 60);
; 	}
; 	else { die("You must specify either a file or directory!\n"); }
; 	shift;
; }

; print int($grandtotal);

; sub format_file
; {
; 	my ($filename, $mp3) = (shift, shift);
; 	return sprintf ("%3d:%02d - %s\n", $mp3->{MM}, $mp3->{SS}, $filename);
; }
;;-------- >8 -------------------- >8 ------------
