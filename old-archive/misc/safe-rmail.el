;From utkcs2!emory!swrinde!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!compass.com!worley Wed Jun  6 14:24:58 EDT 1990
;Article 2911 of gnu.emacs:
;Path: utkcs2!emory!swrinde!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!compass.com!worley
;>From: worley@compass.com (Dale Worley)
;Newsgroups: gnu.emacs
;Subject: Protecting Rmail files from unintentional expunges
;Message-ID: <9006061422.AA00787@sn1987a.compass.com>
;Date: 6 Jun 90 14:22:52 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 48
;
;The following code protects your Rmail files from unintentional
;expunges, by requiring a prefix argument on rmail-expunge (E and X),
;rmail-save (S), and rmail-quit (Q).  If you do not give an argument,
;these commands just beep.  The hook function interacts safely with
;hook functions that redefine the rmail-mode-map, because it uses
;substitute-key-definition.

(require 'rmail)

(setq rmail-mode-hook
      (cons '(lambda ()
	       (substitute-key-definition 'rmail-expunge 'rmail-expunge-safely
					  rmail-mode-map)
	       (substitute-key-definition 'rmail-expunge-and-save
					  'rmail-expunge-and-save-safely
					  rmail-mode-map)
	       (substitute-key-definition 'rmail-quit 'rmail-quit-safely
					  rmail-mode-map))
	    rmail-mode-hook))

(defun rmail-expunge-safely (x)
  "Actually erase all deleted messages in the file.
Requires an argument, for safety's sake."
  (interactive "P")
  (if x
      (rmail-expunge)
    (beep)))

(defun rmail-expunge-and-save-safely (x)
  "Expunge and save RMAIL file.
Requires an argument, for safety's sake."
  (interactive "P")
  (if x
      (rmail-expunge-and-save)
    (beep)))

(defun rmail-quit-safely (x)
  "Quit out of RMAIL.
Requires an argument, for safety's sake."
  (interactive "P")
  (if x
      (rmail-quit)
    (beep)))

;Dale Worley		Compass, Inc.			worley@compass.com
;--
;Don't waste the money to see _Waiting for Godot_.  There isn't
;any point to it -- believe it or not, Godot never arrives.


