;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                       dunnet.el  Version 2.0                         ;
;                                                                      ;
;                   Ron Schnell (ronnie@media.mit.edu)                 ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This is the startup file.  It loads in the other files, and sets up
;; the functions to be bound to keys if you play in window-mode.

(if nil
    (eval-and-compile (setq byte-compile-warnings nil)))

;;;;;  The log file should be set for your system, and it must
;;;;;  be writeable by all.

      (setq log-file "/usr/local/dunscore")

(defun dungeon-mode ()
  "Major mode for running dungeon"
  (interactive)
  (text-mode)
  (use-local-map dungeon-mode-map)
  (setq major-mode 'dungeon-mode)
  (setq mode-name "Dungeon")
)

(setq load-path (append load-path (list ".")))


(defun dungeon-parse (arg)
  "foo"
  (interactive "*p")
  (beginning-of-line)
  (setq beg (+ (point) 1))
  (end-of-line)
  (if (and (not (= beg (point))) (not (< (point) beg))
	   (string= ">" (buffer-substring (- beg 1) beg)))
      (progn
	(setq line (downcase (buffer-substring beg (point))))
	(princ line)
	(if (eq (parse ignore verblist line) -1)
	    (mprinc "I don't understand that.\n")))
    (goto-char (point-max))
    (mprinc "\n"))
    (dungeon-messages))
    
(defun dungeon-messages ()
  (if dead
      (text-mode)
    (if (eq dungeon-mode 'dungeon)
	(progn
	  (if (not (= room current-room))
	      (progn
		(describe-room current-room)
		(setq room current-room)))
	  (fix-screen)
	  (mprinc ">")))))

(defun dungeon-start ()
  (interactive)
  (switch-to-buffer "*dungeon*")
  (dungeon-mode)
  (setq dead nil)
  (setq room 0)
  (dungeon-messages))
(setq load-path (append load-path '("/usr/local/emacs/lisp")))

(require 'cl)

(defun batch-dungeon ()
  (setq load-path (append load-path (list ".")))
  (load "dun-batch")
  (setq visited '(27))
  (mprinc "\n")
  (dungeon-batch-loop))


(load "dun-commands")
(load "dun-util")
(if (setq glob (get-glob-dat))
    (load-d glob)
  (load "dun-globals"))

(load "dun-unix")
(load "dun-dos")
(load "dun-save")
(random t)
(setq tloc (+ 60 (% (abs (random)) 18)))
(replace room-objects tloc (append (nth tloc room-objects) (list 18)))
(setq tcomb (+ 100 (% (abs (random)) 899)))
(setq combination (prin1-to-string tcomb))
