; Date: Tue, 9 Oct 90 14:57:37 EDT
; From: Ken Laprade <laprade@trantor.harris-atd.com>
; Subject: Map handler to get rid of minibuffer
; 
; Here is a map handler that will unmap the minibuffer (and any other special
; screens such as a mouse-helper) when the last normal screen is unmapped and
; will remap the minibuffer (and others) when any window is mapped.  Maybe
; nobody else cares, but when I close my last epoch screen, I want everything
; else to go away; I have no need for a lone minibuffer.
; 
; -- 
; Ken Laprade			INTERNET: laprade@trantor.harris-atd.com
; Harris Corporation 		Usenet:  ...!uunet!x102a!trantor!laprade
; PO Box 37, MS 3A/1912		Voice: (407)727-4433
; Melbourne, FL 32902		FAX: (407)729-2537
; 
; --------------------
; 
;;; Add a map event handler that gets rid of auto-unmap-screens when the
;;; last normal screen is closed and brings them back when any screen is
;;; opened.  auto-unmap-screens is a list of symbols that eval to screens.
;;; The last size and position is remembered for each window because
;;; mapping seems to resize screens to their creation size.

(defvar auto-unmap-screens nil
  "*List of symbols referring to screens that will be automatically unmapped
when the last normal screen is closed and remapped when any screen is opened.")
(defvar auto-unmap-screens-geometry nil
  "Alist of saved geometries for screens referenced by auto-unmap-screens.")

(defun normal-screens (screens)
  "Return a list of all screens in SCREENS that are not referenced by
auto-unmap-screens."
  (let (l)
    (while screens
      (let ((scrn (car screens))
	    isone)
	(mapcar
	 (function (lambda (scr)
		     (setq isone (or isone (eq scrn (eval scr))))))
	 auto-unmap-screens)
	(or isone
	    (setq l (append l (list scrn)))))
      (setq screens (cdr screens)))
    l))

(defun my-map-event-handler (type value screen)
  "Map event handler.  Will unmap auto-unmap-screens if this is the last
normal screen being unmapped.  Will remap auto-unmap-screens if mapping a
screen.  Calls on-event::handler first."
  (on-event::handler type value screen)
  (if value
      ;; Mapping.
      (mapcar
       (function
	(lambda (scr)
	  (setq scr (eval scr))
	  (or (not (screen-p scr))
	      (eq scr screen)
	      (screen-mapped-p scr)
	      (progn
		(map-screen scr)
		;; Unmapped screens can forget their geometry, so we have to restore it.
		(let* ((geom (cdr (assq scr auto-unmap-screens-geometry))))
		  (if geom
		      (progn
			(change-screen-size (car geom) (nth 1 geom) scr)
			(move-screen (nth 2 geom) (nth 3 geom) scr))))))))
       auto-unmap-screens)
    ;; Unmapping.
    (or (normal-screens (screen-list))
	;; No more normal screens are mapped: unmap all others.
	(mapcar
	 (function
	  (lambda (scr)
	    (setq scr (eval scr))
	    (or (not (screen-p scr))
		(not (screen-mapped-p scr))
		;; Unmapped screens can forget their geometry, so we have to remember it.
		(let* ((entry (assq scr auto-unmap-screens-geometry))
		       (info (epoch::screen-information scr))
		       (border (nth 5 info))
		       (x (- (car info) border))
		       (y (- (nth 1 info) border))
		       (geom (list (epoch::screen-width scr)
				   (epoch::screen-height scr)
				   x y)))
		  (if entry
		      (setcdr entry geom)
		    (setq auto-unmap-screens-geometry
			  (append auto-unmap-screens-geometry
				  (list (cons scr geom)))))
		  (unmap-screen scr)))))
	 auto-unmap-screens))))
	
(setq auto-unmap-screens (append auto-unmap-screens (list '(minibuf-screen))))
(push-event 'map 'my-map-event-handler)
