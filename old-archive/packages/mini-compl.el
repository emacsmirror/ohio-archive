;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!usc!bbn!jr@bbn.com Thu Apr 19 12:40:58 EDT 1990
;Article 1787 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!usc!bbn!jr@bbn.com
;From: jr@bbn.com (John Robinson)
;Newsgroups: comp.emacs
;Subject: Re: switch-to-buffer mode request
;Message-ID: <54985@bbn.COM>
;Date: 17 Apr 90 21:43:10 GMT
;References: <1697@sas.UUCP>
;Sender: news@bbn.COM
;Reply-To: jr@bbn.com (John Robinson)
;Organization: BBN Systems and Technologies Corporation, Cambridge MA
;Lines: 83
;In-reply-to: sasjaa@sas.UUCP (Jim Adams)
;
;In article <1697@sas.UUCP>, sasjaa@sas (Jim Adams) writes:
;>I am looking for a way to enhance the switch-to-buffer command.
;>I would like to do it (or get it) in lisp code as I don't have the
;>ability to modify our emacs. (I am using GNU emacs 18.54 on the
;>apollo). Here's what I want. When I press \C-x b, the message window
;>displays the default buffer to switch to. I would like to be able
;>to press SPC to cycle to the next buffer on the list. Is this
;>possible?
;
;Well, this sort of request has come up often enough that I thought I'd
;dive in and attempt it.  With only 15 minutes of hackery, I came up
;with the following, modified from the minibuffer-complete-exit-backup
;function (included) that came across the net many moons ago.  I use
;SPC for the completer, and M-n, M-p to navigate through the
;completions list.  Warning: this is gruesome slow in the middle of,
;for example, ^H f or ^H v :-).  Please offer improvements (esp in the
;list searching loop).  Byte compile for speed of course.  For blinding
;speed, code the C functions Fnext-completion and Fprev-completion like
;the built-in (all-completions), and toss the lisp loops altogether.
;========8<----------------------------------------------------------------
(defun minibuffer-complete-exit-backup nil
   "Minibuffer completion, exiting on unique completion with backup."
   (interactive)
   (let (comp (complete t))
     (while (null (setq comp (all-completions
                              (buffer-substring (point-min) (point))
                              minibuffer-completion-table
                              minibuffer-completion-predicate)))
       (setq complete nil)
       (delete-backward-char 1 nil))
     (and complete (if (null (cdr comp)) (minibuffer-complete-and-exit)
                        (minibuffer-complete)))))

(defun minibuffer-complete-next (dir)
   "Minibuffer complete, then go to next choice, or prev if negative ARG.
Look in completion table for unique completion with backup."
   (interactive "p")
   (let (comp (complete t) head)
     (while (null (setq comp (all-completions
                              (buffer-substring (point-min) (point))
                              minibuffer-completion-table
                              minibuffer-completion-predicate)))
       (setq complete nil)
       (delete-backward-char 1 nil))
     (and complete 
	  (if (cdr comp)
	      (minibuffer-complete)
	    (setq complete (car comp))
	    (setq comp (all-completions
			""
			minibuffer-completion-table
			minibuffer-completion-predicate))
	    (if (< dir 0)
		(setq comp (nreverse comp)))
	    (setq head (car comp))
	    (while (and comp (not (equal complete (car comp))))
	      (setq comp (cdr comp)))
	    (erase-buffer)
	    (if (null (cdr comp))
		(insert head)
	      (insert (car (cdr comp))))))))

(defun minibuffer-complete-prev ()
   "Minibuffer complete, then go to next prev choice.
Look in completion table for unique completion with backup."
   (interactive)
   (minibuffer-complete-next -1))

(define-key minibuffer-local-must-match-map " "
     (define-key minibuffer-local-completion-map " "
          'minibuffer-complete-exit-backup))
(define-key minibuffer-local-must-match-map "\en"
     (define-key minibuffer-local-completion-map "\en"
          'minibuffer-complete-next))
(define-key minibuffer-local-must-match-map "\ep"
     (define-key minibuffer-local-completion-map "\ep"
          'minibuffer-complete-prev))
;========8<----------------------------------------------------------------
;
;Enjoy,
;--
;/jr, nee John Robinson     Life did not take over the globe by combat,
;jr@bbn.com or bbn!jr          but by networking -- Lynn Margulis
;

