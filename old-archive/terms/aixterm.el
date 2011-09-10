; Article: 6604 of comp.emacs
; Path: utkcs2!emory!sol.ctr.columbia.edu!samsung!umich!sharkey!bnlux1.bnl.gov!baron
; From: baron@bnlux1.bnl.gov (ed baron)
; Newsgroups: comp.emacs,gnu.emacs.help
; Subject: aixterm.el
; Date: 23 Apr 91 18:01:21 GMT
; Organization: Brookhaven National Laboratory
; 
; I am posting this version of aixterm.el which was kindly supplied to
; me by Milt Epstein.  I don't know where it came from but it does seem
; to properly define the arrow keys as well as PgUp and PgDn and Ins.
; 
; In a windows environment it must be loaded explicitly:
; 
;      (if (string-equal (getenv "TERM") "aixterm")
; 	 (load-file "/usr/local/emacs-18.57/lisp/aixterm.el"))
; 
; and it is a bit slow to load. 
; 
; enjoy. -- ed baron (baron@bnlux1.bnl.gov)
; 
; 
; 



; @(#)hft.el	1.3  com/gnuemacs/lisp/term,3.1,9005 2/6/90 17:16:22
;;;-------------------------------------------------------------------------
;;;
;;;    AIX key bindings for HFT and X
;;;
;;;----------------------------------------------------------------

(define-key esc-map "[" (make-sparse-keymap))
(global-unset-key "[")

;;;----------------------------------------------------------------
;;;
;;; HFT outboard control keys
;;;
;;;----------------------------------------------------------------
(global-set-key "[A"    'previous-line)       	;  up
(global-set-key "[161q" 'unassigned)                  ;s-up 
(global-set-key "[162q" 'unassigned)			;c-up 
(global-set-key "[163q" 'unassigned)          	;a-up 

(global-set-key "[B"    'next-line)			;  down 
(global-set-key "[164q" 'unassigned)                  ;s-down 
(global-set-key "[165q" 'unassigned)			;c-down 
(global-set-key "[166q" 'unassigned)          	;a-down 

(global-set-key "[C"    'forward-char)		;  right 
(global-set-key "[167q" 'unassigned)  		;s-right 
(global-set-key "[168q" 'unassigned)		        ;c-right 
(global-set-key "[169q" 'unassigned)  		;a-right 

(global-set-key "[D"    'backward-char)		;  left 
(global-set-key "[158q" 'unassigned)  		;s-left 
(global-set-key "[159q" 'unassigned)  		;c-left 
(global-set-key "[160q" 'unassigned)  		;a-left 
;;;----------------------------------------------------------------
(global-set-key "[150q" 'scroll-down)			;  page up
(global-set-key "[151q" 'unassigned)          	;s-page up
(global-set-key "[152q" 'unassigned)			;c-page up
(global-set-key "[153q" 'unassigned)          	;a-page up

(global-set-key "[154q" 'scroll-up)			;  page down
(global-set-key "[155q" 'unassigned)			;s-page down
(global-set-key "[156q" 'unassigned)			;c-page down
(global-set-key "[157q" 'unassigned)          	;a-page down
;;;----------------------------------------------------------------
(global-set-key "[H"    'beginning-of-line)		;  home
(global-set-key "[143q" 'unassigned)  		;s-home
(global-set-key "[144q" 'unassigned)  		;c-home
(global-set-key "[145q" 'unassigned)			;a-home
;;;----------------------------------------------------------------
(global-set-key "[146q" 'end-of-line)			;  end
(global-set-key "[147q" 'unassigned)  		;s-end
(global-set-key "[148q" 'unassigned)  		;c-end
(global-set-key "[149q" 'unassigned)			;a-end
;;;----------------------------------------------------------------
(global-set-key "[139q" 'overwrite-mode)		 ; insert
(global-set-key "[140q" 'unassigned)  		;c-insert
(global-set-key "[141q" 'unassigned)			;a-insert
;;;----------------------------------------------------------------
(global-set-key "[P"    'delete-char)			;  delete
(global-set-key "[142q" 'unassigned)  		;c-delete
(global-set-key "[M"    'unassigned)  		;a-delete
;;;----------------------------------------------------------------
(global-set-key "[209q" 'unassigned)  		;  print screen
(global-set-key "[210q" 'unassigned)  		;s-print screen
(global-set-key "[211q" 'unassigned)			;c-print screen
(global-set-key "[212q" 'unassigned)  		;a-print screen
;;;----------------------------------------------------------------
(global-set-key "[213q" 'unassigned)  		;  scroll lock
(global-set-key "[214q" 'unassigned)		        ;s-scroll lock
(global-set-key "[215q" 'unassigned)  		;c-scroll lock
(global-set-key "[216q" 'unassigned)			;a-scroll lock
;;;----------------------------------------------------------------
(global-set-key "[217q" 'unassigned)	        	;  pause
(global-set-key "[218q" 'unassigned)			;s-pause
;;;----------------------------------------------------------------
(global-set-key "[114q" 'unassigned)        		;  action

;;;----------------------------------------------------------------
;;;
;;; HFT PF keys 
;;;
;;;----------------------------------------------------------------
(global-set-key "[001q" 'unassigned)			;  f1
(global-set-key "[002q" 'unassigned)          	;  f2
(global-set-key "[003q" 'unassigned)  		;  f3
(global-set-key "[004q" 'unassigned)           	;  f4
(global-set-key "[005q" 'unassigned)			;  f5
(global-set-key "[006q" 'unassigned)          	;  f6
(global-set-key "[007q" 'unassigned)			;  f7
(global-set-key "[008q" 'unassigned)		        ;  f8
(global-set-key "[009q" 'unassigned)          	;  f9
(global-set-key "[010q" 'unassigned)  		;  f10
(global-set-key "[011q" 'unassigned)          	;  f11
(global-set-key "[012q" 'unassigned)  		;  f12
;;;----------------------------------------------------------------
(global-set-key "[013q" 'unassigned)  		;s-f1
(global-set-key "[014q" 'unassigned)  		;s-f2
(global-set-key "[015q" 'unassigned)          	;s-f3
(global-set-key "[016q" 'unassigned)  		;s-f4
(global-set-key "[017q" 'unassigned)			;s-f5
(global-set-key "[018q" 'unassigned)                  ;s-f6
(global-set-key "[019q" 'unassigned)  		;s-f7
(global-set-key "[020q" 'unassigned)                  ;s-f8
(global-set-key "[021q" 'unassigned)			;s-f9
(global-set-key "[022q" 'unassigned)			;s-f10
(global-set-key "[023q" 'unassigned)			;s-f11
(global-set-key "[024q" 'unassigned)			;s-f12
;;;----------------------------------------------------------------
(global-set-key "[025q" 'unassigned)  		;c-f1
(global-set-key "[026q" 'unassigned)			;c-f2
(global-set-key "[027q" 'unassigned)			;c-f3
(global-set-key "[028q" 'unassigned)			;c-f4
(global-set-key "[029q" 'unassigned)			;c-f5
(global-set-key "[030q" 'unassinged)			;c-f6
(global-set-key "[031q" 'unassigned)			;c-f7
(global-set-key "[032q" 'unassigned)			;c-f8
(global-set-key "[033q" 'unassigned)			;c-f9
(global-set-key "[034q" 'unassigned)			;c-f10
(global-set-key "[035q" 'unassigned)			;c-f11
(global-set-key "[036q" 'unassinged)          	;c-f12
;;;----------------------------------------------------------------
(global-set-key "[037q" 'unassigned)			;a-f1
(global-set-key "[038q" 'unassigned)			;a-f2
(global-set-key "[039q" 'unassigned)			;a-f3
(global-set-key "[040q" 'unassigned)			;a-f4
(global-set-key "[041q" 'unassinged)			;a-f5
(global-set-key "[042q" 'unassigned)			;a-f6
(global-set-key "[043q" 'unassigned)			;a-f7
(global-set-key "[044q" 'unassigned)			;a-f8
(global-set-key "[045q" 'unassigned)			;a-f9
(global-set-key "[046q" 'unassigned)			;a-f10
(global-set-key "[047q" 'unassigned)			;a-f11
(global-set-key "[048q" 'unassigned)			;a-f12

;;;--------------------------------------------------------------------
;;;
;;; Emulate META key with ALT key for AIX
;;;
;;;--------------------------------------------------------------------
(define-key global-map "\033[087q" 'backward-sentence)    	 	;a-a
(define-key global-map "\033[105q" 'backward-word)     			;a-b 
(define-key global-map "\033[103q" 'capitalize-word)   			;a-c
(define-key global-map "\033[089q" 'kill-word)         			;a-d
(define-key global-map "\033[076q" 'forward-sentence)  			;a-e
(define-key global-map "\033[090q" 'forward-word)      			;a-f
(define-key global-map "\033[091q" 'fill-region)       			;a-g
(define-key global-map "\033[092q" 'mark-paragraph)    			;a-h
(define-key global-map "\033[081q" 'tab-to-tab-stop)   			;a-i
(define-key global-map "\033[093q" 'indent-new-comment-line)		;a-j 
(define-key global-map "\033[094q" 'kill-sentence)     			;a-k
(define-key global-map "\033[095q" 'downcase-word)    			;a-l 
(define-key global-map "\033[107q" 'back-to-indentation)		;a-m 
(define-key global-map "\033[106q" 'unassigned)   			;a-n 
(define-key global-map "\033[082q" 'unassigned)				;a-o 
(define-key global-map "\033[083q" 'unassigned)     			;a-p 
(define-key global-map "\033[074q" 'fill-paragraph)    			;a-q 
(define-key global-map "\033[077q" 'move-to-window-line)		;a-r 
(define-key global-map "\033[088q" 'unassigned)     			;a-s 
(define-key global-map "\033[078q" 'transpose-words)   			;a-t 
(define-key global-map "\033[080q" 'upcase-word)     			;a-u 
(define-key global-map "\033[104q" 'scroll-down)     			;a-v 
(define-key global-map "\033[075q" 'copy-region-as-kill)		;a-w 
(define-key global-map "\033[102q" 'execute-extended-command)		;a-x 
(define-key global-map "\033[079q" 'yank-pop)     			;a-y 
(define-key global-map "\033[101q" 'zap-to-char)  			;a-z 

;;;------------------------------------------------------------------------------
;;;
;;; The HFT support does not allow the ALT, SHIFT, and CONTROL keys to 
;;; be combined.  The following attempts to map the ALT-key to the best 
;;; mapping without SHIFT and CONTROL conbinations
;;;
;;;------------------------------------------------------------------------------
(define-key global-map "\033[115q" 'not-modified)       	        ;a-~ 
(define-key global-map "\033[058q" 'shell-command)      	        ;a-! 
(define-key global-map "\033[059q" 'mark-word)          	        ;a-@
(define-key global-map "\033[060q" 'unassigned)        			;a-#
(define-key global-map "\033[061q" 'spell-word)         	        ;a-$
(define-key global-map "\033[062q" 'query-replace)      	        ;a-%
(define-key global-map "\033[063q" 'delete-indentation) 	        ;a-^
(define-key global-map "\033[064q" 'unassigned)         	        ;a-&
(define-key global-map "\033[065q" 'unassigned)        	                ;a-*
(define-key global-map "\033[066q" 'insert-parentheses) 	        ;a-(
(define-key global-map "\033[067q" 'move-past-close-and-reindent)	;a-)
(define-key global-map "\033[068q" 'negative-argument)		        ;a--
(define-key global-map "\033[070q" 'count-lines-region)		        ;a-=
(define-key global-map "\033[084q" 'backward-paragraph)		        ;a-[
(define-key global-map "\033[085q" 'forward-paragraph)		        ;a-]
(define-key global-map "\033[086q" 'shell-command-on-region)	        ;a-|

(define-key global-map "\033[097q" 'indent-for-comment)		        ;a-;
(define-key global-map "\033[099q" 'abbrev-prefix-mark)		        ;a-'

(define-key global-map "\033[109q" 'beginning-of-buffer)	        ;a-<
(define-key global-map "\033[111q" 'end-of-buffer)		        ;a->
(define-key global-map "\033[113q" 'dabbrev-expand)		        ;a-/

(define-key global-map "\033[071q" 'backward-kill-word)		        ;a-bsp

;;;---------------------------------------------------------------------------
;;; use private key remapping is .emacskeys file in $HOME directory
;;;---------------------------------------------------------------------------

(if (file-readable-p "~/.emacskeys") (load-file "~/.emacskeys"))

;;;---------------------------------------------------------------------------
;;; end of key definitions 
;;;---------------------------------------------------------------------------


