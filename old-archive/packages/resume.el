;From ark1!uakari.primate.wisc.edu!uwm.edu!lll-winken!bu.edu!bu-cs!nntp-read!jbw Wed Feb  7 16:16:14 1990
;Article 1081 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!uwm.edu!lll-winken!bu.edu!bu-cs!nntp-read!jbw
;From jbw@bucsf.bu.edu (Joe Wells)
;Newsgroups: gnu.emacs
;Subject: Re: resuming suspended Emacs
;Message-ID: <JBW.90Feb5151002@bucsf.bu.edu>
;Date: 5 Feb 90 20:10:02 GMT
;References: <9002051421.AA06553@life.ai.mit.edu>
;Sender: news@bu.edu.bu.edu
;Distribution: gnu
;Organization: Boston University Computer Science Department
;Lines: 186
;In-reply-to: DRB@MATH.AMS.COM's message of 5 Feb 90 14:21:23 GMT
;
;In article <9002051421.AA06553@life.ai.mit.edu> DRB@MATH.AMS.COM (Drew Burton) writes:
;
;	   This is a novice question.  We've just brought up Emacs
;   (18.51) on a DECstation 3100 running Ultrix (BSD 4.3).  I need to 
;   know how people resume a suspended (exit with ctrl-Z) Emacs.  I assume
;   that there must be a simple script (Bourne shell if possible) that
;   allows a single command to fire up emacs if necessary or to bring
;   a suspended job to the foreground if it exists.  Or perhaps people
;   use some other tactic entirely.
;
;Well, I imagine most people just use "fg", but you may want fancier
;functionality.  I'm including a package I wrote to handle this.  The idea
;is that the first time you start emacs, command line arguments are handled
;normally.  Then, you suspend your emacs job.  When you want to edit
;something else, you type "emacs filename" as usual, but instead of
;starting a new emacs job, the old job is resumed instead, and the command
;line arguments are placed in a file where the old emacs job looks for
;them.
;
;Enjoy,
;
;-- 
;Joe Wells <jbw@bu.edu>
;jbw%bucsf.bu.edu@bu-it.bu.edu
;...!harvard!bu-cs!bucsf!jbw
;(617) 375-6893
;200-202 Bay State Rd., Box 1486, Boston, MA 02215, USA
;----------------------------------------------------------------------
;; Process command line arguments from within a suspended Emacs job
;; Copyright (C) 1988 Free Software Foundation, Inc.

;; This file is not yet part of GNU Emacs, but soon will be.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Author: Joe Wells
;; Last changed: Tue Jun 13 12:37:19 1989 by jbw ( Joe Wells #360 x2392) on ketchum
;; jbw@bucsf.bu.edu (school year)
;; joew%uswest@boulder.colorado.edu (summer)

;; Stephan Gildea suggested bug fix (gildea@bbn.com).
;; Ideas from Michael DeCorte and other people.

;; For csh users, insert the following alias in your .cshrc file
;; (after removing the leading double semicolons, of course):
;;
;;# The following line could be just EMACS_CMD=emacs, but this depends on
;;# your site.
;;if (! $?EMACS_CMD) set EMACS_CMD=emacs
;;set JOBS_FILE=/tmp/jobs.$USER.$$
;;set ARGS_FILE=~/.emacs_args
;;set STOP_PATT='^\[[0-9]*\] *[ +-] Stopped ............ '
;;set SUNVIEW_CMD='emacstool -nw -f emacstool-init -f server-start'
;;set X_CMD=\'\''$EMACS_CMD -i -f server-start'
;;alias emacs \
;;' \\
;;   jobs >! "$JOBS_FILE" \\
;;   && grep "$STOP_PATT$EMACS_CMD" "$JOBS_FILE" >& /dev/null \\
;;   && echo `pwd` \!* >! "$ARGS_FILE" && ""fg %$EMACS_CMD \\
;;|| if (! -e ~/.emacs_server || -f ~/.emacs_server) set status=1 \\
;;   && emacsclient \!* \\
;;|| @ status=1 - $?DISPLAY && eval "$X_CMD -i \!* &" \\
;;|| @ status=1 - $?WINDOW_PARENT && eval "$SUNVIEW_CMD \!* &" \\
;;|| ""$EMACS_CMD -nw \!* \\
;;'
;;
;; The alias works as follows:
;; 1. If there is a suspended Emacs job that is a child of the
;; current shell, place its arguments in the ~/.emacs_args file and
;; resume it.
;; 2. Else if the ~/.emacs_server socket has been created, presume an
;; Emacs server is running and attempt to connect to it.  If no Emacs
;; server is listening on the socket, this will fail.
;; 3. Else if the DISPLAY environment variable is set, presume we are
;; running under X Windows and start a new GNU Emacs process in the
;; background as an X client.
;; 4. Else if the WINDOW_PARENT environment variable is set, presume we
;; are running under SunView and start an emacstool process in the
;; background.
;; 5. Else start a regular Emacs process.
;;
;; Notes:
;; The output of the "jobs" command is not piped directly into "grep"
;; because that would run the "jobs" command in a subshell.
;; Before resuming a suspended emacs, the current directory and all
;; command line arguments are placed in a file name ~/.emacs_args.
;; The "-nw" switch to Emacs means no windowing system.

;; Insert this in your .emacs file:
;;(setq suspend-resume-hook 'resume-process-args)
;;(setq suspend-hook 'empty-args-file)
;;(autoload 'empty-args-file "resume")
;;(autoload 'resume-process-args "resume")

;; Finally, put the rest in a file named "resume.el" in a lisp library
;; directory.

(defvar emacs-args-file (expand-file-name "~/.emacs_args")
  "*This file is where arguments are placed for a suspended emacs job.")

(defvar emacs-args-buffer " *Command Line Args*"
  "Buffer that is used by resume-process-args.")

(defun resume-process-args ()
  "This should be called from inside of suspend-resume-hook.  This
grabs the contents of the file whose name is stored in
emacs-args-file, and processes these arguments like command line
options."
  (let ((start-buffer (current-buffer))
	(args-buffer (get-buffer-create emacs-args-buffer))
	length args)
    (unwind-protect
	(progn
	  (set-buffer args-buffer)
	  (erase-buffer)
	  ;; get the contents of emacs-args-file
	  (condition-case ()
	      (let ((result (insert-file-contents emacs-args-file)))
		(setq length (car (cdr result))))
	    ;; the file doesn't exist, ergo no arguments
	    (file-error
	      (erase-buffer)
	      (setq length 0)))
	  (if (<= length 0)
	      (setq args nil)
	    ;; get the arguments from the buffer
	    (goto-char (point-min))
	    (while (not (eobp))
	      (skip-chars-forward " \t\n")
	      (let ((begin (point)))
		(skip-chars-forward "^ \t\n")
		(setq args (cons (buffer-substring begin (point)) args)))
	      (skip-chars-forward " \t\n"))
	    ;; arguments are now in reverse order
	    (setq args (nreverse args))
	    ;; make sure they're not read again
	    (erase-buffer))		
	  (write-buffer-to-file (current-buffer) emacs-args-file)
	  ;; if nothing was in buffer, args will be null
	  (or (null args)
	      (setq default-directory (file-name-as-directory (car args))
		    args (cdr args)))
	  ;; actually process the arguments
	  (command-line-1 args))
      ;; If the command line args don't result in a find-file, the
      ;; buffer will be left in args-buffer.  So we change back to the
      ;; original buffer.  The reason I don't just use
      ;; (let ((default-directory foo))
      ;;    (command-line-1 args))
      ;; in the context of the original buffer is because let does not
      ;; work properly with buffer-local variables.
      (if (eq (current-buffer) args-buffer)
	  (set-buffer start-buffer)))))

(defun empty-args-file ()
  "This empties the contents of the file whose name is specified by
emacs-args-file."
  (save-excursion
    (set-buffer (get-buffer-create emacs-args-buffer))
    (erase-buffer)
    (write-buffer-to-file (current-buffer) emacs-args-file)))

(defun write-buffer-to-file (buffer file)
  "Writes the contents of BUFFER into FILE, if permissions allow."
  (if (not (file-writable-p file))
      (error "No permission to write file %s" file))
  (save-excursion
    (set-buffer buffer)
    (clear-visited-file-modtime)
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) file nil 'quiet))
    (set-buffer-modified-p nil)))


