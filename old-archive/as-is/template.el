;Date: 25 Mar 88 03:03:54 GMT
;From: Tom Lord <lord+@ANDREW.CMU.EDU>
;Organization: Carnegie Mellon University
;Subject: template.el

;I have found the following code useful.  To use it, load it into your
;emacs and bind TEMPLATE-COMMAND to some key (I use \M-+).
;TEMPLATE-COMMAND prompts you for the name of a template file stored in
;the directory named in the variable TEMPLATE-DIR.  That file is
;inserted at the point.

;Then, supposing you've loaded the template TEMPLATE-DIR/TEMPL, the
;function DO-SUBS is invoked.  DO-SUBS looks for
;TEMPLATE-DIR/TEMPL.subs and if that file is found, reads the contents
;of that file and interprets the value (which should be a list) as a
;list of instructions for filling out the template.  The odd numbered
;(if you start counting at 1) elements of the list are `keys'.  On the
;other hand, the odd numbered (if you start counting at 0) elements are
;replacement instructions.  All occurences of each key are replaced by
;some string determined by the replacement instruction.  The type of
;the replacement instruction determines the replacement.  If it is a
;string, then that string is used a prompt and you are asked for the
;replacement string.  If the type is anything else, it is EVAL'ed and
;the result (which had better be a string) replaces the key.

;This code is very small, and doesn't bother doing any error checking.
;Hey! what do you want for nothing?

;Ok, here's an example template that I use whenever I write a new
;program:

;/*@ cc @I @F @L -o @R
; **
; **LIBS: -ldtree
; */

;char useline[]={"@PROGRAM@ [-x]"};

;char * flagsexpl[] = {
;	"@PROGRAM@ - ?? 1 line description for -x output",
;	"",
;	"-x		Display these explanations",
;	0,
;};

;#if	!defined(lint) && !defined(NOSCCS)
;	static char	sccsid[] ={"%W% - %E%"};
;#endif

;/* the (((comments))) below were added just for this post.
; * I usually don't stick n+1 lines of comment in all my 
; * programs
; */
;/* created by:
; ** Thomas Lord, busily hacking on @DATE@
; ** the Information Technology Center
; ** Carnegie<no-dash>Mellon - the Un-university
; **	(((note...you won't get that joke until you've read some 
; **	   of the PR dept.'s rules for referring to CM.)))
; ** 	breeding ground for crooked squares
; **	(((note...you won't get THAT joke until you see CM's logo)))
; **	(((Incidently, it was the PR dept. the pushed for the new logo)))
; ** 4910 Forbes Avenue
; ** Pittsburgh PA, 15213
; ** USA
; ** Phone: +1 412 268 5790
; **	(((268 spells CMU on your phone)))
; **	(((I think that was the PR dept.'s idea)))
; **	(((5790 a symmetric pattern.)))
; **	(((That's how I remember it anyway...never had to worry about 
; **	   remembering the area code)))
; ** Site: andrew.cmu.edu
; ** Email: lord+@andrew.cmu.edu
; **	(((Don't ask about the plus.  The explanation sounds really silly.
; **	   If your mailer can't hack the +, send mail to 
; **	   toom.lard@andrew.cmu.edu (no +) and i'll probably get it.)))
; ** Favourite Pizza: Larry and Carol's 687-1189
; ** 	(((This probably violates some rule about advertising on the net)))
; **	(((oh well, too bad)))
; **	(((Besides, since I became a vegetarian I don't 
; **	   eat there much anymore)))
; ** Favourite Beer: Orval
; ** (((Oh yeah, if you happen to work for CM's PR dept., 
; **   nothing personal, ok?  It's just a joke.)))
; */


;#include	<stdio.h>

;static char		?flag;
;extern char	*  leafnm();
;char		*  errlabel;
;static int	   arginterp();

;main(argc,argv)
;	char **argv;
;{
;	arginterp(argc,argv);
;	/* This is the tricky bit */
;	
;	exit(0);
;}

;static int
;arginterp(argc,argv)
;	char **argv;
;{
;	register char *p;
;	register c;

;	errlabel = leafnm(*argv);
;	for (argv++; --argc && **argv == '-'; argv++) {
;		p = &argv[0][1];
;		while (c = *p++) switch (c) {
;		default:
;			usage("-%c: invalid flag", c);
;			/* no return */
;			
;		case 'x':
;			explflags();
;			exit(1);
;			
;		case '?':
;			?flag++;
;			continue;
;			
;		case '?':
;			if (!--argc)
;				errexit("-?: missing argument");
;			?flgarg = *++argv;
;			continue;
;		}
;	}
;	return argc;
;}


;Note that the keys @PROGRAM@ and @DATE@ need to be fixed up.  Also,
;incindently, note that there are question marks (which I just I-search
;for) where I need to fix up the template, but DO-SUBS isn't powerful
;enough to do the job.

;Associated with that template (called "main") is a subs file (called 
;"main.subs").  It reads:

;(
;	"@PROGRAM@"	"Program? "
;	"@DATE@"	(current-time-string)
;)


;Note that you don't need to supply a .subs file.  If none is found, 
;template-command is just an insert file from a fixed directory (which
;is useful, I think).


;Well, this post is, so far, much longer than the actual code which is just:

(defvar template-dir "~/include/templates/"
  "*A directory containing oft-used templates.")

(defun template-command ()
  "Nicely prompt for a template file and insert it."
  (interactive)
  (let* ((name (read-file-name "template: " template-dir "default" t))
	 (sub (concat name ".subs")))
    (insert-file name)
    (if (file-readable-p sub)
	(do-subs sub))))

(defun do-subs (fname)
  "Execute a list of substitution commands."
  (interactive "fSubstitution File: ")
  (let ((buffer (get-buffer-create "*Subs buffer*"))
	subs)
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert-file fname)
      (goto-char 0)
      (setq subs (read buffer)))
    (save-excursion
      (goto-char 0)
      (while subs
	(let ((key (car subs))
	      (val (car (cdr subs))))
	  (if (stringp val)
	      (setq val (read-string val))
	    (setq val (eval val)))
	  (replace-string key val)
	  (goto-char 0)
	  (setq subs (cdr (cdr subs))))))))
