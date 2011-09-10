;From utkcs2!emory!samsung!uunet!mcsun!hp4nl!mhres!jv Wed Jun 13 10:56:29 EDT 1990
;Article 4437 of comp.emacs:
;Xref: utkcs2 comp.emacs:4437 gnu.emacs:2964
;Path: utkcs2!emory!samsung!uunet!mcsun!hp4nl!mhres!jv
;>From: jv@mh.nl (Johan Vromans)
;Newsgroups: comp.emacs,gnu.emacs
;Subject: Re: Release two of CMU process modes  (background.el)
;Message-ID: <JV.90Jun13143702@squirrel.mh.nl>
;Date: 13 Jun 90 20:37:02 GMT
;References: <9601@pt.cs.cmu.edu>
;Sender: news@mhres.mh.nl
;Reply-To: Johan Vromans <jv@mh.nl>
;Organization: Multihouse Automation, the Netherlands
;Lines: 27
;In-reply-to: shivers@a.gp.cs.cmu.edu's message of 12 Jun 90 06:20:17 GMT
;
;In article <9601@pt.cs.cmu.edu> shivers@a.gp.cs.cmu.edu (Olin Shivers) writes:
;> This message contains:
;> 2. A shar file for background.el and cmutex.el
;
;Since I have been working with a background-type function for some
;time, I want to share the following function. Its main function is to
;background a shell command if it terminates with '&'; bind it to your
;favourite 'execute-shell-command' key (e.g. "\e!") and enjoy.
;
;; ================ (a)synch shell command execution ================
;;
(defvar my-last-scommand nil)

(defun my-scommand (command &optional flag)
  "Runs shell command asynchrounous, or synchrounous if terminated
with \"&\""
  (interactive (list (read-string "Shell command: " my-last-scommand)
		     current-prefix-arg))
  (setq my-last-scommand command)
  (if (string-match "&$" command)
      (background (substring command 0 -1))
    (shell-command command flag)))
;--
;Johan Vromans				       jv@mh.nl via internet backbones
;Multihouse Automatisering bv		       uucp: ..!{uunet,hp4nl}!mh.nl!jv
;Doesburgweg 7, 2803 PL Gouda, The Netherlands  phone/fax: +31 1820 62944/62500
;------------------------ "Arms are made for hugging" -------------------------
;

