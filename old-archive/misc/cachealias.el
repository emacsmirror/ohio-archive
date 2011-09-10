;From: montnaro@sprite.crd.ge.com (Skip Montanaro)
;Newsgroups: gnu.emacs
;Subject: Loading up mail-aliases
;Message-ID: <MONTNARO.89Aug24100552@sprite.crd.ge.com>
;Date: 24 Aug 89 14:05:52 GMT
;Reply-To: <montanaro@sprite.crd.ge.com> (Skip Montanaro)
;Followup-To: gnu.emacs
;Organization: GE Corporate Research & Development, Schenectady, NY
;Lines: 46
;
;
;I use a version of mailalias.el that understands the "source" command in
;~/.mailrc. (It's been posted to various Emacs groups before I believe, and I
;could have sworn at one point it was in the Emacs distribution, but it isn't
;at the moment. If you want it, drop me a note.) Unfortunately, we have a
;couple of huge mail alias files at our site that cause the first call to
;sendmail to take forever to start (we're talking over 1800 mail aliases that
;take two-three minutes to parse on a Sun-3/260).
;
;What I needed was a way to cache the mail-alises alist, and only update it
;when the source files changed. I wrote a short Makefile and some ELisp to
;update a cached mail alias file from cron late at night.
;
;
;The Makefile is:
;
;
;$(HOME)/.mail-aliases : $(HOME)/.mailrc /usr/local/lib/unix-aliases \
;			/usr/local/lib/crd-aliases
;	emacs -batch -l cache-mail-aliases.el
;
;
;The file, cache-mail-aliases.el, contains


    (message "Building mail aliases... ")
    (load "~/emacs/mailalias" nil t)     ; understands "source" command
    (build-mail-aliases "~/.mailrc")
    (set-buffer (find-file-noselect (expand-file-name "~/.mail-aliases")))
    (erase-buffer)
    (insert "(setq mail-aliases '\n")
    (insert (format "%s\n" mail-aliases))
    (insert ")\n")
    (save-buffer)
    (kill-buffer (current-buffer))
    (message "Building mail aliases... Done")


;The following additional line in ~/.emacs sets the mail-aliases variable:


;    (load "~/.mail-aliases" nil t t)


;--
;Skip Montanaro (montanaro@sprite.crd.ge.com)
