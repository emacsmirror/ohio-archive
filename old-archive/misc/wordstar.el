;From jimf%saber@harvard.harvard.edu Wed Jan 31 21:16:36 1990
;Date: Wed, 31 Jan 90 19:36:26 EST
;From: jimf%saber@harvard.harvard.edu (Jim Frost)
;To: dsill@relay
;Subject: Emacs Lisp archives
;
;Someone suggested to me that I submit my wordstar.el emacs lisp file
;to the elisp archives.  It looks like you're the target point for the
;archive.
;
;If this isn't the case, I'm sorry to bother you -- please just bounce
;this and tell me you're not the right person.  Otherwise, read on.
;
;What follows is a set of Emacs bindings that give a fair approximation
;of the WordStar 3.x key bindings, with a couple of extensions.  It's
;good enough not to drive WordStar people crazy but I confess that I
;didn't work too hard on it (it shows -- nowadays I'd probably make it
;a mode or something).  If you want to include this in the archives, go
;ahead -- I just give it away to anyone who requests it anyway.
;
;Happy hacking,
;
;jim frost
;saber software
;jimf@saber.com
;
;-- cut here --
; Wordstar Keycap Definition for EMACS
;
; This file causes EMACS to emulate WordStar.  You should do one of
; two things to make this work.  Either copy this file into ~/.emacs,
; after which it will be loaded every time you call up emacs, or
; copy it into ~/.wordstar and make an alias ws 'emacs -l ~/.wordstar'
; which will let you call it up with the "normal" ws command.
;
; caveats: * ^G (delete-character) is hard bound to the emacs function
;            quit, so if you delete very fast you might cause emacs to
;            abort.  tell it to save your file and then to continue and
;            all will be well, although annoying.  this doesn't happen
;            often unless the machine is kind of slow.
;          * block copy and delete work as they do in emacs, not in
;            wordstar, which means you can block end above the block
;            begin and you can also block-delete from block begin to
;            where the cursor is without explicitly doing a block-end.
;          * ^Owv is split-window-vertically
;          * ^Owh is split-window-horizontally
;          * ^Owo is other-window
;          * ^Owd is delete-other-windows
;
; for further information contact jim frost at madd@bu-it.bu.edu
;

(setq inhibit-terminal-defaults t)
(setq C-K-map (make-keymap))
(define-key C-K-map " " ())
(define-key C-K-map "b" 'set-mark-command)
(define-key C-K-map "\002" 'set-mark-command)
(define-key C-K-map "c" 'yank)
(define-key C-K-map "\003" 'yank)
(define-key C-K-map "d" 'save-buffers-kill-emacs)
(define-key C-K-map "\004" 'save-buffers-kill-emacs)
(define-key C-K-map "f" 'find-file)
(define-key C-K-map "\006" 'find-file)
(define-key C-K-map "k" 'copy-region-as-kill)
(define-key C-K-map "\013" 'copy-region-as-kill)
(define-key C-K-map "q" 'kill-emacs)
(define-key C-K-map "\021" 'kill-emacs)
(define-key C-K-map "r" 'insert-file)
(define-key C-K-map "\022" 'insert-file)
(define-key C-K-map "s" 'save-some-buffers)
(define-key C-K-map "\023" 'save-some-buffers)
(define-key C-K-map "\025" ())
(define-key C-K-map "v" 'yank)
(define-key C-K-map "\026" 'yank)
(define-key C-K-map "y" 'kill-region)
(define-key C-K-map "\031" 'kill-region)
(define-key C-K-map "\037" 'kill-emacs)

(setq C-O-map (make-keymap))
(define-key C-O-map " " ())
(define-key C-O-map "c" 'center-line)
(define-key C-O-map "\003" 'center-line)
(define-key C-O-map "b" 'switch-to-buffer)
(define-key C-O-map "\002" 'switch-to-buffer)
(define-key C-O-map "j" 'justify-current-line)
(define-key C-O-map "\012" 'justify-current-line)
(define-key C-O-map "k" 'kill-buffer)
(define-key C-O-map "\013" 'kill-buffer)
(define-key C-O-map "l" 'list-buffers)
(define-key C-O-map "\014" 'list-buffers)
(define-key C-O-map "m" 'auto-fill-mode)
(define-key C-O-map "\015" 'auto-fill-mode)
(define-key C-O-map "r" 'set-fill-column)
(define-key C-O-map "\022" 'set-fill-column)
(define-key C-O-map "\025" ())
(define-key C-O-map "wd" 'delete-other-windows)
(define-key C-O-map "wh" 'split-window-horizontally)
(define-key C-O-map "wo" 'other-window)
(define-key C-O-map "wv" 'split-window-vertically)

(setq C-Q-map (make-keymap))
(define-key C-Q-map " " ())
(define-key C-Q-map "a" 'replace-string)
(define-key C-Q-map "\001" 'replace-string)
(define-key C-Q-map "c" 'end-of-buffer)
(define-key C-Q-map "\003" 'end-of-buffer)
(define-key C-Q-map "d" 'end-of-line)
(define-key C-Q-map "\004" 'end-of-line)
(define-key C-Q-map "f" 're-search-forward)
(define-key C-Q-map "\006" 're-search-forward)
(define-key C-Q-map "r" 'beginning-of-buffer)
(define-key C-Q-map "\022" 'beginning-of-buffer)
(define-key C-Q-map "\025" ())
(define-key C-Q-map "y" 'kill-line)
(define-key C-Q-map "\031" 'kill-line)

(define-key global-map "\001" 'backward-word)
(define-key global-map "\002" 'fill-paragraph)
(define-key global-map "\003" 'scroll-up)
(define-key global-map "\004" 'forward-char)
(define-key global-map "\005" 'previous-line)
(define-key global-map "\006" 'forward-word)
(define-key global-map "\007" 'delete-char)
(define-key global-map "\010" 'backward-char)
(define-key global-map "\011" 'indent-for-tab-command)
(define-key global-map "\012" 'help-for-help)
(define-key global-map "\013" C-K-map)
(define-key global-map "\015" 'newline)
(define-key global-map "\017" C-O-map)
(define-key global-map "\020" 'quoted-insert)
(define-key global-map "\021" C-Q-map)
(define-key global-map "\022" 'scroll-down)
(define-key global-map "\023" 'backward-char)
(define-key global-map "\024" 'kill-word)
(define-key global-map "\025" 'quit)
(define-key global-map "\026" 'overwrite-mode)
(define-key global-map "\030" 'next-line)
(define-key global-map "\031" '(lambda () (interactive)
				(beginning-of-line)
				(delete-region (point)
					       (progn (forward-line 1)
						      (point)))))
(define-key global-map "\035" 'quoted-insert)
(define-key global-map "\037" C-Q-map)

(setq default-fill-column 65)


