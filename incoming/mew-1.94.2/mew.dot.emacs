;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sample file of .emacs
;;;
;
; This is just a sample. You should customize as you like...
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Site configuration
;;;

;(autoload 'mew "mew" nil t)
;(autoload 'mew-send "mew" nil t)
;(setq mew-mail-domain-list '("your mail domain"))
;(setq mew-icon-directory "icon directory")
;(autoload 'mew-user-agent-compose "mew" nil t)
;(if (boundp 'mail-user-agent)
;    (setq mail-user-agent 'mew-user-agent))
;(if (fboundp 'define-mail-user-agent)
;    (define-mail-user-agent
;      'mew-user-agent
;      'mew-user-agent-compose
;      'mew-draft-send-letter
;      'mew-draft-kill
;      'mew-send-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Window tips
;;;

;; If you use XEmacs and your video chip provides only limited
;; color map(e.g. 256), put the following line to avoid exhaustion
;; of colors.
;(setq mew-demo-picture nil)

(cond
 ((featurep 'xemacs)
;  (setq-default buffer-file-coding-system 'iso-2022-jp)
;  (setq keyboard-coding-system    'iso-2022-jp)
;  (setq terminal-coding-system    'iso-2022-jp)
  (add-menu-item '("Apps") "Read Mail (Mew)" 'mew t "Read Mail (VM)...")
  (add-menu-item '("Apps") "Send Mail (Mew)" 'mew-send t "Read Mail (VM)...")
  (delete-menu-item '("Apps" "Read Mail (VM)..."))
  (delete-menu-item '("Apps" "Read Mail (MH)..."))
  (delete-menu-item '("Apps" "Send mail..."))
  (setq toolbar-mail-reader 'Mew)
  (setq toolbar-mail-commands-alist
	(cons '(Mew . mew) toolbar-mail-commands-alist))
  )
 ((>= emacs-major-version 20)
;  (setq standard-fontset-spec14
;	"-*-fixed-medium-r-normal-*-14-*-*-*-*-*-fontset-standard")
;  (create-fontset-from-fontset-spec standard-fontset-spec14 nil 'noerror)
;  (setq default-frame-alist 
;	(cons (cons 'font standard-fontset-spec14) default-frame-alist)))
  (set-face-font 'bold '(bold) t)
  (set-face-font 'italic '(italic) t)
  (set-face-font 'bold-italic '(bold italic) t)
;  (set-language-environment "Japanese")
  )
 (t ;; Mule 2.3 or Emacs 19
  )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Citation tip
;;;
;;;
;;; From: Kazu Yamamoto <Kazu@Mew.org>
;;; Subject: Hello World
;;; Date: Thu, 08 Dec 1994 11:44:08 +0900
;;; Message-Id: <199509291309.WAA21663@decpc07.aist-nara.ac.jp>
;;;
;;; kazu> The quick brown fox jumped over the very lazy dog.
;;;

(setq mew-cite-fields '("From:" "Subject:" "Date:" "Message-ID:"))
(setq mew-cite-format "From: %s\nSubject: %s\nDate: %s\nMessage-ID: %s\n\n")
(setq mew-cite-prefix-function 'mew-cite-prefix-username)
;(setq mew-addrbook-for-cite-label 'nickname)
;(setq mew-addrbook-for-cite-prefix 'nickname)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Poor OSes
;;;

;; If pipe of your OS is not stable, both 'imget' and 'imls' don't
;; succeed. If so, add the following.
;(setq mew-connection-type1 t) ;; use PTY

;; If your OS doesn't have link count for directories, add the following.
;; This gets bootup faster.
;(setq mew-folder-list-skip-pattern "^[0-9]+$")

;;;
;;; End
;;;
