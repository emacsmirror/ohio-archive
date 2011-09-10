;;; -*- Emacs-Lisp -*-
;;; $Id: shell-command.el,v 1.3 1998/11/27 14:03:33 tsuchiya Exp $

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: shell

;;; Commentary:

;; shell-command-with-completion �ϡ����ޥ�����ϻ����䴰�����Ѥ��뤳
;; �Ȥ������褦�ˤ��� shell-command �Ǥ���

;;; Install:

;; ���Υե������Ŭ���ʾ��˥��ԡ����Ƥ��顢���λ���� ~/.emacs ����
;; ���ä��Ʋ�������
;;
;; (autoload 'shell-command-with-completion
;;   "shell-command" "Alternate shell-command" t nil)
;; (define-key global-map "\e!" 'shell-command-with-completion)
;; (autoload 'shell-command-with-completion-on-region
;;   "shell-command" "Alternate shell-command-on-region" t nil)
;; (define-key global-map "\e|" 'shell-command-with-completion-on-region)


;;; ����

(require 'shell)
(require 'comint)


(defvar shell-command-complete-functions
  '(shell-dynamic-complete-environment-variable
    shell-dynamic-complete-command
    shell-replace-by-expanded-directory
    comint-dynamic-complete-filename)
  "*Function list to complete shell commands for shell-command-with-completion")


(defun shell-command-with-completion (command &optional output-buffer)
  "Alternate shell-command"
  (interactive
   (let* ((keymap (copy-keymap minibuffer-local-map))
          (string (unwind-protect
                      (progn
                        (define-key minibuffer-local-map "\t"
                          '(lambda ()
                             (interactive)
                             (run-hook-with-args-until-success 'shell-command-complete-functions)))
                        (read-from-minibuffer (format "Shell command [%s]: " (substring default-directory 0 -1))
                                              nil nil nil 'shell-command-history))
                    (setq minibuffer-local-map keymap))))
     (list string
           current-prefix-arg)))
  (shell-command command output-buffer))


(defun shell-command-with-completion-on-region (start end command &optional output-buffer replace)
  "Alternate shell-command-on-region"
  (interactive
   (let* ((keymap (copy-keymap minibuffer-local-map))
          (string (unwind-protect
                      (progn
                        (define-key minibuffer-local-map "\t"
                          '(lambda ()
                             (interactive)
                             (run-hook-with-args-until-success 'shell-command-complete-functions)))
                        (read-from-minibuffer (format "Shell command on region [%s]: " (substring default-directory 0 -1))
                                              nil nil nil 'shell-command-history))
                    (setq minibuffer-local-map keymap))))
     (list (region-beginning)
           (region-end)
           string
           current-prefix-arg
           current-prefix-arg)))
  (shell-command-on-region start end command output-buffer replace))
