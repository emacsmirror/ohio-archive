;;; mew-virtual.el --- Virtual mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-virtual-version "mew-virtual.el version 0.18")

(require 'mew)

;;;
;;; Virtual mode
;;;

(defun mew-virtual-mode ()
  "Mew Virtual mode:: major mode to visualize messages in a virtual folder.
For more information, see the document of 'mew-summary-mode'."
  (interactive)
  (setq major-mode 'mew-virtual-mode)
  (setq mode-name "Virtual")
  (setq mode-line-buffer-identification mew-mode-line-id)
  (use-local-map mew-summary-mode-map)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq selective-display t)
  (setq selective-display-ellipses nil)
  (mew-summary-setup-mode-line)
  (mew-summary-setup-menu)
  (mew-virtual-highlight-setup)
  (mew-highlight-cursor-line)
  (run-hooks 'mew-virtual-mode-hook))

(defun mew-virtual-folder-message ()
  "Display the real folder of this message."
  (interactive)
  (looking-at "^ *\\([0-9]+\\).*\r\\(.*\\)$")
  (message "%s" (mew-match 2)))

(defun mew-summary-virtual ()
  "Create one Virtual mode with inputed pattern."
  (interactive)
  (mew-summary-only
   (let ((folder (concat 
		  "++" 
		  (mew-input-string "Virtual folder name %s(%s): " 
				    "" ;; dummy
				    "virtual")))
	 (folders (mew-input-folders (buffer-name)))
	 (grep (mew-input-pick-pattern)))
     (mew-folder-setup folder)
     (mew-buffers-setup folder)
     (mew-summary-scan-body mew-prog-imls
			    'mew-virtual-mode
			    folder
			    mew-cs-virtual
			    nil
			    folders
			    grep))))

(provide 'mew-virtual)

;;; Copyright Notice:

;; Copyright (C) 1996, 1997, 1998, 1999 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-virtual.el ends here
