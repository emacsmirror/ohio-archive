(define-key help-map "g" 'get-latexinfo-node)

(defun get-latexinfo-node (string)
  (interactive "sDescribe Latexinfo Topic: ")
  (let* ((info-file-name
	  (save-excursion
	    (set-buffer "*info*")
	    (buffer-file-name)))
	 (index-file-name
	  (concat
	   (substring info-file-name 0 (- (length info-file-name) 2))
	   "dex")))
    (if (and (file-exists-p index-file-name)
	     (file-exists-p info-file-name))
	(get-node index-file-name info-file-name string)
      (error "File(s) not found! \n  %s \n  %s"
	     index-file-name info-file-name
	     ))))

(require 'info)
;;(autoload 'Info-find-node "info" )

(defun get-node (index-file manual-file string)
  (interactive "fIndex File Name; \nfManual File Name; \nsTopic: ")
    (let ((byte) (index-buffer))
      (save-excursion
	(find-file index-file)
	(setq index-buffer (current-buffer))
	(goto-char (point-min))
	(binary-search-forward string)
      	(setq byte (read (current-buffer))))
      (setq manual-file
	    (expand-file-name manual-file))
      (find-file manual-file)
      (save-excursion
	(widen)
	(goto-char byte)
	;; Find beginning of node.
	(search-backward "\n\^_")
	(forward-line 2)
	;; Get nodename spelled as it is in the node.
	(re-search-forward "Node:[ \t]*")
	(setq Info-current-node
	      (buffer-substring (point)
				(progn
				  (skip-chars-forward "^,\t\n")
				  (point)))))
      (message "Node: %s File: %s"
	    Info-current-node manual-file)
   (Info-find-node manual-file Info-current-node)
   (bury-buffer index-buffer)
   (goto-char byte)))

(defvar *binary-search-limit* 300
  "The point of diminuishing returns for a binary-search.")

(defun binary-search-forward (string)
  "Search for the string STRING in the current buffer using a binary search"
  (setq string (concat "\"" string "\""))
  (if (< (point-max) *binary-search-limit*)
      	(re-search-forward (concat "^" (regexp-quote string)))
      (setq string (concat string " "))
      (let* ((hi (point-max))
	     (lo (point-min))
	     (mid (/ (+ hi lo) 2))
	     (len (length string)))
	(while (>= (- hi lo) *binary-search-limit*)
	  (goto-char mid)
	  (forward-line 1)
	  (if (string< (buffer-substring (point) (+ (point) len)) string)
	      (setq lo mid)
	    (setq hi mid))
	  (setq mid (/ (+ hi lo) 2)))
	(goto-char lo)
	(re-search-forward (concat "^" (regexp-quote string))))))

