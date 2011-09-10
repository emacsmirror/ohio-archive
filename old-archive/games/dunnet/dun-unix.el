;;;;;;;;;;;;;;;;;;;
;;;;
;;;; UNIX
;;;;
;;;;;;;;;;;;;;;;;;;

(if nil
    (eval-and-compile (setq byte-compile-warnings nil)))

(defun unix-parse (args)
  (interactive "*p")
  (beginning-of-line)
  (let (beg esign)
    (setq beg (+ (point) 2))
    (end-of-line)
    (if (and (not (= beg (point)))
	     (string= "$" (buffer-substring (- beg 2) (- beg 1))))
	(progn
	  (setq line (downcase (buffer-substring beg (point))))
	  (princ line)
	  (if (eq (parse2 nil unix-verbs line) -1)
	      (progn
		(if (setq esign (string-match "=" line))
		    (doassign line esign)		
		  (mprinc (car line-list))
		  (mprincl ": not found.")))))
      (goto-char (point-max))
      (mprinc "\n"))
    (if (eq dungeon-mode 'unix)
	(progn
	  (fix-screen)
	  (mprinc "$ ")))))

(defun doassign (line esign)
  (if (not wizard)
      (let (passwd)
	(mprinc "Enter wizard password: ")
	(setq passwd (read-line))
	(if (not batch-mode)
	    (mprinc "\n"))
	(if (string= passwd "moby")
	    (progn
	      (setq wizard t)
	      (doassign line esign))
	  (mprincl "Incorrect.")))

    (let (varname epoint afterq i value)
      (setq varname (substring line 0 esign))
      (if (not (setq epoint (string-match ")" line)))
	  (if (string= (substring line (1+ esign) (+ esign 2))
		       "\"")
	      (progn
		(setq afterq (substring line (+ esign 2)))
		(setq epoint (+
			      (string-match "\"" afterq)
			      (+ esign 3))))
	    
	    (if (not (setq epoint (string-match " " line)))
		(setq epoint (length line))))
	(setq epoint (1+ epoint))
	(while (and
		(not (= epoint (length line)))
		(setq i (string-match ")" (substring line epoint))))
	  (setq epoint (+ epoint i 1))))
      (setq value (substring line (1+ esign) epoint))
      (dungeon-eval varname value))))

(defun dungeon-eval (varname value)
  (let (eval-error)
    (switch-to-buffer (get-buffer-create "*dungeon-eval*"))
    (erase-buffer)
    (insert "(setq ")
    (insert varname)
    (insert " ")
    (insert value)
    (insert ")")
    (setq eval-error nil)
    (condition-case nil
	(eval-current-buffer)
      (error (setq eval-error t)))
    (kill-buffer (current-buffer))
    (switch-to-buffer "*dungeon*")
    (if eval-error
	(mprincl "Invalid syntax."))))
  

(defun unix-interface ()
  (login)
  (if logged-in
      (progn
	(setq dungeon-mode 'unix)
	(define-key dungeon-mode-map "\r" 'unix-parse)
	(mprinc "$ "))))



(defun login ()
  (let (tries username password)
    (setq tries 4)
    (while (and (not logged-in) (> (setq tries (- tries 1)) 0))
      (mprinc "\n\nUNIX System V, Release 2.2 (pokey)\n\nlogin: ")
      (setq username (read-line))
      (if (not batch-mode)
	  (mprinc "\n"))
      (mprinc "password: ")
      (setq password (read-line))
      (if (not batch-mode)
	  (mprinc "\n"))
      (if (or (not (string= username "toukmond"))
	      (not (string= password "robert")))
	  (mprincl "login incorrect")
	(setq logged-in t)
	(mprincl "
Welcome to Unix\n
Please clean up your directories.  The filesystem is getting full.
Our tcp/ip link to gamma is a little flakey, but seems to work.
The current version of ftp can only send files from the current
directory, and deletes them after they are sent!  Be careful.

Note: Restricted bourne shell in use.\n")))
  (setq dungeon-mode 'dungeon)))

(defun ls (args)
  (if (car args)
      (let (ocdpath ocdroom)
	(setq ocdpath cdpath)
	(setq ocdroom cdroom)
	(if (not (eq (dunnet-cd args) -2))
	    (ls nil))
	(setq cdpath ocdpath)
	(setq cdroom ocdroom))
    (if (= cdroom -10)
	(ls-inven))
    (if (= cdroom -2)
	(ls-rooms))
    (if (= cdroom -3)
	(ls-root))
    (if (= cdroom -4)
	(ls-usr))
    (if (> cdroom 0)
	(ls-room))))

(defun ls-root ()
  (mprincl "total 4
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 usr
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 rooms"))

(defun ls-usr ()
  (mprincl "total 4
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..
drwxr-xr-x  3 toukmond restricted      512 Jan 1 1970 toukmond"))

(defun ls-rooms ()
  (mprincl "total 16
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..")
  (dolist (x visited)
    (mprinc
"drwxr-xr-x  3 root     staff           512 Jan 1 1970 ")
    (mprincl (nth x room-shorts))))

(defun ls-room ()
  (mprincl "total 4
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..
-rwxr-xr-x  3 root     staff          2048 Jan 1 1970 description")
  (dolist (x (nth cdroom room-objects))
    (if (and (>= x 0) (not (= x 255)))
	(progn
	  (mprinc "-rwxr-xr-x  1 toukmond restricted        0 Jan 1 1970 ")
	  (mprincl (nth x objfiles))))))

(defun ls-inven ()
  (mprinc "total 467
drwxr-xr-x  3 toukmond restricted      512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..")
  (dolist (x unix-verbs)
    (if (not (eq (car x) 'IMPOSSIBLE))
	(progn
	  (mprinc"
-rwxr-xr-x  1 toukmond restricted    10423 Jan 1 1970 ")
	  (mprinc (car x)))))
  (mprinc "\n")
  (if (not uncompressed)
      (mprincl
"-rwxr-xr-x  1 toukmond restricted        0 Jan 1 1970 paper.o.Z"))
  (dolist (x inventory)
    (mprinc 
"-rwxr-xr-x  1 toukmond restricted        0 Jan 1 1970 ")
    (mprincl (nth x objfiles))))

(defun echo (args)
  (let (nomore var)
    (setq nomore nil)
    (dolist (x args)
	    (if (not nomore)
		(progn
		  (if (not (string= (substring x 0 1) "$"))
		      (progn
			(mprinc x)
			(mprinc " "))
		    (setq var (intern (substring x 1)))
		    (if (not (boundp var))
			(mprinc " ")
		      (if (member var restricted)
			  (progn
			    (mprinc var)
			    (mprinc ": Permission denied")
			    (setq nomore t))
			(eval (list 'mprinc var))
			(mprinc " ")))))))
	    (mprinc "\n")))


(defun ftp (args)
  (let (host username passwd ident newlist)
    (if (not (car args))
	(mprincl "ftp: hostname required on command line.")
      (setq host (intern (car args)))
      (if (not (member host '(gamma endgame)))
	  (mprincl "ftp: Unknown host.")
	(if (eq host 'endgame)
	    (mprincl "ftp: connection to endgame not allowed")
	  (if (not ethernet)
	      (mprincl "ftp: host not responding.")
	    (mprincl "Connected to gamma. FTP ver 0.9 00:00:00 01/01/70")
	    (mprinc "Username: ")
	    (setq username (read-line))
	    (if (string= username "toukmond")
		(if batch-mode
		    (mprincl "toukmond ftp access not allowed.")
		  (mprincl "\ntoukmond ftp access not allowed."))
	      (if (string= username "anonymous")
		  (if batch-mode
		      (mprincl
		       "Guest login okay, send your user ident as password.")
		    (mprincl 
		     "\nGuest login okay, send your user ident as password."))
		(if batch-mode
		    (mprinc "Password required for ")
		  (mprinc "\nPassword required for "))
		(mprincl username))
	      (mprinc "Password: ")
	      (setq ident (read-line))
	      (if (not (string= username "anonymous"))
		  (if batch-mode
		      (mprincl "Login failed.")
		    (mprincl "\nLogin failed."))
		(if batch-mode
		   (mprincl "Guest login okay, user access restrictions apply.")
		  (mprincl "\nGuest login okay, user access restrictions apply."))
		(ftp-commands)
		(setq newlist 
'("What password did you use during anonymous ftp to gamma?"))
		(setq newlist (append newlist (list ident)))
		(rplaca (nthcdr 1 endgame-questions) newlist)))))))))
  
(defun ftp-commands ()
    (setq exitf nil)
    (let (line)
      (while (not exitf)
	(mprinc "ftp> ")
	(setq line (read-line))
	(if 
	    (eq
	     (parse2 nil 
		    '((type . ftptype) (binary . bin) (bin . bin) (send . send)
		      (put . send) (quit . ftpquit) (help . ftphelp)
		      (ascii . fascii)
		      ) line)
	     -1)
	    (mprincl "No such command.  Try help.")))
      (setq ftptype 'ascii)))

(defun ftptype (args)
  (if (not (car args))
      (mprincl "Usage: type [binary | ascii]")
    (setq args (intern (car args)))
    (if (eq args 'binary)
	(bin nil)
      (if (eq args 'ascii)
	  (fascii 'nil)
	(mprincl "Unknown type.")))))

(defun bin (args)
  (mprincl "Type set to binary.")
  (setq ftptype 'binary))

(defun fascii (args)
  (mprincl "Type set to ascii.")
  (setq ftptype 'ascii))

(defun ftpquit (args)
  (setq exitf t))

(defun send (args)
  (if (not (car args))
      (mprincl "Usage: send <filename>")
    (setq args (car args))
    (let (counter foo)
      (setq foo nil)
      (setq counter 0)

;;; User can send commands!  Stupid user.


      (if (assq (intern args) unix-verbs)
	  (progn
	    (rplaca (assq (intern args) unix-verbs) 'IMPOSSIBLE)
	    (mprinc "Sending ")
	    (mprinc ftptype)
	    (mprinc " file for ")
	    (mprincl args)
	    (mprincl "Transfer complete."))

	(dolist (x objfiles)
	  (if (string= args x)
	      (progn
		(if (not (member counter inventory))
		    (progn
		      (mprincl "No such file.")
		      (setq foo t))
		  (mprinc "Sending ")
		  (mprinc ftptype)
		  (mprinc " file for ")
		  (mprinc (downcase (cadr (nth counter objects))))
		  (mprincl ", (0 bytes)")
		  (if (not (eq ftptype 'binary))
		      (progn
			(if (not (member obj-protoplasm
					 (nth receiving-room room-objects)))
			    (replace room-objects receiving-room
				     (append (nth receiving-room room-objects)
					     (list obj-protoplasm))))
			(remove-obj-from-inven counter))
		    (remove-obj-from-inven counter)
		    (replace room-objects receiving-room
			     (append (nth receiving-room room-objects)
				     (list counter))))
		  (setq foo t)
		  (mprincl "Transfer complete."))))
	  (setq counter (+ 1 counter)))
	(if (not foo)
	    (mprincl "No such file."))))))

(defun ftphelp (args)
  (mprincl 
   "Possible commands are:\nsend    quit    type   ascii  binary   help"))

(defun uexit (args)
  (setq dungeon-mode 'dungeon)
  (mprincl "\nYou step back from the console.")
  (define-key dungeon-mode-map "\r" 'dungeon-parse)
  (if (not batch-mode)
      (dungeon-messages)))

(defun dunnet-pwd (args)
  (mprincl cdpath))

(defun uncompress (args)
  (if (not (car args))
      (mprincl "Usage: uncompress <filename>")
    (setq args (car args))
    (if (or uncompressed
	    (and (not (string= args "paper.o"))
		 (not (string= args "paper.o.z"))))
	(mprincl "Uncompress command failed.")
      (setq uncompressed t)
      (setq inventory (append inventory (list obj-paper))))))

(defun rlogin (args)
  (let (passwd)
    (if (not (car args))
	(mprincl "Usage: rlogin <hostname>")
      (setq args (car args))
      (if (string= args "endgame")
	  (rlogin-endgame)
	(if (not (string= args "gamma"))
	    (mprincl "No such host.")
	  (if (not ethernet)
	      (mprincl "Host not responding.")
	    (mprinc "Password: ")
	    (setq passwd (read-line))
	    (if (not (string= passwd "worms"))
		(mprincl "\nlogin incorrect")
	      (mprinc 
"\nYou begin to feel strange for a moment, and you lose your items."
)
	      (replace room-objects computer-room 
		       (append (nth computer-room room-objects) inventory))
	      (setq inventory nil)
	      (setq current-room receiving-room)
	      (uexit nil))))))))
  
(defun dunnet-cd (args)
  (let (tcdpath tcdroom path-elemants room-check)
    (if (not (car args))
	(mprincl "Usage: cd <path>")
      (setq tcdpath cdpath)
      (setq tcdroom cdroom)
      (setq badcd nil)
      (condition-case nil
	  (setq path-elements (get-path (car args) nil))
	(error (mprincl "Invalid path.")
	       (setq badcd t)))
      (dolist (pe path-elements)
	      (unless badcd
		      (if (not (string= pe "."))
			  (if (string= pe "..")
			      (progn
				(if (> tcdroom 0)                  ;In a room
				    (progn
				      (setq tcdpath "/rooms")
				      (setq tcdroom -2))
					;In /rooms,/usr,root
				  (if (or 
				       (= tcdroom -2) (= tcdroom -4) 
				       (= tcdroom -3))
				      (progn
					(setq tcdpath "/")
					(setq tcdroom -3))
				    (if (= tcdroom -10)       ;In /usr/toukmond
					(progn
					  (setq tcdpath "/usr")
					  (setq tcdroom -4))))))
			    (if (string= pe "/")
				(progn
				  (setq tcdpath "/")
				  (setq tcdroom -3))
			      (if (= tcdroom -4)
				  (if (string= pe "toukmond")
				      (progn
					(setq tcdpath "/usr/toukmond")
					(setq tcdroom -10))
				    (nosuchdir))
				(if (= tcdroom -10)
				    (nosuchdir)
				  (if (> tcdroom 0)
				      (nosuchdir)
				    (if (= tcdroom -3)
					(progn
					  (if (string= pe "rooms")
					      (progn
						(setq tcdpath "/rooms")
						(setq tcdroom -2))
					    (if (string= pe "usr")
						(progn
						  (setq tcdpath "/usr")
						  (setq tcdroom -4))
					      (nosuchdir))))
				      (if (= tcdroom -2)
					  (progn
					    (dolist (x visited)
						    (setq room-check 
							  (nth x room-shorts))
						    (if (string= room-check pe)
							(progn
							  (setq tcdpath 
						 (concat "/rooms/" room-check))
							  (setq tcdroom x))))
					    (if (= tcdroom -2)
						(nosuchdir)))))))))))))
      (if (not badcd)
	  (progn
	    (setq cdpath tcdpath)
	    (setq cdroom tcdroom)
	    0)
      -2))))

(defun nosuchdir ()
  (mprincl "No such directory.")
  (setq badcd t))

(defun cat (args)
  (let (doto checklist)
    (if (not (setq args (car args)))
	(mprincl "Usage: cat <ascii-file-name>")
      (if (string-match "/" args)
	  (mprincl "cat: only files in current directory allowed.")
	(if (and (> cdroom 0) (string= args "description"))
	    (mprincl (car (nth cdroom rooms)))
	  (if (setq doto (string-match "\\.o" args))
	      (progn
		(if (= cdroom -10)
		    (setq checklist inventory)
		  (setq checklist (nth cdroom room-objects)))
		(if (not (member (cdr 
				  (assq (intern 
					 (substring args 0 doto)) objnames))
				 checklist))
		    (mprincl "File not found.")
		  (mprincl "Ascii files only.")))
	    (if (assq (intern args) unix-verbs)
		(mprincl "Ascii files only.")
	      (mprincl "File not found."))))))))
  
(defun zippy (args)
  (mprincl (yow)))

(defun rlogin-endgame ()
  (if (not (= (score nil) 90))
      (mprincl "You have not achieved enough points to connect to endgame.")
    (mprincl"\nWelcome to the endgame.  You are a truly noble adventurer.")
    (setq current-room treasure-room)
    (setq endgame t)
    (replace room-objects endgame-treasure-room (list obj-bill))
    (uexit nil)))
