(in-package :elispdb)

(defvar *araneida-url* (make-instance 'araneida:http-url
				      :host *araneida-site-name*))
(defvar *araneida-server*
  (make-instance 'araneida:server
                 :name *araneida-site-name*
                 :base-url *araneida-url* :port 8000))

(defun install-handlers ()
  (araneida:export-server *araneida-server*)

  (araneida:export-handler (araneida:merge-url *araneida-url* "/search") 
			   'elispdb::search-handler :match :prefix
			   :method t :stage :response)

  (araneida:export-handler (araneida:merge-url *araneida-url* "/emacs-lisp")
			   `(file-request-handler "/var/www/emacs-lisp/")
			   :match :prefix :method t :stage :response)
  (araneida:export-handler *araneida-url*
			   (lambda (r rest)
			     (araneida:request-redirect
			      r #u"http://www.cis.ohio-state.edu/emacs-lisp/"))
			   :match :prefix :method t :stage :response))
  
