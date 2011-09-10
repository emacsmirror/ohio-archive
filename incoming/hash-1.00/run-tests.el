;; Load this file to run the validation tests of the primes library
;; and the hash library.  
;;
:; The version 19 profile library does invoke (provide 'profile), so
;; we do it here, and load that library explicitly.
;;
;; These tests have been carried out successfully on several UNIX
;; architectures running emacs-19.34 and emacs-20.3.6.

(defun test-setup ()
  (setq load-path (cons "../primes" load-path))
  (setq load-path (cons "./" load-path))
  (load-library "profile")
  (provide 'profile)
  (load-library "test-hash.el")
  (load-library "test-primes.el"))

(eval-when-compile
  (setq load-path (cons "../primes" load-path))
  (setq load-path (cons "./" load-path))
  (load-library "profile")
  (provide 'profile)
  (load-library "test-hash.el")
  (load-library "test-primes.el"))

(test-setup)

(test-primes-with-profile)
(switch-to-buffer "*profile*")
(rename-buffer "*test-primes-profile*" t)

(test-hash-with-profile)
(switch-to-buffer "*profile*")
(rename-buffer "*test-hash-profile*" t)
