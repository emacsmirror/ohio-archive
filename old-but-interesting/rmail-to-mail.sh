#!/bin/sh
#
# usage:	rmail-to-mail file1 file2 ...
#
# result:	file1~     is the original Babyl file
#		file1      is an empty Babyl file
#		file1.mail is a UNIX mail file with the messages from file1

cat > /tmp/$$.el << EOF
;; LCD Archive Entry:
;; rmail-to-mail|Skip Montanaro, Nate Hess|montanaro@sprite.crd.ge.com|
;; Convert an rmail babyl file to unix mail.|
;; 19-May-1993||~/functions/rmail-to-mail.el.Z|

(let ((file-to-be-converted (buffer-file-name))
      (message-count 0)
      (rmail-delete-after-output t)
      (unix-style-file-name (concat (buffer-file-name) ".mail")))
  (kill-buffer (current-buffer))
  (rmail file-to-be-converted)
  (rmail-show-message 1)
  (while (not (rmail-output unix-style-file-name))
    (setq message-count (1+ message-count)))
  (rmail-quit))
EOF

for f in "$@" ; do
    echo ">>> Doing $f"
    emacs -batch $f -l /tmp/$$.el -kill
    echo ">>> Done"
done

rm -f /tmp/$$.el
