Date: 10 Dec 86 18:27:47 GMT
From: Dick King <KESTREL.ARPA!king@eddie.mit.edu>

If slow i-search popup windows came out on top rather than on the
bottom, then a search undertaken before a redisplay is complete would
show its results first rather than last.  This is a noticable
improvement at 1200 baud; it's not infrequent for me to do M-< ^S xxx.

I recommend the following additions to isearch.el

add:
(defvar search-slow-window-on-top nil
  "*make non-nil if you want the search popup-window on top.")

In (defun isearch ...:

		     (split-window nil (if search-slow-window-on-top
					   (1+ search-slow-window-lines)
					 (- (window-height)
					    (1+ search-slow-window-lines))))
		     (or search-slow-window-on-top (other-window 1))

in place of 

		     (split-window nil (- (window-height)
					  (1+ search-slow-window-lines)))
		     (other-window 1)

.

-dick
