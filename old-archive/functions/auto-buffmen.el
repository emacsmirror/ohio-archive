;Article 517 of gnu.emacs
;Path: ark1!uakari.primate.wisc.edu!uwm.edu!wuarchive!wugate!uunet!xyzzy!tiktok!meissner
;From: meissner@tiktok.dg.com (Michael Meissner)
;Newsgroups: gnu.emacs
;Subject: Re: emacs file1 file2 ---> initiates emacs with multiple buffers.
;Message-ID: <1668@xyzzy.UUCP>
;Date: 8 Oct 89 18:41:30 GMT
;References: <22598@sequent.UUCP>
;Sender: usenet@xyzzy.UUCP
;Reply-To: meissner@tiktok.UUCP (Michael Meissner)
;Distribution: gnu
;Organization: Data General (Languages @ Research Triangle Park, NC.)
;Lines: 34
;Keywords: Multiple buffers,
;
;In article <22598@sequent.UUCP> paulr@sequent.UUCP (Paul Reger) writes:
;| Awhile back I posted to this group asking if I initiated emacs with
;| multiple file arguments:
;| 
;| emacs file1 file2 ... filen
;| 
;| How would I go about making emacs come up with 2 buffers or maybe just
;| the *Buffer List* menu.  ...
;
;However the auto-buffer-menu code as posted does contain an assumption
;about how many buffers emacs starts with.  I tend to do things like
;create a *server* buffer in my .emacs file when I automatically start
;the server.  Also, the assumption might break when emacs version 19
;comes out.  Here is a revised auto-buffer-menu, that actually checks
;whether the buffers listed are connected to files (with the elisp
;buffer-file-name function):

(defun auto-buffer-menu ()
  (let ((count 0) buffer (buf-list (buffer-list)))
    (while buf-list
      (setq buffer (car buf-list))
      (setq buf-list (cdr buf-list))
      (if (buffer-file-name buffer)
	  (setq count (+ count 1))))
    (cond ((> count 2)
	   (buffer-menu t)
	   (delete-other-windows))
	  ((= count 2)
	   (pop-to-buffer nil)))))

;--
;Michael Meissner, Data General.
;Uucp:		...!mcnc!rti!xyzzy!meissner		If compiles were much
;Internet:	meissner@dg-rtp.DG.COM			faster, when would we
;Old Internet:	meissner%dg-rtp.DG.COM@relay.cs.net	have time for netnews?
