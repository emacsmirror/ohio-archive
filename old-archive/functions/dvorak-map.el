;From utkcs2!emory!samsung!cs.utexas.edu!mailrus!ames!bionet!agate!shelby!lindy!drapeau Thu Jun 28 14:50:36 EDT 1990
;Article 18367 of comp.windows.x:
;Path: utkcs2!emory!samsung!cs.utexas.edu!mailrus!ames!bionet!agate!shelby!lindy!drapeau
;>From: drapeau@sioux.Stanford.EDU (George Drapeau)
;Newsgroups: comp.windows.x
;Subject: Re: Dvorak keyboard layout for Xmodmap
;Message-ID: <DRAPEAU.90Jun27231718@sioux.Stanford.EDU>
;Date: 28 Jun 90 06:17:18 GMT
;References: <3930013@hpclsv.HP.COM>
;Sender: root@lindy.Stanford.EDU (Rooter)
;Organization: Interactive Classroom Experiment, Stanford University
;Lines: 163
;In-reply-to: sundarv@hpclsv.HP.COM's message of 22 Jun 90 18:18:29 GMT
;
;In article <3930013@hpclsv.HP.COM> sundarv@hpclsv.HP.COM (Sundar Varadarajan) writes:
;
;   Does someone have a file that I can feed to xmodmap to get the dvorak
;   keyboard layout on my qwerty keyboard?
;
;When I learned the Dvorak layout, I wanted to be able to switch back
;and forth on an application-by application basis, so that I could have
;one xterm with a "talk" session going on in Qwerty (which was still
;more comfortable to me) and another xterm session with the Dvorak
;typing tutor program.  This solution is not solvable through xmodmap
;(although I think that xmodmap is a cool idea).
;
;So, I wrote a translation table.  In my opinion, just that I was able
;to remap my keyboard at the touch of a button from within xterm is
;proof that the translation tables are an excellent mechanism (Thanks,
;X-people).
;
;Here are two versions, neither of which are completely correct but are
;certainly good enough for typing.  Specifically what's not correct is
;that control keys retain their normal bindings; in other words, while
;typing "g" produces an "i", typing Control-g still produces Control-g.
;I did this because I tend to use control characters by feel rather
;than by thinking "Press Control-g".  To fully convert to Dvorak, I
;suppose that the Control sequences should also be rebound.
;
;The two bindings are for XTerm and for Emacs.  Load the first file
;into the X Resource Database, and maybe put the second file inte your
;~/.emacs file.
;
;To switch to Dvorak in xterm, press your "F1" key; to switch back to
;Qwerty, press your "F2" key.
;
;In Emacs, to switch to Dvorak, type "M-x Dvorak" (case is important,
;which probably means you can just type "M-x D" then press return).  To
;switch back to Qwerty, type "M-x Qwerty" ("M-x Q" is probably good
;enough).  If you don't remember your Dvorak, the Dvorak for the word
;"Qwerty" is "X,dokt" (in other words, if you're looking at the
;keyboard when you switch back to Qwerty, type "M-x X,dokt").
;
;Okay, enough talk.  I hope this helps you some.
;
;	George
;--------------- Translation table (plus some other Resources) ---------------
;*VT100.Translations:#override	\
;<Key>F1:	keymap(Dvorak)	\n\
;<Key>F2:	keymap(None)
;
;
;*DvorakKeymap.translations:#override\
;:Ctrl<Key>a:		string(0x01)	\n\
;:Ctrl<Key>b:		string(0x02)	\n\
;:Ctrl<Key>c:		string(0x03)	\n\
;:Ctrl<Key>d:		string(0x04)	\n\
;:Ctrl<Key>e:		string(0x05)	\n\
;:Ctrl<Key>f:		string(0x06)	\n\
;:Ctrl<Key>g:		string(0x07)	\n\
;:Ctrl<Key>h:		string(0x08)	\n\
;:Ctrl<Key>i:		string(0x09)	\n\
;:Ctrl<Key>j:		string(0x0a)	\n\
;:Ctrl<Key>k:		string(0x0b)	\n\
;:Ctrl<Key>l:		string(0x0c)	\n\
;:Ctrl<Key>m:		string(0x0d)	\n\
;:Ctrl<Key>n:		string(0x0e)	\n\
;:Ctrl<Key>o:		string(0x0f)	\n\
;:Ctrl<Key>p:		string(0x10)	\n\
;:Ctrl<Key>q:		string(0x11)	\n\
;:Ctrl<Key>r:		string(0x12)	\n\
;:Ctrl<Key>s:		string(0x13)	\n\
;:Ctrl<Key>t:		string(0x14)	\n\
;:Ctrl<Key>u:		string(0x15)	\n\
;:Ctrl<Key>v:		string(0x16)	\n\
;:Ctrl<Key>w:		string(0x17)	\n\
;:Ctrl<Key>x:		string(0x18)	\n\
;:Ctrl<Key>y:		string(0x19)	\n\
;:Ctrl<Key>z:		string(0x1a)	\n\
;:<Key>a:		string(a)	\n\
;:<Key>b:		string(x)	\n\
;:<Key>c:		string(j)	\n\
;:<Key>d:		string(e)	\n\
;:<Key>e:		string(".")	\n\
;:<Key>f:		string(u)	\n\
;:<Key>g:		string(i)	\n\
;:<Key>h:		string(d)	\n\
;:<Key>i:		string(c)	\n\
;:<Key>j:		string(h)	\n\
;:<Key>k:		string(t)	\n\
;:<Key>l:		string(n)	\n\
;:<Key>m:		string(m)	\n\
;:<Key>n:		string(b)	\n\
;:<Key>o:		string(r)	\n\
;:<Key>p:		string(l)	\n\
;:<Key>q:		string("'")	\n\
;:<Key>r:		string(p)	\n\
;:<Key>s:		string(o)	\n\
;:<Key>t:		string(y)	\n\
;:<Key>u:		string(g)	\n\
;:<Key>v:		string(k)	\n\
;:<Key>w:		string(",")	\n\
;:<Key>x:		string(q)	\n\
;:<Key>y:		string(f)	\n\
;:<Key>z:		string(";")	\n\
;:<Key>minus:		string("`")	\n\
;:<Key>semicolon:	string(s)	\n\
;:<Key>grave:		string("/")	\n\
;:<Key>apostrophe:	string("-")	\n\
;:<Key>comma:		string(w)	\n\
;:<Key>period:		string(v)	\n\
;:<Key>slash:		string(z)	\n\
;:<Key>quotedbl:		string("_")	\n\
;:<Key>underscore:	string("~")	\n\
;:<Key>asciitilde:	string("?")	\n\
;:<Key>A:		string(A)	\n\
;:<Key>B:		string(X)	\n\
;:<Key>C:		string(J)	\n\
;:<Key>D:		string(E)	\n\
;:<Key>E:		string(">")	\n\
;:<Key>F:		string(U)	\n\
;:<Key>G:		string(I)	\n\
;:<Key>H:		string(D)	\n\
;:<Key>I:		string(C)	\n\
;:<Key>J:		string(H)	\n\
;:<Key>K:		string(T)	\n\
;:<Key>L:		string(N)	\n\
;:<Key>M:		string(M)	\n\
;:<Key>N:		string(B)	\n\
;:<Key>O:		string(R)	\n\
;:<Key>P:		string(L)	\n\
;:<Key>Q:		string(0x22)	\n\
;:<Key>R:		string(P)	\n\
;:<Key>S:		string(O)	\n\
;:<Key>T:		string(Y)	\n\
;:<Key>U:		string(G)	\n\
;:<Key>V:		string(K)	\n\
;:<Key>W:		string("<")	\n\
;:<Key>X:		string(Q)	\n\
;:<Key>Y:		string(F)	\n\
;:<Key>Z:		string(":")	\n\
;:<Key>less:		string(W)	\n\
;:<Key>greater:		string(V)	\n\
;:<Key>question:		string(Z)	\n\
;:<Key>colon:		string(S)
;
;
;---------------------- Put this in your .emacs file ----------------------

(defun Dvorak ()
  "Written by George Drapeau, uses a translation table that turns your keyboard
into a Dvorak keyboard.  Try it, you'll like it."
  (interactive)
  (setq keyboard-translate-table "\C-@\C-a\C-b\C-c\C-d\C-e\C-f\C-g\C-h\C-i\C-j\C-k\C-l\C-m\C-n\C-o\C-p\C-q\C-r\C-s\C-t\C-u\C-v\C-w\C-x\C-y\C-z\033\034\035\036\037 !_#$%&-()*+w`vz0123456789SsW=VZ@AXJE>UIDCHTNMBRL\"POYGK<QF:[\\]^~/axje.uidchtnmbrl'po


