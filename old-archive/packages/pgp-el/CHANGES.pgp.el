09-Aug.17:46   jackr     create version "../homeVob/emacs/pgp.el@@/main/197"
  "Update all date and version stamps"
09-Aug.17:20   jackr     destroy version on branch "../homeVob/emacs/pgp.el@@/main"
  "Destroyed version "/main/195"."
09-Aug.17:19   jackr     destroy version on branch "../homeVob/emacs/pgp.el@@/main"
  "Destroyed version "/main/196"."
09-Aug.17:13   jackr     create version "../homeVob/emacs/pgp.el@@/main/194"
  "shift strings to match dovervars:
   $Revision:$, $Date$"
09-Aug.15:06   jackr     create version "../homeVob/emacs/pgp.el@@/main/193"
  "Missing docstrings in defconsts for pgp-version and
   pgp-maintainer were causing byte-compilation to fail silently,
   sometimes."
09-Aug.14:58   jackr     create version "../homeVob/emacs/pgp.el@@/main/192"
  "typo in local-unset"
09-Aug.14:51   jackr     create version "../homeVob/emacs/pgp.el@@/main/191"
  "conform keybindings to latest mailcrypt"
17-Jul.16:07   jackr     create version "../homeVob/emacs/pgp.el@@/main/190"
  "make header-signing force clearsigning"
07-Jul.22:29   jackr     create version "../homeVob/emacs/pgp.el@@/main/189"
  "allow pgp-encrypt-with-my-key to have nil 'ask t"
07-Jul.22:14   jackr     create version "../homeVob/emacs/pgp.el@@/main/188"
  "during encryption, when prompting for encryptees, make
   end-of-list prompt "(end)", don't offer 'none' after some have
   been selected
   "
07-Jul.22:01   jackr     create version "../homeVob/emacs/pgp.el@@/main/187"
  "display decoded contents of a pubkeyblock before adding it
   allow user to accept or reject addition
   
   still can't select individual keys to add: all or nothing"
07-Jul.21:17   jackr     create version "../homeVob/emacs/pgp.el@@/main/186"
  "make flags accept nil / 'ask / t:
   	pgp-always-clear-sign (now named pgp-clear-sign)
   	pgp-untabify-clearsig
   	pgp-show-decrypted-boundaries
   	pgp-save-decoded-messages
   "
07-Jul.20:09   jackr     create version "../homeVob/emacs/pgp.el@@/main/185"
  "auto-set pgp-mode from the hook functions
   mark interactive functions for autoload
   don't declare things "interactive" that really aren't"
07-Jul.19:49   jackr     create version "../homeVob/emacs/pgp.el@@/main/184"
  "move pgp-header-sign-message to C-c C-p C-x, so C-h remains help
   and provides completion in the middle of the keysequence.  The
   C-x is supposed to be mnemonic for "Put the signature into the
   X-Pgp-Signed header item."  OK, fine: it's lame.  If you have a
   better inspiration, I'd love to hear it, but after three of us
   spent hours trying, this was the best we could find! ;-)"
07-Jul.19:12   jackr     create version "../homeVob/emacs/pgp.el@@/main/183"
  "Much better fix for the wandering "Process pgp-proc finished"s"
06-Jul.19:20   jackr     create version "../homeVob/emacs/pgp.el@@/main/182"
  "clear "Working.  Please wait" from encryption"
06-Jul.19:15   jackr     create version "../homeVob/emacs/pgp.el@@/main/181"
  "make timeout routines work in FSF and XEmacs (timer.el or itimer.el)
   "
06-Jul.17:31   jackr     create version "../homeVob/emacs/pgp.el@@/main/180"
  "streamline XEmacs menu code"
06-Jul.14:43   jackr     create version "../homeVob/emacs/pgp.el@@/main/179"
  "fix black-on-black for some B&W displays
   remove leftover trash from process-send-eof/kill
   
   "
06-Jul.12:21   jackr     create version "../homeVob/emacs/pgp.el@@/main/178"
  "even-better handling of (local-unset-key "P")
   "
29-Jun.14:39   jackr     create version "../homeVob/emacs/pgp.el@@/main/177"
  "gotta local-unset-key "P" before local-setting "PD"..."
28-Jun.22:26   jackr     create version "../homeVob/emacs/pgp.el@@/main/176"
  "Switch PGP i/o to subprocess and pipe, to avoid putting
   sensitive data into disk files"
28-Jun.20:09   jackr     create version "../homeVob/emacs/pgp.el@@/main/175"
  "change read keymap to reduce collisions probability:
     (local-set-key "PD" 'pgp-decode)
     (local-set-key "PK" 'pgp-add-key)
     (local-set-key "PV" 'pgp-decode)
   "
09-Jun.13:07   jackr     create version "../homeVob/emacs/pgp.el@@/main/174"
  "there's no such thing as pgp-decode-message
   where was my head????"
08-Jun.12:53   jackr     create version "../homeVob/emacs/pgp.el@@/main/173"
  "make write keymap entries \C-c\C-p\C-<x>"
08-Jun.12:45   jackr     create version "../homeVob/emacs/pgp.el@@/main/172"
  "add header-sign, check-and-mark to write keymap"
08-Jun.12:40   jackr     create version "../homeVob/emacs/pgp.el@@/main/171"
  "move write-keymap keys to C-c C-p <x>"
05-Jun.11:31   jackr     create version "../homeVob/emacs/pgp.el@@/main/170"
  "seem to have wrong thing bound to "Header Sign Message" in menu bar"
02-Jun.14:00   jackr     create version "../homeVob/emacs/pgp.el@@/main/169"
  "rename bug reporting routine so it begins with pgp-"
02-Jun.13:58   jackr     create version "../homeVob/emacs/pgp.el@@/main/168"
  "yet more PGP 3.0 disaster messages: "Warning: ASCII armor missing ...""
26-May.15:09   jackr     create version "../homeVob/emacs/pgp.el@@/main/167"
  "found some v.19/fsf problems with faces"
26-May.12:41   jackr     create version "../homeVob/emacs/pgp.el@@/main/166"
  "include decode in Lucid write menu"
25-May.12:48   jackr     create version "../homeVob/emacs/pgp.el@@/main/165"
  "catch (failing) attempt to decode conventional encryption
   
   (note, PGP inists on getting this passphrase from the terminal,
   so this can't be handled under Emacs without going to a
   subprocess and process-send-string and the like)
   "
25-May.12:41   jackr     create version "../homeVob/emacs/pgp.el@@/main/164"
  "lucidate"
17-May.10:01   jackr     create version "../homeVob/emacs/pgp.el@@/main/163"
  "default PGPPATH as pgp does"
02-Apr.15:38   jackr     create version "../homeVob/emacs/pgp.el@@/main/162"
  "Import changes made while on sabbatical"
21-Mar.14:08   jackr     create version "../homeVob/emacs/pgp.el@@/main/161"
  "don't make faces if !window-system"
10-Mar.11:21   jackr     create version "../homeVob/emacs/pgp.el@@/main/160"
  "don't promote explicit requests for signature to encrypt
   protect against (some) redundant decode entries from older
   mh-e's
   "
10-Mar.10:40   jackr     create version "../homeVob/emacs/pgp.el@@/main/159"
  "distinguish pgp-snarf-warnings, pgp-snarf-msgs
   add pgp-check-and-mark-encryptable to write menu"
10-Mar.10:19   jackr     create version "../homeVob/emacs/pgp.el@@/main/158"
  "add some 2.9 messages forms
   include identity of key by which decrypted in markers"
08-Mar.14:37   jackr     create version "../homeVob/emacs/pgp.el@@/main/157"
  "have timer function always return t
   maybe this is the "timer exited abnormally" thing?
   nah, probly not - seems to be the program timer"
08-Mar.13:30   jackr     create version "../homeVob/emacs/pgp.el@@/main/156"
  "fix timer stuff
   could still be better - working too hard over the arithmetic,
   see mailcrypt for example"
08-Mar.13:02   jackr     create version "../homeVob/emacs/pgp.el@@/main/155"
  "make pgp-check-and-mark not obscure the message buffer so often
   only prompt for pw's if one's actually going to be needed
   dump pgp-decrypt-message and pgp-verify-message (fset to
   pgp-decode for BC)"
08-Mar.10:53   jackr     create version "../homeVob/emacs/pgp.el@@/main/154"
  "add You do not have the secret key needed to decrypt this
   file. to disasters"
03-Mar.20:04   jackr     create version "../homeVob/emacs/pgp.el@@/main/153"
03-Mar.20:01   jackr     create version "../homeVob/emacs/pgp.el@@/main/152"
  "add report-pgp-el-bug"
03-Mar.19:41   jackr     create version "../homeVob/emacs/pgp.el@@/main/151"
  "push bbdb dependencies out into pgp-bbdb.el"
03-Mar.18:57   jackr     create version "../homeVob/emacs/pgp.el@@/main/150"
03-Mar.14:11   jackr     create version "../homeVob/emacs/pgp.el@@/main/149"
  "require timer"
03-Mar.12:26   jackr     create version "../homeVob/emacs/pgp.el@@/main/148"
  "fix some typos in the menu work"
03-Mar.00:26   jackr     create version "../homeVob/emacs/pgp.el@@/main/147"
  "menu item for inhibit send action"
03-Mar.00:10   jackr     create version "../homeVob/emacs/pgp.el@@/main/146"
  "menus, I hope
   radically simplified loading"
02-Mar.19:17   jackr     create version "../homeVob/emacs/pgp.el@@/main/145"
  "fix katy problems:
   pgp sometimes whines "no recipient specified"
   no way to force nonencryption for folks with known keys"
02-Mar.19:03   jackr     create version "../homeVob/emacs/pgp.el@@/main/144"
  "workaround for non-jwz bc still not working, but other changes useful"
28-Feb.22:38   jackr     create version "../homeVob/emacs/pgp.el@@/main/143"
  "what kind of idiot am I?
   pgp-decode calling pgp-check-and-mark-encryptable before
   switching to the buffer!
   Also some streamlinigs for installation:
   pgp-tmp-dir to $PGPPATH/tmp
   pgp-bbdb-notes, if bbdb known, is 'pgpkey
   pgp-show-decrypted-bounaries t
   "
28-Feb.22:00   jackr     create version "../homeVob/emacs/pgp.el@@/main/142"
  "make sure it works with v.18"
27-Feb.19:16   jackr     create version "../homeVob/emacs/pgp.el@@/main/141"
  "gray's new email, my new ftp site"
24-Feb.09:04   jackr     create version "../homeVob/emacs/pgp.el@@/main/140"
  "patch from ange"
23-Feb.17:48   jackr     create version "../homeVob/emacs/pgp.el@@/main/139"
  "fix pgp-key-add (was creating new keyring, named inbox/123.pgp,
   to hold the keys read from inbox/123)"
23-Feb.13:59   jackr     create version "../homeVob/emacs/pgp.el@@/main/138"
  "don't kill v.18 with face stuff
   fix pgp/ for pgp versions
   fix pgp/ not to choke if there already is one"
21-Feb.18:52   jackr     create version "../homeVob/emacs/pgp.el@@/main/137"
  "define my own faces"
21-Feb.16:17   jackr     create version "../homeVob/emacs/pgp.el@@/main/136"
  "swap pgp-message-face and pgp-warn-face, the better to match
   default colors"
21-Feb.15:50   jackr     create version "../homeVob/emacs/pgp.el@@/main/135"
  "add  dox for new stuff
   urk - allow anyone to use the $SHELL they like"
21-Feb.15:14   jackr     create version "../homeVob/emacs/pgp.el@@/main/134"
  "add  pgp-remove-alist-name
   "
21-Feb.14:06   jackr     create version "../homeVob/emacs/pgp.el@@/main/133"
  "remove more-mode (wonder why it's there?)
   catch "can't open tty" as bad passphrase (wonder when it was lost?)"
10-Feb.16:47   jackr     create version "../homeVob/emacs/pgp.el@@/main/132"
  "add some pgp-warns
   separate stdout/stderr, blow off parsing the message-to-text
   boundary
   "
06-Feb.20:19   jackr     create version "../homeVob/emacs/pgp.el@@/main/131"
  "allow user to send whined messages"
06-Feb.11:57   jackr     create version "../homeVob/emacs/pgp.el@@/main/130"
  "all warnings might have leading bells now"
06-Feb.11:49   jackr     create version "../homeVob/emacs/pgp.el@@/main/129"
  "add some +batchmode's to accomodate newer PGP's quirks (sig-checking)"
02-Feb.14:51   jackr     create version "../homeVob/emacs/pgp.el@@/main/128"
  "broke autoencrypt"
01-Feb.19:18   jackr     create version "../homeVob/emacs/pgp.el@@/main/127"
  "allow warning to snarf all available text, if colin jerks me
   around over message format
   yet another possible ending message"
01-Feb.19:14   jackr     create version "../homeVob/emacs/pgp.el@@/main/126"
  "allow warning to snarf all available text, if colin jerks me
   around over message format"
01-Feb.19:13   jackr     create version "../homeVob/emacs/pgp.el@@/main/125"
  "allow warning to snarf all available text, if colin jerks me
   around over message format"
01-Feb.13:23   jackr     create version "../homeVob/emacs/pgp.el@@/main/124"
  "don't auto-double-encrypt"
01-Feb.10:54   jackr     create version "../homeVob/emacs/pgp.el@@/main/123"
  "pgp-check-and-mark-encryptable: escalate
   pgp-auto-encrypt-on-send from signing actions to encryption
   where appropriate
   "
30-Jan.19:30   jackr     create version "../homeVob/emacs/pgp.el@@/main/122"
  "make all shell-command-on-region pgp-bimary include -f (makes
   2.9 happy)"
30-Jan.11:18   jackr     create version "../homeVob/emacs/pgp.el@@/main/121"
  "add some new catastropic errors"
30-Jan.09:02   jackr     create version "../homeVob/emacs/pgp.el@@/main/120"
  "some botch in the parentheses"
27-Jan.12:55   jackr     create version "../homeVob/emacs/pgp.el@@/main/119"
  "make signing obey pgp-just-do-it"
27-Jan.12:44   jackr     create version "../homeVob/emacs/pgp.el@@/main/118"
  "have pgp-mail-send-hook check encryptability"
27-Jan.12:24   jackr     create version "../homeVob/emacs/pgp.el@@/main/117"
  "failed encryptions deserve revert even under pgp-just-do-it
   "
27-Jan.12:12   jackr     create version "../homeVob/emacs/pgp.el@@/main/116"
  "pgp-just-do-it"
24-Jan.19:45   jackr     create version "../homeVob/emacs/pgp.el@@/main/115"
  "more work on auto-multi-encrypt
   works now.  not sure it's the right interface, quite sure I
   don't like the performance (issue is the many bbdb-searches).
   try this and meditate"
24-Jan.18:13   jackr     create version "../homeVob/emacs/pgp.el@@/main/114"
  "something broken in defaulting uid for insert key"
20-Jan.13:20   jackr     create version "../homeVob/emacs/pgp.el@@/main/113"
  "2.6.2 sprouts ^G's where I was expecting ^
   broke "bad passphrase" checking"
19-Jan.17:47   jackr     create version "../homeVob/emacs/pgp.el@@/main/112"
  "if not forcing uid-prompt, yet haven't one handy, set to pgp-my-user-id"
19-Jan.16:10   jackr     create version "../homeVob/emacs/pgp.el@@/main/111"
  "user option pgp-always-confirm-uid"
19-Jan.15:26   jackr     create version "../homeVob/emacs/pgp.el@@/main/110"
  "pgp-decode* needn't ask "as who?""
12-Jan.08:28   jackr     create version "../homeVob/emacs/pgp.el@@/main/109"
  "pgp-decode-region: insert additional newline before "end of decoded message" marker"
11-Jan.11:00   jackr     create version "../homeVob/emacs/pgp.el@@/main/108"
  "non-signed encryption doesn't need passphrase"
09-Jan.13:43   jackr     create version "../homeVob/emacs/pgp.el@@/main/107"
  "mark transparent actions"
06-Jan.16:42   jackr     create version "../homeVob/emacs/pgp.el@@/main/106"
  "fix reprompting for pw"
06-Jan.15:25   jackr     create version "../homeVob/emacs/pgp.el@@/main/105"
  "dump pgp-decode's setup (done in p-d-r)"
06-Jan.14:42   jackr     create version "../homeVob/emacs/pgp.el@@/main/104"
  "yet another message ender:
   WARNING: Can't find the right public key-- can't check signature integrity."
06-Jan.14:29   jackr     create version "../homeVob/emacs/pgp.el@@/main/103"
  "Fix:
   
   While compiling pgp-decode-region in file /disk6/people/jackr/homeVob/emacs/pgp.el:
     ** reference to free variable pass
     ** assignment to free variable pass
   "
06-Jan.14:27   jackr     create version "../homeVob/emacs/pgp.el@@/main/102"
  "another pgp-end-of-message: malformed signature
   put setenv, get-uid, get-passwd into a macro (to facilitate
   debugging everything else)"
04-Jan.08:31   jackr     create version "../homeVob/emacs/pgp.el@@/main/101"
  "move password prompting and such setup inside while loop (so
   it's only done if needed)"
03-Jan.20:14   jackr     create version "../homeVob/emacs/pgp.el@@/main/100"
  "different rotation of the sit-for 0s"
03-Jan.20:13   jackr     create version "../homeVob/emacs/pgp.el@@/main/99"
  "add a sit-for 0 before pgp-decoding, so initial state is seen"
03-Jan.20:10   jackr     create version "../homeVob/emacs/pgp.el@@/main/98"
  "pgp-decode, usable interactively or as mh-show-hook"
03-Jan.17:25   jackr     create version "../homeVob/emacs/pgp.el@@/main/97"
03-Jan.17:21   jackr     create version "../homeVob/emacs/pgp.el@@/main/96"
03-Jan.17:12   jackr     create version "../homeVob/emacs/pgp.el@@/main/95"
  "mh-show-hook for decrypt/verify"
03-Jan.15:56   jackr     create version "../homeVob/emacs/pgp.el@@/main/94"
  "remove unused pgp-overlay aliases"
16-Dec.17:39   jackr     create version "../homeVob/emacs/pgp.el@@/main/93"
  "provide "check all keys" and pgp-check-and-mark-encryptable"
16-Dec.15:24   jackr     create version "../homeVob/emacs/pgp.el@@/main/92"
  "pgp-buffer-was-encrypted was a dumb idea: just set
   pgp-auto-encrypt-on-send"
16-Dec.09:58   jackr     create version "../homeVob/emacs/pgp.el@@/main/91"
  "When clearsigning, untabify"
16-Dec.09:21   jackr     create version "../homeVob/emacs/pgp.el@@/main/90"
  "allow "PGP SIGNED MESSAGE" to inhibit reencryption, too"
14-Dec.10:25   jackr     create version "../homeVob/emacs/pgp.el@@/main/89"
  "have pgp-buffer-was-encrypted trigger a prompt"
13-Dec.19:24   jackr     create version "../homeVob/emacs/pgp.el@@/main/88"
  "make replies to encrypted messages really want
   to be encrypted, too"
13-Dec.19:14   jackr     create version "../homeVob/emacs/pgp.el@@/main/87"
  "fix note about getting key IDs into bbdb"
13-Dec.18:28   jackr     create version "../homeVob/emacs/pgp.el@@/main/86"
  "oops!  Buggie-pooh for PGP output buffers (e.g., for
   pgp/bbdb-set-key-id) without any of the expected terminating strings"
08-Dec.20:19   jackr     create version "../homeVob/emacs/pgp.el@@/main/85"
  "add "Pass phrase is good" to good messages"
08-Dec.20:03   jackr     create version "../homeVob/emacs/pgp.el@@/main/84"
  "can't use identity for arbitrary arg set"
08-Dec.19:54   jackr     create version "../homeVob/emacs/pgp.el@@/main/83"
  "try overlaying PGP output"
21-Nov.20:37   jackr     create version "../homeVob/emacs/pgp.el@@/main/82"
  "prune any earlier X-Pgp-{Version|Signed}"
21-Nov.19:32   jackr     create version "../homeVob/emacs/pgp.el@@/main/81"
  "... and handle "^From " lines"
21-Nov.13:45   jackr     create version "../homeVob/emacs/pgp.el@@/main/80"
  "handle dash-lines in header-signed text"
21-Nov.13:37   jackr     create version "../homeVob/emacs/pgp.el@@/main/79"
  "oh yeah, and allow for it when snarfing sig into header,too"
21-Nov.11:08   jackr     create version "../homeVob/emacs/pgp.el@@/main/78"
  "correction for Busdieker-style header siggies (needs newline
   before siggie-proper)
   "
17-Nov.13:42   jackr     create version "../homeVob/emacs/pgp.el@@/main/77"
  "pgp-check-for-errors was missing "Enter pass phrase" (which is
   what you sometimes get from a bad passphrase)"
04-Nov.17:14   jackr     create version "../homeVob/emacs/pgp.el@@/main/76"
  "A tweak from Gray:
   
   How about the following "last-minute" patch.  I've been using
   sign lately a lot and it occurred to me that we should prepent the
   message there too."
24-Oct.16:17   jackr     create version "../homeVob/emacs/pgp.el@@/main/75"
  "better condition-case protection for timer"
24-Oct.16:13   jackr     create version "../homeVob/emacs/pgp.el@@/main/74"
  "no, you idiot: those 2.2's really mean pgp versions!"
24-Oct.16:12   jackr     create version "../homeVob/emacs/pgp.el@@/main/73"
  "show version 2.3 everywhere"
24-Oct.16:11   jackr     create version "../homeVob/emacs/pgp.el@@/main/72"
  "update $date"
24-Oct.16:10   jackr     create version "../homeVob/emacs/pgp.el@@/main/71"
  "protect against absence of timer.el"
17-Aug.18:50   jackr     create version "../homeVob/emacs/pgp.el@@/main/70"
  "fix bug - why didn't it used to fail?  who knows - about finding
   beginning of text when header signing"
17-Aug.18:43   jackr     create version "../homeVob/emacs/pgp.el@@/main/69"
  "don't tail-pad body with superfluous newline when header signing"
25-Jul-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/68"
  "allow for "but you previously approved""
06-Jul-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/67"
  "paren count on a fillarray"
29-Jun-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/66"
  "oops - break up the phrase "enter pass phrase", so I can sign this!"
21-Jun-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/65"
  "recache same copy, neither burned nor lost"
21-Jun-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/64"
  "burn pw's when uncaching them
   lots of other flame-bait around, but at least this one works"
21-Jun-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/63"
  "use Jamie's passwd.el to read passwords somewhat more securely
   still desperately need to burn the old copies, but I can't get it to work"
16-Jun-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/62"
  "fix uint32 arithmetic in pgp-passphrase-timer"
16-Jun-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/61"
  "change order of auto-encrypt selections"
15-Jun-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/60"
  "play with cached passphrases"
13-Jun-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/59"
  "shouldn't automatically save decrypted text to disk"
26-May-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/58"
  "so, how do you know when it's tested enough?
   Header siggies botch when replying (continued item, e.g. in-reply-to,
   just before x-pgp-fingerprint)
   also, pgp-ephemeral-display wasn't clearing its prompt"
23-May-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/57"
  "don't know where that "Done" is coming from, but it's eating my "Working.""
23-May-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/56"
  "fix setcdr for pgp-auto0-encrypt-on-send"
23-May-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/55"
  "add pgp-header-sign routine, and a member of the auto-processing enum"
23-May-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/54"
  "split header-siggie verification into separate routine, preparatory to
   calling from folder"
20-May-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/53" (RCS-1.35)
  "support X-Pgp-Signed header in general files"
29-Apr-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/52"
  "allow aborted signature to also abort send"
28-Apr-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/51"
  "pgp-always-clear-sign"
28-Apr-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/50"
  "make a command to mark a message for outgoing encryption or signing
   while composing
   "
18-Mar-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/49"
  "make pgp-eph delete-other-windows"
18-Mar-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/48"
  "adapt to mh-e 4.0 (mh-pipe-msg parameter reversal)"
15-Feb-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/47"
  "speed up pgp-key-lookup field-snarfing"
15-Feb-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/46"
  "prune "@" from friendly"
15-Feb-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/45"
  "have pgp-key-lookup return both the keyid and the user name"
27-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/44"
  "deal with embedded newlines in addresses"
11-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/43"
  "idiot: handle in-bbdb-but-no-key"
11-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/42"
  "sigh: have pgp-key-lookup handle address forms like
   
   Gray Watson <gray.watson@antaire.com>"
11-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/41"
  "protect mail-send-hook from forwarding to wrong person
   by making pgp-key-lookup actually look it up in bbdb, rather than
   merely using the current record"
10-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/40"
  "protect bbdb-annotate-message-sender from trailing @"
10-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/39"
  "pgp/bbdb-set-key-id
   	Ask pgp for key id for current message's sender (with ARG,
   recipient), add it to BBDB."
10-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/38"
  "make pgp-ephemeral-display-buffer return status from quit/abort"
10-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/37"
  "make pgp-key-lookup able to search other fields"
10-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/36"
  "ingraft bbdb/mh/pgp protections"
10-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/35" (1.35)
  "Gray's 1.35"
10-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/34" (1.32)
  "Gray's 1.32"
07-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/33" (1.31)
  "Gray's 1.31"
07-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/32"
06-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/31"
  "implement pgp-mail-send-and-exit as a hook"
06-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/30" (1.28)
  "Gray's 1.28"
05-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/29"
  "make the mh versions of the merged code work"
05-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/28" (1.27)
  "Gray's merged version"
04-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/27"
  "copy-sequence process-environment"
04-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/26"
  "make pgp-add-key add all keys"
04-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/25" (1.24)
  "Merge in Gray's 1.24"
04-Jan-1994    jackr     create version "../homeVob/emacs/pgp.el@@/main/24"
  "bug in pgp-sign-message (if "Just a moment" is optional, gotta allow
   it to be so)"
27-Dec-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/23"
  "pgp-insert-key: allow PGPPATH to be a colon path"
22-Nov-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/22"
  "be more permissive about markers"
27-Sep-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/21"
  "some wonky bug in pgp causes the text used for signed messages
   sometimes not to include the word "SIGNED ""
27-Sep-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/20"
  "protect process-environment via copy-sequence"
24-Sep-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/19"
  "hand the necessary splitting of marker lines in another way, that even
   protects original code"
24-Sep-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/18"
  "protect all users of process-environment from new getenv"
24-Sep-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/17"
  "add a "provide""
24-Sep-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/16"
  "make ephemeral-display quit on ^G
   only it didn't work"
24-Sep-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/15"
  "split mh-e support into pgp-mh.el"
24-Sep-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/14"
  "typo in pgp-mail-send-and-exit: called (mail-send-and-exit (arg))"
24-Sep-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/13"
  "some difficulty with ^C E while composing (pgp-encrypt-message)"
10-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/12" (18.59)
  "typos in docstring of pgp-decrypt-message
   recommend binding to capital letters "
10-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/11"
  "fix a comment for the difference between pgp-mh-decrypt and pgp-decrypt"
10-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/10"
  "add pgp-mh-decrypt
   fix some typos (pgp-my vice pgp-mh)"
10-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/9"
  "fix single-page pgp-ephemeral-display-buffer to exit on first keypress"
09-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/8"
  "add ehelp-style typeout windows via pgp-ephemeral-display-buffer"
09-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/7"
  "Add  pgp-mh-verify-sig and pgp-mh-key-add"
09-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/6"
  "Oops! gotta include "*" in docs of user options"
09-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/5"
  "cut all pgp message literals, so they don't confuse pgp.el when it's
   mailed around
   "
09-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/4" (1.22)
  "first official version with pgp-header-marker"
08-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/3"
  "Aha.  I think I found the problem.  All pgp commands need -at added to
   their options.  I have armor and texmodes set on in my config file but
   this certainly is not standard.
   "
07-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/2"
06-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/1"
06-Aug-1993    jackr     create version "../homeVob/emacs/pgp.el@@/main/0"
06-Aug-1993    jackr     create branch "../homeVob/emacs/pgp.el@@/main"
06-Aug-1993    jackr     create file element "../homeVob/emacs/pgp.el@@"
