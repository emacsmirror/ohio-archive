# Makefile for meta-server
# Written by William M. Perry for version 2.2
# wmperry@indiana.edu, 2/3/93

.IGNORE:

BACKUPDIR = /u/wmperry/lisp/development/netrek/safe
BGFILES = background.el
EMACSTEMPLATE = add-to-dot-emacs
METASERVER = meta-server.el
DISTFILES = Makefile Changelog COPYING meta-server.doc todo \
	    add-to-dot-emacs $(METASERVER) $(BGFILES)

# Change this to be the emacs on your system.
EMACS = /usr/local/bin/emacs -batch -f batch-byte-compile

# Change this to be where you want the lisp files installed.
# Must escape all / for use in sed...
LISPDIR = \/u\/wmperry\/lisp
TOPDIR = /u/wmperry

    all: 
	 @echo "Compiling $(METASERVER) $(BGFILES)"
	 @$(EMACS) $(METASERVER) $(BGFILES) > /dev/null

  emacs: 
	 @echo "Creating .emacs file
	 @touch $(TOPDIR)/.emacs
	 @cat $(EMACSTEMPLATE) | sed 's/METASERVERPROG/$(METASERVER)/g' | \
	 sed 's/THELISPDIR/$(LISPDIR)/g' | \
	 sed 's/BACKGROUNDPROG/$(BGFILES)/g' >> $(TOPDIR)/.emacs

   dist:
	 @echo Making uuencoded distribution file.
	 @rm -f dist.uu
	 @tar -czf - $(DISTFILES) | uuencode metaserver.tar.Z | \
          sed 's/begin 666/begin 644/g' > dist.uu	

anonftp:	;
	 @echo Making the tar file
	 @tar -czf metaserver.tar.Z $(DISTFILES)
	 @echo Mailing wagle@clementi about the tar file
	 @/usr/ucb/mail -s "Update to metaserver for mooses ftp" \
	 wagle@clementi < /u/wmperry/lisp/development/netrek/update.notice
	
print: 
	 @echo Printing the files
	 @enscript -2rGh $(METASERVER) $(BGFILES)

install: all
	 @echo Copying $(METASERVER)[c] $(BGFILES)[c] to $(LISPDIR)
	 @cp $(METASERVER) $(METASERVER:.el=.elc) $(BGFILES) \
	     $(BGFILES:.el=.elc) $(LISPDIR)

  clean: 
	 @echo Removing intermediate files and backups.
	 @rm -f *.elc *~ \#* *.tar.Z *.uu *.shar

 backup: 
	 @echo Making backup in $(BACKUPDIR)
	 @cp $(DISTFILES) $(BACKUPDIR)

