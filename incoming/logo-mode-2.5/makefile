# Edit next few lines to adjust variables to actual situation on your system.
# The reasonable defaults are shown.

# Change this to point to the base installation directory
prefix	      = /usr/local
# Change this to point to your info directory
INFODIR	      = $(prefix)/info
# Change this to point to your logo library location directory
LIBLOC        = $(prefix)/lib/logo
# Change this to point to your logo binary directory
BINDIR	      = $(prefix)/bin
#
# NEXT TWO MUST BE ALTERED!
# Change my-dir to match your login directory
USERDIR       = /home/my-dir
# Change my-login-name to match your login
USERACCNT     = my-login-name  
#
# There should be no user changes beyond this point


BUILDIR	      = `pwd`
EMACSDIR      = $(LIBLOC)/emacs
ROOTDIR	      = $(HOME)
#USERACCNT     = $(LOGNAME)
#USERDIR	      = /home/$(USERACCNT)
LOADPATH      = $(subst X,$(EMACSDIR),(setq load-path (cons "\"X\"" load-path)))
LOGOBIN	      = $(subst X,$(BINDIR),(setq logo-binary-name \"X/logo\"))
INFOPATH      = $(subst X,$(INFODIR),(setq logo-info-file \"X/ucblogo.info\"))
HELPPATH      = $(subst X,$(LIBLOC),(setq logo-help-path \"X/helpfiles/\"))
TUTORPATH     = $(subst X,$(EMACSDIR),(setq logo-tutorial-path \"X/\"))

all: logo-mode-compile path-subst
	for d in $(INFODIR) $(EMACSDIR); do [ -d $$d ] || mkdir -p $$d || exit 1; done
	cp info/* $(INFODIR)/.
	cp logo.* $(EMACSDIR)/.
	cp tutor* $(EMACSDIR)/.
	cp *.logo $(EMACSDIR)/.
	cp dot.emacs $(EMACSDIR)/.
	cp readme $(EMACSDIR)/.
	-emacs -batch $(ROOTDIR)/.emacs -l $(BUILDIR)/edfunc.el -f edfunc -insert $(BUILDIR)/dot.emacs -f save-buffer -kill
	-cp dot.logo $(ROOTDIR)/.logo

ifneq ($(ROOTDIR),$(USERDIR))
	-emacs -batch $(USERDIR)/.emacs -l $(BUILDIR)/edfunc.el -f edfunc -insert $(BUILDIR)/dot.emacs -f save-buffer -kill
	-cp dot.logo $(USERDIR)/.logo
	-chown -f $(USERACCNT) $(USERDIR)/.emacs $(USERDIR)/.logo
endif

	exec emacs check.lg &

logo-mode-compile:
	emacs -batch -f batch-byte-compile logo.el

path-subst:
	echo "$(LOADPATH)" > e.path
	echo "$(LOGOBIN)" > e.bin
	echo "$(INFOPATH)" > e.info
	echo "$(HELPPATH)" > e.help
	echo "$(TUTORPATH)" > e.tutor
	echo ";;" > e.safe
	cat dot.emacs1 e.safe e.path e.bin e.info e.help e.tutor dot.emacs2 > dot.emacs
