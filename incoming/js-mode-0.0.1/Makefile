PKG-NAME=js-mode

MAJOR-VER=0
MINOR-VER=0
PATCH-LVL=1

FINAL-VER=$(MAJOR-VER).$(MINOR-VER).$(PATCH-LVL)

DIST-DIR=$(PKG-NAME)-$(FINAL-VER)
DIST-TAR=$(DIST-DIR).tar

SITE-LISP=/usr/share/emacs/20.3/site-lisp

TAR-ARGS=2>&1 > /dev/null

all: js-mode

clean: dist-clean
	@rm -f *.elc
	@rm -f *~

dist-clean:
	@rm -rf $(DIST-DIR)
	@rm -f $(DIST-TAR)

realclean: clean
	@rm -f $(DIST-TAR)*

dist:
	@printf "Making distribution directory..."
	@mkdir $(DIST-DIR)
	@printf "done"
	@echo
	@printf "Copying files..."
	@cp *.el $(DIST-DIR)
	@cp Makefile $(DIST-DIR)
	@cp install.sh $(DIST-DIR)
	@cp COPYING $(DIST-DIR)
	@cp CREDITS $(DIST-DIR)
	@cp INSTALL $(DIST-DIR)
	@cp README $(DIST-DIR)
	@cp TODO $(DIST-DIR)
	@printf "done"
	@echo
	@printf "Setting permissions..."
	@chown -R nobody:nobody $(DIST-DIR)
	@printf "done"
	@echo
	@printf "Creating tarball..."
	@tar cvf $(DIST-TAR) $(DIST-DIR) $(TAR-ARGS)
	@printf "done"
	@echo
	@chown nobody:nobody $(DIST-TAR)

bzdist: dist
	@rm -f $(DIST-TAR).bz2
	@bzip2 $(DIST-TAR)
	@echo; echo "Congratulations! You may now distribute $(DIST-TAR).bz2"; echo

zdist: dist
	@rm -f $(DIST-TAR).gz
	@gzip $(DIST-TAR)
	@echo; echo "Congratulations! You may now distribute $(DIST-TAR).gz"; echo

install: all
	@/bin/sh -x ./install.sh $(SITE-LISP)

uninstall: all
	@/bin/sh -x ./install.sh -u $(SITE-LISP)
	$(MAKE) clean

js-mode: js.el
	@emacs --batch --no-init-file --eval='(byte-compile-file "js.el")'
