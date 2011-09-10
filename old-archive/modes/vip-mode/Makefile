# I use gnumake, maybe normal make will work

TeX = tex
TEXINDEX = texindex
MAKEINFO = makeinfo
EMACS = gnuemacs
BATCHCOMMAND = /tmp/bc.el

%.dvi:%.texinfo
	@echo "Texing $<";
	$(TeX) $<
	@echo "Indexing $*.??"
	$(TEXINDEX) $*.??
	@echo "Texing $*";
	$(TeX) $<

%.info:%.texinfo
	@echo "Makeinfo $<"
	$(MAKEINFO) $< 

%.dvi:%.tex
	@echo "Texing $<"
	$(TeX) $<

%.elc:%.el
	@echo "Byte compiling $<"
	@if test ! -f $(BATCHCOMMAND); then \
	 echo '(byte-compile-file (buffer-file-name (current-buffer)))'  > $(BATCHCOMMAND); \
	fi
	$(EMACS) -batch $< -l $(BATCHCOMMAND)

all: vip.dvi vip.info vipcard.dvi vip.elc

vip.dvi: vip.texinfo vip-cmd.texinfo

vip.info: vip.texinfo vip-cmd.texinfo
