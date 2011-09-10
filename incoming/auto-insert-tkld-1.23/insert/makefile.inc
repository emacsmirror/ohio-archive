# Makefile for %[Package name: %]	-*-Makefile-*-
# Copyright (C) %y %U %a
# $Header$
# $Log$

CC = gcc
CFLAGS = -g -O
MAKE = make
COMPRESS = gzip

OBJS =
SRCS = $(OBJS:.o=.c)

RUBBISH = $(OBJS) *.log *.aux *.tp *.vr *.cp *.fn *.ky *.pg

DISTFILES = $(SRCS) Makefile

all: %1
	@echo All done.

clean:
	-rm -f $(RUBBISH)

dist:
	tar cf %1.tar $DISTFILES
	$(COMPRESS) %1.tar
