#
# @(#)Makefile	1.3 92/05/12 14:12:32 (c)NOCC
# Ops Log Handler/Browser Makefile 
#
# Modified by M. Tankenson (June 1994)
# Modified by D. Fineman (Jan 1996)
#
# @(#)Makefile	1.3 95/10/20 11:37:36 (ASF)
#

#INSTALLDIR = $(ASF)/bin
INSTALLBINDIR = ../../bin/$(MACH)
INSTALLSHDIR = ../../etc/sh_scripts
MANEXT = 1
#MANDIR = $(ASF)/man/man$(MANEXT)
MANDIR = ../../etc/man/man$(MANEXT)
#XAPPLOADDIR = $(LOCAL)/app-defaults
XAPPLOADDIR = ../../etc/app-defaults

# This Makefile assumes that the user compiling/installing the code
# will not necessarily be the user running it.

HOME_DIR = ../../etc/install

# SGI
CC = cc
ARCH = SGI
REGEXMAKEARGS = CC='$(CC)'
CFLAGS =		-g -DADD_INFO -D$(ARCH)
CPPFLAGS =	-D_NO_PROTO -I.
LDFLAGS =
LDLIBS =	
#XLIBS = 	-lXm -lXmu -lXt -lX11 
XLIBS = 	-lXm -lXt -lX11 

LINTFLAGS = -axu

#
# ASF Log program stuff
#
EXE = log_browser
SH_FILE = log_lpr.sh
MANPAGE = $(EXE).man
PRINTER_FILE = .printer
APPDEF = Log_browser

#
# GNU regular expression library
#
REGEX = regex.o

INCLUDES= \
	log_alert.h \
	log_browse.h \
	log_file.h \
	log_filter.h \
	log_icon.h \
	log_list.h \
	log_main.h \
	log_menu.h \
	log_print.h \
	log_resrc.h \
	log_sound.h \
	log_status.h

OBJS = $(SRCS:.c=.o)

SRCS = \
	log_alert.c \
	log_browse.c \
	log_file.c \
	log_filter.c \
	log_list.c \
	log_main.c \
	log_menu.c \
	log_print.c \
	log_sound.c \
	log_status.c

app:	all
all:	$(INCLUDES) $(SRCS) $(REGEX) $(INSTALLBINDIR)/$(EXE)

# Installation of the log_lpr.sh script added by Dan Fineman

$(INSTALLBINDIR)/$(EXE):	$(EXE) $(SH_FILE) $(APPDEF) $(PRINTER_FILE)
	install -m 0775 -F $(INSTALLBINDIR) $(EXE)
	install -m 0775 -F $(INSTALLSHDIR) $(SH_FILE)
	install -m 0664 -src $(APPDEF) -F $(XAPPLOADDIR) $(APPDEF).ad
	install -m 0664 -F $(HOME_DIR) $(PRINTER_FILE)
	@echo ""

$(EXE):	$(OBJS)
	$(CC) -o $@ $(OBJS) $(REGEX) $(LDFLAGS) $(XLIBS) $(LDLIBS)

regex.o:
	(cd regex-0.12; $(MAKE) $(REGEXMAKEARGS) justregex )

install.man:
	@echo Installing man page for $(EXE) ....
	install -m 0664 -F $(MANDIR) -src $(MANPAGE) $(EXE).$(MANEXT)
	@echo ""

doc:

clean:
	-/bin/rm -f core
	-/bin/rm -f $(OBJS) $(REGEX)
	-/bin/rm -f $(EXE)

clear:	clean
	-sccs clean

lint:
	$(LINT) $(LINTFLAGS) $(CPPFLAGS) $(SRCS)

depend:

