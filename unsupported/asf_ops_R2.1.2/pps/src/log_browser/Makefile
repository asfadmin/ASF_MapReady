#
# @(#)Makefile	1.7  01/16/97
# Ops Log Handler/Browser Makefile 
#
# Modified by M. Tankenson (June 1994)
#

SUB_PATH =		src/log_browser
BINDIR =		../../bin

sparcCFLAGS =	-g -DHAVE_STRING_H -DREGEX_MALLOC
sparcCPPFLAGS =	-I. -I/usr/openwin/include -I/usr/dt/include

sgiCFLAGS =	-g -DADD_INFO -DSGI -DREGEX_MALLOC
sgiCPPFLAGS =	-D_NO_PROTO -I.

REGEXMAKEARGS = CC='$(CC)'
CFLAGS= $($(MACH)CFLAGS)
CPPFLAGS = $($(MACH)CPPFLAGS)
LDFLAGS =	-L/usr/dt/lib -L/usr/ccs/lib
LDLIBS =	-lgen
XLIBS = 	-lXm -lXt -lX11 -lXext

LINTFLAGS = -axu

# ASF Log program stuff
#
EXE = log_browser
LPR_SH = log_lpr.sh

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
	log_status.h \
	regex.h


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
	log_status.c\
	regex.c

OBJS = $(SRCS:.c=.o)

APPDEF = Log_browser

default:	$(INCLUDES) $(SRCS) $(EXE)
	$(MAKE) install_lprsh

source:
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS

all:
	$(MAKE) clean
	$(MAKE) default

#
# syslog test program
#
msgtest:	msgtest.c
	$(CC) $(CPPFLAGS) -o msgtest msgtest.c

$(EXE):	$(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS) $(XLIBS) $(LDLIBS)
	-/bin/mkdir $(BINDIR)/$(MACH)
	/bin/cp $(EXE) $(BINDIR)/$(MACH)

install_lprsh: $(LPR_SH)
	/bin/cp $(LPR_SH) $(BINDIR); chmod +x $(BINDIR)/$(LPR_SH)

man:
	@echo "$(MANPAGE) created manually"
	install -m 0644 $(MANPAGE) $(MANDIR)
	@echo ""


clean:
	-/bin/rm -f core
	-/bin/rm -f *.o
	-/bin/rm -f $(EXE)
	-/bin/rm -f msgtest

clear:	clean
	-sccs clean

lint:
	$(LINT) $(LINTFLAGS) $(CPPFLAGS) $(SRCS)

depend:

%.c: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.c
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.c

%.h: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.h
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.h

%.sh: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.sh
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.sh
