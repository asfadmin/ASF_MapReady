#
# @(#)Makefile	1.42 96/10/31 12:21:53
#
# This is the Makefile for building the common library "libsps.a."
# The source was originally written for an SGI Challenge XL running
# IRIX 5.2 and has been ported to OSF/1, AIX 4.1.x, and Solaris 2.x.
#
# Note that this Makefile does a recursive make; so the build will
# fail if you rename the Makefile without making the corresponding
# change for each platform (i.e., "... -f makefile ....").
#
# Usage:
#     make all        - Build library
#     make glib       - Same as "make all"
#     make install    - Install library, global header files, and
#                       message template file locally
#     make glib_inst  - Same as "make install"
#     make clean      - Remove binaries from current working directory
#
# Environment:
#     ASF should be set to /ASF/XXX, where XXX is the top-level directory
#         corresponding to this delivery (e.g., R1.4.2).
#     LOCAL should be set to /LOCAL/XXX, where XXX is the top-level directory
#         corresponding to this delivery (e.g., R1.4.2).
#     MACH should be set to the hardware platform identifier.
#         Accepted values are "alpha," "ibm," "sgi," and "sparc."
#         ("other" and "smart" are preserved for historical reasons but
#         are not officially supported for R1.4.)
#

INSTALLINCDIR=$(ASF)/include/sps
INSTALLLIBDIR=$(ASF)/lib
INSTALLTMPDIR=../../lib/$(MACH)/global
INCLUDEDIR=../../include
TEMPLATEDIR=$(LOCAL)/sps/templates
YACC=../toolkit/bin/bison -y -d
LEX=../toolkit/bin/flex
YYLEX_H_DIR=../toolkit/include
SOLARIS_PTHREAD_DIR=/opt/dce/share/include/dce

AR=ar -rcvs
CHMOD= /bin/chmod
CP=/bin/cp -p
ECHO=/bin/echo
MKDIR=/bin/mkdir -m 775 -p
RANLIB=@echo
RM=/bin/rm -f

SPSLIB	= libsps.a
MESSAGE_TEMPLATE = msgTemplate.odl

#
# Note that the use of SOLARIS_PTHREAD_DIR assumes that /opt/dce/share/
# include/dce doesn't exist on any of the other supported platforms (SGI,
# DEC Alpha, IBM).
#
CC_INCLUDE = -I$(SOLARIS_PTHREAD_DIR) -I$(INCLUDEDIR) -I$(YYLEX_H_DIR)

#
# Default platform:  SGI/IRIX 5.2
#
all:	glib
glib:	$(MACH)
glib_inst:	install

# On SGI/IRIX 5.2:
# ---------------
#
#	cws - added posix_source flag to cflags to satsify 
#	sigaction/sigset datatypes, and bsd_compat to satisfy
#	struct timeval
#
sgi:
	@$(ECHO) "Making on SGI/IRIX 5.2 ..."
	$(MAKE) 'BASE_SRCS = $(BASE_SRCS) pthread.c' \
		'BASE_OBJS = $(BASE_OBJS) pthread.o' \
		'CFLAGS= -DYYPURE $(CC_INCLUDE) -D__sgi -D_POSIX_SOURCE -D_BSD_COMPAT -ansi' \
		'LDLIBS= $(SPSLIB)' $(INSTALLTMPDIR)/$(SPSLIB)

#
# On IBM/AIX:
# ----------
#
ibm:
	@$(ECHO) "Making on IBM/AIX version 4.1.x ..."
	@$(MAKE) 'CFLAGS= -DYYPURE $(CC_INCLUDE)' \
                 'CC=xlc_r4' \
                 'LDLIBS= $(SPSLIB) -lc_r -lpthreads' $(INSTALLTMPDIR)/$(SPSLIB)

#
# On DEC/Alpha:
# ------------
#
alpha:	dec
dec:
	@$(ECHO) "Making on DEC Alpha ..."
	@$(MAKE) 'CFLAGS= -DYYPURE $(CC_INCLUDE) -D_ALPHA' \
		 'LDLIBS= $(SPSLIB) -lc_r -lmach -lpthreads' \
		 $(INSTALLTMPDIR)/$(SPSLIB)

#
# On Solaris 2.x and SunOS 4.1.3:
# ------------------------------
#
sparc:	sun
sun:
	@$(ECHO) "Making on Solaris 2.x (or SunOS 4.1.3) with POSIX Threads ..."
	@$(MAKE) 'CFLAGS= -DYYPURE -D_BSD_COMPAT -D_NO_PROTO -DNOSTDARG $(CC_INCLUDE)' \
		'LDLIBS= $(SPSLIB) -lsocket -lnsl -ldce -lthread -lm' \
		'CC=/usr/ucb/cc' $(INSTALLTMPDIR)/$(SPSLIB)

other:
	@$(ECHO) "Making on Solaris 2.x (or SunOS 4.1.3) without POSIX Threads ..."
	@$(MAKE) 'CFLAGS= -DYYPURE -D_NO_PROTO -DNOSTDARG $(CC_INCLUDE)' \
		'LDLIBS= $(SPSLIB)' $(INSTALLTMPDIR)/$(SPSLIB)

#
# On MODCOMP BSD Unix 4.2: (smart)
# -----------------------
#
smart:
	@$(ECHO) "Making on BSD 4.1.2 ..."
	@$(MAKE) 'CFLAGS= -DYYPURE -D_NO_PROTO -DNOSTDARG $(CC_INCLUDE)' \
		 'LDLIBS= $(SPSLIB) -lPW' $(INSTALLTMPDIR)/$(SPSLIB)

#
# The next few macros enable source files to be pulled out of SCCS
# automatically as necessary.
#

#
# Note that pthread.c is included in the library only on the SGI.
#
BASE_SRCS = \
	asf.c \
	genUtils.c \
	logUtils.c \
	memUtils.c \
	socketXport.c

BASE_OBJS = \
	asf.o \
	genUtils.o \
	logUtils.o \
	memUtils.o \
	socketXport.o

ODL_CLASSES = \
	Obj.c \
	Msg.c \
	Relay.c \
	Object.c \
	Group.c \
	Label.c \
	Array.c \
	Ident.c \
	String.c \
	Symbol.c \
	Set.c \
	Seq.c \
	Seq2.c \
	Range.c \
	Int.c \
	UnsignedInt.c \
	Double.c \
	Time.c \
	Unit.c

ODL_INCS = $(ODL_CLASSES:.c=.h) \
	Parser.h

ODL_SRCS = \
	Parser.y \
	Lexer.l \
	odl_api.c \
	$(ODL_CLASSES)

SRCS	= $(ODL_SRCS) $(BASE_SRCS)
OBJS = \
	odl_api.o \
	Obj.o \
	Msg.o \
	Relay.o \
	Object.o \
	Group.o \
	Label.o \
	Array.o \
	Ident.o \
	String.o \
	Symbol.o \
	Set.o \
	Seq.o \
	Seq2.o \
	Range.o \
	Int.o \
	UnsignedInt.o \
	Double.o \
	Time.o \
	Unit.o \
	$(BASE_OBJS) \
	Parser.o \
	Lexer.o

#
# Do not include version.h in this list; it should not be installed
# under $ASF/include/sps.
#
ALL_INCS	= $(ODL_INCS) \
		asf.h \
		asf_syslog.h \
		asfcommon.h \
		inet.h \
		logUtils.h \
		mem.h \
		memUtils.h \
		odl.h \
		pthread_wrapper.h \
		socketXport.h \
		tree.h

$(INSTALLTMPDIR)/$(SPSLIB):	$(SPSLIB)
	-$(MKDIR) $(INSTALLTMPDIR)
	-$(RM) $(INSTALLTMPDIR)/$(SPSLIB)
	$(CP) $(SPSLIB) $(INSTALLTMPDIR)

$(SPSLIB):	$(OBJS)
	$(AR) $@ $?
	$(RANLIB) $@
	-$(CHMOD) 644 $@

#
# Remove the old library from the installation directory, but don't copy
# in the new one.  That step is done by the Makefile in ../../lib.
#
install:	$(INSTALLTMPDIR)/$(SPSLIB)
	-$(MKDIR) $(INSTALLLIBDIR)
	-$(RM) $(INSTALLLIBDIR)/$(SPSLIB)
	-$(MKDIR) $(INSTALLINCDIR)
	-(cd $(INSTALLINCDIR); $(RM) $(ALL_INCS))
	(cd $(INCLUDEDIR); \
	    $(CP) $(ALL_INCS) $(INSTALLINCDIR))
	-$(MKDIR) $(TEMPLATEDIR)
	-$(RM) $(TEMPLATEDIR)/$(MESSAGE_TEMPLATE)
	$(CP) $(MESSAGE_TEMPLATE) $(TEMPLATEDIR)

# IRIX install syntax isn't compatible with OSF/1's.  Fix later.

#.c.o:
#	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<
.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)

$(SRCS) $(ODL_INCS):
	sccs get $@

clean:
	-$(RM) $(OBJS) pthread.o core $(SPSLIB) \
		Lexer.c Parser.c y.tab.h

#
# Dependencies
#
# Note the presence of "hidden" dependencies; for example, asf.h includes
# other header files.
#
# DO NOT DELETE THIS LINE -- make depend depends on it.
Array.o: $(INCLUDEDIR)/Array.h $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Msg.h
Array.o: $(INCLUDEDIR)/Label.h $(INCLUDEDIR)/Ident.h $(INCLUDEDIR)/String.h
Array.o: $(INCLUDEDIR)/Relay.h
Double.o: $(INCLUDEDIR)/Double.h $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Msg.h
Double.o: $(INCLUDEDIR)/String.h $(INCLUDEDIR)/Unit.h $(INCLUDEDIR)/Relay.h
Group.o: $(INCLUDEDIR)/Group.h $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/Label.h
Group.o: $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Ident.h $(INCLUDEDIR)/String.h
Group.o: $(INCLUDEDIR)/Relay.h
Ident.o: $(INCLUDEDIR)/Ident.h $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/String.h
Ident.o: $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Relay.h
Int.o: $(INCLUDEDIR)/Int.h $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Msg.h
Int.o: $(INCLUDEDIR)/String.h $(INCLUDEDIR)/Unit.h $(INCLUDEDIR)/Relay.h
Label.o: $(INCLUDEDIR)/Label.h $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Msg.h
Label.o: $(INCLUDEDIR)/Ident.h $(INCLUDEDIR)/String.h $(INCLUDEDIR)/Relay.h
Msg.o: $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/Obj.h
Obj.o: $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/Relay.h
Object.o: $(INCLUDEDIR)/Object.h $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/Label.h
Object.o: $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Ident.h $(INCLUDEDIR)/String.h
Object.o: $(INCLUDEDIR)/Relay.h
Range.o: $(INCLUDEDIR)/Range.h $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/Array.h
Range.o: $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/String.h $(INCLUDEDIR)/Relay.h
Relay.o: $(INCLUDEDIR)/Relay.h $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/Obj.h
Seq.o: $(INCLUDEDIR)/Seq.h $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/Array.h
Seq.o: $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/String.h $(INCLUDEDIR)/Relay.h
Seq2.o: $(INCLUDEDIR)/Seq2.h $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/Array.h
Seq2.o: $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/String.h $(INCLUDEDIR)/Relay.h
Set.o: $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/String.h
Set.o: $(INCLUDEDIR)/Set.h $(INCLUDEDIR)/Array.h $(INCLUDEDIR)/Relay.h
String.o: $(INCLUDEDIR)/String.h $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Msg.h
String.o: $(INCLUDEDIR)/Relay.h
Symbol.o: $(INCLUDEDIR)/Symbol.h $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/Obj.h
Symbol.o: $(INCLUDEDIR)/String.h $(INCLUDEDIR)/Relay.h
Time.o: $(INCLUDEDIR)/Time.h $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Msg.h
Time.o: $(INCLUDEDIR)/String.h $(INCLUDEDIR)/Unit.h $(INCLUDEDIR)/Relay.h
Unit.o: $(INCLUDEDIR)/Unit.h $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Msg.h
Unit.o: $(INCLUDEDIR)/String.h $(INCLUDEDIR)/Relay.h
UnsignedInt.o: $(INCLUDEDIR)/UnsignedInt.h $(INCLUDEDIR)/Obj.h
UnsignedInt.o: $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/String.h
UnsignedInt.o: $(INCLUDEDIR)/Unit.h $(INCLUDEDIR)/Relay.h
asf.o: $(INCLUDEDIR)/asf.h $(INCLUDEDIR)/odl.h $(INCLUDEDIR)/Msg.h
asf.o: $(INCLUDEDIR)/pthread_wrapper.h $(INCLUDEDIR)/version.h
logUtils.o: $(INCLUDEDIR)/logUtils.h $(INCLUDEDIR)/asf_syslog.h
memUtils.o: $(INCLUDEDIR)/memUtils.h
odl_api.o: $(INCLUDEDIR)/Parser.h $(INCLUDEDIR)/Obj.h $(INCLUDEDIR)/Msg.h
odl_api.o: $(INCLUDEDIR)/Label.h $(INCLUDEDIR)/Ident.h $(INCLUDEDIR)/String.h
odl_api.o: $(INCLUDEDIR)/Group.h $(INCLUDEDIR)/Object.h
odl_api.o: $(INCLUDEDIR)/Symbol.h $(INCLUDEDIR)/Int.h $(INCLUDEDIR)/Unit.h
odl_api.o: $(INCLUDEDIR)/UnsignedInt.h $(INCLUDEDIR)/Double.h
odl_api.o: $(INCLUDEDIR)/Time.h $(INCLUDEDIR)/Array.h $(INCLUDEDIR)/Range.h
odl_api.o: $(INCLUDEDIR)/Set.h $(INCLUDEDIR)/Seq.h $(INCLUDEDIR)/Seq2.h
odl_api.o: $(INCLUDEDIR)/odl.h $(YYLEX_H_DIR)/yylex.h
pthread.o: $(INCLUDEDIR)/pthread_wrapper.h
socketXport.o: $(INCLUDEDIR)/odl.h $(INCLUDEDIR)/Msg.h
socketXport.o: $(INCLUDEDIR)/socketXport.h $(INCLUDEDIR)/inet.h
socketXport.o: $(INCLUDEDIR)/memUtils.h
