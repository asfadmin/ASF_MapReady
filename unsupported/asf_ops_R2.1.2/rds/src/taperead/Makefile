#
# @(#)Makefile	1.9 97/03/20 17:29:24
#
# This is the Makefile for building the DCRSi tape reader (taperead).
#
# The source has been written for an SGI Challenge XL running IRIX 5.2.
#
#
EXES	= taperead
LIBDIR		= $(ASF)/lib
INCLUDEDIR	= $(ASF)/sps/include
AMPEXDIR	= /usr/local/src/ampex
MYRIADIR	= /var/sys/myriad/driver
INSTALLDIR	= ../../bin/$(MACH)

CHMOD		= /bin/chmod
#CC		= gcc
AR		= ar -rcvs
RM		= /bin/rm -f
RANLIB		= @echo
ECHO		= /bin/echo
CC_INCLUDE	= -I. -I$(AMPEXDIR)/VDItest -I$(AMPEXDIR)/VDIdriver \
		  -I$(MYRIADIR)
CFLAGS		= $(CC_INCLUDE) -g
LDLIBS		= -lgen

MAN_SRC		= $(EXES).man
MAN_PATH	= ../../etc/man
MAN_EXT		= 1

#
# The next few macros enable source files to be pulled out of SCCS
# automatically as necessary.
#
C_SRCS	= tape.c
H_SRCS	= tape.h version.h

OBJS	= $(EXES:=.o) $(C_SRCS:.c=.o)
SRCS	= $(EXES:=.c) $(C_SRCS)

app:	all
all:	$(INSTALLDIR)/$(EXES)

$(INSTALLDIR)/$(EXES):	$(EXES)
	install -v -m 775 -F $(INSTALLDIR) $(EXES)

$(EXES): $(EXES:=.o) $(OBJS) $(LOCALLIBS)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)
	-$(CHMOD) g+w $@

install.man:	$(MAN_SRC)
	install -v -m 664 -src $(MAN_SRC) \
	    -F $(MAN_PATH)/man$(MAN_EXT) $(EXES).$(MAN_EXT)

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<
	-$(CHMOD) g+w $@
.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)
	-$(CHMOD) g+w $@
clean:
	-$(RM) core $(EXES:=.o) $(OBJS) $(EXES)
$(SRCS):
	sccs get $@

$(H_SRCS):
	sccs get $@

#
# Dependencies
#
# Note the presence of "hidden" dependencies; for example, asf.h includes
# other header files.
#

tape.o:		tape.h
taperead.o:	tape.h version.h

#
# End of dependencies -- do not add anything after this line
