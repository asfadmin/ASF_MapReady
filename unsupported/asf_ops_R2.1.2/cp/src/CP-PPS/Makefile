#
# @(#)Makefile	1.19 96/11/08 12:22:41
#
# Makefile for CP-PPS application
#
# Note:  The build and installation of the executable must be done only
#        on the Sun.  The man page installation should be done on both
#        the Sun and the SGI.
#

EXECUTABLE      = CP-PPS
#XAPPLOADDIR     = $(LOCAL)/app-defaults
#INSTALLDIR      = $(ASF)/bin
INSTALLDIR      = ../../bin/$(MACH)

CC		= cc
CP		= /bin/cp -p
MKDIR		= /bin/mkdir -m 775 -p
RM		= /bin/rm -f
CHMOD		= /usr/bin/chmod

#MAN_PATH	= $(ASF)/man
MAN_PATH	= ../../etc/man
MAN_SRC		= $(EXECUTABLE).man
MAN_EXT		= 1

INCLUDEPPS	= $(ASF)/include/pps
INCLUDESPS	= $(ASF)/include/sps
INCLUDEDCE	= /opt/dce/share/include

#
# -DDEBUG_PPS prints out ODL messages received from PPS
# -DREAD_JOB_FROM_FILE reads jobs from /tmp/CP-PPS.test
#
CFLAGS=-g -D__sun -DDEBUG_PPS -D_REENTRANT_FUNCTIONS -I$(INCLUDESPS) -I$(INCLUDEPPS) -I$(INCLUDEDCE)
#CFLAGS=-g -D__sun -D_REENTRANT_FUNCTIONS -I$(INCLUDESPS) -I$(INCLUDEPPS) -I$(INCLUDEDCE)
#CFLAGS=-g -D__sun -DDEBUG_PPS -D_REENTRANT_FUNCTIONS -DREAD_JOB_FROM_FILE -I$(INCLUDESPS) -I$(INCLUDEPPS) -I$(INCLUDEDCE)

#
# Use the full name of libpps_client.a to avoid getting the runtime message
#     ld.so.1:  CP-PPS: fatal: libpps_client.so: can't open file: errno=2
# In other words, don't specify the library as -lpps_client in PPSLIB.
#
LIBDIRS		= -L$(ASF)/lib/sparc -L$(ASF)/lib
PPSLIB		= $(ASF)/lib/sparc/libpps_client.a
SPSLIB		= $(ASF)/lib/sparc/libsps.a
LIBS	= $(LIBDIRS) $(SPSLIB) -lsocket -lnsl -ldce -lthread -lm $(PPSLIB)

app:	all

all:	$(INSTALLDIR)/$(EXECUTABLE)

$(INSTALLDIR)/$(EXECUTABLE):	$(EXECUTABLE)
	-$(MKDIR) $(INSTALLDIR)
	$(CP) $(EXECUTABLE) $(INSTALLDIR)

$(EXECUTABLE): pps.o StringToODL.o
	$(CC) $(CFLAGS) -o $(EXECUTABLE) pps.o StringToODL.o $(LIBS)
	-$(CHMOD) 775 $@

.c.o:
	$(CC) -c $(CFLAGS) $<
	-$(CHMOD) 664 $@

clean:
	-$(RM) pps.o StringToODL.o core $(EXECUTABLE)

install.man:	$(EXECUTABLE).man
	-$(RM) $(MAN_PATH)/man$(MAN_EXT)/$(EXECUTABLE).$(MAN_EXT)
	-$(MKDIR) $(MAN_PATH)/man$(MAN_EXT)
	$(CP) $(EXECUTABLE).man \
	    $(MAN_PATH)/man$(MAN_EXT)/$(EXECUTABLE).$(MAN_EXT)

# DO NOT DELETE THIS LINE -- make depend depends on it.
StringToODL.o: $(INCLUDESPS)/odl.h $(INCLUDESPS)/Msg.h
pps.o: $(INCLUDESPS)/asf.h $(INCLUDESPS)/odl.h
pps.o: $(INCLUDESPS)/Msg.h $(INCLUDESPS)/asfcommon.h
pps.o: $(INCLUDESPS)/asf_syslog.h local_messages.h version.h
pps.o: $(INCLUDEPPS)/PPSerr.h $(INCLUDEPPS)/messages.h
