#
# @(#)Makefile	1.58 98/03/17 11:22:35
#
# This is the Makefile for building the Raw Data Scanner application (RDS).
#
# The source has been written for an SGI Challenge XL running IRIX 5.2.
#
#
EXES		= rds
#LIBDIR		= $(ASF)/lib
LIBDIR		= ../../lib/$(MACH)/global
INCLUDEDIR	= ../../include
#INSTALLDIR	= $(ASF)/bin
INSTALLDIR	= ../../bin/$(MACH)
AMPEXDIR	= /usr/local/src/ampex
MYRIADIR        = /var/sys/myriad/driver
#CONFIGDIR	= $(LOCAL)/rds
CONFIGDIR	= ../../etc/install
CONFIGFILE	= rds-config.odl

CHMOD		= /bin/chmod
#CC		= gcc
AR		= ar -rcvs
RM		= /bin/rm -f
RANLIB		= @echo
ECHO		= /bin/echo
CC_INCLUDE	= -I. -I$(INCLUDEDIR) \
		  -I$(AMPEXDIR)/VDItest -I$(AMPEXDIR)/VDIdriver \
		  -I$(MYRIADIR)

CFLAGS		= $(CC_INCLUDE) -g -D__sgi \
		  -D_SGI_SOURCE -D_SGI_MP_SOURCE -D_SGI_REENTRANT_FUNCTIONS
ASFLIB		= $(LIBDIR)/libsps.a
SARLIB		= $(LIBDIR)/libsarproc.a
LOCALLIBS	= $(ASFLIB) $(SARLIB)
LDLIBS		= $(LOCALLIBS) -lgen -lcomplib.sgimath -lm -lftn

MAN_SRC		= $(EXES).man
#MAN_PATH	= $(ASF)/man
MAN_PATH	= ../../etc/man
MAN_EXT		= 1

#
# The next few macros enable source files to be pulled out of SCCS
# automatically as necessary.
#
F_SRCS	= \
	utm.f		\
	ssmi.f		\
	utc2gha.f
#	initdops.f
#	getlocc.f
#	getloc.f
#	getlook.f
#	getdop2.f
#	calcr.f
C_SRCS	= \
	Q.c		\
	Status.c	\
	Tape.c		\
	R1.c		\
	E1.c		\
	J1.c		\
	Pulse.c		\
	RDS.c
#	corn_loc.c
H_SRCS	= \
	Q.h		\
	Status.h	\
	Tape.h		\
	Timer.h		\
	R1.h		\
	R1_prn.h	\
	R1_cal.h	\
	E1.h		\
	E1_prn.h	\
	J1.h		\
	Pulse.h		\
	RDS.h		\
	version.h

OBJS	= $(EXES:=.o) $(C_SRCS:.c=.o) $(F_SRCS:.f=.o)
SRCS	= $(EXES:=.c) $(C_SRCS) $(F_SRCS)

app:	all
all:	$(INSTALLDIR)/$(EXES) $(CONFIGDIR)/$(CONFIGFILE)

$(INSTALLDIR)/$(EXES):	$(EXES)
	install -v -m 775 -F $(INSTALLDIR) $(EXES)

$(CONFIGDIR)/$(CONFIGFILE):	$(CONFIGFILE)
	install -v -m 664 -F $(CONFIGDIR) $(CONFIGFILE)

$(EXES): $(EXES:=.o) $(OBJS) $(LOCALLIBS)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)
	-$(CHMOD) g+w $@

install.man:	$(EXES).man
	install -v -m 664 -src $(MAN_SRC) \
		-F $(MAN_PATH)/man$(MAN_EXT) $(EXES).$(MAN_EXT)

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<
	-$(CHMOD) g+w $@
.f.o:
	$(F77) $(FFLAGS) -c $*.f
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
# DO NOT DELETE THIS LINE -- make depend depends on it.
Q.o: Q.h
E1.o: E1.h E1_prn.h Pulse.h
R1.o: R1.h R1_prn.h R1_cal.h Pulse.h
J1.o: J1.h Pulse.h
Pulse.o: version.h Tape.h Pulse.h
Status.o: Status.h 
Tape.o: $(MYRIADIR)/ml930drv.h
Tape.o: $(AMPEXDIR)/VDItest/vdi_io.h
Tape.o: $(AMPEXDIR)/VDIdriver/ampex.h
Tape.o: $(AMPEXDIR)/VDItest/debug.h
Tape.o: $(AMPEXDIR)/VDItest/debug_e.h
Tape.o: $(AMPEXDIR)/VDIdriver/vdi_defs.h
Tape.o: $(AMPEXDIR)/VDItest/vdi_gen.h
Tape.o: $(AMPEXDIR)/VDItest/vdi_test_e.h Tape.h
rds.o: RDS.h version.h 
rds.o: $(ASF)/include/sps/asf.h 
rds.o: $(ASF)/include/sps/asf_syslog.h
rds.o: $(ASF)/include/sps/logUtils.h
RDS.o: RDS.h Timer.h
RDS.o: $(ASF)/include/sps/asf_syslog.h
RDS.o: $(ASF)/include/sps/asf.h
RDS.o: $(ASF)/include/sps/Object.h
RDS.o: $(ASF)/include/sps/Ident.h
RDS.o: $(ASF)/include/sps/Symbol.h
RDS.o: $(ASF)/include/sps/Int.h
RDS.o: $(ASF)/include/sps/Time.h
