#
# @(#)Makefile	1.42 96/10/31 12:21:53
#
# This is the Makefile for building the CP library "libcp.a."
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

INSTALLINCDIR=$(ASF)/include/cp
INSTALLLIBDIR=$(ASF)/lib
INSTALLTMPDIR=../../lib/$(MACH)/global
INCLUDEDIR=../../include
TEMPLATEDIR=$(LOCAL)/sps/templates

AR=ar -rcvs
CHMOD= /bin/chmod
CP=/bin/cp -p
ECHO=/bin/echo
MKDIR=/bin/mkdir -m 775 -p
RANLIB=@echo
RM=/bin/rm -f

CP_LIB	= libcp.a

#
CC_INCLUDE = -I$(INCLUDEDIR) 

#
# Default platform:  SGI/IRIX 6.2
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
	@$(ECHO) "Making on SGI/IRIX 6.2 ..."
	$(MAKE) 'BASE_SRCS = $(BASE_SRCS)' \
		'BASE_OBJS = $(BASE_OBJS)' \
		'CFLAGS= -DYYPURE $(CC_INCLUDE) -D__sgi -D_POSIX_SOURCE -D_BSD_COMPAT -ansi ' \
		'LDLIBS= $(CP_LIB)' $(INSTALLTMPDIR)/$(CP_LIB)

BASE_SRCS = dataset.c

BASE_OBJS = dataset.o 
BASE_INCS = product.h  qexit_stati.h

SRCS	= $(BASE_SRCS)
OBJS =  $(BASE_OBJS) 

#
# Do not include version.h in this list; it should not be installed
# under $ASF/include/cp.
#
ALL_INCS = $(BASE_INCS) 

$(INSTALLTMPDIR)/$(CP_LIB):	$(CP_LIB)
	-$(MKDIR) $(INSTALLTMPDIR)
	-$(RM) $(INSTALLTMPDIR)/$(CP_LIB)
	$(CP) $(CP_LIB) $(INSTALLTMPDIR)

$(CP_LIB):	$(OBJS)
	$(AR) $@ $?
	$(RANLIB) $@
	-$(CHMOD) 644 $@

#
# Remove the old library from the installation directory, but don't copy
# in the new one.  That step is done by the Makefile in ../../lib.
#
install:	$(INSTALLTMPDIR)/$(CP_LIB)
	-$(MKDIR) $(INSTALLLIBDIR)
	-$(RM) $(INSTALLLIBDIR)/$(CP_LIB)
	-$(MKDIR) $(INSTALLINCDIR)
	-(cd $(INSTALLINCDIR); $(RM) $(ALL_INCS))
	(cd $(INCLUDEDIR); \
	    $(CP) $(ALL_INCS) $(INSTALLINCDIR))

# IRIX install syntax isn't compatible with OSF/1's.  Fix later.

#.c.o:
#	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<
.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)

$(SRCS) $(ODL_INCS):
	sccs get $@

clean:
	-$(RM) $(OBJS) core $(CP_LIB) 

#
# Dependencies
#
# Note the presence of "hidden" dependencies; for example, asf.h includes
# other header files.
#
# DO NOT DELETE THIS LINE -- make depend depends on it.
