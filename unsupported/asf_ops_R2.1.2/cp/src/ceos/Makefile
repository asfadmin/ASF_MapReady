#
# @(#)Makefile	1.18 96/11/19 10:51:03
#
# Makefile for CEOS reader/writer utilities "ceos," "ceos.pre.rsat,"
# and library "libceos.a"
#
#     MACH should be set to the hardware platform identifier.
#     Accepted values are "alpha," "ibm," "sgi," and "sparc."
#     ("other" and "smart" are preserved for historical reasons but
#     are not officially supported for R1.4.)
#

#.SUFFIXES:
#.SUFFIXES:	.o .c

# macros definitions

EXEC_CEOS	 	= ceos
EXEC_CEOS_PRERSAT	= ceos.pre.rsat
LIBCEOS			= libceos.a
#INSTALLDIR		= $(ASF)/bin
INSTALLTMPDIR		= ../../bin/$(MACH)
INSTALLDIR		= $(ASF)/bin
#INSTALLLIB		= $(ASF)/lib
INSTALLLIB		= ../../lib/$(MACH)/global
#
# Because the reader/writer is distributed as a standalone package, keep
# the (global) header files within this directory and install them from
# here directly into $(ASF).  Installation of the library is accomplished
# via the Makefile in ../../lib.
#
INSTALLINC		= $(ASF)/include/sps
ECHO			= /bin/echo
CP			= /bin/cp -p
RM			= /bin/rm -f
AR			= ar rv
MKDIR			= /bin/mkdir -p

#    makedefs.generic  for other systems (it's worth a try)

glib:	$(MACH).glib

app:	all

all:	$(MACH).app

LIBOBJS=init.o	\
	process_ldr.o	\
	process_vdf.o	\
        io_utils.o	\
	tokens_lib.o	\
	odl_lib.o	\
	process_odl.o

OBJS=	main.o	\
	$(LIBOBJS)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Platform-specific definitions
#

#
# Definitions for DEC-AXP OSF/1
#
dec.app:	alpha.app
alpha.app:
	@$(ECHO) Making under OSF/1 ....
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=cc' $(INSTALLTMPDIR)/$(EXEC_CEOS)
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g -DPRE_RADARSAT' 'CC=cc' \
		$(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT)

dec.glib:	alpha.glib
alpha.glib:
	@$(ECHO) Making under OSF/1 ....
	-$(RM) $(LIBOBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=cc' $(INSTALLLIB)/$(LIBCEOS)

#
# Definitions for HP 9000/700 HP-UX 9
#
hp.app:
	@$(ECHO) Making under HP-UX ....
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g -Aa' 'CC=cc' $(INSTALLTMPDIR)/$(EXEC_CEOS)
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g  -Aa -DPRE_RADARSAT' 'CC=cc' \
		$(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT)

hp.glib:
	@$(ECHO) Making under HP-UX ....
	-$(RM) $(LIBOBJS)
	$(MAKE) 'CFLAGS= -g -Aa' 'CC=cc' $(INSTALLLIB)/$(LIBCEOS)

#
# Definitions for IBM RISC (AIX 4.1.x, maybe AIX 3.x)
#
ibm.app:
	@$(ECHO) Making under AIX ....
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=cc' 'LIBS=-lisode' $(INSTALLTMPDIR)/$(EXEC_CEOS)
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g -DPRE_RADARSAT' 'CC=cc' 'LIBS=-lisode' \
		$(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT)

ibm.glib:
	@$(ECHO) Making under AIX ....
	-$(RM) $(LIBOBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=cc' 'LIBS=-lisode' $(INSTALLLIB)/$(LIBCEOS)

#
# Definitions for SGI IRIX 5.2 (and maybe IRIX 4.x)
#
sgi.app:
	@$(ECHO) Making under SGI IRIX ....
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=cc' $(INSTALLTMPDIR)/$(EXEC_CEOS)
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g -DPRE_RADARSAT' 'CC=cc' \
		$(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT)

sgi.glib:
	@$(ECHO) Making under SGI IRIX ....
	-$(RM) $(LIBOBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=cc' $(INSTALLLIB)/$(LIBCEOS)

#
# Definitions for Sun Sparc SunOS 4.1.x
#
sun4.app:
	@$(ECHO) Making under SunOS 4.1.x ....
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=acc' $(INSTALLTMPDIR)/$(EXEC_CEOS)
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g -DPRE_RADARSAT' 'CC=acc' \
		$(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT)

sun4.glib:
	@$(ECHO) Making under SunOS 4.1.x ....
	-$(RM) $(LIBOBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=acc' $(INSTALLLIB)/$(LIBCEOS)

#
# Definitions for Sun Sparc SunOS 5.x (Solaris 2.x)
#
sun5.app:	sparc.app
sparc.app:
	@$(ECHO) Making under Solaris 2.x ....
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=cc' $(INSTALLTMPDIR)/$(EXEC_CEOS)
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g -DPRE_RADARSAT' 'CC=cc' \
		$(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT)

sun5.glib:	sparc.glib
sparc.glib:
	@$(ECHO) Making under Solaris 2.x ....
	-$(RM) $(LIBOBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=cc' $(INSTALLLIB)/$(LIBCEOS)


#
# Definitions for generic systems (give it a try)
#
other.app:	generic.app
generic.app:
	@$(ECHO) Making under generic UNIX ....
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=cc' $(INSTALLTMPDIR)/$(EXEC_CEOS)
	-$(RM) $(OBJS)
	$(MAKE) 'CFLAGS= -g -DPRE_RADARSAT' 'CC=cc' \
		$(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT)

other.glib:	generic.glib
generic.glib:
	@$(ECHO) Making under generic UNIX ....
	-$(RM) $(LIBOBJS)
	$(MAKE) 'CFLAGS= -g' 'CC=cc' $(INSTALLLIB)/$(LIBCEOS)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


$(INSTALLTMPDIR)/$(EXEC_CEOS):	$(EXEC_CEOS)
	-$(RM) $(INSTALLTMPDIR)/$(EXEC_CEOS)
	-$(MKDIR) $(INSTALLTMPDIR)
	$(CP) $(EXEC_CEOS) $(INSTALLTMPDIR)/$(EXEC_CEOS)

$(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT):	$(EXEC_CEOS_PRERSAT)
	-$(RM) $(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT)
	-$(MKDIR) $(INSTALLTMPDIR)
	$(CP) $(EXEC_CEOS_PRERSAT) $(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT)

$(INSTALLLIB)/$(LIBCEOS):	$(LIBCEOS)
	-$(RM) $(INSTALLLIB)/$(LIBCEOS)
	-$(MKDIR) $(INSTALLLIB)
	$(CP) $(LIBCEOS) $(INSTALLLIB)/$(LIBCEOS)

$(EXEC_CEOS):	$(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)

$(LIBCEOS): $(LIBOBJS)	
	$(AR) $@ $(LIBOBJS)

$(EXEC_CEOS_PRERSAT):	$(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)

$(OBJS):
	$(CC) -c  $*.c $(CFLAGS)

#
# Remove the old versions one at a time before installing -- there are
# other files in the target directory!
#
glib_inst:	defs.h extern.h oceos.h
	-$(RM) $(INSTALLINC)/defs.h
	-$(RM) $(INSTALLINC)/extern.h
	-$(RM) $(INSTALLINC)/oceos.h
	-$(MKDIR) $(INSTALLINC)
	$(CP) defs.h extern.h oceos.h $(INSTALLINC)

clean:  
	-$(RM) $(OBJS) $(EXEC_CEOS) $(EXEC_CEOS_PRERSAT) $(LIBCEOS)

bin_inst:	$(INSTALLTMPDIR)/$(EXEC_CEOS) \
		$(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT)
		-$(MKDIR) $(INSTALLDIR)
		$(CP) $(INSTALLTMPDIR)/$(EXEC_CEOS) $(INSTALLDIR)
		$(CP) $(INSTALLTMPDIR)/$(EXEC_CEOS_PRERSAT) $(INSTALLDIR)

# No point in defining dependencies -- all object files are recompiled
# for each build.
