# Makefile for Independent JPEG Group's software
# Hacked for ASF use by Orion Lawlor, 6/17/99

include ../../make_support/system_rules

# This makefile is suitable for Unix-like systems with ANSI-capable compilers.
# If you have a non-ANSI compiler, makefile.unix is a better starting point.

# Generally, we recommend defining any configuration symbols in jconfig.h,
# NOT via -D switches here.

# Link-time cc options:
LDFLAGS= 

# To link any special libraries, add the necessary -l commands here.
LDLIBS= 

# Put here the object file name for the correct system-dependent memory
# manager file.  For Unix this is usually jmemnobs.o, but you may want
# to use jmemansi.o or jmemname.o if you have limited swap space.
SYSDEPMEM= jmemnobs.o

# miscellaneous OS-dependent stuff
# linker
LN= $(CC)
# file deletion command
RM= rm -f
# library (.a) file creation command
AR= ar rc
# second step in .a creation (use "touch" if not needed)

# End of configurable options.


# source files: JPEG library proper
LIBSOURCES= jcapimin.c jcapistd.c jccoefct.c jccolor.c jcdctmgr.c jchuff.c \
        jcinit.c jcmainct.c jcmarker.c jcmaster.c jcomapi.c jcparam.c \
        jcphuff.c jcprepct.c jcsample.c jctrans.c jdapimin.c jdapistd.c \
        jdatadst.c jdatasrc.c jdcoefct.c jdcolor.c jddctmgr.c jdhuff.c \
        jdinput.c jdmainct.c jdmarker.c jdmaster.c jdmerge.c jdphuff.c \
        jdpostct.c jdsample.c jdtrans.c jerror.c jfdctflt.c jfdctfst.c \
        jfdctint.c jidctflt.c jidctfst.c jidctint.c jidctred.c jquant1.c \
        jquant2.c jutils.c jmemmgr.c
# memmgr back ends: compile only one of these into a working library
SYSDEPSOURCES= jmemansi.c jmemname.c jmemnobs.c jmemdos.c jmemmac.c
# source files: cjpeg/djpeg/jpegtran applications, also rdjpgcom/wrjpgcom
APPSOURCES= cjpeg.c djpeg.c jpegtran.c rdjpgcom.c wrjpgcom.c cdjpeg.c \
        rdcolmap.c rdswitch.c transupp.c rdppm.c wrppm.c rdgif.c wrgif.c \
        rdtarga.c wrtarga.c rdbmp.c wrbmp.c rdrle.c wrrle.c
SOURCES= $(LIBSOURCES) $(SYSDEPSOURCES) $(APPSOURCES)
# files included by source files
INCLUDES= jchuff.h jdhuff.h jdct.h jerror.h jinclude.h jmemsys.h jmorecfg.h \
        jpegint.h jpeglib.h jversion.h cdjpeg.h cderror.h transupp.h
# documentation, test, and support files
DOCS= README install.doc usage.doc cjpeg.1 djpeg.1 jpegtran.1 rdjpgcom.1 \
        wrjpgcom.1 wizard.doc example.c libjpeg.doc structure.doc \
        coderules.doc filelist.doc change.log
MKFILES= configure makefile.cfg makefile.ansi makefile.unix makefile.bcc \
        makefile.mc6 makefile.dj makefile.wat makefile.vc makelib.ds \
        makeapps.ds makeproj.mac makcjpeg.st makdjpeg.st makljpeg.st \
        maktjpeg.st makefile.manx makefile.sas makefile.mms makefile.vms \
        makvms.opt
CONFIGFILES= jconfig.cfg jconfig.bcc jconfig.mc6 jconfig.dj jconfig.wat \
        jconfig.vc jconfig.mac jconfig.st jconfig.manx jconfig.sas \
        jconfig.vms
CONFIGUREFILES= config.guess config.sub install-sh ltconfig ltmain.sh
OTHERFILES= jconfig.doc ckconfig.c ansi2knr.c ansi2knr.1 jmemdosa.asm
TESTFILES= testorig.jpg testimg.ppm testimg.bmp testimg.jpg testprog.jpg \
        testimgp.jpg
DISTFILES= $(DOCS) $(MKFILES) $(CONFIGFILES) $(SOURCES) $(INCLUDES) \
        $(CONFIGUREFILES) $(OTHERFILES) $(TESTFILES)
# library object files common to compression and decompression
COMOBJECTS= jcomapi.o jutils.o jerror.o jmemmgr.o $(SYSDEPMEM)
# compression library object files
CLIBOBJECTS= jcapimin.o jcapistd.o jctrans.o jcparam.o jdatadst.o jcinit.o \
        jcmaster.o jcmarker.o jcmainct.o jcprepct.o jccoefct.o jccolor.o \
        jcsample.o jchuff.o jcphuff.o jcdctmgr.o jfdctfst.o jfdctflt.o \
        jfdctint.o
# decompression library object files
DLIBOBJECTS= jdapimin.o jdapistd.o jdtrans.o jdatasrc.o jdmaster.o \
        jdinput.o jdmarker.o jdhuff.o jdphuff.o jdmainct.o jdcoefct.o \
        jdpostct.o jddctmgr.o jidctfst.o jidctflt.o jidctint.o jidctred.o \
        jdsample.o jdcolor.o jquant1.o jquant2.o jdmerge.o
# These objectfiles are included in libjpeg.a
LIBOBJECTS= $(CLIBOBJECTS) $(DLIBOBJECTS) $(COMOBJECTS)
# object files for sample applications (excluding library files)
COBJECTS= cjpeg.o rdppm.o rdgif.o rdtarga.o rdrle.o rdbmp.o rdswitch.o \
        cdjpeg.o
DOBJECTS= djpeg.o wrppm.o wrgif.o wrtarga.o wrrle.o wrbmp.o rdcolmap.o \
        cdjpeg.o
TROBJECTS= jpegtran.o rdswitch.o cdjpeg.o transupp.o


all: libjpeg.a 
	rm *.o

libjpeg.a: $(LIBOBJECTS)
	-$(RM) libjpeg.a
	$(AR) libjpeg.a  $(LIBOBJECTS)
	$(RANLIB) libjpeg.a
	mv libjpeg.a $(LIBDIR)

