#
# @(#)Makefile	1.13 98/03/17 11:21:48
#
# This is the Makefile for building the chkangl utility.
#
# The source has been written for an SGI Challenge XL running IRIX 5.2
# and for DEC Alpha running OSF/1 V1.3.
#
#
EXES		= chkangl
#LIBDIR		= $(ASF)/lib
LIBDIR		= ../../lib/$(MACH)/global
INCLUDEDIR	= ../../include
#INSTALLDIR	= $(ASF)/bin
INSTALLDIR	= ../../bin/$(MACH)

CHMOD		= /bin/chmod
#CC		= gcc
AR		= ar -rcvs
RM		= /bin/rm -f
RANLIB		= @echo
ECHO		= /bin/echo
CC_INCLUDE	= -I$(INCLUDEDIR)
CFLAGS		= $(CC_INCLUDE)
ASFLIB		= $(LIBDIR)/libsps.a
LOCALLIBS	= $(ASFLIB)
#
# On SGI
#
LDLIBS		= $(LOCALLIBS) -lm
#
# On DEC
#
#LDLIBS		= $(LOCALLIBS) -lm -lc_r -lpthreads -lmach

MAN_SRC		= $(EXES).man
#MAN_PATH	= $(ASF)/man
MAN_PATH	= ../../etc/man
MAN_EXT		= 1

#
# The next few macros enable source files to be pulled out of SCCS
# automatically as necessary.
#
H_SRCS	= scene_file.h

SRCS	= $(EXES:=.c)
OBJS	= $(EXES:=.o)

app:	all
all:	$(INSTALLDIR)/$(EXES)

$(INSTALLDIR)/$(EXES):	$(EXES)
	install -v -m 775 -F $(INSTALLDIR) $(EXES)

$(EXES): $(OBJS) $(LOCALLIBS)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)
	-$(CHMOD) g+w $@

#
# chkangl has no man page
#
install.man:
	@ $(ECHO) $(EXES) has no man page -- skipping installation
#	install -v -m 664 -src $(MAN_SRC) \
#		-F $(MAN_PATH)/man$(MAN_EXT) $(EXES).$(MAN_EXT)

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<
	-$(CHMOD) g+w $@
.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)
	-$(CHMOD) g+w $@
clean:
	-$(RM) core $(OBJS) $(EXES)

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
#
# DO NOT DELETE THIS LINE -- make depend depends on it.
chkangl.o: scene_file.h $(INCLUDEDIR)/odl.h
chkangl.o: $(INCLUDEDIR)/Msg.h $(INCLUDEDIR)/Object.h
chkangl.o: $(INCLUDEDIR)/Label.h $(INCLUDEDIR)/Obj.h
chkangl.o: $(INCLUDEDIR)/Ident.h $(INCLUDEDIR)/String.h
chkangl.o: version.h
