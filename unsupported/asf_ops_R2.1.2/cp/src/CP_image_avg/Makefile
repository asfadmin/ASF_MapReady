#
# @(#)Makefile	1.11 96/11/01 17:56:43
#
# Makefile for building the image detection utility "CP_image_avg"
#

CC		= /usr/bin/cc
CFLAGS		=  -g
CHMOD		= /usr/bin/chmod
INCLUDEDIR	= $(ASF)/include/sps
LOCALLIBDIR	= $(ASF)/lib
#INSTALLDIR	= $(ASF)/bin
INSTALLDIR	= ../../bin/$(MACH)
LOCALLIBS	= -lsps
LIBS		= -lXt -lX11 -lm $(LOCALLIBS)
AVG		= CP_image_avg
MAN_SRC         = $(AVG).man
# change MAN_PATH to point to your man page top directory
#MAN_PATH        = $(ASF)/man
MAN_PATH        = ../../etc/man
# change MAN_EXT to the appropriate section
MAN_EXT         = 1


app:	all
all:	$(INSTALLDIR)/$(AVG)

OBJ	=   \
	    gavgfile.o \
	    gavg_out.o \
	    gcommand_line.o \
	    gcomplex_image.o \
	    gcomplex_variables.o \
	    gfile_descriptor.o \
	    gimagefile.o \
	    gimage_subset.o \
	    gleaderfile.o \
	    gfunctions.o \
	    gmain.o \
            gunsigned_image.o \
            gzoom.o

# The next few macros enable source files to be pulled from SCCS automatically
# as necessary.
SRC	= \
	gavgfile.c \
	gavg_out.c \
	gcommand_line.c \
	gcomplex_image.c \
	gcomplex_variables.c \
	gdefines.h \
	gfile_descriptor.c \
	gfunc_decl.h \
	gglobal_var.h \
	gversion.h \
	gimage_subset.c \
	gimagefile.c \
	ginclude.h \
	gleaderfile.c \
	gfunctions.c \
	gmain.c \
	gunsigned_image.c \
        gzoom.c

$(SRC):
	sccs get $@

COMMONSRC	= \
		$(INCLUDEDIR)/asf_syslog.h

$(COMMONSRC):
	(cd $(INCLUDEDIR); sccs get $@)

.c.o:
	$(CC) -c $(CFLAGS) -I$(INCLUDEDIR) $<
	-$(CHMOD) 664 $@

$(INSTALLDIR)/$(AVG):	$(AVG)
	install -v -m 775 -F $(INSTALLDIR) $(AVG)

$(AVG):	$(OBJ)
	-/bin/rm $(AVG)
	$(CC) -o $@ $(CFLAGS) -L$(LOCALLIBDIR) $(OBJ) $(LIBS)
	-$(CHMOD) 775 $@

clean:
	-/bin/rm -f $(AVG) $(OBJ) core

install.man::
	install -v -m 664 -src $(MAN_SRC) \
		-F $(MAN_PATH)/man$(MAN_EXT) $(AVG).$(MAN_EXT)

# DO NOT DELETE THIS LINE -- make depend depends on it.
gavg_out.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gavg_out.o: gfunc_decl.h gglobal_var.h
gavgfile.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gavgfile.o: gfunc_decl.h gglobal_var.h
gcommand_line.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gcommand_line.o: gfunc_decl.h gglobal_var.h gversion.h
gcomplex_image.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gcomplex_image.o: gfunc_decl.h gglobal_var.h
gcomplex_variables.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h
gcomplex_variables.o: gdefines.h gfunc_decl.h gglobal_var.h
gfile_descriptor.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gfile_descriptor.o: gfunc_decl.h gglobal_var.h
gfunctions.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gfunctions.o: gfunc_decl.h gglobal_var.h
gimage_subset.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gimage_subset.o: gfunc_decl.h gglobal_var.h
gimagefile.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gimagefile.o: gfunc_decl.h gglobal_var.h
gleaderfile.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gleaderfile.o: gfunc_decl.h gglobal_var.h
gmain.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gmain.o: gfunc_decl.h gglobal_var.h
gunsigned_image.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gunsigned_image.o: gfunc_decl.h gglobal_var.h
gzoom.o: ginclude.h $(INCLUDEDIR)/asf_syslog.h gdefines.h
gzoom.o: gfunc_decl.h gglobal_var.h
ginclude.o: $(INCLUDEDIR)/asf_syslog.h
