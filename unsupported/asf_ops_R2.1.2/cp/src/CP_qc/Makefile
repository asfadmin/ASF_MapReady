#
# @(#)Makefile	1.36 96/11/01 18:01:35
#
# This is the Makefile for building the Motif Quality Control application,
# "CP_qc."
#

CC		= /usr/bin/cc
CFLAGS		= -g 
CHMOD		= /usr/bin/chmod

INCLUDEDIR	= $(ASF)/include/sps
CP_INCLUDEDIR	= $(ASF)/include/cp
LIBDIR		= $(ASF)/lib

#INSTALLDIR	= $(ASF)/bin
INSTALLDIR	= ../../bin/$(MACH)
#XAPPLOADDIR	= $(LOCAL)/app-defaults
XAPPLOADDIR	= ../../etc/app-defaults
LOCALLIBS	= -lsps
LIBS		= -lXm -lXt -lX11 -lm $(LOCALLIBS)
QC		= CP_qc
QC_AD		= CP_Qc
MAN_SRC        = $(QC).man
# change MAN_PATH to point to your man page top directory
#MAN_PATH        = $(ASF)/man
MAN_PATH        = ../../etc/man
# change MAN_EXT to the appropriate section
MAN_EXT         = 1


app:	all
#all:	$(QC)
all:	$(INSTALLDIR)/$(QC) $(XAPPLOADDIR)/$(QC_AD).ad

OBJ	=   \
	    qavgfile.o \
	    qavg_out.o \
	    qbackground_pixel.o \
	    qbox.o \
	    qcallbacks.o \
	    qcommand_line.o \
	    qcomplex_histogram.o \
	    qcomplex_image.o \
	    qcomplex_variables.o \
	    qfile_descriptor.o \
	    qfunctions.o \
	    qhistogram.o \
	    qimagefile.o \
	    qimage_subset.o \
	    qinfo_widgets.o \
	    qleaderfile.o \
	    qleft.o \
	    qmain.o \
	    qmenubar.o \
	    qpixmap.o \
	    qright.o \
	    qset_labels.o \
	    qstretch.o \
	    qunsigned_image.o \
	    qzoom.o

# The next few macros enable source files to be pulled from SCCS automatically
# as necessary.
SRC	= \
	qavgfile.c \
	qavg_out.c \
	qbackground_pixel.c \
	qbox.c \
	qcallbacks.c \
	qcommand_line.c \
	qcomplex_histogram.c \
	qcomplex_image.c \
	qcomplex_variables.c \
	qversion.h \
	qdefines.h \
	qfile_descriptor.c \
	qfunc_decl.h \
	qfunctions.c \
	qglobal_var.h \
	qhistogram.c \
	qimage_subset.c \
	qimagefile.c \
	qinclude.h \
	qinfo_widgets.c \
	qleaderfile.c \
	qleft.c \
	qmain.c \
	qmenubar.c \
	qpixmap.c \
	qright.c \
	qset_labels.c \
	qstretch.c \
	qunsigned_image.c \
	qzoom.c

$(SRC):
	sccs get $@

COMMONSRC	= \
		$(INCLUDEDIR)/asf_syslog.h

$(COMMONSRC):
	(cd $(INCLUDEDIR); sccs get $@)

.c.o:
	$(CC) -c $(CFLAGS) -I$(INCLUDEDIR) -I$(CP_INCLUDEDIR) $<
	-$(CHMOD) 664 $@

#
# Build procedure now installs a header file used by CP -- should be
# moved to library someday
#
$(INSTALLDIR)/$(QC):	$(QC)
	install -v -m 775 -F $(INSTALLDIR) $(QC)

$(XAPPLOADDIR)/$(QC_AD).ad:	$(QC_AD)
	install -v -m 664 -src $(QC_AD) -F $(XAPPLOADDIR) $(QC_AD).ad

$(QC):	$(OBJ)
	-/bin/rm $(QC)
	$(CC) -o $@ $(CFLAGS) $(OBJ) -L$(LIBDIR) $(LIBS)
	-$(CHMOD) 775 $@

clean:
	-/bin/rm -f $(QC) $(OBJ) core

install.man::
	install -v -m 664 -src $(MAN_SRC) \
		-F $(MAN_PATH)/man$(MAN_EXT) $(QC).$(MAN_EXT)

#
# Dependencies
#
# DO NOT DELETE THIS LINE -- make depend depends on it.
qavg_out.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qavg_out.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qavgfile.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qavgfile.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qbackground_pixel.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h
qbackground_pixel.o: qdefines.h $(CP_INCLUDEDIR)/qexit_stati.h
qbackground_pixel.o: qfunc_decl.h qglobal_var.h
qbox.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qbox.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qcallbacks.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qcallbacks.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qcallbacks.o: qversion.h
qcommand_line.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qcommand_line.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h
qcommand_line.o: qglobal_var.h qversion.h
qcomplex_histogram.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h
qcomplex_histogram.o: qdefines.h $(CP_INCLUDEDIR)/qexit_stati.h
qcomplex_histogram.o: qfunc_decl.h qglobal_var.h
qcomplex_image.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qcomplex_image.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h
qcomplex_image.o: qglobal_var.h
qcomplex_variables.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h
qcomplex_variables.o: qdefines.h $(CP_INCLUDEDIR)/qexit_stati.h
qcomplex_variables.o: qfunc_decl.h qglobal_var.h
qfile_descriptor.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qfile_descriptor.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h
qfile_descriptor.o: qglobal_var.h
qfunctions.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qfunctions.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qfunctions.o: qversion.h
qhistogram.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qhistogram.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qimage_subset.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qimage_subset.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h
qimage_subset.o: qglobal_var.h
qimagefile.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qimagefile.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qinfo_widgets.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qinfo_widgets.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h
qinfo_widgets.o: qglobal_var.h
qleaderfile.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qleaderfile.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qleft.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qleft.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qmain.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qmain.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qmain.o: qversion.h
qmenubar.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qmenubar.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qpixmap.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qpixmap.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qright.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qright.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qset_labels.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qset_labels.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qstretch.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qstretch.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qunsigned_image.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qunsigned_image.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h
qunsigned_image.o: qglobal_var.h
qzoom.o: qinclude.h $(INCLUDEDIR)/asf_syslog.h qdefines.h
qzoom.o: $(CP_INCLUDEDIR)/qexit_stati.h qfunc_decl.h qglobal_var.h
qdefines.o: $(CP_INCLUDEDIR)/qexit_stati.h
qinclude.o: $(INCLUDEDIR)/asf_syslog.h
