#########################################################################
#
#	@(#)Makefile	1.20 96/10/09 17:33:35
#
#       MAKEFILE FOR STAND-ALONE XT CODE APPLICATION.  
#
#       EXECUTABLE      is the name of the executable to be created 
#       MAIN            is the .c file containing your main() function 
#       INTERFACES      is a list of the generated C code files
#       APP_OBJS        is a (possibly empty) list of the object code
#                       files that form the non-interface portion of
#                       your application
#
#       In the first three statements, the variables on the right 
#	of the equal sign will be replaced with their corresponding
#       values when the makefile is automatically generated.
#
#       This template is used for makefiles which do not reference 
#       the Ux runtime library.
#
# Usage:
#     make all        - Build executable
#     make app        - Same as "make all"
#     make install    - Install executable, man page, and app-defaults file
#     make bin_inst   - Same as "make install"
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
#########################################################################


EXECUTABLE	= CP_scan_qc
MAIN		= scan_qc.c
INTERFACES	= scan_qc_gui.c \
		reqHelpDialog.c \
		reqErrorDialog.c 

LANGUAGE        = ANSI C
APPL_OBJS       = UxXt.o UxMethod.o \
                  scan_qc_proc.o
SHELL		= /bin/sh

APPL_LIBS       = $(ASF)/lib/libsps.a
TOOLINC         = $(ASF)/include/sps
CP_INCLUDE_DIR  = $(ASF)/include/cp

#INSTALLDIR      = $(ASF)/bin
INSTALLDIR      = ../../bin/$(MACH)
#XAPPLOADDIR     = $(LOCAL)/app-defaults
XAPPLOADDIR     = ../../etc/app-defaults
CP_SCAN_QC_AD   = CP_Scan_qc

# change MAN_PATH to point to your man page top directory
#MAN_PATH        = $(ASF)/man
# change MAN_EXT to the appropriate section
MAN_EXT         = 1
MAN_SRC         = $(EXECUTABLE).man
MAN_PATH	= ../../etc/man

UX_DIR		= /usr/local/uimx
UX_LIBPATH	= $(UX_DIR)/lib
X_LIBS		= -lXm -lXt -lX11
UXCGEN          = $(UX_DIR)/bin/uxcgen

X_LIBPATH	=
MOTIF_LIBPATH	=
X_CFLAGS	=
MOTIF_CFLAGS	= -DMOTIF

KR_CC           = cc 
ANSI_CC         = cc 
CPLUS_CC        = CC
KR_CFLAGS       = -D_NO_PROTO -cckr
ANSI_CFLAGS     = 
CPLUS_CFLAGS    = 
CFLAGS          =  -DSYSV  -DXT_CODE \
                  -D_SGI_REENTRANT_FUNCTIONS \
		  $(X_CFLAGS) $(MOTIF_CFLAGS) \
		  -signed -G 0 \
                  -g -D__sgi \
                  -I$(CP_INCLUDE_DIR) \
                  -I./ -I$(TOOLINC) \
                  -D_ODL_RAW_IF_       

LIBPATH         = $(X_LIBPATH) $(MOTIF_LIBPATH)
LIBS            = $(APPL_LIBS) $(X_LIBS) -lc -lPW -lm
#LIBS            = $(APPL_LIBS) $(X_LIBS) -lsun -lbsd -lmld -lc -lPW -lm


OBJS = $(MAIN:.c=.o) $(INTERFACES:.c=.o) $(APPL_OBJS)

app:	all
all:	$(INSTALLDIR)/$(EXECUTABLE) $(XAPPLOADDIR)/$(CP_SCAN_QC_AD).ad


$(INSTALLDIR)/$(EXECUTABLE): $(EXECUTABLE)
	install -v -m 755 -F $(INSTALLDIR) $(EXECUTABLE)

$(XAPPLOADDIR)/$(CP_SCAN_QC_AD).ad: $(CP_SCAN_QC_AD)
	install -v -m 664 -src $(CP_SCAN_QC_AD) \
		-F $(XAPPLOADDIR) $(CP_SCAN_QC_AD).ad

$(EXECUTABLE): $(OBJS)   $(INTERFACES)	$(APPL_LIBS)
	@echo Linking    $(EXECUTABLE)
	$(LD) $(OBJS) $(LIBPATH) $(LIBS) -o $(EXECUTABLE)
	chmod 775 $(EXECUTABLE)

.SUFFIXES: .o .c .i

#.i.c:
#	$(UXCGEN) -x -h $<

.c.o:
	@echo Compiling $< [$(LANGUAGE)] [XT-CODE]
	$(CC) -c $(CFLAGS) $< -o $@
	-chmod 664 $@

CC = \
@`if [ "$(LANGUAGE)" = "C++" ]; then echo $(CPLUS_CC) $(CPLUS_CFLAGS);fi` \
`if [ "$(LANGUAGE)" = "ANSI C" ]; then echo $(ANSI_CC) $(ANSI_CFLAGS); fi`\
`if [ "$(LANGUAGE)" = "KR-C" ]; then echo $(KR_CC) $(KR_CFLAGS); fi`



LD = \
@`if [ "$(LANGUAGE)" = "C++" ]; then echo $(CPLUS_CC);fi` \
`if [ "$(LANGUAGE)" = "ANSI C" ]; then echo $(ANSI_CC); fi`\
`if [ "$(LANGUAGE)" = "KR-C" ]; then echo $(KR_CC); fi`

clean:
	-/bin/rm $(OBJS) $(EXECUTABLE)

install.man::
	install -v -m 664 -src $(MAN_SRC) \
		-F $(MAN_PATH)/man$(MAN_EXT) $(EXECUTABLE).$(MAN_EXT)

# DO NOT DELETE THIS LINE -- make depend depends on it.

reqErrorDialog.o: UxXt.h scan_qc_def.h reqErrorDialog.h
reqHelpDialog.o: UxXt.h scan_qc_def.h reqHelpDialog.h
scan_qc_gui.o: UxXt.h scan_qc_def.h scan_qc_gui.h
scan_qc_proc.o: reqErrorDialog.h reqHelpDialog.h scan_qc_gui.h scan_qc_def.h
scan_qc_proc.o: version.h
