###############################################################################
#
# File:         Makefile
#
# Function:     Baseline Version of APS Makefile
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.Makefile"
#
###############################################################################

ifdef CM_HOME
	APS_HOME = $(CM_HOME)/aps
endif

include $(APS_HOME)/src/aps_makedefs


###############################################################################
#
# Source Default Directory
#   Typically off of $APS_HOME/src... this is used to retrieve source
#
###############################################################################
 
SRC_DIR = $(APS_HOME)/src/gui
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL = 		$(IAPS) \
			$(IPPS) \
			$(IFAIF) \
			$(IXMOTIF) \
			$(ISYBASE) \
			$(IODL) \
			$(IIMS) \
			$(ISYS) \
			$(OSIFLAGS) 

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################

LALL =      -L./ $(LASF) $(LAPS) \
			$(LFAIF) $(LPPS) \
			$(LXMOTIF) \
			$(LSYBASE) \
			$(OSLFLAGS) \
			$($(OS)LFLAGS)

RALL = $(foreach Llib, $(LALL), $(subst -L, -R, $(Llib)))

###############################################################################
#
# Library Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLIBS
#    If a line has two libraries the first is dependent on the second
#
#    The STATIC and DYNAMIC macro can be used to set the link option for
#    the libraries that follow them
#    The 3rd Party and system libs are defaulted to link dynamically 
#    (see aps_makedefs)
#    The MATH_LIB for some reason works best when it's last
#
###############################################################################

LIBS = \
	$(EVERY_LIB) \
	$(XMOTIF_LIB) \
	$(SE_XLAT_ASF_WALPS_LIB) \
	$(FAIF_XLAT_ASF_WALPS_LIB) \
	$(FAIF_XLAT_WOS_LIB) \
	$(FAIF_XLAT_SV_LIB) \
	$(FAIF_FASF_LIB) \
	$(FAIF_UTIL_LIB) \
	$(STATIC) \
	$(IMS_LIB) \
	$(DYNAMIC) \
	$(COMMON_ODL_LIB)

###############################################################################
#
# Flags and Defines
#
###############################################################################

UIMX_DEFS = -DSOLARIS -DMOTIF
DEFS =      -D$(MACH) -DXT_CODE -DAPS_GUI -DAPS_GUI_SKIP $(UIMX_DEFS)
CFLAGS =    $(OSCFLAGS) $(DEFS)

###############################################################################
#
# Source and Target Definition
#   Add your source and exe filenames here
#       EXE      is the name of the executable to be created
#       LIB      is the name of the library to be created
#       SRC      is a list of the C source code files
#       HEADER_FILES is a list of the C Header files
#       F77_SRC  is a list (possibly empty) of the Fortran 77 source
#                code files
#       F77_INC  is a list (possibly empty) of the Fortran 77 include files
#
#
###############################################################################

APS_EXE				:=	aps_gui
GET_PRC_FILE_EXE	:=	aps_get_prcapsfile_main
DO_PRC_FILE_EXE		:=	aps_do_prcapsfile_main
EXE          		:=	$(APS_EXE) $(GET_PRC_FILE_EXE) $(DO_PRC_FILE_EXE)

LIB          =  

APS_MAIN			=	aps.c
GET_PRC_FILE_MAIN	=	get_prcapsfile_main.c
DO_PRC_FILE_MAIN	=	do_prcapsfile_main.c

VC_INTERFACE_FILES = \
	APSinterface.prj \
	APSFileGeneration.i \
	APSFileProcessing.i \
	APSFileSelection.i \
	APSLoginForm.i \
	APSMainMenu.i \
	APSMsgBoxDlg.i \
	APSPhaseSelection.i \
	APSWOSCompare.i \
	MUIntervalDialog.i \
	MUPermissionStatus.i \
	RGSDownTimes.i \
	AntennaDownTimes.i \
	ConRoundupForm.i \
	CreateDatatakeOpps.i \
	CreateNominalCoverage.i \
	CreateNominalOrbit.i \
	DARManager.i \
	DTKManager.i \
	FileViewer.i \
	SearchForm.i \
	SortForm.i

INTERFACES	= 	\
	vc_antdntimes.c \
	vc_apslogin.c \
	vc_apsmenu.c \
	vc_cdtakeopps.c \
	vc_cnomcov.c \
	vc_cnomorb.c \
	vc_conrndup.c \
	vc_crtapsfile.c \
	vc_darmanager.c \
	vc_dtkmanager.c \
	vc_fileselectionbox.c \
	vc_fileviewer.c \
	vc_msgbox.c \
	vc_muinterval.c \
	vc_permstatus.c \
	vc_phaseselect.c \
	vc_prcapsfile.c \
	vc_rgsdntimes.c \
	vc_searchform.c \
	vc_sortform.c \
	vc_apswoscompare.c

INTERFACES_HEADERS  = $(INTERFACES:%.c=%.h)

APPL_SRC       = \
	UxMethod.c \
	UxXt.c \
	apsfiledef.c \
	apsfilefunc.c \
	apspath_func.c \
	aps_global.c \
	aps_imsDB.c \
	cb_antdntimes.c \
	cb_apslogin.c \
	cb_apsmenu.c \
	cb_apswoscompare.c \
	cb_cdtakeopps.c \
	cb_cnomcov.c \
	cb_cnomorb.c \
	cb_conrndup.c \
	cb_crtapsfile.c \
	cb_darmanager.c \
	cb_datetime.c \
	cb_dtkmanager.c \
	cb_fileviewer.c \
	cb_permstatus.c \
	cb_prcapsfile.c \
	cb_rgsdntimes.c \
	cb_searchform.c \
	cb_sortform.c \
	gui_mu_utils.c \
	gui_utils.c \
	reqq_phase.c \
	satmenus.c \
	sscvcheckq.c \
	subprocess.c

PRC_FILE_MAIN_SRC	= \
	prcapsfile_main.c

SRC					:=  $(APS_MAIN) \
						$(GET_PRC_FILE_MAIN) \
						$(DO_PRC_FILE_MAIN) \
						$(PRC_FILE_MAIN_SRC) \
						$(APPL_SRC) $(INTERFACES)
APS_SRC				:=	$(APS_MAIN) $(APPL_SRC) $(INTERFACES)
GET_PRC_FILE_SRC	:=	$(GET_PRC_FILE_MAIN) \
						$(PRC_FILE_MAIN_SRC) \
						apspath_func.c apsfiledef.c
DO_PRC_FILE_SRC		:=	$(DO_PRC_FILE_MAIN) \
						$(PRC_FILE_MAIN_SRC) \
						apsfiledef.c

HEADER_FILES =  \
	$(INTERFACES_HEADERS) \
	UxXt.h \
	aps_imsDB.h \
	aps_exe_names.h \
	cb_apslogin.h \
	cb_antdntimes.h \
	cb_apsmenu.h \
	cb_apswoscompare.h \
	cb_cdtakeopps.h \
	cb_cnomcov.h \
	cb_cnomorb.h \
	cb_conrndup.h \
	cb_crtapsfile.h \
	cb_darmanager.h \
	cb_datetime.h \
	cb_dtkmanager.h \
	cb_fileviewer.h \
	cb_permstatus.h \
	cb_prcapsfile.h \
	cb_rgsdntimes.h \
	cb_searchform.h \
	cb_sortform.h \
	gui_defs.h    \
	gui_mu_utils.h \
	gui_utils.h    \
	prcapsfile_main.h \
	reqq_phase.h     \
	satmenus.h       \
	subprocess.h

F77_SRC      =  

F77_INC      =  

OBJS         		=	$(SRC:%.c=%.o) $(F77_SRC:%.for=%.o)
APS_OBJS			:=	$(APS_SRC:%.c=%.o)
GET_PRC_FILE_OBJS	:=	$(patsubst %.c,%.o, \
							$(filter-out apsfiledef.c, $(GET_PRC_FILE_SRC))) \
						$(patsubst %.c,%.o, \
							$(addprefix prcapsfile_main_, \
							apsfiledef.c))
DO_PRC_FILE_OBJS	:=	$(patsubst %.c,%.o, \
							$(filter-out apsfiledef.c, $(DO_PRC_FILE_SRC))) \
						$(patsubst %.c,%.o, \
							$(addprefix prcapsfile_main_, \
							apsfiledef.c))

TARGETS = $(EXE) $(LIB)
 

###############################################################################
#
# Make the Application
#    Add all targets to be made to the next line
#    If more exes or libs are needed make a copy of the set of 
#    needed commands
#
###############################################################################

all appl:   $(TARGETS)

$(APS_EXE)			:	$(APS_OBJS)
$(GET_PRC_FILE_EXE)	:	$(GET_PRC_FILE_OBJS)
$(DO_PRC_FILE_EXE)	:	$(DO_PRC_FILE_OBJS)
$(EXE):
	@echo
	@echo "Linking    $@"
	@$(CC) -o $@ $^ $(LALL) $(RALL) $(LIBS)
	@echo "Done"
 
$(LIB): $(OBJS)
	@echo
	@echo "Creating/Modifying Library $(LIB)"
	@$(ARCHIVE) rucv $(LIB) $?
	@if [ $(OS) = sunos ]; then ranlib $(LIB); fi
	@echo "Done"


###############################################################################
#
# Source Code Retrieval
#
###############################################################################

$(SRC) $(HEADER_FILES) $(F77_SRC) $(F77_INC) :
	@echo
	@echo "Retrieving $@ as of $(FILE_VERSION) "
	$(CM) $(CM_GET) $(FILE_VERSION) $(SRC_DIR)/$@

$(VC_INTERFACE_FILES) :
	@echo
	@echo "Retrieving $@ as of $(FILE_VERSION) "
	$(CM) $(CM_GET) -k $(FILE_VERSION) $(SRC_DIR)/$@
	chmod ug+w $@

 
###############################################################################
#
# C Compilation.
#
###############################################################################

%.o:%.c
	@echo
	@echo "Compiling $<"
	@$(CC) $(CFLAGS) $(IALL) -c $<

prcapsfile_main_%.o : %.c
	@echo
	@echo "Compiling $<"
	@$(CC) $(filter-out -DAPS_GUI_SKIP, $(CFLAGS)) $(IALL) -o $@ -c $<


###############################################################################
#
# Fortran Compilation.
#
###############################################################################

%.o:%.for $(F77_INC)
	@echo
	@echo "Compiling $<"
	$(F77) $<


###############################################################################
#
# Object and executable cleaning.
#
###############################################################################

clean:
	@echo
	@echo "Cleaning " $(TARGETS)
	rm -f $(OBJS) $(TARGETS) prcapsfile_main_*.o
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)


###############################################################################
#
# Definitions
#  Add more definitions as needed via the aps_post_makedefs file
#
###############################################################################

include $(APS_HOME)/src/aps_post_makedefs


###############################################################################
#
# Makedepend (create all the include file dependencies
#	(destroys everything after the "DO NOT DELETE ..." line)
#
###############################################################################
depend: $(SRC) $(F77_SRC)
	@if [ -w Makefile ] ; \
	then \
		echo makedepend -- $(CFLAGS) -- \
			$(IALL) -I$(APS_HOME)/src/makedepend_files $^; \
		makedepend -- $(CFLAGS) -- \
			$(IALL) -I$(APS_HOME)/src/makedepend_files $^; \
		sed -n '/^# DO NOT DELETE/,$$ p' Makefile | grep '^apsfiledef.o' | \
		sed 's/apsfiledef.o/prcapsfile_main_apsfiledef.o/' >> Makefile ; \
		echo rm -f Makefile.bak ; \
		rm -f Makefile.bak ; \
	else \
		echo "*** Unwriteable Makefile: Need to check it out? ***" ; \
	fi

# DO NOT DELETE THIS LINE -- make depend depends on it.

aps.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
aps.o: /usr/include/stdlib.h /usr/include/string.h
aps.o: /usr/openwin/include/X11/Intrinsic.h /usr/openwin/include/X11/Xlib.h
aps.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
aps.o: /usr/include/sys/machtypes.h /usr/openwin/include/X11/X.h
aps.o: /usr/openwin/include/X11/Xfuncproto.h
aps.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
aps.o: /usr/openwin/include/X11/Xutil.h /usr/openwin/include/X11/Xresource.h
aps.o: /usr/openwin/include/X11/Core.h /usr/openwin/include/X11/Composite.h
aps.o: /usr/openwin/include/X11/Constraint.h
aps.o: /usr/openwin/include/X11/Object.h /usr/openwin/include/X11/RectObj.h
aps.o: /usr/openwin/include/X11/StringDefs.h /usr/dt/include/Xm/FileSB.h
aps.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Shell.h
aps.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
aps.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
aps.o: /home/aps/r2.1.1/include/local/aps.h
aps.o: /home/aps/r2.1.1/include/local/aps_defs.h
aps.o: /home/aps/r2.1.1/include/local/aps_extern.h
aps.o: /home/aps/r2.1.1/include/global/db_sybint.h
aps.o: /home/sybase/syb11/include/sybfront.h
aps.o: /home/sybase/syb11/include/cstypes.h
aps.o: /home/sybase/syb11/include/csconfig.h
aps.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
aps.o: /home/sybase/syb11/include/sybdb.h
aps.o: /home/sybase/syb11/include/syblogin.h
aps.o: /home/sybase/syb11/include/syberror.h
aps.o: /home/aps/r2.1.1/include/global/dapps_list.h
aps.o: /home/aps/r2.1.1/include/global/llist_err.h
aps.o: /home/aps/r2.1.1/include/global/llist.h
aps.o: /home/aps/r2.1.1/include/global/ord_llist.h gui_utils.h UxXt.h
get_prcapsfile_main.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
get_prcapsfile_main.o: /usr/include/stdlib.h /usr/include/unistd.h
get_prcapsfile_main.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
get_prcapsfile_main.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
get_prcapsfile_main.o: /usr/include/errno.h /usr/include/sys/errno.h
get_prcapsfile_main.o: /usr/include/signal.h /usr/include/sys/signal.h
get_prcapsfile_main.o: /usr/include/string.h /usr/include/fcntl.h
get_prcapsfile_main.o: /usr/include/sys/fcntl.h /usr/include/sys/wait.h
get_prcapsfile_main.o: /usr/dt/include/Xm/Xm.h
get_prcapsfile_main.o: /usr/openwin/include/X11/Intrinsic.h
get_prcapsfile_main.o: /usr/openwin/include/X11/Xlib.h
get_prcapsfile_main.o: /usr/openwin/include/X11/X.h
get_prcapsfile_main.o: /usr/openwin/include/X11/Xfuncproto.h
get_prcapsfile_main.o: /usr/openwin/include/X11/Xosdefs.h
get_prcapsfile_main.o: /usr/include/stddef.h /usr/openwin/include/X11/Xutil.h
get_prcapsfile_main.o: /usr/openwin/include/X11/Xresource.h
get_prcapsfile_main.o: /usr/openwin/include/X11/Core.h
get_prcapsfile_main.o: /usr/openwin/include/X11/Composite.h
get_prcapsfile_main.o: /usr/openwin/include/X11/Constraint.h
get_prcapsfile_main.o: /usr/openwin/include/X11/Object.h
get_prcapsfile_main.o: /usr/openwin/include/X11/RectObj.h
get_prcapsfile_main.o: /usr/openwin/include/X11/Shell.h
get_prcapsfile_main.o: /usr/openwin/include/X11/Xatom.h
get_prcapsfile_main.o: /usr/dt/include/Xm/XmStrDefs.h
get_prcapsfile_main.o: /usr/openwin/include/X11/StringDefs.h
get_prcapsfile_main.o: /usr/dt/include/Xm/VirtKeys.h
get_prcapsfile_main.o: /usr/dt/include/Xm/VendorS.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/global/dapps_defs.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/local/aps.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/local/aps_defs.h
get_prcapsfile_main.o: aps_exe_names.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/local/aps_extern.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_prcapsfile_main.o: /home/sybase/syb11/include/sybfront.h
get_prcapsfile_main.o: /home/sybase/syb11/include/cstypes.h
get_prcapsfile_main.o: /home/sybase/syb11/include/csconfig.h
get_prcapsfile_main.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_prcapsfile_main.o: /home/sybase/syb11/include/sybdb.h
get_prcapsfile_main.o: /home/sybase/syb11/include/syblogin.h
get_prcapsfile_main.o: /home/sybase/syb11/include/syberror.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/global/llist_err.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/global/llist.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/local/mu_utilities.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/local/apspath.h
get_prcapsfile_main.o: /home/aps/r2.1.1/include/local/apsfiledef.h
get_prcapsfile_main.o: subprocess.h gui_defs.h gui_utils.h prcapsfile_main.h
do_prcapsfile_main.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
do_prcapsfile_main.o: /usr/include/stdlib.h /usr/include/unistd.h
do_prcapsfile_main.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
do_prcapsfile_main.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
do_prcapsfile_main.o: /usr/include/signal.h /usr/include/sys/signal.h
do_prcapsfile_main.o: /usr/include/errno.h /usr/include/sys/errno.h
do_prcapsfile_main.o: /usr/include/string.h /usr/include/fcntl.h
do_prcapsfile_main.o: /usr/include/sys/fcntl.h /usr/include/sys/wait.h
do_prcapsfile_main.o: /usr/openwin/include/X11/Intrinsic.h
do_prcapsfile_main.o: /usr/openwin/include/X11/Xlib.h
do_prcapsfile_main.o: /usr/openwin/include/X11/X.h
do_prcapsfile_main.o: /usr/openwin/include/X11/Xfuncproto.h
do_prcapsfile_main.o: /usr/openwin/include/X11/Xosdefs.h
do_prcapsfile_main.o: /usr/include/stddef.h /usr/openwin/include/X11/Xutil.h
do_prcapsfile_main.o: /usr/openwin/include/X11/Xresource.h
do_prcapsfile_main.o: /usr/openwin/include/X11/Core.h
do_prcapsfile_main.o: /usr/openwin/include/X11/Composite.h
do_prcapsfile_main.o: /usr/openwin/include/X11/Constraint.h
do_prcapsfile_main.o: /usr/openwin/include/X11/Object.h
do_prcapsfile_main.o: /usr/openwin/include/X11/RectObj.h
do_prcapsfile_main.o: /home/aps/r2.1.1/include/global/dapps_defs.h
do_prcapsfile_main.o: /home/aps/r2.1.1/include/local/aps.h
do_prcapsfile_main.o: /home/aps/r2.1.1/include/local/aps_defs.h
do_prcapsfile_main.o: aps_exe_names.h
do_prcapsfile_main.o: /home/aps/r2.1.1/include/local/mu_utilities.h
do_prcapsfile_main.o: /home/aps/r2.1.1/include/global/db_sybint.h
do_prcapsfile_main.o: /home/sybase/syb11/include/sybfront.h
do_prcapsfile_main.o: /home/sybase/syb11/include/cstypes.h
do_prcapsfile_main.o: /home/sybase/syb11/include/csconfig.h
do_prcapsfile_main.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
do_prcapsfile_main.o: /home/sybase/syb11/include/sybdb.h
do_prcapsfile_main.o: /home/sybase/syb11/include/syblogin.h
do_prcapsfile_main.o: /home/sybase/syb11/include/syberror.h
do_prcapsfile_main.o: /home/aps/r2.1.1/include/global/dapps_list.h
do_prcapsfile_main.o: /home/aps/r2.1.1/include/global/llist_err.h
do_prcapsfile_main.o: /home/aps/r2.1.1/include/global/llist.h
do_prcapsfile_main.o: /home/aps/r2.1.1/include/global/ord_llist.h
do_prcapsfile_main.o: /home/aps/r2.1.1/include/local/apspath.h
do_prcapsfile_main.o: /home/aps/r2.1.1/include/local/apsfiledef.h
do_prcapsfile_main.o: subprocess.h gui_defs.h prcapsfile_main.h
prcapsfile_main.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
prcapsfile_main.o: /usr/include/stdlib.h /usr/include/unistd.h
prcapsfile_main.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
prcapsfile_main.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
prcapsfile_main.o: /usr/include/string.h /usr/include/ctype.h
prcapsfile_main.o: /home/aps/r2.1.1/include/global/dapps_defs.h
prcapsfile_main.o: /home/aps/r2.1.1/include/local/aps_defs.h
prcapsfile_main.o: prcapsfile_main.h
prcapsfile_main.o: /home/aps/r2.1.1/include/local/apsfiledef.h
UxMethod.o: UxXt.h /usr/include/stdlib.h /usr/include/sys/feature_tests.h
UxMethod.o: /usr/include/string.h /usr/dt/include/Xm/Xm.h
UxMethod.o: /usr/openwin/include/X11/Intrinsic.h
UxMethod.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
UxMethod.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
UxMethod.o: /usr/openwin/include/X11/X.h
UxMethod.o: /usr/openwin/include/X11/Xfuncproto.h
UxMethod.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
UxMethod.o: /usr/openwin/include/X11/Xutil.h
UxMethod.o: /usr/openwin/include/X11/Xresource.h
UxMethod.o: /usr/openwin/include/X11/Core.h
UxMethod.o: /usr/openwin/include/X11/Composite.h
UxMethod.o: /usr/openwin/include/X11/Constraint.h
UxMethod.o: /usr/openwin/include/X11/Object.h
UxMethod.o: /usr/openwin/include/X11/RectObj.h
UxMethod.o: /usr/openwin/include/X11/Shell.h /usr/openwin/include/X11/Xatom.h
UxMethod.o: /usr/dt/include/Xm/XmStrDefs.h
UxMethod.o: /usr/openwin/include/X11/StringDefs.h
UxMethod.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
UxXt.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
UxXt.o: /usr/include/stdlib.h /usr/openwin/include/X11/Intrinsic.h
UxXt.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
UxXt.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
UxXt.o: /usr/openwin/include/X11/X.h /usr/openwin/include/X11/Xfuncproto.h
UxXt.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
UxXt.o: /usr/openwin/include/X11/Xutil.h /usr/openwin/include/X11/Xresource.h
UxXt.o: /usr/include/string.h /usr/openwin/include/X11/Core.h
UxXt.o: /usr/openwin/include/X11/Composite.h
UxXt.o: /usr/openwin/include/X11/Constraint.h
UxXt.o: /usr/openwin/include/X11/Object.h /usr/openwin/include/X11/RectObj.h
UxXt.o: /usr/openwin/include/X11/Shell.h
UxXt.o: /usr/openwin/include/X11/StringDefs.h /usr/openwin/include/X11/Xmd.h
UxXt.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Xatom.h
UxXt.o: /usr/dt/include/Xm/XmStrDefs.h /usr/dt/include/Xm/VirtKeys.h
UxXt.o: /usr/dt/include/Xm/VendorS.h /usr/dt/include/Xm/DialogS.h UxXt.h
apsfiledef.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
apsfiledef.o: /usr/include/string.h /usr/dt/include/Xm/Xm.h
apsfiledef.o: /usr/openwin/include/X11/Intrinsic.h
apsfiledef.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
apsfiledef.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
apsfiledef.o: /usr/openwin/include/X11/X.h
apsfiledef.o: /usr/openwin/include/X11/Xfuncproto.h
apsfiledef.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
apsfiledef.o: /usr/openwin/include/X11/Xutil.h
apsfiledef.o: /usr/openwin/include/X11/Xresource.h
apsfiledef.o: /usr/openwin/include/X11/Core.h
apsfiledef.o: /usr/openwin/include/X11/Composite.h
apsfiledef.o: /usr/openwin/include/X11/Constraint.h
apsfiledef.o: /usr/openwin/include/X11/Object.h
apsfiledef.o: /usr/openwin/include/X11/RectObj.h
apsfiledef.o: /usr/openwin/include/X11/Shell.h
apsfiledef.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
apsfiledef.o: /usr/openwin/include/X11/StringDefs.h
apsfiledef.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
apsfiledef.o: aps_exe_names.h /home/aps/r2.1.1/include/local/apspath.h
apsfiledef.o: /home/aps/r2.1.1/include/local/mu_utilities.h
apsfiledef.o: /home/aps/r2.1.1/include/global/db_sybint.h
apsfiledef.o: /home/sybase/syb11/include/sybfront.h
apsfiledef.o: /home/sybase/syb11/include/cstypes.h
apsfiledef.o: /home/sybase/syb11/include/csconfig.h
apsfiledef.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
apsfiledef.o: /home/sybase/syb11/include/sybdb.h
apsfiledef.o: /home/sybase/syb11/include/syblogin.h
apsfiledef.o: /home/sybase/syb11/include/syberror.h
apsfiledef.o: /home/aps/r2.1.1/include/global/dapps_list.h
apsfiledef.o: /home/aps/r2.1.1/include/global/llist_err.h
apsfiledef.o: /home/aps/r2.1.1/include/global/llist.h
apsfiledef.o: /home/aps/r2.1.1/include/global/ord_llist.h
apsfiledef.o: /home/aps/r2.1.1/include/local/apsfiledef.h
apsfilefunc.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
apsfilefunc.o: /usr/include/string.h /usr/dt/include/Xm/Xm.h
apsfilefunc.o: /usr/openwin/include/X11/Intrinsic.h
apsfilefunc.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
apsfilefunc.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
apsfilefunc.o: /usr/openwin/include/X11/X.h
apsfilefunc.o: /usr/openwin/include/X11/Xfuncproto.h
apsfilefunc.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
apsfilefunc.o: /usr/openwin/include/X11/Xutil.h
apsfilefunc.o: /usr/openwin/include/X11/Xresource.h
apsfilefunc.o: /usr/openwin/include/X11/Core.h
apsfilefunc.o: /usr/openwin/include/X11/Composite.h
apsfilefunc.o: /usr/openwin/include/X11/Constraint.h
apsfilefunc.o: /usr/openwin/include/X11/Object.h
apsfilefunc.o: /usr/openwin/include/X11/RectObj.h
apsfilefunc.o: /usr/openwin/include/X11/Shell.h
apsfilefunc.o: /usr/openwin/include/X11/Xatom.h
apsfilefunc.o: /usr/dt/include/Xm/XmStrDefs.h
apsfilefunc.o: /usr/openwin/include/X11/StringDefs.h
apsfilefunc.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
apsfilefunc.o: /home/aps/r2.1.1/include/local/timeconv.h
apsfilefunc.o: /home/aps/r2.1.1/include/local/apsfiledef.h gui_utils.h
apspath_func.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
apspath_func.o: /usr/include/stdlib.h /usr/include/string.h
apspath_func.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
apspath_func.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
apspath_func.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
apspath_func.o: /usr/openwin/include/X11/X.h
apspath_func.o: /usr/openwin/include/X11/Xfuncproto.h
apspath_func.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
apspath_func.o: /usr/openwin/include/X11/Xutil.h
apspath_func.o: /usr/openwin/include/X11/Xresource.h
apspath_func.o: /usr/openwin/include/X11/Core.h
apspath_func.o: /usr/openwin/include/X11/Composite.h
apspath_func.o: /usr/openwin/include/X11/Constraint.h
apspath_func.o: /usr/openwin/include/X11/Object.h
apspath_func.o: /usr/openwin/include/X11/RectObj.h
apspath_func.o: /usr/openwin/include/X11/Shell.h
apspath_func.o: /usr/openwin/include/X11/Xatom.h
apspath_func.o: /usr/dt/include/Xm/XmStrDefs.h
apspath_func.o: /usr/openwin/include/X11/StringDefs.h
apspath_func.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
apspath_func.o: /home/aps/r2.1.1/include/global/dapps_defs.h
aps_global.o: /home/aps/r2.1.1/include/global/db_sybint.h
aps_global.o: /home/sybase/syb11/include/sybfront.h
aps_global.o: /home/sybase/syb11/include/cstypes.h
aps_global.o: /home/sybase/syb11/include/csconfig.h
aps_global.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
aps_global.o: /home/sybase/syb11/include/sybdb.h /usr/include/stdio.h
aps_global.o: /usr/include/sys/feature_tests.h
aps_global.o: /home/sybase/syb11/include/syblogin.h
aps_global.o: /home/sybase/syb11/include/syberror.h
aps_global.o: /home/aps/r2.1.1/include/global/dapps_list.h
aps_global.o: /home/aps/r2.1.1/include/global/llist_err.h
aps_global.o: /home/aps/r2.1.1/include/global/llist.h
aps_global.o: /home/aps/r2.1.1/include/global/ord_llist.h
aps_imsDB.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
aps_imsDB.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
aps_imsDB.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
aps_imsDB.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
aps_imsDB.o: /usr/openwin/include/X11/X.h
aps_imsDB.o: /usr/openwin/include/X11/Xfuncproto.h
aps_imsDB.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
aps_imsDB.o: /usr/openwin/include/X11/Xutil.h
aps_imsDB.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
aps_imsDB.o: /usr/openwin/include/X11/Core.h
aps_imsDB.o: /usr/openwin/include/X11/Composite.h
aps_imsDB.o: /usr/openwin/include/X11/Constraint.h
aps_imsDB.o: /usr/openwin/include/X11/Object.h
aps_imsDB.o: /usr/openwin/include/X11/RectObj.h
aps_imsDB.o: /usr/openwin/include/X11/Shell.h
aps_imsDB.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
aps_imsDB.o: /usr/openwin/include/X11/StringDefs.h
aps_imsDB.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
aps_imsDB.o: /home/aps/r2.1.1/include/local/DARconversions.h
aps_imsDB.o: /home/aps/r2.1.1/include/global/fa_defs.h
aps_imsDB.o: /home/aps/r2.1.1/include/global/dapps_list.h
aps_imsDB.o: /home/aps/r2.1.1/include/global/llist_err.h
aps_imsDB.o: /home/aps/r2.1.1/include/global/llist.h
aps_imsDB.o: /home/aps/r2.1.1/include/global/ord_llist.h
aps_imsDB.o: /home/aps/r2.1.1/include/local/aps_defs.h
aps_imsDB.o: /home/aps/r2.1.1/include/local/aps_extern.h
aps_imsDB.o: /home/aps/r2.1.1/include/global/db_sybint.h
aps_imsDB.o: /home/sybase/syb11/include/sybfront.h
aps_imsDB.o: /home/sybase/syb11/include/cstypes.h
aps_imsDB.o: /home/sybase/syb11/include/csconfig.h
aps_imsDB.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
aps_imsDB.o: /home/sybase/syb11/include/sybdb.h /usr/include/stdio.h
aps_imsDB.o: /home/sybase/syb11/include/syblogin.h
aps_imsDB.o: /home/sybase/syb11/include/syberror.h aps_imsDB.h
aps_imsDB.o: /usr/include/errno.h /usr/include/sys/errno.h
aps_imsDB.o: /usr/include/ctype.h /usr/include/sys/utsname.h
aps_imsDB.o: /usr/include/syslog.h /usr/include/sys/syslog.h
aps_imsDB.o: /usr/include/unistd.h /usr/include/sys/unistd.h
aps_imsDB.o: /ASF/PRECM/include/imsdads/ims_query.h
aps_imsDB.o: /ASF/PRECM/include/imsdads/ims_dbms.h
aps_imsDB.o: /home/sybase/syb11/include/ctpublic.h
aps_imsDB.o: /home/sybase/syb11/include/cspublic.h
aps_imsDB.o: /home/sybase/syb11/include/sqlca.h
aps_imsDB.o: /home/sybase/syb11/include/sqlda.h
aps_imsDB.o: /ASF/PRECM/include/imsdads/ims_const.h
aps_imsDB.o: /ASF/PRECM/include/imsdads/ims_msg.h
aps_imsDB.o: /ASF/PRECM/include/imsdads/ims_qi.h
aps_imsDB.o: /ASF/PRECM/include/imsdads/ims_getInput.h
aps_imsDB.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
aps_imsDB.o: /ASF/PRECM/include/imsdads/ims_util.h
cb_antdntimes.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_antdntimes.o: /usr/include/unistd.h /usr/include/sys/types.h
cb_antdntimes.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_antdntimes.o: /usr/include/sys/unistd.h /usr/include/stdlib.h
cb_antdntimes.o: /usr/include/time.h /usr/dt/include/Xm/List.h
cb_antdntimes.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
cb_antdntimes.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
cb_antdntimes.o: /usr/openwin/include/X11/Xfuncproto.h
cb_antdntimes.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_antdntimes.o: /usr/openwin/include/X11/Xutil.h
cb_antdntimes.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
cb_antdntimes.o: /usr/openwin/include/X11/Core.h
cb_antdntimes.o: /usr/openwin/include/X11/Composite.h
cb_antdntimes.o: /usr/openwin/include/X11/Constraint.h
cb_antdntimes.o: /usr/openwin/include/X11/Object.h
cb_antdntimes.o: /usr/openwin/include/X11/RectObj.h
cb_antdntimes.o: /usr/openwin/include/X11/Shell.h
cb_antdntimes.o: /usr/openwin/include/X11/Xatom.h
cb_antdntimes.o: /usr/dt/include/Xm/XmStrDefs.h
cb_antdntimes.o: /usr/openwin/include/X11/StringDefs.h
cb_antdntimes.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_antdntimes.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/TextF.h
cb_antdntimes.o: vc_antdntimes.h /usr/dt/include/Xm/MwmUtil.h
cb_antdntimes.o: /usr/openwin/include/X11/Xmd.h
cb_antdntimes.o: /usr/dt/include/Xm/MenuShell.h UxXt.h
cb_antdntimes.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_antdntimes.o: /home/aps/r2.1.1/include/local/aps_defs.h aps_exe_names.h
cb_antdntimes.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_antdntimes.o: /home/sybase/syb11/include/sybfront.h
cb_antdntimes.o: /home/sybase/syb11/include/cstypes.h
cb_antdntimes.o: /home/sybase/syb11/include/csconfig.h
cb_antdntimes.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_antdntimes.o: /home/sybase/syb11/include/sybdb.h
cb_antdntimes.o: /home/sybase/syb11/include/syblogin.h
cb_antdntimes.o: /home/sybase/syb11/include/syberror.h
cb_antdntimes.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_antdntimes.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_antdntimes.o: /home/aps/r2.1.1/include/global/llist.h
cb_antdntimes.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_antdntimes.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_antdntimes.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_antdntimes.o: /home/aps/r2.1.1/include/local/db_antenna_down_times.h
cb_antdntimes.o: /home/aps/r2.1.1/include/local/aps_extern.h
cb_antdntimes.o: /home/aps/r2.1.1/include/global/nmalloc.h satmenus.h
cb_antdntimes.o: gui_utils.h gui_mu_utils.h
cb_antdntimes.o: /home/aps/r2.1.1/include/local/mu_utilities.h subprocess.h
cb_antdntimes.o: cb_antdntimes.h cb_datetime.h
cb_antdntimes.o: /home/aps/r2.1.1/include/local/timeconv.h
cb_apslogin.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_apslogin.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
cb_apslogin.o: /usr/include/sys/machtypes.h /usr/include/unistd.h
cb_apslogin.o: /usr/include/sys/unistd.h /usr/include/stdlib.h
cb_apslogin.o: /usr/include/string.h /usr/include/pwd.h
cb_apslogin.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
cb_apslogin.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
cb_apslogin.o: /usr/openwin/include/X11/Xfuncproto.h
cb_apslogin.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_apslogin.o: /usr/openwin/include/X11/Xutil.h
cb_apslogin.o: /usr/openwin/include/X11/Xresource.h
cb_apslogin.o: /usr/openwin/include/X11/Core.h
cb_apslogin.o: /usr/openwin/include/X11/Composite.h
cb_apslogin.o: /usr/openwin/include/X11/Constraint.h
cb_apslogin.o: /usr/openwin/include/X11/Object.h
cb_apslogin.o: /usr/openwin/include/X11/RectObj.h
cb_apslogin.o: /usr/openwin/include/X11/Shell.h
cb_apslogin.o: /usr/openwin/include/X11/Xatom.h
cb_apslogin.o: /usr/dt/include/Xm/XmStrDefs.h
cb_apslogin.o: /usr/openwin/include/X11/StringDefs.h
cb_apslogin.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_apslogin.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/TextF.h UxXt.h
cb_apslogin.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_apslogin.o: /home/aps/r2.1.1/include/local/aps_defs.h
cb_apslogin.o: /home/aps/r2.1.1/include/local/aps_extern.h
cb_apslogin.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_apslogin.o: /home/sybase/syb11/include/sybfront.h
cb_apslogin.o: /home/sybase/syb11/include/cstypes.h
cb_apslogin.o: /home/sybase/syb11/include/csconfig.h
cb_apslogin.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_apslogin.o: /home/sybase/syb11/include/sybdb.h
cb_apslogin.o: /home/sybase/syb11/include/syblogin.h
cb_apslogin.o: /home/sybase/syb11/include/syberror.h
cb_apslogin.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_apslogin.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_apslogin.o: /home/aps/r2.1.1/include/global/llist.h
cb_apslogin.o: /home/aps/r2.1.1/include/global/ord_llist.h gui_utils.h
cb_apslogin.o: vc_apslogin.h /usr/dt/include/Xm/MwmUtil.h
cb_apslogin.o: /usr/openwin/include/X11/Xmd.h /usr/dt/include/Xm/MenuShell.h
cb_apslogin.o: cb_apslogin.h
cb_apsmenu.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_apsmenu.o: /usr/include/stdlib.h /usr/dt/include/Xm/FileSB.h
cb_apsmenu.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
cb_apsmenu.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
cb_apsmenu.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_apsmenu.o: /usr/openwin/include/X11/X.h
cb_apsmenu.o: /usr/openwin/include/X11/Xfuncproto.h
cb_apsmenu.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_apsmenu.o: /usr/openwin/include/X11/Xutil.h
cb_apsmenu.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
cb_apsmenu.o: /usr/openwin/include/X11/Core.h
cb_apsmenu.o: /usr/openwin/include/X11/Composite.h
cb_apsmenu.o: /usr/openwin/include/X11/Constraint.h
cb_apsmenu.o: /usr/openwin/include/X11/Object.h
cb_apsmenu.o: /usr/openwin/include/X11/RectObj.h
cb_apsmenu.o: /usr/openwin/include/X11/Shell.h
cb_apsmenu.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
cb_apsmenu.o: /usr/openwin/include/X11/StringDefs.h
cb_apsmenu.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_apsmenu.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_apsmenu.o: /home/sybase/syb11/include/sybfront.h
cb_apsmenu.o: /home/sybase/syb11/include/cstypes.h
cb_apsmenu.o: /home/sybase/syb11/include/csconfig.h
cb_apsmenu.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_apsmenu.o: /home/sybase/syb11/include/sybdb.h
cb_apsmenu.o: /home/sybase/syb11/include/syblogin.h
cb_apsmenu.o: /home/sybase/syb11/include/syberror.h
cb_apsmenu.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_apsmenu.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_apsmenu.o: /home/aps/r2.1.1/include/global/llist.h
cb_apsmenu.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_apsmenu.o: /home/aps/r2.1.1/include/local/aps.h
cb_apsmenu.o: /home/aps/r2.1.1/include/local/aps_defs.h
cb_apsmenu.o: /home/aps/r2.1.1/include/local/aps_extern.h aps_exe_names.h
cb_apsmenu.o: /home/aps/r2.1.1/include/local/apspath.h gui_utils.h
cb_apsmenu.o: vc_apsmenu.h /usr/dt/include/Xm/MwmUtil.h
cb_apsmenu.o: /usr/openwin/include/X11/Xmd.h /usr/dt/include/Xm/MenuShell.h
cb_apsmenu.o: UxXt.h /usr/dt/include/Xm/RepType.h vc_cnomorb.h vc_cnomcov.h
cb_apsmenu.o: vc_sortform.h vc_searchform.h vc_cdtakeopps.h vc_crtapsfile.h
cb_apsmenu.o: vc_muinterval.h vc_permstatus.h vc_prcapsfile.h vc_darmanager.h
cb_apsmenu.o: vc_dtkmanager.h vc_rgsdntimes.h vc_antdntimes.h
cb_apsmenu.o: vc_fileselectionbox.h vc_fileviewer.h vc_conrndup.h
cb_apsmenu.o: vc_apswoscompare.h vc_phaseselect.h
cb_apsmenu.o: /home/aps/r2.1.1/include/global/dapps_defs.h subprocess.h
cb_apswoscompare.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_apswoscompare.o: /usr/include/unistd.h /usr/include/sys/types.h
cb_apswoscompare.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_apswoscompare.o: /usr/include/sys/unistd.h /usr/include/stdlib.h
cb_apswoscompare.o: /usr/include/string.h /usr/include/signal.h
cb_apswoscompare.o: /usr/include/sys/signal.h /usr/include/errno.h
cb_apswoscompare.o: /usr/include/sys/errno.h /usr/dt/include/Xm/Xm.h
cb_apswoscompare.o: /usr/openwin/include/X11/Intrinsic.h
cb_apswoscompare.o: /usr/openwin/include/X11/Xlib.h
cb_apswoscompare.o: /usr/openwin/include/X11/X.h
cb_apswoscompare.o: /usr/openwin/include/X11/Xfuncproto.h
cb_apswoscompare.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_apswoscompare.o: /usr/openwin/include/X11/Xutil.h
cb_apswoscompare.o: /usr/openwin/include/X11/Xresource.h
cb_apswoscompare.o: /usr/openwin/include/X11/Core.h
cb_apswoscompare.o: /usr/openwin/include/X11/Composite.h
cb_apswoscompare.o: /usr/openwin/include/X11/Constraint.h
cb_apswoscompare.o: /usr/openwin/include/X11/Object.h
cb_apswoscompare.o: /usr/openwin/include/X11/RectObj.h
cb_apswoscompare.o: /usr/openwin/include/X11/Shell.h
cb_apswoscompare.o: /usr/openwin/include/X11/Xatom.h
cb_apswoscompare.o: /usr/dt/include/Xm/XmStrDefs.h
cb_apswoscompare.o: /usr/openwin/include/X11/StringDefs.h
cb_apswoscompare.o: /usr/dt/include/Xm/VirtKeys.h
cb_apswoscompare.o: /usr/dt/include/Xm/VendorS.h /usr/dt/include/Xm/Text.h
cb_apswoscompare.o: UxXt.h /home/aps/r2.1.1/include/global/dapps_defs.h
cb_apswoscompare.o: /home/aps/r2.1.1/include/local/aps_defs.h aps_exe_names.h
cb_apswoscompare.o: /home/aps/r2.1.1/include/local/aps_extern.h
cb_apswoscompare.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_apswoscompare.o: /home/sybase/syb11/include/sybfront.h
cb_apswoscompare.o: /home/sybase/syb11/include/cstypes.h
cb_apswoscompare.o: /home/sybase/syb11/include/csconfig.h
cb_apswoscompare.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_apswoscompare.o: /home/sybase/syb11/include/sybdb.h
cb_apswoscompare.o: /home/sybase/syb11/include/syblogin.h
cb_apswoscompare.o: /home/sybase/syb11/include/syberror.h
cb_apswoscompare.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_apswoscompare.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_apswoscompare.o: /home/aps/r2.1.1/include/global/llist.h
cb_apswoscompare.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_apswoscompare.o: /home/aps/r2.1.1/include/local/apspath.h
cb_apswoscompare.o: /home/aps/r2.1.1/include/local/apsfiledef.h
cb_apswoscompare.o: vc_apswoscompare.h /usr/dt/include/Xm/MwmUtil.h
cb_apswoscompare.o: /usr/openwin/include/X11/Xmd.h
cb_apswoscompare.o: /usr/dt/include/Xm/MenuShell.h cb_apswoscompare.h
cb_apswoscompare.o: vc_msgbox.h /usr/dt/include/Xm/DialogS.h subprocess.h
cb_apswoscompare.o: gui_utils.h
cb_cdtakeopps.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_cdtakeopps.o: /usr/include/stdlib.h /usr/include/unistd.h
cb_cdtakeopps.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
cb_cdtakeopps.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
cb_cdtakeopps.o: /usr/include/malloc.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_cdtakeopps.o: /home/sybase/syb11/include/sybfront.h
cb_cdtakeopps.o: /home/sybase/syb11/include/cstypes.h
cb_cdtakeopps.o: /home/sybase/syb11/include/csconfig.h
cb_cdtakeopps.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_cdtakeopps.o: /home/sybase/syb11/include/sybdb.h
cb_cdtakeopps.o: /home/sybase/syb11/include/syblogin.h
cb_cdtakeopps.o: /home/sybase/syb11/include/syberror.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/global/llist.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/local/aps_defs.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/local/db_dar.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/local/db_site.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/local/db_satsensor.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/local/db_station.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/local/aps_extern.h aps_exe_names.h
cb_cdtakeopps.o: gui_defs.h /home/aps/r2.1.1/include/global/nmalloc.h
cb_cdtakeopps.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/Xm.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Intrinsic.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Xfuncproto.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Xutil.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Core.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Composite.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Constraint.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Object.h
cb_cdtakeopps.o: /usr/openwin/include/X11/RectObj.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Shell.h
cb_cdtakeopps.o: /usr/openwin/include/X11/Xatom.h
cb_cdtakeopps.o: /usr/dt/include/Xm/XmStrDefs.h
cb_cdtakeopps.o: /usr/openwin/include/X11/StringDefs.h
cb_cdtakeopps.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_cdtakeopps.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/TextF.h
cb_cdtakeopps.o: /usr/dt/include/Xm/ToggleB.h UxXt.h vc_cdtakeopps.h
cb_cdtakeopps.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
cb_cdtakeopps.o: /usr/dt/include/Xm/MenuShell.h cb_cdtakeopps.h cb_datetime.h
cb_cdtakeopps.o: /home/aps/r2.1.1/include/local/timeconv.h gui_utils.h
cb_cdtakeopps.o: satmenus.h subprocess.h
cb_cnomcov.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_cnomcov.o: /usr/include/stdlib.h /usr/include/unistd.h
cb_cnomcov.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
cb_cnomcov.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
cb_cnomcov.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_cnomcov.o: /home/sybase/syb11/include/sybfront.h
cb_cnomcov.o: /home/sybase/syb11/include/cstypes.h
cb_cnomcov.o: /home/sybase/syb11/include/csconfig.h
cb_cnomcov.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_cnomcov.o: /home/sybase/syb11/include/sybdb.h
cb_cnomcov.o: /home/sybase/syb11/include/syblogin.h
cb_cnomcov.o: /home/sybase/syb11/include/syberror.h
cb_cnomcov.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_cnomcov.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_cnomcov.o: /home/aps/r2.1.1/include/global/llist.h
cb_cnomcov.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
cb_cnomcov.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/aps_defs.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/apspath.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/apsfiledef.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/db_dtk.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/db_ephemeris.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/db_phase.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/db_satsensor.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/db_station.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/aps_extern.h aps_exe_names.h
cb_cnomcov.o: /home/aps/r2.1.1/include/global/nmalloc.h
cb_cnomcov.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/Xm.h
cb_cnomcov.o: /usr/openwin/include/X11/Intrinsic.h
cb_cnomcov.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
cb_cnomcov.o: /usr/openwin/include/X11/Xfuncproto.h
cb_cnomcov.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_cnomcov.o: /usr/openwin/include/X11/Xutil.h
cb_cnomcov.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
cb_cnomcov.o: /usr/openwin/include/X11/Core.h
cb_cnomcov.o: /usr/openwin/include/X11/Composite.h
cb_cnomcov.o: /usr/openwin/include/X11/Constraint.h
cb_cnomcov.o: /usr/openwin/include/X11/Object.h
cb_cnomcov.o: /usr/openwin/include/X11/RectObj.h
cb_cnomcov.o: /usr/openwin/include/X11/Shell.h
cb_cnomcov.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
cb_cnomcov.o: /usr/openwin/include/X11/StringDefs.h
cb_cnomcov.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_cnomcov.o: /usr/dt/include/Xm/Label.h /usr/dt/include/Xm/Text.h
cb_cnomcov.o: /usr/dt/include/Xm/TextF.h /usr/dt/include/Xm/ToggleB.h UxXt.h
cb_cnomcov.o: subprocess.h satmenus.h gui_utils.h vc_cnomcov.h
cb_cnomcov.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
cb_cnomcov.o: /usr/dt/include/Xm/MenuShell.h cb_cnomcov.h cb_datetime.h
cb_cnomcov.o: /home/aps/r2.1.1/include/local/timeconv.h
cb_cnomorb.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_cnomorb.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/Xm.h
cb_cnomorb.o: /usr/openwin/include/X11/Intrinsic.h
cb_cnomorb.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
cb_cnomorb.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_cnomorb.o: /usr/openwin/include/X11/X.h
cb_cnomorb.o: /usr/openwin/include/X11/Xfuncproto.h
cb_cnomorb.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_cnomorb.o: /usr/openwin/include/X11/Xutil.h
cb_cnomorb.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
cb_cnomorb.o: /usr/openwin/include/X11/Core.h
cb_cnomorb.o: /usr/openwin/include/X11/Composite.h
cb_cnomorb.o: /usr/openwin/include/X11/Constraint.h
cb_cnomorb.o: /usr/openwin/include/X11/Object.h
cb_cnomorb.o: /usr/openwin/include/X11/RectObj.h
cb_cnomorb.o: /usr/openwin/include/X11/Shell.h
cb_cnomorb.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
cb_cnomorb.o: /usr/openwin/include/X11/StringDefs.h
cb_cnomorb.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_cnomorb.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/TextF.h UxXt.h
cb_cnomorb.o: /usr/include/stdlib.h vc_cnomorb.h /usr/dt/include/Xm/MwmUtil.h
cb_cnomorb.o: /usr/openwin/include/X11/Xmd.h /usr/dt/include/Xm/MenuShell.h
cb_cnomorb.o: vc_msgbox.h /usr/dt/include/Xm/DialogS.h
cb_cnomorb.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_cnomorb.o: /home/aps/r2.1.1/include/local/aps_defs.h
cb_cnomorb.o: /home/aps/r2.1.1/include/local/aps_extern.h
cb_cnomorb.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_cnomorb.o: /home/sybase/syb11/include/sybfront.h
cb_cnomorb.o: /home/sybase/syb11/include/cstypes.h
cb_cnomorb.o: /home/sybase/syb11/include/csconfig.h
cb_cnomorb.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_cnomorb.o: /home/sybase/syb11/include/sybdb.h
cb_cnomorb.o: /home/sybase/syb11/include/syblogin.h
cb_cnomorb.o: /home/sybase/syb11/include/syberror.h
cb_cnomorb.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_cnomorb.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_cnomorb.o: /home/aps/r2.1.1/include/global/llist.h
cb_cnomorb.o: /home/aps/r2.1.1/include/global/ord_llist.h aps_exe_names.h
cb_cnomorb.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_cnomorb.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_cnomorb.o: /home/aps/r2.1.1/include/local/db_phase.h subprocess.h
cb_cnomorb.o: satmenus.h gui_utils.h
cb_conrndup.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_conrndup.o: /usr/include/stdlib.h /usr/include/string.h
cb_conrndup.o: /usr/include/ctype.h /usr/dt/include/Xm/Text.h
cb_conrndup.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
cb_conrndup.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
cb_conrndup.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_conrndup.o: /usr/openwin/include/X11/X.h
cb_conrndup.o: /usr/openwin/include/X11/Xfuncproto.h
cb_conrndup.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_conrndup.o: /usr/openwin/include/X11/Xutil.h
cb_conrndup.o: /usr/openwin/include/X11/Xresource.h
cb_conrndup.o: /usr/openwin/include/X11/Core.h
cb_conrndup.o: /usr/openwin/include/X11/Composite.h
cb_conrndup.o: /usr/openwin/include/X11/Constraint.h
cb_conrndup.o: /usr/openwin/include/X11/Object.h
cb_conrndup.o: /usr/openwin/include/X11/RectObj.h
cb_conrndup.o: /usr/openwin/include/X11/Shell.h
cb_conrndup.o: /usr/openwin/include/X11/Xatom.h
cb_conrndup.o: /usr/dt/include/Xm/XmStrDefs.h
cb_conrndup.o: /usr/openwin/include/X11/StringDefs.h
cb_conrndup.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_conrndup.o: /usr/dt/include/Xm/TextF.h vc_conrndup.h
cb_conrndup.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
cb_conrndup.o: /usr/dt/include/Xm/MenuShell.h UxXt.h
cb_conrndup.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_conrndup.o: /home/aps/r2.1.1/include/local/aps_defs.h aps_exe_names.h
cb_conrndup.o: /home/aps/r2.1.1/include/local/aps_extern.h
cb_conrndup.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_conrndup.o: /home/sybase/syb11/include/sybfront.h
cb_conrndup.o: /home/sybase/syb11/include/cstypes.h
cb_conrndup.o: /home/sybase/syb11/include/csconfig.h
cb_conrndup.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_conrndup.o: /home/sybase/syb11/include/sybdb.h
cb_conrndup.o: /home/sybase/syb11/include/syblogin.h
cb_conrndup.o: /home/sybase/syb11/include/syberror.h
cb_conrndup.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_conrndup.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_conrndup.o: /home/aps/r2.1.1/include/global/llist.h
cb_conrndup.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_conrndup.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_conrndup.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_conrndup.o: /home/aps/r2.1.1/include/local/db_dtk.h gui_utils.h
cb_conrndup.o: subprocess.h cb_datetime.h
cb_crtapsfile.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_crtapsfile.o: /usr/include/unistd.h /usr/include/sys/types.h
cb_crtapsfile.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_crtapsfile.o: /usr/include/sys/unistd.h /usr/include/stdlib.h
cb_crtapsfile.o: /usr/include/libgen.h /usr/include/time.h
cb_crtapsfile.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
cb_crtapsfile.o: /usr/include/sys/file.h /usr/include/sys/t_lock.h
cb_crtapsfile.o: /usr/include/sys/machlock.h /usr/include/sys/dki_lkinfo.h
cb_crtapsfile.o: /usr/include/sys/dl.h /usr/include/sys/sleepq.h
cb_crtapsfile.o: /usr/include/sys/turnstile.h /usr/include/sys/param.h
cb_crtapsfile.o: /usr/include/sys/pirec.h /usr/include/sys/mutex.h
cb_crtapsfile.o: /usr/include/sys/rwlock.h /usr/include/sys/semaphore.h
cb_crtapsfile.o: /usr/include/sys/condvar.h /usr/include/string.h
cb_crtapsfile.o: /usr/include/errno.h /usr/include/sys/errno.h
cb_crtapsfile.o: /usr/dt/include/Xm/FileSB.h /usr/dt/include/Xm/Xm.h
cb_crtapsfile.o: /usr/openwin/include/X11/Intrinsic.h
cb_crtapsfile.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
cb_crtapsfile.o: /usr/openwin/include/X11/Xfuncproto.h
cb_crtapsfile.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_crtapsfile.o: /usr/openwin/include/X11/Xutil.h
cb_crtapsfile.o: /usr/openwin/include/X11/Xresource.h
cb_crtapsfile.o: /usr/openwin/include/X11/Core.h
cb_crtapsfile.o: /usr/openwin/include/X11/Composite.h
cb_crtapsfile.o: /usr/openwin/include/X11/Constraint.h
cb_crtapsfile.o: /usr/openwin/include/X11/Object.h
cb_crtapsfile.o: /usr/openwin/include/X11/RectObj.h
cb_crtapsfile.o: /usr/openwin/include/X11/Shell.h
cb_crtapsfile.o: /usr/openwin/include/X11/Xatom.h
cb_crtapsfile.o: /usr/dt/include/Xm/XmStrDefs.h
cb_crtapsfile.o: /usr/openwin/include/X11/StringDefs.h
cb_crtapsfile.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_crtapsfile.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/Text.h
cb_crtapsfile.o: /usr/dt/include/Xm/TextF.h /usr/dt/include/Xm/ToggleB.h
cb_crtapsfile.o: UxXt.h /home/aps/r2.1.1/include/global/dapps_defs.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_crtapsfile.o: /home/sybase/syb11/include/sybfront.h
cb_crtapsfile.o: /home/sybase/syb11/include/cstypes.h
cb_crtapsfile.o: /home/sybase/syb11/include/csconfig.h
cb_crtapsfile.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_crtapsfile.o: /home/sybase/syb11/include/sybdb.h
cb_crtapsfile.o: /home/sybase/syb11/include/syblogin.h
cb_crtapsfile.o: /home/sybase/syb11/include/syberror.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/global/llist.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/local/db_reqq_phase.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/local/aps_defs.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/local/aps_extern.h aps_exe_names.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/local/apspath.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/local/mu_utilities.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/local/apsfiledef.h
cb_crtapsfile.o: /home/aps/r2.1.1/include/local/timeconv.h vc_crtapsfile.h
cb_crtapsfile.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
cb_crtapsfile.o: /usr/dt/include/Xm/MenuShell.h vc_phaseselect.h vc_msgbox.h
cb_crtapsfile.o: /usr/dt/include/Xm/DialogS.h cb_crtapsfile.h cb_datetime.h
cb_crtapsfile.o: subprocess.h gui_utils.h gui_mu_utils.h
cb_crtapsfile.o: /ASF/PRECM/include/faif/xlate.h
cb_crtapsfile.o: /ASF/PRECM/include/sys/odldef.h
cb_crtapsfile.o: /ASF/PRECM/include/faif/faifdefs.h /usr/include/syslog.h
cb_crtapsfile.o: /usr/include/sys/syslog.h
cb_darmanager.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_darmanager.o: /usr/include/unistd.h /usr/include/sys/types.h
cb_darmanager.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_darmanager.o: /usr/include/sys/unistd.h /usr/include/stdlib.h
cb_darmanager.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
cb_darmanager.o: /usr/include/malloc.h /usr/include/string.h
cb_darmanager.o: /usr/include/sys/stat.h /usr/include/sys/time.h
cb_darmanager.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/Xm.h
cb_darmanager.o: /usr/openwin/include/X11/Intrinsic.h
cb_darmanager.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
cb_darmanager.o: /usr/openwin/include/X11/Xfuncproto.h
cb_darmanager.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_darmanager.o: /usr/openwin/include/X11/Xutil.h
cb_darmanager.o: /usr/openwin/include/X11/Xresource.h
cb_darmanager.o: /usr/openwin/include/X11/Core.h
cb_darmanager.o: /usr/openwin/include/X11/Composite.h
cb_darmanager.o: /usr/openwin/include/X11/Constraint.h
cb_darmanager.o: /usr/openwin/include/X11/Object.h
cb_darmanager.o: /usr/openwin/include/X11/RectObj.h
cb_darmanager.o: /usr/openwin/include/X11/Shell.h
cb_darmanager.o: /usr/openwin/include/X11/Xatom.h
cb_darmanager.o: /usr/dt/include/Xm/XmStrDefs.h
cb_darmanager.o: /usr/openwin/include/X11/StringDefs.h
cb_darmanager.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_darmanager.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/TextF.h
cb_darmanager.o: /usr/dt/include/Xm/FileSB.h
cb_darmanager.o: /home/aps/r2.1.1/include/global/dapps_defs.h vc_darmanager.h
cb_darmanager.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
cb_darmanager.o: /usr/dt/include/Xm/MenuShell.h UxXt.h
cb_darmanager.o: /ASF/PRECM/include/imsdads/ims_query.h
cb_darmanager.o: /ASF/PRECM/include/imsdads/ims_dbms.h
cb_darmanager.o: /home/sybase/syb11/include/sybfront.h
cb_darmanager.o: /home/sybase/syb11/include/cstypes.h
cb_darmanager.o: /home/sybase/syb11/include/csconfig.h
cb_darmanager.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_darmanager.o: /home/sybase/syb11/include/ctpublic.h
cb_darmanager.o: /home/sybase/syb11/include/cspublic.h
cb_darmanager.o: /home/sybase/syb11/include/sqlca.h
cb_darmanager.o: /home/sybase/syb11/include/sqlda.h
cb_darmanager.o: /home/sybase/syb11/include/sybdb.h
cb_darmanager.o: /home/sybase/syb11/include/syblogin.h
cb_darmanager.o: /ASF/PRECM/include/imsdads/ims_const.h
cb_darmanager.o: /ASF/PRECM/include/imsdads/ims_msg.h
cb_darmanager.o: /ASF/PRECM/include/imsdads/ims_qi.h
cb_darmanager.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
cb_darmanager.o: /home/aps/r2.1.1/include/global/fa_defs.h
cb_darmanager.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_darmanager.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_darmanager.o: /home/aps/r2.1.1/include/global/llist.h
cb_darmanager.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/DARconversions.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/ODLconversions.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/ODL_defs.h
cb_darmanager.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_darmanager.o: /home/sybase/syb11/include/syberror.h
cb_darmanager.o: /ASF/PRECM/include/sys/odldef.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/phase_utilities.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/db_phase.h
cb_darmanager.o: /home/aps/r2.1.1/include/global/nmalloc.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/db_dar.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/aps_defs.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/aps_extern.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_darmanager.o: aps_imsDB.h /usr/include/errno.h /usr/include/sys/errno.h
cb_darmanager.o: /usr/include/ctype.h /usr/include/sys/utsname.h
cb_darmanager.o: /usr/include/syslog.h /usr/include/sys/syslog.h
cb_darmanager.o: /ASF/PRECM/include/imsdads/ims_getInput.h
cb_darmanager.o: /ASF/PRECM/include/imsdads/ims_util.h gui_defs.h gui_utils.h
cb_darmanager.o: gui_mu_utils.h satmenus.h cb_darmanager.h cb_datetime.h
cb_darmanager.o: subprocess.h /home/aps/r2.1.1/include/local/apspath.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/aps_encrypt.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/apsfiledef.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/timeconv.h
cb_darmanager.o: /home/aps/r2.1.1/include/local/mu_utilities.h
cb_datetime.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_datetime.o: /usr/include/stdlib.h /usr/include/math.h /usr/include/ctype.h
cb_datetime.o: /usr/include/string.h /usr/dt/include/Xm/Text.h
cb_datetime.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
cb_datetime.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
cb_datetime.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_datetime.o: /usr/openwin/include/X11/X.h
cb_datetime.o: /usr/openwin/include/X11/Xfuncproto.h
cb_datetime.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_datetime.o: /usr/openwin/include/X11/Xutil.h
cb_datetime.o: /usr/openwin/include/X11/Xresource.h
cb_datetime.o: /usr/openwin/include/X11/Core.h
cb_datetime.o: /usr/openwin/include/X11/Composite.h
cb_datetime.o: /usr/openwin/include/X11/Constraint.h
cb_datetime.o: /usr/openwin/include/X11/Object.h
cb_datetime.o: /usr/openwin/include/X11/RectObj.h
cb_datetime.o: /usr/openwin/include/X11/Shell.h
cb_datetime.o: /usr/openwin/include/X11/Xatom.h
cb_datetime.o: /usr/dt/include/Xm/XmStrDefs.h
cb_datetime.o: /usr/openwin/include/X11/StringDefs.h
cb_datetime.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_datetime.o: /usr/dt/include/Xm/TextF.h
cb_datetime.o: /home/aps/r2.1.1/include/global/dapps_defs.h cb_datetime.h
cb_datetime.o: gui_utils.h /home/aps/r2.1.1/include/local/timeconv.h
cb_dtkmanager.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_dtkmanager.o: /usr/include/stdlib.h /usr/include/fcntl.h
cb_dtkmanager.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
cb_dtkmanager.o: /usr/include/sys/machtypes.h /usr/include/sys/fcntl.h
cb_dtkmanager.o: /usr/include/string.h /usr/include/sys/stat.h
cb_dtkmanager.o: /usr/include/sys/time.h /usr/include/time.h
cb_dtkmanager.o: /usr/dt/include/Xm/FileSB.h /usr/dt/include/Xm/Xm.h
cb_dtkmanager.o: /usr/openwin/include/X11/Intrinsic.h
cb_dtkmanager.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
cb_dtkmanager.o: /usr/openwin/include/X11/Xfuncproto.h
cb_dtkmanager.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_dtkmanager.o: /usr/openwin/include/X11/Xutil.h
cb_dtkmanager.o: /usr/openwin/include/X11/Xresource.h
cb_dtkmanager.o: /usr/openwin/include/X11/Core.h
cb_dtkmanager.o: /usr/openwin/include/X11/Composite.h
cb_dtkmanager.o: /usr/openwin/include/X11/Constraint.h
cb_dtkmanager.o: /usr/openwin/include/X11/Object.h
cb_dtkmanager.o: /usr/openwin/include/X11/RectObj.h
cb_dtkmanager.o: /usr/openwin/include/X11/Shell.h
cb_dtkmanager.o: /usr/openwin/include/X11/Xatom.h
cb_dtkmanager.o: /usr/dt/include/Xm/XmStrDefs.h
cb_dtkmanager.o: /usr/openwin/include/X11/StringDefs.h
cb_dtkmanager.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_dtkmanager.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/Text.h
cb_dtkmanager.o: /usr/dt/include/Xm/TextF.h /usr/dt/include/Xm/ToggleB.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/aps.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/aps_defs.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/aps_extern.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_dtkmanager.o: /home/sybase/syb11/include/sybfront.h
cb_dtkmanager.o: /home/sybase/syb11/include/cstypes.h
cb_dtkmanager.o: /home/sybase/syb11/include/csconfig.h
cb_dtkmanager.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_dtkmanager.o: /home/sybase/syb11/include/sybdb.h
cb_dtkmanager.o: /home/sybase/syb11/include/syblogin.h
cb_dtkmanager.o: /home/sybase/syb11/include/syberror.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/global/llist.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/mu_utilities.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/apspath.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/db_dtk.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/db_j1_dn_times.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/db_dl2obs.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/global/nmalloc.h satmenus.h
cb_dtkmanager.o: gui_defs.h gui_utils.h gui_mu_utils.h subprocess.h
cb_dtkmanager.o: /home/aps/r2.1.1/include/local/timeconv.h vc_dtkmanager.h
cb_dtkmanager.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
cb_dtkmanager.o: /usr/dt/include/Xm/MenuShell.h UxXt.h cb_dtkmanager.h
cb_dtkmanager.o: cb_datetime.h
cb_fileviewer.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_fileviewer.o: /usr/include/sys/stat.h /usr/include/sys/time.h
cb_fileviewer.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
cb_fileviewer.o: /usr/include/sys/machtypes.h /usr/dt/include/Xm/Text.h
cb_fileviewer.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
cb_fileviewer.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
cb_fileviewer.o: /usr/openwin/include/X11/Xfuncproto.h
cb_fileviewer.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_fileviewer.o: /usr/openwin/include/X11/Xutil.h
cb_fileviewer.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
cb_fileviewer.o: /usr/openwin/include/X11/Core.h
cb_fileviewer.o: /usr/openwin/include/X11/Composite.h
cb_fileviewer.o: /usr/openwin/include/X11/Constraint.h
cb_fileviewer.o: /usr/openwin/include/X11/Object.h
cb_fileviewer.o: /usr/openwin/include/X11/RectObj.h
cb_fileviewer.o: /usr/openwin/include/X11/Shell.h
cb_fileviewer.o: /usr/openwin/include/X11/Xatom.h
cb_fileviewer.o: /usr/dt/include/Xm/XmStrDefs.h
cb_fileviewer.o: /usr/openwin/include/X11/StringDefs.h
cb_fileviewer.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_fileviewer.o: /usr/dt/include/Xm/TextF.h UxXt.h /usr/include/stdlib.h
cb_fileviewer.o: vc_fileviewer.h /usr/dt/include/Xm/MwmUtil.h
cb_fileviewer.o: /usr/openwin/include/X11/Xmd.h
cb_fileviewer.o: /usr/dt/include/Xm/MenuShell.h
cb_permstatus.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_permstatus.o: /usr/include/stdlib.h /usr/include/string.h
cb_permstatus.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
cb_permstatus.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
cb_permstatus.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_permstatus.o: /usr/openwin/include/X11/X.h
cb_permstatus.o: /usr/openwin/include/X11/Xfuncproto.h
cb_permstatus.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_permstatus.o: /usr/openwin/include/X11/Xutil.h
cb_permstatus.o: /usr/openwin/include/X11/Xresource.h
cb_permstatus.o: /usr/openwin/include/X11/Core.h
cb_permstatus.o: /usr/openwin/include/X11/Composite.h
cb_permstatus.o: /usr/openwin/include/X11/Constraint.h
cb_permstatus.o: /usr/openwin/include/X11/Object.h
cb_permstatus.o: /usr/openwin/include/X11/RectObj.h
cb_permstatus.o: /usr/openwin/include/X11/Shell.h
cb_permstatus.o: /usr/openwin/include/X11/Xatom.h
cb_permstatus.o: /usr/dt/include/Xm/XmStrDefs.h
cb_permstatus.o: /usr/openwin/include/X11/StringDefs.h
cb_permstatus.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_permstatus.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/TextF.h
cb_permstatus.o: /usr/dt/include/Xm/ToggleB.h
cb_permstatus.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_permstatus.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_permstatus.o: /home/sybase/syb11/include/sybfront.h
cb_permstatus.o: /home/sybase/syb11/include/cstypes.h
cb_permstatus.o: /home/sybase/syb11/include/csconfig.h
cb_permstatus.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_permstatus.o: /home/sybase/syb11/include/sybdb.h
cb_permstatus.o: /home/sybase/syb11/include/syblogin.h
cb_permstatus.o: /home/sybase/syb11/include/syberror.h
cb_permstatus.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_permstatus.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_permstatus.o: /home/aps/r2.1.1/include/global/llist.h
cb_permstatus.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_permstatus.o: /home/aps/r2.1.1/include/local/timeconv.h
cb_permstatus.o: /home/aps/r2.1.1/include/local/aps_defs.h
cb_permstatus.o: /home/aps/r2.1.1/include/local/aps_extern.h
cb_permstatus.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_permstatus.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_permstatus.o: /home/aps/r2.1.1/include/local/db_active_dar_activities.h
cb_permstatus.o: /home/aps/r2.1.1/include/local/db_active_planning_activities.h
cb_permstatus.o: /home/aps/r2.1.1/include/local/db_active_single_activities.h
cb_permstatus.o: /home/aps/r2.1.1/include/local/mu_utilities.h gui_defs.h
cb_permstatus.o: gui_utils.h vc_permstatus.h /usr/dt/include/Xm/MwmUtil.h
cb_permstatus.o: /usr/openwin/include/X11/Xmd.h
cb_permstatus.o: /usr/dt/include/Xm/MenuShell.h UxXt.h cb_permstatus.h
cb_prcapsfile.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_prcapsfile.o: /usr/include/unistd.h /usr/include/sys/types.h
cb_prcapsfile.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_prcapsfile.o: /usr/include/sys/unistd.h /usr/include/stdlib.h
cb_prcapsfile.o: /usr/include/string.h /usr/include/signal.h
cb_prcapsfile.o: /usr/include/sys/signal.h /usr/include/errno.h
cb_prcapsfile.o: /usr/include/sys/errno.h /usr/include/values.h
cb_prcapsfile.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
cb_prcapsfile.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
cb_prcapsfile.o: /usr/openwin/include/X11/Xfuncproto.h
cb_prcapsfile.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_prcapsfile.o: /usr/openwin/include/X11/Xutil.h
cb_prcapsfile.o: /usr/openwin/include/X11/Xresource.h
cb_prcapsfile.o: /usr/openwin/include/X11/Core.h
cb_prcapsfile.o: /usr/openwin/include/X11/Composite.h
cb_prcapsfile.o: /usr/openwin/include/X11/Constraint.h
cb_prcapsfile.o: /usr/openwin/include/X11/Object.h
cb_prcapsfile.o: /usr/openwin/include/X11/RectObj.h
cb_prcapsfile.o: /usr/openwin/include/X11/Shell.h
cb_prcapsfile.o: /usr/openwin/include/X11/Xatom.h
cb_prcapsfile.o: /usr/dt/include/Xm/XmStrDefs.h
cb_prcapsfile.o: /usr/openwin/include/X11/StringDefs.h
cb_prcapsfile.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_prcapsfile.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/Text.h UxXt.h
cb_prcapsfile.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_prcapsfile.o: /home/aps/r2.1.1/include/local/aps_defs.h aps_exe_names.h
cb_prcapsfile.o: /home/aps/r2.1.1/include/local/aps_extern.h
cb_prcapsfile.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_prcapsfile.o: /home/sybase/syb11/include/sybfront.h
cb_prcapsfile.o: /home/sybase/syb11/include/cstypes.h
cb_prcapsfile.o: /home/sybase/syb11/include/csconfig.h
cb_prcapsfile.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_prcapsfile.o: /home/sybase/syb11/include/sybdb.h
cb_prcapsfile.o: /home/sybase/syb11/include/syblogin.h
cb_prcapsfile.o: /home/sybase/syb11/include/syberror.h
cb_prcapsfile.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_prcapsfile.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_prcapsfile.o: /home/aps/r2.1.1/include/global/llist.h
cb_prcapsfile.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_prcapsfile.o: /home/aps/r2.1.1/include/local/mu_utilities.h
cb_prcapsfile.o: /home/aps/r2.1.1/include/local/apspath.h
cb_prcapsfile.o: /home/aps/r2.1.1/include/local/apsfiledef.h vc_prcapsfile.h
cb_prcapsfile.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
cb_prcapsfile.o: /usr/dt/include/Xm/MenuShell.h cb_prcapsfile.h
cb_prcapsfile.o: prcapsfile_main.h vc_msgbox.h /usr/dt/include/Xm/DialogS.h
cb_prcapsfile.o: subprocess.h gui_utils.h gui_mu_utils.h
cb_rgsdntimes.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_rgsdntimes.o: /usr/include/unistd.h /usr/include/sys/types.h
cb_rgsdntimes.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_rgsdntimes.o: /usr/include/sys/unistd.h /usr/include/stdlib.h
cb_rgsdntimes.o: /usr/include/time.h /usr/dt/include/Xm/List.h
cb_rgsdntimes.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
cb_rgsdntimes.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
cb_rgsdntimes.o: /usr/openwin/include/X11/Xfuncproto.h
cb_rgsdntimes.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_rgsdntimes.o: /usr/openwin/include/X11/Xutil.h
cb_rgsdntimes.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
cb_rgsdntimes.o: /usr/openwin/include/X11/Core.h
cb_rgsdntimes.o: /usr/openwin/include/X11/Composite.h
cb_rgsdntimes.o: /usr/openwin/include/X11/Constraint.h
cb_rgsdntimes.o: /usr/openwin/include/X11/Object.h
cb_rgsdntimes.o: /usr/openwin/include/X11/RectObj.h
cb_rgsdntimes.o: /usr/openwin/include/X11/Shell.h
cb_rgsdntimes.o: /usr/openwin/include/X11/Xatom.h
cb_rgsdntimes.o: /usr/dt/include/Xm/XmStrDefs.h
cb_rgsdntimes.o: /usr/openwin/include/X11/StringDefs.h
cb_rgsdntimes.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_rgsdntimes.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/TextF.h
cb_rgsdntimes.o: /usr/dt/include/Xm/ToggleB.h vc_rgsdntimes.h
cb_rgsdntimes.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
cb_rgsdntimes.o: /usr/dt/include/Xm/MenuShell.h UxXt.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_rgsdntimes.o: /home/sybase/syb11/include/sybfront.h
cb_rgsdntimes.o: /home/sybase/syb11/include/cstypes.h
cb_rgsdntimes.o: /home/sybase/syb11/include/csconfig.h
cb_rgsdntimes.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_rgsdntimes.o: /home/sybase/syb11/include/sybdb.h
cb_rgsdntimes.o: /home/sybase/syb11/include/syblogin.h
cb_rgsdntimes.o: /home/sybase/syb11/include/syberror.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/global/llist.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/local/db_rgs_down_times.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/local/aps_extern.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/global/nmalloc.h satmenus.h
cb_rgsdntimes.o: gui_utils.h gui_mu_utils.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/local/mu_utilities.h subprocess.h
cb_rgsdntimes.o: cb_rgsdntimes.h cb_datetime.h
cb_rgsdntimes.o: /home/aps/r2.1.1/include/local/timeconv.h
cb_searchform.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_searchform.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/Xm.h
cb_searchform.o: /usr/openwin/include/X11/Intrinsic.h
cb_searchform.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
cb_searchform.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_searchform.o: /usr/openwin/include/X11/X.h
cb_searchform.o: /usr/openwin/include/X11/Xfuncproto.h
cb_searchform.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_searchform.o: /usr/openwin/include/X11/Xutil.h
cb_searchform.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
cb_searchform.o: /usr/openwin/include/X11/Core.h
cb_searchform.o: /usr/openwin/include/X11/Composite.h
cb_searchform.o: /usr/openwin/include/X11/Constraint.h
cb_searchform.o: /usr/openwin/include/X11/Object.h
cb_searchform.o: /usr/openwin/include/X11/RectObj.h
cb_searchform.o: /usr/openwin/include/X11/Shell.h
cb_searchform.o: /usr/openwin/include/X11/Xatom.h
cb_searchform.o: /usr/dt/include/Xm/XmStrDefs.h
cb_searchform.o: /usr/openwin/include/X11/StringDefs.h
cb_searchform.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_searchform.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/TextF.h
cb_searchform.o: vc_searchform.h /usr/dt/include/Xm/MwmUtil.h
cb_searchform.o: /usr/openwin/include/X11/Xmd.h
cb_searchform.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
cb_searchform.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_searchform.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_searchform.o: /home/sybase/syb11/include/sybfront.h
cb_searchform.o: /home/sybase/syb11/include/cstypes.h
cb_searchform.o: /home/sybase/syb11/include/csconfig.h
cb_searchform.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_searchform.o: /home/sybase/syb11/include/sybdb.h
cb_searchform.o: /home/sybase/syb11/include/syblogin.h
cb_searchform.o: /home/sybase/syb11/include/syberror.h
cb_searchform.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_searchform.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_searchform.o: /home/aps/r2.1.1/include/global/llist.h
cb_searchform.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_searchform.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_searchform.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_searchform.o: cb_searchform.h /home/aps/r2.1.1/include/local/aps_extern.h
cb_searchform.o: gui_utils.h
cb_sortform.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cb_sortform.o: /usr/include/string.h /usr/dt/include/Xm/List.h
cb_sortform.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
cb_sortform.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
cb_sortform.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
cb_sortform.o: /usr/openwin/include/X11/X.h
cb_sortform.o: /usr/openwin/include/X11/Xfuncproto.h
cb_sortform.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
cb_sortform.o: /usr/openwin/include/X11/Xutil.h
cb_sortform.o: /usr/openwin/include/X11/Xresource.h
cb_sortform.o: /usr/openwin/include/X11/Core.h
cb_sortform.o: /usr/openwin/include/X11/Composite.h
cb_sortform.o: /usr/openwin/include/X11/Constraint.h
cb_sortform.o: /usr/openwin/include/X11/Object.h
cb_sortform.o: /usr/openwin/include/X11/RectObj.h
cb_sortform.o: /usr/openwin/include/X11/Shell.h
cb_sortform.o: /usr/openwin/include/X11/Xatom.h
cb_sortform.o: /usr/dt/include/Xm/XmStrDefs.h
cb_sortform.o: /usr/openwin/include/X11/StringDefs.h
cb_sortform.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
cb_sortform.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/TextF.h
cb_sortform.o: /usr/dt/include/Xm/ToggleB.h vc_sortform.h
cb_sortform.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
cb_sortform.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
cb_sortform.o: /home/aps/r2.1.1/include/global/dapps_defs.h
cb_sortform.o: /home/aps/r2.1.1/include/local/aps_extern.h
cb_sortform.o: /home/aps/r2.1.1/include/global/db_sybint.h
cb_sortform.o: /home/sybase/syb11/include/sybfront.h
cb_sortform.o: /home/sybase/syb11/include/cstypes.h
cb_sortform.o: /home/sybase/syb11/include/csconfig.h
cb_sortform.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cb_sortform.o: /home/sybase/syb11/include/sybdb.h
cb_sortform.o: /home/sybase/syb11/include/syblogin.h
cb_sortform.o: /home/sybase/syb11/include/syberror.h
cb_sortform.o: /home/aps/r2.1.1/include/global/dapps_list.h
cb_sortform.o: /home/aps/r2.1.1/include/global/llist_err.h
cb_sortform.o: /home/aps/r2.1.1/include/global/llist.h
cb_sortform.o: /home/aps/r2.1.1/include/global/ord_llist.h
cb_sortform.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cb_sortform.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cb_sortform.o: cb_sortform.h gui_utils.h
gui_mu_utils.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
gui_mu_utils.o: /usr/include/stdlib.h /usr/include/string.h
gui_mu_utils.o: /usr/include/errno.h /usr/include/sys/errno.h
gui_mu_utils.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
gui_mu_utils.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
gui_mu_utils.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
gui_mu_utils.o: /usr/openwin/include/X11/X.h
gui_mu_utils.o: /usr/openwin/include/X11/Xfuncproto.h
gui_mu_utils.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
gui_mu_utils.o: /usr/openwin/include/X11/Xutil.h
gui_mu_utils.o: /usr/openwin/include/X11/Xresource.h
gui_mu_utils.o: /usr/openwin/include/X11/Core.h
gui_mu_utils.o: /usr/openwin/include/X11/Composite.h
gui_mu_utils.o: /usr/openwin/include/X11/Constraint.h
gui_mu_utils.o: /usr/openwin/include/X11/Object.h
gui_mu_utils.o: /usr/openwin/include/X11/RectObj.h
gui_mu_utils.o: /usr/openwin/include/X11/Shell.h
gui_mu_utils.o: /usr/openwin/include/X11/Xatom.h
gui_mu_utils.o: /usr/dt/include/Xm/XmStrDefs.h
gui_mu_utils.o: /usr/openwin/include/X11/StringDefs.h
gui_mu_utils.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
gui_mu_utils.o: /home/aps/r2.1.1/include/global/dapps_defs.h
gui_mu_utils.o: /home/aps/r2.1.1/include/local/aps_extern.h
gui_mu_utils.o: /home/aps/r2.1.1/include/global/db_sybint.h
gui_mu_utils.o: /home/sybase/syb11/include/sybfront.h
gui_mu_utils.o: /home/sybase/syb11/include/cstypes.h
gui_mu_utils.o: /home/sybase/syb11/include/csconfig.h
gui_mu_utils.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
gui_mu_utils.o: /home/sybase/syb11/include/sybdb.h
gui_mu_utils.o: /home/sybase/syb11/include/syblogin.h
gui_mu_utils.o: /home/sybase/syb11/include/syberror.h
gui_mu_utils.o: /home/aps/r2.1.1/include/global/dapps_list.h
gui_mu_utils.o: /home/aps/r2.1.1/include/global/llist_err.h
gui_mu_utils.o: /home/aps/r2.1.1/include/global/llist.h
gui_mu_utils.o: /home/aps/r2.1.1/include/global/ord_llist.h
gui_mu_utils.o: /home/aps/r2.1.1/include/local/aps_db_table.h
gui_mu_utils.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
gui_mu_utils.o: /home/aps/r2.1.1/include/local/db_active_planning_activities.h
gui_mu_utils.o: /home/aps/r2.1.1/include/local/db_active_dar_activities.h
gui_mu_utils.o: /home/aps/r2.1.1/include/local/mu_utilities.h gui_mu_utils.h
gui_utils.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
gui_utils.o: /usr/include/ctype.h /usr/include/sys/stat.h
gui_utils.o: /usr/include/sys/time.h /usr/include/sys/types.h
gui_utils.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
gui_utils.o: /usr/include/unistd.h /usr/include/sys/unistd.h
gui_utils.o: /usr/dt/include/Xm/DialogS.h /usr/dt/include/Xm/Xm.h
gui_utils.o: /usr/openwin/include/X11/Intrinsic.h
gui_utils.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
gui_utils.o: /usr/openwin/include/X11/Xfuncproto.h
gui_utils.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
gui_utils.o: /usr/openwin/include/X11/Xutil.h
gui_utils.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
gui_utils.o: /usr/openwin/include/X11/Core.h
gui_utils.o: /usr/openwin/include/X11/Composite.h
gui_utils.o: /usr/openwin/include/X11/Constraint.h
gui_utils.o: /usr/openwin/include/X11/Object.h
gui_utils.o: /usr/openwin/include/X11/RectObj.h
gui_utils.o: /usr/openwin/include/X11/Shell.h
gui_utils.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
gui_utils.o: /usr/openwin/include/X11/StringDefs.h
gui_utils.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
gui_utils.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/MessageB.h
gui_utils.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/RowColumn.h
gui_utils.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/TextF.h
gui_utils.o: /usr/openwin/include/X11/cursorfont.h
gui_utils.o: /home/aps/r2.1.1/include/global/dapps_defs.h gui_utils.h
gui_utils.o: gui_defs.h cb_datetime.h
reqq_phase.o: /home/aps/r2.1.1/include/global/dapps_defs.h
reqq_phase.o: /home/aps/r2.1.1/include/global/db_sybint.h
reqq_phase.o: /home/sybase/syb11/include/sybfront.h
reqq_phase.o: /home/sybase/syb11/include/cstypes.h
reqq_phase.o: /home/sybase/syb11/include/csconfig.h
reqq_phase.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
reqq_phase.o: /home/sybase/syb11/include/sybdb.h /usr/include/stdio.h
reqq_phase.o: /usr/include/sys/feature_tests.h
reqq_phase.o: /home/sybase/syb11/include/syblogin.h
reqq_phase.o: /home/sybase/syb11/include/syberror.h
reqq_phase.o: /home/aps/r2.1.1/include/global/dapps_list.h
reqq_phase.o: /home/aps/r2.1.1/include/global/llist_err.h
reqq_phase.o: /home/aps/r2.1.1/include/global/llist.h
reqq_phase.o: /home/aps/r2.1.1/include/global/ord_llist.h
reqq_phase.o: /home/aps/r2.1.1/include/local/aps_db_table.h
reqq_phase.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
reqq_phase.o: /home/aps/r2.1.1/include/local/db_reqq_phase.h
reqq_phase.o: /home/aps/r2.1.1/include/local/timeconv.h /usr/include/string.h
satmenus.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
satmenus.o: /usr/include/stdlib.h /usr/include/unistd.h
satmenus.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
satmenus.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
satmenus.o: /usr/include/malloc.h /usr/include/string.h
satmenus.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
satmenus.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
satmenus.o: /usr/openwin/include/X11/Xfuncproto.h
satmenus.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
satmenus.o: /usr/openwin/include/X11/Xutil.h
satmenus.o: /usr/openwin/include/X11/Xresource.h
satmenus.o: /usr/openwin/include/X11/Core.h
satmenus.o: /usr/openwin/include/X11/Composite.h
satmenus.o: /usr/openwin/include/X11/Constraint.h
satmenus.o: /usr/openwin/include/X11/Object.h
satmenus.o: /usr/openwin/include/X11/RectObj.h
satmenus.o: /usr/openwin/include/X11/Shell.h /usr/openwin/include/X11/Xatom.h
satmenus.o: /usr/dt/include/Xm/XmStrDefs.h
satmenus.o: /usr/openwin/include/X11/StringDefs.h
satmenus.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
satmenus.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/PushBG.h
satmenus.o: /usr/dt/include/Xm/RowColumn.h UxXt.h
satmenus.o: /home/aps/r2.1.1/include/global/dapps_defs.h
satmenus.o: /home/aps/r2.1.1/include/local/aps.h
satmenus.o: /home/aps/r2.1.1/include/local/aps_defs.h
satmenus.o: /home/aps/r2.1.1/include/global/db_sybint.h
satmenus.o: /home/sybase/syb11/include/sybfront.h
satmenus.o: /home/sybase/syb11/include/cstypes.h
satmenus.o: /home/sybase/syb11/include/csconfig.h
satmenus.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
satmenus.o: /home/sybase/syb11/include/sybdb.h
satmenus.o: /home/sybase/syb11/include/syblogin.h
satmenus.o: /home/sybase/syb11/include/syberror.h
satmenus.o: /home/aps/r2.1.1/include/global/dapps_list.h
satmenus.o: /home/aps/r2.1.1/include/global/llist_err.h
satmenus.o: /home/aps/r2.1.1/include/global/llist.h
satmenus.o: /home/aps/r2.1.1/include/global/ord_llist.h
satmenus.o: /home/aps/r2.1.1/include/local/aps_db_table.h
satmenus.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
satmenus.o: /home/aps/r2.1.1/include/local/db_satsensor.h
satmenus.o: /home/aps/r2.1.1/include/local/db_station.h
satmenus.o: /home/aps/r2.1.1/include/local/db_antenna.h
satmenus.o: /home/aps/r2.1.1/include/local/aps_extern.h
satmenus.o: /home/aps/r2.1.1/include/global/nmalloc.h satmenus.h gui_utils.h
sscvcheckq.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
sscvcheckq.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
sscvcheckq.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
sscvcheckq.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
sscvcheckq.o: /usr/openwin/include/X11/X.h
sscvcheckq.o: /usr/openwin/include/X11/Xfuncproto.h
sscvcheckq.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
sscvcheckq.o: /usr/openwin/include/X11/Xutil.h
sscvcheckq.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
sscvcheckq.o: /usr/openwin/include/X11/Core.h
sscvcheckq.o: /usr/openwin/include/X11/Composite.h
sscvcheckq.o: /usr/openwin/include/X11/Constraint.h
sscvcheckq.o: /usr/openwin/include/X11/Object.h
sscvcheckq.o: /usr/openwin/include/X11/RectObj.h
sscvcheckq.o: /usr/openwin/include/X11/Shell.h
sscvcheckq.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
sscvcheckq.o: /usr/openwin/include/X11/StringDefs.h
sscvcheckq.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
sscvcheckq.o: /home/aps/r2.1.1/include/local/mpsf77calls.h
subprocess.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
subprocess.o: /usr/include/unistd.h /usr/include/sys/types.h
subprocess.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
subprocess.o: /usr/include/sys/unistd.h /usr/include/stdlib.h
subprocess.o: /usr/include/errno.h /usr/include/sys/errno.h
subprocess.o: /usr/include/string.h /usr/include/signal.h
subprocess.o: /usr/include/sys/signal.h /usr/include/fcntl.h
subprocess.o: /usr/include/sys/fcntl.h /usr/include/sys/stat.h
subprocess.o: /usr/include/sys/time.h /usr/include/sys/wait.h
subprocess.o: /usr/include/sys/resource.h /usr/dt/include/Xm/XmP.h
subprocess.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
subprocess.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
subprocess.o: /usr/openwin/include/X11/Xfuncproto.h
subprocess.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
subprocess.o: /usr/openwin/include/X11/Xutil.h
subprocess.o: /usr/openwin/include/X11/Xresource.h
subprocess.o: /usr/openwin/include/X11/Core.h
subprocess.o: /usr/openwin/include/X11/Composite.h
subprocess.o: /usr/openwin/include/X11/Constraint.h
subprocess.o: /usr/openwin/include/X11/Object.h
subprocess.o: /usr/openwin/include/X11/RectObj.h
subprocess.o: /usr/openwin/include/X11/Shell.h
subprocess.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
subprocess.o: /usr/openwin/include/X11/StringDefs.h
subprocess.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
subprocess.o: /usr/openwin/include/X11/IntrinsicP.h
subprocess.o: /usr/openwin/include/X11/CoreP.h
subprocess.o: /usr/openwin/include/X11/CompositeP.h
subprocess.o: /usr/openwin/include/X11/ConstrainP.h
subprocess.o: /usr/openwin/include/X11/ObjectP.h
subprocess.o: /usr/openwin/include/X11/RectObjP.h
subprocess.o: /usr/dt/include/Xm/BaseClassP.h
subprocess.o: /home/aps/r2.1.1/include/global/dapps_defs.h subprocess.h
subprocess.o: /home/aps/r2.1.1/include/local/apspath.h
subprocess.o: /home/aps/r2.1.1/include/local/apsfiledef.h
subprocess.o: /home/aps/r2.1.1/include/local/aps_defs.h
subprocess.o: /home/aps/r2.1.1/include/local/timeconv.h
vc_antdntimes.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_antdntimes.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_antdntimes.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_antdntimes.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_antdntimes.o: /usr/openwin/include/X11/X.h
vc_antdntimes.o: /usr/openwin/include/X11/Xfuncproto.h
vc_antdntimes.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_antdntimes.o: /usr/openwin/include/X11/Xutil.h
vc_antdntimes.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_antdntimes.o: /usr/openwin/include/X11/Core.h
vc_antdntimes.o: /usr/openwin/include/X11/Composite.h
vc_antdntimes.o: /usr/openwin/include/X11/Constraint.h
vc_antdntimes.o: /usr/openwin/include/X11/Object.h
vc_antdntimes.o: /usr/openwin/include/X11/RectObj.h
vc_antdntimes.o: /usr/openwin/include/X11/Shell.h
vc_antdntimes.o: /usr/openwin/include/X11/Xatom.h
vc_antdntimes.o: /usr/dt/include/Xm/XmStrDefs.h
vc_antdntimes.o: /usr/openwin/include/X11/StringDefs.h
vc_antdntimes.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_antdntimes.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_antdntimes.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_antdntimes.o: /usr/dt/include/Xm/RowColumn.h /usr/dt/include/Xm/Text.h
vc_antdntimes.o: /usr/dt/include/Xm/TextF.h /usr/dt/include/Xm/Separator.h
vc_antdntimes.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/List.h
vc_antdntimes.o: /usr/dt/include/Xm/ScrolledW.h /usr/dt/include/Xm/Label.h
vc_antdntimes.o: /usr/dt/include/Xm/Form.h /usr/dt/include/Xm/BulletinB.h
vc_antdntimes.o: /usr/dt/include/Xm/DialogS.h
vc_antdntimes.o: /home/aps/r2.1.1/include/global/db_sybint.h
vc_antdntimes.o: /home/sybase/syb11/include/sybfront.h
vc_antdntimes.o: /home/sybase/syb11/include/cstypes.h
vc_antdntimes.o: /home/sybase/syb11/include/csconfig.h
vc_antdntimes.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
vc_antdntimes.o: /home/sybase/syb11/include/sybdb.h
vc_antdntimes.o: /home/sybase/syb11/include/syblogin.h
vc_antdntimes.o: /home/sybase/syb11/include/syberror.h
vc_antdntimes.o: /home/aps/r2.1.1/include/global/dapps_list.h
vc_antdntimes.o: /home/aps/r2.1.1/include/global/llist_err.h
vc_antdntimes.o: /home/aps/r2.1.1/include/global/llist.h
vc_antdntimes.o: /home/aps/r2.1.1/include/global/ord_llist.h
vc_antdntimes.o: /home/aps/r2.1.1/include/local/aps_db_table.h
vc_antdntimes.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h satmenus.h
vc_antdntimes.o: cb_sortform.h cb_searchform.h cb_antdntimes.h cb_datetime.h
vc_antdntimes.o: vc_antdntimes.h
vc_apslogin.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_apslogin.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_apslogin.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_apslogin.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_apslogin.o: /usr/openwin/include/X11/X.h
vc_apslogin.o: /usr/openwin/include/X11/Xfuncproto.h
vc_apslogin.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_apslogin.o: /usr/openwin/include/X11/Xutil.h
vc_apslogin.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_apslogin.o: /usr/openwin/include/X11/Core.h
vc_apslogin.o: /usr/openwin/include/X11/Composite.h
vc_apslogin.o: /usr/openwin/include/X11/Constraint.h
vc_apslogin.o: /usr/openwin/include/X11/Object.h
vc_apslogin.o: /usr/openwin/include/X11/RectObj.h
vc_apslogin.o: /usr/openwin/include/X11/Shell.h
vc_apslogin.o: /usr/openwin/include/X11/Xatom.h
vc_apslogin.o: /usr/dt/include/Xm/XmStrDefs.h
vc_apslogin.o: /usr/openwin/include/X11/StringDefs.h
vc_apslogin.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_apslogin.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_apslogin.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_apslogin.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/TextF.h
vc_apslogin.o: /usr/dt/include/Xm/Label.h /usr/dt/include/Xm/Form.h
vc_apslogin.o: /usr/dt/include/Xm/BulletinB.h cb_apslogin.h gui_utils.h
vc_apslogin.o: vc_apslogin.h
vc_apsmenu.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_apsmenu.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_apsmenu.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_apsmenu.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_apsmenu.o: /usr/openwin/include/X11/X.h
vc_apsmenu.o: /usr/openwin/include/X11/Xfuncproto.h
vc_apsmenu.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_apsmenu.o: /usr/openwin/include/X11/Xutil.h
vc_apsmenu.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_apsmenu.o: /usr/openwin/include/X11/Core.h
vc_apsmenu.o: /usr/openwin/include/X11/Composite.h
vc_apsmenu.o: /usr/openwin/include/X11/Constraint.h
vc_apsmenu.o: /usr/openwin/include/X11/Object.h
vc_apsmenu.o: /usr/openwin/include/X11/RectObj.h
vc_apsmenu.o: /usr/openwin/include/X11/Shell.h
vc_apsmenu.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
vc_apsmenu.o: /usr/openwin/include/X11/StringDefs.h
vc_apsmenu.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_apsmenu.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_apsmenu.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_apsmenu.o: /usr/dt/include/Xm/RepType.h /usr/dt/include/Xm/Label.h
vc_apsmenu.o: /usr/dt/include/Xm/Form.h /usr/dt/include/Xm/BulletinB.h
vc_apsmenu.o: /usr/dt/include/Xm/CascadeB.h /usr/dt/include/Xm/Separator.h
vc_apsmenu.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/RowColumn.h
vc_apsmenu.o: /usr/dt/include/Xm/MainW.h
vc_apsmenu.o: /home/aps/r2.1.1/include/local/aps_defs.h gui_defs.h
vc_apsmenu.o: cb_apsmenu.h cb_rgsdntimes.h gui_utils.h vc_apsmenu.h
vc_cdtakeopps.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_cdtakeopps.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_cdtakeopps.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_cdtakeopps.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_cdtakeopps.o: /usr/openwin/include/X11/X.h
vc_cdtakeopps.o: /usr/openwin/include/X11/Xfuncproto.h
vc_cdtakeopps.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_cdtakeopps.o: /usr/openwin/include/X11/Xutil.h
vc_cdtakeopps.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_cdtakeopps.o: /usr/openwin/include/X11/Core.h
vc_cdtakeopps.o: /usr/openwin/include/X11/Composite.h
vc_cdtakeopps.o: /usr/openwin/include/X11/Constraint.h
vc_cdtakeopps.o: /usr/openwin/include/X11/Object.h
vc_cdtakeopps.o: /usr/openwin/include/X11/RectObj.h
vc_cdtakeopps.o: /usr/openwin/include/X11/Shell.h
vc_cdtakeopps.o: /usr/openwin/include/X11/Xatom.h
vc_cdtakeopps.o: /usr/dt/include/Xm/XmStrDefs.h
vc_cdtakeopps.o: /usr/openwin/include/X11/StringDefs.h
vc_cdtakeopps.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_cdtakeopps.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_cdtakeopps.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_cdtakeopps.o: /usr/dt/include/Xm/CascadeB.h /usr/dt/include/Xm/PushB.h
vc_cdtakeopps.o: /usr/dt/include/Xm/Separator.h /usr/dt/include/Xm/Text.h
vc_cdtakeopps.o: /usr/dt/include/Xm/ToggleB.h /usr/dt/include/Xm/RowColumn.h
vc_cdtakeopps.o: /usr/dt/include/Xm/TextF.h /usr/dt/include/Xm/List.h
vc_cdtakeopps.o: /usr/dt/include/Xm/ScrolledW.h /usr/dt/include/Xm/Label.h
vc_cdtakeopps.o: /usr/dt/include/Xm/Form.h /usr/dt/include/Xm/BulletinB.h
vc_cdtakeopps.o: cb_cdtakeopps.h cb_datetime.h satmenus.h
vc_cdtakeopps.o: /home/aps/r2.1.1/include/global/dapps_list.h
vc_cdtakeopps.o: /home/aps/r2.1.1/include/global/llist_err.h
vc_cdtakeopps.o: /home/aps/r2.1.1/include/global/llist.h
vc_cdtakeopps.o: /home/aps/r2.1.1/include/global/ord_llist.h vc_cdtakeopps.h
vc_cnomcov.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_cnomcov.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_cnomcov.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_cnomcov.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_cnomcov.o: /usr/openwin/include/X11/X.h
vc_cnomcov.o: /usr/openwin/include/X11/Xfuncproto.h
vc_cnomcov.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_cnomcov.o: /usr/openwin/include/X11/Xutil.h
vc_cnomcov.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_cnomcov.o: /usr/openwin/include/X11/Core.h
vc_cnomcov.o: /usr/openwin/include/X11/Composite.h
vc_cnomcov.o: /usr/openwin/include/X11/Constraint.h
vc_cnomcov.o: /usr/openwin/include/X11/Object.h
vc_cnomcov.o: /usr/openwin/include/X11/RectObj.h
vc_cnomcov.o: /usr/openwin/include/X11/Shell.h
vc_cnomcov.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
vc_cnomcov.o: /usr/openwin/include/X11/StringDefs.h
vc_cnomcov.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_cnomcov.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_cnomcov.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_cnomcov.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/RowColumn.h
vc_cnomcov.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/ScrolledW.h
vc_cnomcov.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/ToggleB.h
vc_cnomcov.o: /usr/dt/include/Xm/Separator.h /usr/dt/include/Xm/TextF.h
vc_cnomcov.o: /usr/dt/include/Xm/Label.h /usr/dt/include/Xm/Form.h
vc_cnomcov.o: /usr/dt/include/Xm/BulletinB.h cb_cnomcov.h cb_datetime.h
vc_cnomcov.o: satmenus.h /home/aps/r2.1.1/include/global/dapps_list.h
vc_cnomcov.o: /home/aps/r2.1.1/include/global/llist_err.h
vc_cnomcov.o: /home/aps/r2.1.1/include/global/llist.h
vc_cnomcov.o: /home/aps/r2.1.1/include/global/ord_llist.h vc_cnomcov.h
vc_cnomorb.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_cnomorb.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_cnomorb.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_cnomorb.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_cnomorb.o: /usr/openwin/include/X11/X.h
vc_cnomorb.o: /usr/openwin/include/X11/Xfuncproto.h
vc_cnomorb.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_cnomorb.o: /usr/openwin/include/X11/Xutil.h
vc_cnomorb.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_cnomorb.o: /usr/openwin/include/X11/Core.h
vc_cnomorb.o: /usr/openwin/include/X11/Composite.h
vc_cnomorb.o: /usr/openwin/include/X11/Constraint.h
vc_cnomorb.o: /usr/openwin/include/X11/Object.h
vc_cnomorb.o: /usr/openwin/include/X11/RectObj.h
vc_cnomorb.o: /usr/openwin/include/X11/Shell.h
vc_cnomorb.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
vc_cnomorb.o: /usr/openwin/include/X11/StringDefs.h
vc_cnomorb.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_cnomorb.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_cnomorb.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_cnomorb.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/PushB.h
vc_cnomorb.o: /usr/dt/include/Xm/TextF.h /usr/dt/include/Xm/Separator.h
vc_cnomorb.o: /usr/dt/include/Xm/Label.h /usr/dt/include/Xm/List.h
vc_cnomorb.o: /usr/dt/include/Xm/ScrolledW.h /usr/dt/include/Xm/Form.h
vc_cnomorb.o: /usr/dt/include/Xm/BulletinB.h cb_cnomorb.h vc_cnomorb.h
vc_conrndup.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_conrndup.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_conrndup.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_conrndup.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_conrndup.o: /usr/openwin/include/X11/X.h
vc_conrndup.o: /usr/openwin/include/X11/Xfuncproto.h
vc_conrndup.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_conrndup.o: /usr/openwin/include/X11/Xutil.h
vc_conrndup.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_conrndup.o: /usr/openwin/include/X11/Core.h
vc_conrndup.o: /usr/openwin/include/X11/Composite.h
vc_conrndup.o: /usr/openwin/include/X11/Constraint.h
vc_conrndup.o: /usr/openwin/include/X11/Object.h
vc_conrndup.o: /usr/openwin/include/X11/RectObj.h
vc_conrndup.o: /usr/openwin/include/X11/Shell.h
vc_conrndup.o: /usr/openwin/include/X11/Xatom.h
vc_conrndup.o: /usr/dt/include/Xm/XmStrDefs.h
vc_conrndup.o: /usr/openwin/include/X11/StringDefs.h
vc_conrndup.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_conrndup.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_conrndup.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_conrndup.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/RowColumn.h
vc_conrndup.o: /usr/dt/include/Xm/TextF.h /usr/dt/include/Xm/Label.h
vc_conrndup.o: /usr/dt/include/Xm/Form.h /usr/dt/include/Xm/BulletinB.h
vc_conrndup.o: gui_utils.h satmenus.h
vc_conrndup.o: /home/aps/r2.1.1/include/global/dapps_list.h
vc_conrndup.o: /home/aps/r2.1.1/include/global/llist_err.h
vc_conrndup.o: /home/aps/r2.1.1/include/global/llist.h
vc_conrndup.o: /home/aps/r2.1.1/include/global/ord_llist.h cb_datetime.h
vc_conrndup.o: cb_conrndup.h vc_conrndup.h
vc_crtapsfile.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_crtapsfile.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_crtapsfile.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_crtapsfile.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_crtapsfile.o: /usr/openwin/include/X11/X.h
vc_crtapsfile.o: /usr/openwin/include/X11/Xfuncproto.h
vc_crtapsfile.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_crtapsfile.o: /usr/openwin/include/X11/Xutil.h
vc_crtapsfile.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_crtapsfile.o: /usr/openwin/include/X11/Core.h
vc_crtapsfile.o: /usr/openwin/include/X11/Composite.h
vc_crtapsfile.o: /usr/openwin/include/X11/Constraint.h
vc_crtapsfile.o: /usr/openwin/include/X11/Object.h
vc_crtapsfile.o: /usr/openwin/include/X11/RectObj.h
vc_crtapsfile.o: /usr/openwin/include/X11/Shell.h
vc_crtapsfile.o: /usr/openwin/include/X11/Xatom.h
vc_crtapsfile.o: /usr/dt/include/Xm/XmStrDefs.h
vc_crtapsfile.o: /usr/openwin/include/X11/StringDefs.h
vc_crtapsfile.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_crtapsfile.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_crtapsfile.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_crtapsfile.o: /usr/dt/include/Xm/ToggleB.h /usr/dt/include/Xm/TextF.h
vc_crtapsfile.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/PushB.h
vc_crtapsfile.o: /usr/dt/include/Xm/Separator.h /usr/dt/include/Xm/List.h
vc_crtapsfile.o: /usr/dt/include/Xm/ScrolledW.h /usr/dt/include/Xm/Label.h
vc_crtapsfile.o: /usr/dt/include/Xm/Form.h /usr/dt/include/Xm/BulletinB.h
vc_crtapsfile.o: cb_datetime.h cb_crtapsfile.h cb_fileviewer.h
vc_crtapsfile.o: vc_crtapsfile.h
vc_darmanager.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_darmanager.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_darmanager.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_darmanager.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_darmanager.o: /usr/openwin/include/X11/X.h
vc_darmanager.o: /usr/openwin/include/X11/Xfuncproto.h
vc_darmanager.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_darmanager.o: /usr/openwin/include/X11/Xutil.h
vc_darmanager.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_darmanager.o: /usr/openwin/include/X11/Core.h
vc_darmanager.o: /usr/openwin/include/X11/Composite.h
vc_darmanager.o: /usr/openwin/include/X11/Constraint.h
vc_darmanager.o: /usr/openwin/include/X11/Object.h
vc_darmanager.o: /usr/openwin/include/X11/RectObj.h
vc_darmanager.o: /usr/openwin/include/X11/Shell.h
vc_darmanager.o: /usr/openwin/include/X11/Xatom.h
vc_darmanager.o: /usr/dt/include/Xm/XmStrDefs.h
vc_darmanager.o: /usr/openwin/include/X11/StringDefs.h
vc_darmanager.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_darmanager.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_darmanager.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_darmanager.o: /usr/dt/include/Xm/SeparatoG.h /usr/dt/include/Xm/CascadeB.h
vc_darmanager.o: /usr/dt/include/Xm/RowColumn.h /usr/dt/include/Xm/Text.h
vc_darmanager.o: /usr/dt/include/Xm/Separator.h /usr/dt/include/Xm/PushB.h
vc_darmanager.o: /usr/dt/include/Xm/TextF.h /usr/dt/include/Xm/List.h
vc_darmanager.o: /usr/dt/include/Xm/ScrolledW.h /usr/dt/include/Xm/Label.h
vc_darmanager.o: /usr/dt/include/Xm/Form.h /usr/dt/include/Xm/BulletinB.h
vc_darmanager.o: /usr/dt/include/Xm/FileSB.h /usr/dt/include/Xm/DialogS.h
vc_darmanager.o: /home/aps/r2.1.1/include/global/db_sybint.h
vc_darmanager.o: /home/sybase/syb11/include/sybfront.h
vc_darmanager.o: /home/sybase/syb11/include/cstypes.h
vc_darmanager.o: /home/sybase/syb11/include/csconfig.h
vc_darmanager.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
vc_darmanager.o: /home/sybase/syb11/include/sybdb.h
vc_darmanager.o: /home/sybase/syb11/include/syblogin.h
vc_darmanager.o: /home/sybase/syb11/include/syberror.h
vc_darmanager.o: /home/aps/r2.1.1/include/global/dapps_list.h
vc_darmanager.o: /home/aps/r2.1.1/include/global/llist_err.h
vc_darmanager.o: /home/aps/r2.1.1/include/global/llist.h
vc_darmanager.o: /home/aps/r2.1.1/include/global/ord_llist.h
vc_darmanager.o: /home/aps/r2.1.1/include/local/aps_db_table.h
vc_darmanager.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
vc_darmanager.o: /home/aps/r2.1.1/include/local/DARconversions.h
vc_darmanager.o: /home/aps/r2.1.1/include/global/fa_defs.h gui_utils.h
vc_darmanager.o: cb_sortform.h cb_searchform.h cb_datetime.h cb_darmanager.h
vc_darmanager.o: vc_darmanager.h
vc_dtkmanager.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_dtkmanager.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_dtkmanager.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_dtkmanager.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_dtkmanager.o: /usr/openwin/include/X11/X.h
vc_dtkmanager.o: /usr/openwin/include/X11/Xfuncproto.h
vc_dtkmanager.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_dtkmanager.o: /usr/openwin/include/X11/Xutil.h
vc_dtkmanager.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_dtkmanager.o: /usr/openwin/include/X11/Core.h
vc_dtkmanager.o: /usr/openwin/include/X11/Composite.h
vc_dtkmanager.o: /usr/openwin/include/X11/Constraint.h
vc_dtkmanager.o: /usr/openwin/include/X11/Object.h
vc_dtkmanager.o: /usr/openwin/include/X11/RectObj.h
vc_dtkmanager.o: /usr/openwin/include/X11/Shell.h
vc_dtkmanager.o: /usr/openwin/include/X11/Xatom.h
vc_dtkmanager.o: /usr/dt/include/Xm/XmStrDefs.h
vc_dtkmanager.o: /usr/openwin/include/X11/StringDefs.h
vc_dtkmanager.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_dtkmanager.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_dtkmanager.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_dtkmanager.o: /usr/dt/include/Xm/Frame.h /usr/dt/include/Xm/ToggleB.h
vc_dtkmanager.o: /usr/dt/include/Xm/CascadeB.h /usr/dt/include/Xm/RowColumn.h
vc_dtkmanager.o: /usr/dt/include/Xm/Separator.h /usr/dt/include/Xm/PushB.h
vc_dtkmanager.o: /usr/dt/include/Xm/TextF.h /usr/dt/include/Xm/List.h
vc_dtkmanager.o: /usr/dt/include/Xm/ScrolledW.h /usr/dt/include/Xm/Label.h
vc_dtkmanager.o: /usr/dt/include/Xm/Form.h /usr/dt/include/Xm/BulletinB.h
vc_dtkmanager.o: /usr/dt/include/Xm/FileSB.h /usr/dt/include/Xm/DialogS.h
vc_dtkmanager.o: /home/aps/r2.1.1/include/global/db_sybint.h
vc_dtkmanager.o: /home/sybase/syb11/include/sybfront.h
vc_dtkmanager.o: /home/sybase/syb11/include/cstypes.h
vc_dtkmanager.o: /home/sybase/syb11/include/csconfig.h
vc_dtkmanager.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
vc_dtkmanager.o: /home/sybase/syb11/include/sybdb.h
vc_dtkmanager.o: /home/sybase/syb11/include/syblogin.h
vc_dtkmanager.o: /home/sybase/syb11/include/syberror.h
vc_dtkmanager.o: /home/aps/r2.1.1/include/global/dapps_list.h
vc_dtkmanager.o: /home/aps/r2.1.1/include/global/llist_err.h
vc_dtkmanager.o: /home/aps/r2.1.1/include/global/llist.h
vc_dtkmanager.o: /home/aps/r2.1.1/include/global/ord_llist.h
vc_dtkmanager.o: /home/aps/r2.1.1/include/local/aps_db_table.h
vc_dtkmanager.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
vc_dtkmanager.o: gui_utils.h satmenus.h cb_datetime.h cb_sortform.h
vc_dtkmanager.o: cb_searchform.h cb_dtkmanager.h vc_dtkmanager.h
vc_fileselectionbox.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_fileselectionbox.o: /usr/dt/include/Xm/Xm.h
vc_fileselectionbox.o: /usr/openwin/include/X11/Intrinsic.h
vc_fileselectionbox.o: /usr/openwin/include/X11/Xlib.h
vc_fileselectionbox.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
vc_fileselectionbox.o: /usr/include/sys/machtypes.h
vc_fileselectionbox.o: /usr/openwin/include/X11/X.h
vc_fileselectionbox.o: /usr/openwin/include/X11/Xfuncproto.h
vc_fileselectionbox.o: /usr/openwin/include/X11/Xosdefs.h
vc_fileselectionbox.o: /usr/include/stddef.h /usr/openwin/include/X11/Xutil.h
vc_fileselectionbox.o: /usr/openwin/include/X11/Xresource.h
vc_fileselectionbox.o: /usr/include/string.h /usr/openwin/include/X11/Core.h
vc_fileselectionbox.o: /usr/openwin/include/X11/Composite.h
vc_fileselectionbox.o: /usr/openwin/include/X11/Constraint.h
vc_fileselectionbox.o: /usr/openwin/include/X11/Object.h
vc_fileselectionbox.o: /usr/openwin/include/X11/RectObj.h
vc_fileselectionbox.o: /usr/openwin/include/X11/Shell.h
vc_fileselectionbox.o: /usr/openwin/include/X11/Xatom.h
vc_fileselectionbox.o: /usr/dt/include/Xm/XmStrDefs.h
vc_fileselectionbox.o: /usr/openwin/include/X11/StringDefs.h
vc_fileselectionbox.o: /usr/dt/include/Xm/VirtKeys.h
vc_fileselectionbox.o: /usr/dt/include/Xm/VendorS.h
vc_fileselectionbox.o: /usr/dt/include/Xm/MwmUtil.h
vc_fileselectionbox.o: /usr/openwin/include/X11/Xmd.h
vc_fileselectionbox.o: /usr/dt/include/Xm/MenuShell.h UxXt.h
vc_fileselectionbox.o: /usr/include/stdlib.h /usr/dt/include/Xm/FileSB.h
vc_fileselectionbox.o: vc_fileselectionbox.h
vc_fileviewer.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_fileviewer.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_fileviewer.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_fileviewer.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_fileviewer.o: /usr/openwin/include/X11/X.h
vc_fileviewer.o: /usr/openwin/include/X11/Xfuncproto.h
vc_fileviewer.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_fileviewer.o: /usr/openwin/include/X11/Xutil.h
vc_fileviewer.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_fileviewer.o: /usr/openwin/include/X11/Core.h
vc_fileviewer.o: /usr/openwin/include/X11/Composite.h
vc_fileviewer.o: /usr/openwin/include/X11/Constraint.h
vc_fileviewer.o: /usr/openwin/include/X11/Object.h
vc_fileviewer.o: /usr/openwin/include/X11/RectObj.h
vc_fileviewer.o: /usr/openwin/include/X11/Shell.h
vc_fileviewer.o: /usr/openwin/include/X11/Xatom.h
vc_fileviewer.o: /usr/dt/include/Xm/XmStrDefs.h
vc_fileviewer.o: /usr/openwin/include/X11/StringDefs.h
vc_fileviewer.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_fileviewer.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_fileviewer.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_fileviewer.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/Text.h
vc_fileviewer.o: /usr/dt/include/Xm/ScrolledW.h /usr/dt/include/Xm/Form.h
vc_fileviewer.o: /usr/dt/include/Xm/BulletinB.h vc_fileviewer.h
vc_msgbox.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_msgbox.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_msgbox.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_msgbox.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_msgbox.o: /usr/openwin/include/X11/X.h
vc_msgbox.o: /usr/openwin/include/X11/Xfuncproto.h
vc_msgbox.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_msgbox.o: /usr/openwin/include/X11/Xutil.h
vc_msgbox.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_msgbox.o: /usr/openwin/include/X11/Core.h
vc_msgbox.o: /usr/openwin/include/X11/Composite.h
vc_msgbox.o: /usr/openwin/include/X11/Constraint.h
vc_msgbox.o: /usr/openwin/include/X11/Object.h
vc_msgbox.o: /usr/openwin/include/X11/RectObj.h
vc_msgbox.o: /usr/openwin/include/X11/Shell.h
vc_msgbox.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
vc_msgbox.o: /usr/openwin/include/X11/StringDefs.h
vc_msgbox.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_msgbox.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_msgbox.o: /usr/dt/include/Xm/DialogS.h /usr/dt/include/Xm/MenuShell.h
vc_msgbox.o: UxXt.h /usr/include/stdlib.h /usr/dt/include/Xm/Text.h
vc_msgbox.o: /usr/dt/include/Xm/ScrolledW.h /usr/dt/include/Xm/MessageB.h
vc_msgbox.o: gui_utils.h vc_msgbox.h
vc_muinterval.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_muinterval.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_muinterval.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_muinterval.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_muinterval.o: /usr/openwin/include/X11/X.h
vc_muinterval.o: /usr/openwin/include/X11/Xfuncproto.h
vc_muinterval.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_muinterval.o: /usr/openwin/include/X11/Xutil.h
vc_muinterval.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_muinterval.o: /usr/openwin/include/X11/Core.h
vc_muinterval.o: /usr/openwin/include/X11/Composite.h
vc_muinterval.o: /usr/openwin/include/X11/Constraint.h
vc_muinterval.o: /usr/openwin/include/X11/Object.h
vc_muinterval.o: /usr/openwin/include/X11/RectObj.h
vc_muinterval.o: /usr/openwin/include/X11/Shell.h
vc_muinterval.o: /usr/openwin/include/X11/Xatom.h
vc_muinterval.o: /usr/dt/include/Xm/XmStrDefs.h
vc_muinterval.o: /usr/openwin/include/X11/StringDefs.h
vc_muinterval.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_muinterval.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_muinterval.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_muinterval.o: /usr/dt/include/Xm/TextF.h /usr/dt/include/Xm/Label.h
vc_muinterval.o: /usr/dt/include/Xm/RowColumn.h /usr/dt/include/Xm/MessageB.h
vc_muinterval.o: /usr/dt/include/Xm/DialogS.h gui_utils.h cb_permstatus.h
vc_muinterval.o: vc_muinterval.h
vc_permstatus.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_permstatus.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_permstatus.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_permstatus.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_permstatus.o: /usr/openwin/include/X11/X.h
vc_permstatus.o: /usr/openwin/include/X11/Xfuncproto.h
vc_permstatus.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_permstatus.o: /usr/openwin/include/X11/Xutil.h
vc_permstatus.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_permstatus.o: /usr/openwin/include/X11/Core.h
vc_permstatus.o: /usr/openwin/include/X11/Composite.h
vc_permstatus.o: /usr/openwin/include/X11/Constraint.h
vc_permstatus.o: /usr/openwin/include/X11/Object.h
vc_permstatus.o: /usr/openwin/include/X11/RectObj.h
vc_permstatus.o: /usr/openwin/include/X11/Shell.h
vc_permstatus.o: /usr/openwin/include/X11/Xatom.h
vc_permstatus.o: /usr/dt/include/Xm/XmStrDefs.h
vc_permstatus.o: /usr/openwin/include/X11/StringDefs.h
vc_permstatus.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_permstatus.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_permstatus.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_permstatus.o: /usr/dt/include/Xm/Label.h /usr/dt/include/Xm/PushB.h
vc_permstatus.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/ScrolledW.h
vc_permstatus.o: /usr/dt/include/Xm/ToggleB.h /usr/dt/include/Xm/Form.h
vc_permstatus.o: /usr/dt/include/Xm/BulletinB.h cb_permstatus.h
vc_permstatus.o: vc_permstatus.h
vc_phaseselect.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_phaseselect.o: /usr/dt/include/Xm/Xm.h
vc_phaseselect.o: /usr/openwin/include/X11/Intrinsic.h
vc_phaseselect.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_phaseselect.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_phaseselect.o: /usr/openwin/include/X11/X.h
vc_phaseselect.o: /usr/openwin/include/X11/Xfuncproto.h
vc_phaseselect.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_phaseselect.o: /usr/openwin/include/X11/Xutil.h
vc_phaseselect.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_phaseselect.o: /usr/openwin/include/X11/Core.h
vc_phaseselect.o: /usr/openwin/include/X11/Composite.h
vc_phaseselect.o: /usr/openwin/include/X11/Constraint.h
vc_phaseselect.o: /usr/openwin/include/X11/Object.h
vc_phaseselect.o: /usr/openwin/include/X11/RectObj.h
vc_phaseselect.o: /usr/openwin/include/X11/Shell.h
vc_phaseselect.o: /usr/openwin/include/X11/Xatom.h
vc_phaseselect.o: /usr/dt/include/Xm/XmStrDefs.h
vc_phaseselect.o: /usr/openwin/include/X11/StringDefs.h
vc_phaseselect.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_phaseselect.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_phaseselect.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_phaseselect.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/List.h
vc_phaseselect.o: /usr/dt/include/Xm/ScrolledW.h /usr/dt/include/Xm/Label.h
vc_phaseselect.o: /usr/dt/include/Xm/Form.h /usr/dt/include/Xm/BulletinB.h
vc_phaseselect.o: vc_phaseselect.h
vc_prcapsfile.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_prcapsfile.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_prcapsfile.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_prcapsfile.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_prcapsfile.o: /usr/openwin/include/X11/X.h
vc_prcapsfile.o: /usr/openwin/include/X11/Xfuncproto.h
vc_prcapsfile.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_prcapsfile.o: /usr/openwin/include/X11/Xutil.h
vc_prcapsfile.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_prcapsfile.o: /usr/openwin/include/X11/Core.h
vc_prcapsfile.o: /usr/openwin/include/X11/Composite.h
vc_prcapsfile.o: /usr/openwin/include/X11/Constraint.h
vc_prcapsfile.o: /usr/openwin/include/X11/Object.h
vc_prcapsfile.o: /usr/openwin/include/X11/RectObj.h
vc_prcapsfile.o: /usr/openwin/include/X11/Shell.h
vc_prcapsfile.o: /usr/openwin/include/X11/Xatom.h
vc_prcapsfile.o: /usr/dt/include/Xm/XmStrDefs.h
vc_prcapsfile.o: /usr/openwin/include/X11/StringDefs.h
vc_prcapsfile.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_prcapsfile.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_prcapsfile.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_prcapsfile.o: /usr/dt/include/Xm/CascadeB.h /usr/dt/include/Xm/RowColumn.h
vc_prcapsfile.o: /usr/dt/include/Xm/Separator.h /usr/dt/include/Xm/Text.h
vc_prcapsfile.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/List.h
vc_prcapsfile.o: /usr/dt/include/Xm/ScrolledW.h /usr/dt/include/Xm/Label.h
vc_prcapsfile.o: /usr/dt/include/Xm/Form.h /usr/dt/include/Xm/BulletinB.h
vc_prcapsfile.o: /home/aps/r2.1.1/include/local/apsfiledef.h gui_utils.h
vc_prcapsfile.o: cb_prcapsfile.h vc_prcapsfile.h
vc_rgsdntimes.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_rgsdntimes.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_rgsdntimes.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_rgsdntimes.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_rgsdntimes.o: /usr/openwin/include/X11/X.h
vc_rgsdntimes.o: /usr/openwin/include/X11/Xfuncproto.h
vc_rgsdntimes.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_rgsdntimes.o: /usr/openwin/include/X11/Xutil.h
vc_rgsdntimes.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_rgsdntimes.o: /usr/openwin/include/X11/Core.h
vc_rgsdntimes.o: /usr/openwin/include/X11/Composite.h
vc_rgsdntimes.o: /usr/openwin/include/X11/Constraint.h
vc_rgsdntimes.o: /usr/openwin/include/X11/Object.h
vc_rgsdntimes.o: /usr/openwin/include/X11/RectObj.h
vc_rgsdntimes.o: /usr/openwin/include/X11/Shell.h
vc_rgsdntimes.o: /usr/openwin/include/X11/Xatom.h
vc_rgsdntimes.o: /usr/dt/include/Xm/XmStrDefs.h
vc_rgsdntimes.o: /usr/openwin/include/X11/StringDefs.h
vc_rgsdntimes.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_rgsdntimes.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_rgsdntimes.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_rgsdntimes.o: /usr/dt/include/Xm/ToggleB.h /usr/dt/include/Xm/Text.h
vc_rgsdntimes.o: /usr/dt/include/Xm/RowColumn.h /usr/dt/include/Xm/TextF.h
vc_rgsdntimes.o: /usr/dt/include/Xm/Separator.h /usr/dt/include/Xm/PushB.h
vc_rgsdntimes.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/ScrolledW.h
vc_rgsdntimes.o: /usr/dt/include/Xm/Label.h /usr/dt/include/Xm/Form.h
vc_rgsdntimes.o: /usr/dt/include/Xm/BulletinB.h /usr/dt/include/Xm/DialogS.h
vc_rgsdntimes.o: /home/aps/r2.1.1/include/global/db_sybint.h
vc_rgsdntimes.o: /home/sybase/syb11/include/sybfront.h
vc_rgsdntimes.o: /home/sybase/syb11/include/cstypes.h
vc_rgsdntimes.o: /home/sybase/syb11/include/csconfig.h
vc_rgsdntimes.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
vc_rgsdntimes.o: /home/sybase/syb11/include/sybdb.h
vc_rgsdntimes.o: /home/sybase/syb11/include/syblogin.h
vc_rgsdntimes.o: /home/sybase/syb11/include/syberror.h
vc_rgsdntimes.o: /home/aps/r2.1.1/include/global/dapps_list.h
vc_rgsdntimes.o: /home/aps/r2.1.1/include/global/llist_err.h
vc_rgsdntimes.o: /home/aps/r2.1.1/include/global/llist.h
vc_rgsdntimes.o: /home/aps/r2.1.1/include/global/ord_llist.h
vc_rgsdntimes.o: /home/aps/r2.1.1/include/local/aps_db_table.h
vc_rgsdntimes.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h satmenus.h
vc_rgsdntimes.o: cb_sortform.h cb_searchform.h cb_rgsdntimes.h cb_datetime.h
vc_rgsdntimes.o: vc_rgsdntimes.h
vc_searchform.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_searchform.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_searchform.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_searchform.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_searchform.o: /usr/openwin/include/X11/X.h
vc_searchform.o: /usr/openwin/include/X11/Xfuncproto.h
vc_searchform.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_searchform.o: /usr/openwin/include/X11/Xutil.h
vc_searchform.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_searchform.o: /usr/openwin/include/X11/Core.h
vc_searchform.o: /usr/openwin/include/X11/Composite.h
vc_searchform.o: /usr/openwin/include/X11/Constraint.h
vc_searchform.o: /usr/openwin/include/X11/Object.h
vc_searchform.o: /usr/openwin/include/X11/RectObj.h
vc_searchform.o: /usr/openwin/include/X11/Shell.h
vc_searchform.o: /usr/openwin/include/X11/Xatom.h
vc_searchform.o: /usr/dt/include/Xm/XmStrDefs.h
vc_searchform.o: /usr/openwin/include/X11/StringDefs.h
vc_searchform.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_searchform.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_searchform.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_searchform.o: /usr/dt/include/Xm/Label.h /usr/dt/include/Xm/PushB.h
vc_searchform.o: /usr/dt/include/Xm/RowColumn.h /usr/dt/include/Xm/TextF.h
vc_searchform.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/Text.h
vc_searchform.o: /usr/dt/include/Xm/ScrolledW.h /usr/dt/include/Xm/Form.h
vc_searchform.o: /usr/dt/include/Xm/BulletinB.h cb_searchform.h
vc_searchform.o: vc_searchform.h
vc_sortform.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_sortform.o: /usr/dt/include/Xm/Xm.h /usr/openwin/include/X11/Intrinsic.h
vc_sortform.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_sortform.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_sortform.o: /usr/openwin/include/X11/X.h
vc_sortform.o: /usr/openwin/include/X11/Xfuncproto.h
vc_sortform.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_sortform.o: /usr/openwin/include/X11/Xutil.h
vc_sortform.o: /usr/openwin/include/X11/Xresource.h /usr/include/string.h
vc_sortform.o: /usr/openwin/include/X11/Core.h
vc_sortform.o: /usr/openwin/include/X11/Composite.h
vc_sortform.o: /usr/openwin/include/X11/Constraint.h
vc_sortform.o: /usr/openwin/include/X11/Object.h
vc_sortform.o: /usr/openwin/include/X11/RectObj.h
vc_sortform.o: /usr/openwin/include/X11/Shell.h
vc_sortform.o: /usr/openwin/include/X11/Xatom.h
vc_sortform.o: /usr/dt/include/Xm/XmStrDefs.h
vc_sortform.o: /usr/openwin/include/X11/StringDefs.h
vc_sortform.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
vc_sortform.o: /usr/dt/include/Xm/MwmUtil.h /usr/openwin/include/X11/Xmd.h
vc_sortform.o: /usr/dt/include/Xm/MenuShell.h UxXt.h /usr/include/stdlib.h
vc_sortform.o: /usr/dt/include/Xm/ToggleB.h /usr/dt/include/Xm/RowColumn.h
vc_sortform.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/TextF.h
vc_sortform.o: /usr/dt/include/Xm/Label.h /usr/dt/include/Xm/Text.h
vc_sortform.o: /usr/dt/include/Xm/List.h /usr/dt/include/Xm/ScrolledW.h
vc_sortform.o: /usr/dt/include/Xm/Form.h /usr/dt/include/Xm/BulletinB.h
vc_sortform.o: cb_sortform.h vc_sortform.h
vc_apswoscompare.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
vc_apswoscompare.o: /usr/dt/include/Xm/Xm.h
vc_apswoscompare.o: /usr/openwin/include/X11/Intrinsic.h
vc_apswoscompare.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
vc_apswoscompare.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
vc_apswoscompare.o: /usr/openwin/include/X11/X.h
vc_apswoscompare.o: /usr/openwin/include/X11/Xfuncproto.h
vc_apswoscompare.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
vc_apswoscompare.o: /usr/openwin/include/X11/Xutil.h
vc_apswoscompare.o: /usr/openwin/include/X11/Xresource.h
vc_apswoscompare.o: /usr/include/string.h /usr/openwin/include/X11/Core.h
vc_apswoscompare.o: /usr/openwin/include/X11/Composite.h
vc_apswoscompare.o: /usr/openwin/include/X11/Constraint.h
vc_apswoscompare.o: /usr/openwin/include/X11/Object.h
vc_apswoscompare.o: /usr/openwin/include/X11/RectObj.h
vc_apswoscompare.o: /usr/openwin/include/X11/Shell.h
vc_apswoscompare.o: /usr/openwin/include/X11/Xatom.h
vc_apswoscompare.o: /usr/dt/include/Xm/XmStrDefs.h
vc_apswoscompare.o: /usr/openwin/include/X11/StringDefs.h
vc_apswoscompare.o: /usr/dt/include/Xm/VirtKeys.h
vc_apswoscompare.o: /usr/dt/include/Xm/VendorS.h /usr/dt/include/Xm/MwmUtil.h
vc_apswoscompare.o: /usr/openwin/include/X11/Xmd.h
vc_apswoscompare.o: /usr/dt/include/Xm/MenuShell.h UxXt.h
vc_apswoscompare.o: /usr/include/stdlib.h /usr/dt/include/Xm/Separator.h
vc_apswoscompare.o: /usr/dt/include/Xm/Text.h /usr/dt/include/Xm/ScrolledW.h
vc_apswoscompare.o: /usr/dt/include/Xm/PushB.h /usr/dt/include/Xm/TextF.h
vc_apswoscompare.o: /usr/dt/include/Xm/Label.h /usr/dt/include/Xm/Form.h
vc_apswoscompare.o: /usr/dt/include/Xm/BulletinB.h cb_apswoscompare.h
vc_apswoscompare.o: /home/aps/r2.1.1/include/local/apsfiledef.h gui_utils.h
vc_apswoscompare.o: vc_apswoscompare.h
prcapsfile_main_apsfiledef.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
prcapsfile_main_apsfiledef.o: /usr/include/string.h /usr/dt/include/Xm/Xm.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Intrinsic.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
prcapsfile_main_apsfiledef.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/X.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Xfuncproto.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Xutil.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Xresource.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Core.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Composite.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Constraint.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Object.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/RectObj.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Shell.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
prcapsfile_main_apsfiledef.o: /usr/openwin/include/X11/StringDefs.h
prcapsfile_main_apsfiledef.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
prcapsfile_main_apsfiledef.o: aps_exe_names.h /home/aps/r2.1.1/include/local/apspath.h
prcapsfile_main_apsfiledef.o: /home/aps/r2.1.1/include/local/mu_utilities.h
prcapsfile_main_apsfiledef.o: /home/aps/r2.1.1/include/global/db_sybint.h
prcapsfile_main_apsfiledef.o: /home/sybase/syb11/include/sybfront.h
prcapsfile_main_apsfiledef.o: /home/sybase/syb11/include/cstypes.h
prcapsfile_main_apsfiledef.o: /home/sybase/syb11/include/csconfig.h
prcapsfile_main_apsfiledef.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
prcapsfile_main_apsfiledef.o: /home/sybase/syb11/include/sybdb.h
prcapsfile_main_apsfiledef.o: /home/sybase/syb11/include/syblogin.h
prcapsfile_main_apsfiledef.o: /home/sybase/syb11/include/syberror.h
prcapsfile_main_apsfiledef.o: /home/aps/r2.1.1/include/global/dapps_list.h
prcapsfile_main_apsfiledef.o: /home/aps/r2.1.1/include/global/llist_err.h
prcapsfile_main_apsfiledef.o: /home/aps/r2.1.1/include/global/llist.h
prcapsfile_main_apsfiledef.o: /home/aps/r2.1.1/include/global/ord_llist.h
prcapsfile_main_apsfiledef.o: /home/aps/r2.1.1/include/local/apsfiledef.h
