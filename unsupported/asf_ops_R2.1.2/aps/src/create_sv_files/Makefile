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
#	"@(#) /home/aps/r2.1.2/src/create_sv_files/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/create_sv_files
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################
IALL =      \
			$(IIMS) \
			$(IAPS) \
			$(IPPS) \
			$(ISYBASE) \
			$(OSIFLAGS) \
			$(IODL) \
			$(IFAIF)

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################
LALL =      \
			$(LASF) \
			$(LAPS) \
			$(LSYBASE) \
			$(LODL) \
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
	$(STATIC) \
	$(IMS_LIB) \
	$(FAIF_UTIL_LIB) \
	$(COMMON_ODL_LIB)

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      -D$(MACH)
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

EXE          =  aps_crt_sv_File

LIB          =  

SRC          =  sv_main.c sv_query.c sv_create_odl.c

HEADER_FILES =  

F77_SRC      =  

F77_INC      =  

OBJS         = $(SRC:%.c=%.o) $(F77_SRC:%.for=%.o)

TARGETS = $(EXE) $(LIB)
 

###############################################################################
#
# Make the Application
#    Add all targets to be made to the next line
#    If more exes or libs are needed make a copy of the set of 
#    needed commands
#
###############################################################################

all appl:  $(TARGETS) 

$(EXE)_purify: $(OBJS)
	@echo
	@echo "Linking    $@
	@purify $(CC) -o $@ $(OBJS) $(LALL) $(RALL) $(LIBS)
	@echo "Done"
 
$(EXE): $(OBJS)
	@echo
	@echo "Linking    $@
	@$(CC) -o $@ $(OBJS) $(LALL) $(RALL) $(LIBS)
	@echo "Done"
 
$(EXE)_purecov : $(OBJS)
	@echo
	@echo "Linking    $@
	@purecov $(CC) -o $@ $(OBJS) $(LALL) $(RALL) $(LIBS)
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
#  Note there is a brute force rule for apsfiledef.c because because it
#  needs to be retrieved from the GUI directory... This is where the
#  latest and greatest copy should be kept... It is retrieved from sccs
#  There are conditional compiles in here so the file is used by the 
#  create_aps_file application and the gui
#
###############################################################################

$(SRC) $(HEADER_FILES) $(F77_SRC) $(F77_INC) :
	@echo
	@echo "Retrieving $@ as of $(FILE_VERSION) "
	$(CM) $(CM_GET) $(FILE_VERSION) $(SRC_DIR)/$@

$(SRC_DIR)/../gui/apsfiledef.c :
	@echo "Invoking FORCE rules..."
	rm -f apsfiledef.c apsfiledef.o
	@echo "Retrieving apsfiledef.c as of $(FILE_VERSION) "
	$(CM) $(CM_GET) $(FILE_VERSION) -G$@ $@
		

 
###############################################################################
#
# C Compilation.
#
###############################################################################

apsfiledef.o : $(SRC_DIR)/../gui/apsfiledef.c
	@echo
	@echo "Compiling $<"
	@$(CC) $(CFLAGS) $(IALL) -c $< 

%.o:%.c
	@echo
	@echo "Compiling $<"
	@$(CC) $(CFLAGS) $(IALL) -c $< 


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
	rm -f $(OBJS) $(TARGETS)
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
#  Add post make definitions
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
		echo rm -f Makefile.bak ; \
		rm -f Makefile.bak ; \
	else \
		echo "*** Unwriteable Makefile: Need to check it out? ***" ; \
	fi

# DO NOT DELETE THIS LINE -- make depend depends on it.

sv_main.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
sv_main.o: /home/aps/r2.1.1/include/local/mu_utilities.h
sv_main.o: /home/aps/r2.1.1/include/global/db_sybint.h
sv_main.o: /home/sybase/sol/include/sybfront.h
sv_main.o: /home/sybase/sol/include/cstypes.h
sv_main.o: /home/sybase/sol/include/csconfig.h
sv_main.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
sv_main.o: /home/sybase/sol/include/sybdb.h
sv_main.o: /home/sybase/sol/include/syblogin.h
sv_main.o: /home/sybase/sol/include/syberror.h
sv_main.o: /home/aps/r2.1.1/include/global/dapps_list.h
sv_main.o: /home/aps/r2.1.1/include/global/llist_err.h
sv_main.o: /home/aps/r2.1.1/include/global/llist.h
sv_main.o: /home/aps/r2.1.1/include/global/ord_llist.h create_sv_file.h
sv_main.o: /usr/include/stdlib.h /usr/include/errno.h
sv_main.o: /usr/include/sys/errno.h /usr/include/ctype.h
sv_main.o: /usr/include/string.h /usr/include/sys/utsname.h
sv_main.o: /usr/include/syslog.h /usr/include/sys/syslog.h
sv_main.o: /usr/include/unistd.h /usr/include/sys/types.h
sv_main.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
sv_main.o: /usr/include/sys/unistd.h /ASF/PRECM/include/imsdads/ims_dbms.h
sv_main.o: /home/sybase/sol/include/ctpublic.h
sv_main.o: /home/sybase/sol/include/cspublic.h
sv_main.o: /home/sybase/sol/include/sqlca.h
sv_main.o: /ASF/PRECM/include/imsdads/ims_query.h
sv_main.o: /ASF/PRECM/include/imsdads/ims_const.h
sv_main.o: /ASF/PRECM/include/imsdads/ims_msg.h
sv_main.o: /ASF/PRECM/include/imsdads/ims_qi.h
sv_main.o: /ASF/PRECM/include/imsdads/ims_getInput.h
sv_main.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
sv_main.o: /ASF/PRECM/include/imsdads/ims_util.h
sv_main.o: /home/aps/r2.1.1/include/local/aps_defs.h
sv_main.o: /ASF/PRECM/include/sys/odldef.h /ASF/PRECM/include/sys/odlinter.h
sv_main.o: /home/aps/r2.1.1/include/global/GENconversions.h
sv_main.o: /home/aps/r2.1.1/include/global/fa_defs.h
sv_main.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
sv_main.o: /ASF/PRECM/include/faif/faifdefs.h
sv_main.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
sv_main.o: /home/aps/r2.1.1/include/local/apspath.h
sv_main.o: /home/aps/r2.1.1/include/local/apsfiledef.h
sv_main.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
sv_query.o: create_sv_file.h /usr/include/stdio.h
sv_query.o: /usr/include/sys/feature_tests.h /usr/include/stdlib.h
sv_query.o: /usr/include/errno.h /usr/include/sys/errno.h
sv_query.o: /usr/include/ctype.h /usr/include/string.h
sv_query.o: /usr/include/sys/utsname.h /usr/include/syslog.h
sv_query.o: /usr/include/sys/syslog.h /usr/include/unistd.h
sv_query.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
sv_query.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
sv_query.o: /ASF/PRECM/include/imsdads/ims_dbms.h
sv_query.o: /home/sybase/sol/include/sybfront.h
sv_query.o: /home/sybase/sol/include/cstypes.h
sv_query.o: /home/sybase/sol/include/csconfig.h
sv_query.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
sv_query.o: /home/sybase/sol/include/ctpublic.h
sv_query.o: /home/sybase/sol/include/cspublic.h
sv_query.o: /home/sybase/sol/include/sqlca.h /home/sybase/sol/include/sybdb.h
sv_query.o: /home/sybase/sol/include/syblogin.h
sv_query.o: /ASF/PRECM/include/imsdads/ims_query.h
sv_query.o: /ASF/PRECM/include/imsdads/ims_const.h
sv_query.o: /ASF/PRECM/include/imsdads/ims_msg.h
sv_query.o: /ASF/PRECM/include/imsdads/ims_qi.h
sv_query.o: /ASF/PRECM/include/imsdads/ims_getInput.h
sv_query.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
sv_query.o: /ASF/PRECM/include/imsdads/ims_util.h
sv_query.o: /home/aps/r2.1.1/include/local/aps_defs.h
sv_query.o: /ASF/PRECM/include/sys/odldef.h /ASF/PRECM/include/sys/odlinter.h
sv_query.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
sv_create_odl.o: create_sv_file.h /usr/include/stdio.h
sv_create_odl.o: /usr/include/sys/feature_tests.h /usr/include/stdlib.h
sv_create_odl.o: /usr/include/errno.h /usr/include/sys/errno.h
sv_create_odl.o: /usr/include/ctype.h /usr/include/string.h
sv_create_odl.o: /usr/include/sys/utsname.h /usr/include/syslog.h
sv_create_odl.o: /usr/include/sys/syslog.h /usr/include/unistd.h
sv_create_odl.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
sv_create_odl.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
sv_create_odl.o: /ASF/PRECM/include/imsdads/ims_dbms.h
sv_create_odl.o: /home/sybase/sol/include/sybfront.h
sv_create_odl.o: /home/sybase/sol/include/cstypes.h
sv_create_odl.o: /home/sybase/sol/include/csconfig.h
sv_create_odl.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
sv_create_odl.o: /home/sybase/sol/include/ctpublic.h
sv_create_odl.o: /home/sybase/sol/include/cspublic.h
sv_create_odl.o: /home/sybase/sol/include/sqlca.h
sv_create_odl.o: /home/sybase/sol/include/sybdb.h
sv_create_odl.o: /home/sybase/sol/include/syblogin.h
sv_create_odl.o: /ASF/PRECM/include/imsdads/ims_query.h
sv_create_odl.o: /ASF/PRECM/include/imsdads/ims_const.h
sv_create_odl.o: /ASF/PRECM/include/imsdads/ims_msg.h
sv_create_odl.o: /ASF/PRECM/include/imsdads/ims_qi.h
sv_create_odl.o: /ASF/PRECM/include/imsdads/ims_getInput.h
sv_create_odl.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
sv_create_odl.o: /ASF/PRECM/include/imsdads/ims_util.h
sv_create_odl.o: /home/aps/r2.1.1/include/local/aps_defs.h
sv_create_odl.o: /ASF/PRECM/include/sys/odldef.h
sv_create_odl.o: /ASF/PRECM/include/sys/odlinter.h
