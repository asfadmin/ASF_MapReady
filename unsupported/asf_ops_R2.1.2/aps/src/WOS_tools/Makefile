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
#	"@(#) /home/aps/r2.1.2/src/WOS_tools/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/WOS_tools
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL = \
        $(IFAIF) \
        $(IODL) \
        $(IAPS) \
        $(ISYBASE) \
        $(IPPS) \
        $(OSIFLAGS) 

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################

LALL =      $(LASF) \
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
    $(COMMON_ODL_LIB) \
    $(FAIF_UTIL_LIB)

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
#       F77_SRC  is a list (possibly empty) of the Fortran 77 source
#                code files
#
#
###############################################################################

EXE          =  aps_WOS_compare

LIB          =  

SRC          =  WOS_compare.c

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

all appl:   $(TARGETS)

$(EXE): $(OBJS)
	@echo
	@echo "Linking    $(EXE)"
	@$(CC) -o $(EXE) $(OBJS) $(LALL) $(RALL) $(LIBS)
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
	@echo $(CM) $(CM_GET) $(FILE_VERSION) $(SRC_DIR)/$@
	@$(CM) $(CM_GET) $(FILE_VERSION) $(SRC_DIR)/$@

 
###############################################################################
#
# C Compilation.
#
###############################################################################

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
	@$(F77_LOG) ; $(F77) $<


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
#	Add Post Make definitions
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

WOS_compare.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
WOS_compare.o: /usr/include/stdio.h /usr/include/string.h
WOS_compare.o: /usr/include/unistd.h /usr/include/sys/types.h
WOS_compare.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
WOS_compare.o: /usr/include/sys/unistd.h
WOS_compare.o: /home/aps/r2.1.1/include/global/db_sybint.h
WOS_compare.o: /home/sybase/sol/include/sybfront.h
WOS_compare.o: /home/sybase/sol/include/cstypes.h
WOS_compare.o: /home/sybase/sol/include/csconfig.h
WOS_compare.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
WOS_compare.o: /home/sybase/sol/include/sybdb.h
WOS_compare.o: /home/sybase/sol/include/syblogin.h
WOS_compare.o: /home/sybase/sol/include/syberror.h
WOS_compare.o: /home/aps/r2.1.1/include/global/dapps_list.h
WOS_compare.o: /home/aps/r2.1.1/include/global/llist_err.h
WOS_compare.o: /home/aps/r2.1.1/include/global/llist.h
WOS_compare.o: /home/aps/r2.1.1/include/global/ord_llist.h
WOS_compare.o: /home/aps/r2.1.1/include/local/aps_db_table.h
WOS_compare.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
WOS_compare.o: /home/aps/r2.1.1/include/global/dapps_defs.h
WOS_compare.o: /home/aps/r2.1.1/include/local/apsfiledef.h
WOS_compare.o: /home/aps/r2.1.1/include/local/db_dtk.h
WOS_compare.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
WOS_compare.o: /usr/include/syslog.h /usr/include/sys/syslog.h
WOS_compare.o: /home/aps/r2.1.1/include/local/aps_defs.h
WOS_compare.o: /home/aps/r2.1.1/include/local/apspath.h
WOS_compare.o: /ASF/PRECM/include/sys/odldef.h
WOS_compare.o: /home/aps/r2.1.1/include/local/ODLconversions.h
WOS_compare.o: /usr/include/stddef.h
WOS_compare.o: /home/aps/r2.1.1/include/local/ODL_defs.h
WOS_compare.o: /home/aps/r2.1.1/include/global/fa_defs.h
WOS_compare.o: /home/aps/r2.1.1/include/local/phase_utilities.h
WOS_compare.o: /home/aps/r2.1.1/include/local/db_phase.h
WOS_compare.o: /home/aps/r2.1.1/include/global/GENconversions.h
WOS_compare.o: /home/aps/r2.1.1/include/local/file_utilities.h
WOS_compare.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
WOS_compare.o: /ASF/PRECM/include/faif/faifdefs.h
WOS_compare.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
WOS_compare.o: /ASF/PRECM/include/sys/odlinter.h
WOS_compare.o: /home/aps/r2.1.1/include/local/mu_utilities.h
WOS_compare.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
WOS_compare.o: /home/aps/r2.1.1/include/local/timeconv.h
