###############################################################################
#
# File:         Makefile
#
# Function:     Makefile for src/lib_interface
#
# Author:       Lawrence Stevens from Ron Green (Inspired by S. Hardman)
#
# Date:         Fri Jan 12 10:44:44 PST 1996 
#
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/lib_interface
 
###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL = \
        $(IPPS) \
        $(IFAIF) \
        $(IODL) \
        $(IAPS) \
        $(ISYBASE) \
        $(OSIFLAGS) 

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################

LALL = 

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

LIBS = 

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

EXE          =  

LIB   		 = 

GLOBAL_LIB   =  libAPSinterface.a

SRC          =  

SRC          =  CSAconversions.c     \
				DARconversions.c     \
				ESAconversions.c     \
				ESAc_yymmdd_pad_hh_mm_ss2asftime.c \
				GENconversions.c     \
				gen_field_is_numeric.c \
				gen_string2str.c \
				IMSconversions.c     \
				NASDAconversions.c   \
				ODLingestions.c		\
				ODLconversions.c    \
				ODLequivalences.c

HEADER_FILES =  

F77_SRC      =  

F77_INC      =  

OBJS         = $(SRC:%.c=%.o) $(F77_SRC:%.for=%.o)

TARGETS = $(EXE) $(LIB) $(GLOBAL_LIB)
 

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

$(GLOBAL_LIB): $(OBJS)
	@echo
	@echo "Creating/Modifying Library $(GLOBAL_LIB)"
	@$(ARCHIVE) rucv $(GLOBAL_LIB) $?
	@if [ $(OS) = sunos ]; then ranlib $(GLOBAL_LIB); fi
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
# List definitions, install target:
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

CSAconversions.o: /usr/include/string.h /usr/include/sys/feature_tests.h
CSAconversions.o: /home/aps/r2.1.1/include/local/timeconv.h
CSAconversions.o: /home/aps/r2.1.1/include/local/CSAconversions.h
CSAconversions.o: /home/aps/r2.1.1/include/global/fa_defs.h
CSAconversions.o: /home/aps/r2.1.1/include/global/dapps_list.h
CSAconversions.o: /home/aps/r2.1.1/include/global/llist_err.h
CSAconversions.o: /home/aps/r2.1.1/include/global/llist.h
CSAconversions.o: /home/aps/r2.1.1/include/global/ord_llist.h
CSAconversions.o: /home/aps/r2.1.1/include/global/GENconversions.h
CSAconversions.o: /usr/include/stdio.h
CSAconversions.o: /home/aps/r2.1.1/include/local/db_dtk.h
CSAconversions.o: /home/aps/r2.1.1/include/global/dapps_defs.h
CSAconversions.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
CSAconversions.o: /home/aps/r2.1.1/include/global/db_sybint.h
CSAconversions.o: /home/sybase/sol/include/sybfront.h
CSAconversions.o: /home/sybase/sol/include/cstypes.h
CSAconversions.o: /home/sybase/sol/include/csconfig.h
CSAconversions.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
CSAconversions.o: /home/sybase/sol/include/sybdb.h
CSAconversions.o: /home/sybase/sol/include/syblogin.h
CSAconversions.o: /home/sybase/sol/include/syberror.h
CSAconversions.o: /home/aps/r2.1.1/include/local/mu_utilities.h
DARconversions.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
DARconversions.o: /usr/include/string.h
DARconversions.o: /home/aps/r2.1.1/include/local/DARconversions.h
DARconversions.o: /home/aps/r2.1.1/include/global/fa_defs.h
DARconversions.o: /home/aps/r2.1.1/include/global/dapps_list.h
DARconversions.o: /home/aps/r2.1.1/include/global/llist_err.h
DARconversions.o: /home/aps/r2.1.1/include/global/llist.h
DARconversions.o: /home/aps/r2.1.1/include/global/ord_llist.h
ESAconversions.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
ESAconversions.o: /usr/include/libgen.h /usr/include/sys/types.h
ESAconversions.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
ESAconversions.o: /usr/include/stdio.h /usr/include/time.h
ESAconversions.o: /usr/include/string.h
ESAconversions.o: /home/aps/r2.1.1/include/local/timeconv.h
ESAconversions.o: /home/aps/r2.1.1/include/global/GENconversions.h
ESAconversions.o: /home/aps/r2.1.1/include/global/fa_defs.h
ESAconversions.o: /home/aps/r2.1.1/include/global/dapps_list.h
ESAconversions.o: /home/aps/r2.1.1/include/global/llist_err.h
ESAconversions.o: /home/aps/r2.1.1/include/global/llist.h
ESAconversions.o: /home/aps/r2.1.1/include/global/ord_llist.h
ESAconversions.o: /home/aps/r2.1.1/include/global/ESAconversions.h
ESAconversions.o: /home/aps/r2.1.1/include/global/dapps_defs.h
ESAconversions.o: /home/aps/r2.1.1/include/local/db_dtk.h
ESAconversions.o: /home/aps/r2.1.1/include/local/mu_utilities.h
ESAconversions.o: /home/aps/r2.1.1/include/global/db_sybint.h
ESAconversions.o: /home/sybase/sol/include/sybfront.h
ESAconversions.o: /home/sybase/sol/include/cstypes.h
ESAconversions.o: /home/sybase/sol/include/csconfig.h
ESAconversions.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
ESAconversions.o: /home/sybase/sol/include/sybdb.h
ESAconversions.o: /home/sybase/sol/include/syblogin.h
ESAconversions.o: /home/sybase/sol/include/syberror.h
ESAconversions.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
ESAc_yymmdd_pad_hh_mm_ss2asftime.o: /usr/include/string.h
ESAc_yymmdd_pad_hh_mm_ss2asftime.o: /usr/include/sys/feature_tests.h
ESAc_yymmdd_pad_hh_mm_ss2asftime.o: /home/aps/r2.1.1/include/local/timeconv.h
GENconversions.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
GENconversions.o: /usr/include/ctype.h /usr/include/stdlib.h
GENconversions.o: /usr/include/string.h /usr/include/math.h
GENconversions.o: /home/aps/r2.1.1/include/local/mu_utilities.h
GENconversions.o: /home/aps/r2.1.1/include/global/db_sybint.h
GENconversions.o: /home/sybase/sol/include/sybfront.h
GENconversions.o: /home/sybase/sol/include/cstypes.h
GENconversions.o: /home/sybase/sol/include/csconfig.h
GENconversions.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
GENconversions.o: /home/sybase/sol/include/sybdb.h
GENconversions.o: /home/sybase/sol/include/syblogin.h
GENconversions.o: /home/sybase/sol/include/syberror.h
GENconversions.o: /home/aps/r2.1.1/include/global/dapps_list.h
GENconversions.o: /home/aps/r2.1.1/include/global/llist_err.h
GENconversions.o: /home/aps/r2.1.1/include/global/llist.h
GENconversions.o: /home/aps/r2.1.1/include/global/ord_llist.h
GENconversions.o: /home/aps/r2.1.1/include/global/GENconversions.h
GENconversions.o: /home/aps/r2.1.1/include/global/fa_defs.h
GENconversions.o: /home/aps/r2.1.1/include/global/dapps_defs.h
GENconversions.o: /home/aps/r2.1.1/include/local/timeconv.h
GENconversions.o: /home/aps/r2.1.1/include/local/aps_db_table.h
GENconversions.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
GENconversions.o: /home/aps/r2.1.1/include/local/db_dtk.h
GENconversions.o: /home/aps/r2.1.1/include/local/db_antenna.h
GENconversions.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
gen_field_is_numeric.o: /usr/include/string.h
gen_field_is_numeric.o: /usr/include/sys/feature_tests.h /usr/include/ctype.h
gen_field_is_numeric.o: /home/aps/r2.1.1/include/global/dapps_defs.h
gen_string2str.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
gen_string2str.o: /usr/include/string.h
gen_string2str.o: /home/aps/r2.1.1/include/global/dapps_defs.h
IMSconversions.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
IMSconversions.o: /usr/include/string.h
IMSconversions.o: /home/aps/r2.1.1/include/local/DARconversions.h
IMSconversions.o: /home/aps/r2.1.1/include/global/fa_defs.h
IMSconversions.o: /home/aps/r2.1.1/include/global/dapps_list.h
IMSconversions.o: /home/aps/r2.1.1/include/global/llist_err.h
IMSconversions.o: /home/aps/r2.1.1/include/global/llist.h
IMSconversions.o: /home/aps/r2.1.1/include/global/ord_llist.h
NASDAconversions.o: /usr/include/ctype.h /usr/include/sys/feature_tests.h
NASDAconversions.o: /usr/include/stdio.h /usr/include/stdlib.h
NASDAconversions.o: /usr/include/stddef.h /usr/include/string.h
NASDAconversions.o: /home/aps/r2.1.1/include/local/timeconv.h
NASDAconversions.o: /home/aps/r2.1.1/include/global/GENconversions.h
NASDAconversions.o: /home/aps/r2.1.1/include/global/fa_defs.h
NASDAconversions.o: /home/aps/r2.1.1/include/global/dapps_list.h
NASDAconversions.o: /home/aps/r2.1.1/include/global/llist_err.h
NASDAconversions.o: /home/aps/r2.1.1/include/global/llist.h
NASDAconversions.o: /home/aps/r2.1.1/include/global/ord_llist.h
NASDAconversions.o: /home/aps/r2.1.1/include/local/NASDAconversions.h
NASDAconversions.o: /home/aps/r2.1.1/include/local/phase_utilities.h
NASDAconversions.o: /home/aps/r2.1.1/include/global/db_sybint.h
NASDAconversions.o: /home/sybase/sol/include/sybfront.h
NASDAconversions.o: /home/sybase/sol/include/cstypes.h
NASDAconversions.o: /home/sybase/sol/include/csconfig.h
NASDAconversions.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
NASDAconversions.o: /home/sybase/sol/include/sybdb.h
NASDAconversions.o: /home/sybase/sol/include/syblogin.h
NASDAconversions.o: /home/sybase/sol/include/syberror.h
NASDAconversions.o: /home/aps/r2.1.1/include/local/db_phase.h
NASDAconversions.o: /home/aps/r2.1.1/include/global/dapps_defs.h
NASDAconversions.o: /home/aps/r2.1.1/include/local/db_dtk.h
NASDAconversions.o: /home/aps/r2.1.1/include/local/file_utilities.h
NASDAconversions.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
NASDAconversions.o: /ASF/PRECM/include/faif/faifdefs.h /usr/include/syslog.h
NASDAconversions.o: /usr/include/sys/syslog.h
NASDAconversions.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
NASDAconversions.o: /ASF/PRECM/include/sys/odldef.h
NASDAconversions.o: /ASF/PRECM/include/sys/odlinter.h
NASDAconversions.o: /home/aps/r2.1.1/include/local/mu_utilities.h
NASDAconversions.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
NASDAconversions.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
ODLingestions.o: /usr/include/string.h /usr/include/sys/feature_tests.h
ODLingestions.o: /usr/include/math.h /usr/include/stdio.h
ODLingestions.o: /usr/include/errno.h /usr/include/sys/errno.h
ODLingestions.o: /usr/include/stdlib.h /usr/include/sys/types.h
ODLingestions.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
ODLingestions.o: /usr/include/sys/uio.h /usr/include/sys/unistd.h
ODLingestions.o: /home/aps/r2.1.1/include/local/timeconv.h
ODLingestions.o: /home/aps/r2.1.1/include/local/mu_utilities.h
ODLingestions.o: /home/aps/r2.1.1/include/global/db_sybint.h
ODLingestions.o: /home/sybase/sol/include/sybfront.h
ODLingestions.o: /home/sybase/sol/include/cstypes.h
ODLingestions.o: /home/sybase/sol/include/csconfig.h
ODLingestions.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
ODLingestions.o: /home/sybase/sol/include/sybdb.h
ODLingestions.o: /home/sybase/sol/include/syblogin.h
ODLingestions.o: /home/sybase/sol/include/syberror.h
ODLingestions.o: /home/aps/r2.1.1/include/global/dapps_list.h
ODLingestions.o: /home/aps/r2.1.1/include/global/llist_err.h
ODLingestions.o: /home/aps/r2.1.1/include/global/llist.h
ODLingestions.o: /home/aps/r2.1.1/include/global/ord_llist.h
ODLingestions.o: /home/aps/r2.1.1/include/local/aps_db_table.h
ODLingestions.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
ODLingestions.o: /home/aps/r2.1.1/include/global/dapps_defs.h
ODLingestions.o: /home/aps/r2.1.1/include/local/apsfiledef.h
ODLingestions.o: /home/aps/r2.1.1/include/global/fa_defs.h
ODLingestions.o: /home/aps/r2.1.1/include/local/ODL_defs.h
ODLingestions.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
ODLingestions.o: /home/aps/r2.1.1/include/local/db_dtk.h
ODLingestions.o: /home/aps/r2.1.1/include/local/db_antenna.h
ODLingestions.o: /home/aps/r2.1.1/include/local/apspath.h
ODLingestions.o: /home/aps/r2.1.1/include/global/GENconversions.h
ODLingestions.o: /home/aps/r2.1.1/include/local/ODLconversions.h
ODLingestions.o: /usr/include/stddef.h /ASF/PRECM/include/sys/odldef.h
ODLingestions.o: /home/aps/r2.1.1/include/local/phase_utilities.h
ODLingestions.o: /home/aps/r2.1.1/include/local/db_phase.h
ODLconversions.o: /usr/include/string.h /usr/include/sys/feature_tests.h
ODLconversions.o: /usr/include/math.h /usr/include/stdio.h
ODLconversions.o: /usr/include/errno.h /usr/include/sys/errno.h
ODLconversions.o: /usr/include/stdlib.h /usr/include/sys/types.h
ODLconversions.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
ODLconversions.o: /usr/include/sys/uio.h /usr/include/sys/unistd.h
ODLconversions.o: /home/aps/r2.1.1/include/local/timeconv.h
ODLconversions.o: /home/aps/r2.1.1/include/local/mu_utilities.h
ODLconversions.o: /home/aps/r2.1.1/include/global/db_sybint.h
ODLconversions.o: /home/sybase/sol/include/sybfront.h
ODLconversions.o: /home/sybase/sol/include/cstypes.h
ODLconversions.o: /home/sybase/sol/include/csconfig.h
ODLconversions.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
ODLconversions.o: /home/sybase/sol/include/sybdb.h
ODLconversions.o: /home/sybase/sol/include/syblogin.h
ODLconversions.o: /home/sybase/sol/include/syberror.h
ODLconversions.o: /home/aps/r2.1.1/include/global/dapps_list.h
ODLconversions.o: /home/aps/r2.1.1/include/global/llist_err.h
ODLconversions.o: /home/aps/r2.1.1/include/global/llist.h
ODLconversions.o: /home/aps/r2.1.1/include/global/ord_llist.h
ODLconversions.o: /home/aps/r2.1.1/include/local/aps_db_table.h
ODLconversions.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
ODLconversions.o: /home/aps/r2.1.1/include/global/dapps_defs.h
ODLconversions.o: /home/aps/r2.1.1/include/local/apsfiledef.h
ODLconversions.o: /home/aps/r2.1.1/include/global/fa_defs.h
ODLconversions.o: /home/aps/r2.1.1/include/local/ODL_defs.h
ODLconversions.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
ODLconversions.o: /home/aps/r2.1.1/include/local/file_utilities.h
ODLconversions.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
ODLconversions.o: /ASF/PRECM/include/faif/faifdefs.h /usr/include/syslog.h
ODLconversions.o: /usr/include/sys/syslog.h
ODLconversions.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
ODLconversions.o: /ASF/PRECM/include/sys/odldef.h
ODLconversions.o: /ASF/PRECM/include/sys/odlinter.h
ODLconversions.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
ODLconversions.o: /home/aps/r2.1.1/include/local/db_dtk.h
ODLconversions.o: /home/aps/r2.1.1/include/local/db_antenna.h
ODLconversions.o: /home/aps/r2.1.1/include/local/db_maskinout.h
ODLconversions.o: /home/aps/r2.1.1/include/local/apspath.h
ODLconversions.o: /home/aps/r2.1.1/include/global/GENconversions.h
ODLconversions.o: /home/aps/r2.1.1/include/local/ODLconversions.h
ODLconversions.o: /usr/include/stddef.h
ODLconversions.o: /home/aps/r2.1.1/include/local/phase_utilities.h
ODLconversions.o: /home/aps/r2.1.1/include/local/db_phase.h
ODLequivalences.o: /usr/include/string.h /usr/include/sys/feature_tests.h
ODLequivalences.o: /usr/include/math.h /usr/include/stdio.h
ODLequivalences.o: /usr/include/errno.h /usr/include/sys/errno.h
ODLequivalences.o: /usr/include/stdlib.h /usr/include/sys/types.h
ODLequivalences.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
ODLequivalences.o: /usr/include/sys/uio.h /usr/include/sys/unistd.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/timeconv.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/mu_utilities.h
ODLequivalences.o: /home/aps/r2.1.1/include/global/db_sybint.h
ODLequivalences.o: /home/sybase/sol/include/sybfront.h
ODLequivalences.o: /home/sybase/sol/include/cstypes.h
ODLequivalences.o: /home/sybase/sol/include/csconfig.h
ODLequivalences.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
ODLequivalences.o: /home/sybase/sol/include/sybdb.h
ODLequivalences.o: /home/sybase/sol/include/syblogin.h
ODLequivalences.o: /home/sybase/sol/include/syberror.h
ODLequivalences.o: /home/aps/r2.1.1/include/global/dapps_list.h
ODLequivalences.o: /home/aps/r2.1.1/include/global/llist_err.h
ODLequivalences.o: /home/aps/r2.1.1/include/global/llist.h
ODLequivalences.o: /home/aps/r2.1.1/include/global/ord_llist.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/aps_db_table.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
ODLequivalences.o: /home/aps/r2.1.1/include/global/dapps_defs.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/apsfiledef.h
ODLequivalences.o: /home/aps/r2.1.1/include/global/fa_defs.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/ODL_defs.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/db_dtk.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/db_antenna.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/apspath.h
ODLequivalences.o: /home/aps/r2.1.1/include/global/GENconversions.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/ODLconversions.h
ODLequivalences.o: /usr/include/stddef.h /ASF/PRECM/include/sys/odldef.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/phase_utilities.h
ODLequivalences.o: /home/aps/r2.1.1/include/local/db_phase.h
