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
#	"@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/FA_dtkf_c
 

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
			$(OSLFLAGS) \
			$(LODL) \
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

EXE          = aps_crt_FA_File 

LIB          =  

SRC          =  \
                FA_dtkf_creator.c  boundingbox.c \
                latlon2grs.c  station_mask_flag.c \
                calc_grs_errors.c   create_grs_list.c \
                REQQ_append.c     \
                get_observe_times.c \
                list_handling.c \
                crt_grs_reqq.c  \
                load_dar.c

#
#  NOTE:  this is for .h files in the current directory:
#
HEADER_FILES =  crt_grs_reqq.h list_handling.h


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

FA_dtkf_creator.o: /usr/include/string.h /usr/include/sys/feature_tests.h
FA_dtkf_creator.o: /usr/include/stdio.h /usr/include/errno.h
FA_dtkf_creator.o: /usr/include/sys/errno.h /usr/include/stdlib.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/mu_utilities.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/global/db_sybint.h
FA_dtkf_creator.o: /home/sybase/sol/include/sybfront.h
FA_dtkf_creator.o: /home/sybase/sol/include/cstypes.h
FA_dtkf_creator.o: /home/sybase/sol/include/csconfig.h
FA_dtkf_creator.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
FA_dtkf_creator.o: /home/sybase/sol/include/sybdb.h
FA_dtkf_creator.o: /home/sybase/sol/include/syblogin.h
FA_dtkf_creator.o: /home/sybase/sol/include/syberror.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/global/dapps_list.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/global/llist_err.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/global/llist.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/global/ord_llist.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/aps_db_table.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/global/dapps_defs.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/apsfiledef.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/db_dtk.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/db_schstat.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/apspath.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/phase_utilities.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/db_phase.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/file_utilities.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/global/fa_defs.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
FA_dtkf_creator.o: /ASF/PRECM/include/faif/faifdefs.h /usr/include/syslog.h
FA_dtkf_creator.o: /usr/include/sys/syslog.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
FA_dtkf_creator.o: /ASF/PRECM/include/sys/odldef.h
FA_dtkf_creator.o: /ASF/PRECM/include/sys/odlinter.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/global/GENconversions.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/NASDAconversions.h
FA_dtkf_creator.o: /usr/include/stddef.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/timeconv.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
FA_dtkf_creator.o: /home/aps/r2.1.1/include/local/aps_defs.h crt_grs_reqq.h
FA_dtkf_creator.o: list_handling.h
boundingbox.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
boundingbox.o: /usr/include/stdio.h /usr/include/math.h crt_grs_reqq.h
boundingbox.o: list_handling.h
latlon2grs.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
latlon2grs.o: /usr/include/stdio.h /usr/include/math.h
station_mask_flag.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
station_mask_flag.o: /usr/include/stdio.h /usr/include/string.h
station_mask_flag.o: /home/aps/r2.1.1/include/global/db_sybint.h
station_mask_flag.o: /home/sybase/sol/include/sybfront.h
station_mask_flag.o: /home/sybase/sol/include/cstypes.h
station_mask_flag.o: /home/sybase/sol/include/csconfig.h
station_mask_flag.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
station_mask_flag.o: /home/sybase/sol/include/sybdb.h
station_mask_flag.o: /home/sybase/sol/include/syblogin.h
station_mask_flag.o: /home/sybase/sol/include/syberror.h
station_mask_flag.o: /home/aps/r2.1.1/include/global/dapps_list.h
station_mask_flag.o: /home/aps/r2.1.1/include/global/llist_err.h
station_mask_flag.o: /home/aps/r2.1.1/include/global/llist.h
station_mask_flag.o: /home/aps/r2.1.1/include/global/ord_llist.h
station_mask_flag.o: /home/aps/r2.1.1/include/local/aps_db_table.h
station_mask_flag.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
station_mask_flag.o: /home/aps/r2.1.1/include/local/db_station.h
station_mask_flag.o: crt_grs_reqq.h list_handling.h
create_grs_list.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
create_grs_list.o: /usr/include/stdio.h /usr/include/math.h list_handling.h
create_grs_list.o: crt_grs_reqq.h
REQQ_append.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
REQQ_append.o: /usr/include/string.h /usr/include/stdlib.h
REQQ_append.o: /usr/include/time.h
REQQ_append.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
REQQ_append.o: /usr/include/syslog.h /usr/include/sys/syslog.h
REQQ_append.o: list_handling.h crt_grs_reqq.h
get_observe_times.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
get_observe_times.o: /usr/include/stdlib.h /usr/include/string.h
get_observe_times.o: /home/aps/r2.1.1/include/local/timeconv.h
get_observe_times.o: /home/aps/r2.1.1/include/global/dapps_defs.h
list_handling.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
list_handling.o: /usr/include/stdio.h /usr/include/math.h list_handling.h
crt_grs_reqq.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
crt_grs_reqq.o: /usr/include/stdio.h /usr/include/string.h
crt_grs_reqq.o: /usr/include/math.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/global/db_sybint.h
crt_grs_reqq.o: /home/sybase/sol/include/sybfront.h
crt_grs_reqq.o: /home/sybase/sol/include/cstypes.h
crt_grs_reqq.o: /home/sybase/sol/include/csconfig.h
crt_grs_reqq.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
crt_grs_reqq.o: /home/sybase/sol/include/sybdb.h
crt_grs_reqq.o: /home/sybase/sol/include/syblogin.h
crt_grs_reqq.o: /home/sybase/sol/include/syberror.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/global/dapps_list.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/global/llist_err.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/global/llist.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/global/ord_llist.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
crt_grs_reqq.o: /usr/include/syslog.h /usr/include/sys/syslog.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/local/apspath.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/global/dapps_defs.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/local/timeconv.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/local/aps_db_table.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
crt_grs_reqq.o: /home/aps/r2.1.1/include/local/db_dar.h list_handling.h
crt_grs_reqq.o: crt_grs_reqq.h
load_dar.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
load_dar.o: /usr/include/stdlib.h /usr/include/string.h
load_dar.o: /home/aps/r2.1.1/include/global/db_sybint.h
load_dar.o: /home/sybase/sol/include/sybfront.h
load_dar.o: /home/sybase/sol/include/cstypes.h
load_dar.o: /home/sybase/sol/include/csconfig.h
load_dar.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
load_dar.o: /home/sybase/sol/include/sybdb.h
load_dar.o: /home/sybase/sol/include/syblogin.h
load_dar.o: /home/sybase/sol/include/syberror.h
load_dar.o: /home/aps/r2.1.1/include/global/dapps_list.h
load_dar.o: /home/aps/r2.1.1/include/global/llist_err.h
load_dar.o: /home/aps/r2.1.1/include/global/llist.h
load_dar.o: /home/aps/r2.1.1/include/global/ord_llist.h
load_dar.o: /home/aps/r2.1.1/include/local/timeconv.h
load_dar.o: /home/aps/r2.1.1/include/local/phase_utilities.h
load_dar.o: /home/aps/r2.1.1/include/local/db_phase.h
load_dar.o: /home/aps/r2.1.1/include/global/dapps_defs.h
load_dar.o: /home/aps/r2.1.1/include/local/aps_db_table.h
load_dar.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
load_dar.o: /home/aps/r2.1.1/include/local/db_dar.h crt_grs_reqq.h
load_dar.o: list_handling.h /home/aps/r2.1.1/include/local/aps_log_msg.h
load_dar.o: /usr/include/syslog.h /usr/include/sys/syslog.h
load_dar.o: /home/aps/r2.1.1/include/local/mu_utilities.h
