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
#	"@(#)Makefile	1.3 98/03/09 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/stats/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/stats
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL = \
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
    $(EVERY_LIB)     \
	$(STATIC) \
	$(IMS_LIB) \
    $(FAIF_UTIL_LIB)  \
    $(COMMON_ODL_LIB) \
	$(ENCRYPT_LIB)


###############################################################################
#
# Flags and Defines
#
###############################################################################
#
#  Test return codes from ims_archive():  -DRANDOM_CODES
#  Write log file to standard out      :  use -L stdout at execution
#  Diagnostic print                    :  -DPRINT_DIAG 
#  Print args, do not call IMS         :  use -N IMS at execution
#  Print files, do not create files    :  -DDO_NOT_CREATE_FILES
#  Call a dummy, not the real IMS.     :  -DDUMMY_IMS_ROUTINE
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

EXE          =  aps_stats

LIB          =  

SRC          =  main.c              \
                aps_Statistics.c     \
                IMS_interface.c       \
                print_ims_event.c      \
                stats_call_reported.c   \
                stats_compute_aos_los.c  \
                stats_dtk_is_sch.c        \
                stats_dtk_is_cancelled.c   \
                stats_dtk_is_reduced.c       \
                stats_error_messages.c       \
                stats_init_pmf_values.c      \
                stats_report_all_downlinks.c \
                stats_report_all_masks.c    \
                stats_report_all_aos_los.c  \
                stats_report_sar_pln.c

HEADER_FILES =  aps_Statistics.h

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
	@echo "Linking    $@
	@$(CC) -o $@ $(OBJS) $(LALL) $(RALL) $(LIBS)
	@echo "Done"
 
$(EXE)_purify: $(OBJS)
	@echo
	@echo "Linking    $@
	@purify $(CC) -o $@ $(OBJS) $(LALL) $(RALL) $(LIBS)
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

main.o: /usr/include/unistd.h /usr/include/sys/feature_tests.h
main.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
main.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
main.o: /usr/include/stdio.h /usr/include/stdlib.h /usr/include/string.h
main.o: /home/aps/r2.1.2/include/global/db_sybint.h
main.o: /home/sybase/sol/include/sybfront.h
main.o: /home/sybase/sol/include/cstypes.h
main.o: /home/sybase/sol/include/csconfig.h
main.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
main.o: /home/sybase/sol/include/sybdb.h /home/sybase/sol/include/syblogin.h
main.o: /home/sybase/sol/include/syberror.h
main.o: /home/aps/r2.1.2/include/global/dapps_list.h
main.o: /home/aps/r2.1.2/include/global/llist_err.h
main.o: /home/aps/r2.1.2/include/global/llist.h
main.o: /home/aps/r2.1.2/include/global/ord_llist.h
main.o: /home/aps/r2.1.2/include/local/aps_db_table.h
main.o: /home/aps/r2.1.2/include/local/aps_db_tablenums.h
main.o: /home/aps/r2.1.2/include/global/dapps_defs.h
main.o: /home/aps/r2.1.2/include/local/db_dtk.h
main.o: /home/aps/r2.1.2/include/local/aps_encrypt.h
main.o: /home/aps/r2.1.2/include/local/aps_defs.h
main.o: /home/aps/r2.1.2/include/local/apspath.h
main.o: /home/aps/r2.1.2/include/local/open_log_file.h
main.o: /home/aps/r2.1.2/include/local/aps_log_msg.h /usr/include/syslog.h
main.o: /usr/include/sys/syslog.h
main.o: /home/aps/r2.1.2/include/local/mu_utilities.h
main.o: /home/aps/r2.1.2/include/local/timeconv.h aps_Statistics.h
main.o: /ASF/PRECM/include/imsdads/ims_const.h
main.o: /ASF/PRECM/include/imsdads/ims_msg.h
main.o: /ASF/PRECM/include/imsdads/ims_archive.h
main.o: /home/aps/r2.1.2/include/local/dtkm_utilities.h
main.o: /home/aps/r2.1.2/include/local/db_stats_calls.h
aps_Statistics.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
aps_Statistics.o: /usr/include/string.h
aps_Statistics.o: /home/aps/r2.1.2/include/global/db_sybint.h
aps_Statistics.o: /home/sybase/sol/include/sybfront.h
aps_Statistics.o: /home/sybase/sol/include/cstypes.h
aps_Statistics.o: /home/sybase/sol/include/csconfig.h
aps_Statistics.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
aps_Statistics.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
aps_Statistics.o: /home/sybase/sol/include/syblogin.h
aps_Statistics.o: /home/sybase/sol/include/syberror.h
aps_Statistics.o: /home/aps/r2.1.2/include/global/dapps_list.h
aps_Statistics.o: /home/aps/r2.1.2/include/global/llist_err.h
aps_Statistics.o: /home/aps/r2.1.2/include/global/llist.h
aps_Statistics.o: /home/aps/r2.1.2/include/global/ord_llist.h
aps_Statistics.o: /home/aps/r2.1.2/include/local/aps_db_table.h
aps_Statistics.o: /home/aps/r2.1.2/include/local/aps_db_tablenums.h
aps_Statistics.o: /home/aps/r2.1.2/include/local/aps_defs.h
aps_Statistics.o: /home/aps/r2.1.2/include/local/db_dtk.h
aps_Statistics.o: /home/aps/r2.1.2/include/local/timeconv.h
aps_Statistics.o: /home/aps/r2.1.2/include/local/dtkm_utilities.h
aps_Statistics.o: /home/aps/r2.1.2/include/local/DARconversions.h
aps_Statistics.o: /home/aps/r2.1.2/include/global/fa_defs.h
aps_Statistics.o: /home/aps/r2.1.2/include/local/aps_log_msg.h
aps_Statistics.o: /usr/include/syslog.h /usr/include/sys/syslog.h
aps_Statistics.o: aps_Statistics.h /ASF/PRECM/include/imsdads/ims_const.h
aps_Statistics.o: /ASF/PRECM/include/imsdads/ims_msg.h
aps_Statistics.o: /ASF/PRECM/include/imsdads/ims_archive.h
aps_Statistics.o: /home/aps/r2.1.2/include/local/db_stats_calls.h
aps_Statistics.o: /home/aps/r2.1.2/include/global/dapps_defs.h
IMS_interface.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
IMS_interface.o: /home/aps/r2.1.2/include/local/aps_log_msg.h
IMS_interface.o: /usr/include/stdio.h /usr/include/syslog.h
IMS_interface.o: /usr/include/sys/syslog.h aps_Statistics.h
IMS_interface.o: /home/aps/r2.1.2/include/global/db_sybint.h
IMS_interface.o: /home/sybase/sol/include/sybfront.h
IMS_interface.o: /home/sybase/sol/include/cstypes.h
IMS_interface.o: /home/sybase/sol/include/csconfig.h
IMS_interface.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
IMS_interface.o: /home/sybase/sol/include/sybdb.h
IMS_interface.o: /home/sybase/sol/include/syblogin.h
IMS_interface.o: /home/sybase/sol/include/syberror.h
IMS_interface.o: /home/aps/r2.1.2/include/global/dapps_list.h
IMS_interface.o: /home/aps/r2.1.2/include/global/llist_err.h
IMS_interface.o: /home/aps/r2.1.2/include/global/llist.h
IMS_interface.o: /home/aps/r2.1.2/include/global/ord_llist.h
IMS_interface.o: /usr/include/string.h /ASF/PRECM/include/imsdads/ims_const.h
IMS_interface.o: /ASF/PRECM/include/imsdads/ims_msg.h
IMS_interface.o: /ASF/PRECM/include/imsdads/ims_archive.h
IMS_interface.o: /home/aps/r2.1.2/include/local/dtkm_utilities.h
IMS_interface.o: /home/aps/r2.1.2/include/local/db_stats_calls.h
IMS_interface.o: /home/aps/r2.1.2/include/local/db_dtk.h
IMS_interface.o: /home/aps/r2.1.2/include/local/timeconv.h
IMS_interface.o: /home/aps/r2.1.2/include/global/dapps_defs.h
IMS_interface.o: /home/aps/r2.1.2/include/local/aps_db_table.h
IMS_interface.o: /home/aps/r2.1.2/include/local/aps_db_tablenums.h
IMS_interface.o: /home/aps/r2.1.2/include/global/GENconversions.h
IMS_interface.o: /home/aps/r2.1.2/include/global/fa_defs.h
IMS_interface.o: /home/aps/r2.1.2/include/local/ODLconversions.h
IMS_interface.o: /usr/include/stddef.h
IMS_interface.o: /home/aps/r2.1.2/include/local/ODL_defs.h
IMS_interface.o: /ASF/PRECM/include/sys/odldef.h
IMS_interface.o: /home/aps/r2.1.2/include/local/phase_utilities.h
IMS_interface.o: /home/aps/r2.1.2/include/local/db_phase.h
print_ims_event.o: aps_Statistics.h
print_ims_event.o: /home/aps/r2.1.2/include/global/db_sybint.h
print_ims_event.o: /home/sybase/sol/include/sybfront.h
print_ims_event.o: /home/sybase/sol/include/cstypes.h
print_ims_event.o: /home/sybase/sol/include/csconfig.h
print_ims_event.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
print_ims_event.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
print_ims_event.o: /usr/include/sys/feature_tests.h
print_ims_event.o: /home/sybase/sol/include/syblogin.h
print_ims_event.o: /home/sybase/sol/include/syberror.h
print_ims_event.o: /home/aps/r2.1.2/include/global/dapps_list.h
print_ims_event.o: /home/aps/r2.1.2/include/global/llist_err.h
print_ims_event.o: /home/aps/r2.1.2/include/global/llist.h
print_ims_event.o: /home/aps/r2.1.2/include/global/ord_llist.h
print_ims_event.o: /usr/include/string.h
print_ims_event.o: /ASF/PRECM/include/imsdads/ims_const.h
print_ims_event.o: /ASF/PRECM/include/imsdads/ims_msg.h
print_ims_event.o: /ASF/PRECM/include/imsdads/ims_archive.h
print_ims_event.o: /home/aps/r2.1.2/include/local/dtkm_utilities.h
print_ims_event.o: /home/aps/r2.1.2/include/local/db_stats_calls.h
print_ims_event.o: /home/aps/r2.1.2/include/local/db_dtk.h
print_ims_event.o: /home/aps/r2.1.2/include/local/timeconv.h
print_ims_event.o: /home/aps/r2.1.2/include/global/dapps_defs.h
print_ims_event.o: /home/aps/r2.1.2/include/local/aps_db_table.h
print_ims_event.o: /home/aps/r2.1.2/include/local/aps_db_tablenums.h
stats_call_reported.o: aps_Statistics.h
stats_call_reported.o: /home/aps/r2.1.2/include/global/db_sybint.h
stats_call_reported.o: /home/sybase/sol/include/sybfront.h
stats_call_reported.o: /home/sybase/sol/include/cstypes.h
stats_call_reported.o: /home/sybase/sol/include/csconfig.h
stats_call_reported.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
stats_call_reported.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
stats_call_reported.o: /usr/include/sys/feature_tests.h
stats_call_reported.o: /home/sybase/sol/include/syblogin.h
stats_call_reported.o: /home/sybase/sol/include/syberror.h
stats_call_reported.o: /home/aps/r2.1.2/include/global/dapps_list.h
stats_call_reported.o: /home/aps/r2.1.2/include/global/llist_err.h
stats_call_reported.o: /home/aps/r2.1.2/include/global/llist.h
stats_call_reported.o: /home/aps/r2.1.2/include/global/ord_llist.h
stats_call_reported.o: /usr/include/string.h
stats_call_reported.o: /ASF/PRECM/include/imsdads/ims_const.h
stats_call_reported.o: /ASF/PRECM/include/imsdads/ims_msg.h
stats_call_reported.o: /ASF/PRECM/include/imsdads/ims_archive.h
stats_call_reported.o: /home/aps/r2.1.2/include/local/dtkm_utilities.h
stats_call_reported.o: /home/aps/r2.1.2/include/local/db_stats_calls.h
stats_call_reported.o: /home/aps/r2.1.2/include/local/db_dtk.h
stats_call_reported.o: /home/aps/r2.1.2/include/local/timeconv.h
stats_call_reported.o: /home/aps/r2.1.2/include/global/dapps_defs.h
stats_call_reported.o: /home/aps/r2.1.2/include/local/aps_db_table.h
stats_call_reported.o: /home/aps/r2.1.2/include/local/aps_db_tablenums.h
stats_compute_aos_los.o: /usr/include/string.h
stats_compute_aos_los.o: /usr/include/sys/feature_tests.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/local/db_antenna.h
stats_compute_aos_los.o: aps_Statistics.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/global/db_sybint.h
stats_compute_aos_los.o: /home/sybase/sol/include/sybfront.h
stats_compute_aos_los.o: /home/sybase/sol/include/cstypes.h
stats_compute_aos_los.o: /home/sybase/sol/include/csconfig.h
stats_compute_aos_los.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
stats_compute_aos_los.o: /home/sybase/sol/include/sybdb.h
stats_compute_aos_los.o: /usr/include/stdio.h
stats_compute_aos_los.o: /home/sybase/sol/include/syblogin.h
stats_compute_aos_los.o: /home/sybase/sol/include/syberror.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/global/dapps_list.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/global/llist_err.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/global/llist.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/global/ord_llist.h
stats_compute_aos_los.o: /ASF/PRECM/include/imsdads/ims_const.h
stats_compute_aos_los.o: /ASF/PRECM/include/imsdads/ims_msg.h
stats_compute_aos_los.o: /ASF/PRECM/include/imsdads/ims_archive.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/local/dtkm_utilities.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/local/db_stats_calls.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/local/db_dtk.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/local/timeconv.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/global/dapps_defs.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/local/aps_db_table.h
stats_compute_aos_los.o: /home/aps/r2.1.2/include/local/aps_db_tablenums.h
stats_dtk_is_sch.o: /usr/include/string.h /usr/include/sys/feature_tests.h
stats_dtk_is_sch.o: /home/aps/r2.1.2/include/global/db_sybint.h
stats_dtk_is_sch.o: /home/sybase/sol/include/sybfront.h
stats_dtk_is_sch.o: /home/sybase/sol/include/cstypes.h
stats_dtk_is_sch.o: /home/sybase/sol/include/csconfig.h
stats_dtk_is_sch.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
stats_dtk_is_sch.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
stats_dtk_is_sch.o: /home/sybase/sol/include/syblogin.h
stats_dtk_is_sch.o: /home/sybase/sol/include/syberror.h
stats_dtk_is_sch.o: /home/aps/r2.1.2/include/global/dapps_list.h
stats_dtk_is_sch.o: /home/aps/r2.1.2/include/global/llist_err.h
stats_dtk_is_sch.o: /home/aps/r2.1.2/include/global/llist.h
stats_dtk_is_sch.o: /home/aps/r2.1.2/include/global/ord_llist.h
stats_dtk_is_sch.o: /home/aps/r2.1.2/include/global/dapps_defs.h
stats_dtk_is_sch.o: /home/aps/r2.1.2/include/local/db_dtk.h
stats_dtk_is_cancelled.o: /usr/include/string.h
stats_dtk_is_cancelled.o: /usr/include/sys/feature_tests.h
stats_dtk_is_cancelled.o: /home/aps/r2.1.2/include/global/db_sybint.h
stats_dtk_is_cancelled.o: /home/sybase/sol/include/sybfront.h
stats_dtk_is_cancelled.o: /home/sybase/sol/include/cstypes.h
stats_dtk_is_cancelled.o: /home/sybase/sol/include/csconfig.h
stats_dtk_is_cancelled.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
stats_dtk_is_cancelled.o: /home/sybase/sol/include/sybdb.h
stats_dtk_is_cancelled.o: /usr/include/stdio.h
stats_dtk_is_cancelled.o: /home/sybase/sol/include/syblogin.h
stats_dtk_is_cancelled.o: /home/sybase/sol/include/syberror.h
stats_dtk_is_cancelled.o: /home/aps/r2.1.2/include/global/dapps_list.h
stats_dtk_is_cancelled.o: /home/aps/r2.1.2/include/global/llist_err.h
stats_dtk_is_cancelled.o: /home/aps/r2.1.2/include/global/llist.h
stats_dtk_is_cancelled.o: /home/aps/r2.1.2/include/global/ord_llist.h
stats_dtk_is_cancelled.o: /home/aps/r2.1.2/include/global/dapps_defs.h
stats_dtk_is_cancelled.o: /home/aps/r2.1.2/include/local/db_dtk.h
stats_dtk_is_reduced.o: /usr/include/string.h
stats_dtk_is_reduced.o: /usr/include/sys/feature_tests.h
stats_dtk_is_reduced.o: /home/aps/r2.1.2/include/global/db_sybint.h
stats_dtk_is_reduced.o: /home/sybase/sol/include/sybfront.h
stats_dtk_is_reduced.o: /home/sybase/sol/include/cstypes.h
stats_dtk_is_reduced.o: /home/sybase/sol/include/csconfig.h
stats_dtk_is_reduced.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
stats_dtk_is_reduced.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
stats_dtk_is_reduced.o: /home/sybase/sol/include/syblogin.h
stats_dtk_is_reduced.o: /home/sybase/sol/include/syberror.h
stats_dtk_is_reduced.o: /home/aps/r2.1.2/include/global/dapps_list.h
stats_dtk_is_reduced.o: /home/aps/r2.1.2/include/global/llist_err.h
stats_dtk_is_reduced.o: /home/aps/r2.1.2/include/global/llist.h
stats_dtk_is_reduced.o: /home/aps/r2.1.2/include/global/ord_llist.h
stats_dtk_is_reduced.o: /home/aps/r2.1.2/include/global/dapps_defs.h
stats_dtk_is_reduced.o: /home/aps/r2.1.2/include/local/db_dtk.h
stats_init_pmf_values.o: /usr/include/string.h
stats_init_pmf_values.o: /usr/include/sys/feature_tests.h aps_Statistics.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/global/db_sybint.h
stats_init_pmf_values.o: /home/sybase/sol/include/sybfront.h
stats_init_pmf_values.o: /home/sybase/sol/include/cstypes.h
stats_init_pmf_values.o: /home/sybase/sol/include/csconfig.h
stats_init_pmf_values.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
stats_init_pmf_values.o: /home/sybase/sol/include/sybdb.h
stats_init_pmf_values.o: /usr/include/stdio.h
stats_init_pmf_values.o: /home/sybase/sol/include/syblogin.h
stats_init_pmf_values.o: /home/sybase/sol/include/syberror.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/global/dapps_list.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/global/llist_err.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/global/llist.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/global/ord_llist.h
stats_init_pmf_values.o: /ASF/PRECM/include/imsdads/ims_const.h
stats_init_pmf_values.o: /ASF/PRECM/include/imsdads/ims_msg.h
stats_init_pmf_values.o: /ASF/PRECM/include/imsdads/ims_archive.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/local/dtkm_utilities.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/local/db_stats_calls.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/local/db_dtk.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/local/timeconv.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/global/dapps_defs.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/local/aps_db_table.h
stats_init_pmf_values.o: /home/aps/r2.1.2/include/local/aps_db_tablenums.h
stats_report_all_downlinks.o: /usr/include/string.h
stats_report_all_downlinks.o: /usr/include/sys/feature_tests.h
stats_report_all_downlinks.o: /usr/include/stdlib.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/global/dapps_defs.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/local/timeconv.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/local/db_dtk.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/local/dtkm_utilities.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/global/db_sybint.h
stats_report_all_downlinks.o: /home/sybase/sol/include/sybfront.h
stats_report_all_downlinks.o: /home/sybase/sol/include/cstypes.h
stats_report_all_downlinks.o: /home/sybase/sol/include/csconfig.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
stats_report_all_downlinks.o: /home/sybase/sol/include/sybdb.h
stats_report_all_downlinks.o: /usr/include/stdio.h
stats_report_all_downlinks.o: /home/sybase/sol/include/syblogin.h
stats_report_all_downlinks.o: /home/sybase/sol/include/syberror.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/global/dapps_list.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/global/llist_err.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/global/llist.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/global/ord_llist.h
stats_report_all_downlinks.o: aps_Statistics.h
stats_report_all_downlinks.o: /ASF/PRECM/include/imsdads/ims_const.h
stats_report_all_downlinks.o: /ASF/PRECM/include/imsdads/ims_msg.h
stats_report_all_downlinks.o: /ASF/PRECM/include/imsdads/ims_archive.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/local/db_stats_calls.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/local/aps_db_table.h
stats_report_all_downlinks.o: /home/aps/r2.1.2/include/local/aps_db_tablenums.h
stats_report_all_masks.o: /usr/include/string.h
stats_report_all_masks.o: /usr/include/sys/feature_tests.h
stats_report_all_masks.o: /usr/include/stdlib.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/global/dapps_defs.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/local/timeconv.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/local/db_dtk.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/local/dtkm_utilities.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/global/db_sybint.h
stats_report_all_masks.o: /home/sybase/sol/include/sybfront.h
stats_report_all_masks.o: /home/sybase/sol/include/cstypes.h
stats_report_all_masks.o: /home/sybase/sol/include/csconfig.h
stats_report_all_masks.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
stats_report_all_masks.o: /home/sybase/sol/include/sybdb.h
stats_report_all_masks.o: /usr/include/stdio.h
stats_report_all_masks.o: /home/sybase/sol/include/syblogin.h
stats_report_all_masks.o: /home/sybase/sol/include/syberror.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/global/dapps_list.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/global/llist_err.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/global/llist.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/global/ord_llist.h
stats_report_all_masks.o: aps_Statistics.h
stats_report_all_masks.o: /ASF/PRECM/include/imsdads/ims_const.h
stats_report_all_masks.o: /ASF/PRECM/include/imsdads/ims_msg.h
stats_report_all_masks.o: /ASF/PRECM/include/imsdads/ims_archive.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/local/db_stats_calls.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/local/aps_db_table.h
stats_report_all_masks.o: /home/aps/r2.1.2/include/local/aps_db_tablenums.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/local/aps_db_table.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/global/db_sybint.h
stats_report_all_aos_los.o: /home/sybase/sol/include/sybfront.h
stats_report_all_aos_los.o: /home/sybase/sol/include/cstypes.h
stats_report_all_aos_los.o: /home/sybase/sol/include/csconfig.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
stats_report_all_aos_los.o: /home/sybase/sol/include/sybdb.h
stats_report_all_aos_los.o: /usr/include/stdio.h
stats_report_all_aos_los.o: /usr/include/sys/feature_tests.h
stats_report_all_aos_los.o: /home/sybase/sol/include/syblogin.h
stats_report_all_aos_los.o: /home/sybase/sol/include/syberror.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/global/dapps_list.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/global/llist_err.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/global/llist.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/global/ord_llist.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/local/aps_db_tablenums.h
stats_report_all_aos_los.o: /usr/include/string.h /usr/include/stdlib.h
stats_report_all_aos_los.o: aps_Statistics.h
stats_report_all_aos_los.o: /ASF/PRECM/include/imsdads/ims_const.h
stats_report_all_aos_los.o: /ASF/PRECM/include/imsdads/ims_msg.h
stats_report_all_aos_los.o: /ASF/PRECM/include/imsdads/ims_archive.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/local/dtkm_utilities.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/local/db_stats_calls.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/local/db_dtk.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/local/timeconv.h
stats_report_all_aos_los.o: /home/aps/r2.1.2/include/global/dapps_defs.h
stats_report_sar_pln.o: /usr/include/string.h
stats_report_sar_pln.o: /usr/include/sys/feature_tests.h
stats_report_sar_pln.o: /usr/include/stdlib.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/local/timeconv.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/global/dapps_defs.h
stats_report_sar_pln.o: aps_Statistics.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/global/db_sybint.h
stats_report_sar_pln.o: /home/sybase/sol/include/sybfront.h
stats_report_sar_pln.o: /home/sybase/sol/include/cstypes.h
stats_report_sar_pln.o: /home/sybase/sol/include/csconfig.h
stats_report_sar_pln.o: /home/aps/r2.1.2/src/makedepend_files/stsdef.h
stats_report_sar_pln.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
stats_report_sar_pln.o: /home/sybase/sol/include/syblogin.h
stats_report_sar_pln.o: /home/sybase/sol/include/syberror.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/global/dapps_list.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/global/llist_err.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/global/llist.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/global/ord_llist.h
stats_report_sar_pln.o: /ASF/PRECM/include/imsdads/ims_const.h
stats_report_sar_pln.o: /ASF/PRECM/include/imsdads/ims_msg.h
stats_report_sar_pln.o: /ASF/PRECM/include/imsdads/ims_archive.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/local/dtkm_utilities.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/local/db_stats_calls.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/local/db_dtk.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/local/aps_db_table.h
stats_report_sar_pln.o: /home/aps/r2.1.2/include/local/aps_db_tablenums.h
