###############################################################################
#
# File:         lib_dtkm/Makefile 
#
# Function:     APS Makefile for libdtkm.a
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#	"@(#)Makefile	5.2 98/03/03 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.Makefile"
#
###############################################################################

ifdef CM_HOME
	APS_HOME = $(CM_HOME)/aps
endif

include ${APS_HOME}/src/aps_makedefs


###############################################################################
#
# Source Default Directory
#   Typically off of $APS_HOME/src... this is used to retrieve source
#
###############################################################################
 
SRC_DIR = $(APS_HOME)/src/lib_dtkm

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL = \
	$(IODL) \
	$(IAPS) \
	$(IXMOTIF) \
	$(IGKS) \
	$(ISYBASE) \
	$(IQAPARTNER) \
	$(IENCRYPT) \
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

LIB          =  libdtkm.a

SRC          =       \
				dtkm_activities_conflict.c       \
				dtkm_add_rdl2dtks.c \
				dtkm_aps_does_cvrg4sat.c         \
				dtkm_blank_values.c              \
				dtkm_check_antenna_down_times.c   \
				dtkm_check_antenna_conflicts.c     \
				dtkm_check_bumpability.c           \
				dtkm_check_same_sat_conflicts.c   \
				dtkm_check_sat_equipment.c       \
				dtkm_check_sat_group_conflict.c   \
				dtkm_check_values.c             \
				dtkm_combine_dtks.c               \
				dtkm_copy_dtks_value.c               \
				dtkm_create_antenna_list.c          \
				dtkm_create_same_sat_time_bracket.c  \
				dtkm_default_values.c                \
				dtkm_delete_dl2obs_recs.c            \
				dtkm_delete_dtk.c                   \
				dtkm_delete_dtk_from_list.c          \
				dtkm_delete_dtk_record.c             \
				dtkm_determine_station_mask.c        \
				dtkm_dl2obs.c                 \
				dtkm_duplicate_dtk_into_list.c       \
				dtkm_error_message.c                 \
				dtkm_extract_first_sat.c             \
				dtkm_extract_other_sat_dtks.c        \
				dtkm_extract_same_sat_dtks.c        \
				dtkm_find_antenna_for_dtk.c          \
				dtkm_fix_dl2obs.c                   \
				dtkm_get_antenna_dtk_padding_time.c  \
				dtkm_get_antenna_preference.c        \
				dtkm_get_antenna_priority.c          \
				dtkm_get_cvrg_points.c               \
				dtkm_get_dtkstat_where_clause.c      \
				dtkm_get_earliest_submit_time.c      \
				dtkm_get_firstlast_track.c			\
				dtkm_get_highest_antenna_priority.c  \
				dtkm_get_mask_times.c                  \
				dtkm_get_quicklook_values.c             \
				dtkm_get_rej_conflicts.c             \
				dtkm_get_sitename.c                  \
				dtkm_in_station_mask.c              \
				dtkm_insert_dtk_record.c             \
				dtkm_insert_dl2obs_rec.c             \
				dtkm_is_a_downlink.c                \
				dtkm_is_a_realtime_downlink.c        \
				dtkm_is_a_realtime_observation.c     \
				dtkm_is_a_recording.c               \
				dtkm_is_a_tape_dump.c               \
				dtkm_is_an_observation.c            \
				dtkm_is_in_antarctic_mode.c        \
				dtkm_obs2dl.c                      \
				dtkm_print_list.c                  \
				dtkm_process_antenna_down_time.c  \
				dtkm_process_dtk.c                \
				dtkm_process_dtk_proposal.c         \
				dtkm_process_dtk_proposal_list.c    \
				dtkm_reduce_accepted_dtks.c             \
				dtkm_remove_dtks_from_list.c          \
				dtkm_remove_observations.c              \
				dtkm_r1_update_sensor.c              \
				dtkm_set_asf_reduction_min.c    \
				dtkm_set_dtkstat_from_dl.c    \
				dtkm_sat_has_recorder.c              \
				dtkm_set_fa_schedule_link_from_list.c  \
				dtkm_sat_sensor_is_low_bit_rate.c    \
				dtkm_set_proposed_dtkstat.c    \
				dtkm_time_bracket_dtk_list.c   \
				dtkm_update_best_antenna.c    \
				dtkm_update_dl_quicklook.c     \
				dtkm_update_dl2obs_4dtk.c     \
				dtkm_update_dtk.c            \
				dtkm_update_dtk_darid.c         \
				dtkm_update_dtk_record.c     \
				dtkm_update_dtk_sensor.c     \
				dtkm_update_dtks_field.c     \
				dtkm_update_obs_dtkstat.c     \
				dtkm_update_obs_times_from_rdl.c     \
				dtkm_2dtks_concur.c   

HEADER_FILES =  dtkm.h

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
	@echo "Linking    $@"
	@$(CC) -o $@ $(OBJS) $(LALL) $(RALL) $(LIBS)
	@echo "Done"
 
$(LIB): $(OBJS)
	@echo
	@echo "Creating/Modifying Library $(LIB)"
	@$(ARCHIVE) rucv $@ $?
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

dtkm_activities_conflict.o: dtkm.h /usr/include/string.h
dtkm_activities_conflict.o: /usr/include/sys/feature_tests.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_activities_conflict.o: /home/sybase/sol/include/sybfront.h
dtkm_activities_conflict.o: /home/sybase/sol/include/cstypes.h
dtkm_activities_conflict.o: /home/sybase/sol/include/csconfig.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_activities_conflict.o: /home/sybase/sol/include/sybdb.h
dtkm_activities_conflict.o: /usr/include/stdio.h
dtkm_activities_conflict.o: /home/sybase/sol/include/syblogin.h
dtkm_activities_conflict.o: /home/sybase/sol/include/syberror.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_activities_conflict.o: /usr/include/values.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_activities_conflict.o: /usr/include/stddef.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_activities_conflict.o: /ASF/PRECM/include/sys/odldef.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/local/db_activities.h
dtkm_activities_conflict.o: /home/aps/r2.1.1/include/local/db_activ_conf.h
dtkm_add_rdl2dtks.o: dtkm.h /usr/include/string.h
dtkm_add_rdl2dtks.o: /usr/include/sys/feature_tests.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_add_rdl2dtks.o: /home/sybase/sol/include/sybfront.h
dtkm_add_rdl2dtks.o: /home/sybase/sol/include/cstypes.h
dtkm_add_rdl2dtks.o: /home/sybase/sol/include/csconfig.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_add_rdl2dtks.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_add_rdl2dtks.o: /home/sybase/sol/include/syblogin.h
dtkm_add_rdl2dtks.o: /home/sybase/sol/include/syberror.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_add_rdl2dtks.o: /usr/include/values.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_add_rdl2dtks.o: /usr/include/stddef.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_add_rdl2dtks.o: /ASF/PRECM/include/sys/odldef.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_add_rdl2dtks.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_aps_does_cvrg4sat.o: dtkm.h /usr/include/string.h
dtkm_aps_does_cvrg4sat.o: /usr/include/sys/feature_tests.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_aps_does_cvrg4sat.o: /home/sybase/sol/include/sybfront.h
dtkm_aps_does_cvrg4sat.o: /home/sybase/sol/include/cstypes.h
dtkm_aps_does_cvrg4sat.o: /home/sybase/sol/include/csconfig.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_aps_does_cvrg4sat.o: /home/sybase/sol/include/sybdb.h
dtkm_aps_does_cvrg4sat.o: /usr/include/stdio.h
dtkm_aps_does_cvrg4sat.o: /home/sybase/sol/include/syblogin.h
dtkm_aps_does_cvrg4sat.o: /home/sybase/sol/include/syberror.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_aps_does_cvrg4sat.o: /usr/include/values.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_aps_does_cvrg4sat.o: /usr/include/stddef.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_aps_does_cvrg4sat.o: /ASF/PRECM/include/sys/odldef.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_aps_does_cvrg4sat.o: /home/aps/r2.1.1/include/local/db_satsensor.h
dtkm_blank_values.o: dtkm.h /usr/include/string.h
dtkm_blank_values.o: /usr/include/sys/feature_tests.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_blank_values.o: /home/sybase/sol/include/sybfront.h
dtkm_blank_values.o: /home/sybase/sol/include/cstypes.h
dtkm_blank_values.o: /home/sybase/sol/include/csconfig.h
dtkm_blank_values.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_blank_values.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_blank_values.o: /home/sybase/sol/include/syblogin.h
dtkm_blank_values.o: /home/sybase/sol/include/syberror.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_blank_values.o: /usr/include/values.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_blank_values.o: /usr/include/stddef.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_blank_values.o: /ASF/PRECM/include/sys/odldef.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_blank_values.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_check_antenna_down_times.o: dtkm.h /usr/include/string.h
dtkm_check_antenna_down_times.o: /usr/include/sys/feature_tests.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_check_antenna_down_times.o: /home/sybase/sol/include/sybfront.h
dtkm_check_antenna_down_times.o: /home/sybase/sol/include/cstypes.h
dtkm_check_antenna_down_times.o: /home/sybase/sol/include/csconfig.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_check_antenna_down_times.o: /home/sybase/sol/include/sybdb.h
dtkm_check_antenna_down_times.o: /usr/include/stdio.h
dtkm_check_antenna_down_times.o: /home/sybase/sol/include/syblogin.h
dtkm_check_antenna_down_times.o: /home/sybase/sol/include/syberror.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_check_antenna_down_times.o: /usr/include/values.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_check_antenna_down_times.o: /usr/include/stddef.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_check_antenna_down_times.o: /ASF/PRECM/include/sys/odldef.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_check_antenna_down_times.o: /home/aps/r2.1.1/include/local/db_antenna_down_times.h
dtkm_check_antenna_conflicts.o: /usr/include/string.h
dtkm_check_antenna_conflicts.o: /usr/include/sys/feature_tests.h
dtkm_check_antenna_conflicts.o: /usr/include/stdlib.h dtkm.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_check_antenna_conflicts.o: /home/sybase/sol/include/sybfront.h
dtkm_check_antenna_conflicts.o: /home/sybase/sol/include/cstypes.h
dtkm_check_antenna_conflicts.o: /home/sybase/sol/include/csconfig.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_check_antenna_conflicts.o: /home/sybase/sol/include/sybdb.h
dtkm_check_antenna_conflicts.o: /usr/include/stdio.h
dtkm_check_antenna_conflicts.o: /home/sybase/sol/include/syblogin.h
dtkm_check_antenna_conflicts.o: /home/sybase/sol/include/syberror.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_check_antenna_conflicts.o: /usr/include/values.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_check_antenna_conflicts.o: /usr/include/stddef.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_check_antenna_conflicts.o: /ASF/PRECM/include/sys/odldef.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_check_antenna_conflicts.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_check_bumpability.o: dtkm.h /usr/include/string.h
dtkm_check_bumpability.o: /usr/include/sys/feature_tests.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_check_bumpability.o: /home/sybase/sol/include/sybfront.h
dtkm_check_bumpability.o: /home/sybase/sol/include/cstypes.h
dtkm_check_bumpability.o: /home/sybase/sol/include/csconfig.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_check_bumpability.o: /home/sybase/sol/include/sybdb.h
dtkm_check_bumpability.o: /usr/include/stdio.h
dtkm_check_bumpability.o: /home/sybase/sol/include/syblogin.h
dtkm_check_bumpability.o: /home/sybase/sol/include/syberror.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_check_bumpability.o: /usr/include/values.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_check_bumpability.o: /usr/include/stddef.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_check_bumpability.o: /ASF/PRECM/include/sys/odldef.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_check_bumpability.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_check_same_sat_conflicts.o: dtkm.h /usr/include/string.h
dtkm_check_same_sat_conflicts.o: /usr/include/sys/feature_tests.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_check_same_sat_conflicts.o: /home/sybase/sol/include/sybfront.h
dtkm_check_same_sat_conflicts.o: /home/sybase/sol/include/cstypes.h
dtkm_check_same_sat_conflicts.o: /home/sybase/sol/include/csconfig.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_check_same_sat_conflicts.o: /home/sybase/sol/include/sybdb.h
dtkm_check_same_sat_conflicts.o: /usr/include/stdio.h
dtkm_check_same_sat_conflicts.o: /home/sybase/sol/include/syblogin.h
dtkm_check_same_sat_conflicts.o: /home/sybase/sol/include/syberror.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_check_same_sat_conflicts.o: /usr/include/values.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_check_same_sat_conflicts.o: /usr/include/stddef.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_check_same_sat_conflicts.o: /ASF/PRECM/include/sys/odldef.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_check_same_sat_conflicts.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_check_sat_equipment.o: dtkm.h /usr/include/string.h
dtkm_check_sat_equipment.o: /usr/include/sys/feature_tests.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_check_sat_equipment.o: /home/sybase/sol/include/sybfront.h
dtkm_check_sat_equipment.o: /home/sybase/sol/include/cstypes.h
dtkm_check_sat_equipment.o: /home/sybase/sol/include/csconfig.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_check_sat_equipment.o: /home/sybase/sol/include/sybdb.h
dtkm_check_sat_equipment.o: /usr/include/stdio.h
dtkm_check_sat_equipment.o: /home/sybase/sol/include/syblogin.h
dtkm_check_sat_equipment.o: /home/sybase/sol/include/syberror.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_check_sat_equipment.o: /usr/include/values.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_check_sat_equipment.o: /usr/include/stddef.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_check_sat_equipment.o: /ASF/PRECM/include/sys/odldef.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/local/db_j1_dn_times.h
dtkm_check_sat_equipment.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_check_sat_group_conflict.o: dtkm.h /usr/include/string.h
dtkm_check_sat_group_conflict.o: /usr/include/sys/feature_tests.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_check_sat_group_conflict.o: /home/sybase/sol/include/sybfront.h
dtkm_check_sat_group_conflict.o: /home/sybase/sol/include/cstypes.h
dtkm_check_sat_group_conflict.o: /home/sybase/sol/include/csconfig.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_check_sat_group_conflict.o: /home/sybase/sol/include/sybdb.h
dtkm_check_sat_group_conflict.o: /usr/include/stdio.h
dtkm_check_sat_group_conflict.o: /home/sybase/sol/include/syblogin.h
dtkm_check_sat_group_conflict.o: /home/sybase/sol/include/syberror.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_check_sat_group_conflict.o: /usr/include/values.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_check_sat_group_conflict.o: /usr/include/stddef.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_check_sat_group_conflict.o: /ASF/PRECM/include/sys/odldef.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_check_sat_group_conflict.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_check_values.o: /usr/include/string.h /usr/include/sys/feature_tests.h
dtkm_check_values.o: /usr/include/math.h /usr/include/stdio.h dtkm.h
dtkm_check_values.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_check_values.o: /home/sybase/sol/include/sybfront.h
dtkm_check_values.o: /home/sybase/sol/include/cstypes.h
dtkm_check_values.o: /home/sybase/sol/include/csconfig.h
dtkm_check_values.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_check_values.o: /home/sybase/sol/include/sybdb.h
dtkm_check_values.o: /home/sybase/sol/include/syblogin.h
dtkm_check_values.o: /home/sybase/sol/include/syberror.h
dtkm_check_values.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_check_values.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_check_values.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_check_values.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_check_values.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_check_values.o: /usr/include/values.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_check_values.o: /usr/include/stddef.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_check_values.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_check_values.o: /ASF/PRECM/include/sys/odldef.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/db_activ_conf.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/db_activities.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/db_antenna.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/db_antenna_pref.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/db_sat_inclusion_period.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/db_satsensor.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/db_station.h
dtkm_check_values.o: /home/aps/r2.1.1/include/local/check_lat_lon.h
dtkm_combine_dtks.o: dtkm.h /usr/include/string.h
dtkm_combine_dtks.o: /usr/include/sys/feature_tests.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_combine_dtks.o: /home/sybase/sol/include/sybfront.h
dtkm_combine_dtks.o: /home/sybase/sol/include/cstypes.h
dtkm_combine_dtks.o: /home/sybase/sol/include/csconfig.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_combine_dtks.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_combine_dtks.o: /home/sybase/sol/include/syblogin.h
dtkm_combine_dtks.o: /home/sybase/sol/include/syberror.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_combine_dtks.o: /usr/include/values.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_combine_dtks.o: /usr/include/stddef.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_combine_dtks.o: /ASF/PRECM/include/sys/odldef.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/local/db_dl2obs.h
dtkm_combine_dtks.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_copy_dtks_value.o: dtkm.h /usr/include/string.h
dtkm_copy_dtks_value.o: /usr/include/sys/feature_tests.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_copy_dtks_value.o: /home/sybase/sol/include/sybfront.h
dtkm_copy_dtks_value.o: /home/sybase/sol/include/cstypes.h
dtkm_copy_dtks_value.o: /home/sybase/sol/include/csconfig.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_copy_dtks_value.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_copy_dtks_value.o: /home/sybase/sol/include/syblogin.h
dtkm_copy_dtks_value.o: /home/sybase/sol/include/syberror.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_copy_dtks_value.o: /usr/include/values.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_copy_dtks_value.o: /usr/include/stddef.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_copy_dtks_value.o: /ASF/PRECM/include/sys/odldef.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_copy_dtks_value.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_create_antenna_list.o: dtkm.h /usr/include/string.h
dtkm_create_antenna_list.o: /usr/include/sys/feature_tests.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_create_antenna_list.o: /home/sybase/sol/include/sybfront.h
dtkm_create_antenna_list.o: /home/sybase/sol/include/cstypes.h
dtkm_create_antenna_list.o: /home/sybase/sol/include/csconfig.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_create_antenna_list.o: /home/sybase/sol/include/sybdb.h
dtkm_create_antenna_list.o: /usr/include/stdio.h
dtkm_create_antenna_list.o: /home/sybase/sol/include/syblogin.h
dtkm_create_antenna_list.o: /home/sybase/sol/include/syberror.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_create_antenna_list.o: /usr/include/values.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_create_antenna_list.o: /usr/include/stddef.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_create_antenna_list.o: /ASF/PRECM/include/sys/odldef.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/local/db_antenna_pref.h
dtkm_create_antenna_list.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_create_same_sat_time_bracket.o: /home/sybase/sol/include/sybfront.h
dtkm_create_same_sat_time_bracket.o: /home/sybase/sol/include/cstypes.h
dtkm_create_same_sat_time_bracket.o: /home/sybase/sol/include/csconfig.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_create_same_sat_time_bracket.o: /home/sybase/sol/include/sybdb.h
dtkm_create_same_sat_time_bracket.o: /usr/include/stdio.h
dtkm_create_same_sat_time_bracket.o: /usr/include/sys/feature_tests.h
dtkm_create_same_sat_time_bracket.o: /home/sybase/sol/include/syblogin.h
dtkm_create_same_sat_time_bracket.o: /home/sybase/sol/include/syberror.h
dtkm_create_same_sat_time_bracket.o: dtkm.h /usr/include/string.h
dtkm_create_same_sat_time_bracket.o: /usr/include/values.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_create_same_sat_time_bracket.o: /usr/include/stddef.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_create_same_sat_time_bracket.o: /ASF/PRECM/include/sys/odldef.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_create_same_sat_time_bracket.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_default_values.o: /usr/include/string.h /usr/include/sys/feature_tests.h
dtkm_default_values.o: /usr/include/math.h /usr/include/stdio.h dtkm.h
dtkm_default_values.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_default_values.o: /home/sybase/sol/include/sybfront.h
dtkm_default_values.o: /home/sybase/sol/include/cstypes.h
dtkm_default_values.o: /home/sybase/sol/include/csconfig.h
dtkm_default_values.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_default_values.o: /home/sybase/sol/include/sybdb.h
dtkm_default_values.o: /home/sybase/sol/include/syblogin.h
dtkm_default_values.o: /home/sybase/sol/include/syberror.h
dtkm_default_values.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_default_values.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_default_values.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_default_values.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_default_values.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_default_values.o: /usr/include/values.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_default_values.o: /usr/include/stddef.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_default_values.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_default_values.o: /ASF/PRECM/include/sys/odldef.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/db_activ_conf.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/db_activities.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/db_dar.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/db_satsensor.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/db_station.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_default_values.o: /home/aps/r2.1.1/include/local/check_lat_lon.h
dtkm_delete_dl2obs_recs.o: dtkm.h /usr/include/string.h
dtkm_delete_dl2obs_recs.o: /usr/include/sys/feature_tests.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_delete_dl2obs_recs.o: /home/sybase/sol/include/sybfront.h
dtkm_delete_dl2obs_recs.o: /home/sybase/sol/include/cstypes.h
dtkm_delete_dl2obs_recs.o: /home/sybase/sol/include/csconfig.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_delete_dl2obs_recs.o: /home/sybase/sol/include/sybdb.h
dtkm_delete_dl2obs_recs.o: /usr/include/stdio.h
dtkm_delete_dl2obs_recs.o: /home/sybase/sol/include/syblogin.h
dtkm_delete_dl2obs_recs.o: /home/sybase/sol/include/syberror.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_delete_dl2obs_recs.o: /usr/include/values.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_delete_dl2obs_recs.o: /usr/include/stddef.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_delete_dl2obs_recs.o: /ASF/PRECM/include/sys/odldef.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/local/db_dl2obs.h
dtkm_delete_dl2obs_recs.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_delete_dtk.o: /usr/include/string.h /usr/include/sys/feature_tests.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_delete_dtk.o: /home/sybase/sol/include/sybfront.h
dtkm_delete_dtk.o: /home/sybase/sol/include/cstypes.h
dtkm_delete_dtk.o: /home/sybase/sol/include/csconfig.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_delete_dtk.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_delete_dtk.o: /home/sybase/sol/include/syblogin.h
dtkm_delete_dtk.o: /home/sybase/sol/include/syberror.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h dtkm.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_delete_dtk.o: /usr/include/values.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_delete_dtk.o: /usr/include/stddef.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_delete_dtk.o: /ASF/PRECM/include/sys/odldef.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_delete_dtk.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_delete_dtk_from_list.o: dtkm.h /usr/include/string.h
dtkm_delete_dtk_from_list.o: /usr/include/sys/feature_tests.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_delete_dtk_from_list.o: /home/sybase/sol/include/sybfront.h
dtkm_delete_dtk_from_list.o: /home/sybase/sol/include/cstypes.h
dtkm_delete_dtk_from_list.o: /home/sybase/sol/include/csconfig.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_delete_dtk_from_list.o: /home/sybase/sol/include/sybdb.h
dtkm_delete_dtk_from_list.o: /usr/include/stdio.h
dtkm_delete_dtk_from_list.o: /home/sybase/sol/include/syblogin.h
dtkm_delete_dtk_from_list.o: /home/sybase/sol/include/syberror.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_delete_dtk_from_list.o: /usr/include/values.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_delete_dtk_from_list.o: /usr/include/stddef.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_delete_dtk_from_list.o: /ASF/PRECM/include/sys/odldef.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_delete_dtk_from_list.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_delete_dtk_record.o: dtkm.h /usr/include/string.h
dtkm_delete_dtk_record.o: /usr/include/sys/feature_tests.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_delete_dtk_record.o: /home/sybase/sol/include/sybfront.h
dtkm_delete_dtk_record.o: /home/sybase/sol/include/cstypes.h
dtkm_delete_dtk_record.o: /home/sybase/sol/include/csconfig.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_delete_dtk_record.o: /home/sybase/sol/include/sybdb.h
dtkm_delete_dtk_record.o: /usr/include/stdio.h
dtkm_delete_dtk_record.o: /home/sybase/sol/include/syblogin.h
dtkm_delete_dtk_record.o: /home/sybase/sol/include/syberror.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_delete_dtk_record.o: /usr/include/values.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_delete_dtk_record.o: /usr/include/stddef.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_delete_dtk_record.o: /ASF/PRECM/include/sys/odldef.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_delete_dtk_record.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_determine_station_mask.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_determine_station_mask.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_determine_station_mask.o: /home/sybase/sol/include/sybfront.h
dtkm_determine_station_mask.o: /home/sybase/sol/include/cstypes.h
dtkm_determine_station_mask.o: /home/sybase/sol/include/csconfig.h
dtkm_determine_station_mask.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_determine_station_mask.o: /home/sybase/sol/include/sybdb.h
dtkm_determine_station_mask.o: /usr/include/stdio.h
dtkm_determine_station_mask.o: /usr/include/sys/feature_tests.h
dtkm_determine_station_mask.o: /home/sybase/sol/include/syblogin.h
dtkm_determine_station_mask.o: /home/sybase/sol/include/syberror.h
dtkm_determine_station_mask.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_determine_station_mask.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_determine_station_mask.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_determine_station_mask.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_determine_station_mask.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_determine_station_mask.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_determine_station_mask.o: /home/aps/r2.1.1/include/local/db_station.h
dtkm_determine_station_mask.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_determine_station_mask.o: /usr/include/string.h
dtkm_dl2obs.o: dtkm.h /usr/include/string.h /usr/include/sys/feature_tests.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_dl2obs.o: /home/sybase/sol/include/sybfront.h
dtkm_dl2obs.o: /home/sybase/sol/include/cstypes.h
dtkm_dl2obs.o: /home/sybase/sol/include/csconfig.h
dtkm_dl2obs.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_dl2obs.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_dl2obs.o: /home/sybase/sol/include/syblogin.h
dtkm_dl2obs.o: /home/sybase/sol/include/syberror.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_dl2obs.o: /usr/include/values.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_dl2obs.o: /usr/include/stddef.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_dl2obs.o: /ASF/PRECM/include/sys/odldef.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_dl2obs.o: /home/aps/r2.1.1/include/local/db_dl2obs.h
dtkm_duplicate_dtk_into_list.o: dtkm.h /usr/include/string.h
dtkm_duplicate_dtk_into_list.o: /usr/include/sys/feature_tests.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_duplicate_dtk_into_list.o: /home/sybase/sol/include/sybfront.h
dtkm_duplicate_dtk_into_list.o: /home/sybase/sol/include/cstypes.h
dtkm_duplicate_dtk_into_list.o: /home/sybase/sol/include/csconfig.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_duplicate_dtk_into_list.o: /home/sybase/sol/include/sybdb.h
dtkm_duplicate_dtk_into_list.o: /usr/include/stdio.h
dtkm_duplicate_dtk_into_list.o: /home/sybase/sol/include/syblogin.h
dtkm_duplicate_dtk_into_list.o: /home/sybase/sol/include/syberror.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_duplicate_dtk_into_list.o: /usr/include/values.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_duplicate_dtk_into_list.o: /usr/include/stddef.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_duplicate_dtk_into_list.o: /ASF/PRECM/include/sys/odldef.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_duplicate_dtk_into_list.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_extract_first_sat.o: dtkm.h /usr/include/string.h
dtkm_extract_first_sat.o: /usr/include/sys/feature_tests.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_extract_first_sat.o: /home/sybase/sol/include/sybfront.h
dtkm_extract_first_sat.o: /home/sybase/sol/include/cstypes.h
dtkm_extract_first_sat.o: /home/sybase/sol/include/csconfig.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_extract_first_sat.o: /home/sybase/sol/include/sybdb.h
dtkm_extract_first_sat.o: /usr/include/stdio.h
dtkm_extract_first_sat.o: /home/sybase/sol/include/syblogin.h
dtkm_extract_first_sat.o: /home/sybase/sol/include/syberror.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_extract_first_sat.o: /usr/include/values.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_extract_first_sat.o: /usr/include/stddef.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_extract_first_sat.o: /ASF/PRECM/include/sys/odldef.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_extract_first_sat.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_extract_other_sat_dtks.o: dtkm.h /usr/include/string.h
dtkm_extract_other_sat_dtks.o: /usr/include/sys/feature_tests.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_extract_other_sat_dtks.o: /home/sybase/sol/include/sybfront.h
dtkm_extract_other_sat_dtks.o: /home/sybase/sol/include/cstypes.h
dtkm_extract_other_sat_dtks.o: /home/sybase/sol/include/csconfig.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_extract_other_sat_dtks.o: /home/sybase/sol/include/sybdb.h
dtkm_extract_other_sat_dtks.o: /usr/include/stdio.h
dtkm_extract_other_sat_dtks.o: /home/sybase/sol/include/syblogin.h
dtkm_extract_other_sat_dtks.o: /home/sybase/sol/include/syberror.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_extract_other_sat_dtks.o: /usr/include/values.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_extract_other_sat_dtks.o: /usr/include/stddef.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_extract_other_sat_dtks.o: /ASF/PRECM/include/sys/odldef.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_extract_other_sat_dtks.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_extract_same_sat_dtks.o: dtkm.h /usr/include/string.h
dtkm_extract_same_sat_dtks.o: /usr/include/sys/feature_tests.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_extract_same_sat_dtks.o: /home/sybase/sol/include/sybfront.h
dtkm_extract_same_sat_dtks.o: /home/sybase/sol/include/cstypes.h
dtkm_extract_same_sat_dtks.o: /home/sybase/sol/include/csconfig.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_extract_same_sat_dtks.o: /home/sybase/sol/include/sybdb.h
dtkm_extract_same_sat_dtks.o: /usr/include/stdio.h
dtkm_extract_same_sat_dtks.o: /home/sybase/sol/include/syblogin.h
dtkm_extract_same_sat_dtks.o: /home/sybase/sol/include/syberror.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_extract_same_sat_dtks.o: /usr/include/values.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_extract_same_sat_dtks.o: /usr/include/stddef.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_extract_same_sat_dtks.o: /ASF/PRECM/include/sys/odldef.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_extract_same_sat_dtks.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_find_antenna_for_dtk.o: dtkm.h /usr/include/string.h
dtkm_find_antenna_for_dtk.o: /usr/include/sys/feature_tests.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_find_antenna_for_dtk.o: /home/sybase/sol/include/sybfront.h
dtkm_find_antenna_for_dtk.o: /home/sybase/sol/include/cstypes.h
dtkm_find_antenna_for_dtk.o: /home/sybase/sol/include/csconfig.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_find_antenna_for_dtk.o: /home/sybase/sol/include/sybdb.h
dtkm_find_antenna_for_dtk.o: /usr/include/stdio.h
dtkm_find_antenna_for_dtk.o: /home/sybase/sol/include/syblogin.h
dtkm_find_antenna_for_dtk.o: /home/sybase/sol/include/syberror.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_find_antenna_for_dtk.o: /usr/include/values.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_find_antenna_for_dtk.o: /usr/include/stddef.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_find_antenna_for_dtk.o: /ASF/PRECM/include/sys/odldef.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_find_antenna_for_dtk.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_fix_dl2obs.o: dtkm.h /usr/include/string.h
dtkm_fix_dl2obs.o: /usr/include/sys/feature_tests.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_fix_dl2obs.o: /home/sybase/sol/include/sybfront.h
dtkm_fix_dl2obs.o: /home/sybase/sol/include/cstypes.h
dtkm_fix_dl2obs.o: /home/sybase/sol/include/csconfig.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_fix_dl2obs.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_fix_dl2obs.o: /home/sybase/sol/include/syblogin.h
dtkm_fix_dl2obs.o: /home/sybase/sol/include/syberror.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_fix_dl2obs.o: /usr/include/values.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_fix_dl2obs.o: /usr/include/stddef.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_fix_dl2obs.o: /ASF/PRECM/include/sys/odldef.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_fix_dl2obs.o: /home/aps/r2.1.1/include/local/db_dl2obs.h
dtkm_get_antenna_dtk_padding_time.o: dtkm.h /usr/include/string.h
dtkm_get_antenna_dtk_padding_time.o: /usr/include/sys/feature_tests.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_get_antenna_dtk_padding_time.o: /home/sybase/sol/include/sybfront.h
dtkm_get_antenna_dtk_padding_time.o: /home/sybase/sol/include/cstypes.h
dtkm_get_antenna_dtk_padding_time.o: /home/sybase/sol/include/csconfig.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_get_antenna_dtk_padding_time.o: /home/sybase/sol/include/sybdb.h
dtkm_get_antenna_dtk_padding_time.o: /usr/include/stdio.h
dtkm_get_antenna_dtk_padding_time.o: /home/sybase/sol/include/syblogin.h
dtkm_get_antenna_dtk_padding_time.o: /home/sybase/sol/include/syberror.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_get_antenna_dtk_padding_time.o: /usr/include/values.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_get_antenna_dtk_padding_time.o: /usr/include/stddef.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_get_antenna_dtk_padding_time.o: /ASF/PRECM/include/sys/odldef.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/local/db_antenna.h
dtkm_get_antenna_dtk_padding_time.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_get_antenna_preference.o: dtkm.h /usr/include/string.h
dtkm_get_antenna_preference.o: /usr/include/sys/feature_tests.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_get_antenna_preference.o: /home/sybase/sol/include/sybfront.h
dtkm_get_antenna_preference.o: /home/sybase/sol/include/cstypes.h
dtkm_get_antenna_preference.o: /home/sybase/sol/include/csconfig.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_get_antenna_preference.o: /home/sybase/sol/include/sybdb.h
dtkm_get_antenna_preference.o: /usr/include/stdio.h
dtkm_get_antenna_preference.o: /home/sybase/sol/include/syblogin.h
dtkm_get_antenna_preference.o: /home/sybase/sol/include/syberror.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_get_antenna_preference.o: /usr/include/values.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_get_antenna_preference.o: /usr/include/stddef.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_get_antenna_preference.o: /ASF/PRECM/include/sys/odldef.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/local/db_antenna_pref.h
dtkm_get_antenna_preference.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_get_antenna_priority.o: dtkm.h /usr/include/string.h
dtkm_get_antenna_priority.o: /usr/include/sys/feature_tests.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_get_antenna_priority.o: /home/sybase/sol/include/sybfront.h
dtkm_get_antenna_priority.o: /home/sybase/sol/include/cstypes.h
dtkm_get_antenna_priority.o: /home/sybase/sol/include/csconfig.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_get_antenna_priority.o: /home/sybase/sol/include/sybdb.h
dtkm_get_antenna_priority.o: /usr/include/stdio.h
dtkm_get_antenna_priority.o: /home/sybase/sol/include/syblogin.h
dtkm_get_antenna_priority.o: /home/sybase/sol/include/syberror.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_get_antenna_priority.o: /usr/include/values.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_get_antenna_priority.o: /usr/include/stddef.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_get_antenna_priority.o: /ASF/PRECM/include/sys/odldef.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/local/db_antenna_priority.h
dtkm_get_antenna_priority.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_get_cvrg_points.o: dtkm.h /usr/include/string.h
dtkm_get_cvrg_points.o: /usr/include/sys/feature_tests.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_get_cvrg_points.o: /home/sybase/sol/include/sybfront.h
dtkm_get_cvrg_points.o: /home/sybase/sol/include/cstypes.h
dtkm_get_cvrg_points.o: /home/sybase/sol/include/csconfig.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_get_cvrg_points.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_get_cvrg_points.o: /home/sybase/sol/include/syblogin.h
dtkm_get_cvrg_points.o: /home/sybase/sol/include/syberror.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_get_cvrg_points.o: /usr/include/values.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_get_cvrg_points.o: /usr/include/stddef.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_get_cvrg_points.o: /ASF/PRECM/include/sys/odldef.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_get_cvrg_points.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_get_dtkstat_where_clause.o: dtkm.h /usr/include/string.h
dtkm_get_dtkstat_where_clause.o: /usr/include/sys/feature_tests.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_get_dtkstat_where_clause.o: /home/sybase/sol/include/sybfront.h
dtkm_get_dtkstat_where_clause.o: /home/sybase/sol/include/cstypes.h
dtkm_get_dtkstat_where_clause.o: /home/sybase/sol/include/csconfig.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_get_dtkstat_where_clause.o: /home/sybase/sol/include/sybdb.h
dtkm_get_dtkstat_where_clause.o: /usr/include/stdio.h
dtkm_get_dtkstat_where_clause.o: /home/sybase/sol/include/syblogin.h
dtkm_get_dtkstat_where_clause.o: /home/sybase/sol/include/syberror.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_get_dtkstat_where_clause.o: /usr/include/values.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_get_dtkstat_where_clause.o: /usr/include/stddef.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_get_dtkstat_where_clause.o: /ASF/PRECM/include/sys/odldef.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_get_dtkstat_where_clause.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_get_earliest_submit_time.o: dtkm.h /usr/include/string.h
dtkm_get_earliest_submit_time.o: /usr/include/sys/feature_tests.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_get_earliest_submit_time.o: /home/sybase/sol/include/sybfront.h
dtkm_get_earliest_submit_time.o: /home/sybase/sol/include/cstypes.h
dtkm_get_earliest_submit_time.o: /home/sybase/sol/include/csconfig.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_get_earliest_submit_time.o: /home/sybase/sol/include/sybdb.h
dtkm_get_earliest_submit_time.o: /usr/include/stdio.h
dtkm_get_earliest_submit_time.o: /home/sybase/sol/include/syblogin.h
dtkm_get_earliest_submit_time.o: /home/sybase/sol/include/syberror.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_get_earliest_submit_time.o: /usr/include/values.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_get_earliest_submit_time.o: /usr/include/stddef.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_get_earliest_submit_time.o: /ASF/PRECM/include/sys/odldef.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_get_earliest_submit_time.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/local/timeconv.h dtkm.h
dtkm_get_firstlast_track.o: /usr/include/string.h
dtkm_get_firstlast_track.o: /usr/include/sys/feature_tests.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_get_firstlast_track.o: /home/sybase/sol/include/sybfront.h
dtkm_get_firstlast_track.o: /home/sybase/sol/include/cstypes.h
dtkm_get_firstlast_track.o: /home/sybase/sol/include/csconfig.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_get_firstlast_track.o: /home/sybase/sol/include/sybdb.h
dtkm_get_firstlast_track.o: /usr/include/stdio.h
dtkm_get_firstlast_track.o: /home/sybase/sol/include/syblogin.h
dtkm_get_firstlast_track.o: /home/sybase/sol/include/syberror.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_get_firstlast_track.o: /usr/include/values.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_get_firstlast_track.o: /usr/include/stddef.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_get_firstlast_track.o: /ASF/PRECM/include/sys/odldef.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_get_firstlast_track.o: /home/aps/r2.1.1/include/global/GENconversions.h
dtkm_get_highest_antenna_priority.o: dtkm.h /usr/include/string.h
dtkm_get_highest_antenna_priority.o: /usr/include/sys/feature_tests.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_get_highest_antenna_priority.o: /home/sybase/sol/include/sybfront.h
dtkm_get_highest_antenna_priority.o: /home/sybase/sol/include/cstypes.h
dtkm_get_highest_antenna_priority.o: /home/sybase/sol/include/csconfig.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_get_highest_antenna_priority.o: /home/sybase/sol/include/sybdb.h
dtkm_get_highest_antenna_priority.o: /usr/include/stdio.h
dtkm_get_highest_antenna_priority.o: /home/sybase/sol/include/syblogin.h
dtkm_get_highest_antenna_priority.o: /home/sybase/sol/include/syberror.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_get_highest_antenna_priority.o: /usr/include/values.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_get_highest_antenna_priority.o: /usr/include/stddef.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_get_highest_antenna_priority.o: /ASF/PRECM/include/sys/odldef.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_get_highest_antenna_priority.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_get_quicklook_values.o: dtkm.h /usr/include/string.h
dtkm_get_quicklook_values.o: /usr/include/sys/feature_tests.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_get_quicklook_values.o: /home/sybase/sol/include/sybfront.h
dtkm_get_quicklook_values.o: /home/sybase/sol/include/cstypes.h
dtkm_get_quicklook_values.o: /home/sybase/sol/include/csconfig.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_get_quicklook_values.o: /home/sybase/sol/include/sybdb.h
dtkm_get_quicklook_values.o: /usr/include/stdio.h
dtkm_get_quicklook_values.o: /home/sybase/sol/include/syblogin.h
dtkm_get_quicklook_values.o: /home/sybase/sol/include/syberror.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_get_quicklook_values.o: /usr/include/values.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_get_quicklook_values.o: /usr/include/stddef.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_get_quicklook_values.o: /ASF/PRECM/include/sys/odldef.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_get_quicklook_values.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_get_rej_conflicts.o: dtkm.h /usr/include/string.h
dtkm_get_rej_conflicts.o: /usr/include/sys/feature_tests.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_get_rej_conflicts.o: /home/sybase/sol/include/sybfront.h
dtkm_get_rej_conflicts.o: /home/sybase/sol/include/cstypes.h
dtkm_get_rej_conflicts.o: /home/sybase/sol/include/csconfig.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_get_rej_conflicts.o: /home/sybase/sol/include/sybdb.h
dtkm_get_rej_conflicts.o: /usr/include/stdio.h
dtkm_get_rej_conflicts.o: /home/sybase/sol/include/syblogin.h
dtkm_get_rej_conflicts.o: /home/sybase/sol/include/syberror.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_get_rej_conflicts.o: /usr/include/values.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_get_rej_conflicts.o: /usr/include/stddef.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_get_rej_conflicts.o: /ASF/PRECM/include/sys/odldef.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_get_rej_conflicts.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_get_sitename.o: dtkm.h /usr/include/string.h
dtkm_get_sitename.o: /usr/include/sys/feature_tests.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_get_sitename.o: /home/sybase/sol/include/sybfront.h
dtkm_get_sitename.o: /home/sybase/sol/include/cstypes.h
dtkm_get_sitename.o: /home/sybase/sol/include/csconfig.h
dtkm_get_sitename.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_get_sitename.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_get_sitename.o: /home/sybase/sol/include/syblogin.h
dtkm_get_sitename.o: /home/sybase/sol/include/syberror.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_get_sitename.o: /usr/include/values.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_get_sitename.o: /usr/include/stddef.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_get_sitename.o: /ASF/PRECM/include/sys/odldef.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_get_sitename.o: /home/aps/r2.1.1/include/local/db_dar.h
dtkm_in_station_mask.o: dtkm.h /usr/include/string.h
dtkm_in_station_mask.o: /usr/include/sys/feature_tests.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_in_station_mask.o: /home/sybase/sol/include/sybfront.h
dtkm_in_station_mask.o: /home/sybase/sol/include/cstypes.h
dtkm_in_station_mask.o: /home/sybase/sol/include/csconfig.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_in_station_mask.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_in_station_mask.o: /home/sybase/sol/include/syblogin.h
dtkm_in_station_mask.o: /home/sybase/sol/include/syberror.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_in_station_mask.o: /usr/include/values.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_in_station_mask.o: /usr/include/stddef.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_in_station_mask.o: /ASF/PRECM/include/sys/odldef.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/local/db_maskinout.h
dtkm_in_station_mask.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_insert_dtk_record.o: dtkm.h /usr/include/string.h
dtkm_insert_dtk_record.o: /usr/include/sys/feature_tests.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_insert_dtk_record.o: /home/sybase/sol/include/sybfront.h
dtkm_insert_dtk_record.o: /home/sybase/sol/include/cstypes.h
dtkm_insert_dtk_record.o: /home/sybase/sol/include/csconfig.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_insert_dtk_record.o: /home/sybase/sol/include/sybdb.h
dtkm_insert_dtk_record.o: /usr/include/stdio.h
dtkm_insert_dtk_record.o: /home/sybase/sol/include/syblogin.h
dtkm_insert_dtk_record.o: /home/sybase/sol/include/syberror.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_insert_dtk_record.o: /usr/include/values.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_insert_dtk_record.o: /usr/include/stddef.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_insert_dtk_record.o: /ASF/PRECM/include/sys/odldef.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/global/aps_get_sequence_number.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_insert_dtk_record.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_insert_dl2obs_rec.o: dtkm.h /usr/include/string.h
dtkm_insert_dl2obs_rec.o: /usr/include/sys/feature_tests.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_insert_dl2obs_rec.o: /home/sybase/sol/include/sybfront.h
dtkm_insert_dl2obs_rec.o: /home/sybase/sol/include/cstypes.h
dtkm_insert_dl2obs_rec.o: /home/sybase/sol/include/csconfig.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_insert_dl2obs_rec.o: /home/sybase/sol/include/sybdb.h
dtkm_insert_dl2obs_rec.o: /usr/include/stdio.h
dtkm_insert_dl2obs_rec.o: /home/sybase/sol/include/syblogin.h
dtkm_insert_dl2obs_rec.o: /home/sybase/sol/include/syberror.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_insert_dl2obs_rec.o: /usr/include/values.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_insert_dl2obs_rec.o: /usr/include/stddef.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_insert_dl2obs_rec.o: /ASF/PRECM/include/sys/odldef.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_insert_dl2obs_rec.o: /home/aps/r2.1.1/include/local/db_dl2obs.h
dtkm_is_a_downlink.o: dtkm.h /usr/include/string.h
dtkm_is_a_downlink.o: /usr/include/sys/feature_tests.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_is_a_downlink.o: /home/sybase/sol/include/sybfront.h
dtkm_is_a_downlink.o: /home/sybase/sol/include/cstypes.h
dtkm_is_a_downlink.o: /home/sybase/sol/include/csconfig.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_is_a_downlink.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_is_a_downlink.o: /home/sybase/sol/include/syblogin.h
dtkm_is_a_downlink.o: /home/sybase/sol/include/syberror.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_is_a_downlink.o: /usr/include/values.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_is_a_downlink.o: /usr/include/stddef.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_is_a_downlink.o: /ASF/PRECM/include/sys/odldef.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_is_a_downlink.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_is_a_realtime_downlink.o: dtkm.h /usr/include/string.h
dtkm_is_a_realtime_downlink.o: /usr/include/sys/feature_tests.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_is_a_realtime_downlink.o: /home/sybase/sol/include/sybfront.h
dtkm_is_a_realtime_downlink.o: /home/sybase/sol/include/cstypes.h
dtkm_is_a_realtime_downlink.o: /home/sybase/sol/include/csconfig.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_is_a_realtime_downlink.o: /home/sybase/sol/include/sybdb.h
dtkm_is_a_realtime_downlink.o: /usr/include/stdio.h
dtkm_is_a_realtime_downlink.o: /home/sybase/sol/include/syblogin.h
dtkm_is_a_realtime_downlink.o: /home/sybase/sol/include/syberror.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_is_a_realtime_downlink.o: /usr/include/values.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_is_a_realtime_downlink.o: /usr/include/stddef.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_is_a_realtime_downlink.o: /ASF/PRECM/include/sys/odldef.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_is_a_realtime_downlink.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_is_a_realtime_observation.o: dtkm.h /usr/include/string.h
dtkm_is_a_realtime_observation.o: /usr/include/sys/feature_tests.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_is_a_realtime_observation.o: /home/sybase/sol/include/sybfront.h
dtkm_is_a_realtime_observation.o: /home/sybase/sol/include/cstypes.h
dtkm_is_a_realtime_observation.o: /home/sybase/sol/include/csconfig.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_is_a_realtime_observation.o: /home/sybase/sol/include/sybdb.h
dtkm_is_a_realtime_observation.o: /usr/include/stdio.h
dtkm_is_a_realtime_observation.o: /home/sybase/sol/include/syblogin.h
dtkm_is_a_realtime_observation.o: /home/sybase/sol/include/syberror.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_is_a_realtime_observation.o: /usr/include/values.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_is_a_realtime_observation.o: /usr/include/stddef.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_is_a_realtime_observation.o: /ASF/PRECM/include/sys/odldef.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_is_a_realtime_observation.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_is_a_recording.o: dtkm.h /usr/include/string.h
dtkm_is_a_recording.o: /usr/include/sys/feature_tests.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_is_a_recording.o: /home/sybase/sol/include/sybfront.h
dtkm_is_a_recording.o: /home/sybase/sol/include/cstypes.h
dtkm_is_a_recording.o: /home/sybase/sol/include/csconfig.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_is_a_recording.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_is_a_recording.o: /home/sybase/sol/include/syblogin.h
dtkm_is_a_recording.o: /home/sybase/sol/include/syberror.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_is_a_recording.o: /usr/include/values.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_is_a_recording.o: /usr/include/stddef.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_is_a_recording.o: /ASF/PRECM/include/sys/odldef.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_is_a_recording.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_is_a_tape_dump.o: dtkm.h /usr/include/string.h
dtkm_is_a_tape_dump.o: /usr/include/sys/feature_tests.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_is_a_tape_dump.o: /home/sybase/sol/include/sybfront.h
dtkm_is_a_tape_dump.o: /home/sybase/sol/include/cstypes.h
dtkm_is_a_tape_dump.o: /home/sybase/sol/include/csconfig.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_is_a_tape_dump.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_is_a_tape_dump.o: /home/sybase/sol/include/syblogin.h
dtkm_is_a_tape_dump.o: /home/sybase/sol/include/syberror.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_is_a_tape_dump.o: /usr/include/values.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_is_a_tape_dump.o: /usr/include/stddef.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_is_a_tape_dump.o: /ASF/PRECM/include/sys/odldef.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_is_a_tape_dump.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_is_an_observation.o: dtkm.h /usr/include/string.h
dtkm_is_an_observation.o: /usr/include/sys/feature_tests.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_is_an_observation.o: /home/sybase/sol/include/sybfront.h
dtkm_is_an_observation.o: /home/sybase/sol/include/cstypes.h
dtkm_is_an_observation.o: /home/sybase/sol/include/csconfig.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_is_an_observation.o: /home/sybase/sol/include/sybdb.h
dtkm_is_an_observation.o: /usr/include/stdio.h
dtkm_is_an_observation.o: /home/sybase/sol/include/syblogin.h
dtkm_is_an_observation.o: /home/sybase/sol/include/syberror.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_is_an_observation.o: /usr/include/values.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_is_an_observation.o: /usr/include/stddef.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_is_an_observation.o: /ASF/PRECM/include/sys/odldef.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_is_an_observation.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_is_in_antarctic_mode.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_is_in_antarctic_mode.o: /home/sybase/sol/include/sybfront.h
dtkm_is_in_antarctic_mode.o: /home/sybase/sol/include/cstypes.h
dtkm_is_in_antarctic_mode.o: /home/sybase/sol/include/csconfig.h
dtkm_is_in_antarctic_mode.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_is_in_antarctic_mode.o: /home/sybase/sol/include/sybdb.h
dtkm_is_in_antarctic_mode.o: /usr/include/stdio.h
dtkm_is_in_antarctic_mode.o: /usr/include/sys/feature_tests.h
dtkm_is_in_antarctic_mode.o: /home/sybase/sol/include/syblogin.h
dtkm_is_in_antarctic_mode.o: /home/sybase/sol/include/syberror.h
dtkm_is_in_antarctic_mode.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_is_in_antarctic_mode.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_is_in_antarctic_mode.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_is_in_antarctic_mode.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_is_in_antarctic_mode.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_is_in_antarctic_mode.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_is_in_antarctic_mode.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_is_in_antarctic_mode.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_is_in_antarctic_mode.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_obs2dl.o: dtkm.h /usr/include/string.h /usr/include/sys/feature_tests.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_obs2dl.o: /home/sybase/sol/include/sybfront.h
dtkm_obs2dl.o: /home/sybase/sol/include/cstypes.h
dtkm_obs2dl.o: /home/sybase/sol/include/csconfig.h
dtkm_obs2dl.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_obs2dl.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_obs2dl.o: /home/sybase/sol/include/syblogin.h
dtkm_obs2dl.o: /home/sybase/sol/include/syberror.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_obs2dl.o: /usr/include/values.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_obs2dl.o: /usr/include/stddef.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_obs2dl.o: /ASF/PRECM/include/sys/odldef.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_obs2dl.o: /home/aps/r2.1.1/include/local/db_dl2obs.h
dtkm_print_list.o: /usr/include/string.h /usr/include/sys/feature_tests.h
dtkm_print_list.o: /usr/include/stdio.h dtkm.h
dtkm_print_list.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_print_list.o: /home/sybase/sol/include/sybfront.h
dtkm_print_list.o: /home/sybase/sol/include/cstypes.h
dtkm_print_list.o: /home/sybase/sol/include/csconfig.h
dtkm_print_list.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_print_list.o: /home/sybase/sol/include/sybdb.h
dtkm_print_list.o: /home/sybase/sol/include/syblogin.h
dtkm_print_list.o: /home/sybase/sol/include/syberror.h
dtkm_print_list.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_print_list.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_print_list.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_print_list.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_print_list.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_print_list.o: /usr/include/values.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_print_list.o: /usr/include/stddef.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_print_list.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_print_list.o: /ASF/PRECM/include/sys/odldef.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/db_antenna_down_times.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/check_lat_lon.h
dtkm_print_list.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_process_antenna_down_time.o: dtkm.h /usr/include/string.h
dtkm_process_antenna_down_time.o: /usr/include/sys/feature_tests.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_process_antenna_down_time.o: /home/sybase/sol/include/sybfront.h
dtkm_process_antenna_down_time.o: /home/sybase/sol/include/cstypes.h
dtkm_process_antenna_down_time.o: /home/sybase/sol/include/csconfig.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_process_antenna_down_time.o: /home/sybase/sol/include/sybdb.h
dtkm_process_antenna_down_time.o: /usr/include/stdio.h
dtkm_process_antenna_down_time.o: /home/sybase/sol/include/syblogin.h
dtkm_process_antenna_down_time.o: /home/sybase/sol/include/syberror.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_process_antenna_down_time.o: /usr/include/values.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_process_antenna_down_time.o: /usr/include/stddef.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_process_antenna_down_time.o: /ASF/PRECM/include/sys/odldef.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/local/db_antenna_down_times.h
dtkm_process_antenna_down_time.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_process_dtk.o: dtkm.h /usr/include/string.h
dtkm_process_dtk.o: /usr/include/sys/feature_tests.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_process_dtk.o: /home/sybase/sol/include/sybfront.h
dtkm_process_dtk.o: /home/sybase/sol/include/cstypes.h
dtkm_process_dtk.o: /home/sybase/sol/include/csconfig.h
dtkm_process_dtk.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_process_dtk.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_process_dtk.o: /home/sybase/sol/include/syblogin.h
dtkm_process_dtk.o: /home/sybase/sol/include/syberror.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_process_dtk.o: /usr/include/values.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_process_dtk.o: /usr/include/stddef.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_process_dtk.o: /ASF/PRECM/include/sys/odldef.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_process_dtk.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_process_dtk_proposal.o: dtkm.h /usr/include/string.h
dtkm_process_dtk_proposal.o: /usr/include/sys/feature_tests.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_process_dtk_proposal.o: /home/sybase/sol/include/sybfront.h
dtkm_process_dtk_proposal.o: /home/sybase/sol/include/cstypes.h
dtkm_process_dtk_proposal.o: /home/sybase/sol/include/csconfig.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_process_dtk_proposal.o: /home/sybase/sol/include/sybdb.h
dtkm_process_dtk_proposal.o: /usr/include/stdio.h
dtkm_process_dtk_proposal.o: /home/sybase/sol/include/syblogin.h
dtkm_process_dtk_proposal.o: /home/sybase/sol/include/syberror.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_process_dtk_proposal.o: /usr/include/values.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_process_dtk_proposal.o: /usr/include/stddef.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_process_dtk_proposal.o: /ASF/PRECM/include/sys/odldef.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_process_dtk_proposal.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_process_dtk_proposal_list.o: dtkm.h /usr/include/string.h
dtkm_process_dtk_proposal_list.o: /usr/include/sys/feature_tests.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_process_dtk_proposal_list.o: /home/sybase/sol/include/sybfront.h
dtkm_process_dtk_proposal_list.o: /home/sybase/sol/include/cstypes.h
dtkm_process_dtk_proposal_list.o: /home/sybase/sol/include/csconfig.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_process_dtk_proposal_list.o: /home/sybase/sol/include/sybdb.h
dtkm_process_dtk_proposal_list.o: /usr/include/stdio.h
dtkm_process_dtk_proposal_list.o: /home/sybase/sol/include/syblogin.h
dtkm_process_dtk_proposal_list.o: /home/sybase/sol/include/syberror.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_process_dtk_proposal_list.o: /usr/include/values.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_process_dtk_proposal_list.o: /usr/include/stddef.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_process_dtk_proposal_list.o: /ASF/PRECM/include/sys/odldef.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_process_dtk_proposal_list.o: /home/aps/r2.1.1/include/local/db_sat_inclusion_period.h
dtkm_reduce_accepted_dtks.o: dtkm.h /usr/include/string.h
dtkm_reduce_accepted_dtks.o: /usr/include/sys/feature_tests.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_reduce_accepted_dtks.o: /home/sybase/sol/include/sybfront.h
dtkm_reduce_accepted_dtks.o: /home/sybase/sol/include/cstypes.h
dtkm_reduce_accepted_dtks.o: /home/sybase/sol/include/csconfig.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_reduce_accepted_dtks.o: /home/sybase/sol/include/sybdb.h
dtkm_reduce_accepted_dtks.o: /usr/include/stdio.h
dtkm_reduce_accepted_dtks.o: /home/sybase/sol/include/syblogin.h
dtkm_reduce_accepted_dtks.o: /home/sybase/sol/include/syberror.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_reduce_accepted_dtks.o: /usr/include/values.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_reduce_accepted_dtks.o: /usr/include/stddef.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_reduce_accepted_dtks.o: /ASF/PRECM/include/sys/odldef.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_reduce_accepted_dtks.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_remove_dtks_from_list.o: dtkm.h /usr/include/string.h
dtkm_remove_dtks_from_list.o: /usr/include/sys/feature_tests.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_remove_dtks_from_list.o: /home/sybase/sol/include/sybfront.h
dtkm_remove_dtks_from_list.o: /home/sybase/sol/include/cstypes.h
dtkm_remove_dtks_from_list.o: /home/sybase/sol/include/csconfig.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_remove_dtks_from_list.o: /home/sybase/sol/include/sybdb.h
dtkm_remove_dtks_from_list.o: /usr/include/stdio.h
dtkm_remove_dtks_from_list.o: /home/sybase/sol/include/syblogin.h
dtkm_remove_dtks_from_list.o: /home/sybase/sol/include/syberror.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_remove_dtks_from_list.o: /usr/include/values.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_remove_dtks_from_list.o: /usr/include/stddef.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_remove_dtks_from_list.o: /ASF/PRECM/include/sys/odldef.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_remove_dtks_from_list.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_remove_observations.o: dtkm.h /usr/include/string.h
dtkm_remove_observations.o: /usr/include/sys/feature_tests.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_remove_observations.o: /home/sybase/sol/include/sybfront.h
dtkm_remove_observations.o: /home/sybase/sol/include/cstypes.h
dtkm_remove_observations.o: /home/sybase/sol/include/csconfig.h
dtkm_remove_observations.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_remove_observations.o: /home/sybase/sol/include/sybdb.h
dtkm_remove_observations.o: /usr/include/stdio.h
dtkm_remove_observations.o: /home/sybase/sol/include/syblogin.h
dtkm_remove_observations.o: /home/sybase/sol/include/syberror.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_remove_observations.o: /usr/include/values.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_remove_observations.o: /usr/include/stddef.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_remove_observations.o: /ASF/PRECM/include/sys/odldef.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_remove_observations.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_r1_update_sensor.o: dtkm.h /usr/include/string.h
dtkm_r1_update_sensor.o: /usr/include/sys/feature_tests.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_r1_update_sensor.o: /home/sybase/sol/include/sybfront.h
dtkm_r1_update_sensor.o: /home/sybase/sol/include/cstypes.h
dtkm_r1_update_sensor.o: /home/sybase/sol/include/csconfig.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_r1_update_sensor.o: /home/sybase/sol/include/sybdb.h
dtkm_r1_update_sensor.o: /usr/include/stdio.h
dtkm_r1_update_sensor.o: /home/sybase/sol/include/syblogin.h
dtkm_r1_update_sensor.o: /home/sybase/sol/include/syberror.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_r1_update_sensor.o: /usr/include/values.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_r1_update_sensor.o: /usr/include/stddef.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_r1_update_sensor.o: /ASF/PRECM/include/sys/odldef.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_r1_update_sensor.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_set_dtkstat_from_dl.o: dtkm.h /usr/include/string.h
dtkm_set_dtkstat_from_dl.o: /usr/include/sys/feature_tests.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_set_dtkstat_from_dl.o: /home/sybase/sol/include/sybfront.h
dtkm_set_dtkstat_from_dl.o: /home/sybase/sol/include/cstypes.h
dtkm_set_dtkstat_from_dl.o: /home/sybase/sol/include/csconfig.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_set_dtkstat_from_dl.o: /home/sybase/sol/include/sybdb.h
dtkm_set_dtkstat_from_dl.o: /usr/include/stdio.h
dtkm_set_dtkstat_from_dl.o: /home/sybase/sol/include/syblogin.h
dtkm_set_dtkstat_from_dl.o: /home/sybase/sol/include/syberror.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_set_dtkstat_from_dl.o: /usr/include/values.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_set_dtkstat_from_dl.o: /usr/include/stddef.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_set_dtkstat_from_dl.o: /ASF/PRECM/include/sys/odldef.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_set_dtkstat_from_dl.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_sat_has_recorder.o: dtkm.h /usr/include/string.h
dtkm_sat_has_recorder.o: /usr/include/sys/feature_tests.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_sat_has_recorder.o: /home/sybase/sol/include/sybfront.h
dtkm_sat_has_recorder.o: /home/sybase/sol/include/cstypes.h
dtkm_sat_has_recorder.o: /home/sybase/sol/include/csconfig.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_sat_has_recorder.o: /home/sybase/sol/include/sybdb.h
dtkm_sat_has_recorder.o: /usr/include/stdio.h
dtkm_sat_has_recorder.o: /home/sybase/sol/include/syblogin.h
dtkm_sat_has_recorder.o: /home/sybase/sol/include/syberror.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_sat_has_recorder.o: /usr/include/values.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_sat_has_recorder.o: /usr/include/stddef.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_sat_has_recorder.o: /ASF/PRECM/include/sys/odldef.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_sat_has_recorder.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_set_fa_schedule_link_from_list.o: dtkm.h /usr/include/string.h
dtkm_set_fa_schedule_link_from_list.o: /usr/include/sys/feature_tests.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_set_fa_schedule_link_from_list.o: /home/sybase/sol/include/sybfront.h
dtkm_set_fa_schedule_link_from_list.o: /home/sybase/sol/include/cstypes.h
dtkm_set_fa_schedule_link_from_list.o: /home/sybase/sol/include/csconfig.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_set_fa_schedule_link_from_list.o: /home/sybase/sol/include/sybdb.h
dtkm_set_fa_schedule_link_from_list.o: /usr/include/stdio.h
dtkm_set_fa_schedule_link_from_list.o: /home/sybase/sol/include/syblogin.h
dtkm_set_fa_schedule_link_from_list.o: /home/sybase/sol/include/syberror.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_set_fa_schedule_link_from_list.o: /usr/include/values.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_set_fa_schedule_link_from_list.o: /usr/include/stddef.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_set_fa_schedule_link_from_list.o: /ASF/PRECM/include/sys/odldef.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_set_fa_schedule_link_from_list.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_sat_sensor_is_low_bit_rate.o: dtkm.h /usr/include/string.h
dtkm_sat_sensor_is_low_bit_rate.o: /usr/include/sys/feature_tests.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/sybase/sol/include/sybfront.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/sybase/sol/include/cstypes.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/sybase/sol/include/csconfig.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/sybase/sol/include/sybdb.h
dtkm_sat_sensor_is_low_bit_rate.o: /usr/include/stdio.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/sybase/sol/include/syblogin.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/sybase/sol/include/syberror.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_sat_sensor_is_low_bit_rate.o: /usr/include/values.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_sat_sensor_is_low_bit_rate.o: /usr/include/stddef.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_sat_sensor_is_low_bit_rate.o: /ASF/PRECM/include/sys/odldef.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_sat_sensor_is_low_bit_rate.o: /home/aps/r2.1.1/include/local/db_satsensor.h
dtkm_set_proposed_dtkstat.o: dtkm.h /usr/include/string.h
dtkm_set_proposed_dtkstat.o: /usr/include/sys/feature_tests.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_set_proposed_dtkstat.o: /home/sybase/sol/include/sybfront.h
dtkm_set_proposed_dtkstat.o: /home/sybase/sol/include/cstypes.h
dtkm_set_proposed_dtkstat.o: /home/sybase/sol/include/csconfig.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_set_proposed_dtkstat.o: /home/sybase/sol/include/sybdb.h
dtkm_set_proposed_dtkstat.o: /usr/include/stdio.h
dtkm_set_proposed_dtkstat.o: /home/sybase/sol/include/syblogin.h
dtkm_set_proposed_dtkstat.o: /home/sybase/sol/include/syberror.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_set_proposed_dtkstat.o: /usr/include/values.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_set_proposed_dtkstat.o: /usr/include/stddef.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_set_proposed_dtkstat.o: /ASF/PRECM/include/sys/odldef.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_set_proposed_dtkstat.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_time_bracket_dtk_list.o: dtkm.h /usr/include/string.h
dtkm_time_bracket_dtk_list.o: /usr/include/sys/feature_tests.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_time_bracket_dtk_list.o: /home/sybase/sol/include/sybfront.h
dtkm_time_bracket_dtk_list.o: /home/sybase/sol/include/cstypes.h
dtkm_time_bracket_dtk_list.o: /home/sybase/sol/include/csconfig.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_time_bracket_dtk_list.o: /home/sybase/sol/include/sybdb.h
dtkm_time_bracket_dtk_list.o: /usr/include/stdio.h
dtkm_time_bracket_dtk_list.o: /home/sybase/sol/include/syblogin.h
dtkm_time_bracket_dtk_list.o: /home/sybase/sol/include/syberror.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_time_bracket_dtk_list.o: /usr/include/values.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_time_bracket_dtk_list.o: /usr/include/stddef.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_time_bracket_dtk_list.o: /ASF/PRECM/include/sys/odldef.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_time_bracket_dtk_list.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_update_best_antenna.o: dtkm.h /usr/include/string.h
dtkm_update_best_antenna.o: /usr/include/sys/feature_tests.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_update_best_antenna.o: /home/sybase/sol/include/sybfront.h
dtkm_update_best_antenna.o: /home/sybase/sol/include/cstypes.h
dtkm_update_best_antenna.o: /home/sybase/sol/include/csconfig.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_update_best_antenna.o: /home/sybase/sol/include/sybdb.h
dtkm_update_best_antenna.o: /usr/include/stdio.h
dtkm_update_best_antenna.o: /home/sybase/sol/include/syblogin.h
dtkm_update_best_antenna.o: /home/sybase/sol/include/syberror.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_update_best_antenna.o: /usr/include/values.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_update_best_antenna.o: /usr/include/stddef.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_update_best_antenna.o: /ASF/PRECM/include/sys/odldef.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_update_best_antenna.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_update_dl_quicklook.o: dtkm.h /usr/include/string.h
dtkm_update_dl_quicklook.o: /usr/include/sys/feature_tests.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_update_dl_quicklook.o: /home/sybase/sol/include/sybfront.h
dtkm_update_dl_quicklook.o: /home/sybase/sol/include/cstypes.h
dtkm_update_dl_quicklook.o: /home/sybase/sol/include/csconfig.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_update_dl_quicklook.o: /home/sybase/sol/include/sybdb.h
dtkm_update_dl_quicklook.o: /usr/include/stdio.h
dtkm_update_dl_quicklook.o: /home/sybase/sol/include/syblogin.h
dtkm_update_dl_quicklook.o: /home/sybase/sol/include/syberror.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_update_dl_quicklook.o: /usr/include/values.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_update_dl_quicklook.o: /usr/include/stddef.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_update_dl_quicklook.o: /ASF/PRECM/include/sys/odldef.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_update_dl_quicklook.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_update_dl2obs_4dtk.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_update_dl2obs_4dtk.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_update_dl2obs_4dtk.o: /home/sybase/sol/include/sybfront.h
dtkm_update_dl2obs_4dtk.o: /home/sybase/sol/include/cstypes.h
dtkm_update_dl2obs_4dtk.o: /home/sybase/sol/include/csconfig.h
dtkm_update_dl2obs_4dtk.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_update_dl2obs_4dtk.o: /home/sybase/sol/include/sybdb.h
dtkm_update_dl2obs_4dtk.o: /usr/include/stdio.h
dtkm_update_dl2obs_4dtk.o: /usr/include/sys/feature_tests.h
dtkm_update_dl2obs_4dtk.o: /home/sybase/sol/include/syblogin.h
dtkm_update_dl2obs_4dtk.o: /home/sybase/sol/include/syberror.h
dtkm_update_dl2obs_4dtk.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_update_dl2obs_4dtk.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_update_dl2obs_4dtk.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_update_dl2obs_4dtk.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_update_dl2obs_4dtk.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_update_dl2obs_4dtk.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_update_dl2obs_4dtk.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_update_dl2obs_4dtk.o: /home/aps/r2.1.1/include/local/db_dl2obs.h
dtkm_update_dtk.o: dtkm.h /usr/include/string.h
dtkm_update_dtk.o: /usr/include/sys/feature_tests.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_update_dtk.o: /home/sybase/sol/include/sybfront.h
dtkm_update_dtk.o: /home/sybase/sol/include/cstypes.h
dtkm_update_dtk.o: /home/sybase/sol/include/csconfig.h
dtkm_update_dtk.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_update_dtk.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_update_dtk.o: /home/sybase/sol/include/syblogin.h
dtkm_update_dtk.o: /home/sybase/sol/include/syberror.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_update_dtk.o: /usr/include/values.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_update_dtk.o: /usr/include/stddef.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_update_dtk.o: /ASF/PRECM/include/sys/odldef.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/local/db_dl2obs.h
dtkm_update_dtk.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_update_dtk_darid.o: dtkm.h /usr/include/string.h
dtkm_update_dtk_darid.o: /usr/include/sys/feature_tests.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_update_dtk_darid.o: /home/sybase/sol/include/sybfront.h
dtkm_update_dtk_darid.o: /home/sybase/sol/include/cstypes.h
dtkm_update_dtk_darid.o: /home/sybase/sol/include/csconfig.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_update_dtk_darid.o: /home/sybase/sol/include/sybdb.h
dtkm_update_dtk_darid.o: /usr/include/stdio.h
dtkm_update_dtk_darid.o: /home/sybase/sol/include/syblogin.h
dtkm_update_dtk_darid.o: /home/sybase/sol/include/syberror.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_update_dtk_darid.o: /usr/include/values.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_update_dtk_darid.o: /usr/include/stddef.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_update_dtk_darid.o: /ASF/PRECM/include/sys/odldef.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_update_dtk_darid.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_update_dtk_record.o: dtkm.h /usr/include/string.h
dtkm_update_dtk_record.o: /usr/include/sys/feature_tests.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_update_dtk_record.o: /home/sybase/sol/include/sybfront.h
dtkm_update_dtk_record.o: /home/sybase/sol/include/cstypes.h
dtkm_update_dtk_record.o: /home/sybase/sol/include/csconfig.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_update_dtk_record.o: /home/sybase/sol/include/sybdb.h
dtkm_update_dtk_record.o: /usr/include/stdio.h
dtkm_update_dtk_record.o: /home/sybase/sol/include/syblogin.h
dtkm_update_dtk_record.o: /home/sybase/sol/include/syberror.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_update_dtk_record.o: /usr/include/values.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_update_dtk_record.o: /usr/include/stddef.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_update_dtk_record.o: /ASF/PRECM/include/sys/odldef.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/local/db_dl2obs.h
dtkm_update_dtk_record.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_update_dtk_sensor.o: dtkm.h /usr/include/string.h
dtkm_update_dtk_sensor.o: /usr/include/sys/feature_tests.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_update_dtk_sensor.o: /home/sybase/sol/include/sybfront.h
dtkm_update_dtk_sensor.o: /home/sybase/sol/include/cstypes.h
dtkm_update_dtk_sensor.o: /home/sybase/sol/include/csconfig.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_update_dtk_sensor.o: /home/sybase/sol/include/sybdb.h
dtkm_update_dtk_sensor.o: /usr/include/stdio.h
dtkm_update_dtk_sensor.o: /home/sybase/sol/include/syblogin.h
dtkm_update_dtk_sensor.o: /home/sybase/sol/include/syberror.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_update_dtk_sensor.o: /usr/include/values.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_update_dtk_sensor.o: /usr/include/stddef.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_update_dtk_sensor.o: /ASF/PRECM/include/sys/odldef.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_update_dtk_sensor.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_update_dtks_field.o: dtkm.h /usr/include/string.h
dtkm_update_dtks_field.o: /usr/include/sys/feature_tests.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_update_dtks_field.o: /home/sybase/sol/include/sybfront.h
dtkm_update_dtks_field.o: /home/sybase/sol/include/cstypes.h
dtkm_update_dtks_field.o: /home/sybase/sol/include/csconfig.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_update_dtks_field.o: /home/sybase/sol/include/sybdb.h
dtkm_update_dtks_field.o: /usr/include/stdio.h
dtkm_update_dtks_field.o: /home/sybase/sol/include/syblogin.h
dtkm_update_dtks_field.o: /home/sybase/sol/include/syberror.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_update_dtks_field.o: /usr/include/values.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_update_dtks_field.o: /usr/include/stddef.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_update_dtks_field.o: /ASF/PRECM/include/sys/odldef.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_update_dtks_field.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_update_obs_dtkstat.o: dtkm.h /usr/include/string.h
dtkm_update_obs_dtkstat.o: /usr/include/sys/feature_tests.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_update_obs_dtkstat.o: /home/sybase/sol/include/sybfront.h
dtkm_update_obs_dtkstat.o: /home/sybase/sol/include/cstypes.h
dtkm_update_obs_dtkstat.o: /home/sybase/sol/include/csconfig.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_update_obs_dtkstat.o: /home/sybase/sol/include/sybdb.h
dtkm_update_obs_dtkstat.o: /usr/include/stdio.h
dtkm_update_obs_dtkstat.o: /home/sybase/sol/include/syblogin.h
dtkm_update_obs_dtkstat.o: /home/sybase/sol/include/syberror.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_update_obs_dtkstat.o: /usr/include/values.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_update_obs_dtkstat.o: /usr/include/stddef.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_update_obs_dtkstat.o: /ASF/PRECM/include/sys/odldef.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_update_obs_dtkstat.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_update_obs_times_from_rdl.o: dtkm.h /usr/include/string.h
dtkm_update_obs_times_from_rdl.o: /usr/include/sys/feature_tests.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_update_obs_times_from_rdl.o: /home/sybase/sol/include/sybfront.h
dtkm_update_obs_times_from_rdl.o: /home/sybase/sol/include/cstypes.h
dtkm_update_obs_times_from_rdl.o: /home/sybase/sol/include/csconfig.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_update_obs_times_from_rdl.o: /home/sybase/sol/include/sybdb.h
dtkm_update_obs_times_from_rdl.o: /usr/include/stdio.h
dtkm_update_obs_times_from_rdl.o: /home/sybase/sol/include/syblogin.h
dtkm_update_obs_times_from_rdl.o: /home/sybase/sol/include/syberror.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_update_obs_times_from_rdl.o: /usr/include/values.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_update_obs_times_from_rdl.o: /usr/include/stddef.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_update_obs_times_from_rdl.o: /ASF/PRECM/include/sys/odldef.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_update_obs_times_from_rdl.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_2dtks_concur.o: dtkm.h /usr/include/string.h
dtkm_2dtks_concur.o: /usr/include/sys/feature_tests.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_2dtks_concur.o: /home/sybase/sol/include/sybfront.h
dtkm_2dtks_concur.o: /home/sybase/sol/include/cstypes.h
dtkm_2dtks_concur.o: /home/sybase/sol/include/csconfig.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_2dtks_concur.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkm_2dtks_concur.o: /home/sybase/sol/include/syblogin.h
dtkm_2dtks_concur.o: /home/sybase/sol/include/syberror.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_2dtks_concur.o: /usr/include/values.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/local/ODLconversions.h
dtkm_2dtks_concur.o: /usr/include/stddef.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/local/ODL_defs.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/global/fa_defs.h
dtkm_2dtks_concur.o: /ASF/PRECM/include/sys/odldef.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/local/phase_utilities.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/local/db_phase.h
dtkm_2dtks_concur.o: /home/aps/r2.1.1/include/local/db_dtk.h
