###############################################################################
#
# File:         lib_APS/Makefile 
#
# Function:     APS Makefile for libaps.a
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/lib_APS
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL = \
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

LIB          = 

GLOBAL_LIB	 =  libaps.a

SRC          =  \
		aps_access.c		\
		aps_chmod.c			\
		aps_internal_error.c \
		aps_log_msg.c       \
		aps_report_header.c \
		apspath.c			\
		asf_head.c			\
		banner_exit.c		\
		change_to_stoicdir.c \
		check_lat_lon.c     \
		cvrg_allowed.c     \
		del_blanks.c		\
		esa_head.c			\
		f77_aps_fullpath.c	\
		get_APS_upass.c     \
		get_cvg.c		   \
		get_phase.c		   \
		get_phase_r.c	   \
		get_phase_t.c	   \
		get_satsensor.c	   \
		getdup.c			\
		j1_get_phase_t.c 	\
		j1drsp2r_c.c		\
		j1rsp2rt_c.c		\
		j1rt2rsp_c.c		\
		open_log_file.c		\
		quote_doubler.c		\
		rsp_angle_check.c	\
		rsp_check.c			\
		rsp_path_check.c	\
		sscvrev2time.c

HEADER_FILES =  

F77_SRC      =  \
	aps_log_msg_fortran.for    \
	gcbomb.for	\
	j1drsp2r.for	\
	j1rsp2rt.for	\
	j1rt2rsp.for	\
	lastc.for	\
	rev2time.for	\
	revs2t.for	\
	rgetx.for	\
	tfname.for	\
	time2rev.for	\
	times2r.for	\
	writecheck.for

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

aps_access.o: /usr/include/unistd.h /usr/include/sys/feature_tests.h
aps_access.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
aps_access.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
aps_chmod.o: /usr/include/sys/types.h /usr/include/sys/feature_tests.h
aps_chmod.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
aps_chmod.o: /usr/include/sys/stat.h /usr/include/sys/time.h
aps_internal_error.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
aps_log_msg.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
aps_log_msg.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
aps_log_msg.o: /usr/include/syslog.h /usr/include/sys/syslog.h
aps_log_msg.o: /usr/include/time.h /usr/include/sys/utsname.h
aps_report_header.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
aps_report_header.o: /home/aps/r2.1.1/include/global/dapps_defs.h
aps_report_header.o: /home/aps/r2.1.1/include/global/fa_defs.h
aps_report_header.o: /home/aps/r2.1.1/include/global/dapps_list.h
aps_report_header.o: /home/aps/r2.1.1/include/global/llist_err.h
aps_report_header.o: /home/aps/r2.1.1/include/global/llist.h
aps_report_header.o: /home/aps/r2.1.1/include/global/ord_llist.h
apspath.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
apspath.o: /usr/include/stdlib.h /usr/include/string.h /usr/include/unistd.h
apspath.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
apspath.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
apspath.o: /home/aps/r2.1.1/include/global/dapps_defs.h
apspath.o: /home/aps/r2.1.1/include/local/apspath.h
asf_head.o: /home/aps/r2.1.1/include/local/mps.h /usr/include/stdio.h
asf_head.o: /usr/include/sys/feature_tests.h /usr/include/stdlib.h
asf_head.o: /usr/include/string.h /usr/include/time.h
change_to_stoicdir.o: /usr/include/unistd.h /usr/include/sys/feature_tests.h
change_to_stoicdir.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
change_to_stoicdir.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
change_to_stoicdir.o: /home/aps/r2.1.1/include/local/timeconv.h
change_to_stoicdir.o: /home/aps/r2.1.1/include/local/aps_defs.h
change_to_stoicdir.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
change_to_stoicdir.o: /usr/include/stdio.h /usr/include/syslog.h
change_to_stoicdir.o: /usr/include/sys/syslog.h
check_lat_lon.o: /home/aps/r2.1.1/include/local/check_lat_lon.h
check_lat_lon.o: /home/aps/r2.1.1/include/global/dapps_defs.h
check_lat_lon.o: /usr/include/math.h
cvrg_allowed.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
cvrg_allowed.o: /home/aps/r2.1.1/include/global/db_sybint.h
cvrg_allowed.o: /home/sybase/sol/include/sybfront.h
cvrg_allowed.o: /home/sybase/sol/include/cstypes.h
cvrg_allowed.o: /home/sybase/sol/include/csconfig.h
cvrg_allowed.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cvrg_allowed.o: /home/sybase/sol/include/sybdb.h
cvrg_allowed.o: /home/sybase/sol/include/syblogin.h
cvrg_allowed.o: /home/sybase/sol/include/syberror.h
cvrg_allowed.o: /home/aps/r2.1.1/include/global/dapps_list.h
cvrg_allowed.o: /home/aps/r2.1.1/include/global/llist_err.h
cvrg_allowed.o: /home/aps/r2.1.1/include/global/llist.h
cvrg_allowed.o: /home/aps/r2.1.1/include/global/ord_llist.h
cvrg_allowed.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cvrg_allowed.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cvrg_allowed.o: /home/aps/r2.1.1/include/local/db_satsensor.h
esa_head.o: /usr/include/sys/types.h /usr/include/sys/feature_tests.h
esa_head.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
esa_head.o: /usr/include/stdio.h /home/aps/r2.1.1/include/local/timeconv.h
f77_aps_fullpath.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
f77_aps_fullpath.o: /home/aps/r2.1.1/include/local/apspath.h
get_APS_upass.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
get_APS_upass.o: /usr/include/string.h
get_APS_upass.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
get_APS_upass.o: /usr/include/syslog.h /usr/include/sys/syslog.h
get_APS_upass.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
get_APS_upass.o: /usr/include/sys/machtypes.h /usr/include/sys/stat.h
get_APS_upass.o: /usr/include/sys/time.h /usr/include/rpc/des_crypt.h
get_APS_upass.o: /home/aps/r2.1.1/include/local/aps_encrypt.h
get_cvg.o: /home/sybase/sol/include/sybfront.h
get_cvg.o: /home/sybase/sol/include/cstypes.h
get_cvg.o: /home/sybase/sol/include/csconfig.h
get_cvg.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_cvg.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
get_cvg.o: /usr/include/sys/feature_tests.h
get_cvg.o: /home/sybase/sol/include/syblogin.h
get_cvg.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
get_cvg.o: /home/aps/r2.1.1/include/local/timeconv.h
get_cvg.o: /home/aps/r2.1.1/include/global/dapps_defs.h
get_cvg.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_cvg.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_cvg.o: /home/aps/r2.1.1/include/global/llist_err.h
get_cvg.o: /home/aps/r2.1.1/include/global/llist.h
get_cvg.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_cvg.o: /home/aps/r2.1.1/include/local/aps_db_table.h
get_cvg.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
get_cvg.o: /home/aps/r2.1.1/include/local/db_cvrg.h
get_phase.o: /home/sybase/sol/include/sybfront.h
get_phase.o: /home/sybase/sol/include/cstypes.h
get_phase.o: /home/sybase/sol/include/csconfig.h
get_phase.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_phase.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
get_phase.o: /usr/include/sys/feature_tests.h
get_phase.o: /home/sybase/sol/include/syblogin.h
get_phase.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
get_phase.o: /home/aps/r2.1.1/include/local/timeconv.h
get_phase.o: /home/aps/r2.1.1/include/local/phase_utilities.h
get_phase.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_phase.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_phase.o: /home/aps/r2.1.1/include/global/llist_err.h
get_phase.o: /home/aps/r2.1.1/include/global/llist.h
get_phase.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_phase.o: /home/aps/r2.1.1/include/local/db_phase.h
get_phase.o: /home/aps/r2.1.1/include/global/dapps_defs.h
get_phase_r.o: /home/sybase/sol/include/sybfront.h
get_phase_r.o: /home/sybase/sol/include/cstypes.h
get_phase_r.o: /home/sybase/sol/include/csconfig.h
get_phase_r.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_phase_r.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
get_phase_r.o: /usr/include/sys/feature_tests.h
get_phase_r.o: /home/sybase/sol/include/syblogin.h
get_phase_r.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
get_phase_r.o: /home/aps/r2.1.1/include/local/phase_utilities.h
get_phase_r.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_phase_r.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_phase_r.o: /home/aps/r2.1.1/include/global/llist_err.h
get_phase_r.o: /home/aps/r2.1.1/include/global/llist.h
get_phase_r.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_phase_r.o: /home/aps/r2.1.1/include/local/db_phase.h
get_phase_r.o: /home/aps/r2.1.1/include/global/dapps_defs.h
get_phase_t.o: /home/sybase/sol/include/sybfront.h
get_phase_t.o: /home/sybase/sol/include/cstypes.h
get_phase_t.o: /home/sybase/sol/include/csconfig.h
get_phase_t.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_phase_t.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
get_phase_t.o: /usr/include/sys/feature_tests.h
get_phase_t.o: /home/sybase/sol/include/syblogin.h
get_phase_t.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
get_phase_t.o: /home/aps/r2.1.1/include/local/timeconv.h
get_phase_t.o: /home/aps/r2.1.1/include/local/phase_utilities.h
get_phase_t.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_phase_t.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_phase_t.o: /home/aps/r2.1.1/include/global/llist_err.h
get_phase_t.o: /home/aps/r2.1.1/include/global/llist.h
get_phase_t.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_phase_t.o: /home/aps/r2.1.1/include/local/db_phase.h
get_phase_t.o: /home/aps/r2.1.1/include/global/dapps_defs.h
get_satsensor.o: /home/sybase/sol/include/sybfront.h
get_satsensor.o: /home/sybase/sol/include/cstypes.h
get_satsensor.o: /home/sybase/sol/include/csconfig.h
get_satsensor.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_satsensor.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
get_satsensor.o: /usr/include/sys/feature_tests.h
get_satsensor.o: /home/sybase/sol/include/syblogin.h
get_satsensor.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
get_satsensor.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_satsensor.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_satsensor.o: /home/aps/r2.1.1/include/global/llist_err.h
get_satsensor.o: /home/aps/r2.1.1/include/global/llist.h
get_satsensor.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_satsensor.o: /home/aps/r2.1.1/include/local/aps_db_table.h
get_satsensor.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
get_satsensor.o: /home/aps/r2.1.1/include/local/db_satsensor.h
getdup.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
getdup.o: /usr/include/stdio.h
j1_get_phase_t.o: /home/sybase/sol/include/sybfront.h
j1_get_phase_t.o: /home/sybase/sol/include/cstypes.h
j1_get_phase_t.o: /home/sybase/sol/include/csconfig.h
j1_get_phase_t.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
j1_get_phase_t.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
j1_get_phase_t.o: /usr/include/sys/feature_tests.h
j1_get_phase_t.o: /home/sybase/sol/include/syblogin.h
j1_get_phase_t.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
j1_get_phase_t.o: /home/aps/r2.1.1/include/local/timeconv.h
j1_get_phase_t.o: /home/aps/r2.1.1/include/local/phase_utilities.h
j1_get_phase_t.o: /home/aps/r2.1.1/include/global/db_sybint.h
j1_get_phase_t.o: /home/aps/r2.1.1/include/global/dapps_list.h
j1_get_phase_t.o: /home/aps/r2.1.1/include/global/llist_err.h
j1_get_phase_t.o: /home/aps/r2.1.1/include/global/llist.h
j1_get_phase_t.o: /home/aps/r2.1.1/include/global/ord_llist.h
j1_get_phase_t.o: /home/aps/r2.1.1/include/local/db_phase.h
j1_get_phase_t.o: /home/aps/r2.1.1/include/global/dapps_defs.h
j1rt2rsp_c.o: /usr/include/string.h /usr/include/sys/feature_tests.h
j1rt2rsp_c.o: /usr/include/stdio.h
open_log_file.o: /home/aps/r2.1.1/include/local/open_log_file.h
open_log_file.o: /home/aps/r2.1.1/include/global/dapps_defs.h
open_log_file.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
open_log_file.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
open_log_file.o: /usr/include/syslog.h /usr/include/sys/syslog.h
open_log_file.o: /home/aps/r2.1.1/include/local/apspath.h
quote_doubler.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
quote_doubler.o: /usr/include/stdio.h
rsp_check.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
sscvrev2time.o: /home/sybase/sol/include/sybfront.h
sscvrev2time.o: /home/sybase/sol/include/cstypes.h
sscvrev2time.o: /home/sybase/sol/include/csconfig.h
sscvrev2time.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
sscvrev2time.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
sscvrev2time.o: /usr/include/sys/feature_tests.h
sscvrev2time.o: /home/sybase/sol/include/syblogin.h
sscvrev2time.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
sscvrev2time.o: /home/aps/r2.1.1/include/local/timeconv.h
sscvrev2time.o: /home/aps/r2.1.1/include/local/phase_utilities.h
sscvrev2time.o: /home/aps/r2.1.1/include/global/db_sybint.h
sscvrev2time.o: /home/aps/r2.1.1/include/global/dapps_list.h
sscvrev2time.o: /home/aps/r2.1.1/include/global/llist_err.h
sscvrev2time.o: /home/aps/r2.1.1/include/global/llist.h
sscvrev2time.o: /home/aps/r2.1.1/include/global/ord_llist.h
sscvrev2time.o: /home/aps/r2.1.1/include/local/db_phase.h
sscvrev2time.o: /home/aps/r2.1.1/include/global/dapps_defs.h
sscvrev2time.o: /home/aps/r2.1.1/include/local/aps_db_table.h
sscvrev2time.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
