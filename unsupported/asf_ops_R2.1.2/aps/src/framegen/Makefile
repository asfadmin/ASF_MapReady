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
#	"@(#) /home/aps/r2.1.2/src/framegen/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/framegen
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL = \
        $(ISYS) \
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

LALL =      $(LAPS) \
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
	${FRAMEGEN_LIB}   \
    $(SYS_LIB)       \
    $(STATIC)        \
    $(FAIF_UTIL_LIB)  \
    $(COMMON_ODL_LIB) \
	$(IMS_LIB) \
    $(IMSODL_LIB)     \
    $(COMMON_ODL_LIB) \
	$(ENCRYPT_LIB)


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

EXE          =  aps_framegen
GENSTTM_EXE  =  gensttm

LIB          =  

GENSTTM_SRC  =  gensttm.c

SRC          =  main.c  \
				aps_framegen.c                  \
				aps_framegen_error_message.c    \
				call_framegen.c                 \
				clean_up_framegen_calls.c       \
				dtk_was_reported.c              \
				report_dtk.c                    \
				report_expired_dtks.c


HEADER_FILES =  aps_framegen.h aps_framegen_msg.h

F77_SRC      =  

F77_INC      =  

OBJS         = $(SRC:%.c=%.o) $(F77_SRC:%.for=%.o)

GENSTTM_OBJS = $(GENSTTM_SRC:%.c=%.o) 

TARGETS = $(EXE) $(LIB)

#TARGETS = $(GENSTTM_EXE) $(EXE) $(LIB)
#Per Quentin's request, we do not automatically make the gensttm tool.
#This tools is used by a developer to generate orbit0 files.
###############################################################################
#
# Make the Application
#    Add all targets to be made to the next line
#    If more exes or libs are needed make a copy of the set of 
#    needed commands
#
###############################################################################

all appl:   $(TARGETS)

$(GENSTTM_EXE): $(GENSTTM_OBJS)
	@echo
	@echo "Linking    $@
	@$(CC) -o $@ $(GENSTTM_OBJS) $(LALL) $(RALL) $(LIBS)
	@echo "Done"
 
$(EXE): $(OBJS)
	@echo
	@echo "Linking    $@
	@$(CC) -o $@ $(OBJS) $(LALL) $(RALL) $(LIBS)
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

main.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
main.o: /home/aps/r2.1.1/include/local/mu_utilities.h
main.o: /home/aps/r2.1.1/include/global/db_sybint.h
main.o: /home/sybase/sol/include/sybfront.h
main.o: /home/sybase/sol/include/cstypes.h
main.o: /home/sybase/sol/include/csconfig.h
main.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
main.o: /home/sybase/sol/include/sybdb.h /home/sybase/sol/include/syblogin.h
main.o: /home/sybase/sol/include/syberror.h
main.o: /home/aps/r2.1.1/include/global/dapps_list.h
main.o: /home/aps/r2.1.1/include/global/llist_err.h
main.o: /home/aps/r2.1.1/include/global/llist.h
main.o: /home/aps/r2.1.1/include/global/ord_llist.h
main.o: /home/aps/r2.1.1/include/local/aps_encrypt.h aps_framegen.h
main.o: /usr/include/stdlib.h /usr/include/errno.h /usr/include/sys/errno.h
main.o: /usr/include/ctype.h /usr/include/string.h /usr/include/sys/utsname.h
main.o: /usr/include/syslog.h /usr/include/sys/syslog.h /usr/include/unistd.h
main.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
main.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
main.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
main.o: /home/aps/r2.1.1/include/local/aps_defs.h
main.o: /home/aps/r2.1.1/include/local/aps_db_table.h
main.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
main.o: /home/aps/r2.1.1/include/local/db_dtk.h
main.o: /home/aps/r2.1.1/include/local/db_framegen_calls.h
main.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
main.o: /home/aps/r2.1.1/include/local/timeconv.h aps_framegen_msg.h
main.o: /ASF/PRECM/include/sys/framegen.h /usr/include/time.h
main.o: /home/aps/r2.1.1/include/local/apspath.h
main.o: /home/aps/r2.1.1/include/local/open_log_file.h
main.o: /home/aps/r2.1.1/include/global/dapps_defs.h
aps_framegen.o: /home/aps/r2.1.1/include/global/db_sybint.h
aps_framegen.o: /home/sybase/sol/include/sybfront.h
aps_framegen.o: /home/sybase/sol/include/cstypes.h
aps_framegen.o: /home/sybase/sol/include/csconfig.h
aps_framegen.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
aps_framegen.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
aps_framegen.o: /usr/include/sys/feature_tests.h
aps_framegen.o: /home/sybase/sol/include/syblogin.h
aps_framegen.o: /home/sybase/sol/include/syberror.h
aps_framegen.o: /home/aps/r2.1.1/include/global/dapps_list.h
aps_framegen.o: /home/aps/r2.1.1/include/global/llist_err.h
aps_framegen.o: /home/aps/r2.1.1/include/global/llist.h
aps_framegen.o: /home/aps/r2.1.1/include/global/ord_llist.h
aps_framegen.o: /home/aps/r2.1.1/include/local/aps_db_table.h
aps_framegen.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
aps_framegen.o: /home/aps/r2.1.1/include/local/aps_defs.h
aps_framegen.o: /home/aps/r2.1.1/include/local/db_dtk.h
aps_framegen.o: /home/aps/r2.1.1/include/local/DARconversions.h
aps_framegen.o: /home/aps/r2.1.1/include/global/fa_defs.h
aps_framegen.o: /home/aps/r2.1.1/include/local/IMSconversions.h
aps_framegen.o: /home/aps/r2.1.1/include/local/ODLconversions.h
aps_framegen.o: /usr/include/stddef.h
aps_framegen.o: /home/aps/r2.1.1/include/local/ODL_defs.h
aps_framegen.o: /ASF/PRECM/include/sys/odldef.h
aps_framegen.o: /home/aps/r2.1.1/include/local/phase_utilities.h
aps_framegen.o: /home/aps/r2.1.1/include/local/db_phase.h
aps_framegen.o: /home/aps/r2.1.1/include/global/dapps_defs.h
aps_framegen.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
aps_framegen.o: aps_framegen.h /usr/include/stdlib.h /usr/include/errno.h
aps_framegen.o: /usr/include/sys/errno.h /usr/include/ctype.h
aps_framegen.o: /usr/include/string.h /usr/include/sys/utsname.h
aps_framegen.o: /usr/include/syslog.h /usr/include/sys/syslog.h
aps_framegen.o: /usr/include/unistd.h /usr/include/sys/types.h
aps_framegen.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
aps_framegen.o: /usr/include/sys/unistd.h
aps_framegen.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
aps_framegen.o: /home/aps/r2.1.1/include/local/db_framegen_calls.h
aps_framegen.o: /home/aps/r2.1.1/include/local/timeconv.h aps_framegen_msg.h
aps_framegen.o: /ASF/PRECM/include/sys/framegen.h /usr/include/time.h
call_framegen.o: /home/aps/r2.1.1/include/local/DARconversions.h
call_framegen.o: /home/aps/r2.1.1/include/global/fa_defs.h
call_framegen.o: /home/aps/r2.1.1/include/global/dapps_list.h
call_framegen.o: /home/aps/r2.1.1/include/global/llist_err.h
call_framegen.o: /home/aps/r2.1.1/include/global/llist.h
call_framegen.o: /home/aps/r2.1.1/include/global/ord_llist.h
call_framegen.o: /home/aps/r2.1.1/include/local/IMSconversions.h
call_framegen.o: /home/aps/r2.1.1/include/local/ODLconversions.h
call_framegen.o: /usr/include/stddef.h
call_framegen.o: /home/aps/r2.1.1/include/local/ODL_defs.h
call_framegen.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
call_framegen.o: /home/aps/r2.1.1/include/global/db_sybint.h
call_framegen.o: /home/sybase/sol/include/sybfront.h
call_framegen.o: /home/sybase/sol/include/cstypes.h
call_framegen.o: /home/sybase/sol/include/csconfig.h
call_framegen.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
call_framegen.o: /home/sybase/sol/include/sybdb.h
call_framegen.o: /home/sybase/sol/include/syblogin.h
call_framegen.o: /home/sybase/sol/include/syberror.h
call_framegen.o: /ASF/PRECM/include/sys/odldef.h
call_framegen.o: /home/aps/r2.1.1/include/local/phase_utilities.h
call_framegen.o: /home/aps/r2.1.1/include/local/db_phase.h
call_framegen.o: /home/aps/r2.1.1/include/global/dapps_defs.h
call_framegen.o: /home/aps/r2.1.1/include/local/timeconv.h aps_framegen.h
call_framegen.o: /usr/include/stdlib.h /usr/include/errno.h
call_framegen.o: /usr/include/sys/errno.h /usr/include/ctype.h
call_framegen.o: /usr/include/string.h /usr/include/sys/utsname.h
call_framegen.o: /usr/include/syslog.h /usr/include/sys/syslog.h
call_framegen.o: /usr/include/unistd.h /usr/include/sys/types.h
call_framegen.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
call_framegen.o: /usr/include/sys/unistd.h
call_framegen.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
call_framegen.o: /home/aps/r2.1.1/include/local/aps_defs.h
call_framegen.o: /home/aps/r2.1.1/include/local/aps_db_table.h
call_framegen.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
call_framegen.o: /home/aps/r2.1.1/include/local/db_dtk.h
call_framegen.o: /home/aps/r2.1.1/include/local/db_framegen_calls.h
call_framegen.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
call_framegen.o: aps_framegen_msg.h /ASF/PRECM/include/sys/framegen.h
call_framegen.o: /usr/include/time.h /ASF/PRECM/include/imsdads/ims_const.h
call_framegen.o: /ASF/PRECM/include/imsdads/ims_msg.h
call_framegen.o: /ASF/PRECM/include/imsdads/ims_query.h
call_framegen.o: /ASF/PRECM/include/imsdads/ims_dbms.h
call_framegen.o: /home/sybase/sol/include/ctpublic.h
call_framegen.o: /home/sybase/sol/include/cspublic.h
call_framegen.o: /home/sybase/sol/include/sqlca.h
call_framegen.o: /ASF/PRECM/include/imsdads/ims_qi.h
clean_up_framegen_calls.o: aps_framegen.h /usr/include/stdio.h
clean_up_framegen_calls.o: /usr/include/sys/feature_tests.h
clean_up_framegen_calls.o: /usr/include/stdlib.h /usr/include/errno.h
clean_up_framegen_calls.o: /usr/include/sys/errno.h /usr/include/ctype.h
clean_up_framegen_calls.o: /usr/include/string.h /usr/include/sys/utsname.h
clean_up_framegen_calls.o: /usr/include/syslog.h /usr/include/sys/syslog.h
clean_up_framegen_calls.o: /usr/include/unistd.h /usr/include/sys/types.h
clean_up_framegen_calls.o: /usr/include/sys/isa_defs.h
clean_up_framegen_calls.o: /usr/include/sys/machtypes.h
clean_up_framegen_calls.o: /usr/include/sys/unistd.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/local/aps_defs.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/global/db_sybint.h
clean_up_framegen_calls.o: /home/sybase/sol/include/sybfront.h
clean_up_framegen_calls.o: /home/sybase/sol/include/cstypes.h
clean_up_framegen_calls.o: /home/sybase/sol/include/csconfig.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
clean_up_framegen_calls.o: /home/sybase/sol/include/sybdb.h
clean_up_framegen_calls.o: /home/sybase/sol/include/syblogin.h
clean_up_framegen_calls.o: /home/sybase/sol/include/syberror.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/global/dapps_list.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/global/llist_err.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/global/llist.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/global/ord_llist.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/local/aps_db_table.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/local/db_dtk.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/local/db_framegen_calls.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
clean_up_framegen_calls.o: /home/aps/r2.1.1/include/local/timeconv.h
clean_up_framegen_calls.o: aps_framegen_msg.h
clean_up_framegen_calls.o: /ASF/PRECM/include/sys/framegen.h
clean_up_framegen_calls.o: /usr/include/time.h
dtk_was_reported.o: aps_framegen.h /usr/include/stdio.h
dtk_was_reported.o: /usr/include/sys/feature_tests.h /usr/include/stdlib.h
dtk_was_reported.o: /usr/include/errno.h /usr/include/sys/errno.h
dtk_was_reported.o: /usr/include/ctype.h /usr/include/string.h
dtk_was_reported.o: /usr/include/sys/utsname.h /usr/include/syslog.h
dtk_was_reported.o: /usr/include/sys/syslog.h /usr/include/unistd.h
dtk_was_reported.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
dtk_was_reported.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
dtk_was_reported.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
dtk_was_reported.o: /home/aps/r2.1.1/include/local/aps_defs.h
dtk_was_reported.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtk_was_reported.o: /home/sybase/sol/include/sybfront.h
dtk_was_reported.o: /home/sybase/sol/include/cstypes.h
dtk_was_reported.o: /home/sybase/sol/include/csconfig.h
dtk_was_reported.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtk_was_reported.o: /home/sybase/sol/include/sybdb.h
dtk_was_reported.o: /home/sybase/sol/include/syblogin.h
dtk_was_reported.o: /home/sybase/sol/include/syberror.h
dtk_was_reported.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtk_was_reported.o: /home/aps/r2.1.1/include/global/llist_err.h
dtk_was_reported.o: /home/aps/r2.1.1/include/global/llist.h
dtk_was_reported.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtk_was_reported.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtk_was_reported.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtk_was_reported.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtk_was_reported.o: /home/aps/r2.1.1/include/local/db_framegen_calls.h
dtk_was_reported.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtk_was_reported.o: /home/aps/r2.1.1/include/local/timeconv.h
dtk_was_reported.o: aps_framegen_msg.h /ASF/PRECM/include/sys/framegen.h
dtk_was_reported.o: /usr/include/time.h
report_dtk.o: aps_framegen.h /usr/include/stdio.h
report_dtk.o: /usr/include/sys/feature_tests.h /usr/include/stdlib.h
report_dtk.o: /usr/include/errno.h /usr/include/sys/errno.h
report_dtk.o: /usr/include/ctype.h /usr/include/string.h
report_dtk.o: /usr/include/sys/utsname.h /usr/include/syslog.h
report_dtk.o: /usr/include/sys/syslog.h /usr/include/unistd.h
report_dtk.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
report_dtk.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
report_dtk.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
report_dtk.o: /home/aps/r2.1.1/include/local/aps_defs.h
report_dtk.o: /home/aps/r2.1.1/include/global/db_sybint.h
report_dtk.o: /home/sybase/sol/include/sybfront.h
report_dtk.o: /home/sybase/sol/include/cstypes.h
report_dtk.o: /home/sybase/sol/include/csconfig.h
report_dtk.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
report_dtk.o: /home/sybase/sol/include/sybdb.h
report_dtk.o: /home/sybase/sol/include/syblogin.h
report_dtk.o: /home/sybase/sol/include/syberror.h
report_dtk.o: /home/aps/r2.1.1/include/global/dapps_list.h
report_dtk.o: /home/aps/r2.1.1/include/global/llist_err.h
report_dtk.o: /home/aps/r2.1.1/include/global/llist.h
report_dtk.o: /home/aps/r2.1.1/include/global/ord_llist.h
report_dtk.o: /home/aps/r2.1.1/include/local/aps_db_table.h
report_dtk.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
report_dtk.o: /home/aps/r2.1.1/include/local/db_dtk.h
report_dtk.o: /home/aps/r2.1.1/include/local/db_framegen_calls.h
report_dtk.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
report_dtk.o: /home/aps/r2.1.1/include/local/timeconv.h aps_framegen_msg.h
report_dtk.o: /ASF/PRECM/include/sys/framegen.h /usr/include/time.h
report_expired_dtks.o: aps_framegen.h /usr/include/stdio.h
report_expired_dtks.o: /usr/include/sys/feature_tests.h /usr/include/stdlib.h
report_expired_dtks.o: /usr/include/errno.h /usr/include/sys/errno.h
report_expired_dtks.o: /usr/include/ctype.h /usr/include/string.h
report_expired_dtks.o: /usr/include/sys/utsname.h /usr/include/syslog.h
report_expired_dtks.o: /usr/include/sys/syslog.h /usr/include/unistd.h
report_expired_dtks.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
report_expired_dtks.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
report_expired_dtks.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
report_expired_dtks.o: /home/aps/r2.1.1/include/local/aps_defs.h
report_expired_dtks.o: /home/aps/r2.1.1/include/global/db_sybint.h
report_expired_dtks.o: /home/sybase/sol/include/sybfront.h
report_expired_dtks.o: /home/sybase/sol/include/cstypes.h
report_expired_dtks.o: /home/sybase/sol/include/csconfig.h
report_expired_dtks.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
report_expired_dtks.o: /home/sybase/sol/include/sybdb.h
report_expired_dtks.o: /home/sybase/sol/include/syblogin.h
report_expired_dtks.o: /home/sybase/sol/include/syberror.h
report_expired_dtks.o: /home/aps/r2.1.1/include/global/dapps_list.h
report_expired_dtks.o: /home/aps/r2.1.1/include/global/llist_err.h
report_expired_dtks.o: /home/aps/r2.1.1/include/global/llist.h
report_expired_dtks.o: /home/aps/r2.1.1/include/global/ord_llist.h
report_expired_dtks.o: /home/aps/r2.1.1/include/local/aps_db_table.h
report_expired_dtks.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
report_expired_dtks.o: /home/aps/r2.1.1/include/local/db_dtk.h
report_expired_dtks.o: /home/aps/r2.1.1/include/local/db_framegen_calls.h
report_expired_dtks.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
report_expired_dtks.o: /home/aps/r2.1.1/include/local/timeconv.h
report_expired_dtks.o: aps_framegen_msg.h /ASF/PRECM/include/sys/framegen.h
report_expired_dtks.o: /usr/include/time.h
