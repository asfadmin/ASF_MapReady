###############################################################################
#
# File:         Makefile
#
# Function:     Baseline Version of APS Makefile, Miguel's test version
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#	"%W% %E% APS/ASF"
#	"%Z% %P%"
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

 
SRC_DIR = $(APS_HOME)/src/lib_grs


###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################
IALL = \
    $(IAPS) \
    $(ISYBASE) \
    $(IPPS) \
    $(IFAIF) \
    $(IODL) \
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

LIB          =  libgrs.a

SRC          =  reqqid_coding.c    

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
all appl:	$(TARGETS)

$(EXE): $(OBJS)
	@echo
	@echo "Linking    $(EXE)"
	@$(CC) -o $(EXE) $(OBJS) $(LALL) $(LIBS)
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

ascii_file_creator.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
ascii_file_creator.o: /usr/include/stddef.h /usr/include/string.h
ascii_file_creator.o: /home/aps/r2.1.1/include/global/db_sybint.h
ascii_file_creator.o: /home/sybase/sol/include/sybfront.h
ascii_file_creator.o: /home/sybase/sol/include/cstypes.h
ascii_file_creator.o: /home/sybase/sol/include/csconfig.h
ascii_file_creator.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
ascii_file_creator.o: /home/sybase/sol/include/sybdb.h
ascii_file_creator.o: /home/sybase/sol/include/syblogin.h
ascii_file_creator.o: /home/sybase/sol/include/syberror.h
ascii_file_creator.o: /home/aps/r2.1.1/include/global/dapps_list.h
ascii_file_creator.o: /home/aps/r2.1.1/include/global/llist_err.h
ascii_file_creator.o: /home/aps/r2.1.1/include/global/llist.h
ascii_file_creator.o: /home/aps/r2.1.1/include/global/ord_llist.h
ascii_file_creator.o: /home/aps/r2.1.1/include/local/db_dtk.h
ascii_file_creator.o: /home/aps/r2.1.1/include/global/fa_defs.h
ascii_file_creator.o: /home/aps/r2.1.1/include/local/file_utilities.h
ascii_file_creator.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
ascii_file_creator.o: /ASF/PRECM/include/faif/faifdefs.h
ascii_file_creator.o: /usr/include/syslog.h /usr/include/sys/syslog.h
ascii_file_creator.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
ascii_file_creator.o: /ASF/PRECM/include/sys/odldef.h
ascii_file_creator.o: /ASF/PRECM/include/sys/odlinter.h
ascii_file_creator.o: /home/aps/r2.1.1/include/local/mu_utilities.h
ascii_file_creator.o: /home/aps/r2.1.1/include/global/GENconversions.h
ODL_file_ingestion.o: /usr/include/string.h /usr/include/sys/feature_tests.h
ODL_file_ingestion.o: /usr/include/math.h /usr/include/stdio.h
ODL_file_ingestion.o: /usr/include/stdlib.h /usr/include/sys/types.h
ODL_file_ingestion.o: /usr/include/sys/isa_defs.h
ODL_file_ingestion.o: /usr/include/sys/machtypes.h /usr/include/sys/uio.h
ODL_file_ingestion.o: /usr/include/sys/unistd.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/mu_utilities.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/global/db_sybint.h
ODL_file_ingestion.o: /home/sybase/sol/include/sybfront.h
ODL_file_ingestion.o: /home/sybase/sol/include/cstypes.h
ODL_file_ingestion.o: /home/sybase/sol/include/csconfig.h
ODL_file_ingestion.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
ODL_file_ingestion.o: /home/sybase/sol/include/sybdb.h
ODL_file_ingestion.o: /home/sybase/sol/include/syblogin.h
ODL_file_ingestion.o: /home/sybase/sol/include/syberror.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/global/dapps_list.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/global/llist_err.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/global/llist.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/global/ord_llist.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/aps_db_table.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/global/dapps_defs.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/apsfiledef.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
ODL_file_ingestion.o: /usr/include/syslog.h /usr/include/sys/syslog.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/aps_defs.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/db_dtk.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/db_antenna_down_times.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/apspath.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/phase_utilities.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/db_phase.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/file_utilities.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/global/fa_defs.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
ODL_file_ingestion.o: /ASF/PRECM/include/faif/faifdefs.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
ODL_file_ingestion.o: /ASF/PRECM/include/sys/odldef.h
ODL_file_ingestion.o: /ASF/PRECM/include/sys/odlinter.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/ODLconversions.h
ODL_file_ingestion.o: /usr/include/stddef.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/local/ODL_defs.h
ODL_file_ingestion.o: /home/aps/r2.1.1/include/global/GENconversions.h
file_ingestion.o: /usr/include/string.h /usr/include/sys/feature_tests.h
file_ingestion.o: /usr/include/math.h /usr/include/stdio.h
file_ingestion.o: /usr/include/errno.h /usr/include/sys/errno.h
file_ingestion.o: /usr/include/stdlib.h /usr/include/sys/types.h
file_ingestion.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
file_ingestion.o: /usr/include/sys/uio.h /usr/include/unistd.h
file_ingestion.o: /usr/include/sys/unistd.h
file_ingestion.o: /home/aps/r2.1.1/include/global/db_sybint.h
file_ingestion.o: /home/sybase/sol/include/sybfront.h
file_ingestion.o: /home/sybase/sol/include/cstypes.h
file_ingestion.o: /home/sybase/sol/include/csconfig.h
file_ingestion.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
file_ingestion.o: /home/sybase/sol/include/sybdb.h
file_ingestion.o: /home/sybase/sol/include/syblogin.h
file_ingestion.o: /home/sybase/sol/include/syberror.h
file_ingestion.o: /home/aps/r2.1.1/include/global/dapps_list.h
file_ingestion.o: /home/aps/r2.1.1/include/global/llist_err.h
file_ingestion.o: /home/aps/r2.1.1/include/global/llist.h
file_ingestion.o: /home/aps/r2.1.1/include/global/ord_llist.h
file_ingestion.o: /home/aps/r2.1.1/include/local/aps_db_table.h
file_ingestion.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
file_ingestion.o: /home/aps/r2.1.1/include/global/dapps_defs.h
file_ingestion.o: /home/aps/r2.1.1/include/local/apsfiledef.h
file_ingestion.o: /home/aps/r2.1.1/include/local/db_dtk.h
file_ingestion.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
file_ingestion.o: /usr/include/syslog.h /usr/include/sys/syslog.h
file_ingestion.o: /home/aps/r2.1.1/include/local/grs_utilities.h
file_ingestion.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
file_ingestion.o: /home/aps/r2.1.1/include/local/apspath.h
file_ingestion.o: /home/aps/r2.1.1/include/global/GENconversions.h
file_ingestion.o: /home/aps/r2.1.1/include/global/fa_defs.h
file_ingestion.o: /home/aps/r2.1.1/include/global/ESAconversions.h
file_ingestion.o: /home/aps/r2.1.1/include/local/NASDAconversions.h
file_ingestion.o: /usr/include/stddef.h
file_ingestion.o: /home/aps/r2.1.1/include/local/phase_utilities.h
file_ingestion.o: /home/aps/r2.1.1/include/local/db_phase.h
file_ingestion.o: /home/aps/r2.1.1/include/local/file_utilities.h
file_ingestion.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
file_ingestion.o: /ASF/PRECM/include/faif/faifdefs.h
file_ingestion.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
file_ingestion.o: /ASF/PRECM/include/sys/odldef.h
file_ingestion.o: /ASF/PRECM/include/sys/odlinter.h
file_ingestion.o: /home/aps/r2.1.1/include/local/mu_utilities.h
fa_move_file.o: /home/aps/r2.1.1/include/local/file_utilities.h
fa_move_file.o: /home/aps/r2.1.1/include/global/fa_defs.h
fa_move_file.o: /home/aps/r2.1.1/include/global/dapps_list.h
fa_move_file.o: /home/aps/r2.1.1/include/global/llist_err.h
fa_move_file.o: /home/aps/r2.1.1/include/global/llist.h
fa_move_file.o: /home/aps/r2.1.1/include/global/ord_llist.h
fa_move_file.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
fa_move_file.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
fa_move_file.o: /ASF/PRECM/include/faif/faifdefs.h /usr/include/syslog.h
fa_move_file.o: /usr/include/sys/syslog.h
fa_move_file.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
fa_move_file.o: /ASF/PRECM/include/sys/odldef.h
fa_move_file.o: /ASF/PRECM/include/sys/odlinter.h
fa_move_file.o: /home/aps/r2.1.1/include/local/mu_utilities.h
fa_move_file.o: /home/aps/r2.1.1/include/global/db_sybint.h
fa_move_file.o: /home/sybase/sol/include/sybfront.h
fa_move_file.o: /home/sybase/sol/include/cstypes.h
fa_move_file.o: /home/sybase/sol/include/csconfig.h
fa_move_file.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
fa_move_file.o: /home/sybase/sol/include/sybdb.h
fa_move_file.o: /home/sybase/sol/include/syblogin.h
fa_move_file.o: /home/sybase/sol/include/syberror.h /usr/include/unistd.h
fa_move_file.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
fa_move_file.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
fa_move_file.o: /usr/include/errno.h /usr/include/sys/errno.h
fa_move_file.o: /home/aps/r2.1.1/include/local/apspath.h
fa_move_file.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
fa_copy_file.o: /home/aps/r2.1.1/include/local/file_utilities.h
fa_copy_file.o: /home/aps/r2.1.1/include/global/fa_defs.h
fa_copy_file.o: /home/aps/r2.1.1/include/global/dapps_list.h
fa_copy_file.o: /home/aps/r2.1.1/include/global/llist_err.h
fa_copy_file.o: /home/aps/r2.1.1/include/global/llist.h
fa_copy_file.o: /home/aps/r2.1.1/include/global/ord_llist.h
fa_copy_file.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
fa_copy_file.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
fa_copy_file.o: /ASF/PRECM/include/faif/faifdefs.h /usr/include/syslog.h
fa_copy_file.o: /usr/include/sys/syslog.h
fa_copy_file.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
fa_copy_file.o: /ASF/PRECM/include/sys/odldef.h
fa_copy_file.o: /ASF/PRECM/include/sys/odlinter.h
fa_copy_file.o: /home/aps/r2.1.1/include/local/mu_utilities.h
fa_copy_file.o: /home/aps/r2.1.1/include/global/db_sybint.h
fa_copy_file.o: /home/sybase/sol/include/sybfront.h
fa_copy_file.o: /home/sybase/sol/include/cstypes.h
fa_copy_file.o: /home/sybase/sol/include/csconfig.h
fa_copy_file.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
fa_copy_file.o: /home/sybase/sol/include/sybdb.h
fa_copy_file.o: /home/sybase/sol/include/syblogin.h
fa_copy_file.o: /home/sybase/sol/include/syberror.h /usr/include/unistd.h
fa_copy_file.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
fa_copy_file.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
fa_copy_file.o: /usr/include/errno.h /usr/include/sys/errno.h
fa_copy_file.o: /home/aps/r2.1.1/include/local/apspath.h
fa_copy_file.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
reqa_grs_processor.o: /home/aps/r2.1.1/include/global/dapps_defs.h
reqa_grs_processor.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
reqa_grs_processor.o: /home/aps/r2.1.1/include/local/grs_utilities.h
reqa_grs_processor.o: /home/aps/r2.1.1/include/global/db_sybint.h
reqa_grs_processor.o: /home/sybase/sol/include/sybfront.h
reqa_grs_processor.o: /home/sybase/sol/include/cstypes.h
reqa_grs_processor.o: /home/sybase/sol/include/csconfig.h
reqa_grs_processor.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
reqa_grs_processor.o: /home/sybase/sol/include/sybdb.h
reqa_grs_processor.o: /home/sybase/sol/include/syblogin.h
reqa_grs_processor.o: /home/sybase/sol/include/syberror.h
reqa_grs_processor.o: /home/aps/r2.1.1/include/global/dapps_list.h
reqa_grs_processor.o: /home/aps/r2.1.1/include/global/llist_err.h
reqa_grs_processor.o: /home/aps/r2.1.1/include/global/llist.h
reqa_grs_processor.o: /home/aps/r2.1.1/include/global/ord_llist.h
reqa_grs_processor.o: /home/aps/r2.1.1/include/local/aps_db_table.h
reqa_grs_processor.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
reqa_grs_processor.o: /home/aps/r2.1.1/include/local/db_dar.h
