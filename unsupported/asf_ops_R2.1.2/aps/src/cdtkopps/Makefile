###############################################################################
#
# File:         cdtkopps/Makefile
#
# Function:     APS Makefile for aps_crt_dtk_opps
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/cdtkopps/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/cdtkopps
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL =      $(IAPS) \
			$(ISYBASE) \
			$(OSIFLAGS) 

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################

LALL =      $(LAPS) \
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

LIBS = $(EVERY_LIB)

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

EXE          =  aps_crt_dtk_opps

LIB          =  

SRC          =  cdto_getopts.c get_dar_sscv.c   get_site_sscv.c   \
				tcopy.c        tcycle.c \
				get_cvrg.c \
				get_dar.c  \
				get_site.c \
				delete_sscvrg.c  \
				insert_sscvrg.c 


HEADER_FILES =  

F77_SRC      =  sscvbatch.for sscvad.for sscvc.for      sscvcb.for  \
				sscvq.for     sscvqb.for sscvrevs2t.for \
				sscovc.for \
				sscovq.for 

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
#   Add post make definitions
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

cdto_getopts.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
cdto_getopts.o: /usr/include/stdio.h /usr/include/string.h
cdto_getopts.o: /home/aps/r2.1.1/include/local/timeconv.h
cdto_getopts.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
cdto_getopts.o: /usr/include/syslog.h /usr/include/sys/syslog.h
cdto_getopts.o: /home/aps/r2.1.1/include/local/aps_defs.h
cdto_getopts.o: /home/aps/r2.1.1/include/global/db_sybint.h
cdto_getopts.o: /home/sybase/sol/include/sybfront.h
cdto_getopts.o: /home/sybase/sol/include/cstypes.h
cdto_getopts.o: /home/sybase/sol/include/csconfig.h
cdto_getopts.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cdto_getopts.o: /home/sybase/sol/include/sybdb.h
cdto_getopts.o: /home/sybase/sol/include/syblogin.h
cdto_getopts.o: /home/sybase/sol/include/syberror.h
cdto_getopts.o: /home/aps/r2.1.1/include/global/dapps_list.h
cdto_getopts.o: /home/aps/r2.1.1/include/global/llist_err.h
cdto_getopts.o: /home/aps/r2.1.1/include/global/llist.h
cdto_getopts.o: /home/aps/r2.1.1/include/global/ord_llist.h
cdto_getopts.o: /home/aps/r2.1.1/include/local/mu_utilities.h
get_dar_sscv.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
get_site_sscv.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
tcopy.o: /home/sybase/sol/include/sybfront.h
tcopy.o: /home/sybase/sol/include/cstypes.h
tcopy.o: /home/sybase/sol/include/csconfig.h
tcopy.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
tcopy.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
tcopy.o: /usr/include/sys/feature_tests.h /home/sybase/sol/include/syblogin.h
tcopy.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
tcopy.o: /home/aps/r2.1.1/include/local/timeconv.h
tcopy.o: /home/aps/r2.1.1/include/global/db_sybint.h
tcopy.o: /home/aps/r2.1.1/include/global/dapps_list.h
tcopy.o: /home/aps/r2.1.1/include/global/llist_err.h
tcopy.o: /home/aps/r2.1.1/include/global/llist.h
tcopy.o: /home/aps/r2.1.1/include/global/ord_llist.h
tcopy.o: /home/aps/r2.1.1/include/local/aps_db_table.h
tcopy.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
tcopy.o: /home/aps/r2.1.1/include/local/db_sscvrg.h
tcycle.o: /home/sybase/sol/include/sybfront.h
tcycle.o: /home/sybase/sol/include/cstypes.h
tcycle.o: /home/sybase/sol/include/csconfig.h
tcycle.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
tcycle.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
tcycle.o: /usr/include/sys/feature_tests.h
tcycle.o: /home/sybase/sol/include/syblogin.h
tcycle.o: /home/sybase/sol/include/syberror.h
tcycle.o: /home/aps/r2.1.1/include/local/timeconv.h
tcycle.o: /home/aps/r2.1.1/include/local/phase_utilities.h
tcycle.o: /home/aps/r2.1.1/include/global/db_sybint.h
tcycle.o: /home/aps/r2.1.1/include/global/dapps_list.h
tcycle.o: /home/aps/r2.1.1/include/global/llist_err.h
tcycle.o: /home/aps/r2.1.1/include/global/llist.h
tcycle.o: /home/aps/r2.1.1/include/global/ord_llist.h
tcycle.o: /home/aps/r2.1.1/include/local/db_phase.h
tcycle.o: /home/aps/r2.1.1/include/global/dapps_defs.h
tcycle.o: /home/aps/r2.1.1/include/local/aps_db_table.h
tcycle.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
get_cvrg.o: /home/sybase/sol/include/sybfront.h
get_cvrg.o: /home/sybase/sol/include/cstypes.h
get_cvrg.o: /home/sybase/sol/include/csconfig.h
get_cvrg.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_cvrg.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
get_cvrg.o: /usr/include/sys/feature_tests.h
get_cvrg.o: /home/sybase/sol/include/syblogin.h
get_cvrg.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
get_cvrg.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_cvrg.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_cvrg.o: /home/aps/r2.1.1/include/global/llist_err.h
get_cvrg.o: /home/aps/r2.1.1/include/global/llist.h
get_cvrg.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_cvrg.o: /home/aps/r2.1.1/include/local/aps_db_table.h
get_cvrg.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
get_cvrg.o: /home/aps/r2.1.1/include/local/db_cvrg.h
get_dar.o: /home/sybase/sol/include/sybfront.h
get_dar.o: /home/sybase/sol/include/cstypes.h
get_dar.o: /home/sybase/sol/include/csconfig.h
get_dar.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_dar.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
get_dar.o: /usr/include/sys/feature_tests.h
get_dar.o: /home/sybase/sol/include/syblogin.h
get_dar.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
get_dar.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_dar.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_dar.o: /home/aps/r2.1.1/include/global/llist_err.h
get_dar.o: /home/aps/r2.1.1/include/global/llist.h
get_dar.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_dar.o: /home/aps/r2.1.1/include/local/aps_db_table.h
get_dar.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
get_dar.o: /home/aps/r2.1.1/include/local/db_dar.h
get_site.o: /home/sybase/sol/include/sybfront.h
get_site.o: /home/sybase/sol/include/cstypes.h
get_site.o: /home/sybase/sol/include/csconfig.h
get_site.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_site.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
get_site.o: /usr/include/sys/feature_tests.h
get_site.o: /home/sybase/sol/include/syblogin.h
get_site.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
get_site.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_site.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_site.o: /home/aps/r2.1.1/include/global/llist_err.h
get_site.o: /home/aps/r2.1.1/include/global/llist.h
get_site.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_site.o: /home/aps/r2.1.1/include/local/aps_db_table.h
get_site.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
get_site.o: /home/aps/r2.1.1/include/local/db_site.h
delete_sscvrg.o: /home/sybase/sol/include/sybfront.h
delete_sscvrg.o: /home/sybase/sol/include/cstypes.h
delete_sscvrg.o: /home/sybase/sol/include/csconfig.h
delete_sscvrg.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
delete_sscvrg.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
delete_sscvrg.o: /usr/include/sys/feature_tests.h
delete_sscvrg.o: /home/sybase/sol/include/syblogin.h
delete_sscvrg.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
delete_sscvrg.o: /home/aps/r2.1.1/include/global/db_sybint.h
delete_sscvrg.o: /home/aps/r2.1.1/include/global/dapps_list.h
delete_sscvrg.o: /home/aps/r2.1.1/include/global/llist_err.h
delete_sscvrg.o: /home/aps/r2.1.1/include/global/llist.h
delete_sscvrg.o: /home/aps/r2.1.1/include/global/ord_llist.h
delete_sscvrg.o: /home/aps/r2.1.1/include/local/aps_db_table.h
delete_sscvrg.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
delete_sscvrg.o: /home/aps/r2.1.1/include/local/db_sscvrg.h
insert_sscvrg.o: /home/sybase/sol/include/sybfront.h
insert_sscvrg.o: /home/sybase/sol/include/cstypes.h
insert_sscvrg.o: /home/sybase/sol/include/csconfig.h
insert_sscvrg.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
insert_sscvrg.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
insert_sscvrg.o: /usr/include/sys/feature_tests.h
insert_sscvrg.o: /home/sybase/sol/include/syblogin.h
insert_sscvrg.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
insert_sscvrg.o: /home/aps/r2.1.1/include/global/db_sybint.h
insert_sscvrg.o: /home/aps/r2.1.1/include/global/dapps_list.h
insert_sscvrg.o: /home/aps/r2.1.1/include/global/llist_err.h
insert_sscvrg.o: /home/aps/r2.1.1/include/global/llist.h
insert_sscvrg.o: /home/aps/r2.1.1/include/global/ord_llist.h
insert_sscvrg.o: /home/aps/r2.1.1/include/local/aps_db_table.h
insert_sscvrg.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
insert_sscvrg.o: /home/aps/r2.1.1/include/local/db_sscvrg.h
