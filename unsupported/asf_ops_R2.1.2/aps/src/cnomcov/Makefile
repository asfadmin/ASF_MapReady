###############################################################################
#
# File:         cnomcov/Makefile 
#
# Function:     APS Makefile for aps_crt_nom_cov
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/cnomcov/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/cnomcov
 

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

LIBS = ${EVERY_LIB}

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

EXE          =  aps_crt_nom_cov

LIB          =  

SRC          =  create_nominal_cov.c \
				get_station.c \
				insert_cvrg.c \
				cvrg_metadata.c \
				delete_cov.c 
 

HEADER_FILES =  

F77_SRC      =  a_ephem.for   a_kepler.for coord.for coverage.for \
	            covformat.for setsun.for

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

create_nominal_cov.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
create_nominal_cov.o: /usr/include/string.h /usr/include/stdio.h
create_nominal_cov.o: /home/aps/r2.1.1/include/global/db_sybint.h
create_nominal_cov.o: /home/sybase/sol/include/sybfront.h
create_nominal_cov.o: /home/sybase/sol/include/cstypes.h
create_nominal_cov.o: /home/sybase/sol/include/csconfig.h
create_nominal_cov.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
create_nominal_cov.o: /home/sybase/sol/include/sybdb.h
create_nominal_cov.o: /home/sybase/sol/include/syblogin.h
create_nominal_cov.o: /home/sybase/sol/include/syberror.h
create_nominal_cov.o: /home/aps/r2.1.1/include/global/dapps_list.h
create_nominal_cov.o: /home/aps/r2.1.1/include/global/llist_err.h
create_nominal_cov.o: /home/aps/r2.1.1/include/global/llist.h
create_nominal_cov.o: /home/aps/r2.1.1/include/global/ord_llist.h
create_nominal_cov.o: /home/aps/r2.1.1/include/local/aps_defs.h
create_nominal_cov.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
create_nominal_cov.o: /usr/include/syslog.h /usr/include/sys/syslog.h
create_nominal_cov.o: /home/aps/r2.1.1/include/local/mu_utilities.h
get_station.o: /home/sybase/sol/include/sybfront.h
get_station.o: /home/sybase/sol/include/cstypes.h
get_station.o: /home/sybase/sol/include/csconfig.h
get_station.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_station.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
get_station.o: /usr/include/sys/feature_tests.h
get_station.o: /home/sybase/sol/include/syblogin.h
get_station.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
get_station.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_station.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_station.o: /home/aps/r2.1.1/include/global/llist_err.h
get_station.o: /home/aps/r2.1.1/include/global/llist.h
get_station.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_station.o: /home/aps/r2.1.1/include/local/aps_db_table.h
get_station.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
get_station.o: /home/aps/r2.1.1/include/local/db_station.h
insert_cvrg.o: /home/sybase/sol/include/sybfront.h
insert_cvrg.o: /home/sybase/sol/include/cstypes.h
insert_cvrg.o: /home/sybase/sol/include/csconfig.h
insert_cvrg.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
insert_cvrg.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
insert_cvrg.o: /usr/include/sys/feature_tests.h
insert_cvrg.o: /home/sybase/sol/include/syblogin.h
insert_cvrg.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
insert_cvrg.o: /home/aps/r2.1.1/include/global/db_sybint.h
insert_cvrg.o: /home/aps/r2.1.1/include/global/dapps_list.h
insert_cvrg.o: /home/aps/r2.1.1/include/global/llist_err.h
insert_cvrg.o: /home/aps/r2.1.1/include/global/llist.h
insert_cvrg.o: /home/aps/r2.1.1/include/global/ord_llist.h
insert_cvrg.o: /home/aps/r2.1.1/include/local/aps_db_table.h
insert_cvrg.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
insert_cvrg.o: /home/aps/r2.1.1/include/local/db_cvrg.h
cvrg_metadata.o: /home/sybase/sol/include/sybfront.h
cvrg_metadata.o: /home/sybase/sol/include/cstypes.h
cvrg_metadata.o: /home/sybase/sol/include/csconfig.h
cvrg_metadata.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
cvrg_metadata.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
cvrg_metadata.o: /usr/include/sys/feature_tests.h
cvrg_metadata.o: /home/sybase/sol/include/syblogin.h
cvrg_metadata.o: /home/sybase/sol/include/syberror.h /usr/include/libgen.h
cvrg_metadata.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
cvrg_metadata.o: /usr/include/sys/machtypes.h /usr/include/time.h
cvrg_metadata.o: /usr/include/stdlib.h
cvrg_metadata.o: /home/aps/r2.1.1/include/global/db_sybint.h
cvrg_metadata.o: /home/aps/r2.1.1/include/global/dapps_list.h
cvrg_metadata.o: /home/aps/r2.1.1/include/global/llist_err.h
cvrg_metadata.o: /home/aps/r2.1.1/include/global/llist.h
cvrg_metadata.o: /home/aps/r2.1.1/include/global/ord_llist.h
cvrg_metadata.o: /home/aps/r2.1.1/include/local/aps_db_table.h
cvrg_metadata.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
cvrg_metadata.o: /home/aps/r2.1.1/include/local/db_cvrgmdat.h
delete_cov.o: /home/sybase/sol/include/sybfront.h
delete_cov.o: /home/sybase/sol/include/cstypes.h
delete_cov.o: /home/sybase/sol/include/csconfig.h
delete_cov.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
delete_cov.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
delete_cov.o: /usr/include/sys/feature_tests.h
delete_cov.o: /home/sybase/sol/include/syblogin.h
delete_cov.o: /home/sybase/sol/include/syberror.h /usr/include/stdlib.h
delete_cov.o: /home/aps/r2.1.1/include/global/db_sybint.h
delete_cov.o: /home/aps/r2.1.1/include/global/dapps_list.h
delete_cov.o: /home/aps/r2.1.1/include/global/llist_err.h
delete_cov.o: /home/aps/r2.1.1/include/global/llist.h
delete_cov.o: /home/aps/r2.1.1/include/global/ord_llist.h
delete_cov.o: /home/aps/r2.1.1/include/local/aps_db_table.h
delete_cov.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
delete_cov.o: /home/aps/r2.1.1/include/local/db_cvrg.h
