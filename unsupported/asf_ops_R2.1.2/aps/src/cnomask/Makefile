###############################################################################
#
# File:         cnomask/Makefile 
#
# Function:     APS Makefile for aps_crt_nom_mask
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#   "@(#)Makefile	5.1 98/01/08 APS/ASF"
#   "@(#) /home/aps/r2.1.2/src/cnomask/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/cnomask
 

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

EXE          =  aps_crt_nom_mask

LIB          =  

SRC          =  main.c \
				get_station.c \
				insert_maskinout.c \
				maskinout_metadata.c \
				delete_maskinout.c 
 

HEADER_FILES =  

F77_SRC      =  maskinout.for maskformat.for 

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

main.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
main.o: /usr/include/string.h /usr/include/stdio.h
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
main.o: /home/aps/r2.1.1/include/local/timeconv.h
main.o: /home/aps/r2.1.1/include/local/aps_db_table.h
main.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
main.o: /home/aps/r2.1.1/include/local/aps_defs.h
main.o: /home/aps/r2.1.1/include/local/aps_log_msg.h /usr/include/syslog.h
main.o: /usr/include/sys/syslog.h
main.o: /home/aps/r2.1.1/include/local/mu_utilities.h
main.o: /home/aps/r2.1.1/include/local/db_phase.h
main.o: /home/aps/r2.1.1/include/local/db_station.h
get_station.o: /usr/include/string.h /usr/include/sys/feature_tests.h
get_station.o: /usr/include/stdlib.h
get_station.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_station.o: /home/sybase/sol/include/sybfront.h
get_station.o: /home/sybase/sol/include/cstypes.h
get_station.o: /home/sybase/sol/include/csconfig.h
get_station.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_station.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
get_station.o: /home/sybase/sol/include/syblogin.h
get_station.o: /home/sybase/sol/include/syberror.h
get_station.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_station.o: /home/aps/r2.1.1/include/global/llist_err.h
get_station.o: /home/aps/r2.1.1/include/global/llist.h
get_station.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_station.o: /home/aps/r2.1.1/include/local/aps_db_table.h
get_station.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
get_station.o: /home/aps/r2.1.1/include/local/db_station.h
insert_maskinout.o: /usr/include/string.h /usr/include/sys/feature_tests.h
insert_maskinout.o: /usr/include/stdlib.h
insert_maskinout.o: /home/aps/r2.1.1/include/global/db_sybint.h
insert_maskinout.o: /home/sybase/sol/include/sybfront.h
insert_maskinout.o: /home/sybase/sol/include/cstypes.h
insert_maskinout.o: /home/sybase/sol/include/csconfig.h
insert_maskinout.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
insert_maskinout.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
insert_maskinout.o: /home/sybase/sol/include/syblogin.h
insert_maskinout.o: /home/sybase/sol/include/syberror.h
insert_maskinout.o: /home/aps/r2.1.1/include/global/dapps_list.h
insert_maskinout.o: /home/aps/r2.1.1/include/global/llist_err.h
insert_maskinout.o: /home/aps/r2.1.1/include/global/llist.h
insert_maskinout.o: /home/aps/r2.1.1/include/global/ord_llist.h
insert_maskinout.o: /home/aps/r2.1.1/include/local/aps_db_table.h
insert_maskinout.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
insert_maskinout.o: /home/aps/r2.1.1/include/local/db_maskinout.h
maskinout_metadata.o: /usr/include/string.h /usr/include/sys/feature_tests.h
maskinout_metadata.o: /usr/include/libgen.h /usr/include/sys/types.h
maskinout_metadata.o: /usr/include/sys/isa_defs.h
maskinout_metadata.o: /usr/include/sys/machtypes.h /usr/include/stdio.h
maskinout_metadata.o: /usr/include/time.h /usr/include/stdlib.h
maskinout_metadata.o: /home/aps/r2.1.1/include/global/db_sybint.h
maskinout_metadata.o: /home/sybase/sol/include/sybfront.h
maskinout_metadata.o: /home/sybase/sol/include/cstypes.h
maskinout_metadata.o: /home/sybase/sol/include/csconfig.h
maskinout_metadata.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
maskinout_metadata.o: /home/sybase/sol/include/sybdb.h
maskinout_metadata.o: /home/sybase/sol/include/syblogin.h
maskinout_metadata.o: /home/sybase/sol/include/syberror.h
maskinout_metadata.o: /home/aps/r2.1.1/include/global/dapps_list.h
maskinout_metadata.o: /home/aps/r2.1.1/include/global/llist_err.h
maskinout_metadata.o: /home/aps/r2.1.1/include/global/llist.h
maskinout_metadata.o: /home/aps/r2.1.1/include/global/ord_llist.h
maskinout_metadata.o: /home/aps/r2.1.1/include/local/aps_db_table.h
maskinout_metadata.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
maskinout_metadata.o: /home/aps/r2.1.1/include/local/db_maskinout_mdat.h
delete_maskinout.o: /usr/include/string.h /usr/include/sys/feature_tests.h
delete_maskinout.o: /usr/include/stdlib.h
delete_maskinout.o: /home/aps/r2.1.1/include/global/db_sybint.h
delete_maskinout.o: /home/sybase/sol/include/sybfront.h
delete_maskinout.o: /home/sybase/sol/include/cstypes.h
delete_maskinout.o: /home/sybase/sol/include/csconfig.h
delete_maskinout.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
delete_maskinout.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
delete_maskinout.o: /home/sybase/sol/include/syblogin.h
delete_maskinout.o: /home/sybase/sol/include/syberror.h
delete_maskinout.o: /home/aps/r2.1.1/include/global/dapps_list.h
delete_maskinout.o: /home/aps/r2.1.1/include/global/llist_err.h
delete_maskinout.o: /home/aps/r2.1.1/include/global/llist.h
delete_maskinout.o: /home/aps/r2.1.1/include/global/ord_llist.h
delete_maskinout.o: /home/aps/r2.1.1/include/local/aps_db_table.h
delete_maskinout.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
delete_maskinout.o: /home/aps/r2.1.1/include/local/db_maskinout.h
