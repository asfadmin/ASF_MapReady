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
#	"@(#) /home/aps/r2.1.2/src/lib_sybint/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/lib_sybint
 

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

GLOBAL_LIB	 =  libsybint.a

SRC          =          \
				db_get_max_value.c           \
				db_get_min_value.c           \
				db_install_error_handler.c    \
				db_install_message_handler.c  \
				db_num_records.c             \
				db_open.c                  \
				db_open_APS_readonly.c    \
				db_sybint.c              \
				db_transaction.c

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
	$(F77_LOG) ; $(F77) $<


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

db_get_max_value.o: /usr/include/string.h /usr/include/sys/feature_tests.h
db_get_max_value.o: /usr/include/stdlib.h
db_get_max_value.o: /home/aps/r2.1.1/include/global/db_sybint.h
db_get_max_value.o: /home/sybase/sol/include/sybfront.h
db_get_max_value.o: /home/sybase/sol/include/cstypes.h
db_get_max_value.o: /home/sybase/sol/include/csconfig.h
db_get_max_value.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_get_max_value.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
db_get_max_value.o: /home/sybase/sol/include/syblogin.h
db_get_max_value.o: /home/sybase/sol/include/syberror.h
db_get_max_value.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_get_max_value.o: /home/aps/r2.1.1/include/global/llist_err.h
db_get_max_value.o: /home/aps/r2.1.1/include/global/llist.h
db_get_max_value.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_get_max_value.o: /home/aps/r2.1.1/include/global/nmalloc.h
db_get_min_value.o: /usr/include/string.h /usr/include/sys/feature_tests.h
db_get_min_value.o: /usr/include/stdlib.h
db_get_min_value.o: /home/aps/r2.1.1/include/global/db_sybint.h
db_get_min_value.o: /home/sybase/sol/include/sybfront.h
db_get_min_value.o: /home/sybase/sol/include/cstypes.h
db_get_min_value.o: /home/sybase/sol/include/csconfig.h
db_get_min_value.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_get_min_value.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
db_get_min_value.o: /home/sybase/sol/include/syblogin.h
db_get_min_value.o: /home/sybase/sol/include/syberror.h
db_get_min_value.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_get_min_value.o: /home/aps/r2.1.1/include/global/llist_err.h
db_get_min_value.o: /home/aps/r2.1.1/include/global/llist.h
db_get_min_value.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_get_min_value.o: /home/aps/r2.1.1/include/global/nmalloc.h
db_install_error_handler.o: /home/aps/r2.1.1/include/global/db_sybint.h
db_install_error_handler.o: /home/sybase/sol/include/sybfront.h
db_install_error_handler.o: /home/sybase/sol/include/cstypes.h
db_install_error_handler.o: /home/sybase/sol/include/csconfig.h
db_install_error_handler.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_install_error_handler.o: /home/sybase/sol/include/sybdb.h
db_install_error_handler.o: /usr/include/stdio.h
db_install_error_handler.o: /usr/include/sys/feature_tests.h
db_install_error_handler.o: /home/sybase/sol/include/syblogin.h
db_install_error_handler.o: /home/sybase/sol/include/syberror.h
db_install_error_handler.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_install_error_handler.o: /home/aps/r2.1.1/include/global/llist_err.h
db_install_error_handler.o: /home/aps/r2.1.1/include/global/llist.h
db_install_error_handler.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_install_message_handler.o: /home/aps/r2.1.1/include/global/db_sybint.h
db_install_message_handler.o: /home/sybase/sol/include/sybfront.h
db_install_message_handler.o: /home/sybase/sol/include/cstypes.h
db_install_message_handler.o: /home/sybase/sol/include/csconfig.h
db_install_message_handler.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_install_message_handler.o: /home/sybase/sol/include/sybdb.h
db_install_message_handler.o: /usr/include/stdio.h
db_install_message_handler.o: /usr/include/sys/feature_tests.h
db_install_message_handler.o: /home/sybase/sol/include/syblogin.h
db_install_message_handler.o: /home/sybase/sol/include/syberror.h
db_install_message_handler.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_install_message_handler.o: /home/aps/r2.1.1/include/global/llist_err.h
db_install_message_handler.o: /home/aps/r2.1.1/include/global/llist.h
db_install_message_handler.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_num_records.o: /home/aps/r2.1.1/include/global/db_sybint.h
db_num_records.o: /home/sybase/sol/include/sybfront.h
db_num_records.o: /home/sybase/sol/include/cstypes.h
db_num_records.o: /home/sybase/sol/include/csconfig.h
db_num_records.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_num_records.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
db_num_records.o: /usr/include/sys/feature_tests.h
db_num_records.o: /home/sybase/sol/include/syblogin.h
db_num_records.o: /home/sybase/sol/include/syberror.h
db_num_records.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_num_records.o: /home/aps/r2.1.1/include/global/llist_err.h
db_num_records.o: /home/aps/r2.1.1/include/global/llist.h
db_num_records.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_num_records.o: /usr/include/stdlib.h /usr/include/string.h
db_open.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
db_open.o: /usr/include/stdlib.h /usr/include/unistd.h
db_open.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
db_open.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
db_open.o: /usr/include/varargs.h /usr/include/fcntl.h
db_open.o: /usr/include/sys/fcntl.h /usr/include/string.h
db_open.o: /usr/include/ctype.h /home/aps/r2.1.1/include/global/db_sybint.h
db_open.o: /home/sybase/sol/include/sybfront.h
db_open.o: /home/sybase/sol/include/cstypes.h
db_open.o: /home/sybase/sol/include/csconfig.h
db_open.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_open.o: /home/sybase/sol/include/sybdb.h
db_open.o: /home/sybase/sol/include/syblogin.h
db_open.o: /home/sybase/sol/include/syberror.h
db_open.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_open.o: /home/aps/r2.1.1/include/global/llist_err.h
db_open.o: /home/aps/r2.1.1/include/global/llist.h
db_open.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_open.o: /home/aps/r2.1.1/include/local/aps.h
db_open.o: /home/aps/r2.1.1/include/local/aps_db_table.h
db_open.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
db_open.o: /home/aps/r2.1.1/include/global/nmalloc.h
db_open_APS_readonly.o: /usr/include/stdlib.h
db_open_APS_readonly.o: /usr/include/sys/feature_tests.h
db_open_APS_readonly.o: /usr/include/string.h
db_open_APS_readonly.o: /home/aps/r2.1.1/include/global/db_sybint.h
db_open_APS_readonly.o: /home/sybase/sol/include/sybfront.h
db_open_APS_readonly.o: /home/sybase/sol/include/cstypes.h
db_open_APS_readonly.o: /home/sybase/sol/include/csconfig.h
db_open_APS_readonly.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_open_APS_readonly.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
db_open_APS_readonly.o: /home/sybase/sol/include/syblogin.h
db_open_APS_readonly.o: /home/sybase/sol/include/syberror.h
db_open_APS_readonly.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_open_APS_readonly.o: /home/aps/r2.1.1/include/global/llist_err.h
db_open_APS_readonly.o: /home/aps/r2.1.1/include/global/llist.h
db_open_APS_readonly.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_sybint.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
db_sybint.o: /usr/include/stdlib.h /usr/include/unistd.h
db_sybint.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
db_sybint.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
db_sybint.o: /usr/include/varargs.h /usr/include/fcntl.h
db_sybint.o: /usr/include/sys/fcntl.h /usr/include/string.h
db_sybint.o: /usr/include/ctype.h /home/aps/r2.1.1/include/global/db_sybint.h
db_sybint.o: /home/sybase/sol/include/sybfront.h
db_sybint.o: /home/sybase/sol/include/cstypes.h
db_sybint.o: /home/sybase/sol/include/csconfig.h
db_sybint.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_sybint.o: /home/sybase/sol/include/sybdb.h
db_sybint.o: /home/sybase/sol/include/syblogin.h
db_sybint.o: /home/sybase/sol/include/syberror.h
db_sybint.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_sybint.o: /home/aps/r2.1.1/include/global/llist_err.h
db_sybint.o: /home/aps/r2.1.1/include/global/llist.h
db_sybint.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_sybint.o: /home/aps/r2.1.1/include/local/aps.h
db_sybint.o: /home/aps/r2.1.1/include/local/aps_db_table.h
db_sybint.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
db_sybint.o: /home/aps/r2.1.1/include/global/nmalloc.h
db_transaction.o: /home/aps/r2.1.1/include/global/db_sybint.h
db_transaction.o: /home/sybase/sol/include/sybfront.h
db_transaction.o: /home/sybase/sol/include/cstypes.h
db_transaction.o: /home/sybase/sol/include/csconfig.h
db_transaction.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_transaction.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
db_transaction.o: /usr/include/sys/feature_tests.h
db_transaction.o: /home/sybase/sol/include/syblogin.h
db_transaction.o: /home/sybase/sol/include/syberror.h
db_transaction.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_transaction.o: /home/aps/r2.1.1/include/global/llist_err.h
db_transaction.o: /home/aps/r2.1.1/include/global/llist.h
db_transaction.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_transaction.o: /home/aps/r2.1.1/include/global/dapps_defs.h
