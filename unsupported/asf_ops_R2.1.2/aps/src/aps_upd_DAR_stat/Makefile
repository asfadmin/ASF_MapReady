###############################################################################
#
# File:         aps_upd_DAR_stat/Makefile 
#
# Function:     APS Makefile for aps_upd_DAR_stat
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#
#
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
 
SRC_DIR = $(APS_HOME)/src/aps_upd_DAR_stat
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL =      $(IAPS) \
			$(ISYBASE) \
			$(OSIFLAGS) \
                        $(IIMS) 

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################

LALL =      $(LASF) $(LAPS) \
			$(LSYBASE) \
			$(OSLFLAGS) \
			$($(OS)LFLAGS) \
			$(LIMS)

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

LIBS = ${EVERY_LIB} \
	$(IMS_LIB) 


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

EXE          =  aps_upd_DAR_stat

LIB          =  

SRC          =  main.c            \
				aps_upd_DAR_stat.c   
 
HEADER_FILES =  aps_upd_DAR_stat.h

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

main.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
main.o: /usr/include/stdlib.h /usr/include/string.h
main.o: /home/aps/r2.1.1/include/local/timeconv.h
main.o: /home/aps/r2.1.1/include/local/aps_log_msg.h /usr/include/syslog.h
main.o: /usr/include/sys/syslog.h /home/aps/r2.1.1/include/local/aps_defs.h
main.o: /home/aps/r2.1.1/include/global/dapps_defs.h
main.o: /home/aps/r2.1.1/include/local/apspath.h
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
main.o: /home/aps/r2.1.1/include/global/ord_llist.h aps_upd_DAR_stat.h
main.o: /usr/include/errno.h /usr/include/sys/errno.h /usr/include/ctype.h
main.o: /usr/include/sys/utsname.h /usr/include/unistd.h
main.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
main.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
main.o: /ASF/PRECM/include/imsdads/ims_query.h
main.o: /ASF/PRECM/include/imsdads/ims_dbms.h
main.o: /home/sybase/sol/include/ctpublic.h
main.o: /home/sybase/sol/include/cspublic.h /home/sybase/sol/include/sqlca.h
main.o: /ASF/PRECM/include/imsdads/ims_const.h
main.o: /ASF/PRECM/include/imsdads/ims_msg.h
main.o: /ASF/PRECM/include/imsdads/ims_qi.h
main.o: /ASF/PRECM/include/imsdads/ims_getInput.h
main.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
main.o: /ASF/PRECM/include/imsdads/ims_util.h
aps_upd_DAR_stat.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
aps_upd_DAR_stat.o: /ASF/PRECM/include/imsdads/ims_query.h
aps_upd_DAR_stat.o: /ASF/PRECM/include/imsdads/ims_dbms.h
aps_upd_DAR_stat.o: /home/sybase/sol/include/sybfront.h
aps_upd_DAR_stat.o: /home/sybase/sol/include/cstypes.h
aps_upd_DAR_stat.o: /home/sybase/sol/include/csconfig.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
aps_upd_DAR_stat.o: /home/sybase/sol/include/ctpublic.h
aps_upd_DAR_stat.o: /home/sybase/sol/include/cspublic.h
aps_upd_DAR_stat.o: /home/sybase/sol/include/sqlca.h
aps_upd_DAR_stat.o: /home/sybase/sol/include/sybdb.h
aps_upd_DAR_stat.o: /home/sybase/sol/include/syblogin.h
aps_upd_DAR_stat.o: /ASF/PRECM/include/imsdads/ims_const.h
aps_upd_DAR_stat.o: /ASF/PRECM/include/imsdads/ims_msg.h
aps_upd_DAR_stat.o: /ASF/PRECM/include/imsdads/ims_qi.h /usr/include/stdlib.h
aps_upd_DAR_stat.o: /usr/include/string.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/global/db_sybint.h
aps_upd_DAR_stat.o: /home/sybase/sol/include/syberror.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/global/dapps_list.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/global/llist_err.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/global/llist.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/global/ord_llist.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/local/timeconv.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/local/db_dar.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/local/aps_db_table.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
aps_upd_DAR_stat.o: /usr/include/syslog.h /usr/include/sys/syslog.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/local/mu_utilities.h
aps_upd_DAR_stat.o: /usr/include/sys/utsname.h aps_upd_DAR_stat.h
aps_upd_DAR_stat.o: /usr/include/errno.h /usr/include/sys/errno.h
aps_upd_DAR_stat.o: /usr/include/ctype.h /usr/include/unistd.h
aps_upd_DAR_stat.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
aps_upd_DAR_stat.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
aps_upd_DAR_stat.o: /ASF/PRECM/include/imsdads/ims_getInput.h
aps_upd_DAR_stat.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
aps_upd_DAR_stat.o: /ASF/PRECM/include/imsdads/ims_util.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/local/aps_defs.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/local/DARconversions.h
aps_upd_DAR_stat.o: /home/aps/r2.1.1/include/global/fa_defs.h
