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
#	"@(#) /home/aps/r2.1.2/src/darstats/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/darstats
 

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

EXE          =  aps_darstats

LIB          =  

SRC          =  main.c aps_darStatistics.c write_ims_darStatistics.c

HEADER_FILES =  aps_darStatistics.h

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

main.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
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
main.o: /home/aps/r2.1.1/include/local/aps_db_table.h
main.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
main.o: /home/aps/r2.1.1/include/local/aps_defs.h
main.o: /home/aps/r2.1.1/include/global/dapps_defs.h
main.o: /home/aps/r2.1.1/include/local/db_dtk.h /usr/include/string.h
main.o: /usr/include/stdlib.h /usr/include/unistd.h /usr/include/sys/types.h
main.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
main.o: /usr/include/sys/unistd.h
main.o: /home/aps/r2.1.1/include/local/aps_encrypt.h
main.o: /home/aps/r2.1.1/include/local/mu_utilities.h
main.o: /home/aps/r2.1.1/include/local/aps_log_msg.h /usr/include/syslog.h
main.o: /usr/include/sys/syslog.h /home/aps/r2.1.1/include/local/apspath.h
main.o: aps_darStatistics.h /usr/include/errno.h /usr/include/sys/errno.h
main.o: /usr/include/ctype.h /usr/include/sys/utsname.h
main.o: /ASF/PRECM/include/imsdads/ims_dbms.h
main.o: /home/sybase/sol/include/ctpublic.h
main.o: /home/sybase/sol/include/cspublic.h /home/sybase/sol/include/sqlca.h
main.o: /ASF/PRECM/include/imsdads/ims_query.h
main.o: /ASF/PRECM/include/imsdads/ims_const.h
main.o: /ASF/PRECM/include/imsdads/ims_msg.h
main.o: /ASF/PRECM/include/imsdads/ims_qi.h
main.o: /ASF/PRECM/include/imsdads/ims_getInput.h
main.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
main.o: /ASF/PRECM/include/imsdads/ims_util.h
aps_darStatistics.o: /home/aps/r2.1.1/include/global/db_sybint.h
aps_darStatistics.o: /home/sybase/sol/include/sybfront.h
aps_darStatistics.o: /home/sybase/sol/include/cstypes.h
aps_darStatistics.o: /home/sybase/sol/include/csconfig.h
aps_darStatistics.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
aps_darStatistics.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
aps_darStatistics.o: /usr/include/sys/feature_tests.h
aps_darStatistics.o: /home/sybase/sol/include/syblogin.h
aps_darStatistics.o: /home/sybase/sol/include/syberror.h
aps_darStatistics.o: /home/aps/r2.1.1/include/global/dapps_list.h
aps_darStatistics.o: /home/aps/r2.1.1/include/global/llist_err.h
aps_darStatistics.o: /home/aps/r2.1.1/include/global/llist.h
aps_darStatistics.o: /home/aps/r2.1.1/include/global/ord_llist.h
aps_darStatistics.o: /home/aps/r2.1.1/include/local/aps_db_table.h
aps_darStatistics.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
aps_darStatistics.o: /home/aps/r2.1.1/include/local/aps_defs.h
aps_darStatistics.o: /home/aps/r2.1.1/include/local/db_dtk.h
aps_darStatistics.o: /home/aps/r2.1.1/include/local/timeconv.h
aps_darStatistics.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
aps_darStatistics.o: /home/aps/r2.1.1/include/local/DARconversions.h
aps_darStatistics.o: /home/aps/r2.1.1/include/global/fa_defs.h
aps_darStatistics.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
aps_darStatistics.o: /usr/include/syslog.h /usr/include/sys/syslog.h
aps_darStatistics.o: aps_darStatistics.h /usr/include/stdlib.h
aps_darStatistics.o: /usr/include/errno.h /usr/include/sys/errno.h
aps_darStatistics.o: /usr/include/ctype.h /usr/include/string.h
aps_darStatistics.o: /usr/include/sys/utsname.h /usr/include/unistd.h
aps_darStatistics.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
aps_darStatistics.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
aps_darStatistics.o: /ASF/PRECM/include/imsdads/ims_dbms.h
aps_darStatistics.o: /home/sybase/sol/include/ctpublic.h
aps_darStatistics.o: /home/sybase/sol/include/cspublic.h
aps_darStatistics.o: /home/sybase/sol/include/sqlca.h
aps_darStatistics.o: /ASF/PRECM/include/imsdads/ims_query.h
aps_darStatistics.o: /ASF/PRECM/include/imsdads/ims_const.h
aps_darStatistics.o: /ASF/PRECM/include/imsdads/ims_msg.h
aps_darStatistics.o: /ASF/PRECM/include/imsdads/ims_qi.h
aps_darStatistics.o: /ASF/PRECM/include/imsdads/ims_getInput.h
aps_darStatistics.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
aps_darStatistics.o: /ASF/PRECM/include/imsdads/ims_util.h
write_ims_darStatistics.o: /usr/include/string.h
write_ims_darStatistics.o: /usr/include/sys/feature_tests.h
write_ims_darStatistics.o: /usr/include/stdlib.h
write_ims_darStatistics.o: /home/aps/r2.1.1/include/local/aps_defs.h
write_ims_darStatistics.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
write_ims_darStatistics.o: /usr/include/stdio.h /usr/include/syslog.h
write_ims_darStatistics.o: /usr/include/sys/syslog.h aps_darStatistics.h
write_ims_darStatistics.o: /usr/include/errno.h /usr/include/sys/errno.h
write_ims_darStatistics.o: /usr/include/ctype.h /usr/include/sys/utsname.h
write_ims_darStatistics.o: /usr/include/unistd.h /usr/include/sys/types.h
write_ims_darStatistics.o: /usr/include/sys/isa_defs.h
write_ims_darStatistics.o: /usr/include/sys/machtypes.h
write_ims_darStatistics.o: /usr/include/sys/unistd.h
write_ims_darStatistics.o: /ASF/PRECM/include/imsdads/ims_dbms.h
write_ims_darStatistics.o: /home/sybase/sol/include/sybfront.h
write_ims_darStatistics.o: /home/sybase/sol/include/cstypes.h
write_ims_darStatistics.o: /home/sybase/sol/include/csconfig.h
write_ims_darStatistics.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
write_ims_darStatistics.o: /home/sybase/sol/include/ctpublic.h
write_ims_darStatistics.o: /home/sybase/sol/include/cspublic.h
write_ims_darStatistics.o: /home/sybase/sol/include/sqlca.h
write_ims_darStatistics.o: /home/sybase/sol/include/sybdb.h
write_ims_darStatistics.o: /home/sybase/sol/include/syblogin.h
write_ims_darStatistics.o: /ASF/PRECM/include/imsdads/ims_query.h
write_ims_darStatistics.o: /ASF/PRECM/include/imsdads/ims_const.h
write_ims_darStatistics.o: /ASF/PRECM/include/imsdads/ims_msg.h
write_ims_darStatistics.o: /ASF/PRECM/include/imsdads/ims_qi.h
write_ims_darStatistics.o: /ASF/PRECM/include/imsdads/ims_getInput.h
write_ims_darStatistics.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
write_ims_darStatistics.o: /ASF/PRECM/include/imsdads/ims_util.h
