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
#	"@(#) /home/aps/r2.1.2/src/DTK_tools/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/WOS_tools
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL = \
        $(IFAIF) \
        $(IODL) \
        $(IAPS) \
        $(ISYBASE) \
        $(IPPS) \
        $(OSIFLAGS) 

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
    $(EVERY_LIB) \
    $(COMMON_ODL_LIB) \
    $(FAIF_UTIL_LIB)

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

EXE          =  aps_upd_DTK_stat

LIB          =  

SRC          =  upd_DTK_stat.c

HEADER_FILES =  

F77_SRC      =  

F77_INC      =  

OBJS         = $(SRC:%.c=%.o) $(F77_SRC:%.for=%.o)

TARGETS = $(EXE) $(LIB)

#TARGETS = $(EXE) $(LIB)
#TARGETS = $(EXE)_purecov $(EXE) $(LIB)
#TARGETS = $(EXE)_purify $(EXE) $(LIB)
#TARGETS = $(EXE)_purecov $(EXE)_purify $(EXE) $(LIB)
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

upd_DTK_stat.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
upd_DTK_stat.o: /usr/include/stdio.h /usr/include/string.h
upd_DTK_stat.o: /usr/include/unistd.h /usr/include/sys/types.h
upd_DTK_stat.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
upd_DTK_stat.o: /usr/include/sys/unistd.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/mu_utilities.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/global/db_sybint.h
upd_DTK_stat.o: /home/sybase/sol/include/sybfront.h
upd_DTK_stat.o: /home/sybase/sol/include/cstypes.h
upd_DTK_stat.o: /home/sybase/sol/include/csconfig.h
upd_DTK_stat.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
upd_DTK_stat.o: /home/sybase/sol/include/sybdb.h
upd_DTK_stat.o: /home/sybase/sol/include/syblogin.h
upd_DTK_stat.o: /home/sybase/sol/include/syberror.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/global/dapps_list.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/global/llist_err.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/global/llist.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/global/ord_llist.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/aps_db_table.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/global/dapps_defs.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/apsfiledef.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/db_dtk.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
upd_DTK_stat.o: /usr/include/syslog.h /usr/include/sys/syslog.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/aps_defs.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/apspath.h
upd_DTK_stat.o: /ASF/PRECM/include/sys/odldef.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/ODLconversions.h
upd_DTK_stat.o: /usr/include/stddef.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/ODL_defs.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/global/fa_defs.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/phase_utilities.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/db_phase.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/global/GENconversions.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/file_utilities.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
upd_DTK_stat.o: /ASF/PRECM/include/faif/faifdefs.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
upd_DTK_stat.o: /ASF/PRECM/include/sys/odlinter.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
upd_DTK_stat.o: /home/aps/r2.1.1/include/local/timeconv.h
