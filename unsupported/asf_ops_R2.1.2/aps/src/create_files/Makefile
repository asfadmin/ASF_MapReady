##############################################################################
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
#	"@(#) /home/aps/r2.1.2/src/create_files/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/create_files

APSFILEDEF_DIR	= $(APS_HOME)/src/gui
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL =      $(IAPS) \
			$(IODL) \
			$(IFAIF) \
			$(IXMOTIF) \
			$(ISYBASE) \
			$(OSIFLAGS) 

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################
#	$(LPPS) \

LALL =      $(LASF) $(LAPS) \
			$(LXMOTIF) \
			$(LGKS) \
			$(LSYBASE) \
			$(LQAPARTNER) \
			$(LENCRYPT) \
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
	$(FAIF_CSA_PARSE_LIB) \
	$(STATIC)	\
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
#       HEADER_FILES is a list of the C Header files
#       F77_SRC  is a list (possibly empty) of the Fortran 77 source
#                code files
#       F77_INC  is a list (possibly empty) of the Fortran 77 include files
#
#
###############################################################################

EXE          	=  aps_crt_dTimeRpt

LIB          	=  

APSFILEDEF_SRC	=	$(APSFILEDEF_DIR)/apsfiledef.c

CREATEFILE_SRC 	=	\
					create_aps_file.c \
					asf_files.c \
					esa_files.c \
					nasda_files.c \
					csa_files.c \
					get_ncvg.c \
					rsndaysvf.c

SRC				:=	$(CREATEFILE_SRC) \
					$(APSFILEDEF_SRC)

#	sch_getdate.c 

HEADER_FILES 	=  

F77_SRC      	=  

F77_INC      	=  

OBJS         	=	$(CREATEFILE_SRC:%.c=%.o) \
					$(APSFILEDEF_SRC:$(APSFILEDEF_DIR)/%.c=%.o) \
					$(F77_SRC:%.for=%.o)

TARGETS 		= $(EXE) $(LIB)
 

###############################################################################
#
# Make the Application
#    Add all targets to be made to the next line
#    If more exes or libs are needed make a copy of the set of 
#    needed commands
#
###############################################################################

all appl:  $(TARGETS) 

$(EXE): $(OBJS) 
	@echo
	@echo "Linking $(EXE) "
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
#  Note there is a force rule for apsfiledef.c because it
#  needs to be retrieved in the GUI directory... This is where the
#  latest and greatest copy should be kept... It is retrieved from sccs
#
###############################################################################

$(CREATEFILE_SRC) $(HEADER_FILES) $(F77_SRC) $(F77_INC) :
	@echo
	@echo "Retrieving $@ as of $(FILE_VERSION) "
	$(CM) $(CM_GET) $(FILE_VERSION) $(SRC_DIR)/$@

$(APSFILEDEF_SRC) :
	@echo FORCING retrieve in $(APSFILEDEF_DIR) of $(notdir $@)
	cd $(APSFILEDEF_DIR) ; $(MAKE) $(notdir $@)

 
###############################################################################
#
# C Compilation.
#
###############################################################################

apsfiledef.o : $(APSFILEDEF_DIR)/apsfiledef.c
	@echo
	@echo "Compiling $<"
	@$(CC) $(CFLAGS) $(IALL) -c $< 

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
	$(F77) $<


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
#  Add post make definitions
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

create_aps_file.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
create_aps_file.o: /usr/include/stdlib.h /usr/include/string.h
create_aps_file.o: /usr/include/unistd.h /usr/include/sys/types.h
create_aps_file.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
create_aps_file.o: /usr/include/sys/unistd.h
create_aps_file.o: /home/aps/r2.1.1/include/local/mu_utilities.h
create_aps_file.o: /home/aps/r2.1.1/include/global/db_sybint.h
create_aps_file.o: /home/sybase/sol/include/sybfront.h
create_aps_file.o: /home/sybase/sol/include/cstypes.h
create_aps_file.o: /home/sybase/sol/include/csconfig.h
create_aps_file.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
create_aps_file.o: /home/sybase/sol/include/sybdb.h
create_aps_file.o: /home/sybase/sol/include/syblogin.h
create_aps_file.o: /home/sybase/sol/include/syberror.h
create_aps_file.o: /home/aps/r2.1.1/include/global/dapps_list.h
create_aps_file.o: /home/aps/r2.1.1/include/global/llist_err.h
create_aps_file.o: /home/aps/r2.1.1/include/global/llist.h
create_aps_file.o: /home/aps/r2.1.1/include/global/ord_llist.h
create_aps_file.o: /home/aps/r2.1.1/include/local/apsfiledef.h
create_aps_file.o: /home/aps/r2.1.1/include/local/apspath.h
create_aps_file.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
create_aps_file.o: /usr/include/syslog.h /usr/include/sys/syslog.h
create_aps_file.o: /home/aps/r2.1.1/include/local/aps_defs.h
create_aps_file.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
create_aps_file.o: /ASF/PRECM/include/faif/faifdefs.h
create_aps_file.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
create_aps_file.o: /ASF/PRECM/include/sys/odldef.h
create_aps_file.o: /ASF/PRECM/include/sys/odlinter.h
create_aps_file.o: /home/aps/r2.1.1/include/global/GENconversions.h
create_aps_file.o: /home/aps/r2.1.1/include/global/fa_defs.h
create_aps_file.o: /home/aps/r2.1.1/include/local/file_utilities.h
asf_files.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
asf_files.o: /home/aps/r2.1.1/include/global/nmalloc.h
asf_files.o: /home/aps/r2.1.1/include/local/mps.h /usr/include/stdlib.h
asf_files.o: /usr/include/string.h /usr/include/time.h
asf_files.o: /home/aps/r2.1.1/include/global/db_sybint.h
asf_files.o: /home/sybase/sol/include/sybfront.h
asf_files.o: /home/sybase/sol/include/cstypes.h
asf_files.o: /home/sybase/sol/include/csconfig.h
asf_files.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
asf_files.o: /home/sybase/sol/include/sybdb.h
asf_files.o: /home/sybase/sol/include/syblogin.h
asf_files.o: /home/sybase/sol/include/syberror.h
asf_files.o: /home/aps/r2.1.1/include/global/dapps_list.h
asf_files.o: /home/aps/r2.1.1/include/global/llist_err.h
asf_files.o: /home/aps/r2.1.1/include/global/llist.h
asf_files.o: /home/aps/r2.1.1/include/global/ord_llist.h
asf_files.o: /home/aps/r2.1.1/include/local/db_sybextern.h
asf_files.o: /home/aps/r2.1.1/include/local/aps_db_table.h
asf_files.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
asf_files.o: /home/aps/r2.1.1/include/local/db_dtk.h
asf_files.o: /home/aps/r2.1.1/include/local/db_cvrg.h
asf_files.o: /home/aps/r2.1.1/include/local/db_satsensor.h
asf_files.o: /home/aps/r2.1.1/include/local/timeconv.h
asf_files.o: /home/aps/r2.1.1/include/local/aps_extern.h
asf_files.o: /home/aps/r2.1.1/include/local/apsfiledef.h
asf_files.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
asf_files.o: /usr/include/syslog.h /usr/include/sys/syslog.h
asf_files.o: /home/aps/r2.1.1/include/local/file_utilities.h
asf_files.o: /home/aps/r2.1.1/include/global/fa_defs.h
asf_files.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
asf_files.o: /ASF/PRECM/include/faif/faifdefs.h
asf_files.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
asf_files.o: /ASF/PRECM/include/sys/odldef.h
asf_files.o: /ASF/PRECM/include/sys/odlinter.h
asf_files.o: /home/aps/r2.1.1/include/local/mu_utilities.h
esa_files.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
esa_files.o: /home/aps/r2.1.1/include/global/dapps_list.h
esa_files.o: /home/aps/r2.1.1/include/global/llist_err.h
esa_files.o: /home/aps/r2.1.1/include/global/llist.h
esa_files.o: /home/aps/r2.1.1/include/global/ord_llist.h
esa_files.o: /home/aps/r2.1.1/include/global/nmalloc.h
esa_files.o: /home/aps/r2.1.1/include/global/db_sybint.h
esa_files.o: /home/sybase/sol/include/sybfront.h
esa_files.o: /home/sybase/sol/include/cstypes.h
esa_files.o: /home/sybase/sol/include/csconfig.h
esa_files.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
esa_files.o: /home/sybase/sol/include/sybdb.h
esa_files.o: /home/sybase/sol/include/syblogin.h
esa_files.o: /home/sybase/sol/include/syberror.h
esa_files.o: /home/aps/r2.1.1/include/local/db_sybextern.h
esa_files.o: /home/aps/r2.1.1/include/local/aps_extern.h
esa_files.o: /home/aps/r2.1.1/include/local/apsfiledef.h
esa_files.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
esa_files.o: /usr/include/syslog.h /usr/include/sys/syslog.h
esa_files.o: /home/aps/r2.1.1/include/local/file_utilities.h
esa_files.o: /home/aps/r2.1.1/include/global/fa_defs.h
esa_files.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
esa_files.o: /ASF/PRECM/include/faif/faifdefs.h
esa_files.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
esa_files.o: /ASF/PRECM/include/sys/odldef.h
esa_files.o: /ASF/PRECM/include/sys/odlinter.h
esa_files.o: /home/aps/r2.1.1/include/local/mu_utilities.h
esa_files.o: /home/aps/r2.1.1/include/local/aps_db_table.h
esa_files.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
esa_files.o: /home/aps/r2.1.1/include/local/db_dtk.h
esa_files.o: /home/aps/r2.1.1/include/local/db_dar.h
esa_files.o: /home/aps/r2.1.1/include/local/db_seg.h
esa_files.o: /home/aps/r2.1.1/include/local/db_schunavail.h
esa_files.o: /home/aps/r2.1.1/include/local/db_rgs_down_times.h
esa_files.o: /home/aps/r2.1.1/include/local/timeconv.h
esa_files.o: /home/aps/r2.1.1/include/local/mps.h /usr/include/stdlib.h
esa_files.o: /usr/include/string.h /usr/include/time.h
nasda_files.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
nasda_files.o: /home/aps/r2.1.1/include/global/dapps_list.h
nasda_files.o: /home/aps/r2.1.1/include/global/llist_err.h
nasda_files.o: /home/aps/r2.1.1/include/global/llist.h
nasda_files.o: /home/aps/r2.1.1/include/global/ord_llist.h
nasda_files.o: /home/aps/r2.1.1/include/global/nmalloc.h
nasda_files.o: /home/aps/r2.1.1/include/global/db_sybint.h
nasda_files.o: /home/sybase/sol/include/sybfront.h
nasda_files.o: /home/sybase/sol/include/cstypes.h
nasda_files.o: /home/sybase/sol/include/csconfig.h
nasda_files.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
nasda_files.o: /home/sybase/sol/include/sybdb.h
nasda_files.o: /home/sybase/sol/include/syblogin.h
nasda_files.o: /home/sybase/sol/include/syberror.h
nasda_files.o: /home/aps/r2.1.1/include/local/db_sybextern.h
nasda_files.o: /home/aps/r2.1.1/include/local/aps_db_table.h
nasda_files.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
nasda_files.o: /home/aps/r2.1.1/include/local/db_dtk.h
nasda_files.o: /home/aps/r2.1.1/include/local/db_seg.h
nasda_files.o: /home/aps/r2.1.1/include/local/db_rgs_down_times.h
nasda_files.o: /home/aps/r2.1.1/include/local/aps_extern.h
nasda_files.o: /home/aps/r2.1.1/include/local/apsfiledef.h
nasda_files.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
nasda_files.o: /usr/include/syslog.h /usr/include/sys/syslog.h
nasda_files.o: /home/aps/r2.1.1/include/local/file_utilities.h
nasda_files.o: /home/aps/r2.1.1/include/global/fa_defs.h
nasda_files.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
nasda_files.o: /ASF/PRECM/include/faif/faifdefs.h
nasda_files.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
nasda_files.o: /ASF/PRECM/include/sys/odldef.h
nasda_files.o: /ASF/PRECM/include/sys/odlinter.h
nasda_files.o: /home/aps/r2.1.1/include/local/mu_utilities.h
nasda_files.o: /home/aps/r2.1.1/include/local/timeconv.h
csa_files.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
csa_files.o: /usr/include/string.h /usr/include/stdlib.h
csa_files.o: /home/aps/r2.1.1/include/local/apspath.h
csa_files.o: /home/aps/r2.1.1/include/local/db_rgs_down_times.h
csa_files.o: /home/aps/r2.1.1/include/local/db_csa_data.h
csa_files.o: /ASF/PRECM/include/faif/CSAparse.h /usr/include/syslog.h
csa_files.o: /usr/include/sys/syslog.h
csa_files.o: /home/aps/r2.1.1/include/local/timeconv.h
csa_files.o: /home/aps/r2.1.1/include/global/GENconversions.h
csa_files.o: /home/aps/r2.1.1/include/global/fa_defs.h
csa_files.o: /home/aps/r2.1.1/include/global/dapps_list.h
csa_files.o: /home/aps/r2.1.1/include/global/llist_err.h
csa_files.o: /home/aps/r2.1.1/include/global/llist.h
csa_files.o: /home/aps/r2.1.1/include/global/ord_llist.h
csa_files.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
csa_files.o: /ASF/PRECM/include/faif/faifdefs.h
csa_files.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
csa_files.o: /ASF/PRECM/include/sys/odldef.h
csa_files.o: /ASF/PRECM/include/sys/odlinter.h
csa_files.o: /home/aps/r2.1.1/include/global/db_sybint.h
csa_files.o: /home/sybase/sol/include/sybfront.h
csa_files.o: /home/sybase/sol/include/cstypes.h
csa_files.o: /home/sybase/sol/include/csconfig.h
csa_files.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
csa_files.o: /home/sybase/sol/include/sybdb.h
csa_files.o: /home/sybase/sol/include/syblogin.h
csa_files.o: /home/sybase/sol/include/syberror.h
csa_files.o: /home/aps/r2.1.1/include/local/aps_db_table.h
csa_files.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
csa_files.o: /home/aps/r2.1.1/include/global/dapps_defs.h
csa_files.o: /home/aps/r2.1.1/include/local/apsfiledef.h
csa_files.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
csa_files.o: /home/aps/r2.1.1/include/local/file_utilities.h
csa_files.o: /home/aps/r2.1.1/include/local/mu_utilities.h
get_ncvg.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
get_ncvg.o: /home/aps/r2.1.1/include/global/db_sybint.h
get_ncvg.o: /home/sybase/sol/include/sybfront.h
get_ncvg.o: /home/sybase/sol/include/cstypes.h
get_ncvg.o: /home/sybase/sol/include/csconfig.h
get_ncvg.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
get_ncvg.o: /home/sybase/sol/include/sybdb.h
get_ncvg.o: /home/sybase/sol/include/syblogin.h
get_ncvg.o: /home/sybase/sol/include/syberror.h
get_ncvg.o: /home/aps/r2.1.1/include/global/dapps_list.h
get_ncvg.o: /home/aps/r2.1.1/include/global/llist_err.h
get_ncvg.o: /home/aps/r2.1.1/include/global/llist.h
get_ncvg.o: /home/aps/r2.1.1/include/global/ord_llist.h
get_ncvg.o: /home/aps/r2.1.1/include/local/db_sybextern.h
get_ncvg.o: /home/aps/r2.1.1/include/local/aps_db_table.h
get_ncvg.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
get_ncvg.o: /home/aps/r2.1.1/include/local/db_cvrg.h
get_ncvg.o: /home/aps/r2.1.1/include/local/aps_extern.h
get_ncvg.o: /home/aps/r2.1.1/include/local/mps.h /usr/include/stdlib.h
get_ncvg.o: /usr/include/string.h /usr/include/time.h
get_ncvg.o: /home/aps/r2.1.1/include/global/nmalloc.h
get_ncvg.o: /home/aps/r2.1.1/include/local/timeconv.h
rsndaysvf.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
rsndaysvf.o: /usr/include/stdio.h /usr/include/string.h /usr/include/ctype.h
rsndaysvf.o: /usr/include/syslog.h /usr/include/sys/syslog.h
rsndaysvf.o: /home/aps/r2.1.1/include/global/dapps_defs.h
rsndaysvf.o: /home/aps/r2.1.1/include/local/apspath.h
rsndaysvf.o: /home/aps/r2.1.1/include/local/apsfiledef.h
rsndaysvf.o: /home/aps/r2.1.1/include/local/timeconv.h
apsfiledef.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
apsfiledef.o: /usr/include/string.h /usr/dt/include/Xm/Xm.h
apsfiledef.o: /usr/openwin/include/X11/Intrinsic.h
apsfiledef.o: /usr/openwin/include/X11/Xlib.h /usr/include/sys/types.h
apsfiledef.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
apsfiledef.o: /usr/openwin/include/X11/X.h
apsfiledef.o: /usr/openwin/include/X11/Xfuncproto.h
apsfiledef.o: /usr/openwin/include/X11/Xosdefs.h /usr/include/stddef.h
apsfiledef.o: /usr/openwin/include/X11/Xutil.h
apsfiledef.o: /usr/openwin/include/X11/Xresource.h
apsfiledef.o: /usr/openwin/include/X11/Core.h
apsfiledef.o: /usr/openwin/include/X11/Composite.h
apsfiledef.o: /usr/openwin/include/X11/Constraint.h
apsfiledef.o: /usr/openwin/include/X11/Object.h
apsfiledef.o: /usr/openwin/include/X11/RectObj.h
apsfiledef.o: /usr/openwin/include/X11/Shell.h
apsfiledef.o: /usr/openwin/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
apsfiledef.o: /usr/openwin/include/X11/StringDefs.h
apsfiledef.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/VendorS.h
apsfiledef.o: /home/aps/r2.1.1/src/gui/aps_exe_names.h
apsfiledef.o: /home/aps/r2.1.1/include/local/apspath.h
apsfiledef.o: /home/aps/r2.1.1/include/local/mu_utilities.h
apsfiledef.o: /home/aps/r2.1.1/include/global/db_sybint.h
apsfiledef.o: /home/sybase/sol/include/sybfront.h
apsfiledef.o: /home/sybase/sol/include/cstypes.h
apsfiledef.o: /home/sybase/sol/include/csconfig.h
apsfiledef.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
apsfiledef.o: /home/sybase/sol/include/sybdb.h
apsfiledef.o: /home/sybase/sol/include/syblogin.h
apsfiledef.o: /home/sybase/sol/include/syberror.h
apsfiledef.o: /home/aps/r2.1.1/include/global/dapps_list.h
apsfiledef.o: /home/aps/r2.1.1/include/global/llist_err.h
apsfiledef.o: /home/aps/r2.1.1/include/global/llist.h
apsfiledef.o: /home/aps/r2.1.1/include/global/ord_llist.h
apsfiledef.o: /home/aps/r2.1.1/include/local/apsfiledef.h
