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
#	"@(#) /home/aps/r2.1.2/src/mapr/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/mapr
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL =      $(IAPS) \
			$(IXMOTIF) \
			$(IGKS) \
			$(ISYBASE) \
			$(OSIFLAGS) 

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################

LALL =      $(LASF) $(LAPS) \
			$(LXMOTIF) \
			$(LGKS) \
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

LIBS = \
	$(EVERY_LIB) \
	$(XMOTIF_LIB) \
	$(GKS_LIB)

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

EXE =  aps_mapper

LIB =  

# this source file was removed; it is in src/aps:
#	f77_aps_fullpath.c \
# end of list of removed files.  
SRC        =  \
	ask_fields.c        \
	db_fortran_bindings.c \
	db_print_planning_list.c  \
	f77_aux.c \
	get_table_index.c \
	getpid_str.c      \
	main.c \
	mapr_getdup.c \
	mu_print_error_msg.c    \
	print2laser.c \
	table_def.c 


HEADER_FILES = table_def.h


F77_SRC    = \
	mapper.for mpscvg.for mpsdar.for mpsdtk.for mpssite.for \
	mpsseg.for mpsspc.for spc2seg.for \
	get_def_ch.for \
	begseg.for begws.for border.for calc_center.for calc_window.for \
	calcobs.for center.for difdeg.for dismsg.for dismwf.for disp_seg.for \
	displot.for disprint.for \
	disseg.for diswmf.for endws.for get_center.for globe.for grid.for \
	grid_display.for grid_setup.for grid_text.for gridline.for ltrans.for \
	manage.for merzoom.for metafile.for minput.for mpsline.for \
	overly.for pol_line.for polchk.for polgrid.for poltext.for \
	polzoom.for remrep.for save.for segmnt.for set_colors.for \
	set_window.for stereo.for text_scale.for trim.for userio.for \
	viewport.for window.for winput.for world.for zmseg.for zoom.for \
	chfnam.for  exfnam.for
 

F77_INC = \
	mapper_port.inc \
   	mapr_db_extern.inc \
 	table_names.inc

OBJS       = $(SRC:%.c=%.o) $(F77_SRC:%.for=%.o)

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

ask_fields.o: /usr/include/unistd.h /usr/include/sys/feature_tests.h
ask_fields.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
ask_fields.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
ask_fields.o: /home/aps/r2.1.1/include/global/db_sybint.h
ask_fields.o: /home/sybase/sol/include/sybfront.h
ask_fields.o: /home/sybase/sol/include/cstypes.h
ask_fields.o: /home/sybase/sol/include/csconfig.h
ask_fields.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
ask_fields.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
ask_fields.o: /home/sybase/sol/include/syblogin.h
ask_fields.o: /home/sybase/sol/include/syberror.h
ask_fields.o: /home/aps/r2.1.1/include/global/dapps_list.h
ask_fields.o: /home/aps/r2.1.1/include/global/llist_err.h
ask_fields.o: /home/aps/r2.1.1/include/global/llist.h
ask_fields.o: /home/aps/r2.1.1/include/global/ord_llist.h
ask_fields.o: /home/aps/r2.1.1/include/local/aps_db_table.h
ask_fields.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
ask_fields.o: /home/aps/r2.1.1/include/local/db_satsensor.h
ask_fields.o: /home/aps/r2.1.1/include/global/dapps_defs.h
db_fortran_bindings.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/global/llist_err.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/global/llist.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/global/db_sybint.h
db_fortran_bindings.o: /home/sybase/sol/include/sybfront.h
db_fortran_bindings.o: /home/sybase/sol/include/cstypes.h
db_fortran_bindings.o: /home/sybase/sol/include/csconfig.h
db_fortran_bindings.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_fortran_bindings.o: /home/sybase/sol/include/sybdb.h
db_fortran_bindings.o: /home/sybase/sol/include/syblogin.h
db_fortran_bindings.o: /home/sybase/sol/include/syberror.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/local/db_cvrg.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/local/db_dar.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/local/db_dtk.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/local/db_seg.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/local/db_satsensor.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/local/db_site.h
db_fortran_bindings.o: /home/aps/r2.1.1/include/local/db_sscvrg.h
db_print_planning_list.o: /home/aps/r2.1.1/include/global/db_sybint.h
db_print_planning_list.o: /home/sybase/sol/include/sybfront.h
db_print_planning_list.o: /home/sybase/sol/include/cstypes.h
db_print_planning_list.o: /home/sybase/sol/include/csconfig.h
db_print_planning_list.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_print_planning_list.o: /home/sybase/sol/include/sybdb.h
db_print_planning_list.o: /usr/include/stdio.h
db_print_planning_list.o: /usr/include/sys/feature_tests.h
db_print_planning_list.o: /home/sybase/sol/include/syblogin.h
db_print_planning_list.o: /home/sybase/sol/include/syberror.h
db_print_planning_list.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_print_planning_list.o: /home/aps/r2.1.1/include/global/llist_err.h
db_print_planning_list.o: /home/aps/r2.1.1/include/global/llist.h
db_print_planning_list.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_print_planning_list.o: /home/aps/r2.1.1/include/local/aps_db_table.h
db_print_planning_list.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
db_print_planning_list.o: /home/aps/r2.1.1/include/local/db_active_planning_activities.h
f77_aux.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
get_table_index.o: table_def.h
getpid_str.o: /usr/include/sys/types.h /usr/include/sys/feature_tests.h
getpid_str.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
getpid_str.o: /usr/include/unistd.h /usr/include/sys/unistd.h
main.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
main.o: /usr/include/stdio.h
mapr_getdup.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
mapr_getdup.o: /usr/include/stdio.h
mu_print_error_msg.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_print_error_msg.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_print_error_msg.o: /home/sybase/sol/include/sybfront.h
mu_print_error_msg.o: /home/sybase/sol/include/cstypes.h
mu_print_error_msg.o: /home/sybase/sol/include/csconfig.h
mu_print_error_msg.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_print_error_msg.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
mu_print_error_msg.o: /usr/include/sys/feature_tests.h
mu_print_error_msg.o: /home/sybase/sol/include/syblogin.h
mu_print_error_msg.o: /home/sybase/sol/include/syberror.h
mu_print_error_msg.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_print_error_msg.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_print_error_msg.o: /home/aps/r2.1.1/include/global/llist.h
mu_print_error_msg.o: /home/aps/r2.1.1/include/global/ord_llist.h
print2laser.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
print2laser.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
print2laser.o: /usr/include/sys/machtypes.h /usr/include/unistd.h
print2laser.o: /usr/include/sys/unistd.h
table_def.o: table_def.h
