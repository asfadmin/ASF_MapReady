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
#	"@(#) /home/aps/r2.1.2/src/archive/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/archive_APS_file
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################
IALL =      $(IIMS) $(IAPS) \
			$(ISYBASE) 

#$(IPPS) 
#$(OSIFLAGS) $(IODL)

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################
LALL =      $(LAPS) \
			$(LIMS) \
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
	$(STATIC) \
	$(IMS_LIB)

###############################################################################
#
# Flags and Defines
#
###############################################################################
DEFS =  -D$(MACH)
CFLAGS = $(OSCFLAGS) $(DEFS) 

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

EXE          = aps_archive_APS_file 

LIB          =  

SRC          = main.c archive_APS_file.c 

HEADER_FILES =  

F77_SRC      =  

F77_INC      =  

OBJS         = $(SRC:%.c=%.o) $(F77_SRC:%.for=%.o)

TARGETS = $(EXE) $(LIB) $(EXE1)
 

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
#	@rm $(OBJS)
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
#  Note there is a brute force rule for apsfiledef.c because because it
#  needs to be retrieved from the GUI directory... This is where the
#  latest and greatest copy should be kept... It is retrieved from sccs
#  There are conditional compiles in here so the file is used by the 
#  create_aps_file application and the gui
#
###############################################################################

$(SRC) $(HEADER_FILES) $(F77_SRC) $(F77_INC) :
	@echo
	@echo "Retrieving $@ as of $(FILE_VERSION) "
	$(CM) $(CM_GET) $(FILE_VERSION) $(SRC_DIR)/$@

$(SRC_DIR)/../gui/apsfiledef.c :
	@echo "Invoking FORCE rules..."
	rm -f apsfiledef.c apsfiledef.o
	@echo "Retrieving apsfiledef.c as of $(FILE_VERSION) "
	$(CM) $(CM_GET) $(FILE_VERSION) -G$@ $@
		

 
###############################################################################
#
# C Compilation.
#
###############################################################################

apsfiledef.o : $(SRC_DIR)/../gui/apsfiledef.c
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
clobber:
	@echo
	@echo "Cleaning " $(OBJS)
	rm -f $(OBJS) 
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

main.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
main.o: /usr/include/string.h /usr/include/stdlib.h /usr/include/malloc.h
main.o: /home/aps/r2.1.1/include/local/aps_log_msg.h /usr/include/syslog.h
main.o: /usr/include/sys/syslog.h /home/aps/r2.1.1/include/local/aps_defs.h
archive_APS_file.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
archive_APS_file.o: /usr/include/stdlib.h /usr/include/string.h
archive_APS_file.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
archive_APS_file.o: /usr/include/syslog.h /usr/include/sys/syslog.h
archive_APS_file.o: /home/aps/r2.1.1/include/local/aps_defs.h
archive_APS_file.o: /usr/include/sys/utsname.h /usr/include/unistd.h
archive_APS_file.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
archive_APS_file.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
archive_APS_file.o: /usr/include/sys/wait.h
archive_APS_file.o: /ASF/PRECM/include/imsdads/ims_query.h
archive_APS_file.o: /ASF/PRECM/include/imsdads/ims_dbms.h
archive_APS_file.o: /home/sybase/sol/include/sybfront.h
archive_APS_file.o: /home/sybase/sol/include/cstypes.h
archive_APS_file.o: /home/sybase/sol/include/csconfig.h
archive_APS_file.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
archive_APS_file.o: /home/sybase/sol/include/ctpublic.h
archive_APS_file.o: /home/sybase/sol/include/cspublic.h
archive_APS_file.o: /home/sybase/sol/include/sqlca.h
archive_APS_file.o: /home/sybase/sol/include/sybdb.h
archive_APS_file.o: /home/sybase/sol/include/syblogin.h
archive_APS_file.o: /ASF/PRECM/include/imsdads/ims_const.h
archive_APS_file.o: /ASF/PRECM/include/imsdads/ims_msg.h
archive_APS_file.o: /ASF/PRECM/include/imsdads/ims_qi.h
archive_APS_file.o: /ASF/PRECM/include/imsdads/ims_archive.h
