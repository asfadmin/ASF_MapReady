###############################################################################
#
# File:         Makefile
#
# Function:     Baseline Version of APS Makefile, Miguel's test version
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#	
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/lib_APSpmfutils/SCCS/s.Makefile"
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

 
SRC_DIR = $(APS_HOME)/src/lib_APSpmfutils


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

LIB          =  libAPSpmfutils.a

SRC          =  APSpmfutilities.c

HEADER_FILES =  

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
	@$(CC) -o $(EXE) $(OBJS) $(LALL) $(LIBS)
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

APSpmfutilities.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
APSpmfutilities.o: /usr/include/stdlib.h /usr/include/time.h
APSpmfutilities.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
APSpmfutilities.o: /usr/include/sys/machtypes.h /usr/include/sys/stat.h
APSpmfutilities.o: /usr/include/sys/time.h /usr/include/string.h
APSpmfutilities.o: /home/aps/r2.1.1/include/global/GENconversions.h
APSpmfutilities.o: /home/aps/r2.1.1/include/global/fa_defs.h
APSpmfutilities.o: /home/aps/r2.1.1/include/global/dapps_list.h
APSpmfutilities.o: /home/aps/r2.1.1/include/global/llist_err.h
APSpmfutilities.o: /home/aps/r2.1.1/include/global/llist.h
APSpmfutilities.o: /home/aps/r2.1.1/include/global/ord_llist.h
APSpmfutilities.o: /home/aps/r2.1.1/include/local/APSpmfutilities.h
APSpmfutilities.o: /ASF/PRECM/include/faif/faifdefs.h /usr/include/syslog.h
APSpmfutilities.o: /usr/include/sys/syslog.h
APSpmfutilities.o: /home/aps/r2.1.1/include/local/APSodlcommonhdr.h
APSpmfutilities.o: /ASF/PRECM/include/sys/odldef.h
APSpmfutilities.o: /ASF/PRECM/include/sys/odlinter.h
