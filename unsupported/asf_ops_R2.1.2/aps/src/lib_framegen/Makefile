###############################################################################
#
# File:         Makefile
#
# Function:     Makefile for src/lib_framegen
#
# Author:       Lawrence Stevens from Ron Green (Inspired by S. Hardman)
#
# Date:         Fri Jan 12 10:44:44 PST 1996 
#
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/lib_framegen/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/lib_framegen
 
###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL = \
        $(IPPS) \
        $(IFAIF) \
        $(IODL) \
		$(IIMS) \
        $(IAPS) \
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
ifeq ($(MACH), sgi)
	DEFS := $(DEFS) -DSGI
endif

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

LIB   		 = 

GLOBAL_LIB   =  libfg.a

SRC          =  

SRC          =  darframe.c     \
				fgtime.c     \
				framegen.c     \
				genpmf.c     \
				get_orbit0.c

HEADER_FILES =  framegen.h

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
	@$(CC) $(CFLAGS) -D_XOPEN_SOURCE $(IALL) -c $<


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

darframe.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
darframe.o: /usr/include/string.h /ASF/PRECM/include/imsdads/ims_query.h
darframe.o: /ASF/PRECM/include/imsdads/ims_dbms.h
darframe.o: /home/sybase/sol/include/sybfront.h
darframe.o: /home/sybase/sol/include/cstypes.h
darframe.o: /home/sybase/sol/include/csconfig.h
darframe.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
darframe.o: /home/sybase/sol/include/ctpublic.h
darframe.o: /home/sybase/sol/include/cspublic.h
darframe.o: /home/sybase/sol/include/sqlca.h /home/sybase/sol/include/sybdb.h
darframe.o: /home/sybase/sol/include/syblogin.h
darframe.o: /ASF/PRECM/include/imsdads/ims_const.h
darframe.o: /ASF/PRECM/include/imsdads/ims_msg.h
darframe.o: /ASF/PRECM/include/imsdads/ims_qi.h
darframe.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
darframe.o: /ASF/PRECM/include/sys/framegen.h /usr/include/stdlib.h
darframe.o: /usr/include/time.h
fgtime.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
fgtime.o: /usr/include/limits.h /usr/include/math.h /usr/include/time.h
fgtime.o: /ASF/PRECM/include/sys/framegen.h /usr/include/stdlib.h
framegen.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
framegen.o: /usr/include/stdlib.h /usr/include/string.h /usr/include/math.h
framegen.o: /ASF/PRECM/include/imsdads/ims_query.h
framegen.o: /ASF/PRECM/include/imsdads/ims_dbms.h
framegen.o: /home/sybase/sol/include/sybfront.h
framegen.o: /home/sybase/sol/include/cstypes.h
framegen.o: /home/sybase/sol/include/csconfig.h
framegen.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
framegen.o: /home/sybase/sol/include/ctpublic.h
framegen.o: /home/sybase/sol/include/cspublic.h
framegen.o: /home/sybase/sol/include/sqlca.h /home/sybase/sol/include/sybdb.h
framegen.o: /home/sybase/sol/include/syblogin.h
framegen.o: /ASF/PRECM/include/imsdads/ims_const.h
framegen.o: /ASF/PRECM/include/imsdads/ims_msg.h
framegen.o: /ASF/PRECM/include/imsdads/ims_qi.h
framegen.o: /ASF/PRECM/include/imsdads/ims_archive.h
framegen.o: /ASF/PRECM/include/imsdads/ims_cmnQuery.h
framegen.o: /ASF/PRECM/include/sys/framegen.h /usr/include/time.h
genpmf.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
genpmf.o: /usr/include/stdlib.h /usr/include/time.h /usr/include/math.h
genpmf.o: /ASF/PRECM/include/sys/framegen.h
get_orbit0.o: /usr/include/string.h /usr/include/sys/feature_tests.h
get_orbit0.o: /usr/include/stdio.h /usr/include/stdlib.h
get_orbit0.o: /ASF/PRECM/include/sys/framegen.h /usr/include/time.h
