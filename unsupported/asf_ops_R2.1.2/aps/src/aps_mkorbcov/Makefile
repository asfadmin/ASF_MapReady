


###############################################################################
#
# File:         cdtkopps/Makefile
#
# Function:     APS Makefile for aps_create_dtk_opps
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
# SCCS Info:
#   "@(#)Makefile	5.1 98/01/08 APS/ASF"
#   "@(#) /home/aps/r2.1.2/src/aps_mkorbcov/SCCS/s.Makefile"
#
###############################################################################

include ../aps_makedefs


###############################################################################
#
# Source Default Directory
#   Typically off of $APS_HOME/src... this is used to retrieve source
#
###############################################################################
 
SRC_DIR = $(APS_HOME)/src/mkorbcov
 

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
			$(solarisLFLAGS)

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

LIBS = $(MATH_LIB) $(OSLIBS) -lV77

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

EXE          =  aps_mkorbcov

LIB          =  

SRC          =  eosmain.c 

HEADER_FILES =  

F77_SRC   =  eosmenu.for annot.for annotps.for annotram.for arc.for   \
   arcsphere.for askerase.for \
   boxn.for checkscreen.for clear.for cropmarks.for cross.for crossscale.for \
   dateline.for daytime.for degmin2deg.for diffang.for drawbox.for drawmap.for \
   drawmask.for drawtargets.for drawtext.for dtwixt.for ellipse.for eossub.for \
   ephem2.for error.for fillit.for findbx.for findkeys.for findq.for flyit.for \
   flyitsimul.for framewindow.for getcol.for getcolor.for getelems1.for    \
   getinstindex.for getinstnum.for getplatnum.for getproj.for glintang.for    \
   grafcomment.for hardcopy.for ijktosez.for initbody.for initdisp.for    \
   initepoch.for initfiles.for initijktosez.for initmask.for initpen.for    \
   initps.for inmask.for kepler.for latitude.for line.for listinst.for    \
   localtimetonode.for logtonum.for mapgrid.for mapticks.for matrices.for    \
   menucbody.for menudisplay.for menuepoch.for menufly.for menuinput.for    \
   menuinst.for menumap.for menumask.for menuorbit.for menupen.for    \
   menuplat.for menusimul.for menuspecial.for nodeperiod.for    \
   nospaces.for oblate.for pagesetup.for parsetabs.for pointaxes.for    \
   presetupinst.for printram.for printronix.for printvt.for project.for    \
   qtohss.for ramtext.for readelements.for readinstfile.for readinstline.for  \
   readplatfile.for readplatline.for readtargets.for rnadir.for    \
   rotation.for rtwixt.for setcol.for setline.for setproj.for    \
   setupinst.for showdisp.for shownames.for showpen.for showproj.for    \
   simul.for spherexyz.for stringlen.for strings.for subsolar.for    \
   swath.for swathglint.for swathhotspot.for swathlimb2.for    \
   sysvms.for target.for tek.for time.for timeflight.for tomerc.for    \
   tomill.for twixt.for unproject.for usecol.for vector.for vectors.for    \
   viewpoint.for waitkey.for xyzsphere.for

F77_INC      =  cbody.inc constants.inc display.inc eossimul.inc     \
                files.inc instruments.inc map.inc orbit.inc pen.inc     \
                simul.inc station.inc time.inc

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

clean: clean_version
	@echo
	@echo "Cleaning " $(EXE) $(LIB)
	rm -f $(OBJS) $(EXE) $(LIB)
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
#   Add post make definitions
#
###############################################################################
include ../aps_post_makedefs


###############################################################################
#
# Makedepend (create all the include file dependencies
#	(destroys everything after the "DO NOT DELETE ..." line)
#
###############################################################################
depend: $(SRC)
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

eosmain.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
eosmain.o: /usr/include/stdio.h /usr/include/string.h
