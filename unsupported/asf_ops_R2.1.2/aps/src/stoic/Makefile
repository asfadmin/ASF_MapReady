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
#	"@(#) /home/aps/r2.1.2/src/stoic/SCCS/s.Makefile"
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

SRC_DIR = $(APS_HOME)/src/stoic


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

GHA_TODAY_EXE	=	gha_today.exe
GHA_UPDATE_EXE 	=	gha_update.exe
STOIC_META_EXE 	=	stoicfile_metadata.exe
EXE				=	$(GHA_TODAY_EXE) $(GHA_UPDATE_EXE) $(STOIC_META_EXE)

LIB				=

GHA_TODAY_MAIN	=	gha_today.for
GHA_UPDATE_MAIN	=	gha_update.for
STOIC_META_MAIN	=	stoicfile_metadata.c

GHA_TODAY_SRC	=	$(GHA_TODAY_MAIN) \
					aps2ims_archive.c \
					asf2et.c \
					create_metadata_IMS.c \
					gha_append.for \
					mps2gha.for \
					et2ut1.for \
					etrans.for \
					gcbomb.for
GHA_UPDATE_SRC	=	$(GHA_UPDATE_MAIN) \
					aps2ims_archive.c \
					asf2et.c \
					create_metadata_IMS.c \
					gha_append.for \
					mps2gha.for \
					et2ut1.for \
					etrans.for \
					gcbomb.for
STOIC_META_SRC	=	$(STOIC_META_MAIN) \
					aps2ims_archive.c \
					asf2et.c \
					create_metadata_IMS.c \
					et2ut1.for \
					etrans.for \
					gcbomb.for

ALL_SRC			:=	$(GHA_TODAY_MAIN) \
					$(GHA_UPDATE_MAIN) \
					$(STOIC_META_MAIN) \
					$(sort $(filter-out $(GHA_TODAY_MAIN), $(GHA_TODAY_SRC)) \
						$(filter-out $(GHA_UPDATE_MAIN), $(GHA_UPDATE_SRC)) \
						$(filter-out $(STOIC_META_SRC), $(STOIC_META_SRC)))

SRC				:=	$(filter %.c, $(ALL_SRC))

F77_SRC			:=	$(filter %.for, $(ALL_SRC))

HEADER_FILES	=

F77_INC			=

GHA_TODAY_OBJS	:=	$(patsubst %.c,%.o,$(filter %.c, $(GHA_TODAY_SRC))) \
					$(patsubst %.for,%.o,$(filter %.for, $(GHA_TODAY_SRC)))
GHA_UPDATE_OBJS	:=	$(patsubst %.c,%.o,$(filter %.c, $(GHA_UPDATE_SRC))) \
					$(patsubst %.for,%.o,$(filter %.for, $(GHA_UPDATE_SRC)))
STOIC_META_OBJS	:=	$(patsubst %.c,%.o,$(filter %.c, $(STOIC_META_SRC))) \
					$(patsubst %.for,%.o,$(filter %.for, $(STOIC_META_SRC)))

OBJS			:=	$(sort $(GHA_TODAY_OBJS) \
						$(GHA_UPDATE_OBJS) \
						$(STOIC_META_OBJS))

TARGETS			=	$(GHA_TODAY_EXE) $(GHA_UPDATE_EXE) $(STOIC_META_EXE) $(LIB)


###############################################################################
#
# Make the Application
#    Add all targets to be made to the next line
#    If more exes or libs are needed make a copy of the set of
#    needed commands
#
###############################################################################

all appl:  $(TARGETS)

$(STOIC_META_EXE): $(STOIC_META_OBJS)
	@echo
	@echo "Linking $(STOIC_META_EXE) "
	@$(CC) -o $(STOIC_META_EXE) $(STOIC_META_OBJS) $(LALL) $(RALL) $(LIBS)
	@echo "Done"

$(GHA_TODAY_EXE): $(GHA_TODAY_OBJS)
	@echo
	@echo "Linking $(GHA_TODAY_EXE) "
	@$(CC) -o $(GHA_TODAY_EXE) $(GHA_TODAY_OBJS) $(LALL) $(RALL) $(LIBS)
	@echo "Done"

$(GHA_UPDATE_EXE): $(GHA_UPDATE_OBJS)
	@echo
	@echo "Linking $(GHA_UPDATE_EXE) "
	@$(CC) -o $(GHA_UPDATE_EXE) $(GHA_UPDATE_OBJS) $(LALL) $(RALL) $(LIBS)
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

$(SRC) $(F77_SRC) $(HEADER_FILES) $(F77_INC) :
	@echo
	@echo "Retrieving $@ as of $(FILE_VERSION) "
	$(CM) $(CM_GET) $(FILE_VERSION) $(SRC_DIR)/$@



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
	$(LINT) $(IALL) $(STOIC_META_MAIN)


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
depend: $(ALL_SRC)
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

stoicfile_metadata.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
stoicfile_metadata.o: /usr/include/string.h /usr/include/stdlib.h
stoicfile_metadata.o: /usr/include/time.h /usr/include/errno.h
stoicfile_metadata.o: /usr/include/sys/errno.h
aps2ims_archive.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
aps2ims_archive.o: /usr/include/stdlib.h /usr/include/string.h
aps2ims_archive.o: /ASF/PRECM/include/imsdads/ims_query.h
aps2ims_archive.o: /ASF/PRECM/include/imsdads/ims_dbms.h
aps2ims_archive.o: /home/sybase/sol/include/sybfront.h
aps2ims_archive.o: /home/sybase/sol/include/cstypes.h
aps2ims_archive.o: /home/sybase/sol/include/csconfig.h
aps2ims_archive.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
aps2ims_archive.o: /home/sybase/sol/include/ctpublic.h
aps2ims_archive.o: /home/sybase/sol/include/cspublic.h
aps2ims_archive.o: /home/sybase/sol/include/sqlca.h
aps2ims_archive.o: /home/sybase/sol/include/sybdb.h
aps2ims_archive.o: /home/sybase/sol/include/syblogin.h
aps2ims_archive.o: /ASF/PRECM/include/imsdads/ims_const.h
aps2ims_archive.o: /ASF/PRECM/include/imsdads/ims_msg.h
aps2ims_archive.o: /ASF/PRECM/include/imsdads/ims_qi.h
aps2ims_archive.o: /ASF/PRECM/include/imsdads/ims_archive.h
asf2et.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
asf2et.o: /usr/include/string.h /usr/include/stdlib.h /usr/include/time.h
asf2et.o: /usr/include/errno.h /usr/include/sys/errno.h
asf2et.o: /home/aps/r2.1.1/include/global/dapps_defs.h
asf2et.o: /home/aps/r2.1.1/include/local/timeconv.h
create_metadata_IMS.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
create_metadata_IMS.o: /usr/include/string.h /usr/include/stdlib.h
create_metadata_IMS.o: /usr/include/time.h /usr/include/errno.h
create_metadata_IMS.o: /usr/include/sys/errno.h
create_metadata_IMS.o: /home/aps/r2.1.1/include/local/timeconv.h
