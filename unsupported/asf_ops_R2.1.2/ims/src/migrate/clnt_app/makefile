# @(#)makefile	1.7  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the Migrate client applications.
#
# Author:       S. Hardman
#
# Date:         4/21/97
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IV0 =       -I../../v0/lincl
IGLB =      -I../../gincl
IPUB =      -I../../pincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IV0) $(IGLB) $(IPUB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

###############################################################################
#
# Library Path Definitions
#
###############################################################################

LIMS =      -L../../../lib/$(MACH)/local
LEXP =      -L../../../lib/$(MACH)/global
LSYB =      -L$(SYBASE)/lib

LALL =      $(LIMS) $(LEXP) $(LASF) $(LSYB) $(MACHLFLAGS)

###############################################################################
#
# Library Definitions
#
###############################################################################

V0LIB =     -lv0
GLBLIB =    -lglb
IMSLIB =    -lims
IMSODLLIB = -limsodl
DBLIB =     -lsybdb
ODLCLIB =   -lodlc
ODLLIB =    -lodl
IKLIB =     -lik
APSLIB = 		-lapsdb -lsybint

LIBS =      $(V0LIB) $(GLBLIB) $(IMSLIB) $(IMSODLLIB) $(DBLIB) $(IKLIB) \
					  $(ODLLIB) $(MACHLIBS) $(APSLIB) -lm 

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =
CFLAGS =    $(MACHCFLAGS) $(DEFS)

###############################################################################
#
# Source and Target Definition
#
###############################################################################

SRC =       *.c
OBJ =       $(SRC:%.c=%.o)
EXE =       \
            ../../../bin/$(MACH)/ims_pmfUpdate \
            ../../../bin/$(MACH)/ims_sequenceFix \
            ../../../bin/$(MACH)/ims_sequenceCheck \
            ../../../bin/$(MACH)/ims_dsFileNameUpdate \
            ../../../bin/$(MACH)/ims_srFileNameUpdate \
            ../../../bin/$(MACH)/ims_spatialUpdate \
            ../../../bin/$(MACH)/ims_scanFilesUpdate \
            ../../../bin/$(MACH)/ims_scanOrderUpdate \
            ../../../bin/$(MACH)/ims_scanOrderUpdate2 \
            ../../../bin/$(MACH)/ims_mapStatusUpdate \
            ../../../bin/$(MACH)/ims_mapStatusUpdate2 \
            ../../../bin/$(MACH)/ims_mapCreateUpdate \
            ../../../bin/$(MACH)/ims_mapCreateUpdate2 \
            ../../../bin/$(MACH)/ims_mapMessageUpdate \
            ../../../bin/$(MACH)/ims_mapMessageUpdate2 \
            ../../../bin/$(MACH)/ims_sensorChangeUpdate \
            ../../../bin/$(MACH)/ims_apsChangeUpdate

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

../../../bin/$(MACH)/ims_pmfUpdate:  ims_pmfUpdate.o
	@echo
	@echo "Creating Executable ims_pmfUpdate"
	$(CC) -o $@ ims_pmfUpdate.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_sequenceFix:  ims_sequenceFix.o
	@echo
	@echo "Creating Executable ims_sequenceFix"
	$(CC) -o $@ ims_sequenceFix.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_sequenceCheck:  ims_sequenceCheck.o
	@echo
	@echo "Creating Executable ims_sequenceCheck"
	$(CC) -o $@ ims_sequenceCheck.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_dsFileNameUpdate:  ims_dsFileNameUpdate.o
	@echo
	@echo "Creating Executable ims_dsFileNameUpdate"
	$(CC) -o $@ ims_dsFileNameUpdate.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_srFileNameUpdate:  ims_srFileNameUpdate.o
	@echo
	@echo "Creating Executable ims_srFileNameUpdate"
	$(CC) -o $@ ims_srFileNameUpdate.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_spatialUpdate:  ims_spatialUpdate.o
	@echo
	@echo "Creating Executable ims_spatialUpdate"
	$(CC) -o $@ ims_spatialUpdate.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_scanFilesUpdate:  ims_scanFilesUpdate.o
	@echo
	@echo "Creating Executable ims_scanFilesUpdate"
	$(CC) -o $@ ims_scanFilesUpdate.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_scanOrderUpdate:  ims_scanOrderUpdate.o
	@echo
	@echo "Creating Executable ims_scanOrderUpdate"
	$(CC) -o $@ ims_scanOrderUpdate.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_scanOrderUpdate2:  ims_scanOrderUpdate2.o
	@echo
	@echo "Creating Executable ims_scanOrderUpdate2"
	$(CC) -o $@ ims_scanOrderUpdate2.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_mapStatusUpdate:  ims_mapStatusUpdate.o
	@echo
	@echo "Creating Executable ims_mapStatusUpdate"
	$(CC) -o $@ ims_mapStatusUpdate.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_mapStatusUpdate2:  ims_mapStatusUpdate2.o
	@echo
	@echo "Creating Executable ims_mapStatusUpdate2"
	$(CC) -o $@ ims_mapStatusUpdate2.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_mapCreateUpdate:  ims_mapCreateUpdate.o
	@echo
	@echo "Creating Executable ims_mapCreateUpdate"
	$(CC) -o $@ ims_mapCreateUpdate.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_mapCreateUpdate2:  ims_mapCreateUpdate2.o
	@echo
	@echo "Creating Executable ims_mapCreateUpdate2"
	$(CC) -o $@ ims_mapCreateUpdate2.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_mapMessageUpdate:  ims_mapMessageUpdate.o
	@echo
	@echo "Creating Executable ims_mapMessageUpdate"
	$(CC) -o $@ ims_mapMessageUpdate.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_mapMessageUpdate2:  ims_mapMessageUpdate2.o
	@echo
	@echo "Creating Executable ims_mapMessageUpdate2"
	$(CC) -o $@ ims_mapMessageUpdate2.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_sensorChangeUpdate:  ims_sensorChangeUpdate.o
	@echo
	@echo "Creating Executable ims_sensorChangeUpdate"
	$(CC) -o $@ ims_sensorChangeUpdate.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_apsChangeUpdate:  ims_apsChangeUpdate.o
	@echo
	@echo "Creating Executable ims_apsChangeUpdate"
	$(CC) -o $@ ims_apsChangeUpdate.o $(LALL) $(LIBS)
	@echo

%.o:%.c
	@echo
	@echo "Compiling $<"
	$(CC) $(CFLAGS) $(IALL) -c $<

###############################################################################
#
# Object and executable cleaning.
#
###############################################################################

clean:
	@echo
	@echo "Cleaning Migrate client executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing Migrate client executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
