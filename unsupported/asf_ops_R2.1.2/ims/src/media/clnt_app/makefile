# @(#)makefile	5.4  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the Media client applications.
#
# Author:       S. Hardman
#
# Date:         7/8/95
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IMED =      -I../lincl
IGLB =      -I../../gincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IMED) $(IGLB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

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

MEDLIB =    -lmed
GLBLIB =    -lglb
IMSLIB =    -lims
DBLIB =     -lsybdb

LIBS =      $(MEDLIB) $(GLBLIB) $(IMSLIB) $(DBLIB) $(MACHLIBS) -lm

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
            ../../../bin/$(MACH)/ims_deviceUtil \
            ../../../bin/$(MACH)/ims_filmListGen \
            ../../../bin/$(MACH)/ims_filmStatusRet \
            ../../../bin/$(MACH)/ims_ftp \
            ../../../bin/$(MACH)/ims_getFile \
            ../../../bin/$(MACH)/ims_mediaClnt \
            ../../../bin/$(MACH)/ims_mediaJob \
            ../../../bin/$(MACH)/ims_qualCheck

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:	$(EXE)

../../../bin/$(MACH)/ims_deviceUtil:  ims_deviceUtil.o
	@echo
	@echo "Creating Executable ims_deviceUtil"
	$(CC) -o $@ ims_deviceUtil.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_filmListGen:  ims_filmListGen.o
	@echo
	@echo "Creating Executable ims_filmListGen"
	$(CC) -o $@ ims_filmListGen.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_filmStatusRet:  ims_filmStatusRet.o
	@echo
	@echo "Creating Executable ims_filmStatusRet"
	$(CC) -o $@ ims_filmStatusRet.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_ftp:  ims_ftp.o
	@echo
	@echo "Creating Executable ims_ftp"
	$(CC) -o $@ ims_ftp.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_getFile:  ims_getFile.o
	@echo
	@echo "Creating Executable ims_getFile"
	$(CC) -o $@ ims_getFile.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_mediaJob:  ims_mediaJob.o
	@echo
	@echo "Creating Executable ims_mediaJob"
	$(CC) -o $@ ims_mediaJob.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_mediaClnt:  ims_mediaClnt.o
	@echo
	@echo "Creating Executable ims_mediaClnt"
	$(CC) -o $@ ims_mediaClnt.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_qualCheck:  ims_qualCheck.o
	@echo
	@echo "Creating Executable ims_qualCheck"
	$(CC) -o $@ ims_qualCheck.o $(LALL) $(LIBS)
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
	@echo "Cleaning Media client executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing Media client executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
