# @(#)makefile	5.8  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the FTS client applications.
#
# Author:       S. Hardman
#
# Date:         7/13/94
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IFTS =      -I../lincl
IV0 =       -I../../v0/lincl
IGLB =      -I../../gincl
IPUB =      -I../../pincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IFTS) $(IV0) $(IGLB) $(IPUB) $(IEXP) $(IASF) $(ISYB) \
						$(MACHIFLAGS)

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

FTSLIB =    -lfts
V0LIB =     -lv0
GLBLIB =    -lglb
IMSLIB =    -lims
ODLCLIB =   -lodlc 
ODLLIB =    -lodl
IKLIB =     -lik
IMSODL =    -limsodl
DBLIB =     -lsybdb
CTLIB =     -lct
CSLIB =     -lcs
TCLLIB =    -ltcl
COMNLIB =   -lcomn
BCPLIB =    -lblk

LIBS =      $(FTSLIB) $(V0LIB) $(GLBLIB) $(IMSLIB)  \
            $(IKLIB) $(IMSODL) $(IMSLIB) $(ODLLIB) \
						$(ODLCLIB) $(BCPLIB) $(DBLIB) $(MACHLIBS) -lm

PLIBS =     $(GLBLIB) $(IMSLIB) $(DBLIB) $(CTLIB) $(CSLIB) $(TCLLIB) \
						$(COMNLIB) $(MACHLIBS) -lintl -lm

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
            ../../../bin/$(MACH)/ims_DLtoDTKAux \
            ../../../bin/$(MACH)/ims_DLtoDTKEx \
            ../../../bin/$(MACH)/ims_askClnt \
            ../../../bin/$(MACH)/ims_ftsClnt \
            ../../../bin/$(MACH)/ims_downlinkAux \
            ../../../bin/$(MACH)/ims_ghaAux \
            ../../../bin/$(MACH)/ims_sequenceScan \
            ../../../bin/$(MACH)/ims_tapeScan \
            ../../../bin/$(MACH)/ims_svAux \
            ../../../bin/$(MACH)/ims_tapeAvailAux  \
            ../../../bin/$(MACH)/ims_tceAux 

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:	$(EXE)

../../../bin/$(MACH)/ims_DLtoDTKAux:  ims_DLtoDTKAux.o
	@echo
	@echo "Creating Executable ims_DLtoDTKAux"
	$(CC) -o $@ ims_DLtoDTKAux.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_DLtoDTKEx:  ims_DLtoDTKEx.o
	@echo
	@echo "Creating Executable ims_DLtoDTKEx"
	$(CC) -o $@ ims_DLtoDTKEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_askClnt:  ims_askClnt.o
	@echo
	@echo "Creating Executable ims_askClnt"
	$(CC) -o $@ ims_askClnt.o $(LALL) $(PLIBS)
	@echo

../../../bin/$(MACH)/ims_ftsClnt:  ims_ftsClnt.o
	@echo
	@echo "Creating Executable ims_ftsClnt"
	$(CC) -o $@ ims_ftsClnt.o $(LALL) $(PLIBS)
	@echo

../../../bin/$(MACH)/ims_downlinkAux:  ims_downlinkAux.o
	@echo
	@echo "Creating Executable ims_downlinkAux"
	$(CC) -o $@ ims_downlinkAux.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_ghaAux:  ims_ghaAux.o
	@echo
	@echo "Creating Executable ims_ghaAux"
	$(CC) -o $@ ims_ghaAux.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_sequenceScan:  ims_sequenceScan.o
	@echo
	@echo "Creating Executable ims_sequenceScan"
	$(CC) -o $@ ims_sequenceScan.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_tapeScan:  ims_tapeScan.o
	@echo
	@echo "Creating Executable ims_tapeScan"
	$(CC) -o $@ ims_tapeScan.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_svAux:  ims_svAux.o
	@echo
	@echo "Creating Executable ims_svAux"
	$(CC) -o $@ ims_svAux.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_tapeAvailAux:  ims_tapeAvailAux.o
	@echo
	@echo "Creating Executable ims_tapeAvailAux"
	$(CC) -o $@ ims_tapeAvailAux.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_tceAux:  ims_tceAux.o
	@echo
	@echo "Creating Executable ims_tceAux"
	$(CC) -o $@ ims_tceAux.o $(LALL) $(LIBS)
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
	@echo "Cleaning FTS client executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing FTS client executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
