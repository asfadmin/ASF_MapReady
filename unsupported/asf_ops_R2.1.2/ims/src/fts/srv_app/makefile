# @(#)makefile	5.4  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the ims_ftsSrv application.
#
# Author:       S. Hardman
#
# Date:         6/16/94
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IFTS =      -I../lincl
IGLB =      -I../../gincl
IPUB =      -I../../pincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IFTS) $(IGLB) $(IPUB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

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
IKLIB =     -lik
ODLLIB =    -lodlc
SRVLIB =    -lsrv
DBLIB =     -lsybdb
CSLIB =     -lcs
TCLLIB =    -ltcl
COMNLIB =   -lcomn

LIBS =      $(FTSLIB) $(V0LIB) $(GLBLIB) $(IMSLIB) $(IKLIB) $(ODLLIB) \
            $(SRVLIB) $(DBLIB) $(CSLIB) $(TCLLIB) $(COMNLIB) \
            $(MACHLIBS) -lintl -lm

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

SRC =       ims_ftsSrv.c
OBJ =       $(SRC:%.c=%.o)
EXE =       ../../../bin/$(MACH)/ims_ftsSrv

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

../../../bin/$(MACH)/ims_ftsSrv:  $(OBJ) ims_ftsSrv.o
	@echo
	@echo "Creating Executable ims_ftsSrv"
	$(CC) -o $@ $(OBJ) $(LALL) $(LIBS)
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
	@echo "Cleaning FTS server executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing FTS server executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
