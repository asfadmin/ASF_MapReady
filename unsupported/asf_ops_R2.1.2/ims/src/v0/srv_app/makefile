# @(#)makefile	5.6  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the ims_v0Srv application.
#
# Author:       S. Hardman
#
# Date:         11/18/94
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IV0 =       -I../lincl
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
IKLIB =     -lik
ODLLIB =    -lodlc
DBLIB =     -lsybdb

LIBS =      $(V0LIB) $(GLBLIB) $(IMSLIB) $(IKLIB) $(ODLLIB) $(DBLIB) \
            $(MACHLIBS) -lm

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      -DSUN_UNIX -DEOSDIS -DDAAC_SERVER -DMDEBUG
CFLAGS =    $(MACHCFLAGS) $(DEFS)

###############################################################################
#
# Source and Target Definition
#
###############################################################################

SRC =       ims_v0Srv.c
OBJ =       $(SRC:%.c=%.o)
EXE =       ../../../bin/$(MACH)/ims_v0Srv

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

../../../bin/$(MACH)/ims_v0Srv:  $(OBJ) ims_v0Srv.o
	@echo
	@echo "Creating Executable ims_v0Srv"
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
	@echo "Cleaning V0 server executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing V0 server executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
