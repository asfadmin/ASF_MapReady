# @(#)makefile	5.5 10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the ims_disp application.
#
# Author:       S. Hardman
#
# Date:         3/7/96
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IDISP =     -I../lincl
IV0 =       -I../../v0/lincl
IGLB =      -I../../gincl
IPUB =      -I../../pincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include
IPPS =      -I$(ASFINC)/pps

IALL =      $(IDISP) $(IV0) $(IGLB) $(IPUB) $(IEXP) $(IASF) $(ISYB) $(IPPS) \
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

DISPLIB =   -ldisp
V0LIB =     -lv0
GLBLIB =    -lglb
IMSLIB =    -lims
IKLIB =     -lik
ODLLIB =    -lodlc
DBLIB =     -lsybdb
PPSLIB =    -ldce -lthread -lpps_client

#LIBS =      $(DISPLIB) $(V0LIB) $(GLBLIB) $(IMSLIB) $(IKLIB) $(ODLLIB) \
            $(DBLIB) $(MACHLIBS) -lm

LIBS =      $(DISPLIB) $(V0LIB) $(GLBLIB) $(IMSLIB) $(IKLIB) $(ODLLIB) \
            $(DBLIB) $(PPSLIB) $(MACHLIBS) -lm

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      -DEOSDIS -DDAAC_SERVER
CFLAGS =    $(MACHCFLAGS) $(DEFS)

###############################################################################
#
# Source and Target Definition
#
###############################################################################

SRC =       ims_disp.c
OBJ =       $(SRC:%.c=%.o)
EXE =       ../../../bin/$(MACH)/ims_disp 

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

../../../bin/$(MACH)/ims_disp:  $(OBJ) ims_disp.o
	@echo
	@echo "Creating Executable ims_disp"
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
	@echo "Cleaning Dispatcher server executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing Dispatcher server executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
