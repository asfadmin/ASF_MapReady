# @(#)makefile	1.7  10/27/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the access gui applications.
#
# Author:       S. Hardman
#
# Date:         5/2/96
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IACCESS =   -I../lincl
IGLB =      -I../../gincl
IPUB =      -I../../pincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IACCESS) $(IGLB) $(IPUB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

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

GLBLIB =    -lglb
IMSLIB =    -lims
DBLIB =     -lsybdb
ODLLIB =    -lodl
IKLIB =     -lik
HDFLIB =    -ldf
XLIBS =     -lXm -lXt -lX11

LIBS =      $(GLBLIB) $(IMSLIB) $(DBLIB) $(ODLLIB) $(IKLIB) \
						$(HDFLIB) $(XLIBS) $(MACHLIBS) -lm

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      -D_NO_PROTO -DSUN -DGUI
CFLAGS =    $(MACHCFLAGS) $(DEFS)

###############################################################################
#
# Target Definition
#
###############################################################################

SRC =       \
            ims_access.c

OBJ =       $(SRC:%.c=%.o)
EXE =       ../../../bin/$(MACH)/ims_access

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

$(EXE):  $(OBJ)
	@echo
	@echo "Creating Executable ims_access"
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
	@echo "Cleaning Access gui executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing Access gui executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
