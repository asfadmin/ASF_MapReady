# @(#)makefile	1.4  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the utility GUI applications.
#
# Author:       S. Hardman
#
# Date:         9/28/94
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IUTIL =     -I../lincl
IGLB =      -I../../gincl
IPUB =      -I../../pincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IUTIL) $(IGLB) $(IPUB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

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
CTLIB =     -lct
CSLIB =     -lcs
TCLLIB =    -ltcl
XLIBS =     -lXm -lXt -lX11 -lgen
ODLLIB =    -limsodl -lodl
IKLIB =     -lik
COMNLIB =   -lcomn


LIBS =      $(GLBLIB) $(IMSLIB) $(DBLIB) $(CTLIB) $(CSLIB) $(TCLLIB) \
            $(COMNLIB) $(XLIBS) $(ODLLIB) $(IKLIB) $(MACHLIBS) -lintl -lm

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      
CFLAGS =    $(MACHCFLAGS) $(DEFS)

###############################################################################
#
# Target Definition
#
###############################################################################

SRC =       *.c
OBJ =       $(SRC:%.c=%.o)
EXE =       \
            ../../../bin/$(MACH)/ims_pmfTool

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

../../../bin/$(MACH)/ims_pmfTool:  ims_pmfTool.o ims_otWidgets.o ims_otODL.o \
								ims_otIngest.o
	@echo
	@echo "Creating Executable ims_pmfTool"
	$(CC) -o $@ ims_pmfTool.o  ims_otWidgets.o ims_otODL.o ims_otIngest.o $(LALL) $(LIBS)
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
	@echo "Cleaning Utility GUI executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing Utility GUI executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
