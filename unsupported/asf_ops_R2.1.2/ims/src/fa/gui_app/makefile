# @(#)makefile	1.5 10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the FA GUI applications.
#
# Author:       S. Hardman
#
# Date:         9/26/96
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IFA =       -I../lincl
IGLB =      -I../../gincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IFA) $(IGLB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

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

FALIB =     -lims_fa
MATHLIB = 	-lims_math
GLBLIB =    -lglb
IMSLIB =    -lims
CSALIB =    -lcsap
DBLIB =     -lsybdb
CTLIB =     -lct
CSLIB =     -lcs
TCLLIB =    -ltcl
XLIBS =     -lXm -lXt -lX11 -lgen
ODLLIB =    -limsodl  -lodl
IKLIB =     -lik
COMNLIB =   -lcomn


LIBS =      $(FALIB) $(GLBLIB) $(IMSLIB) $(CSALIB) $(DBLIB) \
						$(CTLIB) $(CSLIB) $(TCLLIB) $(COMNLIB) $(MATHLIB) \
						$(XLIBS) $(ODLLIB) \
						$(IKLIB) $(MACHLIBS) -lintl -lm

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
            ../../../bin/$(MACH)/ims_fv \
            ../../../bin/$(MACH)/ims_reporter 

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

../../../bin/$(MACH)/ims_fv:  ims_fv.o
	@echo
	@echo "Creating Executable ims_fv"
	$(CC) -o $@ ims_fv.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_reporter:  ims_reporter.o
	@echo
	@echo "Creating Executable ims_reporter"
	$(CC) -o $@ ims_reporter.o $(LALL) $(LIBS)
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
	@echo "Cleaning FA GUI executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing FA GUI executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
