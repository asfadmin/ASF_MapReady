# @(#)makefile	5.6 10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the FA applications.
#
# Author:       S. Hardman, D. Crichton
#
# Date:         2/15/96
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
IPUB =      -I../../pincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IFA) $(IGLB) $(IPUB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

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
ODLLIB =    -limsodl  -lodl
IKLIB =     -lik
COMNLIB =   -lcomn


LIBS =      $(FALIB) $(GLBLIB) $(IMSLIB) $(CSALIB) $(DBLIB) $(CTLIB) \
            $(CSLIB) $(TCLLIB) $(COMNLIB) $(MATHLIB) $(ODLLIB) \
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
            ../../../bin/$(MACH)/ims_fa_reader \
            ../../../bin/$(MACH)/ims_FA_task \
            ../../../bin/$(MACH)/ims_fa_scheduler \
            ../../../bin/$(MACH)/ims_passlog_reader

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

../../../bin/$(MACH)/ims_fa_reader:  ims_fa_reader.o 
	@echo
	@echo "Creating Executable ims_fa_reader"
	$(CC) -o $@ ims_fa_reader.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_FA_task:  ims_FA_task.o
	@echo
	@echo "Creating Executable ims_FA_task"
	$(CC) -o $@ ims_FA_task.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_fa_scheduler:  ims_fa_scheduler.o
	@echo
	@echo "Creating Executable ims_fa_scheduler"
	$(CC) -o $@ ims_fa_scheduler.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_passlog_reader:  ims_passlog_reader.o
	@echo
	@echo "Creating Executable ims_passlog_reader"
	$(CC) -o $@ ims_passlog_reader.o $(LALL) $(LIBS)
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
	@echo "Cleaning FA client executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing FA client executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
