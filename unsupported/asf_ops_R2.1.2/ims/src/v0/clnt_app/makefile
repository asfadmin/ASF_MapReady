# @(#)makefile	5.10  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the v0 client applications.
#
# Author:       S. Hardman
#
# Date:         8/26/95
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
IMSODLLIB = -limsodl
DBLIB =     -lsybdb
ODLCLIB =   -lodlc
ODLLIB =    -lodl
IKLIB =     -lik

LIBS =      $(V0LIB) $(GLBLIB) $(IMSLIB) $(DBLIB) $(IKLIB) $(ODLCLIB) \
            $(MACHLIBS) -lm

OLIBS =     $(V0LIB) $(GLBLIB) $(IMSLIB) $(IMSODLLIB) $(DBLIB) $(IKLIB) \
						$(ODLLIB) $(MACHLIBS) -lm

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

SRC =       *.c
OBJ =       $(SRC:%.c=%.o)
EXE =       \
            ../../../bin/$(MACH)/ims_URF2DAR \
            ../../../bin/$(MACH)/ims_cryptKey \
            ../../../bin/$(MACH)/ims_orderClnt \
            ../../../bin/$(MACH)/ims_reorder \
            ../../../bin/$(MACH)/ims_package \
            ../../../bin/$(MACH)/ims_scanOrder \
            ../../../bin/$(MACH)/ims_valids \
            ../../../bin/$(MACH)/v0_client

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

../../../bin/$(MACH)/ims_URF2DAR:  ims_URF2DAR.o
	@echo
	@echo "Creating Executable ims_URF2DAR"
	$(CC) -o $@ ims_URF2DAR.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_cryptKey:  ims_cryptKey.o
	@echo
	@echo "Creating Executable ims_cryptKey"
	$(CC) -o $@ ims_cryptKey.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_orderClnt:  ims_orderClnt.o
	@echo
	@echo "Creating Executable ims_orderClnt"
	$(CC) -o $@ ims_orderClnt.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_reorder:  ims_reorder.o
	@echo
	@echo "Creating Executable ims_reorder"
	$(CC) -o $@ ims_reorder.o $(LALL) $(OLIBS)
	@echo

../../../bin/$(MACH)/ims_package:  ims_package.o
	@echo
	@echo "Creating Executable ims_package"
	$(CC) -o $@ ims_package.o $(LALL) $(GLBLIB) $(IMSODLLIB) $(IMSLIB) \
	$(DBLIB) $(ODLLIB) $(IKLIB) $(MACHLIBS) -lm
	@echo

../../../bin/$(MACH)/ims_scanOrder:  ims_scanOrder.o
	@echo
	@echo "Creating Executable ims_scanOrder"
	$(CC) -o $@ ims_scanOrder.o $(LALL) $(OLIBS)
	@echo

../../../bin/$(MACH)/ims_valids:  ims_valids.o
	@echo
	@echo "Creating Executable ims_valids"
	$(CC) -o $@ ims_valids.o $(LALL) $(GLBLIB) $(IMSODLLIB) $(IMSLIB) \
	$(DBLIB) $(ODLLIB) $(IKLIB) $(MACHLIBS) -lm
	@echo

../../../bin/$(MACH)/v0_client:  v0_client.o
	@echo
	@echo "Creating Executable v0_client"
	$(CC) -o $@ v0_client.o $(LALL) $(LIBS)
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
	@echo "Cleaning V0 client executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing V0 client executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
