# @(#)makefile	5.3  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the libglb.a library.
#
# Author:       S. Hardman
#
# Date:         10/18/94
#
###############################################################################

include ../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IGLB =      -I../gincl
IEXP =      -I../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IGLB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

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

SRC = \
            ims_childHandler.c \
            ims_cmnAux.c \
            ims_confSubs.c \
            ims_getCmd.c \
            ims_getCmdLn.c \
            ims_hash.c \
            ims_jobControl.c \
            ims_shm.c \
            ims_signal.c \
            ims_version.c

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../lib/$(MACH)/local/libglb.a

###############################################################################
#
# Make the Library
#
###############################################################################

all: $(LIB)

$(LIB):	$(OBJ)
	@echo
	@echo "Archive and Conversion"
	ar rucv $(LIB) $?
	@if [ $(MACH) = sunos ]; then ranlib $(LIB); fi
	@echo

%.o:%.c
	@echo
	@echo "Compiling $<"
	$(CC) $(CFLAGS) $(IALL) -c $<

###############################################################################
#
# Object and library cleaning.
#
###############################################################################

clean:
	@echo
	@echo "Cleaning IMS global library"
	-rm -f $(OBJ) $(LIB)
	@echo

###############################################################################
#
# Library installation.
#
###############################################################################

install:
	@echo
	@echo "No installation required"
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
