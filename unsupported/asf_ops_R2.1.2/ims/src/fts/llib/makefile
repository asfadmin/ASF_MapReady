# @(#)makefile	5.3  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the libfts.a library.
#
# Author:       S. Hardman
#
# Date:         6/17/94
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
            ims_auxCat.c \
            ims_fwMsg.c \
            ims_srvCat.c \
            ims_srvEvent.c \
            ims_srvMutex.c \
            ims_srvReadGranule.c \
            ims_srvShipMsgs.c \
            ims_srvUtil.c

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../../lib/$(MACH)/local/libfts.a

###############################################################################
#
# Make the Library
#
###############################################################################

all core corelib: $(LIB)

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
	@echo "Cleaning FTS local library"
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
