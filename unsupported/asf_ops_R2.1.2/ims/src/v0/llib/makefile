# @(#)makefile	5.10  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the libv0.a library.
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
IDISP =     -I../../disp/lincl
IGLB =      -I../../gincl
IPUB =      -I../../pincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IV0) $(IDISP) $(IGLB) $(IPUB) $(IEXP) $(IASF) $(ISYB) \
						$(MACHIFLAGS)

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

SRC = \
            ims_v0Cat.c \
            ims_v0Dar.c \
            ims_v0Handler.c \
            ims_v0MsgTree.c \
						ims_v0Order.c \
            ims_v0Package.c \
            ims_v0Process.c \
            ims_v0Query.c \
            ims_v0Sort.c \
            ims_v0Spatial.c \
            ims_v0Time.c \
            ims_v0Util.c \
            spatial_query_routines.c

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../../lib/$(MACH)/global/libv0.a

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
	@echo "Cleaning V0 library"
	-rm -f $(OBJ) $(LIB)
	@echo

###############################################################################
#
# Library installation.
#
###############################################################################

install:
	@echo
	@echo "Installing V0 library"
	cp -p $(LIB) $(ASFLIB)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
