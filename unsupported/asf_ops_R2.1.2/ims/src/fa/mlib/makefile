# "@(#)makefile	5.3 10/20/97"
###############################################################################
#
# File:         makefile
#
# Function:     Make the libims_math.a library.
#
# Author:       D. Crichton
#
# Date:         1/31/96
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IMATH =     -I../mincl
IFA =       -I../lincl
IGLB =      -I../../gincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IMATH) $(IFA) $(IGLB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

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
			ims_asf2gha.c \
			ims_asf2jd.c \
			ims_asf2jday.c \
			ims_j1ll2grs.c \
			ims_j1rt2rsp.c \
			ims_sephem.c \
			ims_solarangs.c \
			ims_solazd.c \
			ims_vector.c

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../../lib/$(MACH)/local/libims_math.a

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
	@echo "Cleaning the IMS Math local library"
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
