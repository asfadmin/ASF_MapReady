# @(#)makefile	5.5 10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the libims_fa.a library.
#
# Author:       D. Crichton
#
# Date:         1/31/96
#
# Note:         The _XOPEN_SOURCE directive is defined for building the
#               ims_passlog.c file.
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IFA =       -I../lincl
IMATH =     -I../mincl
IGLB =      -I../../gincl
IEXP =      -I../../eincl
IFAIF =     -I$(ASFINC)/faif
ISYB =      -I$(SYBASE)/include

IALL =      $(IFA) $(IMATH) $(IGLB) $(IEXP) $(IASF) $(IFAIF) $(ISYB) \
						$(MACHIFLAGS)

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      
CFLAGS =    $(MACHCFLAGS) $(DEFS) -D _XOPEN_SOURCE

###############################################################################
#
# Source and Target Definition
#
###############################################################################

SRC = \
			ims_fa_adeos.c \
			ims_fa_csa.c \
			ims_fa_esa.c \
			ims_fa_guilib.c \
			ims_fa_nasda.c \
			ims_fa_read_adeos.c \
			ims_fa_read_esa.c \
			ims_fa_read_nasda.c \
			ims_passlog.c \
			ims_submitReport.c 

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../../lib/$(MACH)/local/libims_fa.a

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
	@echo "Cleaning the IMS FA local library"
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
