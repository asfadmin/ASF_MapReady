# @(#)makefile	1.5  05/16/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the libdf.a library.
#
# Author:       S. Hardman
#
# Date:         5/6/96
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IHDF =      -I.

IALL =      $(IHDF) $(MACHIFLAGS)

###############################################################################
#
# Flags and Defines
#
###############################################################################

sgiDEFS =
sparcDEFS = -DSUN
CFLAGS =    $(MACHCFLAGS) $($(MACH)DEFS)

###############################################################################
#
# Source and Target Definition
#
###############################################################################

SRC =       hfile.c herr.c hblocks.c hextelt.c hkit.c dfan.c dfr8.c dfcomp.c \
            dfrle.c dfimcomp.c dfjpeg.c dfunjpeg.c dfp.c dfgr.c df24.c dfsd.c \
            dfgroup.c dfconv.c dfufp2i.c vg.c vrw.c vio.c vparse.c vconv.c \
            vsfld.c vgp.c vhi.c dfutil.c dfstubs.c dfknat.c dfkswap.c \
            dfkcray.c dfkvms.c dfkconv.c dfkfuji.c hbitio.c tbbt.c hdfalloc.c \
            jbsmooth.c jcarith.c jccolor.c jcdeflts.c jcexpand.c jchuff.c \
            jcmaster.c jcmcu.c jcpipe.c jcsample.c jdarith.c jdcolor.c \
            jddeflts.c jdhuff.c jdmaster.c jdmcu.c jdpipe.c jdsample.c \
            jerror.c jquant1.c jquant2.c jfwddct.c jrevdct.c jutils.c \
            jmemmgr.c jmemsys.c

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../../lib/$(MACH)/local/libdf.a

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
	@echo "Cleaning HDF library"
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
