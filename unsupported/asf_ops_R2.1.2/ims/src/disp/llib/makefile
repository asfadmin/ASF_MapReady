# @(#)makefile	5.4  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the libdisp.a library.
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

IDISP =     -I../lincl
IV0 =       -I../../v0/lincl
IGLB =      -I../../gincl
IPUB =      -I../../pincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include
IDCE =      -I/opt/dce/share/include
IPPS =      -I$(ASFINC)/pps

IALL =      $(IDISP) $(IV0) $(IGLB) $(IPUB) $(IEXP) $(IASF) $(ISYB) $(IDCE) \
						$(IPPS) $(MACHIFLAGS)

###############################################################################
#
# Flags and Defines
#
###############################################################################

#DEFS =      -DSUN_UNIX -DEOSDIS -DODEBUG
DEFS =      -DSUN_UNIX -DEOSDIS -DODEBUG -DDCEON

#
# The DCE headers can't be compiled with the -Xc flag so we have
# to redefine the sparcCFLAGS value with -Xa.
#
sparcCFLAGS = -Xa -Dsparc

CFLAGS =    $(MACHCFLAGS) $(DEFS)

###############################################################################
#
# Source and Target Definition
#
###############################################################################

SRC =       \
            ims_dispHandler.c \
            ims_dispCat.c \
            ims_step.c \
            ims_stepMsgTree.c \
            ims_stepCat.c \
            client_send.c
OBJ =       $(SRC:%.c=%.o)
LIB =       ../../../lib/$(MACH)/local/libdisp.a

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
	@echo "Cleaning Dispatcher local library"
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
