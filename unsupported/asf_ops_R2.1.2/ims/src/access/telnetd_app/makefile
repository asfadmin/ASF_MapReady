# @(#)makefile	1.2  05/14/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the access telnetd applications.
#
# Author:       S. Hardman
#
# Date:         5/2/96
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IALL =      $(MACHIFLAGS)

###############################################################################
#
# Library Path Definitions
#
###############################################################################

LALL =      $(MACHLFLAGS)

###############################################################################
#
# Library Definitions
#
###############################################################################

sgiLIBS =
sparcLIBS = -lsocket -lnsl /usr/ucblib/libucb.a

LIBS =      $($(MACH)LIBS)

###############################################################################
#
# Flags and Defines
#
###############################################################################

sgiDEFS =   -Dsgi
sparcDEFS = -Xc -Dsparc -DSVR4 -DSYSV -DSOLARIS -DECHOCTL

CFLAGS =    -g $($(MACH)DEFS)

###############################################################################
#
# Target Definition
#
###############################################################################

SRC =       *.c
OBJ =       $(SRC:%.c=%.o)
EXE =       \
            ../../../bin/$(MACH)/ims_telnetd \
            ../../../bin/$(MACH)/ims_tty

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

../../../bin/$(MACH)/ims_telnetd:  ims_exec.o ims_telnetd.o
	@echo
	@echo "Creating Executable ims_telnetd"
	$(CC) -o $@ ims_exec.o ims_telnetd.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_tty:  ims_tty.o
	@echo
	@echo "Creating Executable ims_tty"
	$(CC) -o $@ ims_tty.o $(LALL) $(LIBS)
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
	@echo "Cleaning Access telnetd executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing Access telnetd executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
