# @(#)makefile	1.1 05 Nov 1996
###############################################################################
#
# File:         makefile
#
# Function:     Make the libik.a library.
#
# Author:       S. Hardman
#
# Date:         12/8/94
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IIK =       -I.

IALL =      $(IIK) $(MACHIFLAGS)

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      -DBSD_COMP -DEOSDIS -DDAAC_SERVER -DNDEBUG
#CFLAGS =    $(MACHCFLAGS) $(DEFS)
CFLAGS =    -Xc -Dsparc -DDEBUG $(DEFS)

###############################################################################
#
# Source and Target Definition
#
###############################################################################

SRC = \
            IK_Auth.c \
            IK_Clnt.c \
            IK_Comn.c \
            IK_ODLetc.c \
            IK_Patch.c \
            IK_Pong.c \
            IK_Serv.c \
            IK_Session.c \
            IK_SysConf.c \
            IK_Syslog.c

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../../lib/$(MACH)/global/libik.a

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
	@echo "Cleaning IK library"
	-rm -f $(OBJ) $(LIB)
	@echo

###############################################################################
#
# Library installation.
#
###############################################################################
 
install:
	@echo
	@echo "Installing IK library"
	cp -p $(LIB) $(ASFLIB)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
