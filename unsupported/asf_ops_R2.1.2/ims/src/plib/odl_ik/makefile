# @(#)makefile	5.2  30 Sep 1996
###############################################################################
#
# File:         makefile
#
# Function:     Make the libodlc.a library.
#
# Author:       S. Hardman
#
# Date:         12/8/94
#
# Notes:        This version of the library does define EOSDIS.
#
###############################################################################

include ../../makedefs
 
###############################################################################
#
# Include Path Definitions
#
###############################################################################

IODL =      -I.
IIK =       -I../ik

IALL =      $(IODL) $(IIK) $(MACHIFLAGS)

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      -DSUN_UNIX -DEOSDIS
CFLAGS =    $(MACHCFLAGS) $(DEFS)
YFLAGS =    -d

###############################################################################
#
# Source and Target Definition
#
###############################################################################

SRC = \
            a_nodes.c \
            ag_nodes.c \
            ao_nodes.c \
            comments.c \
            cvtvalue.c \
            fmtvalue.c \
            lexan.c \
            output.c \
            p_nodes.c \
            parsact.c \
            parser.c \
            prtlabel.c \
            prtsrc.c \
            rdlabel.c \
            rdvalue.c \
            v_nodes.c \
            wrtlabel.c \
            wrtsrc.c

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../../lib/$(MACH)/local/libodlc.a

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

#odl2.c: odl2.y

.y.c:
	yacc $(YFLAGS) $*.y
	/bin/mv/y.tab.h $(IODL)/odl2.h
	/bin/mv y.tab.c $*.c

#odl2.c: odl2.l

.l.o:
	lex $*.l
	/bin/mv lexer.yy.c $*.c

###############################################################################
#
# Object and library cleaning.
#
###############################################################################

clean:
	@echo
	@echo "Cleaning Internal ODL library"
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
