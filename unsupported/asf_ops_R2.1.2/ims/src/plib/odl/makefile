# @(#)makefile	5.2  30 Sep 1996
###############################################################################
#
# File:         makefile
#
# Function:     Make the libodl.a library.
#
# Author:       S. Hardman
#
# Date:         12/8/94
#
# Notes:        This version of the library does not define EOSDIS.
#
###############################################################################

include ../../makedefs
 
###############################################################################
#
# Include Path Definitions
#
###############################################################################

IODL =      -I.

IALL =      $(IODL) $(MACHIFLAGS)

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      -DSUN_UNIX
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
            rdlabel_buf.c \
            rdvalue.c \
            v_nodes.c \
            wrtlabel.c \
            wrtlabel_buf.c \
            wrtsrc.c

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../../lib/$(MACH)/global/libodl.a

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
	@echo "Cleaning External ODL library"
	-rm -f $(OBJ) $(LIB)
	@echo

###############################################################################
#
# Library and header file installation.
#
###############################################################################

install:
	@echo
	@echo "Installing External ODL library and header files"
	cp -p $(LIB) $(ASFLIB)
	@rm -f $(ASFINC)/sys/odl*.h
	cp -p ../../pincl/odldef.h $(ASFINC)/sys
	cp -p ../../pincl/odlinter.h $(ASFINC)/sys
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
