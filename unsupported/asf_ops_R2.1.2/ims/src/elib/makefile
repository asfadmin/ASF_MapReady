# @(#)makefile	5.3  03/13/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the libims.a library.
#
# Author:       S. Hardman
#
# Date:         3/25/95
#
###############################################################################

include ../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IEXP =      -I../eincl
IPUB =      -I../pincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IEXP) $(IPUB) $(ISYB) $(MACHIFLAGS)

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      
CFLAGS =    $(MACHCFLAGS) $(DEFS)
YFLAGS =    -d

###############################################################################
#
# Source and Target Definition
#
###############################################################################

SRC = \
            ims_acct.c\
            ims_acctCat.c \
            ims_archive.c \
            ims_archiveUtil.c \
            ims_ask.c \
            ims_askCat.c \
            ims_askParser.c \
            ims_askLexer.c \
            ims_cmnQuery.c \
            ims_getInput.c \
            ims_msg.c \
            ims_msgCb.c \
            ims_odlBuffer.c \
            ims_qi.c \
            ims_timeConv.c \
            ims_util.c

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../lib/$(MACH)/global/libims.a

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

ims_askParser.c: ims_askParser.y

.y.c:
	@echo
	@echo "Yaccing $<"
	yacc $(YFLAGS) $*.y
	mv y.tab.h ../eincl/ims_askParser.h
	mv y.tab.c $*.c

ims_askLexer.c:  ims_askLexer.l

.l.o:
	@echo
	@echo "Lexing $<"
	lex $*.l
	mv lex.yy.c $*.c

###############################################################################
#
# Object and library cleaning.
#
###############################################################################

clean:
	@echo
	@echo "Cleaning IMS export library"
	-rm -f ims_askParser.c ims_askLexer.c $(OBJ) $(LIB)
	@echo

###############################################################################
#
# Library and header file installation.
#
###############################################################################

install:
	@echo
	@echo "Installing IMS export library and header files"
	cp -p $(LIB) $(ASFLIB)
	@rm -f $(ASFINC)/imsdads/ims_*.h
	cp -p ../eincl/*.h $(ASFINC)/imsdads
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
