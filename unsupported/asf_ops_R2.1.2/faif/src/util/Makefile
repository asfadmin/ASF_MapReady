#===============================================================================
# Filename:	Makefile
# Description:	Makefile for FAIF common utility library.
# Creator:	Norbert Piega
# Notes:	
#
# SCCS Info:
#  Makefile
#  1
#  06 Sep 1996
#  @(#)Makefile	1.4
#===============================================================================

include ../faif_makedefs

IFAIF=-I../. -I../../../include/local -I../../../include/global
IALL=$(IFAIF)

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=utilmalloc.c process_exit.c catamaran.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)

LIB=../../lib/$(MACH)/global/libutil.a

all:	$(LIB)
	cp ../../lib/$(MACH)/global/libutil.a ../../lib/$(MACH)/local/libutil.a

$(LIB):	$(MACHOBJ)
	$(AR) $(LIB) $?

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)
dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/*
	rm -f $(LIB)
	rm -f ../../lib/$(MACH)/local/libutil.a
