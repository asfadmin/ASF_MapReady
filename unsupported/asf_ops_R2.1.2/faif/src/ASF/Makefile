#===============================================================================
# Filename:	Makefile
# Description:	Makefile for FAIF common utility library.
# Creator:	Norbert Piega
# Notes:	
#
# SCCS Info:
#  %M%
#  %R%
#  %G%
#  %W%
#===============================================================================

include ../faif_makedefs

IFAIF=-I../. -I../../../include/local -I../../../include/global
IALL=$(IFAIF)

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=ASFdt.c ASFhdr.c ASFsv.c ASFwos.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)

LIB=../../lib/$(MACH)/global/libfasf.a

all:	$(LIB)
	cp ../../lib/$(MACH)/global/libfasf.a ../../lib/$(MACH)/local/libfasf.a

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
	rm -f $(LIB) ../../lib/$(MACH)/local/libfasf.a
