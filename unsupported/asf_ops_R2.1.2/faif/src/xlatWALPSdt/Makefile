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
IODL=-I../../../include/local/ODL
IALL=$(IFAIF) $(IODL)

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=xlatWALPSdt.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)

LIB=../../lib/$(MACH)/local/libwxlatdt.a

all:	$(LIB)

$(LIB):	$(MACHOBJ)
	$(AR) $(LIB) $?

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/* $(LIB)

