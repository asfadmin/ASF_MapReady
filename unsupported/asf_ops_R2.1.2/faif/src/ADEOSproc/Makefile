#===============================================================================
# Filename:	Makefile
# Description:	Makefile for common ADEOS processing library.
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
ILLST=-I/ASF/$(ASFVER)/include/aps
IALL=$(IFAIF) $(IENCRYPT) $(ILLST)

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=ADEOS_proc.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)

LIB=../../lib/$(MACH)/local/libADEOSproc.a

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

