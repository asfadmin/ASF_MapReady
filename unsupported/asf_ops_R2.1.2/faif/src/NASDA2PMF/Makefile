#===============================================================================
# Filename:	Makefile
# Description:	Makefile for NASDA2PMF library.
# Creator:	Norbert Piega
# Notes:	
#
# SCCS Info:
#  Makefile
#  1
#  07/16/97
#  @(#)Makefile	1.2
#===============================================================================

include ../faif_makedefs

IFAIF=-I../. -I../../../include/local -I../../../include/global
IODL=-I../../../include/local/ODL
ILLST=-I/ASF/$(ASFVER)/include/aps
IAPS=-I$(APSINC) $(ISYBASE)

IALL= $(OSIFLAGS) $(IFAIF) $(IODL) $(ILLST) $(IAPS)

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=NASDA2PMF.c rename_JERS_file.c

MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)

LIB=../../lib/$(MACH)/local/libnasda2pmf.a

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

