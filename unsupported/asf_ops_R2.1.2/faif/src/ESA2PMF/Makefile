#===============================================================================
# Filename:	Makefile
# Description:	Makefile for ESA2PMF library.
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
ILLST=-I/ASF/$(ASFVER)/include/aps
IAPS=-I$(APSINC) $(ISYBASE)
ISYB=-I$(SYBASE)/include
IIMS=-I$(IMSINC)

IALL= $(IIMS) $(IASF) $(ISYB) $(OSIFLAGS) $(IFAIF) $(IODL) $(ILLST) $(IAPS)

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=ESA2PMF.c gha.c

MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)

LIB=../../lib/$(MACH)/local/libesa2pmf.a

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

