#===============================================================================
# Filename:	Makefile
# Description:	Makefile for utility library.
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
IALL=$(IFAIF) $(IODL) $(ILLST)

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=CSA2PMF.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)

LIB=../../lib/$(MACH)/local/libcsa2pmf.a

all:    $(LIB)

$(LIB): $(MACHOBJ)
	$(AR) $(LIB) $?

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) $(STATIC)  -c ../$<
 
$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/* $(LIB)
 
