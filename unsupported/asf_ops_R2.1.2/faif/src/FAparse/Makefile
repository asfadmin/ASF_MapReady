#===============================================================================
# Filename:	Makefile
# Description:	Makefile for FAIF common utility library.
# Creator:	Phil Yurchuk, Cameron Cooper
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
IAPS=-I$(APSINC)
IALL=$(IFAIF) $(IAPS) $(IODL)

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=FAparse.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)

LIB=../../lib/$(MACH)/local/libfaparse.a

all:	$(LIB)

$(LIB):	$(MACHOBJ)
	$(AR) $(LIB) $?

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<
#cd $(MACH);$(CC) $(CFLAGS) $(IALL) $(APS_LIB) $(DP_VECTOR_LIB) $(MATH_LIB)  -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<) 
#	cd $(MACH);$(CC) $(CFLAGS) $(IALL) \
        $(MATH_LIB) \
        $(SPARC_LIB) $(UCB_LIB) -c $(notdir $<)

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/* $(LIB)

