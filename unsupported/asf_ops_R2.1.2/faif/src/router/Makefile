#===============================================================================
# Filename:	Makefile
# Description:	Makefile for FAIF routing library.
# Creator:	Norbert Piega
# Notes:	
#
# SCCS Info:
#  Makefile
#  1
#  07/16/97
#  @(#)Makefile	1.3
#===============================================================================

include ../faif_makedefs

IFAIF=-I../. -I../../../include/local -I../../../include/global
ILLST=-I/ASF/$(ASFVER)/include/aps
IODL=-I../../../include/local/ODL
IALL=$(IFAIF) $(ILLST) $(IODL)

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=router.c send_to_ims.c bak_log.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)

ifeq ($(DM), ESA_DM)
   LIB=../../lib/$(MACH)/local/libErouter.a
endif
ifeq ($(DM), NASDA_DM)
   LIB=../../lib/$(MACH)/local/libNrouter.a
endif
ifeq ($(DM), CSA_DM)
   LIB=../../lib/$(MACH)/local/libCrouter.a
endif
ifeq ($(DM), WALPS_DM)
   LIB=../../lib/$(MACH)/local/libWrouter.a
endif
ifeq ($(DM), ADEOS_DM)
   LIB=../../lib/$(MACH)/local/libArouter.a
endif

all:	$(LIB)

$(LIB):	$(MACHOBJ)
	$(AR) $(LIB) $?

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) -D$(DM) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) -D$(DM) $(IALL) -c $(notdir $<)

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/* $(LIB)

cleanobjs:
	rm -f $(MACH)/*
