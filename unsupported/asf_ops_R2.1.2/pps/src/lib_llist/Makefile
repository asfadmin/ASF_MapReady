#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Description:	Makefile for FAIF common utility library.
# Creator:	Norbert Piega
# Notes:	
#
# SCCS Info:
#		@(#)Makefile	1.1  11/21/96
#===============================================================================

include ../pps_makedefs

SUB_PATH        = src/lib_llist

IFAIF=-I../. -I../../../include/local -I../../../include/global
ILLST=-I../../../include/local/llist
IALL=$(IFAIF) $(ILLST)

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=dapps_ord_list.c nmalloc.c dapps_list.c znew.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)

LIB=../../lib/$(MACH)/local/libllist.a

default: makedir $(LIB)

source:
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS

all:
	$(MAKE) clean
	$(MAKE) default

$(LIB):	$(MACHOBJ)
	$(AR) $(LIB) $?

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)

makedir :
	if [ -d ${MACH} ]; \
	then \
		${REMOVE} ${MACH}/Makefile; \
	else \
		mkdir ${MACH}; \
	fi
	cp Makefile ${MACH}/ ; \

clean:
	rm -f $(MACH)/*

%.c:	$(PPS_TREE)/$(SUB_PATH)/SCCS/%.c
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.c
