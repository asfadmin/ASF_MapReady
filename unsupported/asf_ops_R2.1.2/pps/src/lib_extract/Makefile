#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Description:	makefile for libextract
# Notes:	
#		use gnumake
# SCCS Info:
#		@(#)Makefile	1.1  11/21/96
#===============================================================================

include ../pps_makedefs

SUB_PATH        = src/lib_extract


LINT 		= lint
LINT_DIR 	= ln
DEBUG_DIR 	= debug
IALL 		= 			\
		-I$(ASFINC)/sys		\
		$(ILOCAL) \
		$(IGLOBAL)

CFLAGS 		= -D${MACH} -c -g 
DEBUG_FLAG	= -DDEBUG

OBJS =  \
	extract_param.o

TARGET_LIB =	libpps_extract.a

default:	$(LIBGLOBAL)/$(TARGET_LIB)

source:
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS

all:
	$(MAKE) clean
	$(MAKE) default

$(LIBGLOBAL)/$(TARGET_LIB): $(OBJS)
	$(AR) $(LIBGLOBAL)/$(TARGET_LIB) $(OBJS)
	$(RANLIB) $(LIBGLOBAL)/$(TARGET_LIB)

.c.o:
	$(CC) $(CFLAGS) $(IALL) $<

clean:
	rm -f *.o

%.c:	$(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.c
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$@
