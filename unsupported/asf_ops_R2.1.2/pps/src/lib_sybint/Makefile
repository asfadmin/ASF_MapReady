#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Description:	makefile for libsybint
# Creator:	Nadia Adhami
# Date:		August 10, 1995
# Notes:	
#		use gnumake
# SCCS Info:
#  		@(#)Makefile	1.1  11/21/96
#===============================================================================

include ../pps_makedefs

DEBUG		= NODEBUG
LIB 		= libsybint
CFLAGS 		= -D${MACH} -c -g -D$(DEBUG)
IALL 		= -I/usr/local/include \
		$(ISYBASE) \
		$(ILOCAL) \
		$(IGLOBAL)

OBJS 		= db_sybint.o 
MACH_OBJS	= $(OBJS:%.o=$(MACH)/%.o)

default:    $(LIBLOCAL)/$(LIB).a

source:
	sccs get $(PPS_TREE)/src/lib_sybint/SCCS

all:
	$(MAKE) clean
	$(MAKE) default

$(LIBLOCAL)/$(LIB).a: makedir $(MACH_OBJS)
	$(LD) $(LDOPTS) -o $(LIBLOCAL)/$(LIB).so $(MACH_OBJS)
	$(AR) $(LIBLOCAL)/$(LIB).a $(MACH_OBJS)
	$(RANLIB) $(LIBLOCAL)/$(LIB).a

$(MACH)/%.o:%.c
	$(CC) $(CFLAGS) $(IALL) -c $< -o $@

makedir :
	if [ -d ${MACH} ]; \
	then \
		${REMOVE} ${MACH}/Makefile; \
	else \
		mkdir ${MACH}; \
	fi
	cp Makefile ${MACH}/ ; \

clean:
	rm -f $(MACH)/*.o

%.c:    $(PPS_TREE)/src/lib_sybint/SCCS/%.c
	sccs get $(PPS_TREE)/src/lib_sybint/SCCS/s.$@
