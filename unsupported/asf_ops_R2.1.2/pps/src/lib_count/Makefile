#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Description:	makefile for libpps_count
# Creator:	Nadia Adhami
# Date:		August 15, 1995
# Notes:	
#		use gnumake
# SCCS Info:
#  "@(#)Makefile	1.1    11/21/96"
#===============================================================================

include ../pps_makedefs

SUB_PATH        = src/lib_count

TEST_DIR 	= test
IALL 		= -I/usr/local/include \
		$(ISYBASE) \
		$(ILOCAL) \
		$(IGLOBAL)

INC_LOCAL 	= $(ROOT)/include/local
LIBPPS_COUNT 	= libpps_count
CFLAGS 		= -D${MACH} -c -g -DDEBUG
OBJS_COUNT 	= ppscount.o
MACH_OBJS_COUNT	= $(OBJS_COUNT:%.o=$(MACH)/%.o)


default:    $(INC_LOCAL)/ppscount.h $(LIBLOCAL)/$(LIBPPS_COUNT).a

source:
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS

all:
	$(MAKE) clean
	$(MAKE) default

$(LIBLOCAL)/$(LIBPPS_COUNT).a: makedir $(MACH_OBJS_COUNT)
	$(LD) $(LDOPTS) -o $(LIBLOCAL)/$(LIBPPS_COUNT).so $(MACH_OBJS_COUNT)
	$(AR) $(LIBLOCAL)/$(LIBPPS_COUNT).a $(MACH_OBJS_COUNT)
	$(RANLIB) $(LIBLOCAL)/$(LIBPPS_COUNT).a

$(MACH)/%.o:%.c
	$(CC) $(CFLAGS) $(IALL) -c $< -o $@

$(INC_LOCAL)/ppscount.h : ppscount.src ppscount.perl
	chmod a+x ppscount.perl
	-/bin/rm -f ppscount.h
	./ppscount.perl ppscount.src ppscount.h
	chmod 0444 ppscount.h
	-/bin/rm -f $(INC_LOCAL)/ppscount.h
	cp ppscount.h $(INC_LOCAL)/ppscount.h

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

%.perl:	$(PPS_TREE)/$(SUB_PATH)/SCCS/%.perl
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.perl

%.src:	$(PPS_TREE)/$(SUB_PATH)/SCCS/%.src
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.src

%.c:	$(PPS_TREE)/$(SUB_PATH)/SCCS/%.c
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.c
