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
#		@(#)Makefile	1.1  11/21/96
#===============================================================================

include ../pps_makedefs

SUB_PATH        = src/lib_mx

TEST_DIR 	= test
IALL 		= \
		$(IDCE) \
		-I/usr/local/include \
		$(ISYBASE) \
		$(ILOCAL) \
		$(IGLOBAL)

INC_LOCAL 	= $(ROOT)/include/local
LIBPPS_MX 	= libpps_mx
CFLAGS 		= -D${MACH} -c -g -DDEBUG
OBJS 		= ppsmx.o
MACH_OBJS	= $(OBJS:%.o=$(MACH)/%.o)


default:    $(LIBLOCAL)/$(LIBPPS_MX).a

source:
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS

all:
	$(MAKE) clean
	$(MAKE) default

$(LIBLOCAL)/$(LIBPPS_MX).a: makedir $(MACH_OBJS)
	$(LD) $(LDOPTS) -o $(LIBLOCAL)/$(LIBPPS_MX).so $(MACH_OBJS)
	$(AR) $(LIBLOCAL)/$(LIBPPS_MX).a $(MACH_OBJS)
	@$(RANLIB) $(LIBLOCAL)/$(LIBPPS_MX).a

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

%.c:	$(PPS_TREE)/$(SUB_PATH)/SCCS/%.c
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.c
