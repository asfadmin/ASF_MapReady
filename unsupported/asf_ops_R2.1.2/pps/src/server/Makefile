#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:     Makefile
# Description:  makefile for pps server
# Creator:      Nadia Adhami
# Date:         August 10, 1995
# Notes:
#               use gnumake
# SCCS Info:
#        "@(#)Makefile	1.2    12/19/96"
#===============================================================================

include ../pps_makedefs

SUB_PATH	= src/server
 
IALL   	= -I. \
	$(ILOCAL) \
	$(IGLOBAL) \
	$(IDCE) \
	$(ISYBASE) \
	$(IENCRYPT) \
	-I$(ASFINC) \
	-I$(ROOT)/include/global/dce

PPS_SERVER 	= pps_server
PPS_SERVER_SENT	= pps_server_sent
PPS_SERVER_DEBUG = pps_server_debug
LIBPPS 		= libpps
LIBPPS_DEBUG	= libpps_debug
CFLAGS		= -DDEBUG -g -Xa -D_REENTRANT
LIBS  		=				\
		$(ASFLIB)/libims.a 		\
		$(ASFLIB)/libimsodl.a 		\
		$(ASFLIB)/libodl.a 		\
		$(LIBLOCAL)/libsybint.a 	\
		$(SYBASE)/lib/libsybdb.a	\
		$(LIBLOCAL)/libpps_count.a	\
		$(LIBLOCAL)/libpps_mx.a		\
		$(LIBLOCAL)/libllist.a		\
		$(LIBGLOBAL)/libpps_extract.a	\
		$(ASFLIB)/libutil.a		\
		-ldce -lsocket -lnsl -lthread -lm -lmalloc


OBJS 		= server.o 
MACH_OBJS=$(OBJS:%.o=$(MACH)/%.o)

default: $(MACH)/$(PPS_SERVER) $(MACH)/$(PPS_SERVER_DEBUG) 

source:
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS

all:
	$(MAKE) clean
	$(MAKE) default

debug:	$(MACH)/$(PPS_SERVER_DEBUG) 

install:
	cp $(MACH)/$(PPS_SERVER) $(ROOT)/bin/$(MACH)
	cp $(MACH)/$(PPS_SERVER_DEBUG) $(ROOT)/bin/$(MACH)

sent:	$(MACH)/$(PPS_SERVER_SENT) 

$(MACH)/$(PPS_SERVER): makedir $(MACH_OBJS) $(LIBLOCAL)/$(LIBPPS).a
	$(CC) -o $(MACH)/$(PPS_SERVER) $(MACH_OBJS) \
		$(LIBLOCAL)/$(LIBPPS).a -L$(ASFLIB) $(LIBS)
	cp $(MACH)/$(PPS_SERVER) $(ROOT)/bin/$(MACH)

$(MACH)/$(PPS_SERVER_SENT): makedir $(MACH_OBJS) $(LIBLOCAL)/$(LIBPPS).a
	sentinel $(CC) -o $(MACH)/$(PPS_SERVER_SENT) $(MACH_OBJS) \
		$(LIBLOCAL)/$(LIBPPS).a -L$(ASFLIB) $(LIBS)
	cp $(MACH)/$(PPS_SERVER_DEBUG) $(ROOT)/bin/$(MACH)

$(MACH)/$(PPS_SERVER_DEBUG): makedir $(MACH_OBJS) $(LIBLOCAL)/$(LIBPPS_DEBUG).a
	$(CC) -o $(MACH)/$(PPS_SERVER_DEBUG) $(MACH_OBJS) \
		$(LIBLOCAL)/$(LIBPPS_DEBUG).a -L$(ASFLIB) $(LIBS)
	cp $(MACH)/$(PPS_SERVER_DEBUG) $(ROOT)/bin/$(MACH)

$(MACH)/%.o:	%.c
	$(CC) $(CFLAGS) $(IALL) -c $< -o $@

clean:
	${REMOVE} $(MACH)/* 

makedir :
	if [ -d ${MACH} ]; \
	then \
		${REMOVE} ${MACH}/Makefile; \
	else \
		mkdir ${MACH}; \
	fi
	cp Makefile ${MACH}/ ; \

%.c:	$(PPS_TREE)/$(SUB_PATH)/SCCS/%.c
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.c
