#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:     Makefile
# Description:  makefile for pps client
# Creator:      Nadia Adhami
# Date:         August 10, 1995
# Notes:
#               use gnumake
# SCCS Info:
#               @(#)Makefile	1.3  10/31/97
#===============================================================================

include ../pps_makedefs

SUB_PATH	= src/server_stop
 
COMMON_INC = $(DAPPS_SRCPATH)/include/global
COMMON_LIB = $(DAPPS_SRCPATH)/lib
LOCAL_INC  = $(DAPPS_SRCPATH)/include/local

IALL   = -I. $(IDCE) \
	$(ILOCAL) \
	$(IGLOBAL) \
	$(ISYBASE) \
	$(IENCRYPT) \
	$(IGLOBAL)/dce

PPS_CLIENT_EXIT	= pps_server_stop
LIBPPS 		= libpps

CFLAGS		= -DDEBUG -g -Xa -D_REENTRANT $(IALL)
LIBS  		= \
		$(LIBGLOBAL)/libpps_client.a \
		-ldce -lsocket -lnsl -lthread -lm

OBJS_EXIT = client_exit.o 
MACH_OBJS_EXIT = $(OBJS_EXIT:%.o=$(MACH)/%.o)

default: ${MACH}/$(PPS_CLIENT_EXIT)

source:
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS

all:
	$(MAKE) clean
	$(MAKE) default

install:
	cp ${MACH}/$(PPS_CLIENT_EXIT) $(ROOT)/bin/$(MACH)

${MACH}/$(PPS_CLIENT_EXIT):	makedir $(MACH_OBJS_EXIT)
	$(CC) -o ${MACH}/$(PPS_CLIENT_EXIT) $(MACH_OBJS_EXIT) $(LIBS)
	cp ${MACH}/$(PPS_CLIENT_EXIT) ${ROOT}/bin/$(MACH)

${MACH}/%.o:	%.c
	$(CC) $(CFLAGS) ${IALL} -c $< -o $@

clean:
	${REMOVE} ${MACH}/*

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
