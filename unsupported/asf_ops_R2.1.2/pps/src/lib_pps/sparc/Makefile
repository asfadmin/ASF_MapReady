#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Description:	makefile for libpps, libpps_client, libpps_debug
# Creator:	Nadia Adhami
# Date:		August 10, 1995
# Notes:	
#		use gnumake
# SCCS Info:
#  "@(#)Makefile	1.3    04/23/97"
#===============================================================================

include ../pps_makedefs

SUB_PATH        = src/lib_pps


LINT 		= lint
LINT_DIR 	= ln
DEBUG_DIR 	= debug
NOTHREAD_DIR	= nothread
IALL 		= \
		$(IDCE) \
		-I/usr/local/include \
		-I$(ASFINC)/imsdads \
		-I$(ASFINC)/sys \
		-I/user/include/sys \
		$(ISYBASE) \
		$(ILOCAL) \
		$(IGLOBAL)

LIBPPS 		= libpps
LIBPPS_NOTHREAD = libpps_nothread
LIBPPS_DEBUG 	= libpps_debug
LIBPPS_CLIENT 	= libpps_client
CFLAGS 		= -D_REENTRANT -D${MACH} -c -g 
DEBUG_FLAG	= -DDEBUG -DODL_DEBUG
NOTHREAD_FLAG   = -DNO_THREAD
OBJS_CLIB 	= messages_cstub.o error_msgs.o

MACH_OBJS_CLIB	= $(OBJS_CLIB:%.o=$(MACH)/%.o)

# note: messages_sstub.o should be the first object 
# since its compilation may generate a new messages.h

OBJS =  \
	pps_global.o			\
	bind_dbrec.o			\
	sigtrap.o			\
	mutex_dblib.o			\
	mutex_odllib.o			\
	match_L1request.o		\
	init_dbprocs.o			\
	read_config.o			\
	error_msgs.o 			\
	convert_odldate.o 		\
	datetime.o	 		\
	extractIMSmsg.o 		\
	extractCPmsg.o 			\
	ingestIMSmsg.o 			\
	ingestCPmsg.o 			\
	common_hdr.o 			\
	process_cp_retry.o      	\
	process_cp_scan.o 		\
	process_cp_scancomp.o 		\
	process_cp_scancancel.o		\
	process_cp_frame.o 		\
	process_cp_framefinal.o 	\
	process_ims_scan.o 		\
	process_ims_l1pr.o 		\
	process_ims_cancel.o 		\
	process_ims_svavail.o 		\
	msg_handler.o			\
	get_jobstate.o			\
	erase_svec.o			\
	cleanup.o			\
	init_sybase_mx.o		\
	pps_rpc_exit.o			\
	pps_rpc_kill.o			\
	pps_rpcs.o			\
	pps_db_table.o			\
	pps_logMsg.o			\
	createIMSmsg.o			\
	createCPjobs.o			\
	fill_order.o			\
	ims_int.o			\
	timerec.o	 		\
	match_sv2scan.o	 		\
	start_xact.o			

OBJS_PPSLIB    = messages_sstub.o $(OBJS)

OBJS_NOTHREAD  = ims_int_nothread.o	\
		 fill_order.o		\
		 createIMSmsg.o		\
		 pps_logMsg.o		\
		 datetime.o		\
		 timerec.o		\
		 error_msgs.o		\
		 read_config.o

MACH_OBJS_PPSLIB  = $(OBJS_PPSLIB:%.o=$(MACH)/%.o)
MACH_OBJS_NOTHREAD = $(OBJS_NOTHREAD:%.o=$(NOTHREAD_DIR)/%.o)
MACH_OBJS_DEBUGLIB = $(OBJS_PPSLIB:%.o=$(DEBUG_DIR)/%.o)
LINT_OBJS_PPSLIB  = $(OBJS:%.o=$(LINT_DIR)/%.ln)


default:    \
	$(LIBLOCAL)/$(LIBPPS).a	\
	$(LIBLOCAL)/$(LIBPPS_NOTHREAD).a \
	$(LIBGLOBAL)/$(LIBPPS_CLIENT).a \
	$(LIBLOCAL)/$(LIBPPS_DEBUG).a	

source:
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS

all:
	$(MAKE) clean
	$(MAKE) default

lint:   makedir_lint $(LINT_OBJS_PPSLIB)

$(LIBGLOBAL)/$(LIBPPS_CLIENT).a: $(MACH_OBJS_CLIB) makedir 
	$(LD) $(LDOPTS) -o $(LIBGLOBAL)/$(LIBPPS_CLIENT).so $(MACH_OBJS_CLIB)
	$(AR) $(LIBGLOBAL)/$(LIBPPS_CLIENT).a $(MACH_OBJS_CLIB)
	$(RANLIB) $(LIBGLOBAL)/$(LIBPPS_CLIENT).a

$(LIBLOCAL)/$(LIBPPS).a: makedir $(MACH_OBJS_PPSLIB)
	${REMOVE} $(LIBLOCAL)/$(LIBPPS).so
	$(LD) $(LDOPTS) -o $(LIBLOCAL)/$(LIBPPS).so $(MACH_OBJS_PPSLIB)
	$(AR) $(LIBLOCAL)/$(LIBPPS).a $(MACH_OBJS_PPSLIB)
	$(RANLIB) $(LIBLOCAL)/$(LIBPPS).a

$(LIBLOCAL)/$(LIBPPS_NOTHREAD).a: makedir_nothread $(MACH_OBJS_NOTHREAD)
	$(AR) $(LIBLOCAL)/$(LIBPPS_NOTHREAD).a $(MACH_OBJS_NOTHREAD)
	$(RANLIB) $(LIBLOCAL)/$(LIBPPS_NOTHREAD).a

$(LIBLOCAL)/$(LIBPPS_DEBUG).a: makedir_debug $(MACH_OBJS_DEBUGLIB)
	$(LD) $(LDOPTS) -o $(LIBLOCAL)/$(LIBPPS_DEBUG).so $(MACH_OBJS_DEBUGLIB)
	$(AR) $(LIBLOCAL)/$(LIBPPS_DEBUG).a $(MACH_OBJS_DEBUGLIB)
	$(RANLIB) $(LIBLOCAL)/$(LIBPPS_DEBUG).a

%.o : %.c defs.h
	$(CC) $(CFLAGS) $(IALL) -c $< -o $@

$(MACH)/%.o:%.c
	$(CC) $(CFLAGS) $(IALL) -c $< -o $@

$(NOTHREAD_DIR)/%.o:%.c
	$(CC) $(CFLAGS) $(NOTHREAD_FLAG) $(IALL) -c $< -o $@

$(NOTHREAD_DIR)/ims_int_nothread.o:ims_int.c
	$(CC) $(CFLAGS) $(NOTHREAD_FLAG) $(IALL) -c $< -o $@

$(LINT_DIR)/%.ln:%.c
	cd ln; $(LINT) $(CFLAGS) $(IALL) -c ../$<

$(DEBUG_DIR)/%.o:%.c
	$(CC) $(DEBUG_FLAG) $(CFLAGS) $(IALL) -c $< -o $@

$(MACH)/messages_cstub.o $(MACH)/messages_sstub.o:   messages.idl
	cd $(MACH);idl ../messages.idl
	/bin/rm -f $(ROOT)/include/global/messages.h
	cp $(MACH)/messages.h $(ROOT)/include/global
	chmod 0444 $(ROOT)/include/global/messages.h

$(DEBUG_DIR)/messages_cstub.o $(DEBUG_DIR)/messages_sstub.o:   messages.idl
	cd $(DEBUG_DIR);idl ../messages.idl

makedir :
	if [ -d ${MACH} ]; \
	then \
		${REMOVE} ${MACH}/Makefile; \
	else \
		mkdir ${MACH}; \
	fi
	cp Makefile ${MACH}/ ; \

makedir_nothread :
	if [ -d ${NOTHREAD_DIR} ]; \
	then \
		${REMOVE} ${NOTHREAD_DIR}/Makefile; \
	else \
		mkdir ${NOTHREAD_DIR}; \
	fi
	cp Makefile ${NOTHREAD_DIR}/ ; \

makedir_debug :
	if [ -d ${DEBUG_DIR} ]; \
	then \
		${REMOVE} ${DEBUG_DIR}/Makefile; \
	else \
		mkdir ${DEBUG_DIR}; \
	fi
	cp Makefile ${DEBUG_DIR}/ ; \

makedir_lint :
	if [ -d ${LINT_DIR} ]; \
	then \
		${REMOVE} ${LINT_DIR}/Makefile; \
	else \
		mkdir ${LINT_DIR}; \
	fi
	cp Makefile ${LINT_DIR}/ ; \

clean:
	rm -f $(MACH)/*.o $(DEBUG_DIR)/*.o $(NOTHREAD_DIR)/*.o

%.idl:	$(PPS_TREE)/$(SUB_PATH)/SCCS/%.idl
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.idl

%.c:	$(PPS_TREE)/$(SUB_PATH)/SCCS/%.c
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.c
