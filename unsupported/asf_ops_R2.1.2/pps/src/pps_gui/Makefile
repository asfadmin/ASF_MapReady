#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
########################################################################
# $Date$		$Revision$
########################################################################
#             Copyright 1991, Visual Edge Software Ltd.
#
# ALL  RIGHTS  RESERVED.  Permission  to  use,  copy,  modify,  and
# distribute  this  software  and its documentation for any purpose
# and  without  fee  is  hereby  granted,  provided  that the above
# copyright  notice  appear  in  all  copies  and  that  both  that
# copyright  notice and this permission notice appear in supporting
# documentation,  and that  the name of Visual Edge Software not be
# used  in advertising  or publicity  pertaining to distribution of
# the software without specific, written prior permission. The year
# included in the notice is the year of the creation of the work.
########################################################################
# 
#    	MAKEFILE FOR STAND-ALONE UX CODE APPLICATION.
#
#       EXECUTABLE      is the name of the executable to be created 
#       MAIN            is the .c file containing your main() function
#       INTERFACES      is a list of the generated C code files 
#       APP_OBJS        is a (possibly empty) list of the object code
#                       files that form the non-interface portion of
#                       your application 
#
#       In the first three statements, the variables on the right 
#	of the equal sign will be replaced with their corresponding 
#       values when the makefile is automatically generated. 
#
# SCCS:
#	@(#)Makefile	1.6  07/08/97
########################################################################
# 
#	List of supported compiler(s):
#
#	gcc - Version 2.6.1
#	cc  - SPARCompiler Version 3.0.1
#	CC  - SPARCompiler Version 3.0.1  
#
########################################################################

SUB_PATH        = src/pps_gui

EXECUTABLE	= pps_gui
REPORT_TO_IMS	=	pps_report_to_ims

MAIN		= pps.c
INTERFACES	= pps_query.c \
	nojoy.c \
	pps_main.c \
	pps_plan.c \
	pps_policy.c \
	pps_file_selector.c \
	pps_error_dialog.c 

LANGUAGE	= ANSI C
APPL_OBJS	=  pps_query_util.o pps_plan_util.o pps_policy_util.o pps_util.o pps_db.o pps_list_util.o pps_motif_util.o pps_check_params.o

HEADER_FILES	= \
	nojoy.h			\
	pps.h			\
	pps_check_params.h	\
	pps_common.h		\
	pps_db.h		\
	pps_error_dialog.h	\
	pps_file_selector.h	\
	pps_main.h		\
	pps_plan.h		\
	pps_policy.h		\
	pps_query.h		\
	pps_util.h

C_FILES		= \
	nojoy.c			\
	pps.c			\
	pps_check_params.c	\
	pps_db.c		\
	pps_error_dialog.c	\
	pps_file_selector.c	\
	pps_list_util.c		\
	pps_main.c		\
	pps_motif_util.c	\
	pps_plan.c		\
	pps_plan_util.c		\
	pps_policy.c		\
	pps_policy_util.c	\
	pps_query.c		\
	pps_query_util.c	\
	pps_util.c

APPL_MSG_FILES	=
MSG_FILES	= $(INTERFACES:.c=.msg) $(APPL_MSG_FILES)
MSG_FILES_STRIP	= $(INTERFACES:.c=) $(APPL_MSG_FILES:.msg=)
MSG_CATALOG	= $(EXECUTABLE).cat
MSG_HEADER	= $(EXECUTABLE)_cat.h
MSG_CPP		= 
MSG_DEPEND	= 

LIB_DEPEND	= $(LIBLOCAL)/libpps_nothread.a $(ASFLIB)/libims.a \
        $(ASFLIB)/libimsodl.a $(ASFLIB)/libutil.a  \
        $(ASFLIB)/libodl.a

SHELL		= /bin/sh

UX_DIR		= /usr/local/uimx2.9
UX_LIBPATH	= $(UX_DIR)/lib
X_LIBS          = -lXm -lXt -lX11 -lXext


X_LIBDIR	= /usr/openwin/lib
X_LIBPATH	= -L$(X_LIBDIR)
MOTIF_LIBDIR	= /usr/dt/lib
MOTIF_LIBPATH	= -L$(MOTIF_LIBDIR)
X_CFLAGS	= -I/usr/openwin/include
MOTIF_CFLAGS	= -DMOTIF -I/usr/dt/include


CAT_CFLAGS	= -DUX_CATALOG_HEADER=\"$(MSG_HEADER)\" \
		  -DUX_CATALOG_NAME=\"$(MSG_CATALOG)\" \
		  $(MSG_CPP)
CAT_LIBPATH	=
CAT_LIB		= -lw -ldl


# For GNU K&R cc
#KR_CC           = gcc
#KR_CFLAGS       = -traditional -D_NO_PROTO

# For SPARCompiler K&R cc
KR_CC		= cc
KR_CFLAGS	= -Xs -D_NO_PROTO 

# For GNU Ansi cc
#ANSI_CC         = gcc
#ANSI_CFLAGS     = -ansi

# For SPARCompiler Ansi cc
ANSI_CC		= cc
ANSI_CFLAGS	= 

# For GNU c++
#CPLUS_CC        = gcc
#CPLUS_CFLAGS    = -xc++

# For SPARCompiler c++
CPLUS_CC	= CC
CPLUS_CFLAGS	= 

PPS_GLOBAL_INCLUDE = ../../include/global
PPS_LOCAL_INCLUDE = ../../include/local
ASFINC		= /ASF/$(ASFVER)/include
ASFLIB		= /ASF/$(ASFVER)/lib/$(MACH)
LIBLOCAL 	= ../../lib/$(MACH)/local


IMSTEST		= NOIMSTEST
DEBUG       = NODEBUG
PUSH_JOB_READY  = NO_PUSH_JOB_READY
CFLAGS		= -g -I$(UX_DIR)/include $(X_CFLAGS) \
		$(MOTIF_CFLAGS) $(CAT_CFLAGS) 				\
		-D$(PUSH_JOB_READY) -D$(DEBUG) -D$(IMSTEST)		\
		-DXOPEN_CATALOG -DSOLARIS -I$(SYBASE)/include	\
		-I$(PPS_GLOBAL_INCLUDE) -I$(PPS_LOCAL_INCLUDE)	\
		-I$(ASFINC)/imsdads
XOPEN_LIBS	=
UCB_LIBDIR	= /usr/ucblib

# For GNU compiler
#GCC_LINKFLAG   = -Xlinker

LIBSEARCHPATH	= $(GCC_LINKFLAG) -R$(MOTIF_LIBDIR):$(X_LIBDIR):$(UCB_LIBDIR)
LIBPATH		= $(MOTIF_LIBPATH) $(X_LIBPATH) $(CAT_LIBPATH) \
		  -L$(UX_LIBPATH) $(LIBSEARCHPATH)

LIBS		= $(LIBLOCAL)/libpps_nothread.a $(ASFLIB)/libims.a \
		$(ASFLIB)/libimsodl.a $(ASFLIB)/libutil.a  \
		$(ASFLIB)/libodl.a \
		-lux $(X_LIBS) $(CAT_LIBS) \
		-L$(SYBASE)/lib -lcs -lct -lcomn -lintl \
		-ltcl -lsybdb -lw -ldl -lm \
		-lsocket -lnsl -lgen -lelf $(XOPEN_LIBS) -lc

CC = $(ANSI_CC)
LD = $(ANSI_CC)
LD_FLAGS = $(ANSI_LDFLAGS)

OBJS = pps.o $(INTERFACES:.c=.o) $(APPL_OBJS) $(EXTRA_OBJS)
#OBJS = $(MAIN:.c=.o) $(INTERFACES:.c=.o) $(APPL_OBJS) $(EXTRA_OBJS)

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@

default:	$(MSG_DEPEND) $(EXECUTABLE) $(REPORT_TO_IMS)

source:
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS

all:
		$(MAKE) clean
		$(MAKE) default

catalog:	$(MSG_HEADER) $(MSG_CATALOG)



$(MSG_HEADER): /ub/pps/pps/R1Bprime/src/pps_gui/Makefile
	@echo "Generating $(MSG_HEADER)" ; \
	rm -f $@ ; \
	touch $@ ; \
	echo "/*---------------------------------------------------------------------" >> $@ ; \
	echo " *" >> $@ ; \
	echo " *        $@" >> $@ ; \
	echo " *        This file is included by all interface files in project" >> $@ ; \
	echo " *" >> $@ ; \
	echo " *-------------------------------------------------------------------*/" >> $@ ; \
	cap_name=`echo $@ | tr a-z. A-Z_` ; \
	echo "" >> $@ ; \
	echo "#ifndef   _$${cap_name}_" >> $@ ; \
	echo "#define   _$${cap_name}_" >> $@ ; \
	echo "" >> $@ ; \
	msg_set_no=1 ; \
	for i in $(MSG_FILES) ; do \
	msg_set_name=`awk '/^\\$$set/ { print $$2; exit }' $${i}` ; \
	echo "#define $${msg_set_name}  $${msg_set_no}" >> $@ ; \
	msg_set_no=`expr $${msg_set_no} + 1` ; \
	done ;\
	echo "" >> $@ ; \
	echo "#endif /* _$${cap_name}_ */" >> $@ ;

$(MSG_CATALOG): $(MSG_HEADER) $(MSG_FILES)
	@echo "Generating $(MSG_CATALOG)" ; \
	msg_set_no=1 ; \
	for i in $(MSG_FILES_STRIP) ; do \
	sed -e 's/^\$$set.*$$/\$$set    '$${msg_set_no}'/' $${i}.msg >> $(MSG_CATALOG).msg; \
	msg_set_no=`expr $${msg_set_no} + 1` ; \
	done; \
	gencat $(MSG_CATALOG) $(MSG_CATALOG).msg ; \
	rm -f $(MSG_CATALOG).msg ;

$(EXECUTABLE): $(OBJS) $(LIB_DEPEND)
	@echo "Linking    $(EXECUTABLE)"
	$(LD) $(LD_FLAGS) $(OBJS) $(LIBPATH) $(LIBS) -o $(EXECUTABLE)
	/bin/cp $(EXECUTABLE) ../../bin/$(MACH)/.
	@echo "Done" with $@

$(REPORT_TO_IMS): $(REPORT_TO_IMS).o pps_db.o send_status.o $(LIB_DEPEND)
	@echo "Linking $(REPORT_TO_IMS)"
	$(LD) $(LD_FLAGS) $@.o pps_db.o send_status.o $(LIBPATH) $(LIBS) -o $@
	/bin/cp $@ ../../bin/$(MACH)/.
	@echo "Done" with $@


clean:
	@echo "Cleaning pps_gui $(OBJS) $(REPORT_TO_IMS)" ; \
	/bin/rm -f pps_gui $(OBJS) $(REPORT_TO_IMS) $(REPORT_TO_IMS).o send_status.o

.PHONY: all catalog


#########################################################
# dependency rules for C files:
#########################################################
nojoy.o:		pps_common.h pps_util.h nojoy.h nojoy.c
pps.o: 			pps.h pps.c pps_util.h
pps_error_dialog.o:	pps_common.h pps_util.h \
			pps_error_dialog.h pps_error_dialog.c
pps_query.o:		pps_common.h pps_util.h pps_query.c
pps_file_selector.o:	pps_file_selector.h pps_file_selector.c
pps_list_util.o:	pps_util.h pps_list_util.c
pps_main.o:		pps_common.h pps_util.h pps_main.h pps_main.c
pps_plan.o:		pps_plan.h pps_plan.c
pps_plan_util.o:	pps.h pps_plan.h pps_file_selector.h pps_util.h \
			pps_plan_util.c
pps_policy.o:		pps.h pps_common.h pps_util.h pps_policy.h \
			pps_policy.c
pps_policy_util.o:	pps.h pps_common.h pps_util.h pps_policy_util.c
pps_query.o:		pps_common.h pps_util.h pps_query.h pps_query.c
pps_query_util.o:	pps.h pps_query.h pps_file_selector.h pps_util.h \
			pps_db.h pps_check_params.h pps_query_util.c
pps_report_to_ims.o: pps_db.h send_status.h pps_report_to_ims.c
pps_util.o:		pps_query.h pps_file_selector.h pps_util.h \
			pps_error_dialog.h pps_util.c
pps_check_params.o:	pps_util.h pps_db.h pps_check_params.h \
			pps_check_params.c
send_status.o: pps_db.h send_status.h send_status.c

#########################################################
# dependency rules for header files:
#########################################################
pps_db.h:		pps_util.h

%.c: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.c
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.c
 
%.h: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.h
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.h
