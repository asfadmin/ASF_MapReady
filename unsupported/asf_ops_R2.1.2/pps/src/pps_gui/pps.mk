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
########################################################################
# 
#	List of supported compiler(s):
#
#	gcc - Version 2.6.1
#	cc  - SPARCompiler Version 3.0.1
#	CC  - SPARCompiler Version 3.0.1  
#
#       @(#)pps.mk	1.1 11/21/96
########################################################################

EXECUTABLE	= ppsgui
MAIN		= pps.c
INTERFACES	= pps_query.c \
	nojoy.c \
	pps_main.c \
	pps_plan.c \
	pps_policy.c \
	pps_file_selector.c \
	pps_error_dialog.c 

LANGUAGE	= ANSI C
APPL_OBJS	=  pps_query_util.o pps_plan_util.o pps_policy_util.o pps_util.o pps_db.o

APPL_MSG_FILES	=
MSG_FILES	= $(INTERFACES:.c=.msg) $(APPL_MSG_FILES)
MSG_FILES_STRIP	= $(INTERFACES:.c=) $(APPL_MSG_FILES:.msg=)
MSG_CATALOG	= $(EXECUTABLE).cat
MSG_HEADER	= $(EXECUTABLE)_cat.h
MSG_CPP		= 
MSG_DEPEND	= 

SHELL		= /bin/sh

UX_DIR		= /usr/local/uimx2.9
UX_LIBPATH	= $(UX_DIR)/lib
X_LIBS          = -lXm -lXt -lX11 -lXext


X_LIBDIR	= /usr/openwin/lib
X_LIBPATH	= -L$(X_LIBDIR)
MOTIF_LIBDIR	= /opt/SUNWmotif/lib
MOTIF_LIBPATH	= -L$(MOTIF_LIBDIR)
X_CFLAGS	= -I/usr/openwin/include
MOTIF_CFLAGS	= -DMOTIF -I/opt/SUNWmotif/include


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

PPS_LINCLUDE = ../../include/global

CFLAGS		= -g -I$(UX_DIR)/include $(X_CFLAGS) $(MOTIF_CFLAGS) $(CAT_CFLAGS) \
		  -DXOPEN_CATALOG -DSOLARIS -I/db/sybase/sol/include \
		  -I$(PPS_LINCLUDE)
XOPEN_LIBS	=
UCB_LIBDIR	= /usr/ucblib

# For GNU compiler
#GCC_LINKFLAG   = -Xlinker

LIBSEARCHPATH	= $(GCC_LINKFLAG) -R$(MOTIF_LIBDIR):$(X_LIBDIR):$(UCB_LIBDIR)
LIBPATH		= $(MOTIF_LIBPATH) $(X_LIBPATH) $(CAT_LIBPATH) \
		  -L$(UX_LIBPATH) $(LIBSEARCHPATH)
LIBS		= -lux $(X_LIBS) $(CAT_LIBS) -L/db/sybase/sol/lib -lcs -lct -lcomn -lintl -ltcl -lw -ldl -lm \
			-lsocket -lnsl -lgen -lelf $(XOPEN_LIBS) -lc


OBJS = $(MAIN:.c=.o) $(INTERFACES:.c=.o) $(APPL_OBJS) $(EXTRA_OBJS)

all:		$(MSG_DEPEND) $(EXECUTABLE)

catalog:	$(MSG_HEADER) $(MSG_CATALOG)



$(MSG_HEADER): /ub/pps/pps/R1Bprime/src/pps_gui/pps.mk
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

$(EXECUTABLE): $(OBJS)
	@echo "Linking    $(EXECUTABLE)"
	$(LD) $(LD_FLAGS) $(OBJS) $(LIBPATH) $(LIBS) -o $(EXECUTABLE)
	@echo "Done"


.SUFFIXES: 
.SUFFIXES: .o .c .c

.c.o:
	@echo Compiling $< [$(LANGUAGE)] [UX-CODE]
	$(CC) -c $(CFLAGS) $< -o $@
.c.o:
	@echo Compiling $< [$(LANGUAGE)] [UX-CODE]
	$(CC) -c $(CFLAGS) $< -o $@


clean:
	@echo "Cleaning pps $(OBJS)" ; \
	/bin/rm -f pps $(OBJS)

.PHONY: all catalog

CC = \
@`if [ "$(LANGUAGE)" = "C++" ]; then echo $(CPLUS_CC) $(CPLUS_CFLAGS);fi` \
`if [ "$(LANGUAGE)" = "ANSI C" ]; then echo $(ANSI_CC) $(ANSI_CFLAGS); fi`\
`if [ "$(LANGUAGE)" = "KR-C" ]; then echo $(KR_CC) $(KR_CFLAGS); fi`



LD = \
@`if [ "$(LANGUAGE)" = "C++" ]; then echo $(CPLUS_CC);fi` \
`if [ "$(LANGUAGE)" = "ANSI C" ]; then echo $(ANSI_CC); fi`\
`if [ "$(LANGUAGE)" = "KR-C" ]; then echo $(KR_CC); fi`

LD_FLAGS = \
`if [ "$(LANGUAGE)" = "C++" ]; then echo $(CPLUS_LDFLAGS);fi`\
`if [ "$(LANGUAGE)" = "ANSI C" ]; then echo $(ANSI_LDFLAGS); fi`\
`if [ "$(LANGUAGE)" = "KR-C" ]; then echo $(KR_LDFLAGS); fi`

