#########################################################################
#
#       @(#)Makefile	1.37 97/01/17 11:41:01
#
#       MAKEFILE FOR STAND-ALONE XT CODE APPLICATION.  
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
#       This template is used for makefiles which do not reference 
#       the Ux runtime library.
#
#########################################################################


COPYRIGHT 	= CP_copyright
EXECUTABLE	= CP
MAIN		= CP.c
INTERFACES	= 	CPmainQ.c \
	CPsubsysQ.c \
	CPerrorBox.c \
	CPfileBox.c \
	CPinfoBox.c \
	CPquestionBox.c \
	CPdetailedInfo.c \
	CPhelp.c 

LANGUAGE        = ANSI C
APPL_OBJS       = UxXt.o UxMethod.o \
                   cpreadThread.o cprtns.o cprtns_xwp.o cpworkProc.o \
                   profile.o checkODL.o validate.o \
                   cpmenu.o cpbuildMsg.o cpconfig.o cpmainrtns.o \
                   listcore.o listmstrq.o listpid.o \
                   pps.o odl.o text.o sb.o debug.o \
                   que_sys.o que_xwp.o serverSocketXport.o xutils.o utils.o \
                   purify.o 

SHELL		= /bin/sh

UX_DIR		= /usr/local/uimx
UX_LIBPATH	= -L$(UX_DIR)/lib
X_LIBS		= -lXm -lXt -lX11
UXCGEN		= $(UX_DIR)/bin/uxcgen


X_LIBPATH	=
MOTIF_LIBPATH	=
X_CFLAGS	= 
MOTIF_CFLAGS	= -DMOTIF

KR_CC           = cc
ANSI_CC         = cc
CPLUS_CC        = CC
KR_CFLAGS       = -D_NO_PROTO -cckr
ANSI_CFLAGS     = 
CPLUS_CFLAGS    = 
ASF_CFLAGS = -D_POSIX1C -D_SGI_REENTRANT_FUNCTIONS \
             -D_DIRECT_TO_SYSLOG -D__sgi -DDO_THREADS \
             -I$(CP_INCLUDE_DIR) \
             -I$(ASF_INCLUDE_DIR) \
             -DDEBUGLEVEL=3 -D_DEBUG -I./ 

CFLAGS          =  -DSYSV -DXT_CODE -DXOPEN_CATALOG \
		  -signed -G 0 -woff 1047,1110,1009,1164\
		  $(ASF_CFLAGS) $(X_CFLAGS) $(MOTIF_CFLAGS)

LIBPATH         = $(X_LIBPATH) $(MOTIF_LIBPATH)
ASF_DIR = $(ASF)/lib
#ASF_DIR = /user/pearl/latest/sps
#ASF_DIR = /user/src/cp/lib/sgi/global
CP_INCLUDE_DIR = $(ASF)/include/cp
ASF_INCLUDE_DIR = $(ASF)/include/sps

#INSTALLDIR	= $(ASF)/bin
INSTALLDIR	= ../../bin/$(MACH)
MAN_SRC        = $(EXECUTABLE).man
#MAN_PATH        = $(ASF)/man
MAN_PATH        = ../../etc/man
MAN_EXT         = 1


EXECUTABLE_AD	= $(EXECUTABLE).ad
COPYRIGHT_AD	= $(COPYRIGHT).ad
#XAPPLOADDIR	= $(LOCAL)/app-defaults
XAPPLOADDIR	= ../../etc/app-defaults
LIBS            = $(ASF_DIR)/libsps.a $(ASF_DIR)/libcp.a \
                  $(X_LIBS) -lc -lPW 
SCRIPTS		= cp-kill remote-kill SETI
#LIBS            = $(ASF_DIR)/libsps.a $(ASF_DIR)/libcp.a \
#                 $(X_LIBS) -lsun -lbsd -lmld -lc -lPW -lm

OBJS = $(INTERFACES:.c=.o) $(MAIN:.c=.o) $(APPL_OBJS)

app:	all

all:	$(INSTALLDIR)/$(EXECUTABLE) $(INSTALLDIR)/$(COPYRIGHT) \
	$(XAPPLOADDIR)/$(EXECUTABLE_AD) $(XAPPLOADDIR)/$(COPYRIGHT_AD)

$(INSTALLDIR)/$(EXECUTABLE):	$(EXECUTABLE)	$(SCRIPTS)
	-install -v -m 775 -F $(INSTALLDIR) $(EXECUTABLE)
	-install -v -m 775 -F $(INSTALLDIR) $(SCRIPTS)

$(EXECUTABLE): $(INTERFACES) $(OBJS) $(ASF_DIR)/libsps.a
	@echo "Linking    $(EXECUTABLE)"
	$(LD) $(LD_FLAGS) $(OBJS) $(LIBPATH) $(LIBS) -o $(EXECUTABLE)
	-chmod g+w $@

$(XAPPLOADDIR)/$(EXECUTABLE_AD):	$(EXECUTABLE_AD)
	-install -v -m 664 -F $(XAPPLOADDIR) $(EXECUTABLE_AD)

$(INSTALLDIR)/$(COPYRIGHT):	$(COPYRIGHT)
	-install -v -m 775 -F $(INSTALLDIR) $(COPYRIGHT)
	-install -v -m 664 -F $(XAPPLOADDIR) $(COPYRIGHT_AD)

$(COPYRIGHT): $(COPYRIGHT).c

$(XAPPLOADDIR)/$(COPYRIGHT_AD):	$(COPYRIGHT_AD)
	-install -v -m 664 -F $(XAPPLOADDIR) $(COPYRIGHT_AD)

.SUFFIXES: 
.SUFFIXES: .o .c .c .i

#.i.c:
#	$(UXCGEN) -x -h $<

.c.o:
	@echo Compiling $< [$(LANGUAGE)] [XT-CODE]
	$(CC) -c -g $(CFLAGS) $< -o $@
	-chmod g+w $@
.c.o:
	@echo Compiling $< [$(LANGUAGE)] [XT-CODE]
	$(CC) -c -g $(CFLAGS) $< -o $@
	-chmod g+w $@

.c: ; $(CC) $(CFLAGS) $@.c $(LIBS) -o $@



UxMethod.o: UxMethod.c
	@echo Compiling $< [ANSI-C] [XT-CODE]
	@$(ANSI_CC) $(ANSI_CFLAGS) $(CFLAGS) -c $<


clean:
	-rm $(OBJS) $(EXECUTABLE) $(COPYRIGHT) core
#	-rm $(OBJS) $(INTERFACES) $(INTERFACES:.c=.h)


install.man::
	-install -v -m 664 -src $(MAN_SRC) \
                -F $(MAN_PATH)/man$(MAN_EXT) $(EXECUTABLE).$(MAN_EXT)



#CC = \
#@`if [ "$(LANGUAGE)" = "C++" ]; then echo $(CPLUS_CC) $(CPLUS_CFLAGS);fi` \
#`if [ "$(LANGUAGE)" = "ANSI C" ]; then echo $(ANSI_CC) $(ANSI_CFLAGS); fi`\
#`if [ "$(LANGUAGE)" = "KR-C" ]; then echo $(KR_CC) $(KR_CFLAGS); fi`



LD=cc 
#LD = \
#@`if [ "$(LANGUAGE)" = "C++" ]; then echo $(CPLUS_CC);fi` \
#`if [ "$(LANGUAGE)" = "ANSI C" ]; then echo $(ANSI_CC); fi`\
#`if [ "$(LANGUAGE)" = "KR-C" ]; then echo $(KR_CC); fi`

#LD_FLAGS = \
#`if [ "$(LANGUAGE)" = "C++" ]; then echo $(CPLUS_LDFLAGS);fi`\
#`if [ "$(LANGUAGE)" = "ANSI C" ]; then echo $(ANSI_LDFLAGS); fi`\
#`if [ "$(LANGUAGE)" = "KR-C" ]; then echo $(KR_LDFLAGS); fi`
# DO NOT DELETE THIS LINE -- make depend depends on it.

CP.o: CPmainQ.h UxXt.h cplogs.h
CPdetailedInfo.o: UxXt.h CPdetailedInfo.h
CPerrorBox.o: UxXt.h CPerrorBox.h
CPfileBox.o: UxXt.h CPfileBox.h
CPhelp.o: UxXt.h CPhelp.h
CPinfoBox.o: UxXt.h CPinfoBox.h
CPmainQ.o: UxXt.h version.h help.h cpdefines.h CPmainQ.h
CPquestionBox.o: UxXt.h que_xwp.h listcore.h cpdefines.h CPquestionBox.h
CPsubsysQ.o: UxXt.h version.h help.h CPsubsysQ.h
UxXt.o: UxXt.h
checkODL.o: cpdefines.h cpbuildMsg.h listpid.h listcore.h listmstrq.h
checkODL.o: que_sys.h cpMsgs.h validate.h
cpbuildMsg.o: cpdefines.h cpconfig.h validate.h listcore.h que_sys.h
cpbuildMsg.o: cpbuildMsg.h listpid.h listmstrq.h cplogs.h cprtns.h que_xwp.h
cpbuildMsg.o: cprtns_xwp.h cpworkProc.h utils.h cpMsgs.h
cpconfig.o: cpdefines.h cpconfig.h validate.h cplogs.h utils.h cprtns.h
cpconfig.o: listpid.h listcore.h que_xwp.h
cpmainrtns.o: CPmainQ.h UxXt.h CPsubsysQ.h CPdetailedInfo.h CPfileBox.h
cpmainrtns.o: CPhelp.h cpdefines.h cplogs.h cpworkProc.h que_xwp.h listcore.h
cpmainrtns.o: cpconfig.h validate.h cprtns.h listpid.h display.h listmstrq.h
cpmainrtns.o: que_sys.h pps.h version.h
cpmenu.o: UxXt.h cpdefines.h cplogs.h listcore.h listpid.h listmstrq.h
cpmenu.o: que_sys.h cpconfig.h validate.h cprtns.h que_xwp.h cprtns_xwp.h
cpmenu.o: cpmenu.h cpmainrtns.h cpworkProc.h xutils.h
cpreadThread.o: cpdefines.h cpreadThread.h serverSocketXport.h listcore.h
cpreadThread.o: listmstrq.h cprtns.h listpid.h que_xwp.h cprtns_xwp.h
cpreadThread.o: cplogs.h cpconfig.h validate.h utils.h pps.h
cprtns.o: UxXt.h cpdefines.h cprtns.h listpid.h listcore.h que_xwp.h
cprtns.o: cprtns_xwp.h listmstrq.h display.h cpconfig.h validate.h
cprtns.o: cpreadThread.h serverSocketXport.h que_sys.h cplogs.h utils.h
cprtns.o: xutils.h cpmainrtns.h cpbuildMsg.h CPmainQ.h CPquestionBox.h
cprtns.o: cpmenu.h cpworkProc.h pps.h
cprtns_xwp.o: cpdefines.h cplogs.h cprtns_xwp.h listcore.h listmstrq.h
cprtns_xwp.o: que_xwp.h cpworkProc.h cpconfig.h validate.h cpreadThread.h
cprtns_xwp.o: serverSocketXport.h listpid.h que_sys.h
cpworkProc.o: CPmainQ.h UxXt.h CPsubsysQ.h CPerrorBox.h CPinfoBox.h
cpworkProc.o: cpdefines.h cpconfig.h validate.h serverSocketXport.h
cpworkProc.o: listcore.h listpid.h listmstrq.h que_sys.h que_xwp.h
cpworkProc.o: CPquestionBox.h cplogs.h cprtns.h cprtns_xwp.h cpmainrtns.h
cpworkProc.o: utils.h xutils.h cpmenu.h cpbuildMsg.h pps.h
debug.o: CPmainQ.h UxXt.h CPsubsysQ.h CPerrorBox.h CPinfoBox.h cpdefines.h
debug.o: cpconfig.h validate.h serverSocketXport.h listcore.h listpid.h
debug.o: listmstrq.h que_sys.h que_xwp.h cprtns.h cprtns_xwp.h cpworkProc.h
debug.o: cplogs.h cpbuildMsg.h cpmenu.h
listcore.o: listcore.h cplogs.h
listmstrq.o: cpdefines.h listcore.h listmstrq.h cpworkProc.h que_xwp.h
listmstrq.o: cprtns.h listpid.h cprtns_xwp.h cplogs.h que_sys.h utils.h pps.h
listmstrq.o: validate.h
listpid.o: UxXt.h CPsubsysQ.h cpdefines.h listcore.h listpid.h cplogs.h
listpid.o: cpmenu.h
pps.o: cpdefines.h listcore.h listmstrq.h cpworkProc.h que_xwp.h cprtns.h
pps.o: listpid.h cplogs.h que_sys.h cpconfig.h validate.h cpbuildMsg.h pps.h
pps.o: UxXt.h
profile.o: cpdefines.h cpconfig.h validate.h cplogs.h
que_sys.o: cpdefines.h listcore.h que_sys.h cplogs.h listmstrq.h
que_xwp.o: listcore.h que_xwp.h cpdefines.h cplogs.h
sb.o: UxXt.h cpdefines.h cpworkProc.h que_xwp.h listcore.h listpid.h
sb.o: listmstrq.h
serverSocketXport.o: serverSocketXport.h
text.o: UxXt.h cpdefines.h cplogs.h cprtns_xwp.h listcore.h listmstrq.h
text.o: que_xwp.h utils.h xutils.h display.h CPmainQ.h
validate.o: validate.h
xutils.o: utils.h UxXt.h
CPdetailedInfo.o: UxXt.h
CPerrorBox.o: UxXt.h
CPfileBox.o: UxXt.h
CPhelp.o: UxXt.h
CPinfoBox.o: UxXt.h
CPmainQ.o: UxXt.h
CPquestionBox.o: UxXt.h
CPsubsysQ.o: UxXt.h
cpbuildMsg.o: listpid.h listcore.h listmstrq.h que_sys.h
cpconfig.o: validate.h
cprtns.o: listpid.h listcore.h que_xwp.h cpdefines.h
cprtns_xwp.o: listcore.h listmstrq.h que_xwp.h cpdefines.h
cpworkProc.o: que_xwp.h listcore.h cpdefines.h
listpid.o: listcore.h
que_xwp.o: listcore.h cpdefines.h
