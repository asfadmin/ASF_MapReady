#########################################################################
#
# @(#)Makefile	1.35 96/11/12 18:22:54
#
#       MAKEFILE FOR STAND-ALONE IMS CODE APPLICATION.  
#
#       EXECUTABLE      is the name of the executable to be created 
#       MAIN            is the .c file containing your main() function 
#       INTERFACES      is a list of the generated C code files
#       APP_OBJS        is a (possibly empty) list of the object code
#                       files that form the non-interface portion of
#                       your application
#
#  	Date: 9/28/95 
#
#########################################################################
EXECUTABLE	= CP-IMS
ALTERNATE_NAME	= CP-IMGS
#INSTALLDIR      = $(ASF)/bin
INSTALLDIR      = ../../bin/$(MACH)
#CONFIGDIR	= $(LOCAL)/cp/config
CONFIGDIR	= ../../etc/install
CONFIGFILE	= ims-config.odl

MAIN		= CP_IMS.c
INTERFACES	= ims.c \
		  pthread_ext.c

LANGUAGE        = ANSI C
APPL_OBJS       = 

CHMOD		= /usr/bin/chmod
RM		= /bin/rm -f

TOOLINC         = $(ASF)/include/sps
LIBDIR		= $(ASF)/lib
OTHER_LIBDIR	= $(ASF)/lib/sgi

APPL_LIBS       = $(LIBDIR)/libsps.a
CP_LIBS         = $(LIBDIR)/libcp.a

SHELL		= /bin/sh
#X_LIBS          = -lXt_s -lX11_s
FG_INC          = $(ASF)/include/sys
FG_LIBS         = -lfg

IMS_INC         = $(ASF)/include/imsdads
IMS_LIB         = -lims

SYBASE_INC	= $(SYBASE)/include
SYBASE_LIB	= $(SYBASE)/lib

#MAN_PATH	= $(ASF)/man
MAN_PATH	= ../../etc/man
MAN_SRC		= $(EXECUTABLE).man
MAN_EXT		= 1

CFLAGS          = -D_SGI_SOURCE -D_SGI_MP_SOURCE -D_SGI_REENTRANT_FUNCTIONS \
		  -g -D__sgi -signed -G 0\
                  -I$(FG_INC) -I$(IMS_INC) -I$(SYBASE_INC) \
                  -I./ -I$(TOOLINC) -D_DEBUG

LIBS            = -L$(LIBDIR) -L$(OTHER_LIBDIR) -L$(SYBASE_LIB) \
		    $(APPL_LIBS) $(CP_LIBS) \
		    $(IMS_LIB) $(FG_LIBS) -lsybdb -lct -lcs -ltcl -linsck\
		    -lc -lPW -lm
#	    -lsun -lbsd -lmld -lc -lPW -lm
 
SRCS		= $(MAIN) $(INTERFACES)
OBJS		= $(SRCS:.c=.o) $(APPL_OBJS)
H_SRCS		= ims.h version.h

app:	all
all:	$(INSTALLDIR)/$(EXECUTABLE) $(CONFIGDIR)/$(CONFIGFILE)

$(INSTALLDIR)/$(EXECUTABLE):	$(EXECUTABLE)

#
# The installation of $(ALTERNATE_NAME) is done relative to $(INSTALLDIR)
# so that the symbolic link uses a relative rather than an absolute pathname.
#
$(EXECUTABLE): $(OBJS) $(INTERFACES) $(APPL_LIBS)
	@echo Linking $(EXECUTABLE)
	$(CC)  $(OBJS) $(LIBS) -o $(EXECUTABLE)
	-$(CHMOD) 775 $(EXECUTABLE)
	-$(RM) $(INSTALLDIR)/$(EXECUTABLE)
	install -v -m 775 -F $(INSTALLDIR) $(EXECUTABLE)
	-$(RM) $(INSTALLDIR)/$(ALTERNATE_NAME)
	(cd $(INSTALLDIR); ln -s ./$(EXECUTABLE) $(ALTERNATE_NAME) )

$(CONFIGDIR)/$(CONFIGFILE):	$(CONFIGFILE)
	-$(RM) $(CONFIGDIR)/$(CONFIGFILE)
	install -v -m 664 -F $(CONFIGDIR) $(CONFIGFILE)
	@echo "Done"

.SUFFIXES: 
.SUFFIXES: .o .c 
.c.o:
	@echo Compiling $<
	$(CC) -c $(CFLAGS) $<
	-$(CHMOD) 664 $@
$(SRCS):
	sccs get $@
$(H_SRCS):
	sccs get $@
clean:
	-$(RM) $(OBJS) $(EXECUTABLE)

install.man:	$(MAN_SRC)
	install -v -m 664 -src $(MAN_SRC) \
		-F $(MAN_PATH)/man$(MAN_EXT) $(EXECUTABLE).$(MAN_EXT)
#
# Dependencies
#
# DO NOT DELETE THIS LINE -- make depend depends on it.
CP_IMS.o: $(ASF)/include/sps/pthread_wrapper.h $(ASF)/include/sps/logUtils.h
CP_IMS.o: $(ASF)/include/sps/asf_syslog.h $(ASF)/include/sps/asf.h
CP_IMS.o: $(ASF)/include/sps/odl.h $(ASF)/include/sps/Msg.h ims.h
CP_IMS.o: $(ASF)/include/imsdads/ims_query.h
CP_IMS.o: $(ASF)/include/imsdads/ims_dbms.h $(SYBASE)/include/sybfront.h
CP_IMS.o: $(SYBASE)/include/cstypes.h $(SYBASE)/include/csconfig.h
CP_IMS.o: $(SYBASE)/include/sybdb.h $(SYBASE)/include/syblogin.h
CP_IMS.o: $(ASF)/include/imsdads/ims_const.h
CP_IMS.o: $(ASF)/include/imsdads/ims_msg.h
CP_IMS.o: $(ASF)/include/imsdads/ims_qi.h
CP_IMS.o: $(ASF)/include/imsdads/ims_cmnQuery.h version.h
ims.o: $(ASF)/include/sys/framegen.h $(ASF)/include/sps/asf_syslog.h
ims.o: $(ASF)/include/sps/logUtils.h $(ASF)/include/sps/Time.h
ims.o: $(ASF)/include/sps/Obj.h $(ASF)/include/sps/Msg.h
ims.o: $(ASF)/include/sps/String.h $(ASF)/include/sps/Unit.h ims.h
ims.o: $(ASF)/include/imsdads/ims_query.h
ims.o: $(ASF)/include/imsdads/ims_dbms.h $(SYBASE)/include/sybfront.h
ims.o: $(SYBASE)/include/cstypes.h $(SYBASE)/include/csconfig.h
ims.o: $(SYBASE)/include/sybdb.h $(SYBASE)/include/syblogin.h
ims.o: $(ASF)/include/imsdads/ims_const.h
ims.o: $(ASF)/include/imsdads/ims_msg.h $(ASF)/include/imsdads/ims_qi.h
ims.o: $(ASF)/include/imsdads/ims_cmnQuery.h $(ASF)/include/sps/odl.h
ims.o: $(ASF)/include/imsdads/ims_archive.h
ims.o: $(ASF)/include/imsdads/ims_keyword.h
ims.o: $(ASF)/include/imsdads/ims_ask.h
pthread_ext.o: $(ASF)/include/sps/pthread_wrapper.h
ims.o: $(ASF)/include/imsdads/ims_query.h
ims.o: $(ASF)/include/imsdads/ims_dbms.h $(SYBASE)/include/sybfront.h
ims.o: $(SYBASE)/include/cstypes.h $(SYBASE)/include/csconfig.h
ims.o: $(SYBASE)/include/sybdb.h $(SYBASE)/include/syblogin.h
ims.o: $(ASF)/include/imsdads/ims_const.h
ims.o: $(ASF)/include/imsdads/ims_msg.h $(ASF)/include/imsdads/ims_qi.h
ims.o: $(ASF)/include/imsdads/ims_cmnQuery.h $(ASF)/include/sps/odl.h
ims.o: $(ASF)/include/sps/Msg.h
