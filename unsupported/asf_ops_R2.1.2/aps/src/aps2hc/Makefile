###############################################################################
#
# File:         Makefile
#
# Function:     Baseline Version of APS Makefile
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/aps2hc/SCCS/s.Makefile"
#
###############################################################################

ifdef CM_HOME
	APS_HOME = $(CM_HOME)/aps
endif

include $(APS_HOME)/src/aps_makedefs


###############################################################################
#
# Source Default Directory
#   Typically off of $APS_HOME/src... this is used to retrieve source
#
###############################################################################
 
SRC_DIR = $(APS_HOME)/src/aps2hc
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################
IALL =      -I. $(IDCE) $(IAPS) \
			$(IPPS) \
			$(OSIFLAGS) 

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################
LALL =      $(LDCE) $(LAPS) \
			$(LSYBASE) \
			$(OSLFLAGS) \
			$($(OS)LFLAGS)

RALL = $(foreach Llib, $(LALL), $(subst -L, -R, $(Llib)))

###############################################################################
#
# Library Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLIBS
#    If a line has two libraries the first is dependent on the second
#
#    The STATIC and DYNAMIC macro can be used to set the link option for
#    the libraries that follow them
#    The 3rd Party and system libs are defaulted to link dynamically 
#    (see aps_makedefs)
#    The MATH_LIB for some reason works best when it's last
#
###############################################################################
LIBS = $(DCE_LIB) ${OSLIBS} $(EVERY_LIB) 

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      -D$(MACH)
CFLAGS =    $(OSCFLAGS) $(DEFS)

###############################################################################
#
# Source and Target Definition
#   Add your source and exe filenames here
#       EXE      is the name of the executable to be created
#       LIB      is the name of the library to be created
#       SRC      is a list of the C source code files
#       HEADER_FILES is a list of the C Header files
#       F77_SRC  is a list (possibly empty) of the Fortran 77 source
#                code files
#       F77_INC  is a list (possibly empty) of the Fortran 77 include files
#
#
###############################################################################

EXE          =  aps_aps2hc_xmitClient

LIB          =  

SRC          =  aps2hc_main.c aps2hc_init.c aps2hc_send.c aps2hc_status.c

HEADER_FILES =  

F77_SRC      =  

F77_INC      =  

OBJS         = $(SRC:%.c=%.o) $(F77_SRC:%.for=%.o)

TARGETS = $(EXE) $(LIB)
 
DCE_INTERFACE = downlink_ingest

 
###############################################################################
#
# Make the Application
#    Add all targets to be made to the next line
#    If more exes or libs are needed make a copy of the set of 
#    needed commands
#
###############################################################################

all appl:  $(TARGETS) 

$(EXE): hst_types.h $(DCE_INTERFACE)_cstub.o $(OBJS)
	@echo
	@echo "Linking $(EXE) "
	@$(CC) -o $(EXE) $(DCE_CFLAGS) $(OBJS) $(DCE_INTERFACE)_cstub.o \
			$(LALL) $(RALL) $(LIBS)
	@echo "Done"

#####################
# Build interface   #
#####################
 
hst_types.h: hst_types.idl
	$(DCE_IDL) $(DCE_IDLFLAGS) $(IDCE) -Dtime_t=long -server none \
		-client none hst_types.idl 

#$(DCE_INTERFACE).h $(DCE_INTERFACE)_cstub.o: $(DCE_INTERFACE).idl $(DCE_INTERFACE).acf
$(DCE_INTERFACE).h $(DCE_INTERFACE)_cstub.o: $(DCE_INTERFACE).idl 
	$(DCE_IDL) $(DCE_IDLFLAGS) $(IDCE) -Dtime_t=long -server none \
		$(DCE_INTERFACE).idl

###############################################################################
#
# Source Code Retrieval
#
#  Note there is a brute force rule for apsfiledef.c because because it
#  needs to be retrieved from the GUI directory... This is where the
#  latest and greatest copy should be kept... It is retrieved from sccs
#  There are conditional compiles in here so the file is used by the 
#  create_aps_file application and the gui
#
###############################################################################

$(SRC) $(HEADER_FILES) $(F77_SRC) $(F77_INC) :
	@echo
	@echo "Retrieving $@ as of $(FILE_VERSION) "
	$(CM) $(CM_GET) $(FILE_VERSION) $(SRC_DIR)/$@

$(SRC_DIR)/../gui/apsfiledef.c :
	@echo "Invoking FORCE rules..."
	rm -f apsfiledef.c apsfiledef.o
	@echo "Retrieving apsfiledef.c as of $(FILE_VERSION) "
	$(CM) $(CM_GET) $(FILE_VERSION) -G$@ $@
		

 
###############################################################################
#
# C Compilation.
#
###############################################################################

apsfiledef.o : $(SRC_DIR)/../gui/apsfiledef.c
	@echo
	@echo "Compiling $<"
	@$(CC) $(CFLAGS) $(IALL) -c $< 

%.o:%.c
	@echo
	@echo "Compiling $<"
	@$(CC) $(CFLAGS) $(IALL) -c $< 


###############################################################################
#
# Fortran Compilation.
#
###############################################################################

%.o:%.for $(F77_INC)
	@echo
	@echo "Compiling $<"
	$(F77) $<


###############################################################################
#
# Object and executable cleaning.
#
###############################################################################

clean:
	@echo
	@echo "Cleaning " $(TARGETS) downlink_ingest.h hst_types.h
	rm -f $(OBJS) $(TARGETS) downlink_ingest.h downlink_ingest_cstub.o \
								hst_types.h

	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)


###############################################################################
#
# Definitions
#  Add post make definitions
#
###############################################################################
include $(APS_HOME)/src/aps_post_makedefs


###############################################################################
#
# Makedepend (create all the include file dependencies
#	(destroys everything after the "DO NOT DELETE ..." line)
#
###############################################################################
depend: hst_types.h $(DCE_INTERFACE).h $(SRC) $(F77_SRC)
	@if [ -w Makefile ] ; \
	then \
		echo makedepend -- $(CFLAGS) -- \
			$(IALL) -I$(APS_HOME)/src/makedepend_files $^; \
		makedepend -- $(CFLAGS) -- \
			$(IALL) -I$(APS_HOME)/src/makedepend_files $^; \
		echo rm -f Makefile.bak ; \
		rm -f Makefile.bak ; \
	else \
		echo "*** Unwriteable Makefile: Need to check it out? ***" ; \
	fi

# DO NOT DELETE THIS LINE -- make depend depends on it.

hst_types.o: /opt/dce/share/include/dce/idlbase.h
hst_types.o: /opt/dce/share/include/dce/ndrtypes.h
hst_types.o: /opt/dce/share/include/dce/nbase.h
downlink_ingest.o: /opt/dce/share/include/dce/idlbase.h
downlink_ingest.o: /opt/dce/share/include/dce/ndrtypes.h
downlink_ingest.o: /opt/dce/share/include/dce/rpc.h
downlink_ingest.o: /opt/dce/share/include/dce/nbase.h
downlink_ingest.o: /opt/dce/share/include/dce/lbase.h
downlink_ingest.o: /opt/dce/share/include/dce/rpcsts.h
downlink_ingest.o: /opt/dce/share/include/dce/rpcbase.h
downlink_ingest.o: /opt/dce/share/include/dce/rpctypes.h
downlink_ingest.o: /opt/dce/share/include/dce/iovector.h
downlink_ingest.o: /opt/dce/share/include/dce/rpcpvt.h
downlink_ingest.o: /opt/dce/share/include/dce/ncastat.h
downlink_ingest.o: /opt/dce/share/include/dce/uuid.h
downlink_ingest.o: /opt/dce/share/include/dce/ndrold.h
downlink_ingest.o: /opt/dce/share/include/dce/twr.h hst_types.h
aps2hc_main.o: aps2hc.h /usr/include/stdio.h /usr/include/sys/feature_tests.h
aps2hc_main.o: /usr/include/stdlib.h /usr/include/errno.h
aps2hc_main.o: /usr/include/sys/errno.h /usr/include/ctype.h
aps2hc_main.o: /usr/include/string.h /usr/include/unistd.h
aps2hc_main.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
aps2hc_main.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
aps2hc_main.o: /opt/dce/share/include/dce/dce_error.h downlink_ingest.h
aps2hc_main.o: /opt/dce/share/include/dce/idlbase.h
aps2hc_main.o: /opt/dce/share/include/dce/ndrtypes.h
aps2hc_main.o: /opt/dce/share/include/dce/rpc.h
aps2hc_main.o: /opt/dce/share/include/dce/nbase.h
aps2hc_main.o: /opt/dce/share/include/dce/lbase.h
aps2hc_main.o: /opt/dce/share/include/dce/rpcsts.h
aps2hc_main.o: /opt/dce/share/include/dce/rpcbase.h
aps2hc_main.o: /opt/dce/share/include/dce/rpctypes.h
aps2hc_main.o: /opt/dce/share/include/dce/iovector.h
aps2hc_main.o: /opt/dce/share/include/dce/rpcpvt.h
aps2hc_main.o: /opt/dce/share/include/dce/ncastat.h
aps2hc_main.o: /opt/dce/share/include/dce/uuid.h
aps2hc_main.o: /opt/dce/share/include/dce/ndrold.h
aps2hc_main.o: /opt/dce/share/include/dce/twr.h hst_types.h hst_status.h
aps2hc_main.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
aps2hc_main.o: /usr/include/syslog.h /usr/include/sys/syslog.h
aps2hc_main.o: /home/aps/r2.1.1/include/local/aps_defs.h
aps2hc_init.o: aps2hc.h /usr/include/stdio.h /usr/include/sys/feature_tests.h
aps2hc_init.o: /usr/include/stdlib.h /usr/include/errno.h
aps2hc_init.o: /usr/include/sys/errno.h /usr/include/ctype.h
aps2hc_init.o: /usr/include/string.h /usr/include/unistd.h
aps2hc_init.o: /usr/include/sys/types.h /usr/include/sys/isa_defs.h
aps2hc_init.o: /usr/include/sys/machtypes.h /usr/include/sys/unistd.h
aps2hc_init.o: /opt/dce/share/include/dce/dce_error.h downlink_ingest.h
aps2hc_init.o: /opt/dce/share/include/dce/idlbase.h
aps2hc_init.o: /opt/dce/share/include/dce/ndrtypes.h
aps2hc_init.o: /opt/dce/share/include/dce/rpc.h
aps2hc_init.o: /opt/dce/share/include/dce/nbase.h
aps2hc_init.o: /opt/dce/share/include/dce/lbase.h
aps2hc_init.o: /opt/dce/share/include/dce/rpcsts.h
aps2hc_init.o: /opt/dce/share/include/dce/rpcbase.h
aps2hc_init.o: /opt/dce/share/include/dce/rpctypes.h
aps2hc_init.o: /opt/dce/share/include/dce/iovector.h
aps2hc_init.o: /opt/dce/share/include/dce/rpcpvt.h
aps2hc_init.o: /opt/dce/share/include/dce/ncastat.h
aps2hc_init.o: /opt/dce/share/include/dce/uuid.h
aps2hc_init.o: /opt/dce/share/include/dce/ndrold.h
aps2hc_init.o: /opt/dce/share/include/dce/twr.h hst_types.h hst_status.h
aps2hc_init.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
aps2hc_init.o: /usr/include/syslog.h /usr/include/sys/syslog.h
aps2hc_init.o: /home/aps/r2.1.1/include/local/aps_defs.h aps2hc_status.h
aps2hc_send.o: /usr/include/time.h /usr/include/sys/feature_tests.h aps2hc.h
aps2hc_send.o: /usr/include/stdio.h /usr/include/stdlib.h
aps2hc_send.o: /usr/include/errno.h /usr/include/sys/errno.h
aps2hc_send.o: /usr/include/ctype.h /usr/include/string.h
aps2hc_send.o: /usr/include/unistd.h /usr/include/sys/types.h
aps2hc_send.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
aps2hc_send.o: /usr/include/sys/unistd.h
aps2hc_send.o: /opt/dce/share/include/dce/dce_error.h downlink_ingest.h
aps2hc_send.o: /opt/dce/share/include/dce/idlbase.h
aps2hc_send.o: /opt/dce/share/include/dce/ndrtypes.h
aps2hc_send.o: /opt/dce/share/include/dce/rpc.h
aps2hc_send.o: /opt/dce/share/include/dce/nbase.h
aps2hc_send.o: /opt/dce/share/include/dce/lbase.h
aps2hc_send.o: /opt/dce/share/include/dce/rpcsts.h
aps2hc_send.o: /opt/dce/share/include/dce/rpcbase.h
aps2hc_send.o: /opt/dce/share/include/dce/rpctypes.h
aps2hc_send.o: /opt/dce/share/include/dce/iovector.h
aps2hc_send.o: /opt/dce/share/include/dce/rpcpvt.h
aps2hc_send.o: /opt/dce/share/include/dce/ncastat.h
aps2hc_send.o: /opt/dce/share/include/dce/uuid.h
aps2hc_send.o: /opt/dce/share/include/dce/ndrold.h
aps2hc_send.o: /opt/dce/share/include/dce/twr.h hst_types.h hst_status.h
aps2hc_send.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
aps2hc_send.o: /usr/include/syslog.h /usr/include/sys/syslog.h
aps2hc_send.o: /home/aps/r2.1.1/include/local/aps_defs.h aps2hc_status.h
aps2hc_status.o: aps2hc.h /usr/include/stdio.h
aps2hc_status.o: /usr/include/sys/feature_tests.h /usr/include/stdlib.h
aps2hc_status.o: /usr/include/errno.h /usr/include/sys/errno.h
aps2hc_status.o: /usr/include/ctype.h /usr/include/string.h
aps2hc_status.o: /usr/include/unistd.h /usr/include/sys/types.h
aps2hc_status.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
aps2hc_status.o: /usr/include/sys/unistd.h
aps2hc_status.o: /opt/dce/share/include/dce/dce_error.h downlink_ingest.h
aps2hc_status.o: /opt/dce/share/include/dce/idlbase.h
aps2hc_status.o: /opt/dce/share/include/dce/ndrtypes.h
aps2hc_status.o: /opt/dce/share/include/dce/rpc.h
aps2hc_status.o: /opt/dce/share/include/dce/nbase.h
aps2hc_status.o: /opt/dce/share/include/dce/lbase.h
aps2hc_status.o: /opt/dce/share/include/dce/rpcsts.h
aps2hc_status.o: /opt/dce/share/include/dce/rpcbase.h
aps2hc_status.o: /opt/dce/share/include/dce/rpctypes.h
aps2hc_status.o: /opt/dce/share/include/dce/iovector.h
aps2hc_status.o: /opt/dce/share/include/dce/rpcpvt.h
aps2hc_status.o: /opt/dce/share/include/dce/ncastat.h
aps2hc_status.o: /opt/dce/share/include/dce/uuid.h
aps2hc_status.o: /opt/dce/share/include/dce/ndrold.h
aps2hc_status.o: /opt/dce/share/include/dce/twr.h hst_types.h hst_status.h
aps2hc_status.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
aps2hc_status.o: /usr/include/syslog.h /usr/include/sys/syslog.h
aps2hc_status.o: /home/aps/r2.1.1/include/local/aps_defs.h aps2hc_status.h
