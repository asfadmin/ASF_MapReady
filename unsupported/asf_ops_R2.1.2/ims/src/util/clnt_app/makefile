# @(#)makefile	6.1  03/09/98
###############################################################################
#
# File:         makefile
#
# Function:     Make the utility applications.
#
# Author:       S. Hardman
#
# Date:         9/28/94
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IUTIL =     -I../lincl
IGLB =      -I../../gincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IUTIL) $(IGLB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

###############################################################################
#
# Library Path Definitions
#
###############################################################################

LIMS =      -L../../../lib/$(MACH)/local
LEXP =      -L../../../lib/$(MACH)/global
LSYB =      -L$(SYBASE)/lib

LALL =      $(LIMS) $(LEXP) $(LASF) $(LSYB) $(MACHLFLAGS)

###############################################################################
#
# Library Definitions
#
###############################################################################

GLBLIB =    -lglb
IMSLIB =    -lims
DBLIB =     -lsybdb
CTLIB =     -lct
CSLIB =     -lcs
TCLLIB =    -ltcl
COMNLIB =   -lcomn

LIBS =      $(GLBLIB) $(IMSLIB) $(DBLIB) $(CTLIB) $(CSLIB) $(TCLLIB) \
            $(COMNLIB) $(MACHLIBS) -lintl -lm

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      
CFLAGS =    $(MACHCFLAGS) $(DEFS)

###############################################################################
#
# Target Definition
#
###############################################################################

SRC =       *.c
OBJ =       $(SRC:%.c=%.o)
EXE =       \
            ../../../bin/$(MACH)/ims_archiveCheck \
            ../../../bin/$(MACH)/ims_archiveSpace \
            ../../../bin/$(MACH)/ims_cgt \
            ../../../bin/$(MACH)/ims_cleaner \
            ../../../bin/$(MACH)/ims_dbq \
            ../../../bin/$(MACH)/ims_frp_server \
            ../../../bin/$(MACH)/ims_jobMaster \
            ../../../bin/$(MACH)/ims_job_tool \
            ../../../bin/$(MACH)/ims_mrd \
            ../../../bin/$(MACH)/ims_oagdr \
            ../../../bin/$(MACH)/ims_orderCleaner \
            ../../../bin/$(MACH)/ims_shm_tool \
            ../../../bin/$(MACH)/ims_stragglers \
            ../../../bin/$(MACH)/ims_syslog 

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

../../../bin/$(MACH)/ims_archiveCheck:  ims_archiveCheck.o
	@echo
	@echo "Creating Executable ims_archiveCheck"
	$(CC) -o $@ ims_archiveCheck.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_archiveSpace:  ims_archiveSpace.o
	@echo
	@echo "Creating Executable ims_archiveSpace"
	$(CC) -o $@ ims_archiveSpace.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_cgt:  ims_cgt.o
	@echo
	@echo "Creating Executable ims_cgt"
	$(CC) -o $@ ims_cgt.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_cleaner:  ims_cleaner.o
	@echo
	@echo "Creating Executable ims_cleaner"
	$(CC) -o $@ ims_cleaner.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_dbq:  ims_dbq.o
	@echo
	@echo "Creating Executable ims_dbq"
	$(CC) -o $@ ims_dbq.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_frp_server:  ims_frp_server.o ims_frpio.o
	@echo
	@echo "Creating Executable ims_frp_server"
	$(CC) -o $@ ims_frp_server.o ims_frpio.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_jobMaster:  ims_jobMaster.o
	@echo
	@echo "Creating Executable ims_jobMaster"
	$(CC) -o $@ ims_jobMaster.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_job_tool:  ims_job_tool.o
	@echo
	@echo "Creating Executable ims_job_tool"
	$(CC) -o $@ ims_job_tool.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_mrd:  ims_mrd.o
	@echo
	@echo "Creating Executable ims_mrd"
	$(CC) -o $@ ims_mrd.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_oagdr:  ims_oagdr.o
	@echo
	@echo "Creating Executable ims_oagdr"
	$(CC) -o $@ ims_oagdr.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_orderCleaner:  ims_orderCleaner.o
	@echo
	@echo "Creating Executable ims_orderCleaner"
	$(CC) -o $@ ims_orderCleaner.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_shm_tool:  ims_shm_tool.o
	@echo
	@echo "Creating Executable ims_shm_tool"
	$(CC) -o $@ ims_shm_tool.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_stragglers:  ims_stragglers.o
	@echo
	@echo "Creating Executable ims_stragglers"
	$(CC) -o $@ ims_stragglers.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_syslog:  ims_syslog.o
	@echo
	@echo "Creating Executable ims_syslog"
	$(CC) -o $@ ims_syslog.o $(LALL) $(LIBS)
	@echo

%.o:%.c
	@echo
	@echo "Compiling $<"
	$(CC) $(CFLAGS) $(IALL) -c $<

###############################################################################
#
# Object and executable cleaning.
#
###############################################################################

clean:
	@echo
	@echo "Cleaning Utility client executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing Utility client executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
