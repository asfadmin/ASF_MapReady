# @(#)makefile	5.7  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the example applications.
#
# Author:       S. Hardman
#
# Date:         6/2/95
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IGLB =      -I../../gincl
IPUB =      -I../../pincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IGLB) $(IPUB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

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
IMSODLLIB = -limsodl
DBLIB =     -lsybdb
CTLIB =     -lct
CSLIB =     -lcs
TCLLIB =    -ltcl
ODLLIB =    -lodl
IKLIB =     -lik
COMNLIB =   -lcomn

LIBS =      $(GLBLIB) $(IMSLIB) $(IMSODLLIB) $(ODLLIB) $(IKLIB) \
	          $(DBLIB) $(CTLIB) $(CSLIB) $(TCLLIB) $(COMNLIB) $(MACHLIBS) \
						-lintl -lm

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
            ../../../bin/$(MACH)/ims_archiveEx \
            ../../../bin/$(MACH)/ims_calParamQueryEx \
            ../../../bin/$(MACH)/ims_darFrameEx \
            ../../../bin/$(MACH)/ims_darFrameStatusEx \
            ../../../bin/$(MACH)/ims_darQueryEx \
            ../../../bin/$(MACH)/ims_darStatisticsEx \
            ../../../bin/$(MACH)/ims_darStatusEx \
            ../../../bin/$(MACH)/ims_dlToDtkQueryEx \
            ../../../bin/$(MACH)/ims_getInputEx \
            ../../../bin/$(MACH)/ims_ghaQueryEx \
            ../../../bin/$(MACH)/ims_frameQueryEx \
            ../../../bin/$(MACH)/ims_incrVersionEx \
            ../../../bin/$(MACH)/ims_msgEx \
            ../../../bin/$(MACH)/ims_orderQueryEx \
            ../../../bin/$(MACH)/ims_orderStatusEx \
            ../../../bin/$(MACH)/ims_parseODLEx \
            ../../../bin/$(MACH)/ims_qiEx1 \
            ../../../bin/$(MACH)/ims_qiEx2 \
            ../../../bin/$(MACH)/ims_scanQueryEx \
            ../../../bin/$(MACH)/ims_spatialQueryEx \
            ../../../bin/$(MACH)/ims_svQueryEx \
            ../../../bin/$(MACH)/ims_tceQueryEx \
            ../../../bin/$(MACH)/ims_timeConvEx

###############################################################################
#
# Make the Application
#
###############################################################################

all appl:   $(EXE)

../../../bin/$(MACH)/ims_archiveEx:  ims_archiveEx.o
	@echo
	@echo "Creating Executable ims_archiveEx"
	$(CC) -o $@ ims_archiveEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_calParamQueryEx:  ims_calParamQueryEx.o
	@echo
	@echo "Creating Executable ims_calParamQueryEx"
	$(CC) -o $@ ims_calParamQueryEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_darFrameEx:  ims_darFrameEx.o
	@echo
	@echo "Creating Executable ims_darFrameEx"
	$(CC) -o $@ ims_darFrameEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_darFrameStatusEx:  ims_darFrameStatusEx.o
	@echo
	@echo "Creating Executable ims_darFrameStatusEx"
	$(CC) -o $@ ims_darFrameStatusEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_darQueryEx:  ims_darQueryEx.o
	@echo
	@echo "Creating Executable ims_darQueryEx"
	$(CC) -o $@ ims_darQueryEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_darStatisticsEx:  ims_darStatisticsEx.o
	@echo
	@echo "Creating Executable ims_darStatisticsEx"
	$(CC) -o $@ ims_darStatisticsEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_darStatusEx:  ims_darStatusEx.o
	@echo
	@echo "Creating Executable ims_darStatusEx"
	$(CC) -o $@ ims_darStatusEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_dlToDtkQueryEx:  ims_dlToDtkQueryEx.o
	@echo
	@echo "Creating Executable ims_dlToDtkQueryEx"
	$(CC) -o $@ ims_dlToDtkQueryEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_getInputEx:  ims_getInputEx.o
	@echo
	@echo "Creating Executable ims_getInputEx"
	$(CC) -o $@ ims_getInputEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_ghaQueryEx:  ims_ghaQueryEx.o
	@echo
	@echo "Creating Executable ims_ghaQueryEx"
	$(CC) -o $@ ims_ghaQueryEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_frameQueryEx:  ims_frameQueryEx.o
	@echo
	@echo "Creating Executable ims_frameQueryEx"
	$(CC) -o $@ ims_frameQueryEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_incrVersionEx:  ims_incrVersionEx.o
	@echo
	@echo "Creating Executable ims_incrVersionEx"
	$(CC) -o $@ ims_incrVersionEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_msgEx:  ims_msgEx.o
	@echo
	@echo "Creating Executable ims_msgEx"
	$(CC) -o $@ ims_msgEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_orderQueryEx:  ims_orderQueryEx.o
	@echo
	@echo "Creating Executable ims_orderQueryEx"
	$(CC) -o $@ ims_orderQueryEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_orderStatusEx:  ims_orderStatusEx.o
	@echo
	@echo "Creating Executable ims_orderStatusEx"
	$(CC) -o $@ ims_orderStatusEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_parseODLEx:  ims_parseODLEx.o
	@echo
	@echo "Creating Executable ims_parseODLEx"
	$(CC) -o $@ ims_parseODLEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_qiEx1:  ims_qiEx1.o
	@echo
	@echo "Creating Executable ims_qiEx1"
	$(CC) -o $@ ims_qiEx1.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_qiEx2:  ims_qiEx2.o
	@echo
	@echo "Creating Executable ims_qiEx2"
	$(CC) -o $@ ims_qiEx2.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_scanQueryEx:  ims_scanQueryEx.o
	@echo
	@echo "Creating Executable ims_scanQueryEx"
	$(CC) -o $@ ims_scanQueryEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_spatialQueryEx:  ims_spatialQueryEx.o
	@echo
	@echo "Creating Executable ims_spatialQueryEx"
	$(CC) -o $@ ims_spatialQueryEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_svQueryEx:  ims_svQueryEx.o
	@echo
	@echo "Creating Executable ims_svQueryEx"
	$(CC) -o $@ ims_svQueryEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_tceQueryEx:  ims_tceQueryEx.o
	@echo
	@echo "Creating Executable ims_tceQueryEx"
	$(CC) -o $@ ims_tceQueryEx.o $(LALL) $(LIBS)
	@echo

../../../bin/$(MACH)/ims_timeConvEx:  ims_timeConvEx.o
	@echo
	@echo "Creating Executable ims_timeConvEx"
	$(CC) -o $@ ims_timeConvEx.o $(LALL) $(LIBS)
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
	@echo "Cleaning Example executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing Example executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
