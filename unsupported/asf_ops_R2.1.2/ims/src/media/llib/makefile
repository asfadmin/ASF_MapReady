# @(#)makefile	5.4  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the libmed.a library.
#
# Author:       S. Hardman
#
# Date:         5/20/95
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IMED =      -I../lincl
IGLB =      -I../../gincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IMED) $(IGLB) $(IEXP) $(IASF) $(ISYB) $(MACHIFLAGS)

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      
CFLAGS =    $(MACHCFLAGS) $(DEFS)

###############################################################################
#
# Source and Target Definition
#
###############################################################################

SRC = \
            ims_create_null_vdf.c \
            ims_calMediaSpace.c \
            ims_device.c \
            ims_formTape.c \
            ims_filmList.c \
            ims_filmStatus.c \
            ims_getProdList.c \
            ims_getTime.c \
            ims_getTxrInfo.c \
            ims_ioUtils.c \
            ims_makeNullVdf.c \
            ims_mediaDist.c \
            ims_mediaFit.c \
            ims_mediaSpace.c \
            ims_mediaUtil.c \
            ims_processDat.c \
            ims_processLdr.c \
            ims_processVdf.c \
            ims_qc.c \
            ims_qcBeginMedia.c \
            ims_qcCheckFdr.c \
            ims_qcCheckFile.c \
            ims_qcCheckFpr.c \
            ims_qcCheckNdf.c \
            ims_qcCheckNonVdf.c \
            ims_qcCheckTxt.c \
            ims_qcCheckVdf.c \
            ims_qcCheckVdr.c \
            ims_qcEndMedia.c \
            ims_qcGetRecTyp.c \
            ims_qcMakeReport.c \
            ims_qcRptTime.c \
            ims_qcWrtReport.c \
            ims_tar.c \
            ims_vdfCat.c \
            ims_writeFpr.c \
            ims_writeTxr.c \
            ims_writeVdr.c \
            ims_writeVolDir.c

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../../lib/$(MACH)/local/libmed.a

###############################################################################
#
# Make the Library
#
###############################################################################

all: $(LIB)

$(LIB):	$(OBJ)
	@echo
	@echo "Archive and Conversion"
	ar rucv $(LIB) $?
	@if [ $(MACH) = sunos ]; then ranlib $(LIB); fi
	@echo

%.o:%.c
	@echo
	@echo "Compiling $<"
	$(CC) $(CFLAGS) $(IALL) -c $<

###############################################################################
#
# Object and library cleaning.
#
###############################################################################

clean:
	@echo
	@echo "Cleaning the Media local library"
	-rm -f $(OBJ) $(LIB)
	@echo

###############################################################################
#
# Library installation.
#
###############################################################################

install:
	@echo
	@echo "No installation required"
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
