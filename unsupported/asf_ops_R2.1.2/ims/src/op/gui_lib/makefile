# @(#)makefile	5.5  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the libop.a library.
#
# Author:       S. Hardman
#
# Date:         11/18/94
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IOP =       -I../lincl \
						-I../gui_app
IMED =      -I../../media/lincl
IGLB =      -I../../gincl
IPUB =      -I../../pincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include

IALL =      $(IOP) $(IMED) $(IGLB) $(IPUB) $(IEXP) $(IASF) $(ISYB) \
						$(MACHIFLAGS)

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
            ims_opCat.c \
            ims_opDLtoDTKCb.c \
            ims_opDLtoDTKsrchCb.c \
            ims_opOrderCb.c \
            ims_opWelcomeCb.c \
            ims_opMsgBoxDlgCb.c \
            ims_opSearchCb.c \
            ims_opBrowseDlgCb.c \
            ims_opSelectionDlgCb.c \
            ims_opCommentDlgCb.c \
            ims_opSrchSelDlgCb.c \
            ims_opUtilCb.c \
            ims_opPhotoOrderCb.c \
            ims_opPhotoJobCb.c \
            ims_opMediaDeviceCb.c \
            ims_opFilmGenCb.c \
            ims_op_accAccountUserCb.c\
            ims_op_accAccountDataCb.c\
            ims_op_accUserDataCb.c\
            ims_op_accAccountUserDeleteDlgCb.c\
            ims_op_accSearchUserCb.c\
            ims_op_accSearchAccountCb.c\
            ims_op_accAssignDatasetCb.c\
            ims_op_accAssignUserCb.c\
            ims_op_accMsgDlgCb.c\
            ims_op_accCat.c\
            ims_op_accUpdateBalanceDlgCb.c \
            ims_op_accManagerSelectDlgCb.c \
            ims_opShippingCb.c \
            ims_opShipViewCb.c \
            ims_opBillingCb.c  \
            ims_opBillViewCb.c \
            ims_opLoginCb.c

OBJ =       $(SRC:%.c=%.o)
LIB =       ../../../lib/$(MACH)/local/libop.a

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
	@if [ $(MACH) = sun ]; then ranlib $(LIB); fi
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
	@echo "Cleaning OP local library"
	rm -f $(OBJ) $(LIB)
	@echo

###############################################################################
#
# Library installation.
#
###############################################################################

install:
	@echo
	@echo "Installation not required"
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
