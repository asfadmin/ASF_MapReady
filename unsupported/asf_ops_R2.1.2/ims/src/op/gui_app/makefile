# @(#)makefile	5.7  10/20/97
###############################################################################
#
# File:         makefile
#
# Function:     Make the ims_op application.
#
# Author:       S. Hardman
#
# Date:         4/13/95
#
###############################################################################

include ../../makedefs

###############################################################################
#
# Include Path Definitions
#
###############################################################################

IOP =       -I../lincl
IMED =      -I../../media/lincl
IGLB =      -I../../gincl
IEXP =      -I../../eincl
ISYB =      -I$(SYBASE)/include
IUX =       -I/usr/local/uimx2.9/include

IALL =      $(IOP) $(IMED) $(IGLB) $(IEXP) $(IASF) $(ISYB) $(IUX) $(MACHIFLAGS)

###############################################################################
#
# Library Path Definitions
#
###############################################################################

LIMS =      -L../../../lib/$(MACH)/local
LEXP =      -L../../../lib/$(MACH)/global
LSYB =      -L$(SYBASE)/lib
LUX =       -L/usr/local/uimx2.9/lib

LALL =      $(LIMS) $(LEXP) $(LASF) $(LSYB) $(LUX) $(MACHLFLAGS)

###############################################################################
#
# Library Definitions
#
###############################################################################

OPLIB =     -lop
MEDLIB = 		-lmed
GLBLIB =    -lglb
IMSLIB = 		-lims
V0LIB =			-lv0
IMSODLLIB =	-limsodl
IKLIB =     -lik
DBLIB =     -lsybdb
ODLLIB =		-lodl
XLIB =      -lXm -lXt -lX11
GENLIB =    -lgen
ELFLIB =    -lelf

LIBS =      $(OPLIB) $(MEDLIB) $(GLBLIB) $(IMSLIB) $(V0LIB) \
						$(IMSODLLIB) $(IKLIB) $(DBLIB) $(ODLLIB) \
						$(XLIB) $(MACHLIBS) $(GENLIB) $(ELFLIB) -lm

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      -DXT_CODE -DMOTIF
CFLAGS =    $(MACHCFLAGS) $(DEFS)

###############################################################################
#
# Source and Target Definition
#
###############################################################################

SRC =       \
            ims_op.c \
            UxXt.c \
            UxMethod.c \
            ims_opWelcome.c \
            ims_opOrder.c \
            ims_opMsgBoxDlg.c \
            ims_opSearch.c \
            ims_opBrowseDlg.c \
            ims_opSelectionBoxDlg.c \
            ims_opCommentDlg.c \
            ims_opSrchSelDlg.c \
            ims_opPhotoOrder.c \
            ims_opPhotoJob.c \
            ims_opMediaDevice.c \
            ims_opAskUserDlg.c \
            ims_opFilmGen.c \
            ims_op_accAccountUser.c \
            ims_op_accUserData.c \
            ims_op_accAccountData.c \
            ims_op_accSearchUser.c \
            ims_op_accAssignUser.c \
            ims_op_accDeleteDlg.c \
            ims_op_accAssignDataset.c \
            ims_op_accSearchAccount.c \
            ims_op_accUpdateBalanceDlg.c \
            ims_op_accMsgDlg.c \
            ims_op_accManagerSelectDlg.c \
            ims_opShipping.c \
            ims_opShipView.c \
            ims_opBilling.c \
            ims_opBillView.c \
            ims_opLogin.c \
            ims_opDLtoDTKsrch.c \
            ims_opDLtoDTK.c \
            ims_opWorkingDlg.c 


OBJ =       $(SRC:%.c=%.o)
EXE =       ../../../bin/$(MACH)/ims_op

###############################################################################
#
# Make the Application
#
###############################################################################

all appl: $(EXE)

$(EXE): $(OBJ)
	@echo
	@echo "Creating Executable ims_op"
	$(CC) -o $@ $(OBJ) $(LALL) $(LIBS)
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
	@echo "Cleaning OP executables"
	rm -f $(OBJ) $(EXE)
	@echo

###############################################################################
#
# Executable installation.
#
###############################################################################

install:
	@echo
	@echo "Installing OP executables"
	cp -p $(EXE) $(ASFBIN)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)
