#===============================================================================
# Filename:	Makefile
# Description:	Makefile for CSAgetfile executable
# Creator:	Norbert Piega
# Notes:	
#
# SCCS Info:
#  Makefile
#  1
#  03/13/98
#  @(#)Makefile	1.5
#===============================================================================

include ../../src/faif_makedefs

IFAIF=-I../. -I../../../include/local -I../../../include/global
IALL=$(IFAIF) $(IENCRYPT)

FAUTILS_LIB=../../../lib/$(MACH)/local/libfautils.a
UTIL_LIB=../../../lib/$(MACH)/local/libutil.a
IDFILE_LIB=../../../lib/$(MACH)/local/libidfile.a
ODL_LIB=/ASF/$(ASFVER)/lib/$(MACH)/libodl.a
LLIST_LIB=/ASF/$(ASFVER)/lib/$(MACH)/libllist.a
ADEOS_LIB=../../../lib/$(MACH)/local/libadeos.a
ASF_LIB=../../../lib/$(MACH)/local/libfasf.a
CSA_LIB=../../../lib/$(MACH)/local/libcsa.a
CSA2PMF_LIB=../../../lib/$(MACH)/local/libcsa2pmf.a
CSAP_LIB=../../../lib/$(MACH)/global/libcsap.a
PMF_LIB=../../../lib/$(MACH)/local/libPMF.a
ESA_LIB=../../../lib/$(MACH)/local/libesa.a
NASDA_LIB=../../../lib/$(MACH)/local/libnasda.a
WALPS_LIB=../../../lib/$(MACH)/local/libwalps.a
DIRMON_LIB=../../../lib/$(MACH)/local/libdirmon.a
SEMA_LIB=../../../lib/$(MACH)/local/libsema.a
CROUTER_LIB=../../../lib/$(MACH)/local/libCrouter.a
CSAXLATSV_LIB=../../../lib/$(MACH)/local/libxlatCSAsv.a
FAIFIMS_LIB=../../../lib/$(MACH)/local/libfaifims.a
LIB_IMS_LIB=$(ASFLIB)/libims.a

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=CSAgetfile.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)
OBJ=$(SRC:%.c=%.o) $(MACHSRC:%.c=../%.o)

BIN=../../bin/$(MACH)/CSAgetfile

all:	$(BIN)

$(BIN):	$(MACH)/CSAgetfile.o $(MACHOBJ) $(XMACHOBJ)
	cd $(MACH);$(CC) -o ../$(BIN) CSAgetfile.o $(OBJ) \
      $(LSYBASE) -ltli \
      $(DIRMON_LIB) \
      $(IDFILE_LIB) \
      $(ADEOS_LIB) \
      $(CSA_LIB) \
      $(SEMA_LIB) \
      $(ESA_LIB) \
      $(NASDA_LIB) \
      $(WALPS_LIB) \
      $(LLIST_LIB) \
      $(CROUTER_LIB) \
      $(CSA2PMF_LIB) \
      $(FAIFIMS_LIB) \
      $(LIB_IMS_LIB) \
      $(PMF_LIB) \
      $(CSAP_LIB) \
      $(ODL_LIB) \
      $(CSAXLATSV_LIB) \
      $(ASF_LIB) \
      $(FAUTILS_LIB) \
      $(ENCRYPT_LIB) \
      $(SYBASE)/lib/libct.a \
      $(SYBASE)/lib/libcs.a \
      $(SYBASE)/lib/libtcl.a \
      $(SYBASE)/lib/libcomn.a \
      $(SYBASE)/lib/libsybdb.a \
      $(UTIL_LIB) \
      $(SPARC_LIB) \
      $(UCB_LIB)

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/* $(BIN)

