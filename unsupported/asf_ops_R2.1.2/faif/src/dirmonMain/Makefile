#===============================================================================
# Filename:	Makefile
# Description:	Makefile for XXXdirmon executables
# Creator:	Norbert Piega
# Notes:	
# 1.  December 97 - RHoffman
#     Change linking to Sybase libtli.so
#
# SCCS Info:
#  Makefile
#  1
#  15 Apr 1996
#  @(#)Makefile	1.4
#===============================================================================

include ../faif_makedefs

IFAIF=-I../. -I../../../include/local -I../../../include/global
IALL=$(IFAIF) $(IENCRYPT)

FAUTILS_LIB=../../../lib/$(MACH)/local/libfautils.a
UTIL_LIB=../../../lib/$(MACH)/local/libutil.a
IDFILE_LIB=../../../lib/$(MACH)/local/libidfile.a
ODL_LIB=/ASF/$(ASFVER)/lib/$(MACH)/libodl.a
LLIST_LIB=/ASF/$(ASFVER)/lib/$(MACH)/libllist.a
ASF_LIB=../../../lib/$(MACH)/local/libfasf.a
CSA_LIB=../../../lib/$(MACH)/local/libcsa.a
ESA_LIB=../../../lib/$(MACH)/local/libesa.a
NASDA_LIB=../../../lib/$(MACH)/local/libnasda.a
WALPS_LIB=../../../lib/$(MACH)/local/libwalps.a
ADEOS_LIB=../../../lib/$(MACH)/local/libadeos.a
DIRMON_LIB=../../../lib/$(MACH)/local/libdirmon.a
SEMA_LIB=../../../lib/$(MACH)/local/libsema.a
WALPSXLATDT_LIB=../../../lib/$(MACH)/local/libwxlatdt.a
CSAXLATSV_LIB=../../../lib/$(MACH)/local/libxlatCSAsv.a
PMF_LIB=../../../lib/$(MACH)/local/libPMF.a

DEFS=-D$(MACH) -D$(DM)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=dirmonMain.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)
OBJ=$(SRC:%.c=%.o) $(MACHSRC:%.c=../%.o)

ifeq ($(DM), ESA_DM)
   BIN=../../bin/$(MACH)/ESAdirmon
   ROUTER_LIB=../../../lib/$(MACH)/local/libErouter.a
   2PMF_LIB=../../../lib/$(MACH)/local/libesa2pmf.a
FAIFIMS_LIB=../../../lib/$(MACH)/local/libfaifims.a
FA_PARSE_LIB=../../../lib/$(MACH)/local/libfaparse.a
GENC_LIB=$(ASFLIB)/libAPSinterface.a
TIME_LIB=$(ASFLIB)/libAPSinterface.a
LIB_IMS_LIB=$(ASFLIB)/libims.a
ESAT_LIB=$(ASFLIB)/libtimes.a
APS_LIB=$(ASFLIB)/libaps.a
SYBDB_LIB=$(SYBASE_LIB)
SYBIN_LIB=$(ASFLIB)/libsybint.a
APSDB_LIB=$(ASFLIB)/libapsdb.a
DP_VECTOR_LIB=$(ASFLIB)/libdp-vector.a
PHASE_LIB=$(ASFLIB)/libphase.a
   LIBRARIES = \
	$(MATH_LIB) \
        $(DIRMON_LIB) \
        $(IDFILE_LIB) \
        $(ROUTER_LIB) \
        $(2PMF_LIB) \
        $(FAIFIMS_LIB)\
        $(LIB_IMS_LIB) \
	$(ADEOS_PARSE_LIB) \
        $(ADEOS_LIB) \
        $(CSA_LIB) \
        $(CSAXLATSV_LIB) \
        $(ESA_LIB) \
        $(NASDA_LIB) \
        $(WALPS_LIB) \
        $(FAUTILS_LIB) \
        $(UTIL_LIB) \
        $(LIBUTIL_LIB) \
        $(SEMA_LIB) \
	$(ODL_LIB) \
        $(ASF_LIB) \
	$(FA_PARSE_LIB) \
	$(TIME_LIB) \
	$(ESAT_LIB) \
	$(PMF_LIB) \
	$(GENC_LIB) \
	$(PHASE_LIB) \
        $(SYBIN_LIB) \
	$(APSDB_LIB) \
	$(APS_LIB) \
	$(PHASE_LIB) \
	$(APSDB_LIB) \
        $(SYBASE)/lib/libct.a \
        $(SYBASE)/lib/libcs.a \
        $(SYBASE)/lib/libtcl.a \
        $(SYBASE)/lib/libcomn.a \
        $(SYBASE)/lib/libsybdb.a \
	$(DP_VECTOR_LIB) \
	$(LLIST_LIB)  \
	$(SPARC_LIB) $(UCB_LIB)
endif
ifeq ($(DM), NASDA_DM)
   BIN=../../bin/$(MACH)/NASDAdirmon
   ROUTER_LIB=../../../lib/$(MACH)/local/libNrouter.a
   2PMF_LIB=../../../lib/$(MACH)/local/libnasda2pmf.a
   EXTRA_LIBS=
FAIFIMS_LIB=../../../lib/$(MACH)/local/libfaifims.a
FA_PARSE_LIB=../../../lib/$(MACH)/local/libfaparse.a
GENC_LIB=$(ASFLIB)/libAPSinterface.a
TIME_LIB=$(ASFLIB)/libAPSinterface.a
LIB_IMS_LIB=$(ASFLIB)/libims.a
ESAT_LIB=$(ASFLIB)/libtimes.a
APS_LIB=$(ASFLIB)/libaps.a
SYBDB_LIB=$(SYBASE_LIB)
SYBIN_LIB=$(ASFLIB)/libsybint.a
APSDB_LIB=$(ASFLIB)/libapsdb.a
DP_VECTOR_LIB=$(ASFLIB)/libdp-vector.a
PHASE_LIB=$(ASFLIB)/libphase.a
   LIBRARIES = \
	$(MATH_LIB) \
        $(DIRMON_LIB) \
        $(IDFILE_LIB) \
        $(ROUTER_LIB) \
        $(2PMF_LIB) \
        $(FAIFIMS_LIB)\
        $(LIB_IMS_LIB) \
	$(ADEOS_PARSE_LIB) \
        $(ADEOS_LIB) \
        $(CSA_LIB) \
        $(CSAXLATSV_LIB) \
        $(ESA_LIB) \
        $(NASDA_LIB) \
        $(WALPS_LIB) \
        $(FAUTILS_LIB) \
        $(LIBUTIL_LIB) \
        $(UTIL_LIB) \
        $(SEMA_LIB) \
	$(ODL_LIB) \
        $(ASF_LIB) \
	$(FA_PARSE_LIB) \
	$(TIME_LIB) \
	$(ESAT_LIB) \
	$(PMF_LIB) \
	$(GENC_LIB) \
	$(PHASE_LIB) \
	$(SYBIN_LIB) \
	$(APS_LIB) \
        $(SYBASE)/lib/libct.a \
        $(SYBASE)/lib/libcs.a \
        $(SYBASE)/lib/libtcl.a \
        $(SYBASE)/lib/libcomn.a \
        $(SYBASE)/lib/libsybdb.a \
	$(APSDB_LIB) \
	$(DP_VECTOR_LIB) \
	$(LLIST_LIB)  \
	$(SPARC_LIB) $(UCB_LIB)
endif
ifeq ($(DM), WALPS_DM)
   BIN=../../bin/$(MACH)/WALPSdirmon
   ROUTER_LIB=../../../lib/$(MACH)/local/libWrouter.a
   2PMF_LIB=../../../lib/$(MACH)/local/libwalps2pmf.a
   EXTRA_LIBS=
FAIFIMS_LIB=../../../lib/$(MACH)/local/libfaifims.a
FA_PARSE_LIB=../../../lib/$(MACH)/local/libfaparse.a
GENC_LIB=$(ASFLIB)/libAPSinterface.a
TIME_LIB=$(ASFLIB)/libAPSinterface.a
LIB_IMS_LIB=$(ASFLIB)/libims.a
ESAT_LIB=$(ASFLIB)/libtimes.a
APS_LIB=$(ASFLIB)/libaps.a
SYBDB_LIB=$(SYBASE_LIB)
SYBIN_LIB=$(ASFLIB)/libsybint.a
APSDB_LIB=$(ASFLIB)/libapsdb.a
DP_VECTOR_LIB=$(ASFLIB)/libdp-vector.a
PHASE_LIB=$(ASFLIB)/libphase.a
   LIBRARIES = \
           $(DIRMON_LIB) \
           $(IDFILE_LIB) \
           $(ROUTER_LIB) \
           $(2PMF_LIB) \
           $(EXTRA_LIBS) \
           $(FAIFIMS_LIB)\
           $(LIB_IMS_LIB) \
           $(ADEOS_PARSE_LIB) \
           $(ADEOS_LIB) \
           $(CSA_LIB) \
           $(CSAXLATSV_LIB) \
           $(ESA_LIB) \
           $(NASDA_LIB) \
           $(WALPS_LIB) \
           $(FAUTILS_LIB) \
           $(UTIL_LIB) \
           $(SEMA_LIB) \
           $(ODL_LIB) \
           $(ASF_LIB) \
           $(FA_PARSE_LIB) \
           $(TIME_LIB) \
           $(ESAT_LIB) \
           $(PMF_LIB) \
           $(GENC_LIB) \
           $(PHASE_LIB) \
           $(SYBASE)/lib/libct.a \
           $(SYBASE)/lib/libcs.a \
           $(SYBASE)/lib/libtcl.a \
           $(SYBASE)/lib/libcomn.a \
           $(SYBASE)/lib/libsybdb.a \
           $(APSDB_LIB) \
           $(DP_VECTOR_LIB) \
           $(LLIST_LIB) \
           $(SPARC_LIB) \
           $(UCB_LIB)
endif
ifeq ($(DM), ADEOS_DM)
   BIN=../../bin/$(MACH)/ADEOSdirmon
   ROUTER_LIB=../../../lib/$(MACH)/local/libArouter.a
   2PMF_LIB=../../../lib/$(MACH)/local/libadeos2pmf.a
FAIFIMS_LIB=../../../lib/$(MACH)/local/libfaifims.a
FA_PARSE_LIB=../../../lib/$(MACH)/local/libfaparse.a
GENC_LIB=$(ASFLIB)/libAPSinterface.a
TIME_LIB=$(ASFLIB)/libAPSinterface.a
ESAT_LIB=$(ASFLIB)/libtimes.a
APS_LIB=$(ASFLIB)/libaps.a
LIB_IMS_LIB=$(ASFLIB)/libims.a
SYBDB_LIB=$(SYBASE_LIB)
SYBIN_LIB=$(ASFLIB)/libsybint.a
APSDB_LIB=$(ASFLIB)/libapsdb.a
DP_VECTOR_LIB=$(ASFLIB)/libdp-vector.a
PHASE_LIB=$(ASFLIB)/libphase.a
   LIBRARIES = \
	$(MATH_LIB) \
        $(DIRMON_LIB) \
        $(IDFILE_LIB) \
        $(ROUTER_LIB) \
        $(2PMF_LIB) \
	$(FAIFIMS_LIB)\
	$(LIB_IMS_LIB) \
	$(ADEOS_PARSE_LIB) \
        $(ADEOS_LIB) \
        $(CSA_LIB) \
        $(CSAXLATSV_LIB) \
        $(ESA_LIB) \
        $(NASDA_LIB) \
        $(WALPS_LIB) \
        $(FAUTILS_LIB) \
        $(UTIL_LIB) \
        $(LIBUTIL_LIB) \
        $(SEMA_LIB) \
	$(ODL_LIB) \
        $(ASF_LIB) \
	$(FA_PARSE_LIB) \
	$(TIME_LIB) \
	$(ESAT_LIB) \
	$(PMF_LIB) \
	$(GENC_LIB) \
	$(PHASE_LIB) \
	$(SYBIN_LIB) \
	$(APS_LIB) \
	$(APSDB_LIB) \
        $(SYBASE)/lib/libct.a \
        $(SYBASE)/lib/libcs.a \
        $(SYBASE)/lib/libtcl.a \
        $(SYBASE)/lib/libcomn.a \
        $(SYBASE)/lib/libsybdb.a \
	$(DP_VECTOR_LIB) \
	$(LLIST_LIB)  \
	$(SPARC_LIB) $(UCB_LIB)
endif

all:	$(BIN)

$(BIN):	$(MACH)/dirmonMain.o $(MACHOBJ) $(XMACHOBJ)
$(BIN):	$(MACH)/dirmonMain.o $(MACHOBJ) $(XMACHOBJ)
	cd $(MACH);$(CC) -o ../$(BIN) dirmonMain.o $(OBJ) \
           $(LSYBASE) -ltli $(LIBRARIES)

#	cd $(MACH);$(CC) -o ../$(BIN) dirmonMain.o $(OBJ) $(STATIC) \
#$(IDFILE_LIB) $(ESA_LIB) $(NASDA_LIB) $(CSA_LIB) $(WALPS_LIB) \
#$(ODL_LIB) $(FAUTILS_LIB) $(UTIL_LIB) $(MACHLFLAGS) $(MACHLLIBS)

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/*.o $(BIN)

cleanobjs:
	rm -f $(MACH)/*.o
