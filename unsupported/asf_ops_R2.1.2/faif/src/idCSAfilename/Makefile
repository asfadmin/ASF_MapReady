#===============================================================================
# Filename:	Makefile
# Description:	Makefile for idCSAfilename executable
# Creator:	Norbert Piega
# Notes:	
#
# SCCS Info:
#  %M%
#  %R%
#  %G%
#  %W%
#===============================================================================

include ../faif_makedefs

IFAIF=-I../. -I../../../include/local -I../../../include/global
ILLST=-I../../../include/local/llist
IALL=$(IFAIF) $(IENCRYPT) $(ILLST)

ODL_LIB=/ASF/$(ASFVER)/lib/$(MACH)/libodl.a
FAUTILS_LIB=../../../lib/$(MACH)/local/libfautils.a
UTIL_LIB=../../../lib/$(MACH)/local/libutil.a
IDFILE_LIB=../../../lib/$(MACH)/local/libidfile.a
ESA_LIB=../../../lib/$(MACH)/local/libesa.a
NASDA_LIB=../../../lib/$(MACH)/local/libnasda.a
CSA_LIB=../../../lib/$(MACH)/local/libcsa.a
XLATCSASV_LIB=../../../lib/$(MACH)/local/libxlatCSAsv.a
WALPS_LIB=../../../lib/$(MACH)/local/libwalps.a
ADEOS_LIB=../../../lib/$(MACH)/local/libadeos.a
FASF_LIB=../../../lib/$(MACH)/local/libfasf.a

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=main.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)
OBJ=$(SRC:%.c=%.o) $(MACHSRC:%.c=../%.o)

BIN=../../bin/$(MACH)/idCSAfilename

all:	$(BIN)

$(BIN):	$(MACH)/main.o $(MACHOBJ) $(XMACHOBJ)
	cd $(MACH);$(CC) -o ../$(BIN) main.o $(OBJ) $(STATIC) \
$(IDFILE_LIB) $(CSA_LIB) $(XLATCSASV_LIB) $(FASF_LIB) $(NASDA_LIB) \
$(ESA_LIB) $(WALPS_LIB) $(ADEOS_LIB) $(ODL_LIB) $(FAUTILS_LIB) $(UTIL_LIB) \
$(MACHLFLAGS) $(MACHLLIBS)

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/* $(BIN)

