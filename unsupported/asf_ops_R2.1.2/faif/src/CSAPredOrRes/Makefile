#===============================================================================
# Filename:	Makefile
# Description:	Makefile for idfilename executable
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

FAUTILS_LIB=../../../lib/$(MACH)/local/libfautils.a
UTIL_LIB=../../../lib/$(MACH)/local/libutil.a
ASF_LIB=../../../lib/$(MACH)/local/libfasf.a
CSA_LIB=../../../lib/$(MACH)/local/libcsa.a
XLATCSASV_LIB=../../../lib/$(MACH)/local/libxlatCSAsv.a

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=main.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)
OBJ=$(SRC:%.c=%.o) $(MACHSRC:%.c=../%.o)

BIN=../../bin/$(MACH)/CSAPredOrRes

all:	$(BIN)

$(BIN):	$(MACH)/main.o $(MACHOBJ) $(XMACHOBJ)
	cd $(MACH);$(CC) -o ../$(BIN) main.o $(OBJ) $(STATIC) \
$(XLATCSASV_LIB) $(CSA_LIB) $(UTIL_LIB) $(ASF_LIB) $(FAUTILS_LIB)

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/* $(BIN)

