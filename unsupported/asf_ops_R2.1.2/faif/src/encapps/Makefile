#===============================================================================
# Filename:	Makefile
# Description:	Makefile for FAIF_encrypt executable
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
ILDCE=-I../. -I../../../include/local/dce
ILLST=-I../../../include/local/llist
IALL= $(IFAIF) $(IENCRYPT) $(ILLST) $(ILDCE)

FAUTILS_LIB=../../../lib/$(MACH)/local/libfautils.a
UTIL_LIB=../../../lib/$(MACH)/local/libutil.a

DEFS=-D$(MACH)
#CFLAGS=$(MACHCFLAGS) $(DEFS)
CFLAGS=-g -Dsparc $(DEFS)

SRC=main.c getpass.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)
OBJ=$(SRC:%.c=%.o) $(MACHSRC:%.c=../%.o)

BIN=../../bin/$(MACH)/FAIF_encrypt

all:	$(BIN)

$(BIN):	$(MACH)/main.o $(MACHOBJ) $(XMACHOBJ)
	cd $(MACH);$(CC) -o ../$(BIN) main.o $(OBJ) $(STATIC) \
$(FAUTILS_LIB) $(UTIL_LIB) $(MACHLFLAGS) $(ENCRYPT_LIB)

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/* $(BIN)

