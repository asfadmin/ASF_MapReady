#===============================================================================
# Filename:	Makefile
# Description:	Makefile for adeosrcpt executable
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
IALL=$(IFAIF) $(IENCRYPT)

DIRMON_LIB=../../../lib/$(MACH)/local/libdirmon.a
FAUTILS_LIB=../../../lib/$(MACH)/local/libfautils.a
LLIST_LIB=/ASF/$(ASFVER)/lib/$(MACH)/libllist.a
UTIL_LIB=../../../lib/$(MACH)/local/libutil.a
ADEOSPROC_LIB=../../../lib/$(MACH)/local/libADEOSproc.a
ADEOS_LIB=../../../lib/$(MACH)/local/libadeos.a

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=ADEOS_rcpt.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)
OBJ=$(SRC:%.c=%.o) $(MACHSRC:%.c=../%.o)

BIN=../../bin/$(MACH)/adeosrcpt

all:	$(BIN)

$(BIN):	$(MACH)/ADEOS_rcpt.o $(MACHOBJ) $(XMACHOBJ)
	cd $(MACH);$(CC) -o ../$(BIN) ADEOS_rcpt.o $(OBJ) $(STATIC) \
	 $(MACHLFLAGS) \
	 $(ADEOSPROC_LIB) \
	 $(ADEOS_LIB) \
	 $(FAUTILS_LIB) \
	 $(DIRMON_LIB) \
	 $(UTIL_LIB) \
	-lcrypt \
	$(LLIST_LIB) -lc /usr/ucblib/libucb.a


$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/* $(BIN)

