#===============================================================================
# Description:	Makefile for par executable
#===============================================================================

include ../faif_makedefs

IFAIF=-I../. -I../../../include/local -I../../../include/global
IALL=$(IFAIF) $(IENCRYPT) 

UTIL_LIB=../../../lib/$(MACH)/local/libutil.a

DEFS=-D$(MACH)
#CFLAGS=$(MACHCFLAGS) $(DEFS)
CFLAGS=-g -Dsparc $(DEFS)

SRC=par.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)
OBJ=$(SRC:%.c=%.o) $(MACHSRC:%.c=../%.o)

BIN=../../bin/$(MACH)/par

all:	$(BIN)

$(BIN):	$(MACH)/par.o $(MACHOBJ) $(XMACHOBJ)
	cd $(MACH);$(CC) -o ../$(BIN) par.o $(OBJ) $(STATIC) \
      $(ENCRYPT_LIB) \
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
	rm -f $(MACH)/*.o $(BIN)



