#===============================================================================
# Filename:	Makefile
# Description:	Makefile for CSA parse library.
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
IALL=$(IFAIF)

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)

SRC=CSAplib.c lex.yy.c y.tab.c
MACHSRC=
MACHOBJ=$(SRC:%.c=$(MACH)/%.o) $(MACHSRC:%.c=%.o)

LIB=../../lib/$(MACH)/global/libcsap.a

all:	$(LIB)

$(LIB):	$(MACHOBJ)
	$(AR) $(LIB) $?

$(MACH)/%.o:%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)

y.tab.c y.tab.h: CSAparse.y ../../include/global/CSAparse.h
	$(YACC) $(YFLAGS) CSAparse.y

lex.yy.c : CSAlexer.l y.tab.h y.tab.c
	$(LEX) CSAlexer.l

CSAplib.c : ../../include/global/CSAparse.h

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/* $(LIB) y.tab.c y.tab.h lex.yy.c

