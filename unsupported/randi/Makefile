# makro paths
#######################################################
#CHANGE THIS - Jay Cable 2/29/96                      #
PWD       = /3dsar1/asf_tools/unsupported/randi
######################################################

ROOT	= $(PWD)
DRD	= $(ROOT)/lib/srcDrdlib
ANDI	= $(ROOT)/src/randi
TOOLS	= $(ROOT)/src/tools
TEST	= $(ROOT)/src/test
EXE	= $(PWD)/bin
OBJ	= $(ROOT)/obj 
LIB	= $(ROOT)/lib
HJWLIB	= $(ROOT)/lib/srcHjwlib
INC	= $(ROOT)/include

LIBC1	= $(DRD)/echo.c $(DRD)/drd.c $(DRD)/noise.c 
LIBC2	= $(DRD)/cal.c $(DRD)/rep.c $(DRD)/zero.c
LIBC	= $(LIBC1) $(LIBC2) $(DRD)/aux.c $(DRD)/report.c
LIBO1	= $(OBJ)/echo.o $(OBJ)/drd.o $(OBJ)/noise.o
LIBO2	= $(OBJ)/cal.o $(OBJ)/rep.o $(OBJ)/zero.o
LIBO	= $(LIBO1) $(LIBO2) $(OBJ)/aux.o $(OBJ)/report.o
ANDISRC	= $(ANDI)/fra.c $(ANDI)/foa.c $(ANDI)/al.c $(ANDI)/randi.c
ANDIOBJ	= $(OBJ)/fra.o $(OBJ)/foa.o $(OBJ)/al.o $(OBJ)/randi.o
FFTLIB  = $(PWD)cal/lib/rcal.a

PROF		= -p
DEBUG		= -g $(PROF)
CFLAGS		= -misalign 
LDLIBS  	= $(LIB)/drdlib.a $(LIB)/hjwlib.a
XLIBS		= -lxview -lolgx -lX11
MATH_LINK	= -lm
LINK		= cc $(DEBUG) -misalign -o $@
CC		= cc -c -O -I$(INC) -o $@ 
CCD		= cc -c $(DEBUG) -I$(INC) -o $@ 

# to build in any case:
#all:	$(DRD)/tags
all: diagram dumpblock randi histo ber  sdevf editheader anapulse power rawlocate
fileshift:	$(EXE)/fileshift

diagram: $(EXE)/diagram

dumpblock: $(EXE)/dumpblock

randi: $(EXE)/randi

histo: $(EXE)/histo

ber: $(EXE)/ber

sdev_vs_saturation: $(EXE)/sdev_vs_saturation

sdevf: $(EXE)/sdevf

editheader: $(EXE)/editheader

anapulse: $(EXE)/anapulse

power: $(EXE)/power

rawlocate: $(EXE)/rawlocate

# tagfiles
$(DRD)/tags: $(LIBC)
	ctags -f $(DRD)/tags $(LIBC)

#######################################################
# diagram
$(EXE)/diagram: $(OBJ)/diagram.o $(LDLIBS)
	$(LINK) $(OBJ)/diagram.o -Bstatic $(XLIBS) $(LDLIBS) $(MATH_LINK)

$(OBJ)/diagram.o: $(TOOLS)/diagram.c
	$(CC) $(TOOLS)/diagram.c -Bstatic $(LDLIBS)

#######################################################
# fileshift - shifts the infile bitwise to the left
$(EXE)/fileshift: $(OBJ)/fileshift.o $(LDLIBS)
	$(LINK) $(OBJ)/fileshift.o $(LDLIBS) $(MATH_LINK)

$(OBJ)/fileshift.o: $(TOOLS)/fileshift.c
	$(CC) $(TOOLS)/fileshift.c $(LDLIBS)

#######################################################
# edit header
$(EXE)/editheader: $(OBJ)/editheader.o $(LDLIBS)
	$(LINK) $(OBJ)/editheader.o $(LDLIBS) $(MATH_LINK)

$(OBJ)/editheader.o: $(TOOLS)/editheader.c
	$(CC) $(TOOLS)/editheader.c $(LDLIBS)

#######################################################
# anapulse
$(EXE)/anapulse: $(OBJ)/anapulse.o $(LDLIBS)
	$(LINK) $(OBJ)/anapulse.o $(LDLIBS) $(DEBUGLIBS) $(FFTLIB) $(MATH_LINK)

$(OBJ)/anapulse.o: $(TOOLS)/anapulse.c
	$(CC) $(TOOLS)/anapulse.c $(LDLIBS)

#######################################################
# power
$(EXE)/power: $(OBJ)/power.o $(LDLIBS)
	$(LINK) $(OBJ)/power.o $(LDLIBS) $(DEBUGLIBS) $(FFTLIB) $(MATH_LINK)

$(OBJ)/power.o: $(TOOLS)/power.c
	$(CC) $(TOOLS)/power.c $(LDLIBS) 


#######################################################
# dumpblock
$(EXE)/dumpblock: $(OBJ)/dumpblock.o $(LDLIBS)
	$(LINK) $(OBJ)/dumpblock.o -Bstatic $(XLIBS) $(LDLIBS) $(MATH_LINK)

$(OBJ)/dumpblock.o: $(TOOLS)/dumpblock.c
	$(CC) $(TOOLS)/dumpblock.c -Bstatic $(LDLIBS)

#######################################################
# ber - bit error rate program
$(EXE)/ber: $(OBJ)/ber.o $(LDLIBS)
	$(LINK) $(OBJ)/ber.o $(LDLIBS)

$(OBJ)/ber.o: $(TOOLS)/ber.c  $(INC)/drdapi.h
	$(CC) $(TOOLS)/ber.c $(LDLIBS)

#######################################################
# histo - histogram
$(EXE)/histo: $(OBJ)/histo.o $(LDLIBS)
	$(LINK) $(OBJ)/histo.o $(LDLIBS) $(MATH_LINK)

$(OBJ)/histo.o: $(TOOLS)/histo.c $(INC)/drdapi.h
	$(CC) $(TOOLS)/histo.c

#######################################################
# sdev_vs_saturation - saturation vs stddev
$(EXE)/sdev_vs_saturation: $(OBJ)/sdev_vs_saturation.o $(LDLIBS)
	$(LINK) $(OBJ)/sdev_vs_saturation.o $(LDLIBS) $(DEBUGLIBS) $(MATH_LINK)
 
$(OBJ)/sdev_vs_saturation.o: $(TOOLS)/sdev_vs_saturation.c $(INC)/drdapi.h
	$(CC) $(TOOLS)/sdev_vs_saturation.c

#######################################################
# sdevf - stddev over blocks
$(EXE)/sdevf: $(OBJ)/sdevf.o $(LDLIBS)
	$(LINK) $(OBJ)/sdevf.o $(LDLIBS) $(DEBUGLIBS) $(MATH_LINK)

$(OBJ)/sdevf.o: $(TOOLS)/sdevf.c $(INC)/drdapi.h
	$(CC) $(TOOLS)/sdevf.c

#######################################################
# rawlocate - data location in raw file
# added by Christian Fischer 8/30/95
$(EXE)/rawlocate: $(OBJ)/rawlocate.o $(LDLIBS)
	$(LINK) $(OBJ)/rawlocate.o $(LDLIBS) $(DEBUGLIBS) $(MATH_LINK)

$(OBJ)/rawlocate.o: $(TOOLS)/rawlocate.c $(INC)/drdapi.h
	$(CC) $(TOOLS)/rawlocate.c

#######################################################
# randi program
$(EXE)/randi: $(ANDIOBJ) $(LDLIBS)
	$(LINK) $(ANDIOBJ) $(LDLIBS) $(DEBUGLIBS) $(MATH_LINK)

$(OBJ)/randi.o: $(ANDI)/randi.c $(ANDI)/randi.h $(INC)/drdapi.h
	$(CC) $(ANDI)/randi.c

$(OBJ)/fra.o: $(ANDI)/fra.c $(ANDI)/randi.h $(INC)/drdapi.h
	$(CC) $(ANDI)/fra.c

$(OBJ)/foa.o: $(ANDI)/foa.c $(ANDI)/randi.h $(INC)/drdapi.h
	$(CC) $(ANDI)/foa.c

$(OBJ)/al.o: $(ANDI)/al.c $(ANDI)/randi.h $(INC)/drdapi.h
	$(CC) $(ANDI)/al.c

#######################################################
# HJWLIB
$(LIB)/hjwlib.a: $(OBJ)/hjwlib.o
	ar rcv $(LIB)/hjwlib.a $(OBJ)/hjwlib.o
	ranlib $(LIB)/hjwlib.a

$(OBJ)/hjwlib.o: $(HJWLIB)/hjwlib.c $(INC)/hjwlib.h
	$(CC) $(HJWLIB)/hjwlib.c

#######################################################
# DRD library
$(LIB)/drdlib.a: $(LIBO)
	ar rcv $(LIB)/drdlib.a $(LIBO)
	ranlib $(LIB)/drdlib.a

$(OBJ)/echo.o: $(DRD)/echo.c $(DRD)/drd.h $(INC)/drdapi.h
	$(CC) $(DRD)/echo.c
 
$(OBJ)/noise.o: $(DRD)/noise.c $(DRD)/drd.h $(INC)/drdapi.h
	$(CC) $(DRD)/noise.c
 
$(OBJ)/zero.o: $(DRD)/zero.c $(DRD)/drd.h $(INC)/drdapi.h
	$(CC) $(DRD)/zero.c
 
$(OBJ)/cal.o: $(DRD)/cal.c $(DRD)/drd.h $(INC)/drdapi.h
	$(CC) $(DRD)/cal.c
 
$(OBJ)/drd.o: $(DRD)/drd.c $(DRD)/drd.h $(INC)/drdapi.h
	$(CC) $(DRD)/drd.c
 
$(OBJ)/rep.o: $(DRD)/rep.c $(DRD)/drd.h $(INC)/drdapi.h
	$(CC) $(DRD)/rep.c
 
$(OBJ)/aux.o: $(DRD)/aux.c $(DRD)/drd.h $(INC)/drdapi.h
	$(CC) $(DRD)/aux.c
 
$(OBJ)/report.o: $(DRD)/report.c $(DRD)/drd.h $(INC)/drdapi.h
	$(CC) $(DRD)/report.c
 

