######################################################################
# Makefile for 		:  point_target.c 
# Module Author 	:  Orion Lawlor
# Module Version	:  1.0
# Makefile Date		:  6/99 
######################################################################
# 
# Set the path to the ASF Step Tools include & library directories
#
INCDIR = ../asf_inc
LIBDIR = ../../lib/$(SYS)

#
# Set compile options, libraries list, and list of objects required
#
CFLAGS =  -I. -I$(INCDIR) $(FLAGS)
LIBS   = $(LIBDIR)/asf_fft.a $(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a 
LIBC = $(LIBS) -lm 

OBJLIB =  point_target.o analyze.o

all: point_target
	rm *.o

point_target:	$(OBJLIB) $(LIBS)
	$(CC) $(CFLAGS) -o point_target $(OBJLIB) $(LIBC)
	mv point_target$(BIN_POSTFIX) ../../bin/$(SYS)
#	cp point_target.man ../../man/cat1/point_target.1
