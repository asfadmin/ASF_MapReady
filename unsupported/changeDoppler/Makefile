# Makefile for 		:  cal_ssar
# Module Author 	:  Tom Logan
# Module Version	:  1.0
# Platform		:  Solaris  
# Makefile Date		:  1/14/98
SYS = solaris
INCDIR = ../asf_inc
CFLAGS = -g -I$(INCDIR) $(FLAGS)
LIBDIR = ../../lib/$(SYS)
0BJLIB =
OBJS =  changeDoppler.o\
	proj.o \
	changeCeos.o \
	cal_ssar.o \
 	$(LIBDIR)/asf_sar.a \
	$(LIBDIR)/geolocate.a \
	$(LIBDIR)/caplib.a

all: changeDoppler clean

changeDoppler: $(OBJS) 
	$(CC) $(CFLAGS) -o changeDoppler $(OBJS) $(OBJLIB) -lm
	mv changeDoppler ../../bin/$(SYS)
#	cp changeDoppler.1 ../../man/man1/

clean:
	-rm *.o
