# Makefile for          :  psconv 
# Module Author         :  Tom Logan   
# Module Version        :  4.0
# Platform              :  Solaris 
# Makefile Date         :  1/97

#
# Set up build options--the debug switch (debug or optimize), 
# and directories to search for include files
#
ASF   = ../../lib/$(SYS)
LIBS = -lm

CFLAGS=$(FLAGS) -DLOCAL_PRM=0 -I../asf_inc

#
# Object libraries required to load the program
#
OBJLIB = imageUtil.o writePPF.o 

LIBS=	$(ASF)/asf_geolib.a $(ASF)/asf_meta.a \
	$(ASF)/asf_meta.a $(ASF)/asf.a \
	$(ASF)/asf_odl.a

all: rgpsMosaic rgpsDop writePPF scripts
	-rm *.o

scripts:
	cp rgpsTape ../../bin/$(SYS)

rgpsMosaic: rgpsMosaic.c $(OBJLIB)
	$(CC) -o rgpsMosaic rgpsMosaic.c $(OBJLIB) $(LIBS) $(CFLAGS)
	mv rgpsMosaic ../../bin/$(SYS)

rgpsDop: rgpsDop.c $(OBJLIB)
	$(CC) -o rgpsDop rgpsDop.c $(OBJLIB) $(LIBS) $(CFLAGS)
	mv rgpsDop ../../bin/$(SYS)

writePPF: writePPF.c
	$(CC) -o writePPF writePPF.c $(LIBS) $(CFLAGS) -DSTANDALONE=1
	

# a PostScript plotting routine.
rgpsPSPlot: rgpsPSPlot.c $(OBJLIB)
	$(CC) -o rgpsPSPlot rgpsPSPlot.c $(OBJLIB) $(LIBS) $(CFLAGS)
	mv rgpsPSPlot ../../bin/$(SYS)
