# Makefile for          :  dopPlot
# Module Author         :  Tom Logan   
# Module Version        :  4.0
# Platform              :  Solaris 
# Makefile Date         :  6/98

#
# Set up build options--the debug switch (debug or optimize), 
# and directories to search for include files
#
SYS = solaris
INCDIR = ../../src_lib/asf_inc
CFLAGS = -I$(INCDIR) $(FLAGS)
LIBDIR = ../../lib/$(SYS)
LFLAGS_STEP= ../../lib/$(SYS)/asf_sar.a

#
# Object libraries required to load the program
#

OBJS =	writePPF.o \
	$(LIBDIR)/asf_odl.a \
	$(LIBDIR)/asf_sar.a
OBJS_P = plotcol.o AscGraf.o

LIBS = -lm

all: plotcol dopTrend tape_utils
	cp dopPlot bin/
	chmod 775 bin/dopPlot
	cp dopPlotOrder bin/
	chmod 775 bin/dopPlotOrder
	cp dopPlot.man man/
	cp step/bin/solaris/metadata bin/
	tar cvf bins.tar bin/*
	rm *.o 

plotcol: $(OBJS_P)
	$(CC) $(CFLAGS) -o plotcol $(OBJS_P)
	mv plotcol bin

dopTrend: dopTrend.o $(OBJS) yaxb.o
	$(CC) $(CFLAGS) -o dopTrend dopTrend.o yaxb.o $(OBJS) $(LIBS)
	mv dopTrend bin
	cp dopTrend.man man

tape_utils: write_dlt concat_vdf create_tape parse_vdf 

write_dlt: VDF.h write_dlt.c
	$(CC) $(CFLAGS) write_dlt.c -o bin/write_dlt $(LFLAGS_STEP)

concat_vdf: VDF.h concat_vdf.c
	$(CC) $(CFLAGS) concat_vdf.c -o bin/concat_vdf

create_tape: create_tape.c
	$(CC) $(CFLAGS) create_tape.c -o bin/create_tape

parse_vdf: VDF.h parse_vdf.c
	$(CC) $(CFLAGS) parse_vdf.c -o bin/parse_vdf

clean:
	- rm -f bins.tar
	- rm -f bin/*
	- rm -f *.o
