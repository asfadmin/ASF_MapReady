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
INCDIR = step/src/asf_inc
CFLAGS = -I$(INCDIR) $(FLAGS)
LIBDIR = step/lib/$(SYS)
LFLAGS_STEP= $(LIBDIR)/asf_sar.a

#
# Object libraries required to load the program
#

OBJS =	writePPF.o \
	$(LIBDIR)/asf_odl.a \
	$(LIBDIR)/asf_sar.a
OBJS_P = plotcol.o AscGraf.o

LIBS = -lm

all: plotcol dopTrend tape_utils writePPF
	cp dopPlot bin/
	chmod 775 bin/dopPlot
	cp dopPlotOrder bin/
	chmod 775 bin/dopPlotOrder
	cp dopPlot.man man/
	chmod 775 man/dopPlot.man
	cp step/bin/solaris/metadata bin/
	-tar cvf bin/bins.tar bin/*
	-rm *.o 

plotcol: $(OBJS_P)
	$(CC) $(CFLAGS) -o plotcol $(OBJS_P)
	mv plotcol bin

writePPF:
	$(CC) $(CFLAGS) -o writePPF -DSTANDALONE=1 -DMAIL_MESSAGE=0 \
		 writePPF.c $(LIBDIR)/asf_odl.a $(LIBDIR)/asf_sar.a $(LIBS)
	mv writePPF bin/

dopTrend: dopTrend.o $(OBJS)
	$(CC) $(CFLAGS) -o dopTrend dopTrend.o $(OBJS) $(LIBS)
	mv dopTrend bin
	cp dopTrend.man man
	chmod 775 man/dopTrend.man

tape_utils: write_dlt write_8mm concat_vdf create_tape parse_vdf 

write_dlt: VDF.h write_dlt.c
	$(CC) $(CFLAGS) write_dlt.c -o bin/write_dlt $(LFLAGS_STEP)

write_8mm: VDF.h write_8mm.c
	$(CC) $(CFLAGS) write_8mm.c -o bin/write_8mm $(LFLAGS_STEP)

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
