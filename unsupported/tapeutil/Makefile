# Makefile for: Create_tape
# Makefile Author: Orion Lawlor
# ASF STEP Lab, 6/1997

CFLAGS=-O
BINDIR=/proj3/asf_tools/bin/$(SYS)

all: clean write_dlt concat_vdf create_tape parse_vdf bins.tar
	- rm *.o

SYS=solaris
CFLAGS_STEP= $(CFLAGS) -I../asf_inc
LFLAGS_STEP= ../../lib/$(SYS)/asf_sar.a

write_dlt: VDF.h write_dlt.c
	$(CC) $(CFLAGS_STEP) write_dlt.c -o write_dlt $(LFLAGS_STEP)

concat_vdf: VDF.h concat_vdf.c
	$(CC) $(CFLAGS) concat_vdf.c -o concat_vdf

create_tape: create_tape.c
	$(CC) $(CFLAGS) create_tape.c -o create_tape

parse_vdf: VDF.h parse_vdf.c
	$(CC) $(CFLAGS) parse_vdf.c -o parse_vdf

bins.tar:
	cp  $(BINDIR)/dopPlot $(BINDIR)/plotcol \
		$(BINDIR)/dopTrend $(BINDIR)/dopPlotOrder \
		$(BINDIR)/metadata .
	tar cvf bins.tar dopPlot plotcol dopTrend dopPlotOrder \
		metadata write_dlt concat_vdf create_tape 

clean:
	- rm -f bins.tar
	- rm -f write_dlt
	- rm -f create_tape
	- rm -f concat_vdf
	- rm -f parse_vdf
	- rm -f core
	- rm -f *.o
