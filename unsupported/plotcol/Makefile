# Makefile for          :  plotcol
# Module Author         :  Orion Lawlor
# Makefile Date         :  2/97

include ../../make_support/system_rules	
INCDIR = ../asf_inc
LIBDIR = ../../lib/$(SYS)
CFLAGS = -I$(INCDIR) $(FLAGS)
LIBS = $(OLIBS) -lm
OBJS = AscGraf.o plotcol.o

plotcol: $(OBJS)
	$(CC) $(CFLAGS) -o plotcol $(OBJS) $(OLIBS) $(LIBS)
	rm *.o
	mv plotcol ../../bin/$(SYS)

