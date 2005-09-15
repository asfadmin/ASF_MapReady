# Makefile for program xy_plotter

include  ../Make_rules

LIBS  = $(LIBDIR)/libUtil.a \
        $(LIBDIR)/asf.a \
        $(LIBDIR)/asf_meta.a \
	-lm
OBJS  = xy_plotter.o

all: xy_plotter clean

xy_plotter: $(OBJS)
	$(CC) $(CFLAGS) -o xy_plotter $(OBJS) $(LIBS)
	mv xy_plotter$(BIN_POSTFIX) $(BINDIR)
#	cp xy_plotter.1 ../../../man/cat1

clean:
	rm -f *.o
