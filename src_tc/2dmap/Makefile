# Makefile for          :  2dmap
# Module Author         :  Tom Logan   
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS  = calc_coef.o \
	calc_res.o \
	modify.o \
	cross.o \
	mag.o \
	dot.o \
	calcplane.o

# Make the executable file for program 2dmap.
2dmap:  2dmap.c $(OBJS) $(CINCFIL)
	$(CC) $(CFLAGS) -o 2dmap $(OBJS) 2dmap.c $(LIBS) $(LDFLAGS)
	mv 2dmap$(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp 2dmap.man ../../man/cat1/2dmap.1

