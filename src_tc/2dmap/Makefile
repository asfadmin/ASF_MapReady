# Makefile for          :  2dmap
# Module Author         :  Tom Logan   
# Module Version        :  2.0

include ../../make_support/system_rules

#       
# Object libraries required to load the program
# 
SUBS = \
	calc_coef.o \
	calc_res.o \
	modify.o \
	cross.o \
	mag.o \
	dot.o \
	calcplane.o

LIBS = \
	$(LIBDIR)/asf_geolib.a $(LIBDIR)/asf_las.a $(LIBDIR)/asf.a -lm

# Make the executable file for program 2dmap.
2dmap:  2dmap.c $(SUBS) $(CINCFIL)
	$(CC) $(CFLAGS) -o 2dmap $(SUBS) 2dmap.c $(LIBS)
	mv 2dmap$(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp 2dmap.man ../../man/cat1/2dmap.1

