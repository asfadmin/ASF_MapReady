# Makefile for          :  dspddr
# Module Author         :  Mike Shindle

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS = dspddr.o \
       dstimg.o \
       dstbnd.o 

all: dspddr clean

dspddr: $(OBJS) 
	$(CC) $(CFLAGS) -o dspddr $(OBJS) $(LIBS)
	mv dspddr$(BIN_POSTFIX) $(BINDIR)
	cp dspddr.man ../../man/cat1/dspddr.1

clean:
	rm *.o
