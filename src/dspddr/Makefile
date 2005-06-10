# Makefile for          :  dspddr
# Module Author         :  Mike Shindle

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm

OBJS = dspddr.o \
       dstimg.o \
       dstbnd.o 

all: dspddr clean

dspddr: $(OBJS) 
	$(CC) $(CFLAGS) -o dspddr $(OBJS) $(LIBS) $(LDFLAGS)
	mv dspddr$(BIN_POSTFIX) $(BINDIR)
	cp dspddr.man ../../man/cat1/dspddr.1

clean:
	rm *.o
