# Makefile for          :  dspddr
# Module Author         :  Mike Shindle

include ../../make_support/system_rules

LIBS =  \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
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
