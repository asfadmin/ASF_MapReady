# Makefile for          :  accum_offset
# Module Author         :  Mike Shindle
# Module Version        :  2.0

include ../../make_support/system_rules

OLIBS = $(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a 
LIBS = $(OLIBS) -lm
OBJS=       accum_offset.o      


accum_offset: $(OBJS) $(OLIBS)
	$(CC) $(CFLAGS)  -o accum_offset $(OBJS) $(LIBS)
	rm *.o
	mv accum_offset$(BIN_POSTFIX) $(BINDIR)
	cp accum_offset.man ../../man/cat1/accum_offset.1
