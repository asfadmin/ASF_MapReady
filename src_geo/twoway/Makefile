# Makefile for          :  twoway
# Module Author         :  Mike Shindle
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS=  twoway.o  

all: twoway
	rm *.o

twoway: $(OBJS)
	$(CC) $(CFLAGS) -o twoway $(OBJS) $(LIBS)
	mv twoway$(BIN_POSTFIX) $(BINDIR)
	cp twoway.man ../../man/cat1/twoway.1

