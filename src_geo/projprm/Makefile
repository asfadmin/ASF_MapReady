# Makefile for          :  projprm 
# Module Author         :  Tom Logan & Mike Shindle

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_lt.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf.a 

OBJS =	wrlabtab.o \
	plstereo.o \
	utm.o
# lamazeqa.o
# albers.o
# geograph.o

 #-lm needed for solaris
projprm: projprm.c $(OBJS)
	$(CC) $(CFLAGS) -o projprm projprm.c $(OBJS) $(LIBS) $(LDFLAGS) -lm
	mv projprm$(BIN_POSTFIX) $(BINDIR)
	cp projprm.1 ../../man/cat1
	rm *.o
