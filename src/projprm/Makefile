# Makefile for  :  projprm 
# Module Author :  Tom Logan, Mike Shindle
#        03/2004:  Patrick Denny

include ../../make_support/system_rules

LIBS  =	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_lt.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/asf_geolib.a \
	-lm

OBJS =	albers.o \
        lamazeqa.o \
	plstereo.o \
	utm.o \
	wrlabtab.o
# geograph.o

 #-lm needed for solaris
projprm: projprm.c $(OBJS)
	$(CC) $(CFLAGS) -o projprm projprm.c $(OBJS) $(LIBS) $(LDFLAGS)
	mv projprm$(BIN_POSTFIX) $(BINDIR)
	cp projprm.1 ../../man/cat1
	rm *.o
