# Makefile for          :  projprm 
# Module Author         :  Tom Logan & Mike Shindle

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_lt.a \
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf.a 

OBJS = 	geograph.o \
	wrlabtab.o \
	plstereo.o \
	utm.o	\
	lamazeqa.o \
	albers.o


 #-lm needed for solaris
projprm: projprm.c $(OBJS)
	$(CC) $(CFLAGS) -o projprm projprm.c $(OBJS) $(LIBS) -lm
	mv projprm $(BINDIR)/projprm
	cp projprm.man ../../man/cat1/projprm.1
	rm *.o

#geograph.o:	geograph.c 
#		$(CC) $(CFLAGS) -c geograph.c
#
#plstereo.o:	plstereo.c 
#		$(CC) $(CFLAGS) -c plstereo.c
#
#wrlabtab.o:	wrlabtab.c 
#		$(CC) $(CFLAGS) -c wrlabtab.c
#
#utm.o:		utm.c
#		$(CC) $(CFLAGS) -c utm.c
#		
#lamazeqa.o:	lamazeqa.c
#		$(CC) $(CFLAGS) -c lamazeqa.c
#
#albers.o:	albers.c
#		$(CC) $(CFLAGS) -c albers.c
#
