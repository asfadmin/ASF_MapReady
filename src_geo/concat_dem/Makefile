#	concat_dem
#	by Orion Lawlor, ASF, 1998
#	A program to overlay a series of DEMs atop
#       one another.

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm
OBJS  = open_dem.o \
	extents.o \
	projection.o

COBJS= main.o $(OBJS)
POBJS= profile.o $(OBJS)

all: concat_dem profile
	rm *.o

concat_dem:	$(COBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o concat_dem $(COBJS) ${LIBS}
	mv concat_dem $(BINDIR)
	cp concat_dem.1 ../../man/cat1/concat_dem.1

profile:	$(POBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o profile $(POBJS) ${LIBS}
	mv profile$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core ${OBJS} *.o
