#	las_text
#	by Orion Lawlor, ASF, 1997
#	A program to write text on LAS images.

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS= main.o

all: las_text
	-rm *.o

las_text: $(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o las_text $(OBJS) ${LIBS}
	mv las_text$(BIN_POSTFIX) $(BINDIR)
	cp las_text.1 ../../man/cat1

clean:
	-rm -f core ${OBJS} *.o
