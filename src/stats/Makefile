# Makefile for ASF stats program

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

OBJS  = stats.o

all: stats clean

stats: $(OBJS)
	${CC} ${CFLAGS} -o stats $(OBJS) ${LIBS}
	mv stats$(BIN_POSTFIX) $(BINDIR)
	cp stats.1 ../../man/man1/.

clean:
	rm -f ${OBJS}
