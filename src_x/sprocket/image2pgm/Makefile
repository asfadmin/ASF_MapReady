# Makefile for image2pgm

include  ../Make_rules

LIBS  = $(LIBDIR)/libUtil.a \
	-lm

OBJS  = image2pgm.o

all: image2pgm clean

image2pgm: $(OBJS)
	$(CC) $(CFLAGS) -o image2pgm $(OBJS) $(LIBS)
	mv image2pgm$(BIN_POSTFIX) $(BINDIR)
#	cp image2pgm.1 ../../man/cat1

clean:
	rm -f *.o
