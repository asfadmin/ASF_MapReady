# Makefile for image_mask

include  ../Make_rules

LIBS  = $(LIBDIR)/libUtil.a \
	-lm

OBJS  = image_mask.o

all: image_mask clean

image_mask: $(OBJS)
	$(CC) $(CFLAGS) -o image_mask $(OBJS) $(LIBS)
	mv image_mask$(BIN_POSTFIX) $(BINDIR)
#	cp image_mask.1 ../../man/cat1

clean:
	rm -f *.o
