# Makefile for 		:  make_slant
# Module Author 	:  Tom Logan
# Module Version	:  1.0

include ../../make_support/system_rules

OBJS =  make_slant.o\
	$(LIBDIR)/asf_meta.a \
 	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a

all: make_slant clean

make_slant: $(OBJS) 
	$(CC) $(CFLAGS) -o make_slant $(OBJS) -lm
	mv make_slant$(BIN_POSTFIX) $(BINDIR)

clean:
	-rm *.o
