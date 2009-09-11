# Makefile for		: auto_refine_base
# Module Author		: Greg Short

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/libifm.a \
	-lm

# Debugging help
#LIBS := $(LIBS) /test/home/pdenny/local/lib/libefence.a
#CC = gcc
#CFLAGS := $(CFLAGS) -g -Wall

OBJS =  auto_refine_base.o

all: auto_refine_base clean

auto_refine_base: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o auto_refine_base $(OBJS) $(LIBS) $(LDFLAGS)
	mv auto_refine_base$(BIN_POSTFIX) $(BINDIR)
#	cp auto_refine_base.1 ../../man/man1

clean:
	rm -f *.o core
