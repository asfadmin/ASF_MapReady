# Makefile for 		:  coregister_coarse
# Module Author 	:  Mike Shindle, Tom Logan, Orion Lawlor

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = coregister_coarse.o \
	find_baseline.o 

all: coregister_coarse clean

coregister_coarse: $(OBJS)
	$(CC) $(CFLAGS) -o coregister_coarse $(OBJS) $(LIBS) $(LDFLAGS)
	mv coregister_coarse$(BIN_POSTFIX) $(BINDIR)
	cp coregister_coarse.1 ../../man/man1

clean:
	rm -f core *.o
