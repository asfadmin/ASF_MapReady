# Makefile for deskew

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	-lm

OBJS  = deskew.o 

all: deskew clean

deskew: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o deskew $(OBJS) $(LIBS) $(LDFLAGS)
	mv deskew$(BIN_POSTFIX) $(BINDIR)
	cp deskew.1 ../../man/cat1/deskew.1

clean:
	rm *.o
