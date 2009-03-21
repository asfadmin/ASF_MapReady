
include ../../make_support/system_rules

LIBS  = \
        $(LIBDIR)/asf.a \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(JPEG_LIBS) \
	-lm

CFLAGS += $(GLIB_CFLAGS)
OBJS  = rick_conv.o

all: rick_conv

rick_conv: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o rick_conv $(OBJS) $(LIBS) $(LDFLAGS)
	mv rick_conv$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
