include ../../make_support/system_rules

PROGRAM = asf_export

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	-lgeotiff -ltiff \
        -ljpeg \
        -L$(HOME)/local/lib -lgsl -lgslcblas -lm \
        -lefence

OBJS  = asf_export.o

SRCS = $(patsubst %.o, %.c, $(OBJS))

CFLAGS += -I$(HOME)/local/include -D_ISOC99_SOURCE -D_GNU_SOURCE

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	echo $(LIBS)
	$(CC) $(CFLAGS) $(LFLAGS) $(OBJS) -L$(LIBDIR) $(LIBS) -o $(PROGRAM)
	cp $(PROGRAM) $(BINDIR)

clean:
	rm -f *.o $(PROGRAM)
