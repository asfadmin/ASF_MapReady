include ../../make_support/system_rules

PROGRAM = export

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	-ltiff -lgeotiff -L$(HOME)/local/lib -lWand -lgsl -lgslcblas -lm

OBJS  = export.o

SRCS = $(patsubst %.o, %.c, $(OBJS))

CFLAGS += -I$(HOME)/local/include -D_ISOC99_SOURCE -D_GNU_SOURCE

all: $(PROGRAM) clean

$(PROGRAM): $(OBJS)
	echo $(LIBS)
	$(CC) $(CFLAGS) $(LFLAGS) $(OBJS) -L$(LIBDIR) $(LIBS) -o $(PROGRAM)
#	mv $(PROGRAM) $(BINDIR)

clean:
	rm -f *.o
