include ../../make_support/system_rules

PROGRAM = export

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_odl.a \
	-lm

OBJS  = export.o

SRCS = $(patsubst %.o, %.c, $(OBJS));

all: $(PROGRAM) clean

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) $(SRCS) $(OBJS) $(LIBS) -o $(PROGRAM)
	mv $(PROGRAM) $(BINDIR)

clean:
	rm -f *.o
