# Makefile for: p2c 

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	-lm

OBJS = p2c.o 

p2c: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o p2c $(OBJS) $(LIBS) $(LDFLAGS)
	rm *.o
	mv p2c$(BIN_POSTFIX) $(BINDIR)
	cp p2c.1 ../../man/cat1

clean:
	rm -f $(OBJS) core
