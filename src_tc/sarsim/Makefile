# Makefile for          :  sarsim 
# Module Author         :  Tom Logan   
# Module Version        :  4.0
 
include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/sarmodel.a \
	$(LIBDIR)/tcobj.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	-lm

OBJS  = sim_init.o 
 
sarsim : sarsim.c $(CINCFIL) $(OBJS) 
	$(CC) $(CFLAGS) -o sarsim sarsim.c $(OBJS) $(LIBS) $(LDFLAGS)
	mv sarsim$(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp sarsim.man ../../man/cat1/sarsim.1

sim_init.o : sim_init.c
	$(CC) $(CFLAGS) -c sim_init.c 
