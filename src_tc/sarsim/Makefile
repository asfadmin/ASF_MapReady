# Makefile for          :  sarsim 
# Module Author         :  Tom Logan   
# Module Version        :  4.0
 
include ../../make_support/system_rules

LIBS = -lm
 
#
# Object libraries required to load the program
#
OBJLIB = $(LIBDIR)/sarmodel.a \
	   $(LIBDIR)/tcobj.a \
	   $(LIBDIR)/asf_las.a \
	   $(LIBDIR)/asf_meta.a \
	   $(LIBDIR)/asf.a \
	   sim_init.o 
 
sarsim : sarsim.c $(CINCFIL) $(OBJLIB) 
	$(CC) $(CFLAGS) -o sarsim sarsim.c $(OBJLIB) $(LIBS)
	mv sarsim$(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp sarsim.man ../../man/cat1/sarsim.1

sim_init.o : sim_init.c
	$(CC) $(CFLAGS) -c sim_init.c 
