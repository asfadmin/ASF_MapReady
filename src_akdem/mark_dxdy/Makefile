# Makefile for          :  mark_dxdy
# Module Author         :  Rudi Gens 
# Module Version        :  1.0

include ../../make_support/system_rules

LIBS   = $(LIBDIR)/asf.a \
	-lm

OBJS = make_arrow.o
 
mark_dxdy : $(OBJS) 
	$(CC) $(CFLAGS) mark_dxdy.c -o mark_dxdy $(OBJS) $(LIBS)
	rm *.o
	mv mark_dxdy$(BIN_POSTFIX) $(BINDIR)
	cp mark_dxdy.man ../../man/cat1/mark_dxdy.1

