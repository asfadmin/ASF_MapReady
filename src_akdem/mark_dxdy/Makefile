# Makefile for          :  mark_dxdy
# Module Author         :  Rudi Gens 
# Module Version        :  1.0

include ../../make_support/system_rules

LIBS   = -lm

#
# Object libraries required to load the program
#
OBJLIB = make_arrow.o \
	 $(LIBDIR)/asf_las.a $(LIBDIR)/asf.a
 
mark_dxdy : $(OBJLIB) 
	$(CC) $(CFLAGS) mark_dxdy.c -o mark_dxdy $(OBJLIB) $(LIBS)
	rm *.o
	mv mark_dxdy$(BIN_POSTFIX) $(BINDIR)
	cp mark_dxdy.man ../../man/cat1/mark_dxdy.1

