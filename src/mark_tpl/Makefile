# Makefile for          :  mark_tpl
# Module Author         :  Tom Logan 
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_las.a \
	-lm

OBJS  = make_cross.o
 
mark_tpl : $(OBJS) 
	$(CC) $(CFLAGS) mark_tpl.c -o mark_tpl $(OBJS) $(LIBS)
	rm *.o
	mv mark_tpl$(BIN_POSTFIX) $(BINDIR)
	cp mark_tpl.man ../../man/cat1/mark_tpl.1

