# Makefile for 		:  orb_pred
# Module Author 	:  Mike Shindle
# Module Version	:  1.0 

include ../../make_support/system_rules

OLIBS = $(LIBDIR)/asf.a  
LIBS = $(OLIBS) -lm 
OBJS = orb_pred.o print_table.o \
	midpoint.o \
	predict_orbit.o date.o solve3AXB.o

orb_pred: $(OBJS) $(OLIBS)
	$(CC) $(CFLAGS) -o orb_pred $(OBJS) $(LIBS)
	rm *.o
	mv orb_pred$(BIN_POSTFIX) $(BINDIR)
	cp orb_pred.man ../../man/cat1/orb_pred.1

