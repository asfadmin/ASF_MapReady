# Makefile for          :  gen_oe
# Module Author         :  Tom Logan   
# Module Version        :  9.0
#
# File to make is the gen_oe executable
#

include ../../make_support/system_rules

FFLAGS = -O
LIBS   = $(LIBDIR)/asf.a -lm 

#
# Object libraries required to load the program
#
 
OBJS  =  asap.o angles.o anomly.o coord.o delm.o \
	dens76.o der.o ephem.o eqnox.o julian.o \
	kepler.o kozsak.o legend.o pout.o rk78.o \
	rk78cn.o setsun.o setthd.o userop.o xthird.o

all:  gen_oe asap
	cp propagate $(BINDIR)
	-chmod 775 $(BINDIR)/propagate
	cp propagate.man ../../man/cat1/propagate.1
	rm *.o

gen_oe:	gen_oe.o
	$(CC) $(CFLAGS) -o gen_oe gen_oe.o $(LIBS)
	mv gen_oe$(BIN_POSTFIX) $(BINDIR)
	cp gen_oe.man ../../man/cat1/gen_oe.1

asap: $(OBJS) $(CINCFIL)
	$(F77) $(FFLAGS) $(LFLAGS) -o asap $(OBJS) $(LIBS)
	mv asap$(BIN_POSTFIX) $(BINDIR)
	cp HOW_TO_ASAP_NEW ../../man/cat1/asap.1
