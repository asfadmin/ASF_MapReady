# Makefile for          :  gen_oe
# Module Author         :  Tom Logan   
# Module Version        :  9.0
#
# File to make is the gen_oe executable
#

include ../../make_support/system_rules

FFLAGS = -O

#
# Object libraries required to load the program
#

OBJS  =  asap.o angles.o anomly.o coord.o delm.o \
	dens76.o der.o ephem.o eqnox.o julian.o \
	kepler.o kozsak.o legend.o pout.o rk78.o \
	rk78cn.o setsun.o setthd.o userop.o xthird.o

# Set to 'true' (exactly, without quotes) to build with f2c.
BUILD_WITH_F2C = false

all:  gen_oe asap
	cp propagate.sh $(BINDIR)/propagate
	-chmod 775 $(BINDIR)/propagate
	cp propagate.man ../../man/cat1/propagate.1
	rm *.o

# Target specific variable LIBS for code build before fortran libs are
# unpacked.
gen_oe: LIBS = $(LIBDIR)/asf.a -lm 
gen_oe:	gen_oe.o
	$(CC) $(CFLAGS) -o gen_oe gen_oe.o $(LIBS)
	mv gen_oe$(BIN_POSTFIX) $(BINDIR)
	cp gen_oe.man ../../man/cat1/gen_oe.1

asap: $(OBJS) $(CINCFIL)
	$(F77) $(FFLAGS) $(LFLAGS) -o asap $(OBJS) $(LIBS)
	mv asap$(BIN_POSTFIX) $(BINDIR)
	cp HOW_TO_ASAP_NEW ../../man/cat1/asap.1

# For the f2c sources.
F2C_UNPACK_DIR = f2c-20010821.orig
ifeq (${BUILD_WITH_F2C}, true)
# Rules to build everything using the f2c translator.

# Disable built in implicit rules for making object files from fortran source.
%.o: %.f

LIBS = $(LIBDIR)/asf.a ./libF77.a ./libI77.a -lm 

# This target just documents and automates the steps needed to get f2c
# working from the debian source package I grabbed it from.
f2c_setup: clean
	tar xzvf f2c_20010821.orig.tar.gz
	cd $(F2C_UNPACK_DIR)/src ; $(MAKE) 'CC = gcc' ; cp f2c ../..
	cp $(F2C_UNPACK_DIR)/f2c.h .
	cp $(F2C_UNPACK_DIR)/libi77 .
	sh libi77
	cp $(F2C_UNPACK_DIR)/f2c.h libI77
	cd libI77 ; perl -p -i -e 's/ld -r -x/ld -r/' makefile ; \
            $(MAKE) 'CC = gcc' ; cp libI77.a ..
	cp $(F2C_UNPACK_DIR)/libf77 .
	sh libf77
	cp $(F2C_UNPACK_DIR)/f2c.h libF77
	cd libF77 ; perl -p -i -e 's/ld -r -x/ld -r/' makefile ; \
            $(MAKE) 'CC = gcc' ; cp libF77.a ..

%.c: %.f f2c_setup
	./f2c $<

endif

# General clean up rule.
clean: 
	rm -rf *.o $(patsubst %.o, %.c, $(OBJS)) *.a libi77 libf77 libI77 \
               libF77 f2c.h f2c $(F2C_UNPACK_DIR) core

