# Makefile for          :  gen_oe
# Module Author         :  Tom Logan   
# Module Version        :  9.0
# Makefile Date         :  10/19/95  
#
# File to make is the gen_oe executable
#
 
#
# Set up build options--the debug switch (debug or optimize), 
# and directories to search for include files
#

INC = ../asf_inc
ASFLIB = ../../lib/$(SYS)
LFLAGS = -L$(ASFLIB)
CFLAGS = -I$(INC) $(FLAGS)
LIBS   = -lm $(SUNMATH)

#
# Include files needed for correlate.  The source code depends on
# these files.  Therefore, if one of the include files are
# modified, the module will be recompiled.
#
 
CINCFIL = $(INC)/geompak.h \
	  $(INC)/asf_sar.h
 
#
# Object libraries required to load the program
#
 
OBJS  =  asap.o angles.o anomly.o coord.o delm.o \
	dens76.o der.o ephem.o eqnox.o julian.o \
	kepler.o kozsak.o legend.o pout.o rk78.o \
	rk78cn.o setsun.o setthd.o userop.o xthird.o


# Make the executable file for program "gen_oe".
#
asap:       $(OBJS) $(CINCFIL)
	$(F77) $(FLAGS) $(LFLAGS) -o asap $(OBJS) $(LIBS)
	rm *.o
#	mv gen_oe ../../bin/$(SYS)
#	cp gen_oe.man ../../man/cat1/gen_oe.1


gen_oe:	gen_oe.o
	$(CC) $(CFLAGS) -o gen_oe gen_oe.o $(LIBS)
