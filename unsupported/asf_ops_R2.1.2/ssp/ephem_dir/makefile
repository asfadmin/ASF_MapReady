# SccsId= @(#)makefile	2.41 3/24/98

# define for local library directory 
LOCAL_LIB   = ../../lib/$(MACH)/local


# Define for compilier options
FCOMP       = mpxlf -Pv  -Wp,-H/usr/lpp/ppe.poe/include  -O3 -qfixed=132
CCOMP       = cc

# Define the default rules
.f.o:
	-$(FCOMP) -c $<
.c.o:
	-$(CCOMP) -c $<

# The object files in the library ephem.a
OBJ_EPHEM = julian.o kozsak.o delm.o kepler.o coord.o \
            setsun.o rk78cn.o pout.o eqnox.o rk78.o userop.o \
            der.o setthd.o anomly.o legend.o angles.o ephem.o \
            xthird.o dens76.o eme_kepler.o gmt2sec.o \
            gmt2double.o gmt_decode.o

# Create the library and move to LOCAL_LIB directory
libephem.a: $(OBJ_EPHEM)
	ar rv libephem.a $(OBJ_EPHEM)
	#rm -f *.o *.lst
	mv libephem.a  $(LOCAL_LIB)  
	@echo libephem.a is now up to date

# Clean the directory
clean:
	rm -f *.o *.lst


