# Makefile for 		:  sarmodel.a 
# Module Author 	:  Tom Logan & Mike Shindle  
# Module Version	:  2.0

include ../../make_support/system_rules

OBJECTS = sar_sim_ers1.o \
	  calcnorm.o \
	  calc4norm.o \
	  ers1read.o \
	  cintensity.o \
	  calcrcoefs.o \
	  calcmask.o \
	  mag.o \
	  sph2cart.o \
	  changellips.o \
	  movesat.o \
	  ic2efc.o \
	  det2centh.o \
	  centh2det.o \
	  inttar.o \
	  movetar.o \
	  dot.o \
	  los.o \
	  fd2dot.o \
	  calcfd2.o \
	  cart2sph.o \
	  compang.o \
	  packtar.o

sarmodel.a:	$(OBJECTS)
	ar rv sarmodel.a $(OBJECTS)
	$(RANLIB) sarmodel.a
	mv sarmodel.a $(LIBDIR)
	rm *.o
	
