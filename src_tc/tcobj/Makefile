# Makefile for          :  tc objects
# Module Author         :  Tom Logan
# Module Version        :  4.0

include ../../make_support/system_rules

OBJECTS = tm.o cproj.o mod_ddr_dtype.o

tcobj.a:	$(OBJECTS)
	ar rv tcobj.a $(OBJECTS)
	$(RANLIB) tcobj.a
	mv tcobj.a $(LIBDIR)
	rm *.o
