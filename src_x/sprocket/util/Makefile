# Makefile for ASF Converter
include ../mk.common

OBJECTS= util.o 
all: $(LIBARY)/libUtil.a  $(INCLUDE)/util.h $(INCLUDE)/metadata.h $(INCLUDE)/ceos_rd.h

$(INCLUDE)/util.h: util.h
	cp util.h $(INCLUDE)/util.h

$(LIBARY)/libUtil.a: $(OBJECTS)
	ar -r $(LIBARY)/libUtil.a $(OBJECTS)

$(INCLUDE)/metadata.h: metadata.h
	cp metadata.h  $(INCLUDE)/metadata.h

$(INCLUDE)/ceos_rd.h: ceos_rd.h
	cp ceos_rd.h $(INCLUDE)/ceos_rd.h

util.o: util.c
	$(COMP) util.c
#	$(BEAT) util.c util.h
clean:
	rm -f $(LIBARY)/libUtil.a $(INCLUDE)/util.h *.o  $(INCLUDE)/metadata.h $(INCLUDE)/ceos_rd.h
