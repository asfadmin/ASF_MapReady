# Makefile for        :  hist
# Module Author       :  Mike Jessop 8/08/00
# Module Version      :  1.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

hist: hist.c
	$(CC) $(CFLAGS) -o hist hist.c $(LIBS)
	cp hist $(BINDIR) 
	mv hist ../bin/
	
