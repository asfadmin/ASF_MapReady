# Makefile for 		:  convert_geo_ddr
# Module Author 	:  Sherman Watts

include ../../make_support/system_rules

OLIBS = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a
LIBS = $(OLIBS) -lm 

convert_geo_ddr:  convert_geo_ddr.c $(OBJS)  $(OLIBS)
	$(CC) $(CFLAGS) -o convert_geo_ddr convert_geo_ddr.c $(OBJS) $(LIBS)
	mv convert_geo_ddr$(BIN_POSTFIX) $(BINDIR)
	cp convert_geo_ddr.1 ../../man/cat1
