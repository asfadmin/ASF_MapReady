# Makefile for 		:  dem_diff
# Module Author 	:  Tom Logan
# Module Version	:  1.0

include ../../make_support/system_rules

0BJLIB =
OBJS =  dem_diff.o\
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf.a


all: dem_diff clean

dem_diff: $(OBJS) 
	$(CC) $(CFLAGS) -o dem_diff $(OBJS) $(OBJLIB) -lm
	mv dem_diff$(BIN_POSTFIX) $(BINDIR)
	cp dem_diff.1 ../../man/cat1/

clean:
	-rm *.o
