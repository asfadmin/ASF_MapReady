# Makefile for          :  gps_vector
# Module Author         :  Mike Shindle  

include ../../make_support/system_rules

OLIBS = $(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a

LIBS = $(OLIBS) -lm 
OBJS = 	gps_vector.o \
	alloc2d_1.o \
	ll_to_ssmi.o \
	ssmi_gps.o \
	s_to_dbl.o \
	drawline.o \
	create_gps_ddr.o \
	vector.o \
	grid.o \
	find_closest_vector.o \
	print_data_info.o


gps_vector: $(OBJS) $(OLIBS)
	$(CC) $(CFLAGS) -o gps_vector $(OBJS) $(LIBS)
	rm *.o
	mv gps_vector$(BIN_POSTFIX) $(BINDIR)
	cp gps_vector.1 ../../man/man1

