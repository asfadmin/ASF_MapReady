# Makefile for 		:  las2geotiff
# Module Author 	:  Sherman Watts (used tae code as referenced)

include ../../make_support/system_rules

TEST = /3dsar1/swatts/mytools2/lib/irix
TEST2 = /usr/lib32
TEST3 = ../../lib/irix
TEST4 = /usr/local/lib

LIBS =  $(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a \
	$(TEST4)/libgeotiff.a $(TEST3)/libtiff.so -lm

OBJS = get_tiepoints.o \
	c_decdeg.o \
	c_getdatum.o \
	upcase.o \
	geo_tiffp.o \
        get_utmzone.o \
	get_utmzonegen.o \
	get_linear_units.o \
	get_angular_units.o \
	get_stateplanezone.o \
        set_major_minor.o \
        get_sphere.o \
	geokeyset.o \
	utmkeyset.o \
	spcskeyset.o \
	alberskeyset.o \
	lamcckeyset.o \
	mercatkeyset.o \
	pskeyset.o \
	polyckeyset.o \
	equidckeyset.o \
	tmkeyset.o \
	stereokeyset.o \
	lamazkeyset.o \
	azmeqdkeyset.o \
	gnomonkeyset.o \
	orthokeyset.o \
	gvnspkeyset.o \
	snsoidkeyset.o \
	eqrectkeyset.o \
	millerkeyset.o \
	vgrintkeyset.o \
	homkeyset.o \
	robinkeyset.o \
	somkeyset.o \
	alaskakeyset.o \
	goodkeyset.o \
	mollkeyset.o \
	imollkeyset.o \
	hammerkeyset.o \
	wagivkeyset.o \
	wagviikeyset.o \
	obeqakeyset.o \
	get_geocode.o \
	las2geotiff_IO.o \
	las2geotiff.o 

las2geotiff: $(OBJS)
	$(CC) $(CFLAGS) -o las2geotiff $(OBJS) $(LIBS)
	cp las2geotiff.1 ../../man/cat1
	cp las2geotiff ../../bin/irix
	rm *.o

