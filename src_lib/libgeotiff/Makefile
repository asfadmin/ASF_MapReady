# GeoTIFF Library source for Makefile
#

# Hacked by Patrick Denny (ASF) Oct. 2002

include ../../make_support/system_rules

AR      = ar
AROPTS  = rcv
LN      = ln -s
RM      = rm -f

LIB_XTIFF = ./libxtiff
XTIFF     = xtiff.o
XTIFFINC  = xtiffio.h xtiffiop.h

VERSION_MAJOR = 1
VERSION_MINOR = 1
VERSION_POINT = 4

# Left for info... all include files
#GT_INCLUDE_FILES = xtiffio.h xtiffiop.h geotiff.h geotiffio.h geovalues.h \
#	geonames.h geokeys.h geo_tiffp.h geo_config.h geo_keyp.h \
#	geo_normalize.h cpl_serv.h cpl_csv.h \
#	epsg_datum.inc epsg_gcs.inc epsg_pm.inc epsg_units.inc geo_ctrans.inc\
#	epsg_ellipse.inc epsg_pcs.inc epsg_proj.inc epsg_vertcs.inc geokeys.inc

# '-I../libtiff' is for a slew of .h files that are needed from libtiff
CFLAGS = $(OPTFLAG) $(INCLUDE) -I../libtiff

OBJS  = ${XTIFF} \
	geo_free.o \
	geo_get.o \
	geo_names.o \
	geo_new.o \
	geo_print.o \
	geo_set.o \
	geo_tiffp.o \
	geo_write.o \
	geo_trans.o \
	geo_normalize.o \
	geotiff_proj4.o \
	geo_extra.o \
	cpl_serv.o \
	cpl_csv.o

all: libgeotiff.a
	mv libgeotiff.a $(LIBDIR)
	$(RM) ${OBJS} ${XTIFFINC}

libgeotiff.a: ${OBJS}
	${AR} ${AROPTS} libgeotiff.a $(OBJS)
	${RANLIB} libgeotiff.a

${XTIFF}: ${LIB_XTIFF}/xtiff.c ${XTIFFINC}
	${CC} -c ${CFLAGS} ${LIB_XTIFF}/xtiff.c

${XTIFFINC}:
	${RM} ${XTIFFINC}
	${LN} ${LIB_XTIFF}/*.h .

clean:
	rm -f libgeotiff.a ${OBJS} ${XTIFFINC} core a.out
