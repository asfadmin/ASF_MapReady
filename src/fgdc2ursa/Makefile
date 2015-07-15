CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for fgdc2ursa

DEST_TARGET = fgdc2ursa
ORACLE_PRECOMP = /oracle/u01/app/oracle/product/10.1.0/Db_1/precomp/public
ORACLE_LIB = /oracle/u01/app/oracle/product/10.1.0/Db_1/lib

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_proj.a \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(PROJ_LIBS) \
        $(TIFF_LIBS) \
        $(JPEG_LIBS) \
        $(PNG_LIBS) \
        $(GEOTIFF_LIBS) \
	$(XML_LIBS) \
	$(ZLIB_LIBS) \
	$(ORACLE_LIB)/libclntsh.so \
	$(ORACLE_LIB)/libsql10.a

CFLAGS += $(XML_CFLAGS) -D REENTRANT

OBJS  = oracle.o fgdc2ursa.o

all:	oracle fgdc2ursa clean

oracle:
	cp $(ORACLE_PRECOMP)/*.h $(ASF_INCLUDE_DIR)
	proc iname=oracle.pc

fgdc2ursa: $(OBJS)
	$(CC) $(CFLAGS) -o fgdc2ursa $(OBJS) $(LIBS) $(LDFLAGS)
	mv fgdc2ursa$(BIN_POSTFIX) $(BINDIR)/$(DEST_TARGET)$(BIN_POSTFIX)

clean:
	rm -f core $(OBJS) *.o *~
