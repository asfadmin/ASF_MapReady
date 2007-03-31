include ../../make_support/system_rules

TARGET = create_thumbs

LIBS = \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_vector.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(GTK_CFLAGS) \
	$(JPEG_LIBS)

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GTK_LIBS) \
	-D_XOPEN_SOURCE=600

$(TARGET):  $(TARGET).c
	$(CC) $(CFLAGS) -o $(TARGET) $(TARGET).c $(OBJS) $(LIBS) $(LDFLAGS)
	rm -f *.o
	mv $(TARGET)$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core* $(TARGET).o $(TARGET)
