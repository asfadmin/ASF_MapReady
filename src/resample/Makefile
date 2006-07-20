# Makefile for 		:  resample
# Module Author 	:  Tom Logan & Mike Shindle

include ../../make_support/system_rules

OLIBS = $(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
        $(LIBDIR)/asf.a \
	$(PROJ_LIBS)

CFLAGS += $(GLIB_CFLAGS)

LIBS = $(OLIBS) -lm

resample:  resample.c $(OLIBS)
	$(CC) $(CFLAGS) -o resample resample.c $(OBJS) $(LIBS) $(LDFLAGS)
	rm -f *.o
	mv resample$(BIN_POSTFIX) $(BINDIR)
	cp resample.1 ../../man/man1/resample.1

clean:
	rm -f core* resample.o resample
