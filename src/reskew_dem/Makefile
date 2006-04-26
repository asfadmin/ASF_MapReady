#	reskew_dem
#	by Orion Lawlor, ASF, 1997
#	Reskew will remap a ground range DEM into a slant-range
#       DEM.

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	-lm

OBJS= reskew_dem.o

reskew_dem: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o reskew_dem $(OBJS) $(LIBS)
	mv reskew_dem$(BIN_POSTFIX) $(BINDIR)
	cp reskew_dem.1 ../../man/cat1/.

clean:
	rm -f core $(OBJS) *.o
