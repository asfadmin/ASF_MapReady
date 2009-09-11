# Makefile for          :  accum_offset
# Module Author         :  Mike Shindle
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	-lm
OBJS  = accum_offset.o      


accum_offset: $(OBJS) $(OLIBS)
	$(CC) $(CFLAGS)  -o accum_offset $(OBJS) $(LIBS) $(LDFLAGS)
	mv accum_offset$(BIN_POSTFIX) $(BINDIR)
	cp accum_offset.1 ../../man/cat1/accum_offset.1
	rm *.o
