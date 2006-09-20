# Makefile for:  geotiffdump
# Module Author:  Brian Dixon, 09/2006

include ../../make_support/system_rules


LIBS =	\
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(JPEG_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	-lm \
	-lz

OBJS =	geotiffdump.o 

CFLAGS += -D_GNU_SOURCE -g

all: geotiffdump clean

geotiffdump: $(OBJS)
	$(CC) $(CFLAGS) -o geotiffdump $(OBJS) $(LIBS)
	#cp geotiffdump.1 ../../man/man1
	mv geotiffdump$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
	@echo "BUILD SUCCESSFUL"
