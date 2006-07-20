# Makefile for          :  sarout 
# Module Author         :  Tom Logan 
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/asf_odl.a \
	-lm

OBJS  = addMetaToCeos.o \
	addRadioToCeos.o \
	get_slant.o \
	calcSwathWidth.o \
	ctok.o \
	sensor_fields.o \
	createLeader.o \
	readLeader.o \
	modifyLeader.o \
	createCeosFromSignal.o \
	writeAsfCeosLeader.o \
	writeAsfCeosData.o \
	writeAsfCeosSignal.o \
	ceos_misc.o


all: sarout clean

sarout: $(CINCFIL) $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) sarout.c -o sarout $(OBJS) $(LIBS) $(LDFLAGS)
	mv sarout$(BIN_POSTFIX) $(BINDIR)
	cp sarout.man ../../man/cat1/sarout.1

clean:
	rm -f *.o
