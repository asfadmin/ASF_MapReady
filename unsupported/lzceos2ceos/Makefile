# Makefile for          :  sarout 
# Module Author         :  Tom Logan 
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
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


all: sarout lzceos2ceos clean

sarout: $(CINCFIL) $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) sarout.c -o sarout $(OBJS) $(LIBS)
	mv sarout$(BIN_POSTFIX) $(BINDIR)
	cp sarout.man ../../man/cat1/sarout.1

lzceos2ceos:
	cp lzceos2ceos.sh $(BINDIR)/lzceos2ceos
	-chmod 775 $(BINDIR)/lzceos2ceos

clean:
	rm -f *.o
