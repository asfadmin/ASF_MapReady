# Makefile
# Author Rudi Gens

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm

DDR_OBJS = \
	c_calcor.o \
	c_dkcre.o \
	c_dkdel.o \
	c_dkfree.o \
	c_dkresv.o \
	c_dkrspac.o \
	c_errmsg.o \
	c_getbdr.o \
	c_getddr.o \
	c_getsys.o \
	c_gettyp.o \
	c_intbdr.o \
	c_intddr.o \
	c_low2up.o \
	c_lsclos.o \
	c_lsmknm.o \
	c_lsopen.o \
	c_lsread.o \
	c_lsrepl.o \
	c_lsstat.o \
	c_lswrit.o \
	c_ptio.o \
	c_ptopen.o \
	c_putbdr.o \
	c_putddr.o \
	c_pxconv.o \
	c_pxcopy.o \
	c_pxfill.o \
	c_pxswap.o \
	c_pxsys.o \
	c_rotcor.o \
	c_rtcoef.o \
	c_sysset.o \
	c_up2low.o \
	convInt.o \
	squeeze.o


H2D_OBJS = \
	$(DDR_OBJS) \
	get_hdr.o \
	hdr2ddr.o \
	hdr2ddr_main.o

D2H_OBJS = \
	$(DDR_OBJS) \
	ddr2hdr.o \
	ddr2hdr_main.o \
	put_hdr.o

all: hdr2ddr ddr2hdr clean

hdr2ddr: $(H2D_OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o hdr2ddr $(H2D_OBJS) $(LIBS) $(LDFLAGS)
	mv hdr2ddr$(BIN_POSTFIX) $(BINDIR)

ddr2hdr: $(D2H_OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o ddr2hdr $(D2H_OBJS) $(LIBS) $(LDFLAGS)
	mv ddr2hdr$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core
