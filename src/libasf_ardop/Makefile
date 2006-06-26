include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_odl.a \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a
LIBC = $(LIBS) -lm 

OBJLIB = ardop_libs.o \
	 cfft1d_risc.o \
	 estdop.o \
	 fft_dop.o \
	 read_signal.o \
	 ardop.o \
	 get_params.o

SPECLIB = \
	specan.o \
	specan_ml.o \
	specan_patch.o \
	specan_file.o \
	quicklook.o

ASPLIB = patch.o \
	 ardop_setup.o \
	 rciq.o rmpatch.o acpatch.o calibration.o

# Filter out the ISOC99 FLAGS which causes some problems.
CFLAGS := $(patsubst -D_ISOC99_SOURCE, , $(CFLAGS))

all: build_only
	mv libasf_ardop.a $(LIBDIR)
	cp ardop_defs.h $(ASF_INCLUDE_DIR)
	cp read_signal.h $(ASF_INCLUDE_DIR)

build_only: $(OBJLIB) $(ASPLIB)
	rm -f libasf_ardop.a
	ar r libasf_ardop.a $(OBJLIB) $(ASPLIB)
	$(RANLIB) libasf_ardop.a

clean:
	rm -f core* $(OBJLIB) *.o
