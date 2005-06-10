# Makefile for          :  airsarin 
# Module Author         :  Joshua Badgley 

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm

OBJS = airsarin.o \
       airsar2ddr.o \
       airsar_io.o

airsarin: $(OBJS)
	$(CC) $(CFLAGS) -o airsarin $(OBJS) $(LIBS)
	mv airsarin $(BIN_POSTFIX) $(BINDIR)
	cp airsarin.1 ../../man/cat1/airsarin.1
	rm -f $(OBJS)

