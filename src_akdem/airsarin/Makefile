# Makefile for          :  airsarin 
# Module Author         :  Joshua Badgley 

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/asf.a  $(LIBDIR)/asf_las.a -lm
OBJS = airsarin.o \
       airsar2ddr.o \
       airsar_io.o

airsarin: $(OBJS)
	$(CC) $(CFLAGS) -o airsarin $(OBJS) $(LIBS)
	rm *.o
	mv airsarin $(BIN_POSTFIX) $(BINDIR)
	cp airsarin.1 ../../man/cat1/airsarin.1


