# Makefile for          :  greycorr
# Module Author         :  Tom Logan   
# Module Version        :  9.0
#
# File to make is the greycorr executable
#
 
include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	-lm
 
OBJS  = greycorr.o \
	eval.o \
	grey.o \
	fitreg.o 
 
# Make the executable file for program "greycorr".
#
all: greycorr clean

greycorr:       $(OBJS) 
	$(CC) $(CFLAGS) $(LFLAGS) -o greycorr $(OBJS) $(LIBS)
	mv greycorr$(BIN_POSTFIX) $(BINDIR)
	cp greycorr.man ../../man/cat1/greycorr.1

clean:
	rm *.o

