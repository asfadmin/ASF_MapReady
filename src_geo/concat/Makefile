# Makefile for        :  concat
# Module Author       :  Tom Logan & Mike Shindle
# Modifier	      :  Patrick Denny
# Module Version      :  2.0

include ../../make_support/system_rules

LIBS =	$(LIBDIR)/asf_concat.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	-lm 

OBJS =  averopt.o \
	cal_lin_smp.o \
	overlay_img.o \
	pad.o \
	set_dtype.o

all: concat
	rm *.o

concat:	concat.c $(OBJS) 
	$(CC) $(CFLAGS) -o concat concat.c $(OBJS) $(LIBS) $(LDFLAGS)
	mv concat$(BIN_POSTFIX) $(BINDIR)
	cp concat.man ../../man/cat1/concat.1

