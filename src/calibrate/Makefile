# Makefile for 		:  calibrate
# Module Author 	:  Tom Logan
# Module Version	:  1.0

include ../../make_support/system_rules

LIBS =	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a

OBJS =  calibrate.o \
	cal_params.o

all: calibrate clean

calibrate: $(OBJS)
	$(CC) $(CFLAGS) -o calibrate $(OBJS) $(LIBS) -lm
	mv calibrate$(BIN_POSTFIX) $(BINDIR)
	cp calibrate.1 ../../man/man1/

clean:
	-rm *.o
