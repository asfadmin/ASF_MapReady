# Makefile for          :  crop
# Module Author         :  Tom Logan 
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS   = \
	 $(LIBDIR)/asf_meta.a \
	 $(LIBDIR)/asf.a \
	 -lm

OBJS =  bounds.o \
	ssar_proj.o \
	find_corners.o \
	exact_corners.o \
	image_resample.o \
	writeAsfCeosData.o \
	modifyAsfLeader.o

ssar_crop : $(CINCFIL) $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) ssar_crop.c -o ssar_crop $(OBJS) $(LIBS)
	rm *.o
	mv ssar_crop$(BIN_POSTFIX) $(BINDIR)
	cp ssar_crop.man ../../man/cat1/ssar_crop.1
	@ echo "**** ssar_crop compiled ****"

