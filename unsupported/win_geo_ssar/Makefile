# Makefile for          :  win_geo_ssar
# Module Author         :  Tom Logan 
# Module Version        :  1.0

include ../../make_support/system_rules

LIBS   = $(LIBDIR)/asf_meta.a \
	 $(LIBDIR)/asf_las.a \
	 $(LIBDIR)/asf.a \
	 -lm

CINCFIL = $(INCDIR)/ddr.h 

win_geo_ssar : $(CINCFIL) $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) win_geo_ssar.c -o win_geo_ssar $(OBJS) $(LIBS)
	mv win_geo_ssar$(BIN_POSTFIX) $(BINDIR)
	cp win_geo_ssar.man ../../man/cat1/win_geo_ssar.1
	@ echo "**** win_geo_ssar compiled ****"

