# Makefile for          :  asf_las.a
# Module Author         :  Tom Logan & Mike Shindle
# Module Version        :  1.2
# Convert .c files to .o list:
#    >  ls *.c | sed -e "s/\.c/.o/" | awk '{print "\t"$1"\\"}'

include ../../make_support/system_rules

OBJ= \
        allocbuf.o \
        c_calcor.o \
        c_comsys.o \
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
        diskio.h \
        doopen.o \
        lasIO.o  \
        packed_deg.o \
        squeeze. o 


all: asf_las.a
	rm *.o
	cp asf_las.man ../../man/cat1/asf_las.1

asf_las.a: $(OBJ)
	ar rv asf_las.a $(OBJ)
	$(RANLIB) asf_las.a
	mv asf_las.a $(LIBDIR)
