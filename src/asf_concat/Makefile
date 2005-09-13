# Makefile for          :  asf_concat.a
# Module Author         :  Tom Logan & Mike Shindle
# Module Version        :  1.2
# Line used to convert .c files into .o list:
# >  ls *.c | sed -e "s/\.c/.o/" | awk '{print "\t"$1"\\"}'

include ../../make_support/system_rules

OBJ= \
        c_ealloc.o\
        c_eclose.o\
        c_egclse.o\
        c_egroup.o\
        c_eopenr.o\
        c_eopens.o\
        c_eread.o\
        c_estep.o\
        c_ewrite.o\
	c_upddr.o\
        com_ppar.o\
        chkwindow.o\
        encode_76.o\
        processit.o\
        set_capture.o\
        set_corners.o\
        set_datum.o\
        set_inc_flag.o\
        set_master.o\
        set_pdist.o\
        set_ppar.o\
        set_proj.o\
        set_punit.o\
        set_zone.o\
        setcornflag.o\
        setincrement.o\
        setpdisflag.o

all: asf_concat.a
	rm *.o

asf_concat.a: $(OBJ)
	ar rv asf_concat.a $(OBJ)
	$(RANLIB) asf_concat.a
	mv asf_concat.a $(LIBDIR)
# Link to library with conventional name (with 'lib' prefix).
	-ln -s $(LIBDIR)/asf_concat.a $(LIBDIR)/libasf_concat.a
