# Makefile for          :  asf_lt.a 
# Module Author         :  Tom Logan   

include ../../make_support/system_rules

SUBS = \
		close_tab.o \
		error.o \
		get_record.o \
		get_vector.o \
		getfieldptr.o \
		getstrfield.o \
		open_tab.o \
		put_matrix.o \
		put_record.o \
		put_vector.o \
		putnullfld.o \
		putnullmatr.o \
		putstrfield.o \
		read_tab.o \
		write_tab.o

asf_lt.a:	$(SUBS) 
		ar rv asf_lt.a $(SUBS)
		$(RANLIB) asf_lt.a
		mv asf_lt.a $(LIBDIR) 
#		cp asf_lt.man ../../man/cat1/asf_lt.1
		rm *.o

