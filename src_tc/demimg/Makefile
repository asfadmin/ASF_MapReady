# Makefile for          :  demimg
# Module Author         :  Mike Shindle/Tom Logan 
# Module Version        :  1.0
 
include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/sarmodel.a \
	$(LIBDIR)/tcobj.a \
	$(LIBDIR)/asf_meta.a \
	-lm

#
# Object libraries required to load the program
#
OBJLIB = demimg.o \
	 init_meta.o
 
demimg : $(OBJLIB) 
	$(CC) $(CFLAGS) $(LFLAGS) -o demimg $(OBJLIB) $(LIBS)
	rm *.o
	mv demimg$(BIN_POSTFIX) $(BINDIR)
	cp demimg.man ../../man/cat1/demimg.1
