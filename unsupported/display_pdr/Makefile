# Makefile for          :  display_pdr 
# Module Author         :  Tom Logan   
# Module Version        :  1.0

include ../../make_support/system_rules

#
# Object libraries required to load the program
#
OBJLIB=$(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a

display_pdr: display_pdr.o $(OBJLIB)
	$(CC) $(CFLAGS) -o display_pdr display_pdr.o $(OBJLIB)
	rm *.o
	mv display_pdr$(BIN_POSTFIX) $(BINDIR)
	cp display_pdr.man ../../man/cat1/display_pdr.1

