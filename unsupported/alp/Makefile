# Makefile for          :  check_lzp
# Module Author         :  Tom Logan   
# Module Version        :  1.0
#
# File to make is the check_lzp executable
#
include ../../make_support/system_rules

#
# Object libraries required to load the program
#

OBJS =  lzFetch.o \
	get_target.o
 
OBJLIBS = $(LIBDIR)/asf_meta.a \
	  $(LIBDIR)/asf.a \
 	  -lm

alp:	check_lzp parse_lzp_par 
	cp alp $(BINDIR)
	cp alp.man ../../man/cat1/alp.1
	rm *.o

check_lzp:  check_lzp.o $(OBJS)
	$(CC) -o check_lzp check_lzp.o $(OBJS) $(CFLAGS) $(OBJLIBS)
	mv check_lzp$(BIN_POSTFIX) $(BINDIR)
	cp check_lzp.man ../../man/cat1/check_lzp.1


parse_lzp_par:  parse_lzp_par.o lzFetch.o
	$(CC) -o parse_lzp_par parse_lzp_par.o lzFetch.o $(CFLAGS) $(OBJLIBS) 
	mv parse_lzp_par$(BIN_POSTFIX) $(BINDIR)
	cp parse_lzp_par.man ../../man/cat1/parse_lzp.1

