# Makefile for          :  sarin 
# Module Author         :  Tom Logan & Mike Shindle 

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm
	
OBJS = sarin.o

sarin: $(OBJS)
	$(CC) $(CFLAGS) -o sarin $(OBJS) $(LIBS) $(LDFLAGS)
	rm *.o
	mv sarin$(BIN_POSTFIX) $(BINDIR)
	cp sarin.1 ../../man/man1/sarin.1


