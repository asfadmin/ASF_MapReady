# Makefile for 	:  dump_multi_volume 
# Module Author 	:  Tom Logan   
# Module Version	:  1.1

include ../../make_support/system_rules

BIND = $(BINDIR)

dump_multi_volume: get_names check_name_of
	-rm *.o
	cp dump_multi_volume $(BIND)/dump_multi_volume
	-chmod 775 $(BIND)/dump_multi_volume
	cp dump_multi_volume.valid_dev $(BIND)
	cp dump_multi_volume.man ../../man/cat1/dump_multi_volume.1

get_names:	get_names.c
	$(CC) $(CFLAGS) -o get_names get_names.c $(LIBDIR)/asf.a
	mv get_names$(BIN_POSTFIX) $(BIND)
	cp get_names.man ../../man/cat1/get_names.1

check_name_of:	check_name_of.c 
	$(CC) $(CFLAGS) -o check_name_of check_name_of.c \
		$(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a
	mv check_name_of$(BIN_POSTFIX) $(BIND)
	cp check_name_of.man ../../man/cat1/check_name_of.1
