#	register_slc
#	By Patrick Denny, ASF, 2001

include ../../make_support/system_rules

LIBS =	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a

register_slc:	register_slc.c
	${CC} ${CFLAGS} ${LDFLAGS} -o register_slc register_slc.c ${LIBS}
	mv register_slc $(BIN_POSTFIX) $(BINDIR)
	cp register_cpx $(BIN_POSTFIX) $(BINDIR)
	cp register_cpx_window $(BIN_POSTFIX) $(BINDIR)
	-chmod 775 $(BINDIR)/register_cpx $(BINDIR)/register_cpx_window
	cp register_slc.1 ../../man/cat1/register_slc.1
	cp register_cpx.1 ../../man/cat1
	cp register_cpx_window.1 ../../man/cat1

clean:
	rm -f core

