#	Diff_las
#	by Orion Lawlor, ASF, 1997
#	A program to compute the diffence between two LAS images.

include ../../make_support/system_rules

LIBS    = $(LIBDIR)/asf_las.a $(LIBDIR)/asf.a -lm
OBJS=

diff_las:	diff_las.c $(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o diff_las diff_las.c $(OBJS) ${LIBS}
	mv diff_las$(BIN_POSTFIX) $(BINDIR)
	cp diff_las.man ../../man/cat1/diff_las.1

clean:
	rm -f core ${OBJS} *.o
