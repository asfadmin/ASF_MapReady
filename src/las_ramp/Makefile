#	las_ramp
#	by Orion Lawlor, ASF, 1997
#	A program to draw a color ramp across a LAS image.

include ../../make_support/system_rules

LIBS    =  $(LIBDIR)/asf_las.a $(LIBDIR)/asf.a -lm
OBJS= main.o

las_ramp:	$(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o las_ramp $(OBJS) ${LIBS}
	mv las_ramp$(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp las_ramp.1 ../../man/cat1

clean:
	rm -f core ${OBJS} *.o
