#	las_op
#	by Orion Lawlor, ASF, 1997
#	A program to compute any mathematical function of a set of las images.

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm
	
OBJS  = tokenizer.o \
	expression.o \
	las_op.o

las_op:	$(OBJS)
	${CC} ${CFLAGS} ${LDFLAGS} -o las_op $(OBJS) ${LIBS}
	mv las_op$(BIN_POSTFIX) $(BINDIR)
	cp las_op.1 ../../man/cat1/las_op.1
	rm *.o

clean:
	rm -f core ${OBJS} *.o
