# Makefile for: asf_odl.a
# Makefile author: Orion Lawlor
# Makefile date: 5/21/98
#

include ../../make_support/system_rules

CFLAGS := -Iinclude $(CFLAGS)

OBJS = \
	odl_api.o \
	Obj.o \
	Msg.o \
	Relay.o \
	Object.o \
	Group.o \
	Label.o \
	Array.o \
	Ident.o \
	String.o \
	Symbol.o \
	Set.o \
	Seq.o \
	Seq2.o \
	Range.o \
	Int.o \
	UnsignedInt.o \
	Double.o \
	Time.o \
	Unit.o \
	y.tab.o \
	Lexer.o

asf_odl.a: $(OBJS)
	ar rcv asf_odl.a $(OBJS)
	$(RANLIB) asf_odl.a
	rm $(OBJS)
	mv asf_odl.a $(LIBDIR)
	echo "ASF ODL Library sucessfully built!"

clean:
	rm *.o asf_odl.a
