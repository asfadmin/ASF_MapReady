# Makefile for sgpsdp library

include ../../make_support/system_rules

CFLAGS := -Wall $(CFLAGS)
TARGET = sgpsdp
LIBNAME = libasf_$(TARGET).a
OBJS = math.o obs.o in.o time.o solar.o $(TARGET).o
LIBS  = $(LIBDIR)/asf.a -lm
CFLAGS += -D_XOPEN_SOURCE=600

all: lib clean

lib: build_only
	mv $(LIBNAME) $(LIBDIR)
	cp $(TARGET).h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f $(LIBNAME)
	ar r $(LIBNAME) $(OBJS)
	$(RANLIB) $(LIBNAME)

exe: $(TARGET)_main.o
	$(CC) $(CFLAGS) -o $(TARGET) $(TARGET)_main.c $(LIBDIR)/$(LIBNAME) $(LIBS) $(LDFLAGS)
	mv $(TARGET)$(BIN_POSTFIX) $(BINDIR)

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)

clean:
	rm -rf $(OBJS) $(TARGET).a *~
