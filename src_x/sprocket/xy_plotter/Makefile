# Makefile for ASF Converter
include  ../mk.common
TARGET=xy_plotter

all: $(BIN)/$(TARGET)

$(BIN)/$(TARGET): $(TARGET).o
	$(LINK) $(BIN)/$(TARGET) $(TARGET).o $(LINK_LIB)

$(TARGET).o: $(TARGET).c
	$(COMP) -g $(TARGET).c
#	$(BEAT) $(TARGET).c

clean:
	rm -f *.o $(BIN)/$(TARGET)
