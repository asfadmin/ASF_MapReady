# Makefile for ASF Converter
include ../mk.common
TARGET1=vexcel_complex_metadata
TARGET2=vexcel_complex_data_planes

all: $(BIN)/$(TARGET1) $(BIN)/$(TARGET2)

$(BIN)/$(TARGET1): $(TARGET1).o ceos_rd.c
	$(LINK) $(BIN)/$(TARGET1) $(TARGET1).o ceos_rd.c $(LINK_LIB)

$(BIN)/$(TARGET2): $(TARGET2).o ceos_rd.c
	$(LINK) $(BIN)/$(TARGET2) $(TARGET2).o ceos_rd.c $(LINK_LIB)

$(TARGET1).o: $(TARGET1).c
	$(COMP) $(TARGET1).c
#	$(BEAT) $(TARGET1).c

$(TARGET2).o: $(TARGET2).c
	$(COMP) $(TARGET2).c
#	$(BEAT) $(TARGET2).c

clean:
	rm -f *.o $(BIN)/$(TARGET1) $(BIN)/$(TARGET2)
