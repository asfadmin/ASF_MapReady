# Makefile for ASF Converter
include  ../mk.common

all: java_gui

java_gui: 
	$(JAVAC) pvs.java
	cp *.class $(JAVA)
	cp pvs.pref $(JAVA)

clean:
	rm -f *.class $(JAVA)/*class $(JAVA)/pvs.pref
