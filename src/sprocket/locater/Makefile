# Makefile for Locater.class

include ../Make_rules

all: locater clean

locater:
	cp geotransform.jar $(JAVADIR)
	$(JAVAC) Locater.java
	cp *.class $(JAVADIR) 

clean:
	rm -f *.class
