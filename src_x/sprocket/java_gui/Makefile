# Makefile for java_gui


include  ../Make_rules

all: java_gui clean

java_gui: 
	$(JAVAC) pvs.java
	cp *.class $(JAVADIR)
	cp pvs.pref $(JAVADIR)

clean:
	rm -f *.class
