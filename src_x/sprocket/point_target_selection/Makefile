# Makefile for java point target fetcher

include ../Make_rules

SOURCE= MainTargetSelection.java \
	SelectArea.java \
	SelectTargets.java

all: point_target_selection clean

point_target_selection:
	$(JAVAC) $(SOURCE)
	cp *.class $(JAVADIR)
	
clean:
	rm -f *.class


