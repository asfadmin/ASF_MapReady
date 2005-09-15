# Makefile for java point target fetcher

include ../Make_rules

SOURCE= ASF_Time.java \
	Target.java \
	Area.java \
	MetaReader.java \
	TargetReader.java \
	AreaReader.java \
	Metadata.java \
	Constants.java \
	Spawner.java \
	TargetIngester.java \
	ImageUtils.java

all: java_util clean

java_util:
	$(JAVAC) $(SOURCE)
	cp *.class $(JAVADIR)
	
clean:
	rm -f *.class


