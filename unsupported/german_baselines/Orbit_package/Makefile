all:
	cd Delft/getorb; make -f Makefile_HP; make -f Makefile_HP install
	cc -Aa PRC2polynom.c  -o PRC2polynom  -lm
	cc -Aa YYYYMMDD2Jul.c -o YYYYMMDD2Jul -lm
