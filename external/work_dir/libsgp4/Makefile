#
#
#
#

SRC = Eci.cpp Julian.cpp Observer.cpp OrbitalElements.cpp \
	SGP4.cpp SolarPosition.cpp Timespan.cpp Tle.cpp Util.cpp

OBJ = $(SRC:.cpp=.o)

OUT = libsgp4.a

# include directories
INCLUDES = -I. -I../include/ -I/usr/local/include

# library paths
LIBS = -L../ -L/usr/local/lib -lm

CCC = c++

.SUFFIXES: .cpp

default: dep $(OUT)

.cpp.o:
	$(CCC) $(INCLUDES) $(CCFLAGS) -c $< -o $@

$(OUT): $(OBJ)
	ar rcs $(OUT) $(OBJ)

depend: dep

dep:
	makedepend -- $(CFLAGS) -- $(INCLUDES) $(SRC)

clean:
	rm -f $(OBJ) $(OUT)

# DO NOT DELETE

Eci.o: Eci.h CoordGeodetic.h Globals.h Util.h Vector.h Julian.h Timespan.h
Julian.o: Globals.h Julian.h Timespan.h Util.h
Observer.o: Observer.h CoordGeodetic.h Globals.h Util.h CoordTopographic.h
Observer.o: Julian.h Timespan.h Eci.h Vector.h
OrbitalElements.o: OrbitalElements.h Tle.h Globals.h Util.h Julian.h
OrbitalElements.o: Timespan.h TleException.h
SGP4.o: SGP4.h Tle.h Globals.h Util.h Julian.h Timespan.h TleException.h
SGP4.o: OrbitalElements.h Eci.h CoordGeodetic.h Vector.h SatelliteException.h
SGP4.o: DecayedException.h
SolarPosition.o: SolarPosition.h Julian.h Globals.h Timespan.h Eci.h
SolarPosition.o: CoordGeodetic.h Util.h Vector.h
Timespan.o: Timespan.h Globals.h
Tle.o: Tle.h Globals.h Util.h Julian.h Timespan.h TleException.h
Util.o: Util.h Globals.h
