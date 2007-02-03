/*
  NAME: spheroids.h

  Header file containing information (semimajor axis and flattening) 
  of the most common spheroids.
*/

/*
Explanation for why WGS-84 is utilized for defining ITRF-xx semi-major and
inverse flattening values:

From the explanation given in the "EUROPEAN ORGANISATION FOR THE SAFETY OF
AIR NAVIGATION", Appendix B, Section B.5 (the most succinct explanation of
major/minor values v. ITRF-xx):

"International Terrestrial Reference Frame (ITRF):
An accurate geodetic reference frame that consists of a globally distributed
network of survey stations whose positions and velocities are determined by
several independent measurement technologies (GPS, satellite laser ranging, very
long baseline interferometry and the French DORIS system). Positions and
velocities are published periodically by the IERS, and each published set is
identified by the epoch of the station positions. Thus, the published position of a
point in ITRF97 is valid at the epoch 1st January 1997, whereas the position of the
point at some future time must take into account the effect of the points velocity.
The ITRF uses the same system parameters as WGS-84, and is the most
accurate realisation of WGS-84 that is available."

Obviously far more information on the ITRS and ITRFs exists, but they all state
that the base spheroid used for ITRS is the GRS-1980 spheroid / WGS-84 datum.  I
just think that the paragraph above captures the gist of things in the briefest
manner and is therefore suitable as a comment here.
*/

#define BESSEL_SEMIMAJOR 6377397.155
#define BESSEL_INV_FLATTENING 299.1528128
#define CLARKE1866_SEMIMAJOR 6378206.4 
#define CLARKE1866_INV_FLATTENING 294.9786982
#define CLARKE1880_SEMIMAJOR 6378249.138 
#define CLARKE1880_INV_FLATTENING 293.4663077
#define GEM6_SEMIMAJOR 6378144
#define GEM6_INV_FLATTENING 298.257
#define GEM10C_SEMIMAJOR 6378137 
#define GEM10C_INV_FLATTENING 298.2572221
#define GRS1980_SEMIMAJOR 6378137 
#define GRS1980_INV_FLATTENING 298.2572221
#define INTERNATIONAL1924_SEMIMAJOR 6378388 
#define INTERNATIONAL1924_INV_FLATTENING 297
#define INTERNATIONAL1967_SEMIMAJOR 6378160 
#define INTERNATIONAL1967_INV_FLATTENING 298.25
#define INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_SEMIMAJOR WGS84_SEMIMAJOR
#define INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_INV_FLATTENING WGS84_INV_FLATTENING
#define WGS66_SEMIMAJOR 6378145
#define WGS66_INV_FLATTENING 298.25
#define WGS72_SEMIMAJOR 6378135 
#define WGS72_INV_FLATTENING 298.26
#define WGS84_SEMIMAJOR 6378137 
#define WGS84_INV_FLATTENING 298.2572236

