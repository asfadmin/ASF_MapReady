#ifndef PROTOS_H
#define PROTOS_H
#include "ddr.h"
#include "xtiffio.h"
#include "geotiffio.h"


void alaskakeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void alberskeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void azmeqdkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

int c_decdeg
(
double *angle,             /* Input and output of degrees            */
char *coform,              /* Type of angle being inputted           */
char *type                 /* Limits output angle is tested against  */
);

void eqrectkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void equidckeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void geokeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);


unsigned short get_angular_units
(
    char *unitstr	  /* I: Units used (in English)              */
);

unsigned short get_geocode
(
    struct DDR ddr        /* I: LAS image file descriptor record    */
);

unsigned short get_linear_units
(
    char *unitstr	 /* I: Units used (in English)             */
);

void get_sphere
(
    struct DDR ddr,      /* I: LAS image file descriptor record    */
    double *sphere       /* O: radius of sphere                    */
);

unsigned short get_stateplanezone
(
    struct DDR ddr      /* I: LAS image file descriptor record    */
);

void get_tiepoints
(
struct DDR ddr,              /* I: LAS image file descriptor record     */
double tiepoints[][6],       /* O: coordinate array for GEOTIEPOINTS    */
long *size                   /* O: number of elements in array          */
);

unsigned short get_utmzone 
(
    struct DDR ddr     /* I: LAS image file descriptor record    */
);

unsigned short get_utmzonegen
(
    struct DDR ddr     /* I: LAS image file descriptor record    */
);

/************************************************************************
void getpar
(
   struct PARBLK *vblock,     
   char  hostinfile[],       
   char  outfile[],           
   char  worldfile[],         
   unsigned long *imagelength,
   unsigned long *imagewidth, 
   long *sl,                 
   long *ss,                 
   long *nbands,              
   long *dtype,               
   long bands[],              
   struct DDR *ddr           
);
*****************************************************************************/


void gnomonkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void goodkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void gvnspkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void hammerkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void homkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void imollkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void lamazkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void lamcckeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void mercatkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void millerkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void mollkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void obeqakeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void orthokeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void polyckeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void pskeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void robinkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void set_major_minor
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void snsoidkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void somkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void spcskeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void stereokeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void tmkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void utmkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void vgrintkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void wagivkeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void wagviikeyset
(       
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
);

void  set_geogEllipseGeoKey(int datum_num, GTIF *gtif);
void  set_geogGeodeticDatumGeoKey(int datum_num, GTIF *gtif );

#endif
