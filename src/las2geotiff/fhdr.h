#define HEADER_SIZE 1536       /* size of the fast format header            */
#define NUMFIELDS 117          /* number of fields in Fast-B/C header       */
#define NUM_ADMIN_FIELDS 133   /* number of fields in Fast-L7 admin header  */
#define NUM_RAD_FIELDS 65      /* number of fields in Fast-L7 radiom header */
#define NUM_GEOM_FIELDS 132    /* number of fields in Fast-L7 geom header   */

/* GEOM will be added to the documented field numbers to get to the geometric
 * fields in this table.  At this point the radiometric fields are not used.
 * The administrative fields can be used as defined in the Fast-L7A docs, since
 * they are at the beginning of the header.  Space Imaging Fast-C will be
 * read from the Fast-L7A tables.
 */
#define GEOM NUM_ADMIN_FIELDS+NUM_RAD_FIELDS

/* The following defines are for each field in the header field lookup table.
   This table defines the position, data type, and formatting information for
   each field in the fast format header specification                        */

#define FSBYTE      0               /* starting byte position field          */
#define FEBYTE      1               /* ending byte position field            */
#define FDTYPE      2               /* data type field                       */
#define FLEN1       3               /* formatting value width field          */
#define FLEN2       4               /* formatting value precision field      */


/* Defines for the visisble, thermal, and pan bands */
#define VISIBLE 30
#define THERMAL 60
#define PAN     15


#define LASPROJKEYLEN    5
#define PROJCOUNT       21

struct PROJKN {     /* Projection keywords and name */
    long    projnum;                /* projection number */
    char    projname[LASPROJKEYLEN];    /* descriptive name */
    };

/*  Projection key definitions  */
#define SETPROJKN(tablename) \
  struct PROJKN tablename[PROJCOUNT] = { \
    {1,     "UTM"       },\
    {2,     "SPCS"      },\
    {3,     "ACEA"      },\
    {4,     "LCC"       },\
    {5,     "MER"       },\
    {6,     "PS"        },\
    {7,     "PC"        },\
    {8,     "EC"        },\
    {9,     "TM"        },\
    {10,    "SG"        },\
    {11,    "LAEA"      },\
    {12,    "AE"        },\
    {13,    "GNO"       },\
    {14,    "OG"        },\
    {15,    "GVNP"      },\
    {16,    "SIN"       },\
    {17,    "ER"        },\
    {18,    "MC"        },\
    {19,    "VDG"       },\
    {20,    "OM"        },\
    {22,    "SOM"       }   }


#define SPHERECOUNT     20
#define SPHERENAMELEN   40

struct SPHEREDEF {  /* Spheroid keywords, names, and semi-axes */
    long    spherenum;          /* sphere number */
    char    spherename[SPHERENAMELEN];/* descriptive name */
    double  smjaxis;            /* Semi-major axis */
    double  smnaxis;            /* semi-minor axis */
    };

/*  Create table of spheroid keys, names, and semi-axes  */
#define SETSPHEREDEF(tablename) \
    struct SPHEREDEF tablename[SPHERECOUNT] = { \
    {0,     "CLARKE 1866",          6378206.4,      6356583.8}, \
    {1,     "CLARKE 1880",          6378249.145,    6356514.86955}, \
    {2,     "BESSEL",               6377397.155,    6356078.9628}, \
    {3,     "INTERNATIONAL 1967",   6378157.5,      6356772.2}, \
    {4,     "INTERNATIONAL 1909",   6378388.0,      6356911.94613}, \
    {5,     "WGS 72",               6378135.0,      6356750.519915}, \
    {6,     "EVEREST",              6377276.3452,   6356075.4133}, \
    {7,     "WGS 66",               6378145.0,      6356759.769356}, \
    {8,     "GRS 1980",             6378137.0,      6356752.31414}, \
    {9,     "AIRY",                 6377563.396,    6356256.91}, \
    {10,    "MODIFIED EVEREST",     6377304.063,    6356103.039}, \
    {11,    "MODIFIED AIRY",        6377341.89,     6356036.143}, \
    {12,    "WGS 1984",             6378137.0,      6356752.3142}, \
    {13,    "SOUTHEAST ASIA",       6378155.0,      6356773.3205}, \
    {14,    "AUSTRALIAN NATIONAL",  6378160.0,      6356774.719}, \
    {15,    "KRASOVSKY",            6378245.0,      6356863.0188}, \
    {16,    "HOUGH",                6378270.0,      6356794.343479}, \
    {17,    "MERCURY 1960",         6378166.0,      6356784.283666}, \
    {18,    "MODIFIED MERCURY 1968",6378150.0,      6356768.337303}, \
    {19,    "6370997 SPHERE",       6370997.0,      6370997.0} }


#define MONTHCOUNT      12
#define MONTHNAMELEN    4

struct MONTHDEF {  /* Month names, numbers */
    char    monthnum[3];
    char    monthname[MONTHNAMELEN];
    };

/* Month name-number definitions */
#define SETMONTHDEF(tablename) \
  struct MONTHDEF tablename[MONTHCOUNT] = { \
    {"01",  "JAN"   },\
    {"02",  "FEB"   },\
    {"03",  "MAR"   },\
    {"04",  "APR"   },\
    {"05",  "MAY"   },\
    {"06",  "JUN"   },\
    {"07",  "JUL"   },\
    {"08",  "AUG"   },\
    {"09",  "SEP"   },\
    {"10",  "OCT"   },\
    {"11",  "NOV"   },\
    {"12",  "DEC"   } }

