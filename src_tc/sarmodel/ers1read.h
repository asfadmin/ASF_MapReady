/********************************************************************
NAME:               ERS1READ.H

PURPOSE:  Include file defining structure for ERS1 file header

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           5/92   C. Taylor
*********************************************************************/
typedef struct
   {
   int    GROUND_RANGE;
   double latdc,        /* Scene center geodetic latitude (rad) */
          longc,        /* Scene center geodetic longitude (rad) */
          elevc,        /* Average terrain height above ellipsoid (m) */
          slength,      /* Scene length (m) */
          azlook,       /* Number of looks in azimuth */
          grapix,       /* Ground range azimuth pixel size (m) */
          grrpix,       /* Ground range range pixel size (m) */
          timeref[10],  /* Reference time for satellite location
                           [1]:  Year 
                           [2]:  Julian day in year
                           [3]:  Total second into day
                           [4]:  Month
                           [5]:  Day of month
                           [6]:  Hour
                           [7]:  Minute
                           [8]:  Second */
          deltimeref,   /* time difference */
          satints[14],  /* Satellite starting position
                           [1]:  x inertial (m)
                           [2]:  y inertial (m)
                           [3]:  z inertial (m)
                           [4]:  mag. of position (m)
                           [5]:  x dot inertial (m/s)
                           [6]:  y dot inertial (m/s)
                           [7]:  z dot inertial (m/s)
                           [8]:  mag. of velocity (m/s)
                           [9]:  x dot dot inertial (m/s*s)
                           [10]: y dot dot inertial (m/s*s)
                           [11]: z dot dot inertial (m/s*s)
                           [12]: mag. of acceleration */
          satintc[14],  /* Satellite center position
                           [1]:  x inertial (m)
                           [2]:  y inertial (m)
                           [3]:  z inertial (m)
                           [4]:  mag. of position (m)
                           [5]:  x dot inertial (m/s)
                           [6]:  y dot inertial (m/s)
                           [7]:  z dot inertial (m/s)
                           [8]:  mag. of velocity (m/s)
                           [9]:  x dot dot inertial (m/s*s)
                           [10]: y dot dot inertial (m/s*s)
                           [11]: z dot dot inertial (m/s*s)
                           [12]: mag. of acceleration */
          satinte[14],  /* Satellite ending position
                           [1]:  x inertial (m)
                           [2]:  y inertial (m)
                           [3]:  z inertial (m)
                           [4]:  mag. of position (m)
                           [5]:  x dot inertial (m/s)
                           [6]:  y dot inertial (m/s)
                           [7]:  z dot inertial (m/s)
                           [8]:  mag. of velocity (m/s)
                           [9]:  x dot dot inertial (m/s*s)
                           [10]: y dot dot inertial (m/s*s)
                           [11]: z dot dot inertial (m/s*s)
                           [12]: mag. of acceleration */
          swathvel,     /* Velocity of swath at scene center */
          prf,          /* Pulse rate frequency */
          slant0,       /* Slant range to near edge (m) */
          srapix,       /* Slant range azimuth pixel size (m) */
          srrpix,       /* Slant range range pixel size (m) */
          times,        /* Time from reference to start */
          timec,        /* Time from reference to center */
          timee,        /* Time from reference to end */
          strtrc,       /* Geocentric radius of satellite at start */
          centrc,       /* Geocentric radius of satellite at center */
          endrc,        /* Geocentric radius of satellite at end */
          ngrapix,      /* Number of ground range azimuth pixels */
          ngrrpix,      /* Number of ground range range pixels */
          nsrapix,      /* Number of slant range azimuth pixels */
          rawstrt,      /* Starting pixel number in raw data */
	  wavelength;   /* Radar wavelength                   */
   } ERS1;    
ERS1  ers1read(char  *hdname);

