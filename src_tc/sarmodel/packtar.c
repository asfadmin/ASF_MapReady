#include "asf.h"

#include "constants.h"

double packtar(latdt,longt,elevt,tarint,tar)
double 	*latdt,
	*longt,
	*elevt,
	tarint[],
	tar[];
{
static int      first = 1;
static double	nsmsm[8],      /* ellipsoid parameters for state vector */
      		osmsm[8],      /* ellipsoid parameters for DEM */
       		offcent[8],    /* offset and rotation to go to new ellipsoid */
       		htaboveel;     /* height geoid is above ellipsoid */
double  	ollh[4],       /* DEM geodetic latitude, longitude and height */
       		nllh[4],       /* converted DEM geodetic lat,lon and height */
		latct,
       		rt;

  if (first) {
      htaboveel = 10.0 ;
      nsmsm[1] = 6378144.0;        /* GEM6 */
      nsmsm[2] = 6356755.0;
      nsmsm[3] = (nsmsm[1] - nsmsm[2]) / nsmsm[1];
      nsmsm[4] = 2.0 * nsmsm[3] - nsmsm[3] * nsmsm[3];
      nsmsm[5] = 1.0 - nsmsm[4];
      nsmsm[6] = 0.0;
      osmsm[1] = 6378137.0;        /* WGS84 */
      osmsm[2] = 6356752.314;
      osmsm[3] = (osmsm[1] - osmsm[2]) / osmsm[1];
      osmsm[4] = 2.0 * osmsm[3] - osmsm[3] * osmsm[3];
      osmsm[5] = 1.0 - osmsm[4];
      osmsm[6] = 0.0;
      offcent[1] = 0.0; offcent[2] = 0.0; offcent[3] = 0.0;
      offcent[5] = 0.0; offcent[6] = 0.0; offcent[7] = 0.0;
      first = 0;
    }

  ollh[1] = *latdt;
  ollh[2] = *longt;
  ollh[3] = *elevt + htaboveel;

  changellips(ollh,osmsm,offcent,nsmsm,nllh);

  *latdt = nllh[1];
  *longt = nllh[2];
  *elevt = nllh[3];

  det2centh (nsmsm,*latdt,*elevt,&latct,&rt);

  if (*longt < 0.0) *longt = PIE2 + *longt;

  inttar (latct,*longt,rt,tarint);

  tar[3] = tarint[3];
  tar[4] = tarint[4];
  tar[8] = tarint[8];
  tar[7] = tarint[7];
  tar[11] = tarint[11];
  tar[12] = tarint[12];
  tar[13] = tarint[13];

  return(rt);
}

