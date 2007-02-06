/********************************************************************
NAME:    prop2ll -- convert the list of propagated ssv inertial
		    coordinates contained in inVecFile to a list of
		    lat/lon coordinates of the near and far edge of
		    the swath, given nominal near and far look angles
		    
SYNOPSIS:    prop2ll <rsvFile> <inVecFile> <outLATLONfile>

DESCRIPTION:

FILE FORMATS
inputs
    rsvFile:	(give without extension)
	x(km)  y(km)  z(km)  x'(m/s)  y'(m/s)  z'(m/s)
	year, julian day, gmtSec
	delta time
	
    inVecFile:	(this is the output from propagate)
	time(hrs)   x   y   z   x'  y'  z'
	...
	
outputs
    outLATLONfile
	year, julian day, total sec.
	offset(sec), nearLat, nearLon, farLat, farLon
	...
	
PROGRAM HISTORY:
    VERS:   DATE:   AUTHOR:         PURPOSE
    --------------------------------------------------------------
    1.0      7/00   M. Jessop       Transform propagated initial satellite
				    state vectors into Lat/Lon
				    coordinates.
    2.0     11/00   J. Badgley      Allow the use of any projection type.

******************************************************************/
/******************************************************************************
*                                                                             *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/
#include "asf.h"


#define PIE	3.141592653589793238
#define PIE2	6.283185307179586476
#define RPD	(PIE / 180.0)		/* Radians per Degree */
#define RE	6378144.0		/* Earth Equatorial Radius */
#define RP	6356754.9		/* Earth Polar Radius */
#define	ECC	8.1827385E-2		/* Eccentricity of Earth */

    
void execute(char *cmd)
{
	printf("%s",cmd);
	fflush(stdin);
	if (system(cmd)!=0) { printf("Program Aborted\n"); exit(1); }
}

double mag(a)
double a[];
{
    double sum;
    int i;
    sum=0.0;
    for (i=1; i<=3; i++) {sum = (a[i]*a[i]) + sum;}
    sum = sqrt(sum);
    return (sum);
}

void myVecNormalize(v)
double v[];
{
    int i;
    double vmag;
    vmag = mag(v);
    for (i=1; i<=3; i++) {v[i]/=vmag;}
}

void myVecCross(a, b, aXb)
double a[], b[], aXb[];
{
    aXb[1]=a[2]*b[3]-a[3]*b[2];
    aXb[2]=a[3]*b[1]-a[1]*b[3];
    aXb[3]=a[1]*b[2]-a[2]*b[1];
}

void fortran_sph2cart(latp,longp,r,vec)

   double latp,
          longp,
          r,
          vec[];

   {

   double rcoslt;

   rcoslt = r * cos(latp);
   vec[1] = rcoslt * cos(longp);
   vec[2] = rcoslt * sin(longp);
   vec[3] = r * sin(latp);
   return;

   } /* fortran_sph2cart */


/* calculate the full julian day ( from practical astronomy with your
   calculator by peter duffett-smith ) */

double fulljd (yr,mo,day)
   double day, yr, mo;
{
   double fjd;
   int iyr, imo, a, b, c, d;

   if (mo == 1 || mo == 2)
      {
      iyr = (int)(yr - 1);
      imo = (int)(mo + 12);
      }
   else
      {
      iyr = (int)(yr);
      imo = (int)(mo);
      }

    a = (int)((double)(iyr) / 100);
    b = 2 - a + (int)((double)(a) / 4);
    c = (int)(365.25 * iyr);
    d = (int)(30.6001 * (imo + 1));
    fjd = b + c + d + day + 1720994.5;
    return (fjd);
} /* fulljd */


/* calculate the   b   value for the calculation of the greenwich hour
   angle ( from practical astronomy with your calculator by
   peter duffett-smith ) */

double calcbee(yr)
  double yr;
{
   double fjd, s, t, r, u, bigbee, mo, day;

   mo = 1.0;
   day = 0.0;
   fjd = fulljd(yr,mo,day);
   s = fjd - 2415020.0;
   t = s / 36525.0;
   r = 6.6460656 + 2400.051262 * t + 0.00002581 * t * t;
   u = r - 24.0 * (yr - 1900);
   bigbee = 24.0 - u;
   return (bigbee);
} /* calcbee */


void ic2efc (satint,timeref)
double satint[],
       timeref[];
{
   double bigb,
          aprime, cprime,
          t0,
          gst, gmt, gha,
          intlongp, intlongv,
          latp, longp, latv, longv,
          r, v;
    
   static double a = 0.0657098;
   static double c = 1.002738;
   
   r = satint[4];
   v = satint[8];

   latp = asin(satint[3] / r);
   latv = asin(satint[7] / v);

   intlongp = atan2(satint[2] , satint[1]);
   if (intlongp < 0.0) intlongp = PIE2 + intlongp;

   intlongv = atan2(satint[6] , satint[5]);
   if (intlongv < 0.0) intlongv = PIE2 + intlongv;
  
   bigb = calcbee(timeref[1]);

   aprime = a * timeref[2];
   t0 = aprime - bigb;
   gmt = timeref[3] / 3600.0;
   cprime = gmt * c;
   gst = cprime + t0;
   if (gst >= 24.0) gst = gst - 24.0;
   if (gst < 0.0) gst = 24.0 + gst;

   gha = gst * 15.0 * RPD;

   longp = intlongp - gha;
   longv = intlongv - gha;

   fortran_sph2cart (latp,longp,r,satint);

   satint[5] = v * cos(latv) * cos(longv);
   satint[6] = v * cos(latv) * sin(longv);
   satint[7] = v * sin(latv);

   return;
} /* ic2efc */


void geopts (satsv, lookang, lat, lon)
double	satsv[],	/* eight element satellite state vector */
	lookang,	/* satellite look angle in degrees */
	*lat, 		/* geodetic latitude of incidence point */
	*lon;		/* geodetic longitude of incidence point */
{
    int	    i;		    /* counter */
    double  spv[4],	    /* satellite position vector */
	    svv[4],	    /* satellite velocity vector */
	    tpv[4],	    /* target position vector */
	    crossP[4],	    /* cross product vector */
	    yawang,	    /* spacecraft yaw angle */
	    u[4], 
	    v[4],
	    w[4],	    /* vectors used to calculate pointing vector */
	    xp, yp, zp,     /* pointing vector x, y, and z values */
	    A, B, C, 
	    beta,	    /* used to calculate target position */
	    r, geoc; 	    /* used to calculate geodetic lat/lon */	    
	    
    /*initialize values */
    yawang  =  0.0;	    /* degrees */
    
    for (i=1; i<=3; i++) {
	spv[i]=satsv[i];
	svv[i]=satsv[i+4];
    }
    
    /* Calculate u, v, w vectors to define a new coordinate system */
    myVecCross(svv, spv, crossP); myVecNormalize(crossP);
    for (i=1; i<=3; i++) {u[i] = crossP[i];}
    myVecCross(spv, u, crossP); myVecNormalize(crossP);
    for (i=1; i<=3; i++) {v[i] = crossP[i];}
    for (i=1; i<=3; i++) {crossP[i] = spv[i];}
    myVecNormalize(crossP);
    for (i=1; i<=3; i++) {w[i] = -1 * crossP[i];}    
    
    /* Convert from degrees to radians for calculations */
    lookang*=RPD;
    yawang*=RPD;
    
    /* Calculate pointing vectors--> assumes ZERO yaw angle!! */
    xp=w[1]*cos(lookang) + (u[1]*cos(yawang)-v[1]*sin(yawang)) * sin(lookang);
    yp=w[2]*cos(lookang) + (u[2]*cos(yawang)-v[2]*sin(yawang)) * sin(lookang);
    zp=w[3]*cos(lookang) + (u[3]*cos(yawang)-v[3]*sin(yawang)) * sin(lookang);
        
	/* Calculate Target Position, (tpv = spv + beta*P) */
	C = (spv[1]*spv[1]+spv[2]*spv[2])/(RE*RE) + (spv[3]*spv[3])/(RP*RP) - 1;
	A = (xp*xp+yp*yp)/(RE*RE) + (zp*zp)/(RP*RP);
	B = 2*((xp*spv[1]+yp*spv[2])/(RE*RE) + (zp*spv[3])/(RP*RP));
	
	beta = (-B - sqrt(B*B - 4*A*C))/(2*A);
	tpv[1] = spv[1] + beta * xp;
	tpv[2] = spv[2] + beta * yp;
	tpv[3] = spv[3] + beta * zp;
	
	
	/* Caculate Lat/Lon from Target Vector */
	r = mag(tpv);
	geoc = asin(tpv[3]/r);
	*lat=atan(tan(geoc)/(1-ECC*ECC)) / RPD;
	*lon=atan2(tpv[2],tpv[1]) / RPD;

    return;
}


int main(int argc, char *argv[])
{
    int     i, j, ch,
            n, sn;		/* counts number of points parsed and written to output*/
    double  ssv[9],		/* satellite state vector */
            timeref[4],		/* time reference */
            ptime[4],		/* time ref. for current point */
            hours,		/* hours past beginning */
            seconds,		/* seconds past beginning */
            tmp,		/* temp value holder */
	    last,		/* holds last value to compare to the current */
            vec[4],		/* temp vector value holder */
	    nearang, farang, 	/* near and far look angles for swath */
	    lolat, hilat,	/* minimum and maximum latitudes for Alaska */
	    lat1, lat2,	
	    lon1, lon2;		/* calculated latitudes and longitudes */
	    
	char inFile[255], tmpFile[255], cmd[255];
            
    FILE    *infp1, *infp2, *outfp, *tmpfp;
        
    
    /* Initialize vars */
    nearang = 17.0;  
    farang  = 23.5;
    hilat   = 9971.5;  /* don't want to clip anything */
    lolat   = -9956.0;
    n	    = 0;
    sn	    = 0;
    
    /* Test for proper arguments */
    if (argc != 4) { 
        printf("Usage: %s <rsvFile> <inVecFile> <outLATLONfile>\n",argv[0]); exit(1); 
    }
    
    /* Get timeref params from rsv file and close */
    infp1 = fopen(argv[1], "r");
    if ( infp1 == (FILE *) NULL) {
        printf("\n*** couldn't open data file: %s\n", argv[1]);
        exit(1);  }
    for (i=1; i<=6; i++) { fscanf(infp1, "%lf", &tmp); }
    for (i=1; i<=3; i++) { fscanf(infp1, "%lf", &timeref[i]); }
    for (i=1; i<=3; i++) { ptime[i] = timeref[i]; }
    fclose (infp1);
        
    /* Open inVecFile and tmp file */
	strcpy(inFile, argv[2]);
	strcat(strcpy(tmpFile, argv[2]), ".tmp");
    infp2 = fopen(inFile, "r");
    if ( infp2 == (FILE *) NULL) {
        printf("\n*** couldn't open data file: %s\n", inFile);
        exit(1);
    }
    tmpfp = fopen(tmpFile, "w");
    
    /* Change D's (FORTRAN format) to E's in the inVecFile for proper reading */
    ch = fgetc(infp2);
    while (ch != EOF) {
    	if (ch=='D') ch='E';
    	fputc(ch, tmpfp);    	
    	ch = fgetc(infp2);
    }
    fclose(tmpfp);
    fclose(infp2);
    sprintf(cmd,"cp %s %s\n",tmpFile,inFile); execute(cmd);
    sprintf(cmd,"rm %s\n",tmpFile); execute(cmd);
    infp2 = fopen(inFile, "r");
    
    /* Write timeref information as header of output file */
    outfp = fopen(argv[3], "w");
    for (i=1; i<=3; i++) { fprintf (outfp, "%lf ", timeref[i]); }
    
    /* Find number of lines, write to outVecFile header */
    while (!feof(infp2)) {
        fscanf (infp2, "%lf", &tmp);
	if ((last-tmp)>0.000011||n==0) {/*n++*/};  /*make sure it's progressing */
	for (i=1; i<=6; i++)  fscanf (infp2, "%lf", &tmp);
	n++;
	last = tmp;
    }
    n -= 2;
    printf("\nNumber of lines parsed: %i", n);
    
    /* Reset file position to start */
    fseek(infp2, 0L, 0);
    
    
    /* Read inVecFile, convert to efc, then find lat/lon */
    for (j=1; j<=n; j++) {
        fscanf (infp2, "%lf", &hours);
        for (i=1; i<=3; i++) { fscanf (infp2, "%lf", &tmp); ssv[i] = tmp*1000; }
        ssv[4] = mag(ssv);
        for (i=1; i<=3; i++) { 
            fscanf (infp2, "%lf", &tmp);
            vec[i] = tmp*1000;
            ssv[i+4] = tmp*1000;
        }
        ssv[8] = mag(vec);
    
	seconds = hours*3600;
        ptime[3]=timeref[3]+seconds;	
        ic2efc(ssv, ptime);
    	
	geopts(ssv, nearang, &lat1, &lon1);
	geopts(ssv, farang, &lat2, &lon2);
	if (lat1<hilat && lat1>lolat) {
	    sn += 1;
	    fprintf(outfp, "\n%lf %lf %lf %lf %lf",seconds,lat1,lon1,lat2,lon2);
	}
    }
    fprintf(outfp, "\n");
    printf ("\nNumber of points written to output lat/lon file: %i", sn);
    printf ("\n");
    fclose (infp2);
    fclose (outfp);
    
    exit(EXIT_SUCCESS);
}
