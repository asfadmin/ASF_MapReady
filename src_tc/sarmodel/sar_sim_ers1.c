#include "asf.h"
/*****************************************************************************
NAME:                           SAR_SIM

PURPOSE:  Given the geodetic latitude longitude and elevation of a point 
	  calculate the line and sample of the point in the slant-range image

VERSION         DATE    AUTHOR         
-------         ----    ------        
  1.0           7/91    C. Wivell	Original SEASAT Development
  2.0           4/92    C. Wivell       Modified to ERS-1 and incorp. the model
  2.5           5/92    C. Taylor       Added header reader
  3.0           9/94    T. Logan        Altered init routine to master-slave
                                        parallel pardigm (for T3D)
  3.1           9/94    T. Logan        Modified to allow individual processor
                                        reads if parameter my_pe < 0
  3.2		4/95	T. Logan	Modified to skip lineerror, samperror
					calculations during a sim run 
  3.3		7/95	T. Logan	Added switches to control program flow
					better; Created mask images; Created
					a switch for RTC processing.  Changed
					the meaning of view flag.  
  4.0		10/95   T. Logan	Port from T3D to Solaris
  4.1 		11/98 	T. Logan	Modified to use JERS data

NOTE: ALL ARRAYS HAVE FIRST VALUE IN INDEX 1
*****************************************************************************/

#include <stdarg.h>
#include "constants.h"
#include "ers1read.h"
#include "ceos.h"

/* Variables which are common to both the init and inverse routines...
  -------------------------------------------------------------------*/
double sat[14],       /* satellite state vector at t = t 		*/
       satintc[14],   /* satellite state vector at t = 0 		*/
       rawstrt,       /* starting pixel number in raw data 		*/
       azoff[4],      /* azimuth error plane coefs. in pixels 		*/
       groff[4],      /* grange error plane coefs. in meters 		*/
       grapix,        /* ground range azimuth pixel size (m) 		*/
       grrpix,        /* ground range range pixel size (m) 		*/
       ngrapix,       /* number of ground range azimuth pixels 		*/
       ngrrpix,       /* number of ground range range pixels 		*/
       nsrapix,       /* number of slant range azimuth pixels           */
       wavelength,    /* radar wavelength				*/
       deltimeref,    /* reference time offset from start of image 	*/
       convtest,      /* convergence test 0.1 line 			*/
       slant0,        /* Slant range to near edge (m) 			*/
       magfactor,     /* mag. factor from full res. 			*/
       trka,	      /* Track angle of the sattelite			*/
       lineslope,     /* Number of lines per second slant range 	*/
       linerate,      /* Number of lines per second ground range 	*/
       smpslope,      /* Sample slope in slant range                    */
       constant_term, /* Constant term used to calculate phase          */
       rcoefs[4],     /* quad. fit of change in r versus time 		*/
       mcoefs[4],     /* quad. of change in mean anomaly versus time    */
       vcoefs[4],     /* quad. of change in velocity mag. versus time   */
       gammacoefs[4]; /* quad. of change in flight path angle vs time   */
int    i,j;
static int form;      /* Formula to use for radiometric t.c. factors    */
int    GROUND_RANGE;  /* Ground range / Slant range flag                */


/* SAR model initialization routine.  
  ---------------------------------------------------------------------*/
void init_sar_model (double azcoefs[], double grcoefs[], double user_val_pix,
                     char *hdname, int frm, ...)
{
va_list ap;
ERS1   ers1;          /* define ers1 structure */
struct VFDRECV ofdr;
double swathvel,      /* along track swath velocity		        */
       timeref[10],   /* reference time of satellite loc (scene center) */
       vecs[4],       /* temp pos. vec. at start 			*/
       vecc[4],       /* temp pos. vec. at center 			*/
       vece[4],       /* temp pos. vec. at end 				*/
       strtrc,        /* geocentric radius of sat. at start 		*/
       centrc,        /* geocentric radius of sat. at center 		*/
       endrc,         /* geocentric radius of sat. at end 		*/
       mdot1,         /* mean motion using dangle1 / dtime1 		*/
       mdot2,         /* mean motion using dangle2 / dtime2 		*/
       mdotc,         /* mean motion at center of image 		*/
       latdc,         /* geodetic latitude of center of image 		*/
       latcc,         /* geocenteric latitude of center of image 	*/
       longc,         /* longitude of center of image 			*/
       elevc,         /* elevaton of center of image (m) 		*/
       rc,            /* ec. radius to center of image 			*/
       bnorm=0,       /* baseline normal seperation                     */ 
       times,         /* time from ref. to start 			*/
       timec,         /* time from ref. to center 			*/
       timee,         /* time from ref. to end 				*/
       angle1,        /* angle between start pos vec and center pos vec */
       angle2,        /* angle between center pos vec and end pos vec 	*/
       angles,        /* ECA from center to start 			*/
       anglec,        /* ECA from center to center 			*/
       anglee,        /* ECA from center to end 			*/
       velangles,     /* flight path angle at start 			*/
       velanglec,     /* flight path angle at center 			*/
       velanglee,     /* flight path angle at end 			*/
       velmags,       /* mag. of velocity vector at start 		*/
       velmagc,       /* mag. of velocity vector at center 		*/
       velmage,       /* mag. of velocity vector at end 		*/
       velvecs[4],    /* velocity vector at start 			*/
       velvecc[4],    /* velocity vector at center 			*/
       velvece[4],    /* velocity vector at end 			*/
       satints[14],   /* satellite state vector at t = start 		*/
       satinte[14];   /* satellite state vector at t = end 		*/

  /* Set the formula type for radiometric terrain correction factors */
  form = frm;
  if (form > 0)
    {
     printf("Radiometric terrain correction formula used: ");
     switch (form)
        {
   	   case 1: printf("li\n"); break;
   	   case 2: printf("go\n"); break;
   	   case 3: printf("sq\n"); break;
   	   case 4: printf("vx\n"); break;
           default: printf("unknown\n"); exit(1); break;
        }
    }
  else if (form == -1) /* We want a simulated phase image */
    {
     va_start(ap, frm);
     bnorm = va_arg(ap,double);
     va_end(ap);
    } 

  ers1 = ers1read(hdname);
  for (i = 1 ; i <= 3 ; i++) { azoff[i] = azcoefs[i]; groff[i] = grcoefs[i]; }

  /* Get the track angle parameter */
  if (get_asf_facdr(hdname,&ofdr)<0)
    { printf("Failure opening file %s\n",hdname); exit(1); }
  trka = ofdr.trackang*RPD;

  /* values to be read in from header files */
  GROUND_RANGE = ers1.GROUND_RANGE;
  latdc      = ers1.latdc;
  longc      = ers1.longc;
  elevc      = ers1.elevc;
  grapix     = ers1.grapix;
  grrpix     = ers1.grrpix;
  ngrapix    = ers1.ngrapix;
  ngrrpix    = ers1.ngrrpix;
  deltimeref = ers1.deltimeref;
  wavelength = ers1.wavelength;
  rawstrt    = ers1.rawstrt;
  swathvel   = ers1.swathvel;
  slant0     = ers1.slant0;
  convtest   = 0.1 * user_val_pix / swathvel;

  if (GROUND_RANGE==1)
   {
     /* set ground range rate for lines */
     linerate = ngrapix / (2.0 * deltimeref);
     
     /* scale number of lines and samples to DEM spacing */
     magfactor = user_val_pix / grapix;
     ngrapix = ngrapix / magfactor;
     ngrrpix = ngrrpix / magfactor;
     printf("Using Ground Range Image Processing\n"); fflush(stdin);
   }
  else
   { /* set slant range rates for line and sample directions -
      ****--->	hardcoded to use a 5 look image  <---*********/
     nsrapix = ers1.nsrapix / 5.0; 
     magfactor = 1.0;
     printf("Using Slant Range Image Processing\n"); fflush(stdin);
     if (form==-1)
       { 
         printf("bnorm = %f; slant0 = %f; \n",bnorm,slant0);
         constant_term = (PI4*bnorm) / (wavelength*slant0);
         printf("Constant_Term for Phase is %f\n",constant_term);
       }
     smpslope = (1.0/ers1.srrpix)/sin(ofdr.incedang*RPD);
     lineslope  = ers1.prf / ers1.azlook;         	/* slant-range rate */
   }

  /* Copy state vectors and timing into local variables */
  for (i=1; i<8; i++)
   {
     satints[i] = ers1.satints[i];
     satintc[i] = ers1.satintc[i];
     satinte[i] = ers1.satinte[i];
   }
  satints[13] = ers1.satints[13];             /* mean motion (rad/s) */   
  satintc[13] = ers1.satintc[13];             /* mean motion (rad/s) */   
  satinte[13] = ers1.satinte[13];             /* mean motion (rad/s) */   
  times = ers1.times; timec = ers1.timec; timee = ers1.timee;
  strtrc = ers1.strtrc; centrc = ers1.centrc; endrc = ers1.endrc;

  /* time ref. is scene center */
  timeref[1] = ers1.timeref[1];               /* year of scene */
  timeref[2] = ers1.timeref[2];               /* julian day in year */
  timeref[3] = ers1.timeref[3];               /* total second into day */
  timeref[4] = ers1.timeref[4];               /* month */
  timeref[5] = ers1.timeref[5];               /* day of month */
  timeref[6] = ers1.timeref[6];               /* hr */
  timeref[7] = ers1.timeref[7];               /* minutes */
  timeref[8] = ers1.timeref[8];               /* seconds */

  for (i = 1 ; i <= 3 ; i++)
    {
      j = i + 4;
      vecs[i] = satints[i]; vecc[i] = satintc[i]; vece[i] = satinte[i];
      velvecs[i]=satints[j]; velvecc[i]=satintc[j]; velvece[i]=satinte[j];
    }

  angle1 = acos(  dot(vecs,vecc)/ (mag(vecs)*mag(vecc)) );
  angle2 = acos(  dot(vecc,vece)/ (mag(vecc)*mag(vece)) );
  angles = -angle1; anglec = 0.0; anglee = angle2;
  velangles = acos(dot(vecs,velvecs) / (mag(vecs) * mag(velvecs)));
  velanglec = acos(dot(vecc,velvecc) / (mag(vecc) * mag(velvecc)));
  velanglee = acos(dot(vece,velvece) / (mag(vece) * mag(velvece)));
  velmags = mag(velvecs); velmagc = mag(velvecc); velmage = mag(velvece);
  calcrcoefs(strtrc,centrc,endrc,times,timec,timee,rcoefs);
  calcrcoefs(angles,anglec,anglee,times,timec,timee,mcoefs);
  calcrcoefs(velmags,velmagc,velmage,times,timec,timee,vcoefs);
  calcrcoefs(velangles,velanglec,velanglee,times,timec,timee,gammacoefs);
  
  /*
  printf("mcoef1,mcoef2,mcoef3 %f\t%f\t%f\n",mcoefs[1],mcoefs[2],mcoefs[3]);
  printf("rcoef1,rcoef2,rcoef3 %f\t%f\t%f\n",rcoefs[1],rcoefs[2],rcoefs[3]);
  printf("vcoef1,vcoef2,vcoef3 %f\t%f\t%f\n",vcoefs[1],vcoefs[2],vcoefs[3]);
  printf("gammacoef1,gammacoef2,gammacoef3 %f\t%f\t%f\n",gammacoefs[1],
                                              gammacoefs[2],gammacoefs[3]);
  */

  mdot1 = angle1 / (timec - times); 
  mdot2 = angle2 / (timee - timec);
  mdotc = (mdot1 + mdot2) / 2.0;
  
  satintc[8] = velmagc;                      /* mag. of velocity vector (m/s) */
  
  ic2efc (satintc,timeref); /* convert inertial to earth fixed at ref. time */
  
  satintc[9] = -satintc[1] * mdotc * mdotc;  /* x dot dot (m/ss) */
  satintc[10] = -satintc[2] * mdotc * mdotc; /* y dot dot (m/ss) */
  satintc[11] = -satintc[3] * mdotc * mdotc; /* z dot dot (m/ss) */
  satintc[12] = satintc[4] * mdotc * mdotc;  /* mag. of acceleration (m/ss) */
  
  sat[4] = satintc[4]; sat[8] = satintc[8]; sat[12] = satintc[12];
  
  return;
}

/* Inverse SAR model, returning one line,sample image coordinate for each
   Lat/Long/Height coordinate entered
  ----------------------------------*/
void sar_sim(latdt, longt, elevt, latdt1, longt1, elevt1, latdt2, longt2, elevt2,
        latdt3, longt3, elevt3, latdt4, longt4, elevt4, viewflg,
        intensity, sini, mask, line, sample)

double latdt;		/* Latitude (in radians)--geodetic 		*/
double longt;		/* Longitude (in radians) 			*/
double elevt;		/* Height (in meters) above ref. ellipsoid 	*/
double latdt1;		/* Latitude (in radians)--geodetic 		*/
double longt1;		/* Longitude (in radians) 			*/
double elevt1;		/* Height (in meters) above ref. ellipsoid 	*/
double latdt2;		/* Latitude (in radians)--geodetic 		*/
double longt2;		/* Longitude (in radians) 			*/
double elevt2;		/* Height (in meters) above ref. ellipsoid 	*/
double latdt3;		/* Latitude (in radians)--geodetic 		*/
double longt3;		/* Longitude (in radians) 			*/
double elevt3;		/* Height (in meters) above ref. ellipsoid 	*/
double latdt4;		/* Latitude (in radians)--geodetic 		*/
double longt4;		/* Longitude (in radians) 			*/
double elevt4;		/* Height (in meters) above ref. ellipsoid 	*/
double *intensity;      /* output--intensity of point 			*/
double *sini;           /* output--sine of local incedence angle 	*/
double *mask;           /* output--mask 0-normal,100-shadow,200-layover */
double *sample;		/* Output--sample coord from sat's perspective  */
double *line;		/* Output--line coord from sat's perspective 	*/
int viewflg;           /* calc. viewing geometry, also simulation flag */
			/*	0  --   sargeom                         */
			/*	1  --   sarsim, no mask, no RTC 	*/
			/*	2  --   sarsim, mask, no RTC 		*/
			/*	3  --   sarsim, mask, RTC		*/
{
static double tdtime = 0.0;	/* Center of image 1st time thru 	*/
int   iter;           		/* iterations used in the solution 	*/
double tar[14],       		/* target state vector at t = t 	*/
       tar1[14],       		/* target 1 state vector 		*/
       tar2[14],       		/* target 2 state vector 		*/
       tar3[14],       		/* target 3 state vector 		*/
       tar4[14],       		/* target 4 state vector 		*/
       tartemp1[14],     	/* target 1 temp state vector 		*/
       tartemp2[14],       	/* target 2 temp state vector 		*/
       tartemp3[14],       	/* target 3 temp state vector 		*/
       tartemp4[14],       	/* target 4 temp state vector 		*/
       tarint[14],    		/* target state vector at t = 0 	*/
       normal[5],               /* normal to point of interest 		*/
       dtime0,        		/* total time past reference time 	*/
       dtime1,        		/* delta time output from solution 	*/
       grline,        		/* ground range line number 		*/
       grsample,      		/* ground range sample number 		*/
       tffl,          		/* time from first line 		*/
       slline,        		/* slant range image line number 	*/
       slsample,      		/* slant range image sample number 	*/
       imgline,                 /* line calc. in raw image 		*/
       imgsample,               /* sample calc. in raw image 		*/
       lineerror,     		/* line error from azimuth meter error 	*/
       samperror,     		/* sample error from range meter error 	*/
       clos,          		/* calculated line of sight distance 	*/
       rdsample,      		/* raw data sample number 		*/ 
       cfd,           		/* calculated fd from coefs 		*/
       cfd2,          		/* calculated fd2 from state vec. 	*/
       cfd2dot,       		/* calculated fd2 dot from state vec. 	*/
       rt,            		/* radius to target input point 	*/
       rt1,            		/* radius to target input point 1 	*/
       psi0,                    /* ECA from nadir to near 		*/
       psi=0,                   /* ECA from nadir to target 		*/
       dpsi,                    /* ECA from near to target 		*/
       theta,                   /* look angle                           */
       talpha, salpha, deltaR,  /* Variables used to calculate phase    */
       rc,
       linced,                  /* local incedence angle 		*/
       ginced;			/* global incedence angle		*/
int    temp;

/* Inverse SAR model code...
  -------------------------*/
rt = packtar(&latdt,&longt,&elevt,tarint,tar);
rc = rt - elevt;
if (viewflg > 0)
 {
   rt1 = packtar(&latdt1,&longt1,&elevt1,tartemp1,tar1);
   rt1 = packtar(&latdt2,&longt2,&elevt2,tartemp2,tar2);
   rt1 = packtar(&latdt3,&longt3,&elevt3,tartemp3,tar3);
   rt1 = packtar(&latdt4,&longt4,&elevt4,tartemp4,tar4);
 }

dtime0 = tdtime;	/* dtime0 = 0.0;  */
tdtime = 0.0;   
cfd = 0.0;		/* Assumes zero doppler image as input */
for (iter = 1 ; iter <= 20 ; iter++ ) 
 {
   tdtime = tdtime + dtime0;
   movesat (satintc,rcoefs,vcoefs,gammacoefs,mcoefs,tdtime,sat);
   movetar (tarint,tdtime,tar);
   clos = los (sat,tar);
   cfd2 = calcfd2 (sat,tar,clos,wavelength,tdtime);
   cfd2dot = fd2dot (sat,tar,clos,wavelength,tdtime);
   dtime1 = (cfd - cfd2) / cfd2dot;
   if (fabs(dtime1) <= convtest) break;
   dtime0 = dtime1;
 }
if (iter >= 20) printf("warning iterations were 20 \n");

tffl = deltimeref + tdtime;
if (GROUND_RANGE)
 {
  grline = ngrapix - (tffl * linerate) / magfactor;
  psi0 = acos((rc*rc + sat[4]*sat[4] - slant0*slant0) / (2.0 * rc * sat[4]));
  psi = acos((rc*rc + sat[4]*sat[4] - clos*clos) / (2.0 * rc * sat[4]));
  dpsi = psi - psi0;
  grsample = (dpsi * rc / grrpix) / magfactor;
  *line   = (imgline   = grline);
  *sample = (imgsample = grsample);
 }
else
 {
  rdsample = (clos-slant0) * smpslope + rawstrt;
  slline = nsrapix - (tffl * lineslope) / 5.0;
  slsample = rdsample;
  *line   = (imgline   = slline);
  *sample = (imgsample = slsample);
 }

*intensity = 0.0;
*sini = 0.0;
*mask = 0.0;

if (viewflg == 0)
  {
    lineerror = azoff[1] * imgline + azoff[2] * imgsample + azoff[3];
    samperror = groff[1] * imgline + groff[2] * imgsample + groff[3];
    *line += lineerror;
    *sample += samperror;
  }
else  /* if (viewflg => 1) */
  { 
    movetar (tartemp1,tdtime,tar1);
    movetar (tartemp2,tdtime,tar2);
    movetar (tartemp3,tdtime,tar3);
    movetar (tartemp4,tdtime,tar4);
    calc4norm (tar,tar1,tar2,tar3,tar4,normal);
    cintensity (sat,tar,normal,intensity,sini);
    if (*line>ngrapix||*line<1||*sample>ngrrpix||*sample<1) *sini = 0.0;

    /* If we are processing in the slant range and the form is set to -1,
       we need to calculate the phase and return it in the sini parameter
     -------------------------------------------------------------------*/ 
    if (!GROUND_RANGE && form == -1)
      {
        psi = acos((rc*rc + sat[4]*sat[4] - clos*clos) / (2.0 * rc * sat[4]));
        theta = acos((sat[4]*sat[4]+clos*clos-rt*rt)/(2.0*clos*sat[4]));
        ginced = theta + psi;
        salpha = sin(ginced);
	talpha = tan(ginced);
	deltaR = (double)(clos-slant0);
	/* *sini = constant_term * ((elevt/salpha)+(deltaR/talpha)); */
	*sini = constant_term * (deltaR/talpha);


      }
    /* If requested, calculate the mask value 
     ---------------------------------------*/ 
    if (viewflg==2||viewflg==4)
      {
	calcmask(sat,tar,normal,mask);
    	temp = (int)(*mask);
    	if (temp == 200) *sini = 0.0;
    	if (temp == 100) *sini = 1.0;
      }

    /* If the pixel is not masked or shadowed, calculate the radiometric
       terrain correction factor from local and global incedence angles
     ------------------------------------------------------------------*/
    if (viewflg==3||viewflg==4)
     if (*sini != 0.0 && *sini != 1.0)
      {
        theta = acos((sat[4]*sat[4]+clos*clos-rt*rt)/(2.0*clos*sat[4]));
        ginced = theta + psi;
        linced = asin(*sini);
        *sini = compang(tdtime,trka,normal,longt,latdt,ginced,linced,form);
      }
  } 
/*
printf("sar_sim: imgline,imgsample=%f\t%f\n",imgline,imgsample);
printf("sar_sim: lineerror,samperror=%f\t%f\n",lineerror,samperror);
printf("sar_sim: line,sample=%f\t%f\n",*line,*sample);
printf("sar_sim: intensity,sini,mask=%f\t%f\t%f\n",*intensity, *sini,*mask); 
*/
return;
}
