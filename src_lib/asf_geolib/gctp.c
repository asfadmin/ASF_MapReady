/*******************************************************************************
NAME                           GCTP 

PROGRAMMER              DATE
----------              ----
T. Mittan		2-26-93		Conversion from FORTRAN to C

ALGORITHM REFERENCES

1.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
    Survey Professional Paper 1395 (Supersedes USGS Bulletin 1532), United
    State Government Printing Office, Washington D.C., 1987.

2.  Snyder, John P. and Voxland, Philip M., "An Album of Map Projections",
    U.S. Geological Survey Professional Paper 1453 , United State Government
    Printing Office, Washington D.C., 1989.
*******************************************************************************/
#include "cproj.h"
#include "proj.h"

#define TRUE 1
#define FALSE 0

static int iter = 0;			/* First time flag		*/
static int inpj[MAXPROJ + 1];		/* input projection array	*/
static int indat[MAXPROJ + 1];		/* input dataum array		*/
static int inzn[MAXPROJ + 1];		/* input zone array		*/
static double pdin[MAXPROJ + 1][15]; 	/* input projection parm array	*/
static int outpj[MAXPROJ + 1];		/* output projection array	*/
static int outdat[MAXPROJ + 1];	/* output dataum array		*/
static int outzn[MAXPROJ + 1];		/* output zone array		*/
static double pdout[MAXPROJ + 1][15]; 	/* output projection parm array	*/
static forward_transform for_trans[MAXPROJ + 1];/* forward function pointer array*/
static inverse_transform inv_trans[MAXPROJ + 1];/* inverse function pointer array*/

static int NADUT[134] = {2, 2, 2, 2, 5, 5, 5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                          2, 2, 2, 2, 2, 2, 2, 2};                            


static int NAD83[134] = {101,102,5010,5300,201,202,203,301,302,401,402,403,
                404,405,406,0000,501,502,503,600,700,901,902,903,1001,1002,
                5101,5102,5103,5104,5105,1101,1102,1103,1201,1202,1301,1302,
                1401,1402,1501,1502,1601,1602,1701,1702,1703,1801,1802,1900,
                2001,2002,2101,2102,2103,2111,2112,2113,2201,2202,2203,2301,
                2302,2401,2402,2403,2500,0000,0000,2600,0000,2701,2702,2703,
                2800,2900,3001,3002,3003,3101,3102,3103,3104,3200,3301,3302,
                3401,3402,3501,3502,3601,3602,3701,3702,3800,3900,0000,4001,
                4002,4100,4201,4202,4203,4204,4205,4301,4302,4303,4400,4501,
                4502,4601,4602,4701,4702,4801,4802,4803,4901,4902,4903,4904,
                5001,5002,5003,5004,5005,5006,5007,5008,5009,5200,0000,5400};

void gctp(incoor,insys,inzone,inparm,inunit,indatum,ipr,efile,jpr,pfile,outcoor,
     outsys,outzone,outparm,outunit,fn27,fn83,iflg)
/*
gtpz0(incoor,insys,inzone,inparm,inunit,indatum,ipr,jpr,lemsg,lparm,outcoor,
     outsys,outzone,outparm,outunit,ln27,ln83,fn27,fn83,length,iflg)
*/

double *incoor;		/* input coordinates				*/
int *insys;		/* input projection code			*/
int *inzone;		/* input zone number				*/
double *inparm;		/* input projection parameter array		*/
int *inunit;		/* input units					*/
int *indatum;		/* input datum 					*/
int *ipr;		/* printout flag for error messages. 0=screen, 1=file,
			   2=both*/
char *efile;		/* error file name				*/
int *jpr;		/* printout flag for projection parameters 0=screen, 
			   1=file, 2 = both*/
char *pfile;		/* error file name				*/
/*
int *lemsg;
int *lparm;
*/
double *outcoor;	/* output coordinates				*/
int *outsys;		/* output projection code			*/
int *outzone;		/* output zone					*/
double *outparm;	/* output projection array			*/
int *outunit;		/* output units					*/
/*
int *ln27;
int *ln83;
*/
char fn27[];		/* file name of NAD 1927 parameter file		*/
char fn83[]; 	 	/* file name of NAD 1983 parameter file		*/
/*
int *length;
*/
int *iflg;		/* error flag					*/
{
double x;		/* x coordinate 				*/
double y;		/* y coordinate					*/
double factor;		/* conversion factor				*/
double lon;		/* longitude					*/
double lat;		/* latitude					*/
double temp;		/* dummy variable				*/
int i,j;		/* loop counters				*/
int ininit_flag;	/* input initilization flag			*/
int outinit_flag;	/* output initilization flag			*/
int *outdatum;		/* output datum -- currently should be same as
			   input datum					*/
int ind;		/* temporary var used to find state plane zone 	*/
int unit;		/* temporary unit variable			*/
double dummy[15];	/* temporary projection array 			*/

/* setup initilization flags and output message flags
---------------------------------------------------*/
outdatum = indatum;
ininit_flag = FALSE;
outinit_flag = FALSE;
*iflg = 0;

   *iflg = init(*ipr,*jpr,efile,pfile);
   if (*iflg != 0)
      return;


/* check to see if initilization is required
only the first 13 projection parameters are currently used.
If more are added the loop should be increased.
---------------------------------------------------------*/
if (iter == 0)
   {
   for (i = 0; i < MAXPROJ + 1; i++)
      {
      inpj[i] = 0;
      indat[i] = 0;
      inzn[i] = 0;
      outpj[i] = 0;
      outdat[i] = 0;
      outzn[i] = 0;
      for (j = 0; j < 15; j++)
         {
         pdin[i][j] = 0.0;
         pdout[i][j] = 0.0;
         }
      }
   ininit_flag = TRUE;
   outinit_flag = TRUE;
   iter = 1;
   }
 else
   {
   if (*insys != GEO)
     {
     if ((inzn[*insys] != *inzone) || (indat[*insys] != *indatum) || 
         (inpj[*insys] != *insys) || (*insys == 2))
        {
        ininit_flag = TRUE;
        }
     else
     for (i = 0; i < 13; i++)
        if (pdin[*insys][i] != inparm[i])
          {
          ininit_flag = TRUE;
          break;
          }
     }
   if (*outsys != GEO)
     {
     if ((outzn[*outsys] != *outzone) || (outdat[*outsys] != *outdatum) || 
         (outpj[*outsys] != *outsys) || (*outsys == 2))
        {
        outinit_flag = TRUE;
        }
     else
     for (i = 0; i < 13; i++)
        if (pdout[*outsys][i] != outparm[i])
          {
          outinit_flag = TRUE;
          break;
          }
     }
   }

/* Check input and output projection numbers
------------------------------------------*/
if ((*insys < 0) || (*insys > MAXPROJ))
   {
   p_error("Insys is illegal","GCTP-INPUT");
   *iflg = 1;
   return;
   }
if ((*outsys < 0) || (*outsys > MAXPROJ))
   {
   p_error("Outsys is illegal","GCTP-OUTPUT");
   *iflg = 2;
   return;
   }

/* find the correct conversion factor for units
---------------------------------------------*/
unit = *inunit;
/* use legislated unit table
-----------------------------*/
     if ((*indatum == 0) && (*insys == 2) && (*inunit == 6)) 
		unit = 1;
     if ((*indatum == 8) && (*insys == 2) && (*inunit == 6))
	{
        ind = 0;
        for (i = 1; i <134; i++)
	    {
            if (*inzone == NAD83[i]) 
		{
		ind = i;
		break;
		}
	    }
         if (ind != 0) 
	    unit = NADUT[ind];
	 }

if (*insys == GEO)
   *iflg = untfz(unit,0,&factor); 
else
   *iflg = untfz(unit,2,&factor); 
if (*iflg != 0)
   {
   close_file();
   return;
   }
 
x = incoor[0] * factor;
y = incoor[1] * factor;

/* Initialize inverse transformation
----------------------------------*/
if (ininit_flag)
   {
   inpj[*insys] = *insys;
   indat[*insys] = *indatum;
   inzn[*insys] = *inzone;
   for (i = 0;i < 15; i++)
      pdin[*insys][i] = inparm[i];
   if (*insys == 1)
      {
      for( i = 2; i < 15; i++)
         dummy[i] = inparm[i];
      dummy[0] = 0;
      dummy[1] = 0;
      sphdz(*indatum,dummy,&dummy[13],&dummy[14],&temp);
      if ((*inzone != 0) || (inparm[0] == 0.0)) 
         {
         dummy[0] = 1.0e6 * (double)(6 * *inzone -183); 
         if ( *inzone >= 0)
            dummy[1] = 4.0e7;         
         else
            dummy[1] = -4.0e7;         
         }
      else
         {
         dummy[0] = inparm[0];
         dummy[1] = inparm[1];
         }
      *indatum = 0;
      inv_init(*insys,*inzone,dummy,*indatum,fn27,fn83,iflg,inv_trans);
      }
   else
      inv_init(*insys,*inzone,inparm,*indatum,fn27,fn83,iflg,inv_trans);
   if (*iflg != 0)
      {
      close_file();
      return;
      }
   }

/* Do actual transformations
--------------------------*/

/* Inverse transformations
------------------------*/
if (*insys == GEO)
   {
   lon = x;
   lat = y;
   }
else
if ((*iflg = inv_trans[*insys](x, y, &lon, &lat)) != 0)
   {
   close_file();
   return;
   }

/* DATUM conversion should go here
--------------------------------*/

/* 
   The datum conversion facilities should go here 
*/

/* Initialize forward transformation
----------------------------------*/
if (outinit_flag)
   {
   outpj[*outsys] = *outsys;
   outdat[*outsys] = *outdatum;
   outzn[*outsys] = *outzone;
   for (i = 0;i < 15; i++)
      pdout[*outsys][i] = outparm[i];
   if (*outsys == 1)
      {
      for (i = 2; i < 15; i++)
          dummy[i] = outparm[i];
      dummy[0] = 0;
      dummy[1] = 0;
      sphdz(*indatum,dummy,&dummy[13],&dummy[14],&temp);
      if (outparm[0] == 0.0)
         {
         dummy[0] = pakr2dm(lon);
         dummy[1] = pakr2dm(lat);
         }
      else
         {
	 dummy[0] = outparm[0];
	 dummy[1] = outparm[1];
	 }
      *outdatum = 0;
      for_init(*outsys,*outzone,dummy,*outdatum,fn27,fn83,iflg,for_trans);
      }
   else
      for_init(*outsys,*outzone,outparm,*outdatum,fn27,fn83,iflg,for_trans);
   if (*iflg != 0)
      {
      close_file();
      return;
      }
   }

/* Forward transformations
------------------------*/
if (*outsys == GEO)
   {
   outcoor[0] = lon;
   outcoor[1] = lat;
   }
else
if ((*iflg = for_trans[*outsys](lon, lat, &outcoor[0], &outcoor[1])) != 0)
   {
   close_file();
   return;
   }

/* find the correct conversion factor for units
---------------------------------------------*/
unit = *outunit;
/* use legislated unit table
----------------------------*/
     if ((*outdatum == 0) && (*outsys == 2) && (*outunit == 6)) 
		unit = 1;
     if ((*outdatum == 8) && (*outsys == 2) && (*outunit == 6))
	{
        ind = 0;
        for (i = 1; i <134; i++)
	    {
            if (*outzone == NAD83[i]) 
		{
		ind = i;
		break;
		}
	    }
         if (ind != 0) 
	    unit = NADUT[ind];
	 }

if (*outsys == GEO)
   *iflg = untfz(0,unit,&factor); 
else
   *iflg = untfz(2,unit,&factor); 

outcoor[0] *= factor;
outcoor[1] *= factor;
close_file();
return;
}
