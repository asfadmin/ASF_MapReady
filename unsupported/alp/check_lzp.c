/******************************************************************************
NAME: check_lzp - Checks LZP par file metadata values

SYNOPSIS: check_lzp <inASAPfile> <inVectfile> <inPARfile> 

DESCRIPTION:

	This is one of several programs designed the automate the analysis of
    LZP .par metadata files.  Its function is to do the actual analysis using
    propagated state vectors (inASAPfile) along with there GHA's (inVectFile)
    to calculate metadata that is compared with that given in an LZP par 
    file (inPARfile).

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    get_trg_pos		Given slant range, doppler, and state vector, returns
			incidence, look, yaw, lat, lon, swath velocity

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    inASAPfile		ASAP propagated state vector file
    inVectFile		Input used by ASAP to create propagated vector file
    inPARfile		Input LZP .par file to be checked    

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    9/98   T. Logan     Analyze LZP .par files.

HARDWARE/SOFTWARE LIMITATIONS:

   The name of the input vector file must contain the corresponding 
   location block number from the inPARfile as the last two characters
   before the first '.' in the filename.

ALGORITHM DESCRIPTION:

	Read propagated state vector
	Read GHA 
	Convert state vector to Earth Fixed
	Determine left/right looking & ellipsoid to use
	Calculate nadir parameters & compare with reported 
	Calculate near range parameters and compare with LZP reported
	Calculate far range parameters and compare with LZP reported
	
ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include "asf.h"
#include "fnc_defs.h"
#include "lzFetch.h"
#include "ceos.h"

#define VERSION 1.0

void getPropagatedVect(char *,double *);
void getInputVect(char *, double *);
void report_diff(double , double , char *);
void report_offsets(double , double , double , double , double );
double calc_distance(double ,double ,double ,double ,double );
double dot(double *, double *);
double mag(double *);
char  PROCMODE;
double RE, RP, ECC_E;

main(int argc,char *argv[])
{
  char  vectFile[256];
  char  PARfile[256];
  char  base[80];
  char  where[256];
  char  *tmp_str;
  int    i;
  int  loc_num;
  double stVec[6];
  double r, lat, lon;
  double sc_alt;
  double tmp1;
  double gha;
  double tlat, tlon;
  double near_range, far_range, inc, look, yaw, dum;
  double calc_range;
  double swathvel, repo_vel;
    
  if (argc != 4)
   {
     printf("Usage: %s <inASAPfile> <inVECTfile> <inPARfile>\n",argv[0]);
     printf("\nASF STEP Tools,  Version %.2f\n\n",VERSION);
     exit(1);
   }
  strcpy(vectFile,argv[2]);
  strcpy(PARfile,argv[3]);

  /* Get the state vector and convert to EBF */
  getPropagatedVect(argv[1],stVec);
  getInputVect(vectFile,&gha);
  gei2fixed(stVec,gha);

  /* determine location block to compare with */
  tmp_str = strchr(vectFile,'.'); tmp_str--; tmp_str--;
  base[0] = tmp_str[0]; base[1] = tmp_str[1]; base[2]='\0';
  loc_num = atoi(base);

  printf("  Reading PAR file %s at location %i\n",PARfile,loc_num);
  sprintf(base,"prep_block.location[%d].",loc_num);

  setGlobalParameters(PARfile);

  nadir(stVec,&r,&lat,&lon,&sc_alt);

  strcpy(where,base);
  strcat(where,"platform_altitude:");
  tmp1 = lzDouble(PARfile,where,NULL);
  tmp1 /= 1000.0;
  report_diff(sc_alt,tmp1,"Spacecraft Altitude");

  /* Get the near slant range to image */
  strcpy(where,base);
  strcat(where,"near_range:");
  near_range = lzDouble(PARfile,where,NULL);
  near_range /= 1000.0;

  /* Calculate the geolocation */
  get_trg_pos(near_range,0.0,stVec,&inc,&look,&yaw,&lat,&lon,&swathvel,&dum);
  if (lon < -180.0) lon += 360.0; 

  repo_vel = lzDouble(PARfile,"prep_block.swath_velocity:",NULL);
  report_diff(swathvel,repo_vel,"Swath Velocity");

  strcpy(where,base);
  strcat(where,"range_gate:");
  tmp1 = lzDouble(PARfile,where,NULL);
  calc_range = ((CSPEED*tmp1)/2.0)/1000.0;
  report_diff(calc_range,near_range,"Near Range");

  printf("\tCalculated look angle = %lf\n", look);

  /* Get the reported geolocation */
  strcpy(where,base);
  strcat(where,"first_pixel_ll:");
  tmp_str = lzStr(PARfile,where,NULL);
  sscanf(tmp_str,"%lf %lf",&tlat,&tlon);
  report_offsets(lat, lon, tlat, tlon, r);

  if (PROCMODE == 'R')
   { 
     printf("\n\tIF WE ASSUME RIGHT LOOKING:\n");
     PROCMODE = 'S';
     get_trg_pos(near_range,0.0,stVec,&inc,&look,&yaw,&lat,&lon,&swathvel,&dum);
     if (lon < -180.0) lon += 360.0; 
     report_offsets(lat, lon, tlat, tlon, r);
     PROCMODE = 'R';
   }

  /* Get the far slant range to image */
  strcpy(where,base);
  strcat(where,"far_range:");
  far_range = lzDouble(PARfile,where,NULL);
  far_range /= 1000.0;
  printf("\n\tFar range = %lf\n", far_range);

  /* Calculate the geolocation */
  get_trg_pos(far_range,0.0,stVec,&inc,&look,&yaw,&lat,&lon,&swathvel,&dum);
  if (lon < -180.0) lon += 360.0; 
  printf("\tCalculated look angle = %lf\n", look);
  
  /* Get the reported geolocation */
  strcpy(where,base);
  strcat(where,"last_pixel_ll:");
  tmp_str = lzStr(PARfile,where,NULL);
  sscanf(tmp_str,"%lf %lf",&tlat,&tlon);
  report_offsets(lat, lon, tlat, tlon, r);

  if (PROCMODE == 'R')
   { 
     printf("\n\tIF WE ASSUME RIGHT LOOKING:\n");
     PROCMODE = 'S';
     get_trg_pos(far_range,0.0,stVec,&inc,&look,&yaw,&lat,&lon,&swathvel,&dum);
     if (lon < -180.0) lon += 360.0; 
     report_offsets(lat, lon, tlat, tlon, r);
   }

  exit(0);
}

setGlobalParameters(char *PARfile)
{
  double tmp1;
  char *tmp_str;

  tmp1 = lzDouble(PARfile,"prep_block.clock_angle:",NULL);
  if (tmp1 == 90.0) 
    { PROCMODE = 'S'; printf("\tThis is right looking data\n"); }
  else if (tmp1 == -90.0)
    { PROCMODE = 'R'; printf("\tThis is left looking data\n"); }
  else {printf("Can't get the clock angle\n"); exit(1); }

  tmp_str = lzStr(PARfile,"prep_block.satellite:",NULL);
  if (strncmp(tmp_str,"RSAT1",5)==0)
   {
     printf("\tUsing INTERNATIONAL ellipsoid\n");
     RE = 6378140.0; 
     RP = 6356755.0;
     ECC_E = sqrt(SQR(RE)-SQR(RP))/RE;
   }
  else
   {
     printf("\tUsing GEM6 ellipsoid\n");
     RE = 6378144.0; 
     RP = 6356759.0;
     ECC_E =  8.1827385E-2;
   }
  free(tmp_str);
}


nadir(double *stVec, double *r, double *lat, double *lon, double *sc_alt)
{
  double R;

  /* Calculate the nadir parameters */
  R = mag(stVec);
  *lat = asin(stVec[2]/R);
  *lat = atand(tan(*lat)/(1-SQR(ECC_E)));

  *lon = atan2(stVec[1],stVec[0]);
  *lon = *lon * rtd;
  if (*lon<-180.0) *lon+=360.0;

  /*printf("\nCalculated spacecraft longitude at nadir is %.3lf\n",lon); */
  /*printf("Calculated spacecraft latitude at nadir is %.3lf\n",lat); */

  *r = (RE*RP)/sqrt(SQR(RP*cosd(*lat)) + SQR(RE*sind(*lat)));  
  *r /= 1000.0;
  *sc_alt = R-*r;

  /*printf("Radius of the earth at the nadir: %lf\n",r);*/
  /*printf("Radius of the spacecraft        : %lf\n",R);*/

  return;
}

void report_diff(double actual, double expected, char *message)
{
 printf("\t%s :: %lf\t%lf\t%lf\n",message, actual, expected, actual-expected);
}

void report_offsets(double lat, double lon, double tlat, double tlon, double r)
{
  double tmp1;
  printf("\tlatitude   = %lf\t%lf\t%lf\n", lat,tlat, lat-tlat);
  printf("\tlongitude  = %lf\t%lf\t%lf\n", lon,tlon, lon-tlon);

  tmp1 = calc_distance(r, tlat, tlon, lat, lon);
  printf("\tGround Distance Offset = %lf km\n",tmp1);
}

void getPropagatedVect(char *ASAPfile,double *stVec)
{
  FILE   *fp;
  double time, power;
  int    svPower[6];
  int    i;
 
  printf("  Reading ASAP file %s\n",ASAPfile);
  fp = fopen(ASAPfile,"r");
  if (fp == NULL) {printf("Unable to open file %s\n",ASAPfile); exit(1); }
  while (fscanf(fp,"%lfD%i",&time,&power) != EOF)
    for (i=0; i<6; i++) fscanf(fp,"%lfD%i",&(stVec[i]),&(svPower[i]));
  fclose(fp);

  time *= pow(10.0,power);
  for (i=0;i<3;i++) stVec[i] *= pow(10.0,(svPower[i]));
  for (i=3;i<6;i++) stVec[i] *= pow(10.0,(svPower[i]+3));

  printf("\tX pos  = %lf\n",stVec[0]);
  printf("\tY pos  = %lf\n",stVec[1]);
  printf("\tZ pos  = %lf\n",stVec[2]);
  printf("\tX vel  = %lf\n",stVec[3]);
  printf("\tY vel  = %lf\n",stVec[4]);
  printf("\tZ vel  = %lf\n",stVec[5]);

  return;
}

void getInputVect(char *vectFile, double *gha)
{
  FILE *fp;
  double tmp1;
  int    i,year, month, day, hour, min;
  double sec;
 
  fp = fopen(vectFile,"r");
  printf("  Reading Vector file %s\n",vectFile);
 
  /* original state vector */ 
  fscanf(fp,"%lf",&tmp1);
  fscanf(fp,"%lf",&tmp1);
  fscanf(fp,"%lf",&tmp1);
  fscanf(fp,"%lf",&tmp1);
  fscanf(fp,"%lf",&tmp1);
  fscanf(fp,"%lf",&tmp1);
  /* original time */
  fscanf(fp,"%i",&i);
  fscanf(fp,"%i",&i);
  fscanf(fp,"%i",&i);
  fscanf(fp,"%lf",&tmp1);
  /* propagated time */
  fscanf(fp,"%i",&year);
  fscanf(fp,"%i",&month);
  fscanf(fp,"%i",&day);
  fscanf(fp,"%lf",&sec);
  /* propagation steps and propagated gha */
  fscanf(fp,"%i",&i);
  fscanf(fp,"%lf",gha);

  fclose(fp);

  hour = (int) (sec / 3600.0);
  sec -= (double) (hour *3600.0);
  min = (int) (sec / 60.0);
  sec -= (double) (min*60.0);

  printf("\tLine_date = %i%.2i%.2i%.2i%.2i%.3lf\n",year,month,day,hour,min,sec);
  printf("\tGHA       = %lf\n",*gha); 

  return;
}

double calc_distance(double R,double lon0,double lat0,double lon1,double lat1)
 {
   double vec0[3], vec1[3], diff[3];
   double distance;

   vec0[0] = R * cosd(lon0) * cosd(lat0);
   vec0[1] = R * sind(lon0) * cosd(lat0);
   vec0[2] = R * (1-SQR(ECC_E)) * sind(lat0);

   vec1[0] = R * cosd(lon1) * cosd(lat1);
   vec1[1] = R * sind(lon1) * cosd(lat1);
   vec1[2] = R * (1-SQR(ECC_E)) * sind(lat1);

   distance = R * acos(dot(vec0, vec1)/(mag(vec0)*mag(vec1)));

   return(distance);
 }

