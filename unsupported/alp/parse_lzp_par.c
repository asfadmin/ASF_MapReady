/******************************************************************************
NAME:  parse_lzp_par - create ASAP input file for each location from LZP par

SYNOPSIS:  parse_lzp_par <inPARfile> <nlocs>

DESCRIPTION:	

  This program is one of several used to automate the analysis of LZP .par
  metadata files. Its function is to set up input files for orbital propagation
  by the program propagate.  This is accomplished by reading the input .par
  file and extracting:
	  1) The prep block State Vector 
	  2) The prep block GHA angle
	  3) The prep block State Vector time
	  4) Each of the location block times
  Once this information is extracted, one loc##.cart file, suitable for input
  to propagate, is created for each location block.  (See propagate for file
  format description).

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    lzDouble		Extracts a double valued parameter from an LZP file
    lzStr		Extracts a string valued parameter from an LZP file
    fixed2gei		Converts earth fixed St. vector into Intertial

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    inPARfile		Input LZP .par file
    loc##.cart		Output St. Vector files, one for each location block

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    9/98   T. Logan	Check LZP product files

HARDWARE/SOFTWARE LIMITATIONS:
	
    Doesn't determine the number of locations in a par file, this must be
    specified on the command line.

    Output file names are hard coded to be "loc##.cart" where ## is the 
    location block number.

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include "asf.h"
#include "lzFetch.h"

#define   VERSION 1.0
#define   WE      0.00417807442	/* Earth's rotation rate in degrees/sec */

struct gmt { int year; int month; int day; int doy; double sec; };
void dssr2gmt(char *time, struct gmt *end);
double diff_time(struct gmt start, struct gmt end);
void create_input_vect(char *file, double *vec,
                         struct gmt *start, struct gmt *end, double gha);

main(int argc,char *argv[])
{
   double stVec[6];
   char  *tmpStr;
   int   block, nblocks;
   char  file[80];
   int   i;
   double gha;
   struct gmt start;

   if (argc != 3) 
    {
	printf("Usage: %s <inParFile> <nlocs>\n",argv[0]);
	printf("\nASF STEP Tools,  Version %.2f\n",VERSION);
        exit(1);
    }
   strcpy(file,argv[1]);
   nblocks = atoi(argv[2]);

   /* Read input state vector */
   stVec[0] = lzDouble(file,"prep_block.state_vector.x:",NULL);
   stVec[1] = lzDouble(file,"prep_block.state_vector.y:",NULL);
   stVec[2] = lzDouble(file,"prep_block.state_vector.z:",NULL);
   stVec[3] = lzDouble(file,"prep_block.state_vector.xv:",NULL);
   stVec[4] = lzDouble(file,"prep_block.state_vector.yv:",NULL);
   stVec[5] = lzDouble(file,"prep_block.state_vector.zv:",NULL);

   /* Read greenwhich hour angle & use to convert the state vector */
   gha = lzDouble(file,"prep_block.GHA.angle:",NULL);
   fixed2gei(stVec,gha);

   /* Read the time for the input state vector & convert to GMT structure */
   tmpStr = lzStr(file,"prep_block.state_vector.Date:",NULL);
   date2gmt(tmpStr,&start);
   free(tmpStr); 

   /* For each block, read the location time & create a loc##.cart file */
   for (block=0; block < nblocks; block++)
     {
      char base[80];
      char where[256];
      struct gmt end;
      double diff;
      double this_gha;

      sprintf(base,"prep_block.location[%d].",block);

      strcpy(where,base);
      strcat(where,"line_date:");
      tmpStr = lzStr(file,where,NULL);
      dssr2gmt(tmpStr,&end);
      free(tmpStr);
      diff = diff_time(start,end);
      printf("Offset %lf\n\n",diff);
      this_gha = gha + WE * diff;
      sprintf(where,"loc%.2d.cart",block); 
      create_input_vect(where,stVec,&start,&end,this_gha);      
     }
  exit(0);
}

/* Creates an input file for the program propagate */
void create_input_vect(char *file, double *vec,
                         struct gmt *start, struct gmt *end, double gha)
 {
   FILE *fp;
   int  nsteps = 1;                     /* Number of propagation steps */
   int  i;

   fp = fopen(file,"w");
   for (i=0; i<3; i++) fprintf(fp,"%lf\n",vec[i]/1000.0);
   for (i=3; i<6; i++) fprintf(fp,"%lf\n",vec[i]);

   fprintf(fp,"%i\n",start->year);
   fprintf(fp,"%i\n",start->month);
   fprintf(fp,"%i\n",start->day);
   fprintf(fp,"%.6lf\n",start->sec);
   fprintf(fp,"%i\n",end->year);
   fprintf(fp,"%i\n",end->month);
   fprintf(fp,"%i\n",end->day);
   fprintf(fp,"%.6lf\n",end->sec);
   fprintf(fp,"%i\n",nsteps);
   fprintf(fp,"%lf\n",gha);
   fclose(fp);
 }  

/* Converts LZP state vector formatted time string into GMT structure */
date2gmt(char *time, struct gmt *start)
 {
   /* Format for input is   DD-MMM-YYYY HH:MM:SS.TTT  */

   int hour, min;
   double sec;
				
   start->year = atoi(&(time[7]));

   if (strstr(time,"JAN")!=NULL) start->month = 1;
   else if (strstr(time,"FEB")!=NULL) start->month = 2;
   else if (strstr(time,"MAR")!=NULL) start->month = 3;
   else if (strstr(time,"APR")!=NULL) start->month = 4;
   else if (strstr(time,"MAY")!=NULL) start->month = 5;
   else if (strstr(time,"JUN")!=NULL) start->month = 6;
   else if (strstr(time,"JUL")!=NULL) start->month = 7;
   else if (strstr(time,"AUG")!=NULL) start->month = 8;
   else if (strstr(time,"SEP")!=NULL) start->month = 9;
   else if (strstr(time,"OCT")!=NULL) start->month = 10;
   else if (strstr(time,"NOV")!=NULL) start->month = 11;
   else if (strstr(time,"DEC")!=NULL) start->month = 12;
   else { printf("Unable to determine month from %s\n",time); exit(1); }

   start->day = atoi(&(time[0]));
   start->doy = gmt_day(start->year, start->month, start->day);
   hour = atoi(&(time[12]));
   min  = atoi(&(time[15]));
   sec  = atof(&(time[18]));
   start->sec = ((hour*60.0)+min)*60.0+sec;

   return;
 }

static int DIM[13]={0,31,28,31,30,31,30,31,31,30,31,30,31}; /* Days in Month */
double SecPerDay = 86400.0;
#define GrabTwoChars    lb[0]=time[i++]; lb[1]=time[i++];

/* Converts DSSR formatted time string into GMT structure */
void dssr2gmt(char *time, struct gmt *end)
 {
  /* Format for input is   YYYYMMDDHHMMSSTTT  */

  int i;
  char lb[6];
  int hour, min;

  for (i=0; i<4; i++) lb[i] = time[i];  end->year = atoi(lb);
  if (end->year%4 == 0) DIM[2]=29; 

  lb[2]= ' '; lb[3]= ' ';
  GrabTwoChars; end->month = atoi(lb);
  GrabTwoChars; end->day   = atoi(lb);
  GrabTwoChars; hour  = atoi(lb);
  GrabTwoChars; min   = atoi(lb);
  GrabTwoChars; lb[2]=time[i++]; lb[3]=time[i++]; lb[4]=time[i++]; lb[5] = ' ';
  end->sec = (double) atof(lb);
  end->sec /= 1000.0;
  end->sec += ((hour*60.0)+min)*60.0;

  end->doy = 0;
  for (i=0; i<end->month; i++) { end->doy += DIM[i]; }
  end->doy+=end->day; 

  return;
 }

/* Calculates the day of the year, from the year, month, day */
int gmt_day(int year, int month, int day)
 {
    int doy=0;
    int i;

    if (year%4==0) DIM[2] = 29;
    for (i = 1; i<month; i++)
	doy += DIM[i];
    doy+=day;

    return(doy);
 }

/* Returns the difference between the start and end times in Seconds */
double diff_time(struct gmt start, struct gmt end)
 {
    double diff_year, diff_doy, diff_sec, total;
    diff_year = end.year - start.year;
    diff_doy  = end.doy - start.doy;
    diff_sec  = end.sec - start.sec;
    total = ((diff_year*365.0)+diff_doy)*(SecPerDay) + diff_sec;
    return(total);
 } 
