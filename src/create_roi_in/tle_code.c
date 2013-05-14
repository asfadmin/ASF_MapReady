
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "seasat.h"

void get_next_tle_time(FILE *fpin, int *this_year, int *this_day, int *this_msec, char *line1, char *line2);


/*-------------------------------------------------------------------------
  This subroutine should:
	- search the TLE input file with all of the TLE for SEASAT
	- find the closest TLE that preceeds the start of this image
	- calculate the timing offset from the TLE found and the start of the image
	- create a new TLE file that has the extra parameters start, end, and inc at the end of the 2nd line
 -------------------------------------------------------------------------*/

void create_input_tle_file(julian_date target_date,hms_time target_time,const char *ofile)
{
    int found=0;
    int first=1;
    int i;
    
    FILE *fpin;
    FILE *fpout;
    
    int target_year;
    int target_day;
    int target_msec;
    
    int this_year;
    int this_day;
    int this_msec;
    
    int save_year;
    int save_day;
    int save_msec;
    
    int diff_year;
    int diff_day;
    int diff_msec;
    
    float diff_sec;
    
    char tle_line1[256];
    char tle_line2[256];
    char save_line1[256];
    char save_line2[256];
    char tmp_line2[256];
 
    /* parse time into correct format */
    target_year = target_date.year;
    target_day  = target_date.jd;
    target_msec = date_hms2sec(&target_time)*1000.0;
 
    /* open tle file */
    fpin = fopen("/home/talogan/bin/SEASAT_TLEs.txt","r");
    if (fpin == NULL) {printf("ERROR: unable to open input TLE file SEASAT_TLEs.txt\n"); exit(1);}

    printf("\tLooking for: %i %i %i\n",target_year,target_day,target_msec);

    while (!found)
      {
        if (!first) {
	  /* save last time */
	  save_year = this_year;
	  save_day = this_day;
	  save_msec = this_msec;
	  for (i=0;i<256;i++) {save_line1[i] = tle_line1[i]; save_line2[i] = tle_line2[i];}
	}

        get_next_tle_time(fpin,&this_year,&this_day,&this_msec,tle_line1,tle_line2);
	
	if (this_year>target_year) {printf("ERROR: WRONG YEAR FOR TLES!!!\n"); exit(1);}
	else if (this_day>target_day) {
	    if (first) {
	      printf("ERROR: first TLE start date is past the start date of this scene\n");
	      exit(1);
	    } else found =1;
	  }
	else if (this_day==target_day) {
	  if (this_msec> target_msec) found = 1;
	}
        first = 0;
      }
      
    fclose(fpin);
  
    /* once we are here, then the "this_" TLE is AFTER, so we need to use the "save_" TLE, 
       unless we found it on the first try, in which case we copy this_ to save_ */
    if (first==1) {
	save_year = this_year;
	save_day = this_day;
	save_msec = this_msec;
	for (i=0;i<256;i++) {save_line1[i] = tle_line1[i]; save_line2[i] = tle_line2[i];}
    }

    /* for (i=0; i++; i<256) { if (save_line2[i]=='\n') save_line2[i]=' '; } */
    

    printf("\tClosest  is: %i %i %i\n",save_year,save_day,save_msec);
    printf("\t%s",save_line1);
    printf("\t%s",save_line2);

    /* Calculate time difference */
    diff_year = target_year - save_year;
    diff_day  = target_day  - save_day;
    diff_msec = target_msec - save_msec;

    if (diff_year != 0) {printf("ERROR: Can not propagate state vectors for a year!\n"); exit(1);}

    diff_sec = diff_day*86400.0 + diff_msec/1000.0;


    strncpy(tmp_line2,save_line2,69);
    save_line2[69]=' ';
    sprintf(&tmp_line2[69]," %f %f 0.0166666667\n",diff_sec/60.0,(diff_sec+600.0)/60.0);
    
    /* sprintf(tmp_line2,"%s %f %f 15.0\n",save_line2,diff_sec,diff_sec+30.0); */

    fpout = fopen(ofile,"w");
    if (fpout==NULL) {printf("ERROR: unable to open input TLE file SEASAT_TLEs.txt\n"); exit(1);}
    fprintf(fpout,"%s",save_line1);
    fprintf(fpout,"%s",tmp_line2);
    fclose(fpout);
}


void get_next_tle_time(FILE *fpin, int *this_year, int *this_day, int *this_msec, char *line1, char *line2)
{
    int max = 256;
    int line_no = 0;
    char tmp1[7];
    char tmp2[7];
    double full_time;
    long msec_per_day = 24 * 60 * 60 * 1000;
    
    if (fgets(line1,max,fpin)==NULL) {printf("ERROR:bad get from tle file\n"); exit(1);}
    if (line1[0]!='1') {printf("ERROR:bad get from tle file\n"); exit(1);}
    if (fgets(line2,max,fpin)==NULL) {printf("ERROR:bad get from tle file\n"); exit(1);}
    if (line2[0]!='2') {printf("ERROR:bad get from tle file\n"); exit(1);}
    
    sscanf(line1,"%1i %s %s %lf",&line_no,tmp1,tmp2,&full_time);
    
    *this_year = (int) full_time / 1000;
    *this_day  = (int) (full_time - (*this_year*1000));
    *this_msec = (int) ((full_time - (*this_year*1000) - *this_day)*msec_per_day);
    *this_year = *this_year+1900;
    
    /* printf("Found: %i %i %i\n",*this_year,*this_day,*this_msec); */
}
    
    
    
    
  
