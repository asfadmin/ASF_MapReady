#include "swath_offset_inc.h"
#include "dateUtil.h"
#include <math.h>


/*
 * swap functions.
 */
/*
void swapi(int *first, int *second)
{
	int Tint;
	Tint = *first;
	*first = *second;
	*second = Tint;
}
void swapd(double *first, double *second)
{
	double Tdbl;
	Tdbl = *first;
	*first = *second;
	*second = Tdbl;
}
void swapf(float *first, float *second)
{
	float Tflt;
	Tflt = *first;
	*first = *second;
	*second = Tflt;
}

void swapc(char **first, char **second)
{
	char *Tchar;
	Tchar = *first;
	*first = *second;
	*second = Tchar;
}*/

/* 
 * dot product.
 */
double dot(double *vec0, double *vec1)
  { double ret; ret = vec0[0]*vec1[0] + vec0[1]*vec1[1] + vec0[2]*vec1[2]; return(ret); }

/*
 * magnitude of a vector.
 */
double mag(double *vec)
  { double ret; ret = sqrt(SQR(vec[0])+SQR(vec[1])+SQR(vec[2])); return(ret); }
  
/* 
 * calculate the arc distance.
 */
double calc_distance(double R, double lon0, double lat0, double lon1, double lat1)
 {
   double vec0[3], vec1[3];
   double distance;
    
   vec0[0] = R * cos(lon0*DTR) * cos(lat0*DTR);
   vec0[1] = R * sin(lon0*DTR) * cos(lat0*DTR);
   vec0[2] = R * (1-ECC2) * sin(lat0*DTR);
    
   vec1[0] = R * cos(lon1*DTR) * cos(lat1*DTR);
   vec1[1] = R * sin(lon1*DTR) * cos(lat1*DTR);
   vec1[2] = R * (1-ECC2) * sin(lat1*DTR);

   distance = R * acos(dot(vec0, vec1)/(mag(vec0)*mag(vec1)));
 
   return(distance);
 }

/*
 * finds a line containing some string as the first 
 * field or word, and returns the second field associated with it 
 * as an integer.
 */

int getFromFilei(FILE *fptr, char *seekFor)
{
	char line1[255], param1[255];
	int tVal = 0;
	
	while(tVal == 0 && NULL != fgets(line1, 255, fptr) )
	{
		sscanf(line1, "%s", param1);
		if(strncmp(param1, seekFor, strlen(seekFor) ) == 0)
			sscanf(line1, "%s %li", param1, &tVal);
	}

	return tVal;
}

/*
 * finds a line containing some string as the first 
 * field or word, and returns the second field associated with it 
 * as a double.
 */
double getFromFiled(FILE *fptr, char *seekFor)
{
	char line1[255], param1[255];
	double tVal = 0.0;
	
	while(tVal == 0.0 && NULL != fgets(line1, 255, fptr) )
	{
		sscanf(line1, "%s", param1);
		if(strncmp(param1, seekFor, strlen(seekFor) ) == 0)
			sscanf(line1, "%s %lf", param1, &tVal);
	}

	return tVal;
}

/*
 * finds a line containing some string as the first 
 * field or word, and returns the second field associated with it 
 * as a character string.
 */
void getFromFilec(FILE *fptr, char *seekFor, char *found)
{
	char line1[255], param1[255];
	int foundVal = 0;
	
	while(foundVal == 0 && NULL != fgets(line1, 255, fptr) )
	{
		sscanf(line1, "%s", param1);
		if(strncmp(param1, seekFor, strlen(seekFor) ) == 0)
		{
			sscanf(line1, "%s %s", param1, found);
			foundVal = 1;
		}
	}
}

/* 
 * returns the number of seconds since 1950 from the date given 
 */ 
double getSecd(double datetime)
{
	char line[255];
        ymd_date date1;
        hms_time time1; 
        julian_date jDate;
	
	sprintf(line, "%lf", datetime);
	parse_ymdTime(line, &date1, &time1);
	date_ymd2jd(&date1, &jDate);
	return date_hms2sec(&time1) + (double)(date_getMJD(&jDate) * 3600 * 24);
}

/* 
 * returns the number of seconds since 1950 from the date given 
 */ 
double getSecc(char *datetime)
{
        ymd_date date1;
        hms_time time1; 
        julian_date jDate;

	parse_ymdTime(datetime, &date1, &time1);
	date_ymd2jd(&date1, &jDate);
	return date_hms2sec(&time1) + (double)(date_getMJD(&jDate) * 3600 * 24);
}
