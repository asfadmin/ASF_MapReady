/*odlUtil:
	A set of utilities to read ODL-style
Restituted State Vector (rsv) files.
*/
#include "asf.h"
#include "geolocate.h"
#include "dateUtil.h"
#include "odlUtil.h"

/**************************
str2ydh:
	Given date string DDD:HH:MM:SS.CCCCC,
parses fields jd, hour, min, and sec of given ydh_date.
You must first initialize the year field of the date.
*/
void str2ydh(char *inStr,ymd_date *date,hms_time *time)
{
	julian_date jdate;
	jdate.year=date->year;
	if (4 != sscanf(inStr,"%d:%d:%d:%lf",
		&(jdate.jd),&(time->hour),&(time->min),&(time->sec)))
	{
		printf("ERROR! Invalid date '%s' passed to str2ydh!\n",inStr);exit(1);
	}
	date_jd2ymd(&jdate,date);
}

/*********************
rsv_open:
	Opens given ODL-style ASF Orbit information file,
extracts creation date and satellite name, and returns file pointer.
*/
FILE *rsv_open(char *fName,ymd_date *date,hms_time *time,char *satName,int *isPredicted)
{
	FILE *inF=FOPEN(fName,"r");
	char line[255];
	char dateStr[255];
	char predOrRest[255];/*"PREDICTED" or "RESTITUTED"*/
/*Extract first line, which contains the creation date for the file.*/
	if (NULL==fgets(line,255,inF))
	{
		printf("ERROR! Input file '%s' contains no lines!\n",fName);exit(1);
	}
	sscanf(line,"%d %s",&date->year,dateStr);
	str2ydh(dateStr,date,time);
	
/*Extract next line, which contains satellite name.*/
	if (NULL==fgets(line,255,inF))
	{
		printf("ERROR! Input file '%s' contains only 1 line!\n",fName);exit(1);
	}
	sscanf(line,"%s%s",satName,predOrRest);
	if (isPredicted!=NULL)
	{
		if (0==strcmp(predOrRest,"PREDICTED"))
			*isPredicted=1;
		else
			*isPredicted=0;
	}
	return inF;
}
/************************
rsv_next:
	Reads next line of ODL-style ASF Orbit information file,
extracts state vector in fixed-earth format, date, and orbit number.
Returns 1 on sucess, 0 on failure.
*/
int rsv_next(FILE *inF,int *orbitNo,ymd_date *date,hms_time *time,stateVector *stVec)
{
	char line[255];
	char dateStr[255];
	
	if (NULL==fgets(line,255,inF)) return 0;
	if (9!=sscanf(line,"%d %d %s %lf%lf%lf %lf%lf%lf",orbitNo,&date->year,dateStr,
		&(stVec->pos.x),&(stVec->pos.y),&(stVec->pos.z),
		&(stVec->vel.x),&(stVec->vel.y),&(stVec->vel.z)))
	{
		printf("Error! Line '%s' does not contain orbit number, year, datestr, state vector!\n",line);exit(1);
	}
	
	stVec->pos.x*=1000.0;
	stVec->pos.y*=1000.0;
	stVec->pos.z*=1000.0;
	
	str2ydh(dateStr,date,time);
	
	return 1;
}
/*************************
rsv_close:
	Closes given file, de-allocating any rsv information.
*/
void rsv_close(FILE *inF)
{
	FCLOSE(inF);
}
