/*odlUtil:
	A set of utilities to read ODL-style
Time Correlation Element (tce) files.
*/
#include "asf.h"
#include "geolocate.h"
#include "dateUtil.h"
#include "odlTceUtil.h"

/*********************
tce_open:
	Opens given ODL-style ASF Orbit information file,
extracts creation date and satellite name, and returns file pointer.
*/
FILE *tce_open(char *fName)
{
	FILE *inF=FOPEN(fName,"r");
	char line[255];
	char dateStr[255];
/*Extract first line, which contains the creation date for the file.*/
	if (NULL==fgets(line,255,inF))
	{
		printf("ERROR! Input file '%s' contains no lines!\n",fName);exit(1);
	}
	
	return inF;
}
/************************
tce_next:
	Reads next line of ODL-style ASF Orbit information file,
extracts time correlation element in fixed-earth format, date, and orbit number.
Returns 1 on sucess, 0 on failure.
*/
int tce_next(FILE *inF,int *orbitNo,ymd_date *date,hms_time *time,int *satClock,int *satMs)
{
	char line[255];
	char dateStr[255];
	
	if (NULL==fgets(line,255,inF)) return 0;
	if (5!=sscanf(line,"%d %d %s %d %d",orbitNo,&date->year,dateStr,
		satClock,satMs))
	{
		printf("Error! Line '%s' does not contain orbit number, year, datestr, clock, ms!\n",line);exit(1);
	}
	
	str2ydh(dateStr,date,time);
	
	return 1;
}
/*************************
tce_close:
	Closes given file, de-allocating any tce information.
*/
void tce_close(FILE *inF)
{
	FCLOSE(inF);
}
