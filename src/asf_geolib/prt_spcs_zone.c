/*******************************************************************************
NAME			      prt_spcs_zone 

PURPOSE	     Print the state or area which corresponds to the state plane
	     coordinate system zone id entered by the user.

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
D. Steinwand		Aug, 1988	Original Development
					

PROJECT     LAS

ALGORITHM 

	Find the appropriate zone code
	Print the state or area information

ALGORITHM REFERENCES

1.  Clarie, Charles N, "State Plane Coordinates by Automatic Data Processing",
    U.S. Department of Commerce, Environmental Science Services Admin.,
    Coast and Geodetic Survey, Publication 62-4, 1973.

2.  "Software Documentation for GCTP General Cartographic Transformation
    Package", U.S. Geological Survey National Mapping Division, May 1982.

3.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
    Survey Professional Paper 1395 (Supersedes USGS Bulletin 1532), United
    State Government Printing Office, Washington D.C., 1987. 
*******************************************************************************/
#include "worgen.h"
#include "asf.h"

void prt_spcs_zone(
	int zone,		/* State Plane Coordinate System zone id */
	FILE *fp)		/* Pointer to print file */
{

switch (zone) {

case 101: fprintf(fp, " Alabama--East\n"); break;
case 102: fprintf(fp, " Alabama--West\n"); break;
case 201: fprintf(fp, " Arizona--East\n"); break;
case 202: fprintf(fp, " Arizona--Central\n"); break; 
case 203: fprintf(fp, " Arizona--West\n"); break;
case 301: fprintf(fp, " Arkansas--North\n"); break;
case 302: fprintf(fp, " Arkansas--South\n"); break;
case 401: fprintf(fp, " California--Zone 1\n"); break;
case 402: fprintf(fp, " California--Zone 2\n"); break;
case 403: fprintf(fp, " California--Zone 3\n"); break;
case 404: fprintf(fp, " California--Zone 4\n"); break;
case 405: fprintf(fp, " California--Zone 5\n"); break;
case 406: fprintf(fp, " California--Zone 6\n"); break;
case 407: fprintf(fp, " California--Zone 7\n"); break;
case 501: fprintf(fp, " Colorado--North\n"); break;
case 502: fprintf(fp, " Colorado--Central\n"); break;
case 503: fprintf(fp, " Colorado--South\n"); break;
case 600: fprintf(fp, " Connecticut\n"); break;
case 700: fprintf(fp, " Delaware\n"); break;
case 901: fprintf(fp, " Florida--East\n"); break;
case 902: fprintf(fp, " Florida--West\n"); break;
case 903: fprintf(fp, " Florida--North\n"); break;
case 1001: fprintf(fp, " Georgia--East\n"); break;
case 1002: fprintf(fp, " Georgia--West\n"); break;
case 1101: fprintf(fp, " Idaho--East\n"); break;
case 1102: fprintf(fp, " Idaho--Central\n"); break;
case 1103: fprintf(fp, " Idaho--West\n"); break;
case 1201: fprintf(fp, " Illinois--East\n"); break;
case 1202: fprintf(fp, " Illinois--West\n"); break;
case 1301: fprintf(fp, " Indiana--East\n"); break;
case 1302: fprintf(fp, " Indiana--West\n"); break;
case 1401: fprintf(fp, " Iowa--North \n"); break;
case 1402: fprintf(fp, " Iowa--South\n"); break;
case 1501: fprintf(fp, " Kansas--North\n"); break;
case 1502: fprintf(fp, " Kansas--South\n"); break;
case 1601: fprintf(fp, " Kentucky--North\n"); break;
case 1602: fprintf(fp, " Kentucky--South\n"); break;
case 1701: fprintf(fp, " Louisiana--North\n"); break;
case 1702: fprintf(fp, " Louisiana--South\n"); break;
case 1703: fprintf(fp, " Louisiana--Offshore\n"); break;
case 1801: fprintf(fp, " Maine--East \n"); break;
case 1802: fprintf(fp, " Maine--West\n"); break;
case 1900: fprintf(fp, " Maryland & District of Columbia\n"); break;
case 2001: fprintf(fp, " Massachusetts--Mainland\n"); break;
case 2002: fprintf(fp, " Massachusetts--Island\n"); break;
case 2101: fprintf(fp, " Michigan--East (TM)\n"); break;
case 2102: fprintf(fp, " Michigan--Central (TM)\n"); break;
case 2103: fprintf(fp, " Michigan--West (TM)\n"); break;
case 2111: fprintf(fp, " Michigan--North (Lambert)\n"); break;
case 2112: fprintf(fp, " Michigan--Central (Lambert)\n"); break;
case 2113: fprintf(fp, " Michigan--South (Lambert)\n"); break;
case 2201: fprintf(fp, " Minnesota--North\n"); break;
case 2202: fprintf(fp, " Minnesota--Central\n"); break;
case 2203: fprintf(fp, " Minnesota--South\n"); break;
case 2301: fprintf(fp, " Mississippi--East\n"); break;
case 2302: fprintf(fp, " Mississippi--West\n"); break;
case 2401: fprintf(fp, " Missouri--East\n"); break;
case 2402: fprintf(fp, " Missouri--Central\n"); break;
case 2403: fprintf(fp, " Missouri--West\n"); break;
case 2501: fprintf(fp, " Montana--North\n"); break;
case 2502: fprintf(fp, " Montana--Central\n"); break;
case 2503: fprintf(fp, " Montana--South\n"); break;
case 2601: fprintf(fp, " Nebraska--North\n"); break;
case 2602: fprintf(fp, " Nebraska--South\n"); break;
case 2701: fprintf(fp, " Nevada--East\n"); break;
case 2702: fprintf(fp, " Nevada--Central\n"); break;
case 2703: fprintf(fp, " Nevada--West \n"); break;
case 2800: fprintf(fp, " New Hampshire\n"); break;
case 2900: fprintf(fp, " New Jersey \n"); break;
case 3001: fprintf(fp, " New Mexico--East\n"); break;
case 3002: fprintf(fp, " New Mexico--Central\n"); break;
case 3003: fprintf(fp, " New Mexico--West\n"); break;
case 3101: fprintf(fp, " New York--East\n"); break;
case 3102: fprintf(fp, " New York--Central\n"); break;
case 3103: fprintf(fp, " New York--West\n"); break;
case 3104: fprintf(fp, " New York--Long Island\n"); break;
case 3200: fprintf(fp, " North Carolina\n"); break;
case 3301: fprintf(fp, " North Dakota--North\n"); break;
case 3302: fprintf(fp, " North Dakota--South\n"); break;
case 3401: fprintf(fp, " Ohio--North\n"); break;
case 3402: fprintf(fp, " Ohio--South\n"); break;
case 3501: fprintf(fp, " Oklahoma--North\n"); break;
case 3502: fprintf(fp, " Oklahoma--South\n"); break;
case 3601: fprintf(fp, " Oregon--North\n"); break;
case 3602: fprintf(fp, " Oregon--South\n"); break;
case 3701: fprintf(fp, " Pennsylvania--North\n"); break;
case 3702: fprintf(fp, " Pennsylvania--South\n"); break;
case 3800: fprintf(fp, " Rhode Island \n"); break;
case 3901: fprintf(fp, " South Carolina--North\n"); break;
case 3902: fprintf(fp, " South Carolina--South\n"); break;
case 4001: fprintf(fp, " South Dakota--North\n"); break;
case 4002: fprintf(fp, " South Dakota--South\n"); break;
case 4100: fprintf(fp, " Tennessee\n"); break;
case 4201: fprintf(fp, " Texas--North\n"); break;
case 4202: fprintf(fp, " Texas--North Central\n"); break;
case 4203: fprintf(fp, " Texas--Central\n"); break;
case 4204: fprintf(fp, " Texas--South\n"); break;
case 4205: fprintf(fp, " Texas--South Central\n"); break;
case 4301: fprintf(fp, " Utah--North\n"); break;
case 4302: fprintf(fp, " Utah--Central\n"); break;
case 4303: fprintf(fp, " Utah--South\n"); break;
case 4400: fprintf(fp, " Vermont\n"); break;
case 4501: fprintf(fp, " Virginia--North\n"); break;
case 4502: fprintf(fp, " Virginia--South\n"); break;
case 4601: fprintf(fp, " Washington--North\n"); break;
case 4602: fprintf(fp, " Washington--South\n"); break;
case 4701: fprintf(fp, " West Virginia--North\n"); break;
case 4702: fprintf(fp, " West Virginia--South\n"); break;
case 4801: fprintf(fp, " Wisconsin--North\n"); break;
case 4802: fprintf(fp, " Wisconsin--Central\n"); break;
case 4803: fprintf(fp, " Wisconsin--South\n"); break;
case 4901: fprintf(fp, " Wyoming--East\n"); break;
case 4902: fprintf(fp, " Wyoming--East Central\n"); break;
case 4903: fprintf(fp, " Wyoming--West Central\n"); break;
case 4904: fprintf(fp, " Wyoming--West\n"); break;
case 5001: fprintf(fp, " Alaska--Zone 1\n"); break;
case 5002: fprintf(fp, " Alaska--Zone 2\n"); break;
case 5003: fprintf(fp, " Alaska--Zone 3\n"); break;
case 5004: fprintf(fp, " Alaska--Zone 4\n"); break;
case 5005: fprintf(fp, " Alaska--Zone 5\n"); break;
case 5006: fprintf(fp, " Alaska--Zone 6\n"); break;
case 5007: fprintf(fp, " Alaska--Zone 7\n"); break;
case 5008: fprintf(fp, " Alaska--Zone 8\n"); break;
case 5009: fprintf(fp, " Alaska--Zone 9\n"); break;
case 5010: fprintf(fp, " Alaska--Zone 10\n"); break;
case 5101: fprintf(fp, " Hawaii--Zone 1\n"); break;
case 5102: fprintf(fp, " Hawaii--Zone 2\n"); break;
case 5103: fprintf(fp, " Hawaii--Zone 3\n"); break;
case 5104: fprintf(fp, " Hawaii--Zone 4\n"); break;
case 5105: fprintf(fp, " Hawaii--Zone 5\n"); break;
case 5201: 
   fprintf(fp, " Puerto Rico & Virgin Islands--St. John, St. Thomas\n"); break;
case 5202: fprintf(fp, " Virgin Islands--St. Croix\n"); break;
case 5300: fprintf(fp, " American Samoa\n"); break;
case 5400: fprintf(fp, " Guam\n"); break;
}
return;
}
