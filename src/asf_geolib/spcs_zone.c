/*******************************************************************************
NAME			      spcs_zone 

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
#include "cproj.h"
#include "proj.h"

void spcs_zone(int zone)		/* State Plane Coordinate System zone id */
{

switch (zone) {

case 101: printf(" Alabama--East"); break;
case 102: printf(" Alabama--West"); break;
case 201: printf(" Arizona--East"); break;
case 202: printf(" Arizona--Central"); break; 
case 203: printf(" Arizona--West"); break;
case 301: printf(" Arkansas--North"); break;
case 302: printf(" Arkansas--South"); break;
case 401: printf(" California--Zone 1"); break;
case 402: printf(" California--Zone 2"); break;
case 403: printf(" California--Zone 3"); break;
case 404: printf(" California--Zone 4"); break;
case 405: printf(" California--Zone 5"); break;
case 406: printf(" California--Zone 6"); break;
case 407: printf(" California--Zone 7"); break;
case 501: printf(" Colorado--North"); break;
case 502: printf(" Colorado--Central"); break;
case 503: printf(" Colorado--South"); break;
case 600: printf(" Connecticut"); break;
case 700: printf(" Delaware"); break;
case 901: printf(" Florida--East"); break;
case 902: printf(" Florida--West"); break;
case 903: printf(" Florida--North"); break;
case 1001: printf(" Georgia--East"); break;
case 1002: printf(" Georgia--West"); break;
case 1101: printf(" Idaho--East"); break;
case 1102: printf(" Idaho--Central"); break;
case 1103: printf(" Idaho--West"); break;
case 1201: printf(" Illinois--East"); break;
case 1202: printf(" Illinois--West"); break;
case 1301: printf(" Indiana--East"); break;
case 1302: printf(" Indiana--West"); break;
case 1401: printf(" Iowa--North "); break;
case 1402: printf(" Iowa--South"); break;
case 1501: printf(" Kansas--North"); break;
case 1502: printf(" Kansas--South"); break;
case 1601: printf(" Kentucky--North"); break;
case 1602: printf(" Kentucky--South"); break;
case 1701: printf(" Louisiana--North"); break;
case 1702: printf(" Louisiana--South"); break;
case 1703: printf(" Louisiana--Offshore"); break;
case 1801: printf(" Maine--East "); break;
case 1802: printf(" Maine--West"); break;
case 1900: printf(" Maryland & District of Columbia"); break;
case 2001: printf(" Massachusetts--Mainland"); break;
case 2002: printf(" Massachusetts--Island"); break;
case 2101: printf(" Michigan--East (TM)"); break;
case 2102: printf(" Michigan--Central (TM)"); break;
case 2103: printf(" Michigan--West (TM)"); break;
case 2111: printf(" Michigan--North (Lambert)"); break;
case 2112: printf(" Michigan--Central (Lambert)"); break;
case 2113: printf(" Michigan--South (Lambert)"); break;
case 2201: printf(" Minnesota--North"); break;
case 2202: printf(" Minnesota--Central"); break;
case 2203: printf(" Minnesota--South"); break;
case 2301: printf(" Mississippi--East"); break;
case 2302: printf(" Mississippi--West"); break;
case 2401: printf(" Missouri--East"); break;
case 2402: printf(" Missouri--Central"); break;
case 2403: printf(" Missouri--West"); break;
case 2501: printf(" Montana--North"); break;
case 2502: printf(" Montana--Central"); break;
case 2503: printf(" Montana--South"); break;
case 2601: printf(" Nebraska--North"); break;
case 2602: printf(" Nebraska--South"); break;
case 2701: printf(" Nevada--East"); break;
case 2702: printf(" Nevada--Central"); break;
case 2703: printf(" Nevada--West "); break;
case 2800: printf(" New Hampshire"); break;
case 2900: printf(" New Jersey "); break;
case 3001: printf(" New Mexico--East"); break;
case 3002: printf(" New Mexico--Central"); break;
case 3003: printf(" New Mexico--West"); break;
case 3101: printf(" New York--East"); break;
case 3102: printf(" New York--Central"); break;
case 3103: printf(" New York--West"); break;
case 3104: printf(" New York--Long Island"); break;
case 3200: printf(" North Carolina"); break;
case 3301: printf(" North Dakota--North"); break;
case 3302: printf(" North Dakota--South"); break;
case 3401: printf(" Ohio--North"); break;
case 3402: printf(" Ohio--South"); break;
case 3501: printf(" Oklahoma--North"); break;
case 3502: printf(" Oklahoma--South"); break;
case 3601: printf(" Oregon--North"); break;
case 3602: printf(" Oregon--South"); break;
case 3701: printf(" Pennsylvania--North"); break;
case 3702: printf(" Pennsylvania--South"); break;
case 3800: printf(" Rhode Island "); break;
case 3901: printf(" South Carolina--North"); break;
case 3902: printf(" South Carolina--South"); break;
case 4001: printf(" South Dakota--North"); break;
case 4002: printf(" South Dakota--South"); break;
case 4100: printf(" Tennessee"); break;
case 4201: printf(" Texas--North"); break;
case 4202: printf(" Texas--North Central"); break;
case 4203: printf(" Texas--Central"); break;
case 4204: printf(" Texas--South"); break;
case 4205: printf(" Texas--South Central"); break;
case 4301: printf(" Utah--North"); break;
case 4302: printf(" Utah--Central"); break;
case 4303: printf(" Utah--South"); break;
case 4400: printf(" Vermont"); break;
case 4501: printf(" Virginia--North"); break;
case 4502: printf(" Virginia--South"); break;
case 4601: printf(" Washington--North"); break;
case 4602: printf(" Washington--South"); break;
case 4701: printf(" West Virginia--North"); break;
case 4702: printf(" West Virginia--South"); break;
case 4801: printf(" Wisconsin--North"); break;
case 4802: printf(" Wisconsin--Central"); break;
case 4803: printf(" Wisconsin--South"); break;
case 4901: printf(" Wyoming--East"); break;
case 4902: printf(" Wyoming--East Central"); break;
case 4903: printf(" Wyoming--West Central"); break;
case 4904: printf(" Wyoming--West"); break;
case 5001: printf(" Alaska--Zone 1"); break;
case 5002: printf(" Alaska--Zone 2"); break;
case 5003: printf(" Alaska--Zone 3"); break;
case 5004: printf(" Alaska--Zone 4"); break;
case 5005: printf(" Alaska--Zone 5"); break;
case 5006: printf(" Alaska--Zone 6"); break;
case 5007: printf(" Alaska--Zone 7"); break;
case 5008: printf(" Alaska--Zone 8"); break;
case 5009: printf(" Alaska--Zone 9"); break;
case 5010: printf(" Alaska--Zone 10"); break;
case 5101: printf(" Hawaii--Zone 1"); break;
case 5102: printf(" Hawaii--Zone 2"); break;
case 5103: printf(" Hawaii--Zone 3"); break;
case 5104: printf(" Hawaii--Zone 4"); break;
case 5105: printf(" Hawaii--Zone 5"); break;
case 5201: 
   printf(" Puerto Rico & Virgin Islands--St. John, St. Thomas"); break;
case 5202: printf(" Virgin Islands--St. Croix"); break;
case 5300: printf(" American Samoa"); break;
case 5400: printf(" Guam"); break;
}
return;
}
