/******************************************************************************
**
** File:        ims_timeConv.h
**
** Function:    Time conversion structures.
**
** Date:        8/9/90
**
** Modified:    5/8/95 - S. Hardman - R1B
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              5/16/95 - D. Crichton - R1B
**              Added function prototypes for type checking. Added ASF   
**              format enumerated types.
**
**              5/25/95 - S. Hardman - R1B
**              Redefined the time types for parsing.
**
**              6/23/95 - D. Crichton - R1B
**              Add new functions to get current date and format for CSA.
**
**              7/10/95 - D. Crichton - R1B
**              Add new function to format date for TTDL format.
**
**              8/30/95 - J. Wang - R1B
**              Add new function to format date for V0 format.
**
**              5/17/96 - S. Hardman - R1B'
**              Modified IMS_LEAP_YEAR() macro.
**
******************************************************************************/

#ifndef _IMS_TIMECONV_H
#define _IMS_TIMECONV_H

static char *sccsTimeConv = "@(#)ims_timeConv.h	5.3  04/16/97";

/*
** Macro to determine if a given year is a leap year.
** The year 1000 does not get determined correctly assuming
** that cal(1) is correct.
*/
#define	IMS_LEAP_YEAR(x) \
(x%4 == 0 && x%100 != 0 || x%400 == 0) ? IMS_TRUE : IMS_FALSE

/*
** Define the types of time for parsing.
*/
enum ims_timeTypes
{
	IMS_DOY,
	IMS_YMD
};

/*
** This structure holds the unsigned integer values for parts of a
** date. Used for holding date/time values that are being converted
** to another format.
*/
typedef struct ims_numericDate
{
	unsigned int year;
	unsigned int doy;
	unsigned int month;
	unsigned int day;
	unsigned int hours;
	unsigned int minutes;
	unsigned int seconds;
	unsigned int msecs;
} IMS_NUMERIC_DATE;

/*
** Function prototypes for the ims_timeConv.c module.
*/
int ims_timeToNumericDate (IMS_MSG_STRUCT *, char *,
	IMS_NUMERIC_DATE *);
void ims_numericDateToDBMSA (IMS_NUMERIC_DATE *, char *);
void ims_numericDateToIMSA  (IMS_NUMERIC_DATE *, char *);
void ims_numericDateToASFA  (IMS_NUMERIC_DATE *, char *);
void ims_numericDateToACSA  (IMS_NUMERIC_DATE *, char *);
void ims_numericDateToCSAA  (IMS_NUMERIC_DATE *, char *);
void ims_numericDateToTTDLA (IMS_NUMERIC_DATE *, char *);
void ims_numericDateToV0A   (IMS_NUMERIC_DATE *, char *);
void ims_numericDateToESAA  (IMS_NUMERIC_DATE *, char *);
void ims_numericDateToESAI  (IMS_NUMERIC_DATE *, int *, int *);
void ims_numericDateToGMTA  (IMS_NUMERIC_DATE *, char *);
int ims_getCurrentDate (IMS_MSG_STRUCT *, IMS_NUMERIC_DATE *);
int ims_numericDateDiff (IMS_MSG_STRUCT *, char *, char *, int *, int *);
int ims_numericDateDiff2 (IMS_MSG_STRUCT *, char *, char *, int *, int *, int *);
void ims_dateItoDef(int, int, char *);

#endif	/* !_IMS_TIMECONV_H */
