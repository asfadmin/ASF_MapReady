static char *sccs = "@(#)ims_v0Time.c	5.4  12/19/96";
/************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** ims_v0Time.c
**
** Purpose
**		Time conversion between various formats.
**
**	Creator   :   Julie Wang
**
**	Date      :   July 07, 1994
**
** Modifications:
**
**   05/24/96    jwang   time comparison routine added. 
**
**   03/12/96    jwang   call ims_nano2msecs for odl time conversion
**
**   02/20/96    jwang   R1Bprime (preliminary).
**
**   01/22/96    jwang   Changes added to handle DARs
**
**   09/05/95    jwang   time formating modification for DoyToDate
**
**   06/05/95    jwang   Changed all reference of cdb_ or CDB_ to 
**                       ims_ or IMS_
**
**   11/18/94    jwang   Added seasonal search capability.
**
**   10/01/94    jwang   IMS/DADS Release 0.
**
************************************************************************/

/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE

#include <stdio.h>
#include <string.h>
#include <IK_Network.h>
#include <odlinter.h>
#include <ims_query.h>
#include <ims_v0.h>


static int day_table[13]={ 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
static int leapyear_day_table[13] =
                         { 0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

static int convert_time (char *, char*);
/*************************************************************************
**
** v0_time__OdlToDatetime -
**
** purpose: conversion routine to extract date related information from 
**          a message and put them into SYBASE datetime format, i.e.
**          mm-dd-yyyy.  The format mmdd will also be created for internal
**          usage.
**
**************************************************************************/

int v0_time__OdlToDatetime (VALUE date_value, char *str, char *mmdd, 
	char *year)

{

	/* 
	** month conversion 
	*/
	(void) sprintf (str, "%d-", (int)date_value->item.value.date_time.month);

	if ( (int)strlen(str) < 3)
	{
		(void) sprintf (mmdd, "0%d", (int)date_value->item.value.date_time.month);
		mmdd = mmdd + strlen(mmdd);
	}
	else
	{
		(void) sprintf (mmdd, "%d", (int)date_value->item.value.date_time.month);
		mmdd = mmdd + strlen(mmdd);
	}
	str = str + strlen(str);

	/* 
	** day conversion 
	*/
	(void) sprintf (str, "%d-", (int)date_value->item.value.date_time.day);
	if ( (int)strlen(str) < 3)
	{
		(void) sprintf (mmdd, "0%d", (int)date_value->item.value.date_time.day);
		mmdd = mmdd + strlen(mmdd);
	}
	else
	{
		(void) sprintf (mmdd, "%d", (int)date_value->item.value.date_time.day);
		mmdd = mmdd + strlen(mmdd);
	}
	str = str + strlen(str);

	/*
	** year conversion
	*/
	(void) sprintf (str, "%d", (int)date_value->item.value.date_time.year);
	(void) sprintf (year, "%d", (int)date_value->item.value.date_time.year);
	str = str + strlen(str);
	year = year + strlen(year);

	/*
	** check whether there is an abs time component 
	*/
	if ( ((int)date_value->item.value.date_time.hours   != 0) ||
		((int)date_value->item.value.date_time.minutes != 0) ||
		((int)date_value->item.value.date_time.seconds != 0))
	{

		(void) sprintf(str, " %d:", (int)date_value->item.value.date_time.hours);
		str = str + strlen(str);

		(void) sprintf(str, "%d:", (int)date_value->item.value.date_time.minutes);
		str = str + strlen(str);

		(void) sprintf(str, "%d", (int)date_value->item.value.date_time.seconds);
		str = str + strlen(str);

	}
	else
	{
		strcpy (str, " 00:00:00");
		str = str + strlen(str);
	}

	return (IMS_OK);

} /* __OdlToDatetime */
 
/*************************************************************************
**
** v0_time__DatetimeToOdl -
**
** purpose: rearranges a time value from SYBASE datetime format
**          i.e. yyyy.mm.ddThh:mm:ssZ, into an ODL recognizable format,  
**          i.e. yyyy-mm-ddThh:mm:ssZ
**
** function return: IMS_OK    if successful
**                  IMS_FATAL if memory allocation failed
**************************************************************************/
int v0_time__DatetimeToOdl (char *str)
{
	/*
	** Because SYBASE fills in 0 to single digit month and day, the only 
	** difference between the two formats are the delimeter.
	** All we need to do here is to replace them with '-'.
	*/

	/* if the datetime field is not null */
	if ( strcmp(str, "TZ") != 0)
	{
		str[4] = '-';
		str[7] = '-';
	}
	else
	{
		strcpy (str, "\0");
	}		

	return (IMS_OK);

} /* __DatetimeToODL */


/****************************************************************************
*
* v0_time__DoyToDate
*
* Purpose:  To convert julian day-of-year to mm-dd, and mmdd for internal use. 
*
******************************************************************************/

int v0_time__DoyToDate (int doy, int year, char *date, char *mmdd)
{
	int  i;
	int  jd;
 
	/*
	** Initialization
	*/
	i = 0;  /* month */
	jd = doy; /* day */

	if ((year % 4) == 0 )  /* This is a leap year */
	{
		for (i = 1; jd > leapyear_day_table[i]; i++)
			jd -= leapyear_day_table[i];
	} 
	else
	{
		if (doy == 366)
		{
			i = 12;
			jd = 31;
		}	
		else
		{
			for (i = 1; jd > day_table[i]; i++)
				jd -= day_table[i];
		}
	}
  
	(void) sprintf (date, "%02d-", i);
	date = date + strlen (date);
	(void) sprintf (date, "%02d", jd);
	date = date + strlen (date);

	(void) sprintf (mmdd, "%02d", i);
	mmdd = mmdd + strlen (mmdd);
	(void) sprintf (mmdd, "%02d", jd);
	mmdd = mmdd + strlen (mmdd);

	return (IMS_OK);
}

/*************************************************************************
**
** v0_time__OdlToIMSTime -
**
** purpose: convert an ODL date to IMS recognizable date/time ASCII string. 
**          e.g. 1995-01-30T12:30:45.333[Z] --> 1995-030T12:30:45.333[Z]
**
**************************************************************************/

int v0_time__OdlToIMSTime (VALUE date_value, char *str)

{
	char temp_str[IMS_COL10_LEN+1];
	long temp_time;

	(void) sprintf (str, "%d", date_value->item.value.date_time.year);
	str = str + strlen(str);

	temp_str[0] = '\0';
	(void) sprintf (temp_str, "%d", date_value->item.value.date_time.doy);

	if ((int)strlen(temp_str) == 1)
	{
		(void)sprintf (str, "-00%sT", temp_str);
		str = str + strlen(str);
	}
	else if ((int)strlen(temp_str) == 2)
	{
		(void)sprintf (str, "-0%sT", temp_str);
		str = str + strlen(str);
	}
	else if ((int)strlen(temp_str) == 3)
	{
		(void)sprintf (str, "-%sT", temp_str);
		str = str + strlen(str);
	}

	if ( ((int)date_value->item.value.date_time.hours   != 0) ||
		((int)date_value->item.value.date_time.minutes != 0) ||
		((int)date_value->item.value.date_time.seconds != 0) ||
		(date_value->item.value.date_time.nanoseconds != 0))
	{

		if ((int)date_value->item.value.date_time.hours != 0)
		{
			
			temp_str[0] = '\0';
			(void) sprintf (temp_str,"%d",(int)date_value->item.value.date_time.hours);

			if ( (int)strlen(temp_str) == 2)
			{
				(void) sprintf(str, "%s", temp_str);
				str = str + strlen(str);
			}
			else if ( (int)strlen(temp_str) == 1)
			{
				(void) sprintf(str, "0%s", temp_str);
				str = str + strlen(str);
			}

		}
		else
		{
			(void) strcpy(str, "00");
			str = str + strlen(str);
		}

		if ((int)date_value->item.value.date_time.minutes != 0)
		{
			temp_str[0] = '\0';
			(void)sprintf(temp_str,"%d",(int)date_value->item.value.date_time.minutes);

			if ( (int)strlen(temp_str) == 2)
			{
				(void) sprintf(str, ":%s", temp_str);
				str = str + strlen(str);
			}
			else if ( (int)strlen(temp_str) == 1)
			{
				(void) sprintf(str, ":0%s", temp_str);
				str = str + strlen(str);
			}

		}
		else
		{
			(void) strcpy(str, ":00");
			str = str + strlen(str);
		}

		if ((int)date_value->item.value.date_time.seconds != 0)
		{
			temp_str[0] = '\0';
			(void)sprintf(temp_str,"%d",(int)date_value->item.value.date_time.seconds);

			if ( (int)strlen(temp_str) == 2)
			{
				(void) sprintf(str, ":%s", temp_str);
				str = str + strlen(str);
			}
			else if ( (int)strlen(temp_str) == 1)
			{
				(void) sprintf(str, ":0%s", temp_str);
				str = str + strlen(str);
			}

		}
		else
		{
			(void) strcpy(str, ":00");
			str = str + strlen(str);
		}


		if (date_value->item.value.date_time.nanoseconds != 0)
		{
			temp_time = 0;
			temp_time = ims_nano2msecs 
				(date_value->item.value.date_time.nanoseconds);

			temp_str[0] = '\0';
			(void)sprintf(temp_str,"%d",temp_time);

			if ( (int)strlen(temp_str) == 3)
			{
				(void) sprintf(str, ".%s", temp_str);
				str = str + strlen(str);
			}
			else if ( (int)strlen(temp_str) == 2)
			{
				(void) sprintf(str, ".0%s", temp_str);
				str = str + strlen(str);
			}
			else if ( (int)strlen(temp_str) == 1)
			{
				(void) sprintf(str, ".00%s", temp_str);
				str = str + strlen(str);
			}

		}
		else
		{
			(void) strcpy(str, ".000");
			str = str + strlen(str);
		}

	}
	else
	{
		strcpy (str, "00:00:00.000");
		str = str + strlen(str);
	}

	strcpy (str, "\0");
	str = str + strlen(str);


} /* end of v0_time__OdlToIMSTime */

/*************************************************************************
**
** v0_time__compareIMSA -
**
** purpose: compare two time. Both have to be in IMSA formatted 
**          (e.g. yyyy-doyThh:mm:ss.fff[Z]) time.
**
** return:  0 if time1 >= time2
**          1 if time1 <  time2
**************************************************************************/

int v0_time__compareIMSA (char *time1, char *time2)
{
	int error;
	char in_time1[IMS_COL30_LEN], in_time2[IMS_COL30_LEN];
	char in_time1_l[IMS_COL30_LEN], in_time2_l[IMS_COL30_LEN];
	char temp1[IMS_COL30_LEN], temp2[IMS_COL30_LEN];

	error = 0;

	in_time1[0] = in_time2[0] = '\0';
	strcpy (in_time1, time1);
	strcpy (in_time2, time2);

	in_time1_l[0] = in_time2_l[0] = '\0';
	(void) convert_time (in_time1, in_time1_l);
	(void) convert_time (in_time2, in_time2_l);

	temp1[0] = temp2[0] = '\0';
	(void) memcpy (&temp1, in_time1_l, 7);
	(void) memcpy (&temp2, in_time2_l, 7);
	temp1[7] = '\0';
	temp2[7] = '\0';

	if ( atoi(temp1) > atoi(temp2) )
	{
		error = 1;
	}
	else if ( atoi(temp1) == atoi(temp2))
	{
		temp1[0] = temp2[0] = '\0';
		(void) memcpy (&temp1, &in_time1_l[7], 9);
		(void) memcpy (&temp2, &in_time2_l[7], 9);
		temp1[9] = '\0';
		temp2[9] = '\0';

		if ( atoi(temp1) > atoi(temp2))
		{
			error = 1;
		}
	}

	return (error);
} /* end of v0_time__compareIMSA */

static int convert_time (char *in_str, char* out_str)
{
	char time[IMS_COL30_LEN+1];
	char in[IMS_COL30_LEN+1];
	char *p, *t;

	time[0] = '\0';
	t = time;
	in[0] = '\0';
	strcpy(in, in_str);

	p = strtok(in, "TZ:.-");

	strcpy (t, p);
	t = t + strlen(t);

	while ( p = strtok('\0', "TZ:.-"))
	{
		strcpy (t, p);
		t = t + strlen(t);
	}

	strcpy (out_str, time);

	return (IMS_OK);

}/* end of convert_time */
