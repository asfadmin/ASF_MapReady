static char *sccs = "@(#)ims_mediaUtil.c	5.2  08/28/96";
/******************************************************************************
**
** File:        ims_mediaUtil.c
**
** Function:    Utility functions for media stuff.
**
** Author:      David Pass
**
** Date:        4/22/95
**
** Modified:    12/12/95 - S. Hardman - R1B'
**              Added the ims_mediaDesc() and ims_statusDesc() functions.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ims_const.h>

/******************************************************************************
**
** trimr ()
**
** takes all the blanks from the end only of
** the input array.  the returned array may be the same
** as the input array.  input array is not changed.
**
******************************************************************************/

char  *trimr( str1 )
char  str1[];
{
short  i,k;

i = strlen( str1 );
if(  str1[i-1]  !=  ' '  ){
    return  str1 ; /*  not any blanks at the end, so return original
        string  */
}
for( k=i-1 ; k  >=  0  ;  k-- ){
    if(  str1[k]  !=  ' ' ){ /*  found last non-blank */
        str1[k+1] = '\0';
        return  str1;
    }
}
*str1 = '\0';
return  str1 ;
}  /*  trimr  */

/******************************************************************************
*
*  SwapBytes ()
*
*  Module Type: Procedure    Language: C
*
*  $Logfile:   ACS003:[BLD.ACS.LIB.CEOS]SWAPBYTES.C_V  $
*
*  Purpose:
*    This prograam swaps the four bytes of an integer to IBM order.
*
*  Input Parameters:
*
*  Name          Type        Description
*  Ivalue        int         The 4 bytes input value in decimal
*  Ibuff         int         The output 4 bytes word after swap
*
*  Modification History:
*
*  Date:   17 Jul 1990 11:47:06   Revision:   2.0  Author:   DBMAN
*
******************************************************************************/

void  SwapBytes(ivalue,ioutword)
unsigned int   ivalue;
unsigned int  *ioutword;
{
    int    i;
    union wrdbyt{
       int word4;
       char word1[4];
       } intchr;
    unsigned char itemp[4];

    intchr.word4 = ivalue;
    i = 0;
    i++;
    itemp[0] = '\0';
    /* ************************************
     this was done on the vax because its integers are
         different than unix:  the bytes are swaped.
         not done on unix.
    itemp[0] = intchr.word1[3];
    itemp[1] = intchr.word1[2];
    itemp[2] = intchr.word1[1];
    itemp[3] = intchr.word1[0];
    for (i=0; i<=3; i++) intchr.word1[i] = itemp[i];
    **************************** */
    *ioutword = intchr.word4;
    return;
}   /*  swapbytes   */

/******************************************************************************
**
** ims_mediaDesc ()
**
******************************************************************************/

char *ims_mediaDesc (
	int mediaType)
{
	int maxType;

	static char *mediaDesc[] =
	{
		"No Media Type",
		"4-MM",
		"4-MM HD",
		"8-MM",
		"8-MM HD",
		"9-TRACK",
		"9-TRACK HD",
		"DISK",
		"LT-NEG",
		"LT-POS",
		"LT-PRINT",
		"FR-NEG",
		"FR-POS",
		"FR-PRINT",
		"PHOTO-CPR",
		"PHOTO-PTP",
		"PHOTO-NTP",
		"RESERVE 1",
		"RESERVE 2",
		"RESERVE 3",
		"DCRSI",
		"ID-1",
		"HD-96",
		"EMI"
	};

	/*
	** Determine the maximum media type value.
	*/
	maxType = (sizeof (mediaDesc) / sizeof (mediaDesc[0])) - 1;

	/*
	** Return the associated media description.
	*/
	if (mediaType >= 1 && mediaType <= maxType)
	{
		return (mediaDesc[mediaType]);
	}
	else
	{
		return (mediaDesc[0]);
	}
}

/******************************************************************************
**
** ims_statusDesc ()
**
******************************************************************************/

char *ims_statusDesc (
	int statusValue)
{
	int maxStatus;

	static char *statusDesc[] =
	{
		"No Status",
		"Device Available",
		"Device In-use",
		"Device Off-line",
		"Device In-service"
	};

	/*
	** Determine the maximum status value.
	*/
	maxStatus = (sizeof (statusDesc) / sizeof (statusDesc[0])) - 1;

	/*
	** Return the associated status description.
	*/
	if (statusValue >= 1 && statusValue <= maxStatus)
	{
		return (statusDesc[statusValue]);
	}
	else
	{
		return (statusDesc[0]);
	}
}

/******************************************************************************
**
** ims_filmStatusDesc ()
**
******************************************************************************/

char *ims_filmStatusDesc (
	int statusValue)
{
	int maxStatus;

	static char *statusDesc[] =
	{
		"No Status",
		"NEW",
		"IN-FILM",
		"GENERATED",
		"COMPLETE",
		"No Status",
		"No Status",
		"No Status",
		"No Status",
		"No Status",
		"No Status",
		"CANCEL",
		"CANCELLED",
		"ERROR",
		"FAILED",
		"HOLD",
		"No Status",
		"No Status",
		"No Status",
		"No Status",
		"LOCKED"
	};

	/*
	** Determine the maximum status value.
	*/
	maxStatus = (sizeof (statusDesc) / sizeof (statusDesc[0])) - 1;

	/*
	** Return the associated status description.
	*/
	if (statusValue >= 1 && statusValue <= maxStatus)
	{
		return (statusDesc[statusValue]);
	}
	else
	{
		return (statusDesc[0]);
	}
}
