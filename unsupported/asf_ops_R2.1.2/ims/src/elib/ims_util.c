static char *sccs = "@(#)ims_util.c	5.2  05/09/97";
/******************************************************************************
**
** File:        ims_util.c
**
** Function:    Miscellaneous utility routines.
**
** Date:        10/15/89
**
** Modified:    July 1991 - H. Sayah
**              Added cdb_isInteger() and cdb_isReal() routines.
**
**              1/27/93 - S. Hardman - V18.1
**              CR 3623 - Added cdb_removeQuotes() and cdb_isNullStr().
**
**              11/8/93 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Removed cdb_closeFile(), and cdb_openFile() because they were
**              not called anywhere. Added the include files string.h,
**              sys/unistd.h, and stdlib.h. Removed the include file sys/file.h
**              because sys/unistd.h had what we needed. Also removed search.h.
**              
**              8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              7/26/95 - S. Hardman - R1B
**              Added D. Pass's functions ims_strIndex() and ims_ltoa().
**
**              9/25/95 - S. Hardman - R1B'
**              Added the ims_extractPath() and ims_isBlankString() functions.
**				
**              3/12/96 - D. Crichton - R1B'
**              Added ims_nano2msecs() function.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <fcntl.h>

#include <ims_const.h>
#include <ims_msg.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_util.h.
** They are listed here for reference.
**
**	char *ims_truncStr (char *);
**	char *ims_trim (char []);
**	char *ims_saveStr (char *);
**	char *ims_timeStamp (void);
**	char *ims_extractFileName (char *);
**	int ims_extractPath (char *, char *);
**	void ims_concatFilePath (char *, char *, char *);
**	int ims_isInteger (char *);
**	int ims_isReal (char *);
**	void ims_formatQuotedString (char *, char *);
**	char *ims_removeQuotes (char *);
**  int ims_isNullStr (char *);
**	int ims_bCmp (unsigned char *, unsigned char *, int);
**	char *ims_toUpper (char *);
**	char *ims_toLower (char *);
**	long ims_strIndex (char [], char []);
**	char *ims_ltoa (long);
**	int ims_isBlankString (char *);
**	int ims_nano2msecs (long, long *);
*/

/******************************************************************************
**
** ims_truncStr ()
**
** Truncate strings by removing trailing and leading space characters.
** Return "" when string is NULL.
**
******************************************************************************/

char *ims_truncStr (
	char *str)
{
	char *ptr;

	if (str == (char *) NULL)
	{
		return (""); 
	}

	if (*str == '\0')
	{
		return (str);
	}

	/*
	** Take out the trailing blank characters.
	*/
	ptr = str + strlen (str) - 1;

	while ((ptr >= str) && isspace (*ptr))
	{
		*ptr = '\0';
		ptr--;
	}

	/*
	** Take out the leading blank characters.
	*/
	while (isspace (*str))
	{
		ptr = str;
		while (*(ptr+1) != '\0')
		{
			*ptr = *(ptr+1);
			ptr++;
		}
		*ptr = '\0';
	}

	return (str);
}

/******************************************************************************
**
** ims_trim ()
**
** Trim trailing blanks of a string.
**
******************************************************************************/

char *ims_trim (
	char str[])
{
	register int i;

	if (str == (char *) NULL)
	{
		return (str);
	}

	for (i = strlen (str) - 1; str[i] == ' '; i--)
		;

	str[i+1] = '\0';

	return (str);
}

/******************************************************************************
**
** ims_saveStr ()
**
******************************************************************************/

char *ims_saveStr (
	char *str)
{
	char *ptr;

	if (str == (char *) NULL)
	{
		return ((char *) NULL);
	}

	if ((ptr = (char *) malloc (strlen (str) + 1)) == (char *) NULL)
	{
		return ((char *) NULL);
	}

	(void) strcpy (ptr, str);

	return (ptr);
}

/******************************************************************************
**
** ims_timeStamp ()
**
** Produce time stamp string suitable for use as file name suffix.
** The time string is stored in new memory allocation.
**
******************************************************************************/

char *ims_timeStamp (void)
{
	struct tm *s_time;
	time_t clock;
	char ascTime[80];

	clock = time ((time_t *) 0); 	
	s_time = localtime (&clock);

	(void) sprintf (ascTime, "%02d%02d%02d%02d%02d",
		s_time->tm_mon+1,
		s_time->tm_mday, s_time->tm_hour,
		s_time->tm_min, s_time->tm_sec);

	return (ims_saveStr (ascTime));
}

/******************************************************************************
**
** ims_extractFileName ()
**
** Extract file name from full path name.
**
******************************************************************************/

char *ims_extractFileName (
	char *fullPath)
{
	char *fileName = "";
	char *ptr;

	if (fullPath == (char *) NULL)
	{
		return (fileName);
	}

	if ((ptr = strrchr (fullPath, '/')) == (char *) NULL)
	{
		return (fullPath);
	}
	else
	{
		return (ptr + 1);
	}
}

/******************************************************************************
**
** ims_extractPath ()
**
** Extract the path from a file specification and place it
** in the provided buffer.
**
******************************************************************************/

int ims_extractPath (
	char *fileSpec,
	char *pathPtr)
{
	char *endPtr;
 
	if (fileSpec == (char *) NULL)
	{
		return (IMS_ERROR);
	}

	(void) strcpy (pathPtr, fileSpec);

	if ((endPtr = strrchr (pathPtr, '/')) == (char *) NULL)
	{
		(void) strcpy (pathPtr, ".");
	}
	else
	{
		pathPtr[abs(pathPtr-endPtr)] = '\0';
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_concatFilePath ()
**
** Concatenate file name to its full path.
** Eliminate any occurrence of the '/' character at the end of the path then
** concatenate '/' along with file name to path 
**
******************************************************************************/

void ims_concatFilePath (
	char *fullPath,
	char *path,
	char *name)
{
	register char *c;

	if (strlen (path) == 0)
	{
		return;
	}
	else
	{
		/* Set c to point to the last character of path. */
		c = path + (strlen (path) - 1);

		while ((*c == '/') && (c > path))
		{
			*c = '\0';
			c--;
		}

		/*
		** Check the case where the path is the root directory, "/"
		*/
		if ((c == path) && (*c == '/'))
		{
			*c = '\0';
		}
	}

	(void) sprintf (fullPath, "%s/%s", path, name);

	return;
}

/******************************************************************************
**
** ims_isInteger ()
**
** Is the input string an integer number.
**
******************************************************************************/

int ims_isInteger (
	char *str)
{
	char *cPtr;
	int digitFlag;
	int spaceFlag;
	int signFlag;

	cPtr = str;
	digitFlag = spaceFlag = signFlag = 0;

	while (*cPtr != '\0')
	{
		if (isspace (*cPtr)) 
		{
			if (!digitFlag && !signFlag)
			{
				cPtr++;
			}
			else
			{
				cPtr++;
				spaceFlag = 1;
			}
		}
		else if (isdigit (*cPtr))
		{
			if (spaceFlag)
			{
				return (0);
			}

			cPtr++;
			digitFlag = signFlag = 1;
		}
		else
		{
			if (((*cPtr == '+') || (*cPtr == '-')) && 
				(!digitFlag) && (!signFlag) && (!spaceFlag))
			{
				cPtr++;
				signFlag = 1;
			}
			else
			{
				return (0);
			}
		}
	}

	if (!digitFlag)
	{
		return (0);
	}

	return (1);
}

/******************************************************************************
**
** ims_isReal ()
**
** Is the input string a real number.
** 
** Valid real number syntax is as following:
** +100, -100, 100, 100.0, .001, +.001, -.0002,
** +12.22e+12, +12.22E+12, -12.22e+12, -12.22e-12
** .002E12, .002E-12, .002E+12, .002e12
**
******************************************************************************/

int ims_isReal (
	char *str)
{
	char *cPtr;
	int digitFlag;
	int spaceFlag;
	int dotFlag;
	int expFlag;
	int sign1Flag;
	int sign2Flag;

	cPtr = str;
	digitFlag = spaceFlag = dotFlag = expFlag = sign1Flag = sign2Flag  = 0;

	while (*cPtr != '\0')
	{
		if (isspace (*cPtr))
		{
			if (!digitFlag && !sign1Flag)
			{
				cPtr++;
			}
			else
			{
				cPtr++;
				spaceFlag = 1;
			}
		}
		else if (isdigit (*cPtr))
		{
			if (spaceFlag)
			{
				return (0);
			}

			cPtr++;
			digitFlag  = sign1Flag = 1;

			if (expFlag)
			{
				sign2Flag = 1;
			}
		}
		else
		{
			if (spaceFlag)
			{
				return (0);
			}

			if (((*cPtr == '+') || (*cPtr == '-')) && 
				(!digitFlag) && (!dotFlag) && (!sign1Flag))
			{
				cPtr++;
				sign1Flag = 1;
			}
			else if ((*cPtr == '.') && (!dotFlag))
			{
				cPtr ++;
				dotFlag = 1;
			}
			else if (((*cPtr == 'E') || (*cPtr == 'e')) && 
				(!expFlag) && (digitFlag))
			{
				cPtr++;
				expFlag = 1;
				digitFlag = 0;
			}
			else if (((*cPtr == '+') || (*cPtr == '-')) 
				&& (expFlag) && (!sign2Flag))
			{
				cPtr++;
				sign2Flag = 1;
			}
			else
			{
				return (0);
			}
		}
	}

	if (!digitFlag)
	{
		return (0);
	}

	return (1);
}

/******************************************************************************
** 
** ims_formatQuotedString ()
**
** This routine searches the keyword value character string for any "'"
** characters and places a "'" character infront of it.
**
******************************************************************************/

void ims_formatQuotedString (
	char *inValue,
	char *outValue)
{
	char *in;
	char *out;

	in = inValue;
	out = outValue;

	for (in = inValue; *in != '\0'; in++)
	{
		*out = *in;
		if (*in == 0x27)
		{
			out++;
			*out = 0x27;
		}
		out++;
	}

	*out = '\0';
}

/******************************************************************************
**
** ims_removeQuotes ()
**
** This function looks for a single or double quote in the first
** position of the string and then effectively removes that
** position and the last position from the string.
**
** CR 3623 - This function was added to allow the implementation of this
** change request.
**
******************************************************************************/

char *ims_removeQuotes (
	char *str)
{
	int length;
	char *p;

	length = strlen (str);

	/*
	** Check for single or double quotes and nix 'em.
	*/
	if (str[0] == '"' || str[0] == 0x27)
	{
		str[length - 1] = '\0';
		p = str;

		/* Shift the string to the left. */
		for (; *p != '\0'; p++)
		{
			*p = *(p+1);
		}
	}

	return (str);
}

/******************************************************************************
**
** ims_isNullStr ()
**
** This function determines whether the input string is a
** recognized NULL value.  The following strings
** are recognized NULL values:
**
**		NULL
**		UNK
**		N/A
**
** CR 3623 - This function was added to allow the implementation of this
** change request.
**
******************************************************************************/

int ims_isNullStr (
	char *str)
{
	char nullString1[5];
	char nullString2[5];
	char nullString3[5];

	(void) strcpy (nullString1, "NULL");
	(void) strcpy (nullString2, "UNK");
	(void) strcpy (nullString3, "N/A");

	if (strcmp (str, nullString1) != 0)
	{
		if (strcmp (str, nullString2) != 0)
		{
			if (strcmp (str, nullString3) != 0)
			{
				return (0);
			}
		}
	}

	return (1);
}

/******************************************************************************
**
** ims_bCmp () 
**
** This function is used to compare two binary values.
**
******************************************************************************/

int ims_bCmp (
	unsigned char *t1,
	unsigned char *t2,
	int length)
{
	register int i;

	/*
	** Compare values byte by byte.
	*/
	for (i = 0; i < length; i++)
	{
		if (t1[i] != t2[i])
		{
			return (t1[i] - t2[i]);
		}
	}

	/*
	** Values are equal.
	*/
	return (0);
}

/******************************************************************************
**
** ims_toUpper ()
**
** Convert the string to upper case characters.
**
******************************************************************************/

char *ims_toUpper (
	char *str)
{
	char *cPtr;

	if (str == (char *) NULL)
	{
		return (str);
	}

	cPtr = str;
	while (*cPtr != '\0')
	{
		if (islower (*cPtr))
		{
			*cPtr = toupper (*cPtr);
		}
		cPtr++;
	}

	return (str);
}

/******************************************************************************
**
** ims_toLower ()
**
** Convert the string to lower case characters.
**
******************************************************************************/

char *ims_toLower (
	char *str)
{
	char *cPtr;

	if (str == (char *) NULL)
	{
		return (str);
	}

	cPtr = str;
	while (*cPtr != '\0')
	{
		if (isupper (*cPtr))
		{
			*cPtr = tolower (*cPtr);
		}
		cPtr++;
	}

	return (str);
}

/******************************************************************************
**
** ims_strIndex ()
**
** This is a utility which gives the index of str2 within str1.
** If no match, returns -1.
**
******************************************************************************/

long ims_strIndex (
	char str1[],
	char str2[])
{

	long   i,j,k;
	short  flag;

	i = (long) strlen (str1);
	j = (long) strlen (str2);

	if (j == 1)
	{
		flag = IMS_TRUE;
	}
	else
	{
		flag = IMS_FALSE;
	}

	if (j > i)
	{
		return (-1); /*  no chance  */
	}

	for (k=0; k <= i-j; k++)
	{
		if (str1[k] == str2[0]) /*  possible match  */
		{
			if (flag == IMS_TRUE)
			{
				return (k);
			}

			if (strncmp (str1+k+1, str2+1, j-1) == 0)
			{
				return (k);
			}
		}
	}

	return (-1);
}

/******************************************************************************
**
** ims_ltoa ()
**
** This utility changes a long integer to a character.
**
******************************************************************************/

char *ims_ltoa (
	long i_in)
{
	static char str_trim[32];

	(void) sprintf (str_trim, "%ld", i_in);
	return (str_trim);
}

/******************************************************************************
**
** ims_isBlankString ()
**
** Is the input string filled with spaces.
**
******************************************************************************/

int ims_isBlankString (
	char *str)
{
	char *cPtr;
	 
	if (str == (char *) NULL)
	{
		return (1);
	}
							
	cPtr = str;
	while (*cPtr != '\0')
	{
		if (isspace (*cPtr))
		{
			*cPtr++;
		}
		else
		{
			return (0);
		}
	}

	return (1);
}

/******************************************************************************
**
** ims_nano2msecs ()
**
** This utility function will convert nanoseconds to milliseconds and
** support rounding.  This function fixes an apparent bug that ODL has
** converting a three digit number into nanoseconds.
**
******************************************************************************/


int ims_nano2msecs (
	long nano_secs)
{
	int msecs;
	float msecs_flt;


	/*
	** Extract the number of seconds from the nano_second.
	*/ 

	while (nano_secs >= (long) 1000000000)
	{
		nano_secs = nano_secs - (long) 1000000000;
		/* Don't bump second.  We loose a millisecond at most in 
		** rounding which is fine currently for our needs...
			*second = *second + 1;
		*/
	}



	msecs = (int) (nano_secs / 1000000);

	msecs_flt = (float) (nano_secs / 1000000.0);

	if (msecs_flt - msecs > 0.499999)
	{
		msecs ++;
	}

	if (msecs > 999)
	{		
		msecs = 999;
		/* Don't round.  Leave at 999
		**
			msecs = msecs - 1000;
			*second = *second + 1;
		*/
	}


	return(msecs);
}
