/*==============================================================================
Filename:       FAparse.c
 
Description:    Contains the functions common to ESAparse.c, NASDAparse.c, and
                ADEOSparse.c.  Performs ascii extraction.
 
External Functions:
 
Static Functions:
 
External Variables Defined:
 
File Scope Static Variables:
 
Notes:
 
==============================================================================*/

/*  Copyright (c)1995, California Institute of Technology. 
    U.S. Government Sponsorship acknowledged.  */
 

#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absloute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <errno.h>          /* for errno definitions...             */
#include <stdlib.h> 	    /* for getopt		    	    */


#include "GENconversions.h"
#include "ESAconversions.h"
#include "FAparse.h"
#include "ESA.h"
#include "PMF.h"
#include "faifdefs.h"
#include "syslog.h"

#define  FA_ASCII_REC_PROCESS_OK		 0
#define  FA_ASCII_REC_INGESTION_OK		 0




/*==============================================================================
Function:	utc2asftime
Description:	Converts ESA's UTC time to ASF-format time
Parameters:     int  utcday     days since 1950
                int  utcms      milliseconds in that day
                char *asftime   converted time yyyy:doy:hh:mm:ss.ccc
Returns:	void
Creator:	Rodney Hoffman
Creation Date:	Aug. '95
Notes:		from ACS: [bld.acs.load.fadirmon]ESA2ASFTIME.c
==============================================================================*/

void utc2asftime(int utcday, int utcms, char *asftime)
{
	int	year = 1950;
	int	day, hour, min;
	float	sec;
	double	leapyear;
	double	dyear, four=4.00;
	int	i;


	/* It appears we are 1 day off from ESA, sooooooooooo lets add one! */
	utcday++;

	while (utcday > 365) {
		dyear = year ;
		leapyear = fmod (dyear, four);
		if ( leapyear > 0.00)
		{
			utcday = utcday - 365;
			year++;
		}
		else if ( utcday > 366)
		{
			utcday = utcday - 366;
			year++;
		}
		else break ;
	}

	day = utcday;

	hour = utcms / 3600000;
	utcms = utcms - (hour * 3600000);
	min = utcms / 60000;
	utcms = utcms - (min * 60000);
	sec = utcms / 1000.00;

	sprintf (asftime, "%4d:%3d:%2d:%2d:%6.3f\0",
		year, day, hour, min, sec);

	for (i=5; i<21; i++)
		if ((asftime[i] != ':') && (asftime[i] != '.') &&
		     ((asftime[i] < '0') || (asftime[i] > '9')))
			asftime[i] = '0';

	return;
}




/*==============================================================================
Function:	rev4bytes
Description:	Reverse the four bytes starting at a given address
                (Needed to interpret VAX-style numbers in binary FA files)
Parameters:     char  *loc     starting address
Returns:	void
Creator:	Rodney Hoffman
Creation Date:	Aug. '95
Notes:		
==============================================================================*/

void rev4bytes (char *loc)
{
  char tempbytes[4];
  int ix;

  for (ix = 0; ix <  4; ix++)  tempbytes[ix] = *(loc+ix);
  for (ix = 3; ix >= 0; ix--)  { *loc = tempbytes[ix]; loc++; }
}




/*==============================================================================
Function:	begin_stvfile
Description:	Open a state vector translation file and write the common
                header.
Parameters:     char  *subsystem   recipient subsystem
                char  *satellite   E1,E2,J1, or A1
                char  *precision   ephemeris precision 
                                     ("PREDICTED" or "RESTITUTED")
                char  *filename    of the output file
                char  *outdir      directory path for output file
                FILE  *fout        of the output file
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Aug. '95
Notes:		adapted from ACS: [bld.acs.lib.stvec]MKSTVFILE.QC
==============================================================================*/


int begin_stvfile (char *subsystem, char *satellite, char *precision, 
		   char *filename, char *outdir, FILE **fout)
{
#define msgheadsize 49

	char		currtime[22];
	char		msgheader[50];
	char		meta[36];
	char		logmsg[MAX_SYSLOG_MSGLEN+1];
	char            fullpathname[100];

        sprintf(fullpathname,"%s/%s", outdir, filename);
	if ((*fout = fopen (fullpathname, "w")) == NULL) {
		sprintf (logmsg, 
		  "WARNING: Unable to open output file %s.  Exiting.\n",
		  fullpathname);
		syslog (LOG_ERR, logmsg);
		return (ERROR);
	}

	/*  Get the current time to put in the common message header */
	tc_systime2asf(currtime);
	currtime[4] = ' ';

	/*  Build the common message header  */
	memset (msgheader, ' ', 49);
	strncpy (msgheader, currtime, 21);
	msgheader[4] = ' ';

	strcpy (&msgheader[22], "SV     FAI                    ");
	msgheader[50] = '\0';
	strncpy (&msgheader[25], subsystem, 3);

	/*  Make  state vector meta record */
	memset (meta, ' ', 34);
	strncpy (meta, satellite, 2);
	meta[0] = toupper(meta[0]);
	strncpy (&meta[3], precision, strlen(precision));
	strcpy (&meta[14], "TRUE EQUATORIAL     \0");

	fprintf (*fout, "%s\n", msgheader);  /* common header */
	fprintf (*fout, "%s\n", meta);   /* stvec meta header */
	
	return OK;
}




/*=============================================================================
Function:       get_ascii_and_convert
Description:    Gets a string identified by offset and length in ascii_buffer
                and then converts the string using specified conversion.
Parameters:     (see function declaration below for definition)
Returns:        *int  conversion_status:  status of the coversion used on the
                                          string (FALSE = error and TRUE =
                                          success)
Creator:        
Creation Date:  
Notes:
=============================================================================*/
int  get_ascii_and_convert(
	char		*ascii_buffer,
	int			offset,
	int			length,
	int 		(conversion()), 
	EQUIV_TABLE *conversion_table,
        void 		*dest)
{
	char		string[512];
	int		conversion_status;

	strncpy (string, &ascii_buffer[offset], length);
 	string[length] = NULL;

	conversion_status = conversion(conversion_table, string, dest);
    return(conversion_status);
}             




/*=============================================================================
Function:       fa_ascii_rec_ingestion
Description:    Opens up file for ascii read and loops through ascii_file array
                and calls get_ascii_and_convert
Parameters:     (see function declaration below for definition)
Returns:        0   FA_ASCII_REC_INGESTION_OK
                1   FA_ASCII_FILE_UNABLE_TO_OPEN
                -1  any other problem
Creator:        Siu/Yurchuk/Cooper
Creation Date:  July '95
Notes:
=============================================================================*/
int fa_ascii_rec_ingestion(
			   char		*file_name,
			   VALUE_DEFS       ascii_file[],
			   int          header,
			   int          record,
			   FILE         **file_ptr
			   )
{
  /* declarations         */
  int             i, j ;
  int             return_code ;
  int		  testcount ;
  
  char		  ascii_buffer[651] ;
  char            number1[6] ;
  char            number2[6] ;

  extern ODL_Catalog_Metadata         PMF_struct ;
  
  /*
  -- Open the ASCII file
  */
  *file_ptr = fopen(file_name,"r");
  if (!*file_ptr)
    return (FA_ASCII_FILE_UNABLE_TO_OPEN);
  
  /*
  -- Read the ASCII header
  */
  fgets (ascii_buffer, header, *file_ptr);
  
  i = 0;
  while(ascii_file[i].source_code)
    {
      if (!get_ascii_and_convert(
			    ascii_buffer,
			    ascii_file[i].offset,
			    ascii_file[i].length,
			    ascii_file[i].conversion,
			    ascii_file[i].equiv_table,
			    ascii_file[i].destination.pointer))
         return (-1);
      i++; 
    }
  
  return (FA_ASCII_REC_INGESTION_OK);
  
}/* end of fa_ascii_rec_ingestion() */

