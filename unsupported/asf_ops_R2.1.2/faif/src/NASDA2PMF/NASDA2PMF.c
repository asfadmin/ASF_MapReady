/*************************************************************************
 * Copyright (c)1995 California Institute of Technology.                 *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	NASDA2PMF.c

Description:    Contains the funtions necessary for parsing NASDA 
                files and creating the PMF.

External Functions:
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:
1.  Feb. '96 - R. Hoffman
    Corrected datasets for translated files.
2.  Feb. '96 - R. Hoffman
    Pass curr_rev as a pointer to read_nasstv.
3.  Feb. '96 - R. Hoffman
    Correct pointer de-referencing error in asf2J1rev.
4.  Jan. '97 - R. Hoffman
    Many changes for PR 2351
5.  June '97 - R. Hoffman
    Check for empty portions of ELMF before opening translation files.
6.  June '97 - R. Hoffman
    Reverse linked list output; send ELMF to IMS last, after translated files.
7.  June '97 - R. Hoffman
    Move file renaming to NASDA/NASDAfname.c
==============================================================================*/

static char SccsFile[] = "NASDA2PMF.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "11 Feb 1996";
static char SccsLastChanger[] = "@(#)NASDA2PMF.c	1.2";
static char SccsState[] = "1.2";

#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absolute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <errno.h>          /* for errno definitions...             */
#include <stdlib.h> 	    /* for getopt		    	    */
#include <time.h>

#include "faifdefs.h"
#include "GENconversions.h"
#include "ESAconversions.h"
#include "NASDA.h"
#include "PMF.h"
#include "NASDA2PMF.h"
#include "llist_err.h"
#include "dapps_list.h"
#include "nmalloc.h"

#define  FA_ASCII_REC_PROCESS_OK		 0
#define  FA_ASCII_REC_INGESTION_OK		 0

/* Global variables */
jers_data_rec  jers_rec;
char          *rootpath;
llist         *bar;
int     llist_errno ;

void          *unused_pointer;


FA_filetype filetypes[] =
{        
  {NASDA_ELMF_HSTR, GFTYPE_SVTIMCOR, NASDA_STR, ORIG_STR,  J_SAT, ASF_STR, NO_STR},
  {NASDA_OPLN_HSTR, GFTYPE_FASKED,   NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NASDA_REQA_HSTR, GFTYPE_FARTLTUR, NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NASDA_REQM_HSTR, GFTYPE_FARQSTS,  NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NASDA_MSGC_HSTR, GFTYPE_TAPE_RD,  NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NASDA_MSGN_HSTR, GFTYPE_SAT_STAT, NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NASDA_REQQ_HSTR, GFTYPE_LTUSRREQ, NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NASDA_REQW_HSTR, GFTYPE_STUSRREQ, NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NASDA_REAC_HSTR, GFTYPE_ACQ_REP,  NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NASDA_CATA_HSTR, GFTYPE_CAT_REP,  NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NASDA_MSGM_HSTR, GFTYPE_SHIP_REP, NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NASDA_MSGF_HSTR, GFTYPE_STN_STAT, NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NASDA_MSGE_HSTR, GFTYPE_RFA_RQST, NASDA_STR, ORIG_STR, J_SAT, ASF_STR, NO_STR},
  {NULL,	    NULL,	     NULL,      NULL,     NULL,  NULL,    NULL}
};	


extern char   fa_file_type_temp[];
extern char   file_creation_time[];

/*==============================================================================
Function:	get_gmt_diff
Description:	Return the difference, in seconds, between two GMT times.
Parameters:     GMT * t1, t2
Returns:	t1 - t2 in seconds
Creator:	Rodney Hoffman
Creation Date:	Oct. '95
Notes:		Adapted from akbar::rdisk:[dave]time_uts.c
==============================================================================*/

int get_gmt_diff(t1,t2)
	GMT *t1,*t2;
{
	double ms = 60.0;
	double hs = 60.0 * ms;
	double ds = 24.0 * hs;
	double ys = 365.0 * ds;
	double diff;

	diff = (t1->yr - t2->yr) * ys;
	diff += (t1->day - t2->day) * ds;
	diff += (t1->hr - t2->hr) * hs;
	diff += (t1->min - t2->min) * ms;
	diff += t1->second - t2->second;
    /* adjust for leap year(s) */
	diff += (((t1->yr - 1) / 4) - ((t2->yr - 1) / 4)) * ds;

	return ((int)diff);
}



/*==============================================================================
Function:	asf2J1rev
Description:	Given an asftime and a state vector z-value, and the
                JERS-1 phase information, determines the orbit #.
Parameters:     char      *asftime   yyyy:ddd:hh:mm:ss.sss
                double    z          state vector position z-coordinate
                int       *rev       orbit #
Returns:	0 (OK) or non-zero (ERROR)
Creator:	Rodney Hoffman
Creation Date:	Oct. '95
Notes:		Adapted from ACS: [bld.mps.lib.src]asf2rev.qf
==============================================================================*/

int asf2J1rev(char *asftime, double z, int *rev1)
{

  double	et0, frev;
  int 		rev2;
  char		asft[22];
  double	etp1;   /* phase_start in Julian Days */
  int		next, prev, nrevs;
  double	tnode, etp2, etdif, et;

  *rev1 = rev2 = 0;
  strcpy (asft, asftime);
  et = et0;
  tc_asf2julian(asft, &et);
  tc_asf2julian(jers_rec.pstart, &etp1);

  /* Compute nodal period, tnode */
  tnode = jers_rec.icdays;
  tnode = tnode / jers_rec.icrevs;

  /* OK - et is within this phase */
  etdif = et - etp1;
  /* number of complete revs since first node of phase: */
  nrevs = (etdif / tnode);
  /* rev number of the current nominal rev: */
  *rev1 = (jers_rec.iplastrev - jers_rec.iprevs + 1) + nrevs;
  /* fractional rev since most recent nominal node. */
  frev = (etdif / tnode ) - nrevs;
  /* check to see if the time close to a nominal node: */
  if ((frev > 0.25) && (frev < 0.75) && (*rev1 > 0)) return (0);
  /* the time is close to a nominal node  */
  /* check to see if the time is close to the next nominal node:  */
  if (frev >= 0.75) *rev1 = *rev1 + 1;
  /* now the time is close to nominal rev number rev1.  */
  /* check z-value to see if the state vector is before the ascending node */
  if (z < 0) *rev1 = *rev1 - 1;
  /* now finished, in the normal case.  */

  if (*rev1 > 0) return (0);

  /* Nothing found.   */
      return (4);
}




/*==============================================================================
Function:	read_nastimecorr
Description:	Read one JERS-1 time correction record, translate it to
                ASF format and write it to the output file.
Parameters:     FILE            *fin          input file
                FILE            *fout         output file
                timespan_rec    *minmaxtime   time range
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Oct. '95
Notes:		
==============================================================================*/

   int read_nastimecorr (FILE *fin, FILE *fout, timespan_rec *minmaxtime)
   {

  	int		rev;
  	char		gmttime[22];
  	int		sattime;
  	int		clockcycle;
	char		temp[20];
	char		buffer[MAXRECLEN];
	int		year, month, day, doy, hour, min, sec, ms;
        float           seconds;
	char		time[13];
	double		zpos;
	char		timerec[60];
        GMT             t0, tr;
        int             td, c0;

	static char satellite[3] = "J1";
	
        fgets (buffer, TCRECLEN + 1, fin);

	/* Pull out the preset ground time, ELMF field #2 (bytes 8-28),
           (T0 in IOM 3349-92-131) */
	strncpy (temp, &buffer[8], 2);
	temp[2] = '\0';	
	year = atoi(temp);
	strncpy (temp, &buffer[11], 2);
	temp[2] = '\0';
	month = atoi(temp);
	strncpy(temp, &buffer[14], 2);
	temp[2] = '\0';
	day = atoi(temp);

	if (year > 70) 
		year = 1900 + year;
	else
		year = 2000 + year;

	tc_cal2doy (year, month, day, &doy);

        strncpy (temp, &buffer[17], 2);
        temp[2] = '\0';
        hour = atoi(temp);
        strncpy (temp, &buffer[20], 2);
        temp[2] = '\0';
        min = atoi(temp);
        strncpy (temp, &buffer[23], 2);
        temp[2] = '\0';
        sec = atoi(temp);
        strncpy (temp, &buffer[26], 3);
        temp[3] = '\0';
        ms = atoi(temp);
        seconds = (float)sec + (float)ms/1000.00;

        t0.yr = year;
        t0.day = doy;
        t0.hr = hour;
        t0.min = min;
        t0.second = seconds;

	/* Pull out the registration time, ELMF field #5 (bytes 44-64),
           (Tr in IOM 3349-92-131, which is gmttime), 
           and convert it to ASF time */
	strncpy (temp, &buffer[44], 2);
	temp[2] = '\0';	
	year = atoi(temp);
	strncpy (temp, &buffer[47], 2);
	temp[2] = '\0';
	month = atoi(temp);
	strncpy(temp, &buffer[50], 2);
	temp[2] = '\0';
	day = atoi(temp);

	if (year > 70) 
		year = 1900 + year;
	else
		year = 2000 + year;

	tc_cal2doy (year, month, day, &doy);

	strncpy (time, &buffer[53], 12);
	time[12] = '\0';

        strncpy (temp, &buffer[53], 2);
        temp[2] = '\0';
        hour = atoi(temp);
        strncpy (temp, &buffer[56], 2);
        temp[2] = '\0';
        min = atoi(temp);
        strncpy (temp, &buffer[59], 2);
        temp[2] = '\0';
        sec = atoi(temp);
        strncpy (temp, &buffer[62], 3);
        temp[3] = '\0';
        ms = atoi(temp);
        seconds = (float)sec + (float)ms/1000.00;

        tr.yr = year;
        tr.day = doy;
        tr.hr = hour;
        tr.min = min;
        tr.second = seconds;

	sprintf (gmttime, "%d:%03d:%s", year, doy, time);

	/* determine the rev number from the time */
	/* - set the z position to 0.0 
	     so asf2J1rev will return the nominal rev number */
	zpos = 0.0;

	if (asf2J1rev (gmttime, zpos, &rev) != 0)
	{
	  syslog (LOG_ERR, "ERROR: Error in call to asf2J1rev.  Exiting.\n");
	  return (ERROR);
	}

	/* pull out the preset satellite time,
           ELMF field #3 (bytes 29 - 38) (C0 in IOM 3349-92-131) */
	strncpy (temp, &buffer[29], 10);
	temp[10] = '\0';
        c0 = atoi(temp);

        /* per IOM 3349-92-131: */
        td = get_gmt_diff(&tr, &t0);
        sattime = c0 + (int)td;

	/* pull out the clock cycle */
	strncpy (temp, &buffer[39], 5);
	temp[5] = '\0';
	clockcycle = atoi (temp);

	/* write it out */
	sprintf (timerec, "%-5d %-21s %-11u %-11u ",
		 rev, gmttime, sattime, clockcycle);
	timerec[10] = ' ';
	fprintf (fout, "%s\n", timerec);

	/* check for time range */
	if (strcmp(gmttime, minmaxtime->mintime) < 0)
	{
	  strcpy (minmaxtime->mintime, gmttime);
	  sprintf (minmaxtime->minrev, "%05d\0", rev);
	}
	if (strcmp(gmttime, minmaxtime->maxtime) > 0)
	{
	  strcpy (minmaxtime->maxtime, gmttime);
	  sprintf (minmaxtime->maxrev, "%05d\0", rev);
	}

	return (OK);
   }





/*==============================================================================
Function:	read_nasstv
Description:	Read one JERS-1 state vector record, translate it to ASF
                format, and write it to the stv_str.
Parameters:     FILE           *fin          input file
                char           *stv_str      one translated output line
                char           *vectime     time of this vector
                int            *curr_rev     current rev #
                int            precision     1 (PREDICTED) or 2 (RESTITUTED)
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Oct. '95
Notes:		
==============================================================================*/

int read_nasstv (FILE *fin, char *stv_str, char *vectime, 
		    int *curr_rev, int precision)
{
	char		temp[20];
	char		buffer[MAXRECLEN];
        char            strttime[22];
  	double		xpos, ypos, zpos;
  	double		xvel, yvel, zvel;
  	int		revnum;
	double		mjd, mjd2;
  	char		stv_prec[2];

	static char sat[3] = "J1";

        fgets (buffer, SVRECLEN + 1, fin);	

	/*  Pull out the time and convert it into asf time */
	strncpy (temp, buffer, 14);
	temp[14] = '\0';
	mjd = atof (temp);
        mjd2 = mjd + 2400000.5;
	tc_julian2asf (&mjd2, &strttime);
	strttime[21] = '\0';
        strcpy (vectime, strttime);

	/* Get the z position */
	strncpy (temp, &buffer[40], 13);
	temp[13] = '\0';
	zpos = atof (temp);

	/* Figure out the rev number from the time */
	/* Pass z position so that asf2J1rev can accurately determine rev */
	if (asf2J1rev (strttime, zpos, &revnum) != 0)
	{
	  syslog (LOG_ERR, "ERROR: Error in call to asf2J1rev.  Exiting.\n");
	  return (ERROR);
	}

        /* Keep only one predicted state vector per rev */
	if (revnum == *curr_rev && precision != 2)
		return (OK);

	strncpy (temp, &buffer[14], 13);
	temp[13] = '\0';
	xpos = atof (temp);
	strncpy (temp, &buffer[27], 13);
	temp[13] = '\0';
	ypos = atof (temp);
	strncpy (temp, &buffer[40], 13);
	temp[13] = '\0';
	zpos = atof (temp);
	strncpy (temp, &buffer[53], 10);
	temp[10] = '\0';
	xvel = atof (temp) * 1000.00;
	strncpy (temp, &buffer[63], 10);
	temp[10] = '\0';
	yvel = atof (temp) * 1000.00;
	strncpy (temp, &buffer[73], 10);
	temp[10] = '\0';
	zvel = atof (temp) * 1000.00;

	/* write it out */
        strttime[4] = ' ';
        sprintf (stv_str,
                "%05d %21s %-11.5f %-11.5f %-11.5f %-11.5f %-11.5f %-11.5f \n",
                revnum, strttime, xpos, ypos, zpos, xvel, yvel, zvel);

	*curr_rev = revnum;

	return (OK);
}



/*==============================================================================
Function:	translate_nasda_elmf
Description:	
Parameters:     char                  *infilename   input file
                ODL_Catalog_Metadata  *pmf          for the input file
                char                  *cfname       config file name
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Oct. '95
Notes:		Adapted from ACS: [bld.acs.load.fadirmon]nasdastv.qc
==============================================================================*/

int translate_nasda_elmf (char *infilename, ODL_Catalog_Metadata *pmf,
                          char *cfname)
{

#define maxrecsize 500

int	i, time_corr, pred_sv, rest_sv;
char	buffer[maxrecsize];
char    temp_outfilename[14];
char    oldname[100], newname[100];
char	filetag[4];
char    msgheader[100];
char    currtime[22];
char    dataset[100];
int	p_count;
FILE	*fd;
timespan_rec            minmaxtime;        /* for each translated file */
ODL_Catalog_Metadata    PMF_struct;        /* for each translated file */

  if ((fd = fopen(infilename, "r")) == (FILE *)NULL)
  {
    syslog (LOG_ERR, "ERROR: Error opening file %s.  Exiting.\n", infilename);
    return (ERROR);
  }

  if (get_elmf_info(cfname, fd) != OK) 
  {
    fclose(fd);
    return (ERROR);
  }

  /* Multiple translated files: */

  /*   1.  Time correction elements (one translated file)  */

    if (jers_rec.tc_count > 0)
    {
       /* Start with parent file's PMF; pieces will be modified */
       PMF_struct = *pmf;
       strcpy (PMF_struct.format, ASF_STR);
       strcpy (PMF_struct.gen_file_type, "TIME_CORRELATION");
       PMF_struct.file_name[0] = NULL;

       strcpy (minmaxtime.minrev, "99999");
       strcpy (minmaxtime.maxrev, "00000");
       strcpy (minmaxtime.mintime, "2100:000:00:00:00.000");
       strcpy (minmaxtime.maxtime, "1900:000:00:00:00.000");

       if (translate_tce (fd, &PMF_struct, &minmaxtime) != OK)
       {
	  syslog(LOG_ERR, 
                 "ERROR: Unable to translate time correction elements.\n");
          delete_translated_files();
          fclose(fd);
          return (ERROR);
	}

       sprintf (temp_outfilename, "%s", "J01_00000.TCE");
       sprintf (filetag, "%s", "TCE");
       sprintf (dataset, "%s", NASDA_TCE_HSTR);
       if (add_to_llist(&PMF_struct, &minmaxtime, filetag, dataset, 
                        temp_outfilename) != OK) 
       {
          delete_translated_files();
          fclose(fd);
          return (ERROR);
	}
    }

  /*   2.  Restituted state vectors (one translated file per rev)  */

    if (jers_rec.d1 + jers_rec.d2 > 0)
    {
       /* Start with parent file's PMF; pieces will be modified */
       PMF_struct = *pmf;
       strcpy (PMF_struct.format, ASF_STR);
       strcpy (PMF_struct.gen_file_type, "RESTITUTED_STATE_VECTORS");
       PMF_struct.file_name[0] = NULL;

       if (translate_rsv (fd, &PMF_struct) != OK)
       {
	  syslog(LOG_ERR, 
                 "ERROR: Unable to translate restituted state vectors.\n");
          delete_translated_files();
          fclose(fd);
          return (ERROR);
       }
    }

  /*   3.  Predicted state vectors (one translated file) */

    p_count = jers_rec.d0 + jers_rec.p1 + jers_rec.p2 + jers_rec.p3;
    if (p_count > 0)
    {
       /* Start with parent file's PMF; pieces will be modified */
       PMF_struct = *pmf;
       strcpy (PMF_struct.format, ASF_STR);
       strcpy (PMF_struct.gen_file_type, "PREDICTED_STATE_VECTORS");
       PMF_struct.file_name[0] = NULL;

       strcpy (minmaxtime.minrev, "99999");
       strcpy (minmaxtime.maxrev, "00000");
       strcpy (minmaxtime.mintime, "2100:000:00:00:00.000");
       strcpy (minmaxtime.maxtime, "1900:000:00:00:00.000");

       if (translate_psv (fd, &PMF_struct, &minmaxtime) != OK)
       {
	  syslog(LOG_ERR, 
                 "ERROR: Unable to translate predicted state vectors.\n");
          delete_translated_files();
          fclose(fd);
          return (ERROR);
	}

       sprintf (temp_outfilename, "%s", "J01_00000.PSV");
       sprintf (filetag, "%s", "PSV");
       sprintf (dataset, "%s", NASDA_SV_HSTR);
       if (add_to_llist(&PMF_struct, &minmaxtime, filetag, dataset, 
                        temp_outfilename) != OK) 
       {
          delete_translated_files();
          fclose(fd);
          return (ERROR);
	}
    }

  /* Close the input file */
  fclose (fd);
  return (OK);
}




/*=============================================================================
Function:       get_opln_time_range
Description:    Read an OPLN file and extract min and max time values
Parameters:     char           *nasda_file   name of file to be read
                char           *path         of the input file
                timespan_rec   *opln_timespan   
Returns:        integer status           OK, or ERROR
Creator:        Rich Norman
Creation Date:  Feb 1996
Notes:          
=============================================================================*/
int get_opln_time_range(char *path, char *nasda_file, 
                        timespan_rec *opln_timespan)
{ 
int      status ;
char     temp[20];
char    *opln_full_name;
FILE    *opln_file_ptr ;
#define  buff_size 81
char     buffer[buff_size] ;
int      year, month, day, doy, hour, min, sec;

/* Initialize the min/max values. */
   strcpy(opln_timespan->mintime,"2030-001T00:00:00.000");
   strcpy(opln_timespan->maxtime,"1970-001T00:00:00.000");

/* Create the full name of the opln file. */
   opln_full_name = (char *)util_do_malloc(sizeof(char)*
                            (strlen(path) + 1 + strlen(nasda_file) + 1)) ;
   strcpy(opln_full_name, path) ;
   strcat(opln_full_name, "/") ;
   strcat(opln_full_name, nasda_file) ;

/* Open the opln file. */
   if ((opln_file_ptr = fopen(opln_full_name, "r")) == (FILE *) NULL)
   {
      syslog(LOG_ERR, "ERROR: Error opening opln file %s.  Exiting.\n", 
                      opln_full_name);
      return (ERROR);
   }

/* Move to the beginning of the file descriptor record. */
   if (fseek(opln_file_ptr, 128, SEEK_SET) !=0)
   {
      syslog(LOG_ERR, "ERROR: get_opln_time_range fseek failure for file=%s\n",
                       opln_full_name);
      return (ERROR) ;
   }

/* Get the start/end string. */
   if (fgets(buffer, 17, opln_file_ptr) == (char *)NULL)
   {
      syslog(LOG_ERR, "ERROR: get_opln_time_range read failure for file=%s\n",
                       opln_full_name);
      return (ERROR) ;
   }

/* Convert the times into doy form. */
         strncpy (temp, &buffer[0], 4);
         temp[4] = '\0';	
         year = atoi(temp);
         strncpy (temp, &buffer[4], 2);
         temp[2] = '\0';
         month = atoi(temp);
         strncpy(temp, &buffer[6], 2);
         temp[2] = '\0';
         day = atoi(temp);
         tc_cal2doy (year, month, day, &doy);
         sprintf(opln_timespan->mintime, "%04d-%03dT00:00:00.000",year,doy);

         strncpy(temp, &buffer[8], 4);
         temp[4] = '\0';
         year = atoi(temp);
         strncpy(temp, &buffer[12], 2);
         temp[2] = '\0';
         month = atoi(temp);
         strncpy(temp, &buffer[14], 2);
         temp[2] = '\0';
         day = atoi(temp);
         tc_cal2doy (year, month, day, &doy);
         sprintf(opln_timespan->maxtime, "%04d-%03dT00:00:00.000",year,doy);

/* Close the input file */
   fclose(opln_file_ptr) ;

status = OK ;

if (strcmp(opln_timespan->mintime, opln_timespan->maxtime) > 0)
{
   status = ERROR;
   syslog(LOG_ERR, "ERROR: Failure finding OPLN start / end times.\n");
}

return(status);

} 

/* End of get_opln_time_range */



/*=============================================================================
Function:       gen_NASDA_PMF
Description:    Fills up PMF_struct which is then used to create the 
                PMF for the given file.
Parameters:     char *path               of the input file
                char *nasda_file         name of file to be parsed 
                char *cfname             configuration filename
Returns:        llist *                  linked list of PMF_FILE_LIST *
Creator:        Cameron Cooper
Creation Date:  Aug. '95
Notes:          To be called from fadirmon
=============================================================================*/
llist *gen_NASDA_PMF(char *path, char *nasda_file, char *cfname)
{ 
  AGGREGATE     root, orig_root ;
  ODL_Catalog_Metadata PMF_struct;
  FILE          *PMF_file_ptr ;
  char          fullpathname[255], fullPMFfilename[255];
  char          filedate[22], new_filename[255], new_fullpathname[255];
  int	        i ;	
  int           renstat;
  int 	        array_size ;
  FILE          *ascii_file_ptr ;
  PMF_FILE_LIST *filerecord;

  syslog (LOG_NOTICE, "NOTICE: Processing file %s.\n", nasda_file);

  rootpath = NULL;
  rootpath = (char *) get_config_val(cfname, "FAIF_ROOTPATH");
  if (rootpath == (char *) NULL)
  {
    syslog (LOG_ERR, "ERROR: FAIF_ROOTPATH not defined.  Exiting.\n");
    return (NULL);
  }

  strcpy (jers_rec.timespan.minrev, "99999");
  strcpy (jers_rec.timespan.maxrev, "00000");
  strcpy (jers_rec.timespan.mintime, "2100:000:00:00:00.000");
  strcpy (jers_rec.timespan.maxtime, "1900:000:00:00:00.000");
  if (strncasecmp(nasda_file, "opln", 4) == 0)
  {
     if (get_opln_time_range(path, nasda_file, &jers_rec.timespan) != OK)
     {
        syslog(LOG_ERR, "Unable to obtain time range for OPLN file. Exiting.") ;
        return (NULL) ;
     }
  }

  sprintf(fullpathname, "%s/%s", path, nasda_file) ;
  array_size = (sizeof(filetypes)/sizeof(FA_filetype));

  strcpy (PMF_struct.FA_file_type, fa_file_type_temp);
  strcpy (PMF_struct.file_creation_time, file_creation_time); 

  strcpy (new_filename, nasda_file);
  strcpy (new_fullpathname, fullpathname);
  
  i = 0;
  for (i = 0; i < array_size; ++i)	
    {
      if (strcmp(filetypes[i].sp_filetype, PMF_struct.FA_file_type) == 0) 
	{
	  strcpy (PMF_struct.gen_file_type, filetypes[i].gen_filetype);
	  strcpy (PMF_struct.file_source,filetypes[i].agency);
	  strcpy (PMF_struct.format,filetypes[i].format);
	  strcpy (PMF_struct.satellite,filetypes[i].sat);
	  strcpy (PMF_struct.file_dest,filetypes[i].dest);
	  break;
	}
    }
  
  strcpy (PMF_struct.file_name,new_filename);
  
  /* Get the time stamp on the file and put it into ODL time format */
  strcpy (PMF_struct.file_arrival_time, (char *)PMFfiletime(new_fullpathname)) ;
    
  /* Create the PMF */

  /* Make a linked list that has filename and PMF filename */
  bar = create_dyn_llist();
  filerecord = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
  strcpy (filerecord->file_name, new_filename);
  sprintf(filerecord->PMF_file_name,"%s.M",new_filename);
  strcpy (filerecord->dataset_suffix, PMF_struct.FA_file_type);
  filerecord->orig_not_tran = TRUE;
  APPEND(bar, filerecord, free, filerecord);

  /* Create the PMF filename */
  sprintf(fullPMFfilename, "%s/NASDA/PMF/%s", 
          rootpath, filerecord->PMF_file_name);

  /* Check to see if file need to be translated. If so, then translate it
     and create the PMF for it as with the original and add it to the 
     linked list */
  if (strcmp(PMF_struct.FA_file_type, NASDA_ELMF_HSTR) == 0)
     if (translate_nasda_elmf(new_fullpathname, &PMF_struct, cfname) != OK)  
           return(NULL);

  /* Complete the PMF for the original file */
  if (strcmp (jers_rec.timespan.minrev, "99999") == 0)
    jers_rec.timespan.minrev[0]  = NULL;
  if (strcmp (jers_rec.timespan.maxrev, "00000") == 0)
    jers_rec.timespan.maxrev[0]  = NULL;
  if (strcmp (jers_rec.timespan.mintime, "2100:000:00:00:00.000") == 0)
    jers_rec.timespan.mintime[0] = NULL;
  if (strcmp (jers_rec.timespan.maxtime, "1900:000:00:00:00.000") == 0)
    jers_rec.timespan.maxtime[0] = NULL;
  strcpy(PMF_struct.valid_start_time, jers_rec.timespan.mintime) ;
  strcpy(PMF_struct.valid_end_time, jers_rec.timespan.maxtime) ;
  strcpy(PMF_struct.start_rev, jers_rec.timespan.minrev) ;
  strcpy(PMF_struct.end_rev, jers_rec.timespan.maxrev) ;
  orig_root = (AGGREGATE)create_PMF(&PMF_struct);

  /* Write out the PMF for the original file */
  if ((PMF_file_ptr = fopen(fullPMFfilename, "w")) == (FILE *)NULL)
  {
    syslog (LOG_ERR, "ERROR: Error opening PMF file %s.  Exiting.\n",
             fullPMFfilename);
    delete_translated_files();
    return (NULL);
  }
  WriteLabel (PMF_file_ptr, orig_root);
  fclose(PMF_file_ptr);
  RemoveAggregate(orig_root);

  syslog (LOG_NOTICE, "NOTICE: Completed processing file %s.\n", new_filename);

  /* Return the linked list */
  return(bar);
} 


/*==============================================================================
Function:	delete_translated_files
Description:	Delete the unusable translated files and their PMFs.
Parameters:     (none)
Returns:	OK
Creator:	Rodney Hoffman
Creation Date:	Dec. '96
Notes:		
==============================================================================*/

int delete_translated_files ()
{
  PMF_FILE_LIST *fileread ;
  char          bad_filename[255];
  cursor        ptr ;
  char          PMF_path[255];
  char          translation_path[255];

  sprintf (PMF_path, "%s/NASDA/PMF", rootpath);
  sprintf (translation_path, "%s/NASDA/tran", rootpath);

  /* get rid of unusable files and PMFs */
  fileread = FIRST(bar, ptr);
  while (fileread)
  {
    sprintf (bad_filename, "%s/%s", translation_path, 
             fileread->file_name);
    remove (bad_filename);
    sprintf (bad_filename, "%s/%s", PMF_path, fileread->PMF_file_name);
    remove (bad_filename); 
    fileread = NEXT(bar, ptr) ;
  }
  return (OK);
}


/*==============================================================================
Function:	get_elmf_info
Description:	Begin processing an ELMF file.  Read and parse and validate
                the header information and the file descriptor information.
                Also get the necessary JERS-1 phase data from the config file.
                Put all the information in the jers_data_rec.  
Parameters:     char            *cfname
                FILE            *fd
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Dec. '96
Notes:		
==============================================================================*/

int get_elmf_info (char *cfname, FILE *fd)
{
  FILE    *config_file;
  static char *pstart = NULL;
  static char *prevs = NULL;
  static char *plastrev = NULL;
  static char *cdays = NULL;
  static char *crevs = NULL;
  char	buffer[maxrecsize];
  char  temp[20];

  /* Get JERS-1 phase information from configuration file */
  if ((config_file = fopen(cfname, "r")) == (FILE *) NULL)
  {
     syslog (LOG_ERR, "ERROR: Error opening config file %s.  Exiting.\n", 
                      cfname);
     return (ERROR);
  }
  fclose (config_file);
  pstart   = (char *) get_config_val(cfname, "PHASE_START");
  prevs    = (char *) get_config_val(cfname, "PHASE_REVS");
  plastrev = (char *) get_config_val(cfname, "PHASE_LASTREV");
  cdays    = (char *) get_config_val(cfname, "PHASE_CYCLEDAYS");
  crevs    = (char *) get_config_val(cfname, "PHASE_CYCLEREVS");
  if ((pstart == NULL) || (prevs == NULL) || 
      (plastrev == NULL) || (cdays == NULL) || (crevs == NULL))
  {
     syslog (LOG_ERR, "ERROR: Unable to find phase info!  Exiting.\n");
     return (ERROR);
  }
  strcpy (jers_rec.pstart, pstart);
  jers_rec.iprevs    = atoi (prevs);
  jers_rec.iplastrev = atoi (plastrev);
  jers_rec.icdays    = atoi (cdays);
  jers_rec.icrevs    = atoi (crevs);
  if ((jers_rec.iprevs < 1) || (jers_rec.iplastrev < 1) || 
      (jers_rec.icdays < 1) || (jers_rec.icrevs < 1))
  {
     syslog (LOG_ERR, "ERROR: Invalid phase info.  Exiting.\n");
     return (ERROR);
  }

  /* Read the header record */
  if (fgets (buffer, HDRRECLEN + 1, fd) == NULL) 
    {
      syslog (LOG_ERR, "ERROR: Error reading ELMF file header.  Exiting.\n");
      return (ERROR);
    }

  /* Validate the first fields */
  if (strncmp(buffer,"ELMF",4) != 0 ||
      strncmp(&buffer[8],"ERS1",4) != 0 ||
      strncmp(&buffer[12],"HMMO",4) != 0 ||
      strncmp(&buffer[16],"FAIS",4) != 0)
	{
          syslog (LOG_ERR, 
                  "ERROR: Error validating NASDA ELMF file header record.\n");
	  return (ERROR);
	}

  /* Read the file descriptor record */
  if (fgets(buffer, FDRRECLEN + 1, fd) == NULL)
    {
      syslog (LOG_ERR, "ERROR: Error reading NASDA ELMF file fdr record.\n");
      return (ERROR);
    }

  /* Parse the file descriptor record */
  /* No. of time corr recs */
  strncpy (temp, buffer, 3);
  temp[3] = '\0';
  jers_rec.tc_count = atoi (temp);
  if (jers_rec.tc_count < 0 || jers_rec.tc_count > 999)
    {
      syslog (LOG_ERR, 
              "ERROR: Error reading ELMF file time. corr. rec. cnt.\n");
      return (ERROR);
    }

  /* No. of restituted svs */
  /* Only the D-1 and D-2 sv's are saved as restituted. */
  temp[4] = '\0';
  strncpy (temp, &buffer[47], 4);
  jers_rec.d2 = atoi (temp);
  strncpy (temp, &buffer[59], 4);
  jers_rec.d1 = atoi (temp);

  /* No. of predicted svs */
  strncpy (temp, &buffer[71], 4);
  jers_rec.d0 = atoi (temp);
  strncpy (temp, &buffer[83], 4);
  jers_rec.p1 = atoi (temp);
  strncpy (temp, &buffer[95], 4);
  jers_rec.p2 = atoi (temp);
  strncpy (temp, &buffer[107], 4);
  jers_rec.p3 = atoi (temp);

  /* Validate the rec counts */
  if (jers_rec.d2 < 0 || jers_rec.d2 > 9999 ||
      jers_rec.d1 < 0 || jers_rec.d1 > 9999 ||
      jers_rec.d0 < 0 || jers_rec.d0 > 9999 ||
      jers_rec.p1 < 0 || jers_rec.p1 > 9999 ||
      jers_rec.p2 < 0 || jers_rec.p2 > 9999 ||
      jers_rec.p3 < 0 || jers_rec.p3 > 9999)
    {
      syslog (LOG_ERR, "ERROR: Error reading NASDA ELMF file sv rec cnts.\n");
      return (ERROR);
    }
  return(OK);
}


/*==============================================================================
Function:	add_to_llist
Description:	Fix the output filename of the new translated file,
                Create a PMF file for it, and append these to the llist.
Parameters:     ODL_Catalog_Metadata  *pmf          For the translated file
                timespan_rec          *minmaxtime   For this file only
                char                  *filetag      For this file
                char                  *dataset      For this file
                char                  *filename     For this file
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Dec. '96
Notes:		
==============================================================================*/

int add_to_llist (ODL_Catalog_Metadata *pmf, timespan_rec *minmaxtime, 
                  char *filetag, char *dataset, char *filename)
{

  int             stat;
  AGGREGATE       root;
  PMF_FILE_LIST  *filerecord;
  int             year, decade, day, hour, min, sec, msec;
  FILE           *pmf_file;
  char            oldname[255], newname[255];
  char            fullPMFfilename[255];
  time_t          current_time;
  time_t         *null_ptr = NULL;
  int             status;

  /* Fix the output filename, now that we know the rev to use */
  sprintf (pmf->file_name, "J01_%s.%s", minmaxtime->minrev, filetag);
  sprintf (oldname, "%s/NASDA/tran/%s", rootpath, filename);
  sprintf (newname, "%s/NASDA/tran/%s", rootpath, pmf->file_name);
  stat = rename (oldname, newname);
  if (stat != 0)
  {
      syslog (LOG_ERR, "ERROR: Unable to rename output file: %s.\n", filename);
      delete_translated_files();
      return (ERROR);
  }

  /*  Fill in timespan info */
  strcpy (pmf->start_rev, minmaxtime->minrev);
  strcpy (pmf->end_rev, minmaxtime->maxrev);
  if (tc_parse_asftime(minmaxtime->mintime,
        &year, &decade, &day, &hour, &min, &sec, &msec))
  {
    sprintf (pmf->valid_start_time, "%04d-%03dT%02d:%02d:%02d.%03d\0",
                year, day, hour, min, sec, msec);
  }
  if (tc_parse_asftime(minmaxtime->maxtime,
        &year, &decade, &day, &hour, &min, &sec, &msec))
  {
    sprintf (pmf->valid_end_time, "%04d-%03dT%02d:%02d:%02d.%03d\0",
                year, day, hour, min, sec, msec);
  }

  /* And adjust input file's timespan info if necessary */
  if (strcmp (pmf->start_rev, jers_rec.timespan.minrev) < 0)
  {
    strcpy (jers_rec.timespan.minrev, pmf->start_rev);
    strcpy (jers_rec.timespan.mintime, pmf->valid_start_time);
  }
  if (strcmp (pmf->end_rev, jers_rec.timespan.maxrev) > 0)
  {
    strcpy (jers_rec.timespan.maxrev, pmf->end_rev);
    strcpy (jers_rec.timespan.maxtime, pmf->valid_end_time);
  }

  current_time = (time_t)time(null_ptr);
  status = cftime(pmf->file_arrival_time, "%Y-%jT%H:%M:%S.000", &current_time);
  strcpy (pmf->file_creation_time, pmf->file_arrival_time);
  root = (AGGREGATE)create_PMF(pmf);
  filerecord = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
  strcpy (filerecord->file_name, pmf->file_name);
  sprintf(filerecord->PMF_file_name,"%s.M", pmf->file_name);
  strcpy (filerecord->dataset_suffix, dataset);
  filerecord->orig_not_tran = FALSE;
  PREPEND(bar, filerecord, free, filerecord);
  sprintf (fullPMFfilename, "%s/NASDA/PMF/%s", 
           rootpath, filerecord->PMF_file_name);
  if ((pmf_file = fopen(fullPMFfilename, "w")) == (FILE *)NULL)
  {
    syslog (LOG_ERR, "ERROR: Error opening PMF file %s.  Exiting.\n",
             fullPMFfilename);
    delete_translated_files();
    return (ERROR);
  }
  WriteLabel (pmf_file, root);
  fclose(pmf_file);
  RemoveAggregate(root);

  syslog (LOG_NOTICE, "NOTICE: Completed translation file %s.", newname);

  return (OK);
}

/*==============================================================================
Function:	translate_tce
Description:	Read and translate the time correction elements in the ELMF file
Parameters:     FILE                   *fd           Input ELMF file
                ODL_Catalog_Metadata   *pmf          For output file
                timespan_rec           *minmaxtime   For output file
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Dec. '96
Notes:		
==============================================================================*/

int translate_tce(FILE *fd, ODL_Catalog_Metadata *pmf, timespan_rec *minmaxtime)
{

  char    oldname[100];
  FILE    *fout;
  char    msgheader[100];  char    currtime[22];
  int     i;

  sprintf (oldname, "%s/NASDA/tran/J01_00000.TCE", rootpath);

  /* Open output file */
  if ((fout = fopen (oldname, "w")) == NULL)
  {
    syslog (LOG_ERR, "ERROR: Error opening NASDA time correlation file %s.\n",
      oldname);
    return (ERROR);
  }

  /* Write the common message header */
  memset (msgheader, ' ', 50);
  msgheader[50] = '\0';
  tc_systime2asf(currtime);
  currtime[4] = ' ';

  strncpy (msgheader, currtime, 21);
  strncpy (&msgheader[22], "TC IMS FAI", 10);
  fprintf (fout, "%s\n", msgheader);

  for (i = 0; i < jers_rec.tc_count; i++)
    {
      if (read_nastimecorr (fd, fout, minmaxtime) != OK)
	{
          syslog (LOG_ERR, 
                 "ERROR: Error reading time corr. rec. # %d from ELMF file.\n",
                  i+1);
          fclose (fd);
          fclose (fout);
          remove (oldname);
          return (ERROR);
        }
    }

  fclose(fout);
  return (OK);

}


/*==============================================================================
Function:	translate_rsv
Description:	Read and translate the restituted state vectors for one rev
                from the ELMF file
Parameters:     FILE                   *fd           Input ELMF file
                ODL_Catalog_Metadata   *pmf          For output file
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Dec. '96
Notes:		
==============================================================================*/

int translate_rsv(FILE *fd, ODL_Catalog_Metadata *pmf)
{
  char   outpath[255];
  char   fullfilename[255];
  char   stv_str[255];
  char   strttime[22];
  int    prev_rev, curr_rev;
  char    precstr[15];
  int    stat;
  int    d_count;
  char   temp_outfilename[14];
  char   filetag[4];
  char   dataset[100];
  timespan_rec  minmaxtime;
  int    precision;
  int    i;
  FILE   *fout;

  prev_rev = -1;
  curr_rev = 0;

  precision = 2;
  strcpy(precstr, "RESTITUTED");
  sprintf (filetag, "%s", "RSV");
  sprintf (dataset, "%s", NASDA_SV_HSTR);

  strcpy (temp_outfilename, "J01_00000.RSV");
  sprintf (outpath, "%s/NASDA/tran", rootpath);
  sprintf (fullfilename, "%s/%s", outpath, temp_outfilename);

  strcpy (minmaxtime.minrev, "99999");
  strcpy (minmaxtime.maxrev, "00000");
  strcpy (minmaxtime.mintime, "2100:000:00:00:00.000");
  strcpy (minmaxtime.maxtime, "1900:000:00:00:00.000");

  d_count = jers_rec.d1 + jers_rec.d2;
  begin_stvfile ("IMS", "J1", precstr, temp_outfilename, outpath, &fout);
  for (i = 0; i < d_count; i++)
  {
	  if (read_nasstv 
	       (fd, stv_str, strttime, &curr_rev, precision) != OK)
	  {
              syslog (LOG_ERR, 
		     "ERROR: Error reading new rest. SV no. %d from ELMF file."
                     , i+1);
              fclose (fout);
              remove (fullfilename);
              return (ERROR);
	  }

          if ((prev_rev != -1) && (curr_rev != prev_rev))
	  {
            /* Close this translated file, add to llist, begin new 
               translated file.  */
            fclose (fout);
            if (add_to_llist(pmf, &minmaxtime, filetag, dataset, 
                             temp_outfilename) != OK) 
	    {
              remove (fullfilename);
              return (ERROR);
            }

            begin_stvfile ("IMS", "J1", precstr, temp_outfilename, outpath, 
                           &fout);
            strcpy (minmaxtime.minrev, "99999");
            strcpy (minmaxtime.maxrev, "00000");
            strcpy (minmaxtime.mintime, "2100:000:00:00:00.000");
            strcpy (minmaxtime.maxtime, "1900:000:00:00:00.000");
          }

          /*  Write one state vector record to the translated file */
          stat = fprintf (fout, "%s", stv_str);

          /* Determine the min and max times */
          if (strcmp (strttime, minmaxtime.mintime) < 0)
          {
            strcpy (minmaxtime.mintime, strttime);
            sprintf (minmaxtime.minrev, "%05d", curr_rev);
          }
          if (strcmp (strttime, minmaxtime.maxtime) > 0)
          {
            strcpy (minmaxtime.maxtime, strttime);
            sprintf (minmaxtime.maxrev, "%05d", curr_rev);
          }

          if (stat < 0)
          {
              syslog (LOG_ERR, "ERROR: Error writing state vector record.\n");
              fclose (fout);
              remove (fullfilename);
              delete_translated_files ();
              return (ERROR);
          }

          prev_rev = curr_rev;

  }

  /* Close last translated file and add to llist. */
  fclose (fout);
  if (add_to_llist(pmf, &minmaxtime, filetag, dataset, 
                   temp_outfilename) != OK)
  {
    remove (fullfilename);
    return (ERROR);
  }

  return (OK);
}


/*==============================================================================
Function:	translate_psv
Description:	Read and translate the predicted state vectors in the ELMF file
Parameters:     FILE                   *fd           Input ELMF file
                ODL_Catalog_Metadata   *pmf          For output file
                timespan_rec           *minmaxtime   For output file
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Dec. '96
Notes:		
==============================================================================*/

int translate_psv(FILE *fd, ODL_Catalog_Metadata *pmf, timespan_rec *minmaxtime)
{
      char   outpath[255];
      char   fullfilename[255];
      char   stv_str[255];
      char   strttime[22];
      int    stat;
      int    curr_rev;
      int    precision;
      char   temp_outfilename[14];
      char   precstr[15];
      int    p_count;
      int    i;
      FILE   *fout;

      curr_rev = 0;

      precision = 1;
      strcpy(precstr, "PREDICTED");
      strcpy (temp_outfilename, "J01_00000.PSV");
      sprintf (outpath, "%s/NASDA/tran", rootpath);
      sprintf (fullfilename, "%s/%s", outpath, temp_outfilename);

  /* Day 0 records are to be considered predicted as well */
  p_count = jers_rec.d0 + jers_rec.p1 + jers_rec.p2 + jers_rec.p3;

  begin_stvfile ("IMS", "J1", precstr, temp_outfilename, outpath, &fout);

  for (i = 0; i < p_count; i++)
    {
      stv_str[0] = NULL;
      if (read_nasstv 
	    (fd, stv_str, strttime, &curr_rev, precision) != OK)
	{
          syslog (LOG_ERR, 
               "ERROR: Error appending new pred. SV no. %d from ELMF file.\n", 
                i+1);
          fclose (fd);
          fclose (fout);
          remove (fullfilename);
          return (ERROR);
	}

        if (stv_str[0] != NULL)
	{
          /*  Write one state vector record to the translated file */
          stat = fprintf (fout, "%s", stv_str);

          /* Determine the min and max times */
          if (strcmp (strttime, minmaxtime->mintime) < 0)
          {
            strcpy (minmaxtime->mintime, strttime);
            sprintf (minmaxtime->minrev, "%05d", curr_rev);
          }
          if (strcmp (strttime, minmaxtime->maxtime) > 0)
          {
            strcpy (minmaxtime->maxtime, strttime);
            sprintf (minmaxtime->maxrev, "%05d", curr_rev);
          }

          if (stat < 0)
          {
            syslog (LOG_ERR, "ERROR: Error writing state vector record.\n");
            fclose (fout);
            remove (fullfilename);
            return (ERROR);
          }
        }
    }

  fclose(fout);
  return (OK);
}


/* End of File */



