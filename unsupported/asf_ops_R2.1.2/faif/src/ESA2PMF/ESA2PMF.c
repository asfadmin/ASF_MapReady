/*************************************************************************
 * Copyright (c)1995, 1997 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	ESA2PMF.c

Description:    Contains the funtions necessary for creating parsing ESA files
                and creating the PMF.

External Functions:
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:
1.  Feb. '96 - R. Hoffman 
    Fix filenames and datasets for translated files.
2.  July '96 - R. Hoffman
    Omit duplicate fopen(fullPMFfilename...) in gen_ESA_PMF()
3.  Sept. '96 - R. Hoffman
    (a) Handle SHAQP - shaqp2pmf()
    (b) RemoveAggregate for ODL trees of translated files' PMF.
4.  Feb. '97 - R. Hoffman
    (a) Write one *.RSV file per rev
    (b) Throw away 7 of every 8 restituted state vectors.
5.  April '97 - R. Hoffman
    (a) Pad short rev numbers with zeros in TCE files.
6.  June '97 - R. Hoffman
    Reverse the linked list output.
==============================================================================*/

static char SccsFile[] = "ESA2PMF.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "16 Jul 1996";
static char SccsLastChanger[] = "@(#)ESA2PMF.c	1.7";
static char SccsState[] = "1.7";


#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absloute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <stdlib.h>         /* for free etc...                      */
#include <errno.h>          /* for errno definitions...             */
#include <syslog.h>         /* for syslog		    	    */
#include <time.h>
#include "faifdefs.h"

#include "PMF.h"
#include "ESAconversions.h"
#include "GENconversions.h"
#include "ESA2PMF.h"
#include "ESA.h"
#include "llist_err.h"
#include "dapps_list.h"
#include "nmalloc.h"

#define  FA_ASCII_REC_PROCESS_OK		 0
#define  FA_ASCII_REC_INGESTION_OK		 0

typedef struct {
	  char    mintime[22];
	  char    maxtime[22];
          char    minrev[6];
	  char    maxrev[6];
	} timespan_rec;

int           llist_errno ;
timespan_rec  overall_timespan;   /* of one input file */
char          fa_creation_time[] =	"yyyy:ddd:hh:mm:ss.mmm";
char          *rootpath = NULL;
char          cfname[255];
llist         *bar;
void          *unused_pointer;
extern        int ESAc_yymmdd_pad_hh_mm_ss2asftime() ;

ODL_Catalog_Metadata infile_pmf;     /* of the input file */

FA_filetype esa_filetypes[] =
{        
  {ESA_SHAQ_HSTR, GFTYPE_FASKED,    ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_ORPD_HSTR, GFTYPE_PSV,	    ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_ORRE_HSTR, GFTYPE_RSV,	    ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_ODMC_HSTR, GFTYPE_RQST_DCP,  ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_ODMR_HSTR, GFTYPE_FADCPRMSG, ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_RQST_HSTR, GFTYPE_FARTLTUR,  ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_PATC_HSTR, GFTYPE_TIME_COR,  ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_RQVR_HSTR, GFTYPE_LTUSRRQV,  ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_MPSG_HSTR, GFTYPE_FAPLAN,    ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_RQUS_HSTR, GFTYPE_LTUSRREQ,  ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_REAQ_HSTR, GFTYPE_ACQ_REP,   ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_RESM_HSTR, GFTYPE_SHIP_REP,  ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_REEX_HSTR, GFTYPE_ARCH_DEX,  ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR},
  {ESA_REUG_HSTR, GFTYPE_STN_STAT,  ESA_STR, ORIG_STR, E_SAT, ASF_STR, NO_STR}, 
  {NULL,	  NULL,		    NULL,    NULL,       NULL,  NULL, NULL}
};	

VALUE_DEFS ascii_file[] =  
{
#ifdef TEMPLATE
    {FILE_HEADER,	REPORT_CONTROL,	 0, 0, function, equiv_table, dest},
    {FILE_HEADER,	REPORT_HEADER,	 0, 0, function, equiv_table, dest},
    {FILE_HEADER,	REPORT_RECORD,	 0, 0, function, equiv_table, dest},
    {FILE_RECORD,	REPORT_HEADER,	 0, 0, function, equiv_table, dest},
    {FILE_RECORD,	REPORT_RECORD,	 0, 0, function, equiv_table, dest},
    {FA_DEFAULT,	REPORT_RECORD,	 0, 0, function, equiv_table, dest},
#endif

    {FILE_HEADER,	REPORT_RECORD,	 0, 4,	gen_string2str,     NULL, (int) infile_pmf.FA_file_type},
    {FILE_HEADER,	REPORT_RECORD,  20, 2,	gen_string2str,	    NULL, (int) infile_pmf.satellite},
    {FILE_HEADER,	REPORT_HEADER,   5,25,	ESAc_yymmdd_pad_hh_mm_ss2asftime, NULL, (int) fa_creation_time},
    {0,                 0,    		-1,-1, 	NULL, 		    NULL, NULL} 
};                                                           




/*=============================================================================
Function:       shaqp2pmf
Description:    Fills up PMF_struct which is then used to create the 
                PMF for the given file.
Parameters:     char *    name of file to be parsed 
                char *    path of input file
Returns:        OK or ERROR
Creator:        Rodney Hoffman
Creation Date:  September 1996
Notes:          Adapted from gen_ESA_PMF()
=============================================================================*/

int shaqp2pmf(char *path, char *esa_filename)
{     
  AGGREGATE              root ;
  FILE                   *PMF_file_ptr ;
  char                   fullpathname[255] ; 
  char                   PMF_path[255], fullPMFfilename[255] ;
  char	                 logmsg[MAX_SYSLOG_MSGLEN+1];
  char                   fnlen;
  char                   sat[3];
  PMF_FILE_LIST          *filerecord ;

  strcpy (infile_pmf.file_name, esa_filename);
  sprintf(fullpathname, "%s/%s", path, esa_filename) ;
  sprintf (PMF_path, "%s/ESA/PMF", rootpath);
  
  /* Fill in the structure for the PMF */
  strcpy (infile_pmf.gen_file_type, GFTYPE_FASKED);
  strcpy (infile_pmf.FA_file_type, "SHAQP");
  strcpy (infile_pmf.file_source, ESA_STR);
  strcpy (infile_pmf.format, ORIG_STR);
  strcpy (infile_pmf.file_dest, ASF_STR);
  infile_pmf.start_rev[0] = NULL;
  infile_pmf.end_rev[0] = NULL; 
  infile_pmf.valid_start_time[0] = NULL;
  infile_pmf.valid_end_time[0] = NULL;

  /* Get satellite from filename suffix */
  fnlen = strlen(esa_filename);
  strncpy(sat, &esa_filename[fnlen-2], 2);
  sat[0] = toupper(sat[0]);
  sat[2] = '\0';
  strcpy (infile_pmf.satellite, sat);
  if ((strcmp(sat, "E1") != 0) && (strcmp(sat, "E2") != 0))
  {
     sprintf (logmsg, 
              "ERROR: filename %s does not end with 'E1' or 'E2'.  Exiting.\n",
	      esa_filename);
     syslog (LOG_ERR, logmsg);
     return (ERROR);
  }
    
  /* Get the time stamp on the file and put it into ODL time format */
  strcpy (infile_pmf.file_arrival_time, (char *)PMFfiletime(fullpathname)) ;

  /* Because SHAQP contains no file creation time, use arrival time */
  strcpy (infile_pmf.file_creation_time, infile_pmf.file_arrival_time) ;
  
  /* Write the PMF out to the PMF file */
  sprintf (fullPMFfilename, "%s/%s.M", PMF_path, esa_filename);
  if ((PMF_file_ptr = fopen (fullPMFfilename, "w")) == (FILE *)NULL)
  {
    sprintf(logmsg, "ERROR: Unable to open PMF file %s.\0", fullPMFfilename);
    syslog (LOG_ERR, logmsg);
    remove (fullPMFfilename);
    return (ERROR);
  }

  root = (AGGREGATE)create_PMF(&infile_pmf);
  WriteLabel (PMF_file_ptr, root);
  fclose(PMF_file_ptr);
  RemoveAggregate(root) ;

  /* Make a linked list item with the SHAQP filename and the PMF filename */
  bar = create_dyn_llist();
  filerecord = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
  strcpy (filerecord->file_name, esa_filename);
  sprintf(filerecord->PMF_file_name,"%s.M",esa_filename);
  strcpy (filerecord->dataset_suffix, "SHAQP");
  filerecord->orig_not_tran = TRUE;
  APPEND(bar, filerecord, free, filerecord);

  sprintf (logmsg, "NOTICE: Completed processing file %s.\n", esa_filename);
  syslog (LOG_NOTICE, logmsg);

  return(OK);
} 




/*==============================================================================
Function:	translate_ESA_stvec
Description:	Reads an ESA state vector file and writes file(s) in
                ASF format, plus PMF.
Parameters:     char *                      ESA state vector filename
                char *                      pathname for output file
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Oct. '95
Notes:		Adapted from ACS:
                  [bld.acs.load.fadirmon]ESASTV.QC
                  [bld.acs.lib.system]CHECK_DATE.C
                  [bld.acs.lib.stvec]SVPM2EQX.FOR
                "The ESA state vectors (114) are converted into 112.
                There is no change in the fundamental plane (xy plane)."
1.  Feb. '97 - RH
    Modified to write separate *.RSV files for each rev
==============================================================================*/

int translate_ESA_stvec (char *infilename, char *outdir)
{
#define maxrecsize 12000

  ODL_Catalog_Metadata    pmf;        /* for each output file */
  int		rstat, stat, i, ix;
  int           orre;                 /* Are we translated an ORRE file? */
  char		recordbuff[maxrecsize];
  char		logmsg[MAX_SYSLOG_MSGLEN+1];
  char		sat[3], precision[2], precstr[11];
  char          filetag[4];
  char          stv_str[255];
  FILE		*fin;
  FILE		*fout;
  int		nbytes;
  int           nvectors;
  char          newname[255], oldname[255];
  char		temp_outfilename[14];
  timespan_rec  minmaxtime;
  char          sv_time[22];
  char		minrev[6], maxrev[6];
  int           curr_rev, prev_rev;
  int           iminrev;
  int		year, decade, day, hour, min, sec, msec;
  int           fnlen;

  /*  Take satellite name from the extension (".E1" or ".E2") of infilename */
  fnlen = strlen(infilename);
  strncpy (sat, &infilename[fnlen-2], 2);
  sat[2] = '\0';
  if ((strcmp(sat, "E1") != 0) && (strcmp(sat, "E2") != 0))
  {
     sprintf (logmsg, 
              "ERROR: filename %s does not end with 'E1' or 'E2'.  Exiting.\n",
	      infilename);
     syslog (LOG_ERR, logmsg);
     return (ERROR);
  }
  
  /*  Figure out the precision and create temporary output filename. 
      We'll have to wait until the end to fill in the right Rev number
      in the filename.  */
  if (strcmp(infile_pmf.FA_file_type, "ORPD") == 0)
  {
  	precision[0] = '1';
  	precision[1] = '\0';
	strcpy(precstr, "PREDICTED");
        sprintf (filetag, "%s", "PSV");
  	sprintf (temp_outfilename, "E0%c_00000.PSV", sat[1]);
        orre = 0;
  }
  else if (strcmp(infile_pmf.FA_file_type, "ORRE") == 0)
  {
  	precision[0] = '2';
  	precision[1] = '\0';	
	strcpy(precstr, "RESTITUTED");
        sprintf (filetag, "%s", "RSV");
  	sprintf (temp_outfilename, "E0%c_00000.RSV", sat[1]);
        orre = 1;
  }

  if ((fin = fopen (infilename, "r")) == (FILE *)NULL)
  {
	sprintf (logmsg, "ERROR: Error opening file %s.  Exiting.\n",
		infilename);
	syslog (LOG_ERR, logmsg);
	return (ERROR);
  }

  /* use impossible dates and revs */
  strcpy(minmaxtime.mintime, "2100:000:00:00:00.000");  
  strcpy(minmaxtime.minrev, "99999");
  strcpy(minmaxtime.maxtime, "1900:000:00:00:00.000");
  strcpy(minmaxtime.maxrev, "00000");

  /* Read in the fixed portion of the file - ESA header */
  nbytes = 30;
  rstat = fscanf (fin, "%30c", recordbuff);
  recordbuff[nbytes] = '\0';
  if (rstat == EOF)
  {
    sprintf (logmsg, "ERROR: Error reading file %s.  Exiting.\n", infilename);
    syslog (LOG_ERR, logmsg);
    fclose (fin);
    return (ERROR);
  } 
  if ((int)strlen(recordbuff) < nbytes)
  {
    sprintf (logmsg, 
	     "ERROR: Invalid no. of bytes read from file %s. Exiting.\n",
	     infilename);
    syslog (LOG_ERR, logmsg);
    fclose (fin);
    return (ERROR);
  }

  prev_rev = -1;  curr_rev = 0;
  nbytes = 40;
  begin_stvfile ("IMS", sat, precstr, temp_outfilename, outdir, &fout);

  /*  Start with the infile_pmf and change pieces of it  */
  pmf = infile_pmf;
  strcpy (pmf.format, ASF_STR);
  pmf.file_name[0] = NULL;

  for (nvectors = 0; TRUE ; nvectors++)
  {
     /* Read in one state vector record */
     if (feof(fin)) break;
     if ((recordbuff[0] = fgetc(fin)) == EOF) break;
     for (ix = 1; ix < nbytes; ix++) recordbuff[ix] =  fgetc(fin);
     recordbuff[nbytes] = '\0';

     /* Only translate one of every 8 restituted state vectors */
     if (orre && ((nvectors % 8) > 0)) continue;

     if (one_sv (recordbuff, stv_str, sv_time, &curr_rev) != OK)
     {
       fclose (fin);
       return (ERROR);
     }

     /*  SCR 2545 - For restituted state vectors, make one file per rev.  */
     if (orre && (prev_rev != -1) && (curr_rev != prev_rev))
     {
        /* Close the output file, add it to the llist, and start a new one */
        fclose (fout);
        if (add_to_llist(&pmf, &minmaxtime, filetag, sat, ESA_SV_HSTR,
                        temp_outfilename) != OK) 
        {
          fclose(fin);
          return (ERROR);
	}

        begin_stvfile ("IMS", sat, precstr, temp_outfilename, outdir, &fout);

        sprintf (minmaxtime.minrev, "%05d", curr_rev);
        sprintf (minmaxtime.maxrev, "%05d", curr_rev);
        strcpy (minmaxtime.mintime, sv_time);
        strcpy (minmaxtime.maxtime, sv_time);
     }

     /*  Write one vector to the output file.  */ 
     stat = fprintf (fout, "%s", stv_str);
     if (stat < 0) 
     {
	syslog (LOG_ERR, "Error writing state vector record.\n");
        delete_translated_files();
  	fclose (fin);
  	return (ERROR);
     }
     if (strcmp(sv_time, minmaxtime.mintime) < 0)
     { 
        strcpy(minmaxtime.mintime, sv_time);
        sprintf(minmaxtime.minrev, "%05d", curr_rev);
     }
     if (strcmp(sv_time, minmaxtime.maxtime) > 0) 
     {
        strcpy(minmaxtime.maxtime, sv_time);
        sprintf(minmaxtime.maxrev, "%05d", curr_rev);
     }

     prev_rev = curr_rev;

  } /* end for loop */

  fclose (fin);

  /* Close last translated file and add to llist. */
  fclose (fout);
  if (add_to_llist(pmf, &minmaxtime, filetag, sat, ESA_SV_HSTR, 
                   temp_outfilename) != OK)
    return (ERROR);

  sprintf (logmsg, "NOTICE: Completed translating file %s.", infilename);
  syslog (LOG_NOTICE, logmsg);
  return (OK);
}




/*==============================================================================
Function:	translate_ESA_timecorr.c
Description:	Reads an ESA PATC file and translates it into ASF format
Parameters:     char                   *infilename  Of the PATC file
                char                   *outdir      For the output file
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Oct. '95
Notes:		Adapted from ACS:
                  [bld.acs.load.fadirmon]ESATIMECORR.QC
                  [bld.acs.oper.mgt]MKTIMFILE.QC
==============================================================================*/

int translate_ESA_timecorr (char *infilename, char *outdir)
{
#define maxrecsize 12000
#define maxmsglen 257

        ODL_Catalog_Metadata     pmf;         /* for the translated file */
  	char	sat[3];
	int	stat, recnum, rstat;
	char	msgheader[100];
	char	recordbuff[maxrecsize];
	char	logmsg[MAX_SYSLOG_MSGLEN+1];
	char	temp_outfilename[255];
        char    tempname[14];
	char    newname[255];
	char    currtime[22];
	char	timerec[60];
	int	utcday, utcms;
	char	temp[10];
	int	ix;
        timespan_rec  minmaxtime;
	int     year, decade, day, hour, min, sec, msec;
	FILE	*fdi;
	FILE    *fdo;
	int	nbytes;
        int     fnlen;
	char	minasftime[22], maxasftime[22];
	char	minrev[6], maxrev[6];
        int     iminrev;
                
  	struct {
  		int	revnum;
  		char	asftime[22];
  		int	sattime;
  		int	clockcycle;
  	}	time_correl;

	if ((fdi = fopen (infilename, "r")) == (FILE *)NULL)
	  {
		sprintf(logmsg, 
		  "ERROR: Error opening ESA time correlation file %s.\0",
		  infilename);
		syslog (LOG_ERR, logmsg);
		return (ERROR);
	  }

	/*  Take satellite name out of infilename */
        fnlen = strlen(infilename);
        strncpy (sat, &infilename[fnlen-2], 2);
        sat[2] = '\0';
        if ((strcmp(sat, "E1") != 0) && (strcmp(sat, "E2") != 0))
        {
           sprintf (logmsg, 
              "ERROR: filename %s does not end with 'E1' or 'E2'.  Exiting.\n",
	      infilename);
           syslog (LOG_ERR, logmsg);
           return (ERROR);
        }

	/*  Create temporary output filename. 
            We'll have to wait until the end to fill in the right Rev number
            in the filename.  */
        sprintf (tempname, "E0%c_00000.TCE", sat[1]);
  	sprintf (temp_outfilename, "%s/%s", outdir, tempname);
	if ((fdo = fopen (temp_outfilename, "w")) == NULL)
	{
		sprintf(logmsg, 
		  "ERROR: Error opening ESA time correlation file %s.\0",
		  temp_outfilename);
		syslog (LOG_ERR, logmsg);
		return (ERROR);
	}

	/* Read fixed record - ESA header */
	nbytes = 30;
        rstat = fscanf (fdi, "%30c", recordbuff);
        recordbuff[nbytes] = '\0';
        if (rstat == EOF)
          {
            sprintf (logmsg, 
		     "ERROR: Error reading file %s.  Exiting.\n",
		     infilename);
	    syslog (LOG_ERR, logmsg);
	    fclose (fdi);
            fclose (fdo);
	    return (ERROR);
	  }
	if ((int)strlen(recordbuff) < nbytes)
	  {
	    sprintf (logmsg,
	       "ERROR: Invalid no. of bytes read from file %s. Exiting.\n",
	       infilename);
	    syslog (LOG_ERR, logmsg);
	    fclose (fdi);
            fclose (fdo);
	    return (ERROR);
	  }

	strncpy (sat, &recordbuff[20], 2);
	sat[2] = '\0';

        strcpy(minmaxtime.mintime, "2100:000:00:00:00.000");
        strcpy(minmaxtime.minrev, "99999");
        strcpy(minmaxtime.maxtime, "1900:000:00:00:00.000");
        strcpy(minmaxtime.maxrev, "00000");

	/*  Write the common message header */
	memset (msgheader, ' ', 50);
	msgheader[50] = '\0';
        tc_systime2asf(currtime);
        currtime[4] = ' ';

	strncpy (msgheader, currtime, 21);
	strncpy (&msgheader[22], "TC IMS FAI", 10);
	fprintf (fdo, "%s\n", msgheader);

        /* Read in the time corr. record */
	nbytes = 24;
	for (ix = 0; ix < nbytes; ix++)
	  recordbuff[ix] = fgetc(fdi);
	recordbuff[nbytes] = '\0';

	/*  Orbit Number */
	strncpy (temp, recordbuff, 5);
	temp[5] = '\0';
        time_correl.revnum = atoi (temp);

	/*  UTC */
	rev4bytes(&recordbuff[8]);
	utcday = *(long*)&recordbuff[8];
	rev4bytes(&recordbuff[12]);
	utcms = *(long*)&recordbuff[12];

	/*  Convert the ESA UTC time to ASF time */
	utc2asftime (utcday, utcms, time_correl.asftime);
        /* check resulting ASF time  */
        if (!(tc_validate_asf_datetime(time_correl.asftime)))
          {
            sprintf (logmsg,
              "ERROR: Invalid ASF time: %s.\n", time_correl.asftime );
            syslog (LOG_ERR, logmsg);
            sprintf (logmsg,
              "   X_UTC: Days = %d, Msecs = %d\n", utcday, utcms ) ;
            syslog (LOG_ERR, logmsg);
            sprintf (logmsg, "  Time error in SV file %s.  Exiting.\n",
              infilename);
            syslog (LOG_ERR, logmsg);
            fclose (fdi);
            fclose (fdo);
            return (ERROR);
          }

        /* time is O.K.  */
	strcpy(minmaxtime.mintime, time_correl.asftime);
	sprintf(minmaxtime.minrev, "%05d", time_correl.revnum);
	strcpy(minmaxtime.maxtime, time_correl.asftime);
	sprintf(minmaxtime.maxrev, "%05d", time_correl.revnum);

	/*  Pull out Satellite Time Binary Counter Value
	    and  Clock Period of Satellite Time Counter */
	rev4bytes(&recordbuff[16]);
	time_correl.sattime = *(long*)&recordbuff[16];
	rev4bytes(&recordbuff[20]);
	time_correl.clockcycle = *(long*)&recordbuff[20];

	sprintf (timerec, "%-5d %-21s %-11u %-11u ", 
		 time_correl.revnum, time_correl.asftime, 
		 (unsigned long int) time_correl.sattime, 
		 (unsigned long int) time_correl.clockcycle);
	timerec[10] = ' ';
	fprintf (fdo, "%s\n", timerec);

	fclose(fdi);
	fclose(fdo);

        /* Initially, pmf = infile_pmf; parts will change. */
        pmf = infile_pmf;
        strcpy (pmf.format, ASF_STR);

        if (add_to_llist(pmf, &minmaxtime, "TCE", sat, ESA_TCE_HSTR, 
                         tempname) != OK)  return(ERROR);
        sprintf (logmsg, "NOTICE: Completed translating file %s.", infilename);
        syslog (LOG_NOTICE, logmsg);
	return (OK);
}


/*=============================================================================
Function:       gen_ESA_PMF
Description:    Fills up infile_pmf which is then used to create the 
                PMF for the given file.
Parameters:     char *    name of file to be parsed 
                char *    path of input file
                char *    full path and filename of configuration file
Returns:        llist *   (or NULL if there were problems)
Creator:        Cameron Cooper (cooper@ditto.jpl.nasa.gov)
Creation Date:  
Notes:
=============================================================================*/

llist *gen_ESA_PMF(char *path, char *esa_filename, char *config_filename)
{     
  AGGREGATE     orig_root, root ;
  FILE          *PMF_file_ptr ;
  char          fullpathname[255], trfilename[255], fullPMFfilename[255] ;
  char          translation_path[255], bad_filename[255];
  char          PMF_path[255] ;
  int	        i ;	
  int           status ;         /* return code for fa_ascii_rec_processor() */
  int 	        array_size ;
  FILE          *ascii_file_ptr ;
  char          ascii_buffer[651] ;
  char	        logmsg[MAX_SYSLOG_MSGLEN+1];
  PMF_FILE_LIST *filerecord, *fileread ;
  cursor        ptr ;

  sprintf (logmsg, "NOTICE: Processing file %s.\n", esa_filename);
  syslog (LOG_NOTICE, logmsg);
  strcpy (cfname, config_filename);

  rootpath = (char *) get_config_val(config_filename, "FAIF_ROOTPATH");
  if (rootpath == (char *) NULL)
  {
    syslog (LOG_ERR, "ERROR: FAIF_ROOTPATH undefined. Exiting.");
    return (NULL);
  }
  sprintf (translation_path, "%s/ESA/tran", rootpath);
  sprintf (PMF_path, "%s/ESA/PMF", rootpath);
 
  /*  Handle SHAQP as a special case  */
  if (strncmp (esa_filename, "SHQP_", 5) == 0)
  {
    if (shaqp2pmf (path, esa_filename) == OK)  return (bar);
    else return (NULL);
  }

  strcpy(overall_timespan.mintime, "2100:000:00:00:00.000");
  strcpy(overall_timespan.minrev, "99999");
  strcpy(overall_timespan.maxtime, "1900:000:00:00:00.000");
  strcpy(overall_timespan.maxrev, "00000");

  array_size = (sizeof(esa_filetypes)/sizeof(FA_filetype));

  strcpy (infile_pmf.file_name, esa_filename);
  sprintf(fullpathname, "%s/%s", path, esa_filename) ;
  
  /* Parse the header of the ESA file */
  status =  fa_ascii_rec_ingestion(fullpathname, 
				   ascii_file, 
				   HEADER_RECORD_SIZE, 
				   DATA_RECORD_SIZE, 
				   &ascii_file_ptr);
  if (status != FA_ASCII_REC_INGESTION_OK)
  {
     sprintf (logmsg, "ERROR: Unable to parse header in %s.\n", esa_filename);
     syslog (LOG_ERR, logmsg);
     return (NULL);
  }

  /* Fill in the structure for the PMF */
  i = 0;
  for (i = 0; i < array_size; ++i)	
  {
     if (strcmp(esa_filetypes[i].sp_filetype, infile_pmf.FA_file_type) == 0) 
     {  
	strcpy (infile_pmf.gen_file_type, esa_filetypes[i].gen_filetype);
        strcpy (infile_pmf.file_source,esa_filetypes[i].agency);
        strcpy (infile_pmf.format,esa_filetypes[i].format);
        strcpy (infile_pmf.file_dest,esa_filetypes[i].dest);

        /* If the file is MPSG file then get the start and end rev */
        if (strcmp (infile_pmf.FA_file_type,ESA_MPSG_HSTR) == 0)
        {
            fgets (ascii_buffer, 651, ascii_file_ptr);
            strncpy (overall_timespan.minrev, &ascii_buffer[636], 5);
            strncpy (overall_timespan.maxrev, &ascii_buffer[641], 5);
        }
        break;
     }
  }
  
  /* Convert creation time from ASF time format to ODL time format */
  tc_asf2odl (fa_creation_time, infile_pmf.file_creation_time);
  
  /* Get the time stamp on the file and put it into ODL time format */
  strcpy (infile_pmf.file_arrival_time, (char *)PMFfiletime(fullpathname)) ;
  
  /* Create the PMF */
  /*  PrintLabel(orig_root) ;    */

  /* Make a link list that has filename and PMF filename */
  bar = create_dyn_llist();
  filerecord = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
  strcpy (filerecord->file_name, esa_filename);
  sprintf(filerecord->PMF_file_name,"%s.M",esa_filename);
  strcpy (filerecord->dataset_suffix, infile_pmf.FA_file_type);
  filerecord->orig_not_tran = TRUE;
  APPEND(bar, filerecord, free, filerecord);
  fclose(ascii_file_ptr);

  sprintf (fullPMFfilename, "%s/%s", PMF_path, filerecord->PMF_file_name);

  /* If the file needs translation, translate it and create the PMF for the
     translation file(s) and add to the linked list */
  if ((strcmp(infile_pmf.FA_file_type, ESA_ORPD_HSTR) == 0) ||
      (strcmp(infile_pmf.FA_file_type, ESA_ORRE_HSTR) == 0))
  {
     if (translate_ESA_stvec (fullpathname, translation_path) != OK)
     {
        sprintf (logmsg, "Unable to translate %s.\n", esa_filename);
        syslog (LOG_ERR, logmsg);
        delete_translated_files ();
        return (NULL);
      }
   }
   if (strcmp(infile_pmf.FA_file_type, ESA_PATC_HSTR) == 0)
   {
      if (translate_ESA_timecorr(fullpathname, translation_path) != OK)
      {
         sprintf (logmsg, "Unable to translate %s.\n", esa_filename);
         syslog (LOG_ERR, logmsg);
         delete_translated_files ();
         return (NULL);
      }
   }

  /* Write the PMF for the input file */

  if ((PMF_file_ptr = fopen (fullPMFfilename, "w")) == (FILE *)NULL)
  {
    sprintf(logmsg, "ERROR: Unable to open PMF file %s.\0", fullPMFfilename);
    syslog (LOG_ERR, logmsg);
    delete_translated_files ();
    return (NULL);
  }

  /*  Fill in timespan info from overall_timespan  */
  if (strcmp (overall_timespan.minrev, "99999") == 0)
    overall_timespan.minrev[0]  = NULL;
  if (strcmp (overall_timespan.maxrev, "00000") == 0)
    overall_timespan.maxrev[0]  = NULL;
  if (strcmp (overall_timespan.mintime, "2100:000:00:00:00.000") == 0)
    overall_timespan.mintime[0] = NULL;
  if (strcmp (overall_timespan.maxtime, "1900:000:00:00:00.000") == 0)
    overall_timespan.maxtime[0] = NULL;
  strcpy(infile_pmf.valid_start_time, overall_timespan.mintime) ;
  strcpy(infile_pmf.valid_end_time, overall_timespan.maxtime) ;
  strcpy(infile_pmf.start_rev, overall_timespan.minrev) ;
  strcpy(infile_pmf.end_rev, overall_timespan.maxrev) ;

  orig_root = (AGGREGATE)create_PMF(&infile_pmf);
  WriteLabel (PMF_file_ptr, orig_root);
  fclose(PMF_file_ptr);
  RemoveAggregate(orig_root) ;

  sprintf (logmsg, "NOTICE: Completed processing file %s.\n", esa_filename);
  syslog (LOG_NOTICE, logmsg);

  /* Return the linked list */
  return(bar);
} 



/*==============================================================================
Function:	add_to_llist
Description:	Fix the output filename of the new translated file,
                Create a PMF file for it, and append these to the llist.
Parameters:     ODL_Catalog_Metadata  *pmf          For the translated file
                timespan_rec          *minmaxtime   For this file only
                char                  *filetag      For this file
                char                  *sat          "E1" or "E2"
                char                  *dataset      For this file
                char                  *filename     For this file
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Jan. '96
Notes:		
==============================================================================*/

int add_to_llist (ODL_Catalog_Metadata *pmf, timespan_rec *minmaxtime, 
                  char *filetag, char *sat, char *dataset, char *filename)
{
  int             stat;
  char            logmsg[255];
  AGGREGATE       root;
  PMF_FILE_LIST  *filerecord;
  int             year, decade, day, hour, min, sec, msec;
  FILE           *pmf_file;
  char            oldname[255], newname[255];
  char            fullPMFfilename[255];
  time_t          current_time;
  time_t         *null_ptr = NULL;
  int             status;

  if ((strcmp(minmaxtime->minrev, "99999") == 0) || 
      (strcmp(minmaxtime->maxrev, "00000") == 0) ||
      (strcmp(minmaxtime->mintime, "2100:000:00:00:00.000") == 0) ||
      (strcmp(minmaxtime->maxtime, "1900:000:00:00:00.000") == 0))
  {
     sprintf (logmsg, 
              "ERROR: Unable to determine time range for %s file.\n",
              filetag);
     syslog (LOG_ERR, logmsg);
     delete_translated_files();
     return (ERROR);
  }

  /* Fix the output filename, now that we know the rev to use */
  sprintf (pmf->file_name, "%s_%s.%s", sat, minmaxtime->minrev, filetag);
  sprintf (oldname, "%s/ESA/tran/%s", rootpath, filename);
  sprintf (newname, "%s/ESA/tran/%s", rootpath, pmf->file_name);
  stat = rename (oldname, newname);
  if (stat != 0)
  {
        sprintf (logmsg,
          "ERROR: Unable to rename output file: %s.\n", filename);
        syslog (LOG_ERR, logmsg);
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
  if (strcmp (pmf->start_rev, overall_timespan.minrev) < 0)
  {
    strcpy (overall_timespan.minrev, pmf->start_rev);
    strcpy (overall_timespan.mintime, pmf->valid_start_time);
  }
  if (strcmp (pmf->end_rev, overall_timespan.maxrev) > 0)
  {
    strcpy (overall_timespan.maxrev, pmf->end_rev);
    strcpy (overall_timespan.maxtime, pmf->valid_end_time);
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
  sprintf (fullPMFfilename, "%s/ESA/PMF/%s", 
           rootpath, filerecord->PMF_file_name);
  if ((pmf_file = fopen(fullPMFfilename, "w")) == (FILE *)NULL)
  {
    sprintf (logmsg, "WARNING: Error opening PMF file %s.  Exiting.\n",
             fullPMFfilename);
    syslog (LOG_ERR, logmsg);
    delete_translated_files();
    return (ERROR);
  }
  WriteLabel (pmf_file, root);
  fclose(pmf_file);
  RemoveAggregate(root);

  sprintf (logmsg, "NOTICE: Completed translation file %s.", newname);
  syslog (LOG_NOTICE, logmsg);

  return (OK);
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

  sprintf (PMF_path, "%s/ESA/PMF", rootpath);
  sprintf (translation_path, "%s/ESA/tran", rootpath);

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
Function:	one_sv
Description:	Translates one ESA state vector and writes a version in
                ASF format.
Parameters:     char *         buff         one ESA state vector from infile
                char *         stv_str      output line
                char *         sv_time      of this vector
                int *          curr_rev     of this vector
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Feb. '97
Notes:		Adapted from ACS:
                  [bld.acs.load.fadirmon]ESASTV.QC
                  [bld.acs.lib.system]CHECK_DATE.C
                  [bld.acs.lib.stvec]SVPM2EQX.FOR
                "The ESA state vectors (114) are converted into 112.
                There is no change in the fundamental plane (xy plane)."
==============================================================================*/
int one_sv (char *buff, char *stv_str, char *sv_time, int *curr_rev)
{

   char	        logmsg[MAX_SYSLOG_MSGLEN+1];
   char		temp[10];
   int		itmp;
   int		utcday, utcms;
   double	xa[6], xb[6];
   char         ghatime[22];  /* for get_gha */
   double 	db_gha, gha, v[3], ve[3];
   int	        revnum;
   char	        asftime[22];
   double	xpos, ypos, zpos, xvel, yvel, zvel;

   /*  from vector library June '89 via PSCONS:  */
   double 	eroti = 1.002737909300; 
   double 	pi    = 3.14159265358979323846264338327950000; 

   strncpy (temp, buff, 5);
   temp[5] = '\0';
   revnum = atoi (temp);

   /*  UTC */
   rev4bytes(&buff[8]);
   utcday = *(long*)&buff[8];
   rev4bytes(&buff[12]);
   utcms = *(long*)&buff[12];

   /*  Convert the ESA time to ASF time */
   utc2asftime (utcday, utcms, asftime);
   /* check resulting ASF time  */
   if (!(tc_validate_asf_datetime(asftime))) 
   { 
     sprintf (logmsg, "ERROR: Invalid ASF time in stvec: %s.\n", asftime );
     syslog (LOG_ERR, logmsg);
     sprintf (logmsg, 
	      "   ESA X_STATE_VECTOR:  X_UTC: Days = %d, Msecs = %d\n", 
	      utcday, utcms ) ;
     syslog (LOG_ERR, logmsg);
     syslog (LOG_ERR, " Time error in SV file. Exiting.\n");
     return (ERROR);
   }
   /* time is O.K.  */
   strcpy (sv_time, asftime);

   /* The state vectors are in km and m/sec. */

   /*  The following note is from the  original ACS module:  */
   /*  Note:  All we currently know about the
       integers and reals is how many bytes they
       take.  The following can read from a VAX
       file but I didn't know what kind of computer
       ESA will be using */
   /*  But note that rev4bytes is new, to handle the byte-swapping needed
       from VAX to Sun */

   rev4bytes(&buff[16]);
   itmp = *(long*)&buff[16]; xpos = itmp / 100000.00000;
   rev4bytes(&buff[20]);
   itmp = *(long*)&buff[20]; ypos = itmp / 100000.00000;
   rev4bytes(&buff[24]);
   itmp = *(long*)&buff[24]; zpos = itmp / 100000.00000;

   rev4bytes(&buff[28]);
   itmp = *(long*)&buff[28]; xvel = itmp / 100000000.00000;
   rev4bytes(&buff[32]);
   itmp = *(long*)&buff[32]; yvel = itmp / 100000000.00000;
   rev4bytes(&buff[36]);
   itmp = *(long*)&buff[36]; zvel = itmp / 100000000.00000;

   /*  Put the state vectors into an array to input to cotrns */
   xa[0] = xpos; xa[1] = ypos; xa[2] = zpos;
   xa[3] = xvel; xa[4] = yvel; xa[5] = zvel;

   /*  Now do: svpm2eqx (&asftime, &xa, &xb);                 */
   /*  Taken from ACS:  [bld.acs.lib.stvec]SVPM2EQX.FOR
    *  svpm2eqx transforms the state vectors xa
    *  from true of date earth equator prime meridian 
    *  to true of date earth equator equinox xb.  
    *
    *     asftime is the time in asf format of the state vector.  
    *     xa are the input vectors in true of date earth equator 
    *     prime meridian.  
    *     As vectors in units of km, sec:  
    *		r(x,y,z), v(x,y,z)
    *
    *		xa(0) = x coordinate of position vector (km)
    *		xa(1) = y coordinate of position vector (km)
    *		xa(2) = z coordinate of position vector (km)
    *		xa(3) = x coordinate of velocity vector (km/sec)
    *		xa(4) = y coordinate of velocity vector (km/sec)
    *		xa(5) = z coordinate of velocity vector (km/sec)
    *     xb are the output orbit vectors true of date earth equator 
    *     equinox.  Same structure as xa.
   */                                                                   

   /* get the Greenwich Hour Angle */
   if (get_gha(asftime, ghatime, &db_gha, cfname) != OK)
   {
     syslog (LOG_ERR, "ERROR: Can't get GHA.  Exiting.");
     return (ERROR);
   }  
   gha_interp(asftime, ghatime, db_gha, &gha);
   gha = gha * (pi/180.0);  /* convert degrees to radians */
   /*  Need to rotate the vector GHA degrees to the east (ccw) 
       about the x-axis  */
   xb[0] = xa[0]*cos(gha) - xa[1]*sin(gha);
   xb[1] = xa[0]*sin(gha) + xa[1]*cos(gha);
   xb[2] = xa[2];

   /*
    *  Now the velocity.  
    *  v = va + ve 
    *  where v is the velocity fixed/non-rotational coord. In earth 
    *  equator equinox true of date.
    *	va is the velocity in rotational earth equator prime meridian 
    *		true of date.
    *	ve is the velocity of the coordinate system at xa due to 
    *		rotation.  (earth equator prime meridian true of date.)
    *	
    *	ve = vroti * xa
    *		where vroti is the earth rotation rate vector and xa is
    *		the position of the spacecraft.  And x is the vector 
    *		cross product.  
    *		vroti = (0,0,eroti*2pi*86400)
    *		using radians per second to go with km per sec.
   */

   ve[0] = - (eroti * (2.0*pi) / 86400.0) * xa[1];
   ve[1] =   (eroti * (2.0*pi) / 86400.0) * xa[0];
   ve[2] = 0.0;
   v[0] = xa[3] + ve[0];
   v[1] = xa[4] + ve[1];
   v[2] = xa[5] + ve[2];

   /*  Now we have v, the velocity in non-rotational frame.  
       Now we must rotate it to earth equator exquinox.  */
   xb[3] = v[0]*cos(gha) - v[1]*sin(gha);
   xb[4] = v[0]*sin(gha) + v[1]*cos(gha);
   xb[5] = v[2];

   xpos = xb[0]; ypos = xb[1]; zpos = xb[2];
   xvel = xb[3]*1000.00; yvel = xb[4]*1000.00; zvel = xb[5]*1000.00;

   /*  Write to stv_str   (from ACS MKSTVFILE.QC)  */ 
   asftime[4] = ' ';
   sprintf (stv_str,
	    "%05d %21s %-11.5f %-11.5f %-11.5f %-11.5f %-11.5f %-11.5f \n",
  	    revnum, asftime, xpos, ypos, zpos, xvel, yvel, zvel);

   *curr_rev = revnum;
   return (OK);
}


/* End of File */
