/*************************************************************************
 * Copyright (c)1995 California Institute of Technology. U.S. Government *
 * Sponsorship acknowledged.                                             *
 *************************************************************************/

/*==============================================================================
Filename:	ADEOS2PMF.c

Description:    Contains the funtions necessary for parsing ADEOS 
                files and creating the PMF.

External Functions:
	generate_ADEOS_metadata

Static Functions:
	None
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:
1.  May '96 - R. Hoffman
    Modified translate_adeos_elmp():
    The "agency" field in an ELMP file header must now be "****"
    (per Version 5.2 of ADEOS SIS)
2.  May '96 - R. Hoffman
    Corrected a pointer dereferencing error in asf2adeosrev()
3.  June '96 - R. Hoffman
    Added a check for not finding appropriate entry in filetypes table
4.  June '97 - R. Hoffman
    Reverse the linked list, putting original file last; clean up syslog msgs.
==============================================================================*/

static char SccsFile[] = "ADEOS2PMF.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "10 Jun 1996";
static char SccsLastChanger[] = "@(#)ADEOS2PMF.c	1.6";
static char SccsState[] = "1.6";

#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absloute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <errno.h>          /* for errno definitions...             */
#include <stdlib.h> 	    /* for getopt		    	    */

#include "GENconversions.h"
#include "faifdefs.h"
#include "ADEOS.h"
#include "PMF.h"
#include "ADEOS2PMF.h"
#include "dapps_list.h"
#include "nmalloc.h"

#define  FA_ASCII_REC_PROCESS_OK    0
#define  FA_ASCII_REC_INGESTION_OK  0

char FA_creation_time[] = "hh:mm:ss";
char FA_creation_date[] = "yyyymmdd";
char begin_date[]       = "yyyymmdd";
char end_date[]         = "yyyymmdd";

int llist_errno;   /* to make make happy ?? */

ODL_Catalog_Metadata PMF_struct;

FA_filetype filetypes[] =
{        
  {ADEOS_REQR_HSTR, GFTYPE_FAPLAN,   ADEOS_STR, ORIG_STR, A_SAT, ASF_STR, NO_STR},
  {ADEOS_RDRD_HSTR, GFTYPE_TAPE_RD,  ADEOS_STR, ORIG_STR, A_SAT, ASF_STR, NO_STR},
  {ADEOS_ELMD_HSTR, GFTYPE_RSV,      ADEOS_STR, ASF_STR,  A_SAT, ASF_STR, NO_STR},
  {ADEOS_ELMP_HSTR, GFTYPE_PSV,      ADEOS_STR, ASF_STR,  A_SAT, ASF_STR, NO_STR},
  {ADEOS_OPL1_HSTR, GFTYPE_FASKED,   ADEOS_STR, ORIG_STR, A_SAT, ASF_STR, NO_STR},
  {ADEOS_RPLN_HSTR, GFTYPE_FAPLAN,   ADEOS_STR, ORIG_STR, A_SAT, ASF_STR, NO_STR},
  {ADEOS_ORST_HSTR, GFTYPE_FAOPRES,  ADEOS_STR, ORIG_STR, A_SAT, ASF_STR, NO_STR},
  {ADEOS_STAD_HSTR, GFTYPE_SAT_STAT, ADEOS_STR, ORIG_STR, A_SAT, ASF_STR, NO_STR},
  {ADEOS_STGS_HSTR, GFTYPE_STN_STAT, ADEOS_STR, ORIG_STR, A_SAT, ASF_STR, NO_STR},
  {ADEOS_REAC_HSTR, GFTYPE_ACQ_REP,  ADEOS_STR, ORIG_STR, A_SAT, ASF_STR, NO_STR},
  {ADEOS_SRRD_HSTR, GFTYPE_SHIP_REP, ADEOS_STR, ORIG_STR, A_SAT, ASF_STR, NO_STR},
  {NULL,	    NULL,	     NULL,      NULL,     NULL,  NULL,    NULL}
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
  {FILE_HEADER,	REPORT_RECORD,   0, 4, gen_string2str, NULL,(int) PMF_struct.FA_file_type} ,
  {FILE_HEADER,	REPORT_HEADER,  28, 8, gen_string2str, NULL,(int) FA_creation_date},
  {FILE_HEADER,	REPORT_HEADER,  37, 8, gen_string2str, NULL,(int) FA_creation_time},
  {FILE_HEADER,	REPORT_HEADER,  57, 8, gen_string2str, NULL,(int) begin_date},
  {FILE_HEADER,	REPORT_HEADER,  66, 8, gen_string2str, NULL,(int) end_date},
  {0, 0, -1, -1, NULL, NULL, NULL} 
};                                                           



/*==============================================================================
Function:	get_phasedata
Description:	Get satellite phase information from the configuration file.
Parameters:     char   *cfile       configuration file name
                pdata  *pd          phase information
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Dec. '95
Notes:		
==============================================================================*/

int get_phasedata (char *cfile, pdata *pd)
{
static char *pstart = NULL;
static char *prevs = NULL;
static char *plastrev = NULL;
static char *cdays = NULL;
static char *crevs = NULL;
FILE    *config_file;

  /* Get ADEOS phase information from configuration file */
  if ((config_file = fopen(cfile, "r")) == (FILE *) NULL)
  {
     syslog (LOG_ERR, "ERROR: Error opening config file %s.  Exiting.\n", 
             cfile);
     return (ERROR);
  }
  fclose (config_file);
  pstart   = (char *) get_config_val(cfile, "PHASE_START");
  prevs    = (char *) get_config_val(cfile, "PHASE_REVS");
  plastrev = (char *) get_config_val(cfile, "PHASE_LASTREV");
  cdays    = (char *) get_config_val(cfile, "PHASE_CYCLEDAYS");
  crevs    = (char *) get_config_val(cfile, "PHASE_CYCLEREVS");
  if ((pstart == NULL) || (prevs == NULL) || (plastrev == NULL) ||
      (cdays == NULL) || (crevs == NULL))
  {
     syslog (LOG_ERR, "ERROR: Unable to find phase info.  Exiting.\n");
     return (ERROR);
  }
  strcpy (pd->pstart, pstart);
  pd->iprevs    = atoi (prevs);
  pd->iplastrev = atoi (plastrev);
  pd->icdays    = atoi (cdays);
  pd->icrevs    = atoi (crevs);
  if ((pd->iprevs < 1) || (pd->iplastrev < 1) || (pd->icdays < 1) ||
      (pd->icrevs < 1))
  {
     syslog (LOG_ERR, "ERROR: Invalid phase info.  Exiting.\n");
     return (ERROR);
  }
return (OK);
}



/*==============================================================================
Function:	asf2adeosrev
Description:	Given an asftime and a state vector z-value, and the 
                satellite phase information from the configuration file,
                determines the rev #.
Parameters:     char   *asftime     yyyy:ddd:hh:mm:ss.sss
                double z            state vector's z-position coordinate
                pdata  *pd          phase information
                int    *rev         output parameter: rev #
Returns:	0 (OK) or -1 (ERROR)
Creator:	Rodney Hoffman
Creation Date:	Oct. '95
Notes:		(adapted from ACS: [bld.mps.lib.src]asf2rev.qf
==============================================================================*/

int asf2adeosrev(char *asftime, double z, pdata *pd, int *rev1)
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
  tc_asf2julian(pd->pstart, &etp1);

  /* Compute nodal period, tnode */
  tnode = pd->icdays;
  tnode = tnode / pd->icrevs;

  /* OK - et is within this phase */
  etdif = et - etp1;

  /* number of complete revs since first node of phase: */
  nrevs = (etdif / tnode);

  /* rev number of the current nominal rev: */
  *rev1 = (pd->iplastrev - pd->iprevs + 1) + nrevs;

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

  /* No records were found.   */
  return (-1);
}     /* end asf2adeosrev */




/*==============================================================================
Function:	read_adeos_sv
Description:	Reads one ADEOS state vector record and translates it into
                ASF format and writes it to the output file.
Parameters:     FILE      *fin        input file
                FILE      *fout       output file
                *timespan minmaxtime  time range
                *pd       pdata       ADEOS phase data
                *int      curr_rev    current rev #
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	Oct. '95
Notes:		
==============================================================================*/

   int read_adeos_sv (FILE *fin, FILE *fout, timespan *minmaxtime, 
                      pdata *pd, int *curr_rev)
   {
	char		temp[20];
	char		buffer[MAXRECLEN];    /* one input state vector */
  	double		xpos, ypos, zpos;     /* position */
  	double		xvel, yvel, zvel;     /* velocity */
  	char		strttime[22];
  	int		revnum;
	double		mjd, mjd2;
	int		stat;

        fgets (buffer, SVRECLEN + 1, fin);	

	/*  Pull out the time and convert it into asf time */
	strncpy (temp, buffer, 14);
	temp[14] = '\0';
	mjd = atof (temp);
        mjd2 = mjd + 2400000.5;
	tc_julian2asf (&mjd2, &strttime);
	strttime[21] = '\0';

	/* Get the z position */
	strncpy (temp, &buffer[43], 13);
	temp[13] = '\0';
	zpos = atof (temp);

	/* Figure out the rev number from the time and z position */
	if (asf2adeosrev (strttime, zpos, pd, &revnum) != 0)
	{
       	  syslog (LOG_ERR, "ERROR: Error in call to asf2adeosrev.  Exiting.\n");
	  return (ERROR);
	}

	/* Use only the first vector per rev */
        if (revnum == *curr_rev) return(OK);

	strncpy (temp, &buffer[15], 13);
	temp[13] = '\0';
	xpos = atof (temp);
	strncpy (temp, &buffer[29], 13);
	temp[13] = '\0';
	ypos = atof (temp);
	strncpy (temp, &buffer[43], 13);
	temp[13] = '\0';
	zpos = atof (temp);
	strncpy (temp, &buffer[57], 10);
	temp[10] = '\0';
	xvel = atof (temp) * 1000.00;
	strncpy (temp, &buffer[68], 10);
	temp[10] = '\0';
	yvel = atof (temp) * 1000.00;
	strncpy (temp, &buffer[79], 10);
	temp[10] = '\0';
	zvel = atof (temp) * 1000.00;

	/* write it out */
        strttime[4] = ' ';

        stat = fprintf (fout,
                "%05d %21s %-11.5f %-11.5f %-11.5f %-11.5f %-11.5f %-11.5f \n",
                revnum, strttime, xpos, ypos, zpos, xvel, yvel, zvel);

        if (stat < 0)
        {
            syslog (LOG_ERR, "Error writing state vector record.\n");
            fclose (fin);
            return (ERROR);
        }

        if (*curr_rev == 0)      /* First vector -- get mintime, minrev */
        {
           strcpy (minmaxtime->mintime, strttime);
           sprintf (minmaxtime->minrev, "%05d", revnum);
        }
        /* Update maxtime, maxrev.  Last vector = last update = max */
        strcpy (minmaxtime->maxtime, strttime);
        sprintf (minmaxtime->maxrev, "%05d", revnum);
	*curr_rev = revnum;
	return (OK);
   }    /* end read_adeos_sv */




/*==============================================================================
Function:	translate_adeos_elmp
Description:	Translate the Orbit Data (ELMP) file from NASDA.  ELMP 
                contains ADEOS predicted state vectors.
Parameters:     char *               infilename   path and name of input file
                ODL_Catalog_Metadata *pmf         PMF for output file
                char *               outpath      path of output file
                pdata *              pd           phase data struct
Returns:	
Creator:	Rodney Hoffman
Creation Date:  Oct. '95
Notes:		adapted from translate_nasda_elmf (see NASDA2PMF)
==============================================================================*/

int translate_adeos_elmp (char *infilename, ODL_Catalog_Metadata *pmf,
			  char *outpath, pdata *pd)
{

int  	curr_rev;
int	stat;
int	i;
int     year, decade, day, hour, min, sec, msec;
char	buffer[MAXRECLEN];
char    precstr[] = "PREDICTED";
char    temp_outfilename[] = "ADEOS01_00000.PSV";
char    oldname[MAXLINE], newname[MAXLINE];
char	temp[20];
FILE	*fd;
FILE    *fout;
timespan minmaxtime;

  strcpy (minmaxtime.minrev, "00000");
  strcpy (minmaxtime.maxrev, "99999");
  strcpy (minmaxtime.mintime, "2010:000:00:00:00.000");
  strcpy (minmaxtime.maxtime, "1900:000:00:00:00.000");
  curr_rev = 0;

  sprintf (oldname, "%s/%s", outpath, temp_outfilename);

  if ((fd = fopen(infilename, "r")) == (FILE *)NULL)
  {
     syslog (LOG_ERR, "ERROR: Error opening file %s.  Exiting.\n", infilename);
     return (ERROR);
  }

  /* Read the header record */
  if (fgets (buffer, HDRRECLEN + 1, fd) == NULL) 
  {
     syslog (LOG_ERR, "ERROR: Error reading file %s.  Exiting.\n", infilename);
     fclose (fd);
     return (ERROR);
  }

  /* Validate the fixed fields */
  if (strncmp(buffer,      "ELMP",   4) != 0 ||
      strncmp(&buffer[11], "ADEOS ", 6) != 0 ||
      strncmp(&buffer[18], "HMMO",   4) != 0 ||
      strncmp(&buffer[23], "****",   4) != 0 ||
/*      strncmp(&buffer[23], "ASF ",   4) != 0 ||      */
      strncmp(&buffer[46], "  90",   4) != 0 ||
      strncmp(&buffer[51], " 1440",  5) != 0)
  {
     syslog (LOG_ERR, "Error validating ADEOS ELMP file: %s, hdr record.\0", 
	      infilename);
     fclose (fd);
     return (ERROR);
  }

  /* Process the Predicted State Vectors */
  begin_stvfile ("IMS", "A1", precstr, temp_outfilename, outpath, &fout);
  for (i = 0; i < 1440; i++)
  {
     if (read_adeos_sv(fd, fout, &minmaxtime, pd, &curr_rev) != OK)
     {
        syslog (LOG_ERR, "Error appending new pred. SV no. %d from file %s.", 
		i+1, infilename);
        fclose (fd);
        return (ERROR);
     }
  }

  /* Close the files */
  fclose (fd);
  fclose (fout);
  /* Fix the output filename, now that we know the rev to use */
  sprintf (pmf->file_name, "ADEOS01_%s.PSV", minmaxtime.minrev);
  sprintf (oldname, "%s/%s", outpath, temp_outfilename);
  sprintf (newname, "%s/%s", outpath, pmf->file_name);
  stat = rename (oldname, newname);
  if (stat != 0)
  {
     syslog (LOG_ERR, "ERROR: Unable to rename output file: %s.\n", 
              temp_outfilename);
     return (ERROR);
  }

  /*  Fill in timespan info */
  strcpy (pmf->start_rev, minmaxtime.minrev);
  strcpy (pmf->end_rev, minmaxtime.maxrev);
  if (tc_parse_asftime(minmaxtime.mintime,
        &year, &decade, &day, &hour, &min, &sec, &msec))
  {
    sprintf (pmf->valid_start_time, "%04d-%03dT%02d:%02d:%02d.%03d\0",
                year, day, hour, min, sec, msec);
  }
  if (tc_parse_asftime(minmaxtime.maxtime,
        &year, &decade, &day, &hour, &min, &sec, &msec))
  {
    sprintf (pmf->valid_end_time, "%04d-%03dT%02d:%02d:%02d.%03d\0",
                year, day, hour, min, sec, msec);
  }

  syslog (LOG_NOTICE, "NOTICE: Completed processing %s from file %s.",
           pmf->gen_file_type, infilename);
  syslog (LOG_NOTICE, "NOTICE: Translated file is %s.", newname);
  return (OK);
}    /* end translate_adeos_elmp */




/*=============================================================================
Function:       gen_ADEOS_PMF
Description:    Fills up PMF_struct which is then used to create the 
                PMF for the given file.
Parameters:     char *     name of file to be parsed
                char *     path of reception directory 
                char *     name of configuration file
Returns:        linked list, each item contains data filename and PMF filename
                (or NULL if there are problems)
Creator:        Cameron Cooper
Creation Date:  Aug. '95
Notes:          
=============================================================================*/

llist *gen_ADEOS_PMF(char *path, char *ADEOS_ascii_file, char *cfile)
{     
  AGGREGATE     root;
  FILE          *PMF_file_ptr;
  char          asf_creation_time[] =  "yyyy:ddd:hh:mm:ss.mmm";
  char          asf_start_time[] =     "yyyy:ddd:hh:mm:ss.mmm";
  char          asf_end_time[] =       "yyyy:ddd:hh:mm:ss.mmm";
  char          fullpathname[MAXLINE], trfilename[MAXLINE], 
                   fullPMFfilename[MAXLINE], bad_file[MAXLINE],
                   PMF_path[MAXLINE], translation_path[MAXLINE] ;
  int	        i;	
  int 	        array_size;
  int           s_rev, e_rev;
  int           return_code ;
  int           f_ptr;
  char          *rootpath = NULL;
  FILE          *ascii_file_ptr;
  llist         *bar;
  PMF_FILE_LIST *filerecord, *fileread;
  cursor        ptr;
  pdata         pd;

  syslog (LOG_NOTICE, "NOTICE: Processing file %s.\n", ADEOS_ascii_file);

  rootpath = (char *) get_config_val(cfile, "FAIF_ROOTPATH");
  if (rootpath == (char *) NULL)
  {
    syslog (LOG_ERR, "ERROR: FAIF_ROOTPATH not defined. Exiting.");
    return (NULL);
  }

  if (get_phasedata(cfile, &pd) != OK)
  {
    syslog (LOG_ERR, "ERROR: Unable to get ADEOS phase data.  Exiting.");
    return (NULL);
  }

  sprintf (PMF_path, "%s/ADEOS/PMF", rootpath) ;
  sprintf (translation_path, "%s/ADEOS/tran", rootpath);

  array_size = (sizeof(filetypes)/sizeof(FA_filetype));

  /* Parse the header of the ADEOS file  */
  sprintf(fullpathname, "%s/%s", path, ADEOS_ascii_file) ;
  if (fa_ascii_rec_ingestion(fullpathname, ascii_file, 
	 HEADER_RECORD_SIZE, DATA_RECORD_SIZE, &f_ptr) != 0)
  {
    syslog(LOG_ERR, "ERROR: Errors in processing file.  Exiting.");
    return (NULL);
  }

  ascii_file_ptr = (FILE *)f_ptr;

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
	
  if (i == array_size)  /* if not found in table */
  {
     syslog (LOG_ERR, "ERROR: Unknown filetype %s.\n", PMF_struct.FA_file_type);
     return (NULL);
  }

  strcpy (PMF_struct.file_name, ADEOS_ascii_file);

  /* time conversions  */
  tc_yyyymmdd_hhmmss2asf(FA_creation_date, FA_creation_time,asf_creation_time);
  tc_asf2odl (asf_creation_time, PMF_struct.file_creation_time);

  /* Get the time stamp on the file and put it into ODL time format  */
  strcpy(PMF_struct.file_arrival_time, (char *)PMFfiletime(&fullpathname)) ;
  
  /* Convert ADEOS times into ASF and then into ODL  */
  tc_yyyymmdd_hhmmss2asf (begin_date, "00:00:00",asf_start_time);
  tc_asf2odl (asf_start_time, PMF_struct.valid_start_time);
  tc_yyyymmdd_hhmmss2asf (end_date, "23:59:59",asf_end_time);
  tc_asf2odl (asf_end_time, PMF_struct.valid_end_time);

  asf2adeosrev(asf_start_time, 1, &pd, &s_rev);
  asf2adeosrev(asf_end_time, 1, &pd, &e_rev);
  sprintf(PMF_struct.start_rev,"%d", s_rev) ;
  sprintf(PMF_struct.end_rev  ,"%d", e_rev) ;

  /* Create the PMF  */
  root = (AGGREGATE)create_PMF(&PMF_struct);

  /* Make a linked list that has the filename and PMF filename  */
  bar = create_dyn_llist();
  filerecord = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
  strcpy (filerecord->file_name, ADEOS_ascii_file);
  sprintf(filerecord->PMF_file_name,"%s.M",ADEOS_ascii_file);
  strcpy (filerecord->dataset_suffix, PMF_struct.FA_file_type);
  filerecord->orig_not_tran = TRUE;
  APPEND(bar, filerecord, free, filerecord);

  /* Write the PMF out to the PMF file and close both files  */
  sprintf(fullPMFfilename, "%s/%s", PMF_path, filerecord->PMF_file_name);
  PMF_file_ptr = fopen(fullPMFfilename,"w");
  if (PMF_file_ptr == NULL)
  {
    syslog (LOG_ERR, "Unable to open PMF file.");
    return (NULL);
  }
  WriteLabel (PMF_file_ptr, root);
  fclose(PMF_file_ptr);
  fclose(ascii_file_ptr);
  RemoveAggregate(root);

  /*  If it's an ELMP file, do the translation (and create the
      corresponding PMF) and add it to the linked list.  */
  if (strcmp(PMF_struct.FA_file_type, ADEOS_ELMP_HSTR) == 0) 
  {
     strcpy (PMF_struct.format, ASF_STR);
     PMF_struct.file_name[0] = NULL;
     if (translate_adeos_elmp(fullpathname, &PMF_struct, translation_path, &pd)
          == OK)
     {
        sprintf (trfilename, "%s/%s", translation_path, PMF_struct.file_name);
        strcpy (PMF_struct.file_arrival_time, (char *)PMFfiletime(trfilename));
        strcpy (PMF_struct.file_creation_time, PMF_struct.file_arrival_time);
        root = (AGGREGATE)create_PMF(&PMF_struct);
        filerecord = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
        strcpy (filerecord->file_name, PMF_struct.file_name);
        strcpy (PMF_struct.file_source,filetypes[i].agency);
        sprintf (filerecord->PMF_file_name, "%s.M", PMF_struct.file_name);
        strcpy(filerecord->dataset_suffix, "STATE VECTORS") ;
        filerecord->orig_not_tran = FALSE;
        PREPEND(bar, filerecord, free, filerecord);
        sprintf (fullPMFfilename, "%s/%s", PMF_path, filerecord->PMF_file_name);
        PMF_file_ptr = fopen (fullPMFfilename, "w");
        WriteLabel (PMF_file_ptr, root);
        fclose (PMF_file_ptr);
     }
     else
     {
        syslog (LOG_ERR, "ERROR: Problems in translating %s\n", fullpathname);
        remove (PMF_struct.file_name);
        fileread = FIRST(bar, ptr);
        while (fileread)
        {
          sprintf (bad_file, "%s/%s", translation_path, fileread->file_name);
          remove (bad_file);
          sprintf (bad_file, "%s/%s", PMF_path, fileread->PMF_file_name);
          remove (bad_file);
          fileread = NEXT(bar, ptr) ;
        }
        return (NULL);
     }
  }

  syslog (LOG_NOTICE, "NOTICE: Completed processing file %s.\n", 
          ADEOS_ascii_file);

  /* Return the linked list. */
  return(bar);

} /* gen_ADEOS_PMF */

/* End of File */
