/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* rc_uts.c - This module contains functions for reading
	records from AOS formatted files.
*/

#include <procfil.h>
#include <procdec.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

/* data associated with current job request */
extern SV sv1,sv2;		/* two state vectors */
extern TC_FILE tcf;		/* time correlation element */


static FILE    *infptr;
static int rc_field_count;	/* shows which field failed */

int    rc_toupper_sw = 1;   /* 1 = convert lower case to upper */


/* rc_open_infile(filename)--------------------------------------
	This routine opens the file filename for read and
	returns PASS if successful.
*/

int rc_open_infile(filename)
char    *filename;
{
    if ((infptr = fopen(filename,"r")) == NULL)
    {
	printf("Unable to open file %s\n",filename);
	return(FAIL);
    }
/*
    printf("Opening file %s\n",filename);
*/
    return(PASS);
}



/* rc_close_infile()--------------------------------------------
	This routine closes the file pointed to by infptr.
*/

void rc_close_infile()
{
    if (infptr != NULL)
	fclose(infptr);
}



/* rc_read_field(strg,size)--------------------------------------
	This routine reads size number of bytes from the
	file pointed to by infptr.  size - 1 bytes are placed
	in strg.  If the last character read is a blank or 
	newline, it is replaced with a null.  If a different
	character is encountered as the last character, the
	routine returns FAIL.
*/

int rc_read_field(strg,size)
char    *strg;
int     size;
{
    char   *p;
    int    c;   /* Character being read */
    int    i;

    p = strg;
    *strg = '\0';  /* Initialize string */
    for (i=1; i<size; i++)
    {
	if ((c = getc(infptr)) != EOF)
	    *strg++ = (rc_toupper_sw) ? toupper(c) : c;
	else  /* End of file, but field not read */
        {
	    *strg = '\0';   /* Add terminator */
	    return(FAIL);
        }
    }

    /* Get last character */
    c = getc(infptr);
    if ((c == ' ') || (c == '\n'))  /* Valid char */
    {
	*strg = '\0';   /* Add terminator */
	rc_field_count++;
	return(PASS);
    }
    *strg = '\0';  /* Add string terminator */
    return(FAIL);
}


/* rc_read_int(ival,len) -----------------------------------------------
	This routine reads len bytes from the input file, converts them
	to integer and stores the value in ival.
*/

rc_read_int(ival,len)
	int *ival;
	int len;
{
	char t[30];

	if (rc_read_field(t,len) == FAIL)
	    return (FAIL);
	*ival = atoi(t);
	return (PASS);
}


/* rc_read_uint(ival,len) ----------------------------------------------
	This routine reads len bytes from the input file, converts them
	to unsigned integer and stores the value in ival.
*/

rc_read_uint(ival,len)
	unsigned int *ival;
	int len;
{
	char t[30];
	char *s;

	if (rc_read_field(t,len) == FAIL)
	    return (FAIL);
	*ival = 0;
	s = t;
	while (isdigit(*s))
	    *ival = *ival * 10 + (*s++ - '0');
	return (PASS);
}


/* rc_read_double(dval,len) --------------------------------------------
	This routine reads len bytes from the input file, converts them
	to double precision, and stores the value in dval.
*/

rc_read_double(dval,len)
	double *dval;
	int len;
{
	char t[30];
	double atof();

	if (rc_read_field(t,len) == FAIL)
	    return (FAIL);
	*dval = atof(t);
	return (PASS);
}


/* rc_read_gmt(gmt) ----------------------------------------------------
	This routine reads 22 characters from the input file, interprets
	them as a Greenwich mean time string, and fills in the values
	in the GMT data block pointed to by gmt.
*/

rc_read_gmt(gmt)
	GMT *gmt;
{
	char t[30];
	double atof();

    /* get year */
	if (rc_read_field(t,5) == FAIL)
	    return (FAIL);
	gmt->yr = atoi(t);
	if (rc_read_field(t,17) == FAIL)
	    return (FAIL);
	gmt->day = atoi(t);
	gmt->hr = atoi(&t[4]);
	gmt->min = atoi(&t[7]);
	gmt->second = (float) atof(&t[10]);
	return (PASS);
}


/* rc_get_rqst(filename,rp,opt)-----------------------------------------
	This routine reads the processing request record from the file 
	and loads it into the structure pointed to by rp.  If opt=1,
	the state vectors and time correlation element are loaded
	as well.
	NOTE: Upon entering this routine, the current directory must be
	      the one in which the job request file resides, and the
	      filename argument must be just the filename without any
	      further path information.
*/

int rc_get_rqst(filename,rp,opt)
	char *filename;
	RQST_PTR rp;
	int opt;
{
	double lat,lon,ave_hght;
    /* Open file */
    if (rc_open_infile(filename) == FAIL)
	return(FAIL);
 
    rc_toupper_sw = 0;    /* Do not convert to upper case */
    rc_field_count = 0;

    /* Read each field in the first record */
    if ((rc_read_field(rp->id,sizeof(rp->id)) == FAIL)
      || (rc_read_field(rp->type,sizeof(rp->type)) == FAIL)
      || (rc_read_field(rp->site,sizeof(rp->site)) == FAIL)
      || (rc_read_field(rp->take_id,sizeof(rp->take_id)) == FAIL)
      || (rc_read_field(rp->tape_id,sizeof(rp->tape_id)) == FAIL)
      || (rc_read_int(&rp->start_blk,9) == FAIL)
      || (rc_read_int(&rp->end_blk,9) == FAIL)
      || (rc_read_gmt(&rp->start) == FAIL)
      || (rc_read_gmt(&rp->end) == FAIL)
      || (rc_read_double(&lat,9) == FAIL)
      || (rc_read_double(&lon,9) == FAIL)
      || (rc_read_gmt(&rp->targ) == FAIL)
      || (rc_read_int(&rp->targ_rg,5) == FAIL)
      || (rc_read_double(&ave_hght,7) == FAIL)
      || (rc_read_int(&rp->proc_gain,4) == FAIL)
      || (rc_read_field(rp->deskew,sizeof(rp->deskew)) == FAIL)
      || (rc_read_field(rp->gnd_slnt_rg,sizeof(rp->gnd_slnt_rg)) == FAIL))
    {
	rc_print_error();
	rc_close_infile();
	return (FAIL);
    }
    rp->lat = (float) lat;
    rp->lon = (float) lon;
    rp->ave_hght = (float) ave_hght;
    /* if option set, read the statevectors & time element */
    if (opt) {
	if ((rc_read_sv(&sv1,rp) == FAIL)
	  || (rc_read_sv(&sv2,rp) == FAIL)
	  || (rc_read_tc(&tcf,rp) == FAIL))
	{
	    rc_print_error();
	    rc_close_infile();
	    return (FAIL);
	}
    }
    rc_close_infile();
    strcpy(rp->jobname,filename);
    return (PASS);
}


/* rc_read_sv(sp,rp) ---------------------------------------------------
	This routine reads one statevector from the job request file,
	and stores it in the statevector block pointed to by sp.
	rp points to the related job request.
*/

rc_read_sv(sp,rp)
	SV_PTR sp;
	RQST_PTR rp;
{
	char t[30];
	int temp;

	if (rc_read_field(t,30) == FAIL)
	    return (FAIL);
	if (rc_read_field(t,4) == FAIL)
	    return (FAIL);
	if (strcmp(t,"STV")) {
	    rc_field_count--;
	    return (FAIL);
	}
	if (rc_read_field(t,3) == FAIL)
	    return (FAIL);
	if (strncmp(t,rp->take_id,2)) {
	    rc_field_count--;
	    return (FAIL);
	}
	if (rc_read_field(t,12) == FAIL) 
	    return (FAIL);
	  temp = 0;
	  if(strcmp(t,"PREDICTED  ") == 0) temp = 1;
	  if(strcmp(t,"RESTITUTED ") == 0) temp = 2;
	  if(strcmp(t,"PRELIMINARY") == 0) temp = 3;
	  if(strcmp(t,"PRECISION  ") == 0) temp = 4;
	  sp->precision = temp;
	  if(temp == 0) return (FAIL);

	if ((rc_read_field(t,20) == FAIL)
	  || (rc_read_int(&sp->rev,6) == FAIL)
	  || (rc_read_gmt(&sp->gmt) == FAIL)
	  || (rc_read_double(&sp->pos.x,12) == FAIL)
	  || (rc_read_double(&sp->pos.y,12) == FAIL)
	  || (rc_read_double(&sp->pos.z,12) == FAIL)
	  || (rc_read_double(&sp->vel.x,12) == FAIL)
	  || (rc_read_double(&sp->vel.y,12) == FAIL)
	  || (rc_read_double(&sp->vel.z,12) == FAIL))
	    return (FAIL);
	sp->vel.x /= 1000.0;
	sp->vel.y /= 1000.0;
	sp->vel.z /= 1000.0;
	return (PASS);
}


/* rc_read_tc(tp,rp) ---------------------------------------------------
	This routine reads the time correlation element from the job
	request file, and stores it in the time correlation data block
	pointed to by tp.  rp points to the related job request.
*/

rc_read_tc(tp,rp)
	TC_FILE_PTR tp;
	RQST_PTR rp;
{
	char t[30];
	double seconds;

	if (rc_read_field(t,30) == FAIL)
	    return (FAIL);
	if (rc_read_field(t,4) == FAIL)
	    return (FAIL);
	if (strcmp(t,"TCE")) {
	    rc_field_count--;
	    return (FAIL);
	}
	if ((rc_read_int(&tp->rev,6) == FAIL)
	  || (rc_read_gmt(&tp->gmt) == FAIL)
	  || (rc_read_uint(&tp->bt,12) == FAIL)
	  || (rc_read_int(&tp->delta,12) == FAIL))
	    return (FAIL);
	if (rp->take_id[0] == 'J') {
	    seconds = tp->delta / -1000.0;  /* convert from mlsc */
	    add_seconds(&tp->gmt,seconds);
	    }
	return (PASS);
}


/* rc_print_error() ------------------------------------------------
	This routine prints a message telling which field failed
	during reading the job request.
*/

rc_print_error()
{
	static char *ftab[] = { "ID",
				"TYPE",
				"SITE",
				"TAKE ID",
				"MEDIA ID",
				"START BLOCK",
				"END BLOCK",
				"START YEAR",
				"START GMT",
				"END YEAR",
				"END GMT",
				"LAT",
				"LON",
				"TARGET YEAR",
				"TARGET GMT",
				"TARGET RANGE",
				"AVERAGE HEIGHT",
				"PROCESSOR GAIN",
				"DESKEW",
				"GROUND/SLANT RANGE",
				"SV1 ID",
				"SV1 STV",
				"SV1 SAT",
				"SV1 PRECISION",
				"SV1 COORD SYSTEM",
				"SV1 ORBIT",
				"SV1 YEAR",
				"SV1 GMT",
				"SV1 X POSITION",
				"SV1 Y POSITION",
				"SV1 Z POSITION",
				"SV2 ID",
				"SV2 STV",
				"SV2 SAT",
				"SV2 PRECISION",
				"SV2 COORD SYSTEM",
				"SV2 ORBIT",
				"SV2 YEAR",
				"SV2 GMT",
				"SV2 X POSITION",
				"SV2 Y POSITION",
				"SV2 Z POSITION",
				"TCE ID",
				"TCE -TCE-",
				"TCE REV",
				"TCE YEAR",
				"TCE GMT",
				"TCE SAT TIME",
				"TCE CLOCK CYCLE",
				};
	int ftablen = sizeof(ftab) / sizeof (char *);

	printf("Cannot read job request: error on %s field\n",
		ftab[rc_field_count]);
}


/* rc_print_rqst(rp) -----------------------------------------------
	This routine dumps the job request pointed to by rp.
*/

rc_print_rqst(rp)
	RQST_PTR rp;
{
	printf("Current job request:\n");
	printf("  ID = %s\n",rp->id);
	printf("  TYPE = %s\n",rp->type);
	printf("  SITE = %s\n",rp->site);
	printf("  TAKE_ID = %s\n",rp->take_id);
	printf("  TAPE_ID = %s\n",rp->tape_id);
	printf("  START_BLK = %d\n",rp->start_blk);
	printf("  END_BLK = %d\n",rp->end_blk);
	printf("  START_TIME = ");
	dump_gmt(&rp->start);
	printf("  END_TIME = ");
	dump_gmt(&rp->end);
	printf("  LAT = %g\n",rp->lat);
	printf("  LON = %g\n",rp->lon);
	printf("  TARG_TIME = ");
	dump_gmt(&rp->targ);
	printf("  TARG_RANGE = %d\n",rp->targ_rg);
	printf("  AVE_HGHT = %g\n",rp->ave_hght);
	printf("  PROC_GAIN = %d\n",rp->proc_gain);
	printf("  DESKEW = %s\n",rp->deskew);
	printf("  GND/SLANT = %s\n",rp->gnd_slnt_rg);
	printf("SV1:\n");
	dump_sv(&sv1);
	printf("SV2:\n");
	dump_sv(&sv2);
	printf("Time correlation element:\n");
	printf("  REV = %d\n",tcf.rev);
	printf("  GMT_TIME = ");
	dump_gmt(&tcf.gmt);
	printf("  SAT_TIME = %d\n",tcf.bt);
	printf("  DELTA = %d\n",tcf.delta);
	printf("\n");
}
