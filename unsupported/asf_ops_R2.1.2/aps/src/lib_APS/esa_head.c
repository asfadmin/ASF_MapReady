#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*
   #define PRINT_DIAG
*/

#include <sys/types.h>
#include <stdio.h>

#include "timeconv.h"


#define ASF_HEAD_LEN 	50
#define ESA_FIXED_LEN 	30


/*****************************************************************************
*
* Name: esa_head
*
*
* Purpose
*	read in the header(s), determine satellite, and check validity
*	for these ESA files:
*	SHAQ	ESA acquisition schedule.  		
*	RQST	ESA user request status file.  
*	MPSG	SAR GAP file. 
*   if no error is discovered, the file position when returning from 
*	this routine will be after the header(s) that were present.  
*	if an error is discovered by this routine, the position of the file 
*	is not important; no more reading of the file is expected.  
*
*	POSSIBLE HEADERS:  
*		a)  ASF common header with 2-byte satellite indicator offset 33.
*		b)  ESA "Fixed portion"
*	if both a) and b) are present, a) will always come first.  
*
*	ACCEPTABLE CONDITIONS:  
*
*		at least one of the headers must exist:
*
*		existance of header		file types where seen 
*
*		a)		b)		# = not allowed.  
*		no		no		#
*		no		yes		SHAQ, RQST MPSG
*		yes		no		SHAQ, RQST MPSG
*		yes		yes		SHAQ, RQST MPSG
*
* Input
*	fd			i*	input file descriptor
*	filetype	c*	expected file type, 2-character code:  
*				ES=ESA Acq Sch  (SHAQ)
*				ET=ESA user request status (RQST)
*				EG=ESA Gap file (MPSG)
*
* Output
* sat	        *char	satellite id:  E1, E2...
* station_id	*char	station id:  ASF, MCM, ...  
*
* Return value
*		int		0	no error; file position is after the header(s).
*				1	error or EOF on reading the file.  
*				2	file does not match the expected file. 
*				3	any other error.
*
* Internal
*	comhead	    c	header buffer
*	intime		c	creation date
*	ftype		c	2 char code for file type
*				ES=ESA Acq Sch  (SHAQ)
*				ET=ESA user request status (RQST)
*				EG=ESA Gap file (MPSG)
*	fdest		c	3 char code for file destination (MPS or APS)
*	fsrc		c	" "    "    "   "    source (ACS)
*	stat		i	return status
*
*****************************************************************/
#pragma ident	"@(#)esa_head.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.esa_head.c"

int esa_head(
	int 	fd, 		/* input:  file descriptor			*/
	char 	*filetype, 	/* input:  expected file type		*/
	char 	*sat,
	char	*station_id )		/* output:  satellite id (E1 or E2)	*/
{
	/* offsets for lseek calls.  	*/
	off_t   posit_ ;
	off_t   offset_0 = 0 ;

	char buf[100];
	int stat;
	int		comhead_yes; /* =1 indicates presence of the ASF common header. */
	int		esahead_yes; /* =1 indicates presence of the ESA fixed header. */

#ifdef PRINT_DIAG
	printf("esa_head.c 1:  fd = %d, filetype = %s\n", fd, filetype); 
#endif

	/* assume that there is only the ESA fixed portion at the start.  	*/
	/* note EOL (end of line) as an extra character					*/
	stat = read(fd,buf,ESA_FIXED_LEN+1);

#ifdef PRINT_DIAG
	printf("esa_head.c 2:  buf = %.30s\n", buf); 
#endif

	if (stat <= 0) 
		return 1;
	
	/********************************************/
	/* check to see if this is an ESA header	*/
	/********************************************/
	stat = esa_head_check(fd, buf, filetype, sat, station_id);
	/* return if this was an ESA header			*/
	/* stat == 2 means unexpected file type.   */
	if(stat == 0 || stat == 2)
		return (stat);

#ifdef PRINT_DIAG
	printf("esa_head.c:  not an ESA header.\n"); 
#endif

	/* the first header was NOT the ESA fixed portion of an expected file.  */
	/* therefore the first header ought to be the ASF common header.	*/
	/* now re-set the file position to the start of the file 			*/
	/* and read the correct number of bytes for the ASF common header	*/
	/* re-set the file position to START of the file					*/
	if ( lseek ( fd, offset_0, SEEK_SET ) < 0 )
	{
		printf ( "ERROR resetting position in input file.\n" ) ;
		return 1 ;
	}

	/* the file didn't start with the ESA fixed portion; we re-wound 	*/
	/* the file; now we can see if the file starts with the ASF header	*/

	/* now read the ASF common header	*/
	stat = read(fd,buf,ASF_HEAD_LEN+1);
	if (stat <= 0) 
		return 1;

	/********************************************/
	/* check to see if this is an ASF header	*/
	/********************************************/
	/* this routine checks the ASF header and returns 0 if OK, 2 		*/
	/* if an error.														*/
	stat = asf_head_check(buf, filetype, sat, station_id);
	/* return if any error at all.  */
	if(stat != 0 )
		return (stat);

	/* the ASF common header was OK; the file started with the ASF header.	*/

	/* at this point, the next bytes could be the ESA fixed portion header 	*/
	/* or ESA variable portion.  									*/
	/* or ESA data records.  										*/

	/* now check for the presence of the ESA fixed portion header.  */

	/* but first, obtain and retain the position (posit_) of 			*/
	/* the file right now.  this is because if the ESA fixed portion 	*/
	/* is not present, we must be prepared to re-set the file 			*/
	/* position back to here.  											*/
	/* this is because if this routine (esa_head.c) later returns 		*/
	/* with no error, the file position must be set to just after the 	*/
	/* header(s) read.  												*/

	if ( (posit_ = lseek ( fd, offset_0, SEEK_CUR )) < 0 )
	{
		printf ( "ERROR: in determining current position of input file.\n" ) ;
		return 1 ;
	}

	stat = read(fd,buf,ESA_FIXED_LEN+1);
	if (stat <= 0) 
		return 1;

	/********************************************/
	/* check to see if this is an ESA header	*/
	/********************************************/
	stat = esa_head_check(fd, buf, filetype, sat, station_id);
	/* return if check O.K. or if unexpected file type.  	*/
	if(stat == 0 || stat == 2)
		return (stat);

#ifdef PRINT_DIAG
	printf("esa_head.c:  not an ESA header.\n"); 
#endif

	/* this was not an ESA header; only the ASF header is in this file.	*/
	/* now re-position the file	to just after the read of 	*/
	/* the ASF header										*/
	if ( lseek ( fd, posit_, SEEK_SET ) < 0 )
	{
		printf ( "ERROR resetting position in input file.\n" ) ;
		return 1 ;
	}

	/* the ASF common header is the only header we have.  	*/
	/* it was already checked								*/
	/* the file was re-positioned to the start of the		*/
	/* data and is ready for the data to be read. 			*/

	return 0;
}


int esa_head_check(
	int		fd, 			/* input file descriptor		*/
	char	*esa_header, 	/* buffer with ESA fixed portion */
	char	*filetype,	/* expected file type				*/
	char	*sat, 		/* output satellite id			*/
	char	*station_id)
/* this routine returns a code:  				*/
/*		-1 = not an esa header					*/
/*		 0 = OK; an esa header, expected file OK.	*/
/*		 2 = an esa header; unexpected file		*/
{
	int stat;
	char buf5[5];
	if(strncmp(esa_header, "RQST_",5) != 0 && 
	   strncmp(esa_header, "MPSG_",5) != 0 && 
	   strncmp(esa_header, "SHAQ_",5) != 0 ) 
	{
		/* not an ESA header	*/
		return -1;
	}


	/* the header is the ESA fixed header.	*/

#ifdef PRINT_DIAG
	printf("esa_head.c 3:  the header is the ESA fixed header.\n");  
#endif

	/* obtain the satellite id.  	*/
	strncpy(sat, esa_header+20, 2);
	sat[2] = '\0';

	/* obtain the station id.  */
	strncpy(station_id, esa_header+13, 2) ;
	station_id[3] = NULL ;

	if (strcmp(station_id, "AF") == 0)
		strcpy(station_id, "ASF") ;
	else if (strcmp(station_id, "MM") == 0)
		strcpy(station_id, "MCM") ;
	else 
	{
		printf("STATION ID: %s\n", station_id) ;
		return(1) ;
	}

	
#ifdef PRINT_DIAG
	printf("esa_head.c 1:  sat = %s\n", sat); 
	printf("esa_head.c 1:  station_id = %s\n", station_id); 
#endif

	/* now determine if this file is expected:	*/

#ifdef PRINT_DIAG
	printf("esa_head_check:  filetype = %.2s, filetype = %.5s\n", 
		filetype, esa_header);  
#endif

	/* compare the expected file with the current file.		*/
	if(strcmp(filetype, "ES") == 0 && strncmp(esa_header, "SHAQ_",5) == 0 ) 
	{
		/* the SHAQ file.  skip the 5-byte variable portion...		*/
		/* 4 bytes plus a newline character.						*/
		stat = read(fd,buf5,5);

#ifdef PRINT_DIAG
		printf("esa_head_check:  buf5 = %.5s\n", buf5); 
#endif

		if(stat != 5)
			return -1;
		return 0;
	}
	if(strcmp(filetype, "ET") == 0 && strncmp(esa_header, "RQST_",5) == 0 )
		return 0;
	if(strcmp(filetype, "EG") == 0 && strncmp(esa_header, "MPSG_",5) == 0 )
		return 0;
	/* the expected file did not match the actual file.		*/

#ifdef PRINT_DIAG
	printf("esa_head_check:  no match.\n"); 
#endif

	return 2;
}



/* this routine checks the ASF common header and 	*/
/* returns 0 if OK, -1 if an error. 2 if the file is an unexpected type.  */
int asf_head_check(
	char	*comhead, 		/* buffer with ASF common header.   */
	char	*filetype,		/* expected file type				*/
	char	*sat,
	char	*station_id )			/* satellite obtained				*/
{
	int	stat;
	char intime[22],ftype[3],fdest[3],fsrc[3];

	/*  printf("asf_head_check:  \n\tcomhead = %.50sX\n", comhead);  */

	/* Parse the header string */
	strncpy(intime,comhead,22);
	strncpy(ftype,&comhead[22],2);
	strncpy(fdest,&comhead[25],3);
	strncpy(fsrc,&comhead[29],3);

#ifdef PRINT_DIAG
	printf("asf_head_check:  \n"); 
#endif

#ifdef PRINT_DIAG
	printf("                intime=%.22s, ftype=%.2s, fdest=%.3s, fsrc=%.3s\n",
		intime, ftype, fdest, fsrc);
#endif

	/* Check the creation date */
	intime[4] = ':';
	stat = tc_validate_asf_datetime(intime);
	if (stat < 0)
		return -1;

#ifdef PRINT_DIAG
	printf("asf_head_check:  date O.K. \n"); 
#endif

	/* Check the file destination */
	if (strncmp(fdest,"MPS",3) != 0 && strncmp(fdest,"APS",3) != 0)
		return -1;

#ifdef PRINT_DIAG
	printf("asf_head_check:  destination  O.K. \n"); 
#endif

	/* Check the file source */
	if (strncmp(fsrc,"ACS",3) != 0)
		return -1;

#ifdef PRINT_DIAG
	printf("asf_head_check:  source  O.K. \n"); 
#endif

	/* Check the file type */
	if (strncmp(ftype,filetype,2) != 0)
		return 2;

#ifdef PRINT_DIAG
	printf("asf_head_check:  file type O.K. \n"); 
	printf("asf_head_check:  all O.K. \n"); 
#endif

	/* get the satellite id:	*/
	strncpy(sat, comhead+33, 2);
	sat[2] = '\0';

	/* if no value, assume satellite to be E1		*/
	if (strcmp(sat, "  ") == 0)
		strcpy(sat, "E1");

	/* no value for station_id; assume station_id to be ASF		*/
	strcpy(station_id, "ASF");

#ifdef PRINT_DIAG
   printf("asf_head_check:  sat = %s\n",sat);
   printf("asf_head_check:  station_id = %s\n", station_id);
   printf("asf_head_check:  Header string =XX%sXX\n",comhead);
#endif

	return 0;
}
