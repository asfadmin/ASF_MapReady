static char *sccs = "@(#)ims_processDat.c	5.3  08/15/97";
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/utsname.h>

#include "../lincl/defs.h"
#include "../lincl/extern.h"

#include    <ims_dbms.h>
#include    <ims_const.h>
#include    <ims_msg.h>
#include    <ims_qi.h>

int Debug;

#define   MAX_BUF  40000

int process_dat( char * dat_name, int desc,
    short debug, IMS_MSG_STRUCT *msgDesc )
{
/* ***************************************
    subr process_dat reads in and writes out the data
        file, the image options file (IMOP).
    note:  not dumped if the debug flag is on. (only
        first record in any case).
******************************************* */
FILE *in;
int ret;
long  n_recs, n_bytes;

int   read_all_recs(  FILE *, int, long *, long *, IMS_MSG_STRUCT *);


Debug = debug;
/* open the dat file */
if ( ( in = fopen( dat_name, "r" ) ) == NULL) {
    (void) ims_msg( msgDesc, IMS_ERROR,
"process_dat: Could not open the Image Options (data) file: %s",
        dat_name);
    return( IMS_ERROR );
}

if(  debug  )  (void) printf(
    "\n process_dat:  IMOP (data) file being dumped.\n" );

/*  read all the records - the description record is first */
ret = read_all_recs( in, desc, &n_recs, &n_bytes, msgDesc );
switch (ret) {
    case (END_OF_FILE) :
        break;
    default :
        (void) ims_msg( msgDesc, IMS_ERROR,
"Process_dat: Aborting read of data file: %ld recs, %ld bytes read.",
            n_recs, n_bytes );
        return( IMS_ERROR );
}
(void) fclose(in);
if(  debug  )  (void) printf(
"\n process_dat:  IMOP (data) file finished: %ld records, %ld bytes.\n",
    n_recs, n_bytes );
return( IMS_OK );
}   /*  process_dat  */


int read_all_recs( FILE* fp, int  desc, long *n_recs_out,
    long *n_bytes_out, IMS_MSG_STRUCT * msgDesc )
/* ***************************************
    subr read_all_vdrs reads in the volume directory file
        records.  the first 4 words determine the type
        of file is being read, then the appropriate
        subprogram is called.
******************************************* */
{
int nbytes;
int rsize;
int ret;
unsigned  char  *buf;
long  n_recs;
long  n_bytes_sum;
int  i;

int  cvt2int( unsigned char * );


buf = (unsigned char *) malloc( MAX_BUF * sizeof( char ) );
ret = MATCH_FOUND;
n_recs = 0;
n_bytes_sum = 0;
while (ret==MATCH_FOUND) {
    /* read first 12 bytes of the record */
    nbytes = read_record( fp, buf, sizeof(unsigned char), 12 );

    switch ( nbytes ) {

        case (END_OF_FILE):
            *n_bytes_out = n_bytes_sum;
            *n_recs_out = n_recs;
            (void) free( buf );
            return(END_OF_FILE);
        case (READ_ERROR):
            *n_bytes_out = n_bytes_sum;
            *n_recs_out = n_recs;
            (void) free( buf );
            return(READ_ERROR);

        default:
        if (nbytes == 12 ) {
            rsize = cvt2int(&buf[8]) - 12;
            n_recs++;
            n_bytes_sum += (rsize+12);
            if(  rsize+12  >=  MAX_BUF  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Process_dat:  buffer size too small %d",
                    rsize+12 );
                *n_bytes_out = n_bytes_sum;
                *n_recs_out = n_recs;
                (void) free( buf );
                return( -50 );
            }
            if ( rsize  >  0 ) {
                nbytes = read_record( fp, (buf+12),
                sizeof(unsigned char), rsize );
                if ( nbytes == rsize) {
                    /* ****  write file to tape  **** */
                    i = write( desc, buf, rsize+12 );
                    if(  i  !=  rsize+12 ){
                        /* error on write: same as for read */
                        *n_bytes_out = n_bytes_sum;
                        *n_recs_out = n_recs;
                        (void) free( buf );
                        return(READ_ERROR);
                    }
                }
            }
        }
        else{
            *n_bytes_out = n_bytes_sum;
            *n_recs_out = n_recs;
            (void) free( buf );
            return(READ_ERROR);
        }
    }
}
*n_bytes_out = n_bytes_sum;
*n_recs_out = n_recs;
(void) free( buf );
return(ret);
}   /*  read_all_recs   */
