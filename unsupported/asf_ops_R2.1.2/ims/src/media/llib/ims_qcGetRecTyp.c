static char *sccs = "@(#)ims_qcGetRecTyp.c	5.1  03/17/96";
/* *****************************************************************
*
*  Name: GetRecTyp
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*  This routine interprets the first 12 bytes of the record to
*   determine if it's CEOS and, if CEOS, what type of record.
*
*  Input Parameters:
*
*   Name           Type        Description
*   rec_buf         int         Record buffer
*   buflen          int         Length of record
*
*  Output Parameters:
*
*   Name           Type    Description
*   ceosflag        int     CEOS flag, 0 = non-CEOS, 1=CEOS
*   type            char    Type of record
*   istat           int     Status, -1 = error
*
*  Modification History:
*
*   Date:   18 Jul 1990 10:23:56    Revision:   2.0  Author:   DBMAN
*   Date:   16 Oct 1991 14:25:58    Revision:   2.1  Author:   INGRID
*
******************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_util.h>
#include <ims_msg.h>

#define ERR -1

int GetRecTyp (rec_buf,buflen,ceosflag,type,message)

int rec_buf[], *ceosflag, buflen;
char    type[];
char    message[];
{
struct  record {
    int seq;
    unsigned char   sub1;
    unsigned char   code;
    unsigned char   sub2;
    unsigned char   sub3;
    int length;
    };
struct record *ceos;
union {
    unsigned char uchar[2];
    char  ary[2];
    short  i_s;
    } cnv_char;
short  i1,i2;
int istat;
/* ************************************************************** */

istat = IMS_OK;
*ceosflag = TRUE;
ceos = (struct record *) rec_buf;

cnv_char.i_s = 0;
cnv_char.uchar[1] = ceos->sub1; /* for debugging: check nos  */
cnv_char.uchar[1] = ceos->code;

if( ceos->length == buflen){
    if( ceos->code == 63 && ceos->sub1 == 18){
        (void) strcpy(type,"TXT");
    }
    else if( ceos->code == 192){ /* CEOS descriptor records */
        if( ceos->sub1 == 192){
            if( ceos->sub2 == 18) (void) strcpy( type,"VDR");
            else if( ceos->sub2 == 63) (void) strcpy( type,"NUL");
            else (void) strcpy( type,"UNK");
        }
        else if( ceos->sub1 == 219) (void) strcpy( type,"FPR");
        else if( ceos->sub1 == 63) (void) strcpy( type,"FDR");
        else (void) strcpy( type,"UNK");
    }
    else{
        if( ceos->sub1 == 10){  /* CEOS leader metadata records */
            if( ceos->code == 10) (void) strcpy( type,"DSR");
            else if( ceos->code == 20) (void) strcpy( type,"MPR");
            else if( ceos->code == 30) (void) strcpy( type,"PPR");
            else if( ceos->code == 40) (void) strcpy( type,"ADR");
            else if( ceos->code == 50) (void) strcpy( type,"RDR");
            else if( ceos->code == 51) (void) strcpy( type,"RCR");
            else if( ceos->code == 60) (void) strcpy( type,"DQR");
            else if( ceos->code == 70) (void) strcpy( type,"DHR");
            else if( ceos->code == 80) (void) strcpy( type,"RSR");
            else if( ceos->code == 90) (void) strcpy( type,"DEM");
            else if( ceos->code == 100) (void) strcpy( type,"RPU");
            else if( ceos->code == 110) (void) strcpy( type,"ANR");
            else if( ceos->code == 120) (void) strcpy( type,"DPR");
            else if( ceos->code == 130) (void) strcpy( type,"CDR");
            else if( ceos->code == 140) (void) strcpy( type,"GCP");
            else if( ceos->code == 200) (void) strcpy( type,"FSR");
            else if( ceos->code == 210) (void) strcpy( type,"FSR");
            else if( ceos->code == 201) (void) strcpy( type,"FIM");
            else if( ceos->code == 202) (void) strcpy( type,"FIC");
            else if( ceos->code == 203) (void) strcpy( type,"FWS");
            else (void) strcpy( type,"UNK");
        }
        else if( ceos->sub1 == 50){/* CEOS imagery options data recs */
            if( ceos->code == 10) (void) strcpy( type,"SIG");
            else if( ceos->code == 11) (void) strcpy( type,"PRC");
            else if( ceos->code == 12) (void) strcpy( type,"IMV");
            else if( ceos->code == 13) (void) strcpy( type,"ICG");
            else if( ceos->code == 14) (void) strcpy( type,"ICI");
            else if( ceos->code == 15) (void) strcpy( type,"OWS");
            else (void) strcpy( type,"UNK");
        }
        else if( ceos->sub1 == 90){ /* CEOS trailer metadata records */
            if( ceos->code == 120) (void) strcpy( type,"DPR");
            else if( ceos->code == 130) (void) strcpy( type,"CDR");
            else if( ceos->code == 140) (void) strcpy( type,"GCP");
            else if( ceos->code == 200) (void) strcpy( type,"FSR");
            else if( ceos->code == 210) (void) strcpy( type,"FSR");
            else if( ceos->code == 201) (void) strcpy( type,"FIM");
            else if( ceos->code == 202) (void) strcpy( type,"FIC");
            else if( ceos->code == 203) (void) strcpy( type,"FWS");
            else (void) strcpy( type,"UNK");
        }
        else (void) strcpy( type,"UNK");
    }
    if( strcmp (type,"UNK") == 0){
        (void) sprintf( message,
"Unknown record type: record: %d, code: %d, 1st subtype code: %d.",
        ceos->seq,ceos->code,ceos->sub1);
        istat = IMS_ERROR;
    }
}
else{
    cnv_char.i_s = 0;
    cnv_char.uchar[1] = ceos->sub1; /* for debugging: check nos  */
    i1 = cnv_char.i_s;
    cnv_char.uchar[1] = ceos->code;
    i2 = cnv_char.i_s;
    (void) sprintf( message,
        "Record length mismatch: record: %d, internal length: %d, \
RMS length:%d  sub1 = %d   code = %d.",
        ceos->seq,ceos->length,buflen, i1, i2 );
    istat = IMS_ERROR;
}

return (istat);
}   /*  GetRecTyp   */
