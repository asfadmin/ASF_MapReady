static char *sccs = "@(#)ims_processVdf.c	5.2  03/07/97";
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <ims_util.h>
#include <unistd.h>
#include <sys/utsname.h>

#include    "../lincl/defs.h"
#include    "../lincl/extern.h"
#include    <ims_dbms.h>
#include    <ims_const.h>
#include    <ims_msg.h>
#include    <ims_qi.h>

int Debug;
int Dump;

#define   MAX_BUF   8000

int  cvt2int( unsigned char * );


int process_vdf( char * vdf_name, int desc,
    pnt_vdf_file_t  pnt_vdf, short debug, short dump,
    IMS_MSG_STRUCT * msgDesc )
{
/* ***************************************
    subr process_vdf reads in and writes out the volume
        directory file.
    note:  currently only vdf files read, and dumped if the
        debug flag is on.
******************************************* */
FILE *in;
int ret;
int  status;

int   read_vdr( FILE *, pnt_vdf_file_t, int, IMS_MSG_STRUCT * );
int   read_all_vdrs(  FILE *, pnt_vdf_file_t, int, IMS_MSG_STRUCT * );


Debug = debug;
Dump = dump;

/* open the vdf file */
if ( ( in = fopen( vdf_name, "r" ) ) == NULL) {
    (void) ims_msg( msgDesc, IMS_ERROR,
       "Process_vdf: Could not open the Volume Directory File %s.",
        vdf_name);
    return( IMS_ERROR );
}

/* allocate memory for a vdf structure */
if(  pnt_vdf  ==  NULL  ){
    pnt_vdf = (pnt_vdf_file_t)  malloc( sizeof( vdf_file_t ) );
    if(  pnt_vdf  ==  NULL ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "Process_vdf:  Failed to allocate VDF area.");
        return ( IMS_ERROR );
    }
}
pnt_vdf->pnt_descript = NULL;
pnt_vdf->pnt_gdata  = (pnt_granule_data_t) malloc( sizeof(
    granule_data_t ) );
pnt_vdf->pnt_gdata->pnt_text_rec = NULL;
pnt_vdf->pnt_gdata->pnt_file_rec = NULL;
pnt_vdf->pnt_gdata->pnt_tfile_rec = NULL;
pnt_vdf->pnt_gdata->pnt_ifile_rec = NULL;
pnt_vdf->pnt_gdata->pnt_next = NULL;

/*  read the first record - the vdr record  */
ret = read_vdr( in, pnt_vdf, desc, msgDesc );
if(  ret  <  IMS_OK )  return( ret );

/* now read the other records:  first the file pointer records,
    then the text records.  */
ret = read_all_vdrs( in, pnt_vdf, desc, msgDesc );
status = IMS_OK;
switch (ret) {
    case (END_OF_FILE) :
        if(  Debug  )  (void) printf( "\n Read VDF records.\n" );
        break;
    default :
        if(  Debug )  (void) printf(
            "\n Aborting the read of the input file\n");
        status = IMS_ERROR;
        break;
}
(void) fclose(in);

return( status );
}   /*  process_from_vdf  */


int read_all_vdrs( FILE* fp, pnt_vdf_file_t  pnt_vdf,
    int  desc, IMS_MSG_STRUCT * msgDesc )
/* ***************************************
    subr read_all_vdrs reads in the volume directory file
        records.  the first 4 words determine the type
        of file is being read, then the appropriate
        subprogram is called.
******************************************* */
{
long  i,j;
short   flag;

int nbytes;
int rsize;
int ret;
unsigned char* tb;
unsigned  char  *buf;
short  n_text;     /* cntr for no. of text records */
pnt_granule_data_t  pnt_gdata,pnt_gdata2;
short  k,l;
union  char_is{
    short  i_c;
    unsigned  char   ary[2];
};
union  char_is  char_i;

int  read_file_ptr( pnt_vdf_file_t, unsigned char *,
        short *, IMS_MSG_STRUCT * ) ;
int  read_text_rec( unsigned char *,
        pnt_granule_data_t );


/*  I am going to assume that every file is unique and
    that the files are in order:  thus the third leader
    file will match with the 3rd text file.  this may
    not be true, but the user can determine this, as
    the data does not care.
 */
n_text = 0;
ret = MATCH_FOUND;
pnt_gdata2 = NULL;
buf = (unsigned char *) malloc( MAX_BUF * sizeof( char ) );

while (ret==MATCH_FOUND) {
    /* read first 12 bytes of the record */
    nbytes = read_record( fp, buf, sizeof(unsigned char), 12 );

    switch ( nbytes ) {

        case (END_OF_FILE):
            (void) free( buf );
            return(END_OF_FILE);
        case (READ_ERROR):
            (void) free( buf );
            return(READ_ERROR);

        default:
        if (nbytes == 12 ) {
            tb=&buf[4];
            if ( MATCH_RECORD(tb, VL1SUB, VLTYPE, VL2SUB, VL3SUB) ){
                /* ****  SAR Leader Pointer Rec, SAR Trailler
                    Pointer Rec, or Imagery Options Rec.  These have
                    different names in the oceos file, but have the
                    same values.  to determine which, the file class
                    variable must be looked at.  */
                rsize = cvt2int(&buf[8]) - 12;
                if ( rsize  >  0 ) {
                    nbytes = read_record( fp, (buf+12),
                    sizeof(unsigned char), rsize );
                    if ( nbytes == rsize) {
                        /* ****  write file to tape  **** */
                        i = write( desc, buf, rsize+12 );
                        if(  i  !=  rsize+12 ){
                            /*  same as read error */
                            (void) free( buf );
                            ret = READ_ERROR;
                        }
                        /* decode the record into the Rile_Ptr_Rec
                            structure */
                        pnt_gdata = pnt_vdf->pnt_gdata;
                        if ( (ret = read_file_ptr( pnt_vdf, buf,
                            &flag, msgDesc ) )  ==
                            DECODE_ERROR) {
                            ret=READ_ERROR;
                        }
                    }
                }
                else{
                    /*  same as read error */
                    (void) free( buf );
                    ret = READ_ERROR;
                }
            }
            else if ( MATCH_RECORD(tb, VX1SUB, VXTYPE, VX2SUB,
                VX3SUB) ){
                /* ******  text record  ***** */
                rsize = cvt2int(&buf[8]) - 12;
                if ( rsize>0 ) {
                    nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                    if ( nbytes == rsize) {
                        /* ****  write file to tape  **** */
                        i = write( desc, buf, rsize+12 );
                        if(  i  !=  rsize+12 ){
                            /*  same as read error */
                            (void) free( buf );
                            return(READ_ERROR);
                        }
                        n_text++;
                        pnt_gdata = pnt_vdf->pnt_gdata;
                        for( i=0 ; i  <  (n_text-1) ; i++ ){
                            if(  pnt_gdata  ==  NULL  ){
                                pnt_gdata  =  (pnt_granule_data_t)
                                    malloc( sizeof(granule_data_t ) );
                                pnt_gdata->pnt_text_rec = NULL;
                                pnt_gdata->pnt_file_rec = NULL;
                                pnt_gdata->pnt_tfile_rec = NULL;
                                pnt_gdata->pnt_ifile_rec = NULL;
                                pnt_gdata->pnt_next = NULL;
                                pnt_gdata2->pnt_next = pnt_gdata;
                            }
                            pnt_gdata2 = pnt_gdata;
                            pnt_gdata = pnt_gdata->pnt_next;
                        }
                        if(  pnt_gdata  ==  NULL  ){
                            pnt_gdata  =  (pnt_granule_data_t)
                                malloc( sizeof(granule_data_t ) );
                            pnt_gdata->pnt_text_rec = NULL;
                            pnt_gdata->pnt_file_rec = NULL;
                            pnt_gdata->pnt_tfile_rec = NULL;
                            pnt_gdata->pnt_ifile_rec = NULL;
                            pnt_gdata->pnt_next = NULL;
                            pnt_gdata2->pnt_next = pnt_gdata;
                        }
                        if ( (ret = read_text_rec( buf,
                            pnt_gdata ) )  ==
                            DECODE_ERROR) {
                            ret=READ_ERROR;
                        }
                    }
                    else {
                        ret=READ_ERROR;
                    }
                }
            }
            else {
                rsize = cvt2int(&buf[8]) - 12;
                char_i.i_c = 0;
                char_i.ary[1] = buf[4];
                i = char_i.i_c;
                char_i.ary[1] = buf[5];
                j = char_i.i_c;
                char_i.ary[1] = buf[6];
                k = char_i.i_c;
                char_i.ary[1] = buf[7];
                l = char_i.i_c;
                (void) ims_msg( msgDesc, IMS_INFO,
            "Process_vdf:  Header record not recognized: %ld %ld %d %d",
                 i,j,k,l );
                if( rsize  >  0 ) {
                    nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char),rsize );
                    if ( nbytes != rsize) {
                        ret=READ_ERROR;
                    }
                }
                else{
                    ret = READ_ERROR;
                }
            }
        }
        else{   /*  same as end-of-file */
            (void) free( buf );
            ret = END_OF_FILE;
        }
    }
}
(void) free( buf );
return(ret);
}   /*  read_all_vdrs   */


int read_vdr( FILE *fp, pnt_vdf_file_t  pnt_vdf,
    int  desc, IMS_MSG_STRUCT * msgDesc  )
/* ***************************************
    subr read_vdr reads the vdr record - assumed to
        be the first on the tape or file.
******************************************* */
{
pnt_desc_rec_t  pnt_d;
pnt_vol_desc_rec_t      pnt_fdesc;
int nbytes;
unsigned char *buf;
long  rsize;
int  at;
int  len;
unsigned char  *  ary;
long i;

char *  trimr( char *);


buf = (unsigned char *) malloc( 4098 * sizeof( char ) );
/* read the record */
nbytes = read_record( fp, buf, sizeof(unsigned char), 12 );
rsize = cvt2int( &buf[8] ) - 12;
if( rsize  >  0 ) {
    nbytes = read_record( fp, (buf+12), sizeof(unsigned char),rsize );
    if ( nbytes != rsize) {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ims_processVdf: Error reading VDR file." );
        return( IMS_ERROR );
    }
    /* ****  write file to tape  **** */
    i = write( desc, buf, rsize+12 );
    if(  i  !=  rsize+12 ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ims_processVdf: Error writting VDR file." );
        return( IMS_ERROR );
    }
}
else{
    (void) ims_msg( msgDesc, IMS_ERROR,
        "ims_processVdf: Error reading VDR file." );
    return( IMS_ERROR );
}

pnt_vdf->pnt_descript = (pnt_vol_desc_rec_t)  malloc( sizeof(
    Vol_Desc_Rec ) );
pnt_fdesc = pnt_vdf->pnt_descript;
pnt_d = &(pnt_fdesc->desc);
pnt_vdf->pnt_vdr = (pnt_recvdr_t)  malloc( sizeof( struct
    recvdr )+4 );

pnt_d->rec_seq = cvt2int(buf);
pnt_d->rec_sub1 = *(buf+4);
pnt_d->rec_type = *(buf+5);
pnt_d->rec_sub2 = *(buf+6);
pnt_d->rec_sub3 = *(buf+7);
pnt_d->length = cvt2int(&buf[8]);

/*  put buffer in array: move as characters.  this is
    used to create the null directory   */
ary = (unsigned char *) (pnt_vdf->pnt_vdr);
for(  i=0 ; i  <  pnt_d->length ; i++  )  ary[i] = buf[i];

at = 12;
len = 2;
(void) strncpy(pnt_fdesc->ascii_flag, (char*) (buf+at), len);
pnt_fdesc->ascii_flag[len] = '\0';
(void) trimr( pnt_fdesc->ascii_flag );
at += len;
len = 2;
(void) strncpy(pnt_fdesc->spare1, (char*) (buf+at), len);
pnt_fdesc->spare1[len] = '\0';
(void) trimr( pnt_fdesc->spare1 );
at += len;
len = 12;
(void) strncpy(pnt_fdesc->format_doc, (char*) (buf+at), len);
pnt_fdesc->format_doc[len] = '\0';
(void) trimr( pnt_fdesc->format_doc );
at += len;
len = 2;
(void) strncpy(pnt_fdesc->format_ver, (char*) (buf+at), len);
pnt_fdesc->format_ver[len] = '\0';
(void) trimr( pnt_fdesc->format_ver );
at += len;
len = 2;
(void) strncpy(pnt_fdesc->format_rev, (char*) (buf+at), len);
pnt_fdesc->format_rev[len] = '\0';
(void) trimr( pnt_fdesc->format_rev );
at += len;
len = 12;
(void) strncpy(pnt_fdesc->software_id, (char*) (buf+at), len );
pnt_fdesc->software_id[len] = '\0';
(void) trimr( pnt_fdesc->software_id );
at += len;
len = 16;
(void) strncpy(pnt_fdesc->phyvol_id, (char*) (buf+at), len );
pnt_fdesc->phyvol_id[len] = '\0';
(void) trimr( pnt_fdesc->phyvol_id );
at += len;
len = 16;
(void) strncpy(pnt_fdesc->logvol_id, (char*) (buf+at), len );
pnt_fdesc->logvol_id[len] = '\0';
(void) trimr( pnt_fdesc->logvol_id );
at += len;
len = 16;
(void) strncpy(pnt_fdesc->volset_id, (char*) (buf+at), len );
pnt_fdesc->volset_id[len] = '\0';
(void) trimr( pnt_fdesc->volset_id );
at += len;
len = 2;
pnt_fdesc->phyvol_cnt = (short) get_I4(&buf[at], len);
at += len;
len = 2;
pnt_fdesc->first_phyvol = (short) get_I4(&buf[at], len);
at += len;
len = 2;
pnt_fdesc->last_phyvol = (short) get_I4(&buf[at], len);
at += len;
len = 2;
pnt_fdesc->curr_phyvol = (short) get_I4(&buf[at], len);
at += len;
len = 4;
pnt_fdesc->first_file =  get_I4(&buf[at], len);
at += len;
len = 4;
pnt_fdesc->volset_log =  get_I4(&buf[at], len);
at += len;
len = 4;
pnt_fdesc->phyvol_log = get_I4(&buf[at], len);
at += len;
len = 8;
(void) strncpy(pnt_fdesc->logvol_date, (char*) (buf+at), len );
pnt_fdesc->logvol_date[len] = '\0';
(void) trimr( pnt_fdesc->logvol_date );
at += len;
len = 8;
(void) strncpy(pnt_fdesc->logvol_time, (char*) (buf+at), len );
pnt_fdesc->logvol_time[len] = '\0';
(void) trimr( pnt_fdesc->logvol_time );
at += len;
len = 12;
(void) strncpy(pnt_fdesc->logvol_country, (char*) (buf+at), len );
pnt_fdesc->logvol_country[len] = '\0';
(void) trimr( pnt_fdesc->logvol_country );
at += len;
len = 8;
(void) strncpy(pnt_fdesc->logvol_agency, (char*) (buf+at), len );
pnt_fdesc->logvol_agency[len] = '\0';
(void) trimr( pnt_fdesc->logvol_agency );
at += len;
len = 12;
(void) strncpy(pnt_fdesc->logvol_facility, (char*) (buf+at), len );
pnt_fdesc->logvol_facility[len] = '\0';
(void) trimr( pnt_fdesc->logvol_facility );
at += len;
len = 4;
pnt_fdesc->n_filepoint = get_I4(&buf[at], len);
at += len;
len = 4;
pnt_fdesc->n_voldir = get_I4(&buf[at], len);
at += len;
len = 4;
pnt_fdesc->n_tot_logvol = get_I4(&buf[at], len);
at += len;
len = 88;
(void) strncpy(pnt_fdesc->spare2, (char*) (buf+at), len );
pnt_fdesc->spare2[len] = '\0';
(void) trimr( pnt_fdesc->spare2 );
at += len;
len = 100;
(void) strncpy(pnt_fdesc->spare3, (char*) (buf+at), len );
pnt_fdesc->spare3[len] = '\0';
(void) trimr( pnt_fdesc->spare3 );
at += len;


if(  !Dump  ){
    (void) free( buf );
    return( IMS_OK );
}
/* *************************  Debug output  *********************
        print out the results
************************************ */
(void) printf(
"\n*************** Begin File Directory Record *************\n");
(void) printf(
"*************** (Part of Volume Directory File) *************\n");
(void) printf("\n RECORD SEQUENCE        %ld",pnt_d->rec_seq);
(void) printf("\n RECORD SUB 1           %d",pnt_d->rec_sub1);
(void) printf("\n RECORD TYPE            %d",pnt_d->rec_type);
(void) printf("\n RECORD SUB 2           %d",pnt_d->rec_sub2);
(void) printf("\n RECORD SUB 3           %d",pnt_d->rec_sub3);
(void) printf("\n RECORD LENGTH          %ld\n",pnt_d->length);
(void) printf("\n ACSII FLAG             %s",pnt_fdesc->ascii_flag);
(void) printf("\n DOCUMENT FORMAT        %s",pnt_fdesc->format_doc);
(void) printf("\n FORMAT VERSION         %s",pnt_fdesc->format_rev);
(void) printf("\n FORMAT REVISION        %s",pnt_fdesc->format_rev);
(void) printf("\n SOFTWARE ID            %s",pnt_fdesc->software_id);
(void) printf("\n PHYSICAL TAPE ID       %s",pnt_fdesc->phyvol_id);
(void) printf("\n LOGICAL SET ID         %s",pnt_fdesc->logvol_id);
(void) printf("\n VOLUME SET ID          %s",pnt_fdesc->volset_id);
(void) printf("\n NO. OF PHYSICAL VOLS   %d",pnt_fdesc->phyvol_cnt);
(void) printf("\n 1st P. VOL SEQ NO.     %d",pnt_fdesc->first_phyvol);
(void) printf("\n LAST P. VOL SEQ NO.    %d",pnt_fdesc->last_phyvol);
(void) printf("\n CURR P. VOL SEQ NO.    %d",pnt_fdesc->curr_phyvol);
(void) printf("\n 1st REF FILE IN VOL    %ld",pnt_fdesc->first_file);
(void) printf("\n LOGICAL VOL IN SET     %ld",pnt_fdesc->volset_log);
(void) printf("\n LOGICAL VOL IN PHY VOL %ld",pnt_fdesc->phyvol_log);
(void) printf("\n TAPE CREATION DATE     %s",pnt_fdesc->logvol_date);
(void) printf("\n TAPE CREATION TIME     %s",pnt_fdesc->logvol_time);
(void) printf("\n TAPE CREATION CNTRY    %s",pnt_fdesc->logvol_country);
(void) printf("\n TAPE CREATION AGENCY   %s",pnt_fdesc->logvol_agency);
(void) printf("\n TAPE CREATION FACILITY %s",
    pnt_fdesc->logvol_facility);
(void) printf("\n NO. OF POINTER RECS    %ld",pnt_fdesc->n_filepoint);
(void) printf("\n NO. OF RECORDS         %ld",pnt_fdesc->n_voldir);
(void) printf("\n NO. OF TOT,LOG VOLS    %ld",pnt_fdesc->n_tot_logvol);
if(  pnt_fdesc->spare1[0]  !=  '\0'  ){
    (void) printf("\n SPARE 1                %s",pnt_fdesc->spare1);
}
if(  pnt_fdesc->spare2[0]  !=  '\0'  ){
    (void) printf("\n SPARE 2                %s",pnt_fdesc->spare2);
}
if(  pnt_fdesc->spare3[0]  !=  '\0'  ){
    (void) printf("\n SPARE 3                %s",pnt_fdesc->spare3);
}

(void) free( buf );
(void) printf(
"\n*************** End of File Directory Record ***************\n");
return( IMS_OK );
}   /*  read_vdr   */


int  read_file_ptr( pnt_vdf_file_t  pnt_vdf, unsigned char * buf,
    short *flag_type, IMS_MSG_STRUCT * msgDesc )
/* ***************************************
    subr read_file_ptr process the buffer data into the
        pnt_rec area.  the strings are zero ended and
        with blanks taken from the end.
    there are three posibilities:  this is the leader file record,
        trailer file record or the imagery otpions file.  this
        has to be determined using the variable class_code.
        flag = 1 if leader, 0 if trailer, 2 if imagery.

******************************************* */
{
pnt_desc_rec_t  pnt_d ;
int  at,len;
pnt_file_ptr_rec_t  pnt_rec;
short  flag2;
pnt_granule_data_t  pnt_gdata,pnt_gdata2;

char *  trimr( char * );


pnt_rec = (pnt_file_ptr_rec_t)  malloc( sizeof( File_Ptr_Rec ) );
pnt_d = &(pnt_rec->desc);

pnt_d->rec_seq = cvt2int(buf);
pnt_d->rec_sub1 = *(buf+4);
pnt_d->rec_type = *(buf+5);
pnt_d->rec_sub2 = *(buf+6);
pnt_d->rec_sub3 = *(buf+7);
pnt_d->length = cvt2int(&buf[8]);

at = 12;
len = 2;
(void) strncpy(pnt_rec->ascii_flag, (char*) (buf+at), len);
pnt_rec->ascii_flag[len] = '\0';
(void) trimr( pnt_rec->ascii_flag );
at += len;
len = 2;
(void) strncpy(pnt_rec->spare1, (char*) (buf+at), len);
pnt_rec->spare1[len] = '\0';
(void) trimr( pnt_rec->spare1 );
at += len;
len = 4;
/* *******  should be 2  */
pnt_rec->file_num = get_I4(&buf[at], len);
at += len;
len = 16;
(void) strncpy(pnt_rec->file_name, (char*) (buf+at), len);
pnt_rec->file_name[len] = '\0';
(void) trimr( pnt_rec->file_name );
at += len;
len = 28;
(void) strncpy(pnt_rec->file_class, (char*) (buf+at), len);
pnt_rec->file_class[len] = '\0';
(void) trimr( pnt_rec->file_class );
at += len;

*flag_type = -1;
if(  ims_strIndex( pnt_rec->file_class, "LEA" )  !=  -1 )
    *flag_type = 1;
else  if(  ims_strIndex( pnt_rec->file_class, "TRA" )  !=  -1 )
    *flag_type = 0;
else  if(  ims_strIndex( pnt_rec->file_class, "IMA" )  !=  -1 )
    *flag_type = 2;
if(  *flag_type  ==  -1  ){
    (void) ims_msg( msgDesc, IMS_ERROR,
        "Process_vdf:  File Pointer record not recognized." );
    return(  NO_MATCH );
}
/*  now store the data in the appropriate place  */
pnt_gdata = pnt_vdf->pnt_gdata;
if(  *flag_type  ==  1  ){   /*  leader file  */
    flag2 = TRUE;
    while( flag2 ){
        if(  pnt_gdata->pnt_file_rec  ==  NULL  ){ /* put it here  */
            pnt_gdata->pnt_file_rec = pnt_rec;
            flag2 = FALSE;
        }
        else{
            pnt_gdata2 = pnt_gdata;
            pnt_gdata = pnt_gdata->pnt_next;
            if(  pnt_gdata  ==  NULL  ){ /* need to allocate another */
                pnt_gdata = (pnt_granule_data_t) malloc( sizeof(
                    granule_data_t ) );
                pnt_gdata->pnt_text_rec = NULL;
                pnt_gdata->pnt_file_rec = NULL;
                pnt_gdata->pnt_tfile_rec = NULL;
                pnt_gdata->pnt_ifile_rec = NULL;
                pnt_gdata->pnt_next = NULL;
                pnt_gdata2->pnt_next = pnt_gdata;
            }
        }
    }
}
else  if(  *flag_type  ==  0  ){   /*  trailer file  */
    flag2 = TRUE;
    while( flag2 ){
        if(  pnt_gdata->pnt_tfile_rec  ==  NULL  ){ /* put it here  */
            pnt_gdata->pnt_tfile_rec = pnt_rec;
            flag2 = FALSE;
        }
        else{
            pnt_gdata2 = pnt_gdata;
            pnt_gdata = pnt_gdata->pnt_next;
            if(  pnt_gdata  ==  NULL  ){ /* need to allocate another */
                pnt_gdata = (pnt_granule_data_t) malloc( sizeof(
                    granule_data_t ) );
                pnt_gdata->pnt_text_rec = NULL;
                pnt_gdata->pnt_file_rec = NULL;
                pnt_gdata->pnt_ifile_rec = NULL;
                pnt_gdata->pnt_tfile_rec = NULL;
                pnt_gdata->pnt_next = NULL;
                pnt_gdata2->pnt_next = pnt_gdata;
            }
        }
    }
}
else  if(  *flag_type  ==  2  ){   /*  imagery options file  */
    flag2 = TRUE;
    while( flag2 ){
        if(  pnt_gdata->pnt_ifile_rec  ==  NULL  ){ /* put it here  */
            pnt_gdata->pnt_ifile_rec = pnt_rec;
            flag2 = FALSE;
        }
        else{
            pnt_gdata2 = pnt_gdata;
            pnt_gdata = pnt_gdata->pnt_next;
            if(  pnt_gdata  ==  NULL  ){ /* need to allocate another */
                pnt_gdata = (pnt_granule_data_t) malloc( sizeof(
                    granule_data_t ) );
                pnt_gdata->pnt_text_rec = NULL;
                pnt_gdata->pnt_file_rec = NULL;
                pnt_gdata->pnt_tfile_rec = NULL;
                pnt_gdata->pnt_ifile_rec = NULL;
                pnt_gdata->pnt_next = NULL;
                pnt_gdata2->pnt_next = pnt_gdata;
            }
        }
    }
}


len = 4;
(void) strncpy(pnt_rec->file_code, (char*) (buf+at), len);
pnt_rec->file_code[len] = '\0';
(void) trimr( pnt_rec->file_code );
at += len;
len = 28;
(void) strncpy(pnt_rec->data_type, (char*) (buf+at), len);
pnt_rec->data_type[len] = '\0';
(void) trimr( pnt_rec->data_type );
at += len;
len = 4;
(void) strncpy(pnt_rec->data_code, (char*) (buf+at), len);
pnt_rec->data_code[len] = '\0';
(void) trimr( pnt_rec->data_code );
at += len;
len = 8;
/* *******  should be 4  */
pnt_rec->nrec = get_I4(&buf[at], len);
at += len;
len = 8;
/* *******  should be 4  */
pnt_rec->first_len = get_I4(&buf[at], len);
at += len;
len = 8;
/* *******  should be 4  */
pnt_rec->max_len = get_I4(&buf[at], len);
at += len;
len = 12;
(void) strncpy(pnt_rec->len_type, (char*) (buf+at), len );
pnt_rec->len_type[len] = '\0';
(void) trimr( pnt_rec->len_type );
at += len;
len = 4;
(void) strncpy(pnt_rec->len_code, (char*) (buf+at), len);
pnt_rec->len_code[len] = '\0';
(void) trimr( pnt_rec->len_code );
at += len;
len = 2;
/* *******  should be 1  */
pnt_rec->first_phyvol = (short) get_I4(&buf[at], len);
at += len;
len = 2;
/* *******  should be 1  */
pnt_rec->last_phyvol = (short) get_I4(&buf[at], len);
at += len;
len = 8;
/* *******  should be 4  */
pnt_rec->first_rec = get_I4(&buf[at], len);
at += len;
len = 8;
/* *******  should be 4  */
pnt_rec->last_rec = get_I4(&buf[at], len);
at += len;
len = 100;
(void) strncpy(pnt_rec->spare2, (char*) (buf+at), len );
pnt_rec->spare2[len] = '\0';
(void) trimr( pnt_rec->spare2 );
at += len;
len = 2;
(void) strncpy(pnt_rec->orderline, (char*) (buf+at), len );
pnt_rec->orderline[len] = '\0';
(void) trimr( pnt_rec->orderline );
at += len;
len = 16;
(void) strncpy(pnt_rec->mediaid, (char*) (buf+at), len );
pnt_rec->mediaid[len] = '\0';
(void) trimr( pnt_rec->mediaid );
at += len;
len = 3;
(void) strncpy(pnt_rec->proccode, (char*) (buf+at), len );
pnt_rec->proccode[len] = '\0';
(void) trimr( pnt_rec->proccode );
at += len;
len = 79;
(void) strncpy(pnt_rec->spare3, (char*) (buf+at), len );
pnt_rec->spare3[len] = '\0';
(void) trimr( pnt_rec->spare3 );
at += len;


if(  !Dump  )      return MATCH_FOUND;
/* *************************  Debug output  *********************
        print out the results
************************************ */

if(  *flag_type  ==  1 )   (void) printf(
"\n*************** Begin SAR Leader Pointer record *************\n");
else if(  *flag_type  ==  0  )     (void) printf(
"\n*************** Begin SAR Trailer Pointer record ************\n");
else if(  *flag_type  ==  2  )     (void) printf(
"\n*************** Begin Imagery Options record ************\n");
(void) printf(
"*************** (Part of Volume Directory File) *************\n");
(void) printf("\n RECORD SEQUENCE        %ld",pnt_d->rec_seq);
(void) printf("\n RECORD SUB 1           %d",pnt_d->rec_sub1);
(void) printf("\n RECORD TYPE            %d",pnt_d->rec_type);
(void) printf("\n RECORD SUB 2           %d",pnt_d->rec_sub2);
(void) printf("\n RECORD SUB 3           %d",pnt_d->rec_sub3);
(void) printf("\n RECORD LENGTH          %ld\n",pnt_d->length);
(void) printf("\n ACSII FLAG             %s",pnt_rec->ascii_flag);
(void) printf("\n FILE NUMBER            %d",pnt_rec->file_num  );
(void) printf("\n FILE NAME              %s",pnt_rec->file_name );
(void) printf("\n FILE CLASS             %s",pnt_rec->file_class);
(void) printf("\n FILE CLASS CODE        %s",pnt_rec->file_code  );
(void) printf("\n DATA TYPE              %s",pnt_rec->data_type);
(void) printf("\n DATA CODE              %s",pnt_rec->data_code);
(void) printf("\n NUMBER OF RECORDS      %ld",pnt_rec->nrec     );
(void) printf("\n 1ST RECORD LENGTH      %ld",pnt_rec->first_len );
(void) printf("\n MAX RECORD LENGTH      %ld",pnt_rec->max_len     );
(void) printf("\n RECORD TYPE            %s",pnt_rec->len_type);
(void) printf("\n RECORD TYPE CODE       %s",pnt_rec->len_code   );
(void) printf("\n START FILE VOL NO.     %d",pnt_rec->first_phyvol );
(void) printf("\n END FILE VOLUME NO.    %d",pnt_rec->last_phyvol );
(void) printf("\n FIRST RECORD ON TAPE   %ld",pnt_rec->first_rec );
(void) printf("\n LAST RECORD ON TAPE    %ld",pnt_rec->last_rec);
(void) printf("\n ORDER LINE             %s",pnt_rec->orderline  );
(void) printf("\n MEDIA ID               %s",pnt_rec->mediaid);
(void) printf("\n PRODUCT CODE           %s",pnt_rec->proccode);
if(  pnt_rec->spare1[0]  !=  '\0'  ){
    (void) printf("\n SPARE 1                %s",pnt_rec->spare1);
}
if(  pnt_rec->spare2[0]  !=  '\0'  ){
    (void) printf("\n SPARE 2                %s",pnt_rec->spare2);
}
if(  pnt_rec->spare3[0]  !=  '\0'  ){
    (void) printf("\n SPARE 3                %s",pnt_rec->spare3);
}
if(  *flag_type  ==  1 )   (void) printf(
"\n*************** End SAR Leader Pointer record *************\n");
else if(  *flag_type  ==  0  )     (void) printf(
"\n*************** End SAR Trailer Pointer record ************\n");
else if(  *flag_type  ==  2  )     (void) printf(
"\n*************** End Imagery Options record ************\n");

return  MATCH_FOUND;
}   /*  read_file_ptr   */


int  read_text_rec( unsigned char * buf,
    pnt_granule_data_t  pnt_gdata )
/* ***************************************
    subr read_text_rec process the buffer data into the
        pnt_tex area.  the strings are zero ended and
        with blanks taken from the end.
******************************************* */
{
pnt_desc_rec_t  pnt_d ;
int  at,len;
pnt_text_rec_t  pnt_text;

char *  trimr( char * );


if(  pnt_gdata->pnt_text_rec  ==  NULL  )
    pnt_gdata->pnt_text_rec = (pnt_text_rec_t)  malloc( sizeof(
    Text_Rec ) );
pnt_text = pnt_gdata->pnt_text_rec;
pnt_d = &(pnt_text->desc);

pnt_d->rec_seq = cvt2int(buf);
pnt_d->rec_sub1 = *(buf+4);
pnt_d->rec_type = *(buf+5);
pnt_d->rec_sub2 = *(buf+6);
pnt_d->rec_sub3 = *(buf+7);
pnt_d->length = cvt2int(&buf[8]);

at = 12;
len = 2;
(void) strncpy(pnt_text->ascii_flag, (char*) (buf+at), len);
pnt_text->ascii_flag[len] = '\0';
(void) trimr( pnt_text->ascii_flag );
at += len;
len = 2;
pnt_text->cont_flag[0] = '\0';
(void) strncpy(pnt_text->cont_flag, (char*) (buf+at), len);
pnt_text->cont_flag[len] = '\0';
(void) trimr( pnt_text->cont_flag );
at += len;
len = 40;
(void) strncpy(pnt_text->product_type, (char*) (buf+at), len);
pnt_text->product_type[len] = '\0';
(void) trimr( pnt_text->product_type );
at += len;
len = 60;
(void) strncpy(pnt_text->product_create, (char*) (buf+at), len);
pnt_text->product_create[len] = '\0';
(void) trimr( pnt_text->product_create );
at += len;
len = 40;
(void) strncpy(pnt_text->phyvol_id, (char*) (buf+at), len);
pnt_text->phyvol_id[len] = '\0';
(void) trimr( pnt_text->phyvol_id );
at += len;
len = 40;
(void) strncpy(pnt_text->scene_id, (char*) (buf+at), len );
pnt_text->scene_id[len] = '\0';
(void) trimr( pnt_text->scene_id );
at += len;
len = 40;
(void) strncpy(pnt_text->scene_loc, (char*) (buf+at), len );
pnt_text->scene_loc[len] = '\0';
(void) trimr( pnt_text->scene_loc );
at += len;
len = 20;
(void) strncpy(pnt_text->copyright_info, (char*) (buf+at), len );
pnt_text->copyright_info[len] = '\0';
(void) trimr( pnt_text->copyright_info );
at += len;
len = 104;
(void) strncpy(pnt_text->spare2, (char*) (buf+at), len );
pnt_text->spare2[len] = '\0';
(void) trimr( pnt_text->spare2 );
at += len;

if(  !Dump  )      return  MATCH_FOUND;
/* *************************  Debug output  *********************
        print out the results
************************************ */
(void) printf(
"\n*************** Begin Text Record *************\n");
(void) printf(
"*************** (Part of Volume Directory File) *************\n");
(void) printf("\n RECORD SEQUENCE        %ld",pnt_d->rec_seq);
(void) printf("\n RECORD SUB 1           %d",pnt_d->rec_sub1);
(void) printf("\n RECORD TYPE            %d",pnt_d->rec_type);
(void) printf("\n RECORD SUB 2           %d",pnt_d->rec_sub2);
(void) printf("\n RECORD SUB 3           %d",pnt_d->rec_sub3);
(void) printf("\n RECORD LENGTH          %ld\n",pnt_d->length);
(void) printf("\n ACSII FLAG             %s",pnt_text->ascii_flag);
(void) printf("\n PRODUCT TYPE SPEC      %s",pnt_text->product_type );
(void) printf("\n LOCATION, DATE/TIME    %s",pnt_text->product_create );
(void) printf("\n PHYSICAL VOLUME ID     %s",pnt_text->phyvol_id);
(void) printf("\n SCENE ID               %s",pnt_text->scene_id);
(void) printf("\n SCENE LOCATION         %s",pnt_text->scene_loc);
(void) printf("\n COPYRIGHT INFO         %s",pnt_text->copyright_info );
if(  pnt_text->cont_flag[0]  !=  '\0'  ){
    (void) printf("\n SPARE 1  (CONT FLAG)   %s",pnt_text->cont_flag );
}
if(  pnt_text->spare2[0]  !=  '\0'  ){
    (void) printf("\n SPARE 2                %s",pnt_text->spare2);
}
(void) printf(
"\n*************** End of Text Record ***************\n");

return  MATCH_FOUND;
}   /*  read_text_rec   */


void  free_vdf( pnt_vdf_file_t pnt_vdf )
/* ***************************************
    subr free_vdf frees all the allocated structures
        in the vdf tree.
******************************************* */
{
pnt_granule_data_t  pnt_gdata, pnt_gdata2;


free( pnt_vdf->pnt_descript );
free( pnt_vdf->pnt_vdr );
pnt_gdata = pnt_vdf->pnt_gdata;
while(  pnt_gdata  !=  NULL ){
    if(  pnt_gdata->pnt_file_rec  !=  NULL )  free(
        pnt_gdata->pnt_file_rec );
    if(  pnt_gdata->pnt_tfile_rec !=  NULL )  free(
        pnt_gdata->pnt_tfile_rec);
    if(  pnt_gdata->pnt_ifile_rec !=  NULL )  free(
        pnt_gdata->pnt_ifile_rec);
    if(  pnt_gdata->pnt_text_rec  !=  NULL )  free(
        pnt_gdata->pnt_text_rec );
    pnt_gdata2 = pnt_gdata;
    pnt_gdata = pnt_gdata->pnt_next;
    free( pnt_gdata2 );
}
free( pnt_vdf );
return;
}   /*  free_vdf   */
