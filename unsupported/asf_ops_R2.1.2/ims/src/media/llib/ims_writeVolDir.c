static char *sccs = "@(#)ims_writeVolDir.c	5.1  03/17/96";
/********************************************************************
*
*  Name: WriteVolDir
*
*  Module Type: PROCEDURE    Language: C
*
*  Purpose: Write CEOS volume directory file out based on product
*               buffer.
*
*  Input Parameters:
*
*  Name             Type            Description
*  orderid          DBINT           Order id.
*  pnt_first_prod   pnt_product_t   Pointer to first product.
*  pnt_last_prod    pnt_product_t   Pointer to last product.
*  logvolset        int             Logical volume set sequence number
*                                   within this order.
*
*
*  Output Parameters:
*
*  Name          Type        Description
*  NONE
*
*
*  Modification History:
*
*  Author:   TOM      Revision:   1.0   Date:   13 Dec 1988 10:29:34
*  Author:   TOM      Revision:   1.1   Date:   19 Apr 1989  9:56:28
*  Author:   TOM      Revision:   1.2   Date:   16 Jun 1989 15:41:20
*  Author:   TOM      Revision:   1.3   Date:   30 Aug 1989 14:15:42
*  Author:   TOM      Revision:   1.4   Date:   05 Oct 1989 10:16:40
*  Author:   TOM      Revision:   1.5   Date:   28 Feb 1990 10:30:30
*  Author:   TOM      Revision:   1.7   Date:   07 Jun 1990 16:03:56
*  Author:   DBMAN    Revision:   2.0   Date:   18 Jul 1990  9:28:10
*  Author:   DBMAN    Revision:   2.1   Date:   10 Aug 1990 15:30:40
*  Author:   MARIAN   Revision:   2.2   Date:   07 Dec 1990 15:35:08
*  Author:   DBMAN    Revision:   2.3   Date:   02 Apr 1992 19:35:08
*  Author:   RODNEY   Revision:   2.4   Date:   17 Aug 1993  9:57:58
*
*********************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <sys/types.h>
#include    <unistd.h>
#include    <sys/utsname.h>
#include    <ctype.h>
#include    <sys/stat.h>
#include    <sys/stat.h>
#include    <fcntl.h>
#include    <errno.h>

#include    <ims_dbms.h>
#include    <ims_const.h>
#include    <ims_msg.h>
#include    <ims_qi.h>
#include    <ims_media.h>
#include    <ims_util.h>


int  WriteVolDir (orderid, pnt_first_prod, pnt_last_prod, logvolset,
    pnt_vdfReq, msgDesc )
DBINT orderid;
pnt_product_t   pnt_first_prod;
pnt_product_t   pnt_last_prod;
int logvolset;
pnt_vdf_cat_request_t  pnt_vdfReq;
IMS_MSG_STRUCT  * msgDesc;
{

pnt_product_t   pnt_curr_prod;
pnt_product_t   pnt_prev_prod;
pnt_product_t   pnt_next_prod;
char    file_name[256];
char    logical_vol[17];
char    vdf_filenm[17];
char    vdf_temp[21];
char    medialoc[17];
char    curr_time[22];
int max_vol;
int file_count;
int txr_count;
int curr_vol;
int curr_rec;
int file_desc;
int write_stat;
int istat;
int rend, rbeg;
char    sat[14];
int rev;
char    ctime[22];
double   clat;
double   clon;
short   more_change;

char    sw_version[17];
char    fclass[5];
int     numrec;
int     reclen1;
int     maxlen;
char    ptype[33];
char    msg[100];
long    save_maxlen;

void    GetTime( char * );
int     GetTXRInfo( char *, int *, char *, double *, double *,
            pnt_vdf_cat_request_t );
int     WriteVDR( int, char *, char *, int, int, int, int, int );
int     WriteFPR( int, char *, int, char *, int, int, int, int, int,
            int, int, int, int, int, char *, char *, char * );
int     WriteTXR( int, int, char *, char *, char *, char *, int, int,
            char *, double, double, char *, int );


max_vol = 0;
file_count = 0;
txr_count = 0;
pnt_curr_prod = pnt_first_prod;

/*  first ge the number of files and text records
    for this order */

while (pnt_curr_prod != pnt_last_prod->pnt_next){

    if ( (pnt_curr_prod->curr_vol) > max_vol ){
        max_vol = pnt_curr_prod->curr_vol;
    }

    if (pnt_curr_prod->rec_beg == 1){
        txr_count++;
        file_count++;
        /* always ceos format  */
        file_count++;

        /*  need to check to see if a TLR file exists
            for this item.  */
        pnt_vdfReq->pnt_save_prod = pnt_curr_prod;
        save_maxlen =  pnt_vdfReq->maxlen;
        pnt_vdfReq->maxlen = 0;
        event = VDF_GET_PATH;
        istat = ims_vdfCat (   pnt_vdfReq,  event);
        if( istat  <  IMS_OK )  return( istat );
        /*  use the selected value: all are selected,
            and this is not used.  */
        if(  pnt_vdfReq->maxlen  ==  0 )
            pnt_curr_prod->selected = IMS_TRUE; /* is tlr file */
        else  pnt_curr_prod->selected = IMS_FALSE;/* no tlr file */
        if(  pnt_curr_prod->selected  )  file_count++;
        pnt_vdfReq->maxlen  = save_maxlen;
    }
    pnt_curr_prod = pnt_curr_prod->pnt_next;
}


more_change = TRUE;
while (more_change){
    more_change = FALSE;
    pnt_prev_prod = NULL;
    pnt_curr_prod = pnt_first_prod;
    pnt_next_prod = pnt_curr_prod->pnt_next;
    while (pnt_next_prod != pnt_last_prod->pnt_next){
        if (pnt_curr_prod->file_num > pnt_next_prod->file_num){
            if (pnt_curr_prod == pnt_first_prod)
                pnt_first_prod = pnt_curr_prod->pnt_next;

            if (pnt_prev_prod != NULL)
                pnt_prev_prod->pnt_next = pnt_curr_prod->pnt_next;

            pnt_curr_prod->pnt_next = pnt_next_prod->pnt_next;
            pnt_next_prod->pnt_next = pnt_curr_prod;

            pnt_prev_prod = pnt_next_prod;
            pnt_next_prod = pnt_curr_prod->pnt_next;

            more_change = TRUE;
        }
        else{
            pnt_prev_prod = pnt_curr_prod;
            pnt_curr_prod = pnt_next_prod;
            pnt_next_prod = pnt_next_prod->pnt_next;
        }
    }
}



/* Create file name */

(void) sprintf (logical_vol,"%ld%02d",orderid,logvolset);
(void) sprintf (vdf_filenm,
    "%ld%03d",orderid, pnt_first_prod->item_id);

/*  get staging area  */
event = VDF_GET_STAGE_AREA;
pnt_vdfReq->stage_type = CEOS_VDF;
istat = ims_vdfCat (   pnt_vdfReq,  event);
if( istat  <  IMS_OK )  return( istat );

(void) sprintf (vdf_temp, "%s.VDF", vdf_filenm);
ims_concatFilePath (file_name, pnt_vdfReq->path_name, vdf_temp);

/* Create file */
file_desc = creat (file_name, 0700);

if (file_desc == -1){
    (void) ims_msg( msgDesc, IMS_ERROR,
      "WriteVolDir: Could not open the VDF file '%s'. %s",
        file_name, strerror (errno));
    return ( IMS_ERROR );
}

/* Write out volume descriptor record */

/* *******************************
##  retrieve (sw_version = ancversion.version)
##      where ancversion.sysname = "ACS"
********************************* */
(void) strcpy( sw_version, "SW_Ver r1b " );
/* *************  should I get this from the data base????  */

curr_vol = 1;
pnt_curr_prod = pnt_first_prod;
while (pnt_curr_prod->curr_vol != curr_vol)
    pnt_curr_prod = pnt_curr_prod->pnt_next;

write_stat = WriteVDR (file_desc,
        logical_vol,
        sw_version,
        max_vol,
        curr_vol,
        pnt_curr_prod->file_num,
        file_count,
        txr_count);

if (write_stat == -1){
    (void) ims_msg( msgDesc, IMS_ERROR,
        "WriteVolDir: Cannot write VDR to file." );
    return ( IMS_ERROR );
}

/* Write out FPR records */

curr_rec = 2;
pnt_curr_prod = pnt_first_prod;
while (pnt_curr_prod != pnt_last_prod->pnt_next) {
    (void) strcpy(medialoc,pnt_curr_prod->mediaid);

    (void) sprintf (fclass,"SARL");
    (void) sprintf (ptype,"%s",pnt_curr_prod->prodtype);
/*
##      range of ac is ancceoslen;
##      range of op is ordproduct;
##      repeat retrieve (   numrec = ac.numrecords,
##                      reclen1 = ac.reclenfirst,
##                      maxlen = ac.reclenmax)
##      where   ac.fileclass = @fclass
##      and ac.prodtype = @ptype;
 */
    /* values alread obtained in get_prod_list call  */
    numrec =  pnt_vdfReq->numrec;
    reclen1 = pnt_vdfReq->reclen1;
    maxlen =  pnt_vdfReq->maxlen;

    if ((curr_vol == pnt_curr_prod->phy_vol1) &&
        (curr_vol == pnt_curr_prod->curr_vol)) {
        rbeg = 1;
        rend = numrec;
    }
    else{
        rbeg = 0;
        rend = 0;
    }

    if ((rbeg != 0) || ((rbeg == 0) && (pnt_curr_prod->rec_beg
        ==  1))){
        write_stat = WriteFPR (file_desc,
            pnt_curr_prod->prodid,
            curr_rec,
            fclass,
            pnt_curr_prod->file_num,
            numrec,
            reclen1,
            maxlen,
            curr_vol,
            pnt_curr_prod->phy_vol1,
            pnt_curr_prod->phy_vol1,
            rbeg,
            rend,
            pnt_curr_prod->item_id,
            medialoc,
            pnt_curr_prod->prodcreate,
            pnt_curr_prod->prodftype);


        if (write_stat <  IMS_OK ){
            (void) sprintf(msg,
                "Error writing FPR record to file\0");
            (void) ims_msg( msgDesc, IMS_ERROR,
                "WriteVolDir: Cannot write FPR to file." );
            return ( IMS_ERROR );
        }

        curr_rec++;
    }


    if ((curr_vol >= pnt_curr_prod->phy_vol1)&&(curr_vol <=
        pnt_curr_prod->phy_vol2)) {
        if (pnt_curr_prod->curr_vol == curr_vol) {
            rbeg = pnt_curr_prod->rec_beg;
            rend = pnt_curr_prod->rec_end;
        }
        else{
            rbeg = -1;
            rend = -1;
        }
    }
    else{
        if (pnt_curr_prod->rec_beg == 1){
            rbeg = 0;
            rend = 0;
        }
        else{
            rbeg = -1;
            rend = -1;
        }
    }

    if (rbeg >= 0) {

/******* Added by Craig *******/
/*
    If the file type is variable, the record length for the first
    record and the maximum record length are the same and equal
    the length of the File Descriptor Record. The length of the FDR is
    stored in the ANCCEOSLEN table; reclenfirst column.
*/
        if (strcmp(pnt_curr_prod->prodftype,"VARE") == 0){
            (void) sprintf (fclass,"SARL");
            (void) sprintf (ptype,"%s",pnt_curr_prod->prodtype);
/*
##          range of ac is ancceoslen;
##          range of op is ordproduct;
##          repeat retrieve (reclen1 = ac.reclenfirst)
##          where   ac.fileclass = @fclass
##          and ac.prodtype = @ptype;
  */
            /* values already obtained */
            pnt_curr_prod->rec_len = reclen1;
        }
/*****************************/

        write_stat = WriteFPR (file_desc,
            pnt_curr_prod->prodid,
            curr_rec,
            "IMOP",
            (pnt_curr_prod->file_num)+1,
            pnt_curr_prod->num_recs,
            pnt_curr_prod->rec_len,
            pnt_curr_prod->rec_len,
            curr_vol,
            pnt_curr_prod->phy_vol1,
            pnt_curr_prod->phy_vol2,
            rbeg,
            rend,
            pnt_curr_prod->item_id,
            medialoc,
            pnt_curr_prod->prodcreate,
            pnt_curr_prod->prodftype);

        if (write_stat <  IMS_OK ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "WriteVolDir: Error writing FPR record to file.");
            return( IMS_ERROR );
        }

        pnt_curr_prod->written = TRUE;
        curr_rec++;

        (void) strcpy ( (pnt_curr_prod->vdf_name),file_name);

    }

    (void) sprintf (fclass,"SART");
    (void) sprintf (ptype,"%s",pnt_curr_prod->prodtype);
/*
##      range of ac is ancceoslen;
##      range of op is ordproduct;
##      repeat retrieve (   numrec = ac.numrecords,
##                      reclen1 = ac.reclenfirst,
##                      maxlen = ac.reclenmax)
##      where   ac.fileclass = @fclass
##      and ac.prodtype = @ptype;
*/
    if ((curr_vol == pnt_curr_prod->phy_vol2) &&
        (curr_vol == pnt_curr_prod->curr_vol)){
        rbeg = 1;
        rend = numrec;
    }
    else{
        rbeg = 0;
        rend = 0;
    }


    if ((rbeg != 0) ||
        ((rbeg == 0) && (pnt_curr_prod->rec_end ==
        pnt_curr_prod->num_recs))){
        if(  pnt_curr_prod->selected ){
            /* file exits  */
            write_stat = WriteFPR (file_desc,
                pnt_curr_prod->prodid,
                curr_rec,
                fclass,
                (pnt_curr_prod->file_num+2),
                numrec,
                reclen1,
                maxlen,
                curr_vol,
                pnt_curr_prod->phy_vol2,
                pnt_curr_prod->phy_vol2,
                rbeg,
                rend,
                pnt_curr_prod->item_id,
                medialoc,
                pnt_curr_prod->prodcreate,
                pnt_curr_prod->prodftype);

            if (write_stat <  IMS_OK){
                (void) ims_msg( msgDesc, IMS_ERROR,
                "WriteVolDir:  Error writing FPR record to file.");
                return( IMS_ERROR );
            }
            curr_rec++;
        }
    }
    pnt_curr_prod = pnt_curr_prod->pnt_next;
}

/* Write out text records */

GetTime(curr_time);

pnt_curr_prod = pnt_first_prod;
while (pnt_curr_prod != pnt_last_prod->pnt_next){
    if (pnt_curr_prod->rec_beg == 1){
        istat = GetTXRInfo (
            sat, &rev, ctime, &clat, &clon, pnt_vdfReq );
        if(istat <  IMS_OK ){
            (void) sprintf (msg,
"WriteVolDir:  Error getting TEXT record for %s; order %ld.\0",
                pnt_curr_prod->prodid, orderid);
            (void) ims_msg( msgDesc, IMS_ERROR, msg );
            return ( IMS_ERROR );
        }

        write_stat = WriteTXR ( file_desc,
                curr_rec,
                logical_vol,
                pnt_curr_prod->prodid,
                pnt_curr_prod->prodtype,
                curr_time,
                pnt_curr_prod->phy_vol1,
                max_vol,
                ctime,
                clat,
                clon,
                sat,
                rev);

        if (write_stat  <  IMS_OK ){
            (void) sprintf(msg,
            "WriteVolDir: Error writing TEXT record to file.");
            (void) ims_msg( msgDesc, IMS_ERROR, msg );
            return( IMS_ERROR );
        }
    }

    pnt_curr_prod = pnt_curr_prod->pnt_next;
    curr_rec++;
}

/* Close file */
(void) close(file_desc);

return( IMS_OK );
}   /*  WriteVolDir */
