static char *sccs = "@(#)ims_mediaSpace.c	5.3  08/15/97";
/***************************************************************
*
*  Name: MediaSpace
*
*  Module Type: PROCEDURE    Language: QC
*
*  Purpose: Determine the space taken up on a media by a product.
*   The product is to be copied to an output media and the space
*   taken up will depend on the media type, the product size,
*   and the product type.
*       This routine allows for the inter block gaps on CCT tapes
*   and for between file gaps or marks for CCT and 4- or 8MM tapes.
*
*  Input Parameters:
*
*  Name         Type    Description
*  mediacode    *char   media code:  8MMH, 8MML, CCTH, etc.
*           see ordmedia.mediacode for legal values
*  prodtype *char   product type:  FULL, LOWR, LRGC, FRGC, IVEC etc.
*           see ordproduct.prodtype for legal values
*  prodsize float   size in megabytes of the product.
*           originates in ordproduct.prodsize
*  printflag    int =0 no print.  =1 print.
*
*  Output Parameters:
*
*  Name          Type        Description
*  prodspace    *float  size in megabytes of the space take un on the
*           media volume by the indicated product.
*
* RETURN CODE   int 0 if OK, < 0 if there was an error.
*
*
*  Modification History:
*
*   Author:   DBMAN     Revision:   2.0     Date:   17 Jul 1990 15:21:54
*   Author:   JULIE     Revision:   2.2     Date:   05 Aug 1992 14:57:32
*   Author:   LARRY     Revision:   1.0     Date:   26 May 1993 13:46:50
*   Author:   MIGUEL    Revision:   1.1     Date:   08 Aug 1994 10:45:44
*   David Pass: 4/7/97  :  call generic routine for calculations.
*
*********************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <sys/types.h>

#include    <ims_dbms.h>
#include    <ims_const.h>
#include    <ims_msg.h>
#include    <ims_qi.h>
#include    <ims_media.h>


int  MediaSpace( this_prod, mediacode, prodtype, prodsize, printflag,
        prodspace, pnt_vdfReq, msgDesc )
pnt_product_t  this_prod;
char    *mediacode ;
char    *prodtype ;
double   prodsize ;
int     printflag ;
double   *prodspace ;
pnt_vdf_cat_request_t  pnt_vdfReq;
IMS_MSG_STRUCT * msgDesc;
{

float filegap, blkgap ;
char code[16], ptype[32] ;
char ftype[16] ;
int blocksize, nblocks ;
int recl;
int  status;
char  str[16];


if ( printflag == 1 ) (void) printf (
" MediaSpace:   mediacode=%s, prodtype=%s, prodsize=%f\n",
    mediacode, prodtype, prodsize ) ;

(void) strcpy ( code, mediacode ) ;
(void) strcpy ( ptype, prodtype ) ;

/* check media code */
if ( strncmp ( code, "CCT", 3 ) != 0 && strncmp ( code, "8MM", 3 )
     != 0  &&  strncmp ( code, "4MM", 3 ) != 0)
{
    /* the media is neither CCT nor 4 or 8MM tape. */
    /* just return the input media space.       */
    *prodspace = prodsize ;
    (void) ims_msg( msgDesc, IMS_ERROR,
        " MediaSpace:  not a tape.  code=%s", code );
    return( IMS_ERROR );
}

/* The media is a tape.  either CCT or 4 or 8MM */
if ( printflag == 1 ) (void) printf (
    " MediaSpace:  media is a tape. \n");

/* retrieve the file type and the record length for the product.  */
/* *****************************
## RETRIEVE ( ftype = ordproduct.prodftype, recl = ordproduct.prodrecl )
##  WHERE ordproduct.prodtype = ptype
## INQUIRE_INGRES ( errnum = ERRORNO, nrows = ROWCOUNT )
if ( errnum != 0 )
{
##  INQUIRE_INGRES ( emsg = ERRORTEXT )
##  PROMPT (
## "INGRES error in MediaSpace.c.  Press Return and report message: ",
##  ans ) WITH STYLE=POPUP
##  PROMPT ( emsg, ans )  WITH STYLE=POPUP
**************************************** */

pnt_vdfReq->pnt_save_prod = this_prod;
event = VDF_GET_MEDIA_SPACE;
status = ims_vdfCat (   pnt_vdfReq,  event);
if(  status <  IMS_OK  ){
    return( status );
}

(void) strcpy( ftype, this_prod->prodftype );
recl = pnt_vdfReq->recl;
blkgap = pnt_vdfReq->blkgap;
filegap = pnt_vdfReq->filegap;

if ( printflag == 1 )
    (void) printf ( " MediaSpace:  ftype=%s, recl=%d \n", ftype, recl );
/* retrieve the inter block gap size and the between file size for  */
/* the media                                */
/* *************************************
## RETRIEVE ( blkgap = ordmedia.mediablkgap, filegap =
##    ordmedia.mediafilegap )
##  WHERE ordmedia.mediacode = code
## INQUIRE_INGRES ( errnum = ERRORNO, nrows = ROWCOUNT )
if ( errnum != 0 )
{
##  INQUIRE_INGRES ( emsg = ERRORTEXT )
##  PROMPT (
## "INGRES error in MediaSpace.c.  Press Return and report message: ",
##  ans ) WITH STYLE=POPUP
##  PROMPT ( emsg, ans )  WITH STYLE=POPUP
    return -1 ;
}
*************************************** */
/*  note:  this done above  */

if ( printflag == 1 ) (void) printf (
    " MediaSpace:  blkgap=%f, filegap=%f\n", blkgap, filegap );

/* *****  product size calculations changed.  The numbers in the
    data base were obtained empirically by running the tape to where
    it was full, and then solving the equations.  the size is not
    actually the mb capacity of the tape, but the mb size plus the
    space for the records and file marks.  the size of the records
    of ceos files are 1k, 8k, and 11k, and these were used in
    determining the numbers.  The results are in the directory
    ttape (currently at dspass/asf/ceos/ttape).  the program solve
    solves the matrix equation.  the program ttape writes the tape.
    Note that prodsize includes the vdf, the file that explains the
    data file and has associated parameters, and that it adds an
    additional 10 (approx) records and an eof mark.
    David Pass  12/4/96
************************** */

    strcpy( str, "CEOS" );
    status = ims_calMediaSpace( blkgap, filegap, this_prod->num_recs,
        this_prod->rec_len, str, prodspace );

if ( printflag == 1 ) (void) printf ( " MediaSpace:  prodspace=%f\n",
    *prodspace );

return( IMS_OK ) ;
}   /*  MediaSpace  */
