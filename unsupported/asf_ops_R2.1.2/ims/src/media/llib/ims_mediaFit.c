static char *sccs = "@(#)ims_mediaFit.c	5.2  03/07/97";
/********************************************************************
*
*  Name: MediaFit
*
*  Module Type: PROCEDURE    Language: C
*
*  Purpose: Determine how products fit onto media for user orders
*           for digital media (CCT and 8MM).  Write CEOS volume
*           directory file out for each piece of media.
*           Note:  for R1B, only one media allowed.  put all onto
*               one tape.
*
*  Input Parameters:
*
*  Name            Type             Description
*  orderid         DBINT            Order id.
*  pnt_first_prod  pnt_product_t    Pointer to first digital product.
*  pnt_last_prod   pnt_product_t    Pointer to last digital product.
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
*   Author:   DBMAN     Revision:   2.0     Date:   17 Jul 1990 15:21:54
*   Author:   JULIE     Revision:   2.2     Date:   05 Aug 1992 14:57:32
*   Author:   LARRY     Revision:   2.3     Date:   26 May 1993 13:46:28
*   Author:   LARRY     Revision:   2.4     Date:   28 Jun 1993 15:25:04
*   Author:   D. Pass   Revision:   R1B     Date:   18 May 1995
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

int  MediaFit (orderid, pnt_first_prod, pnt_last_prod, debug_print,
    pnt_vdfReq, msgDesc, no_size_check )
DBINT orderid;
pnt_product_t   pnt_first_prod;
pnt_product_t   pnt_last_prod;
short  debug_print; /* print flag: to std out  */
pnt_vdf_cat_request_t  pnt_vdfReq;/* storage for database requests */
IMS_MSG_STRUCT  *msgDesc;
int  no_size_check; /*  if true, ignore tape size constrint  */
{

int curr_vol;
int curr_file;
int logvolset;
float   free_space, prodspace ;
pnt_product_t   pnt_curr_prod;
pnt_product_t   pnt_first_mg_prod;
pnt_product_t   pnt_last_mg_prod;
pnt_product_t   pnt_temp_prod;
double  ftemp, ftemp2;
int  first_space;

int stat, printflag ;

int  MediaSpace( pnt_product_t, char *, char *, double, int, double *,
    pnt_vdf_cat_request_t, IMS_MSG_STRUCT * );
int  WriteVolDir(  DBINT, pnt_product_t, pnt_product_t, int,
    pnt_vdf_cat_request_t, IMS_MSG_STRUCT *  );

/* retrieve the print flag from the database */
/* **********************************
## RETRIEVE ( yesno = print_flag.MediaFit )
## INQUIRE_INGRES ( errnum = ERRORNO, nrows = ROWCOUNT )
if ( errnum != 0 )
{
##      INQUIRE_INGRES ( emsg = ERRORTEXT )
##      PROMPT (
## "INGRES error in MediaFit.qc.  Press Return and report message: ",
##      ans ) WITH STYLE=POPUP
##      PROMPT ( emsg, ans )  WITH STYLE=POPUP
        (void) strcpy ( yesno, "N" ) ;
    nrows = 1 ;
}
if ( nrows != 1 )
{
        (void) sprintf ( emsg,
"PRINT_FLAG db relation does not have 1 record.  Press Return" );
##      PROMPT ( emsg, ans ) WITH STYLE=POPUP
##      PROMPT ( "Please report this.  Press Return", ans ) WITH
##                STYLE=POPUP
}
if ( strncmp(yesno,"Y",1) == 0 )
******************************************* */

if(  debug_print ){
    /* print flag is ON */
    printflag = 1 ;
}
else{
    /* print flag is OFF */
    printflag = 0 ;
}

if ( printflag == 1 )   {
    (void) printf ( " MediaFit:  starting.  order=%ld \n", orderid );
    (void) printf (
        "               first prod=%s line %d, last prod=%s line %d\n",
        pnt_first_prod->prodid, pnt_first_prod->item_id,
        pnt_last_prod->prodid, pnt_last_prod->item_id );
}

/*  with R1B, all media is the same and it has already been constrained
    to fit on one tape.    */
logvolset = 1;
curr_file = 1;
pnt_curr_prod = pnt_first_prod;
pnt_temp_prod = NULL;
prodspace = 0.0;
if(  strcmp( pnt_curr_prod->mediacode, "DISK" )  ==  0 )
    pnt_curr_prod->mediacap = 999.0;
free_space = pnt_curr_prod->mediacap - prodspace ;
pnt_first_mg_prod = pnt_first_prod;
first_space = IMS_TRUE;

while (  pnt_temp_prod  !=  pnt_last_prod  ) {
    pnt_temp_prod = pnt_curr_prod;
    curr_vol = 1;

    /* Find last list entry for this media type; set pnt_last_mg_prod */
    /* to its pointer.                      */

    if ( printflag == 1 )   {
        (void) printf (
            " MediaFit:  Now assigning to media %s, vol %d "
            "capacity %d megabytes.\n",
        pnt_curr_prod->mediacode, curr_vol, pnt_curr_prod->mediacap ) ;
    }

    if ( strcmp( pnt_first_prod->mediacode,
        pnt_curr_prod->mediacode)  == 0  ) {
        /*  type of media is same for this one - add
            this one.  */
        if ( printflag == 1 ) (void) printf (
            " MediaFit:  current prodid=%s, line %d\n",
            pnt_curr_prod->prodid, pnt_curr_prod->item_id );
        /* change in the code 5-23-93 LJS.  */
        /* compute the space taken up in the output media, which   */
        /* will vary according to the kind of tape or other factors. */
        if(  strcmp( pnt_curr_prod->mediacode, "DISK" )  ==  0 ){
            /* for disk, no size checks  */
            free_space = 999.0;
            ftemp2 = 0.0;
        }
        else{
            ftemp = pnt_curr_prod->prodsize;
            stat = MediaSpace( pnt_curr_prod, pnt_curr_prod->mediacode,
                pnt_curr_prod->prodtype,
                ftemp, printflag,
                &ftemp2, pnt_vdfReq, msgDesc ) ;
            if(  stat  <  IMS_OK )  return( stat );
        }
        prodspace = ftemp2;
        free_space = free_space - prodspace ;
        if ( free_space  <=  0.0  &&  !no_size_check ){
            /* this product makes the tape too big - print error  */
            (void) ims_msg ( msgDesc, IMS_ERROR,
            "MediaFit: Order too big for media at item %d.  Cap = %ld",
                pnt_curr_prod->item_id, pnt_curr_prod->mediacap );
            return( IMS_ERROR );
        }
        else{
            if ( free_space  <=  0.0  &&  first_space ){
                (void) ims_msg ( msgDesc, IMS_WARNING,
                "MediaFit: Order too big for media at item %d.  "
                    "Cap = %ld", pnt_curr_prod->item_id,
                    pnt_curr_prod->mediacap );
                first_space = IMS_FALSE;
            }
            /* this product can fit on the media  */
            pnt_curr_prod->selected = TRUE;
            /* change in the code 5-23-93 LJS.  */
            /* use prodspace instead of product size */
            if ( printflag == 1 ){
                (void) printf (
            " MediaFit:  PRODID %s LINE %d ASSIGNED TO %s VOL %d\n",
                    pnt_curr_prod->prodid,
                    pnt_curr_prod->item_id, pnt_curr_prod->mediacode,
                    curr_vol ) ;
                    (void) printf (
                "                 space remaining=%f.\n",free_space );
            }
            pnt_curr_prod->rec_beg = 1;
            pnt_curr_prod->rec_end = pnt_curr_prod->num_recs;
            pnt_curr_prod->curr_vol = curr_vol;
            pnt_curr_prod->phy_vol1 = curr_vol;
            pnt_curr_prod->phy_vol2 = curr_vol;
            pnt_curr_prod->file_num = curr_file++;
            /* note: this is not always true, but this value
                is currently not used.  */
            curr_file = curr_file + 2;
            pnt_last_mg_prod = pnt_curr_prod;
        }
    }
    pnt_curr_prod = pnt_curr_prod->pnt_next;
}

if ( printflag == 1 ){
    (void) printf(" MediaFit:  All %s completely assigned.\n",
    pnt_first_prod->mediacode );
}
stat = WriteVolDir (orderid,pnt_first_mg_prod,pnt_last_mg_prod,logvolset,
    pnt_vdfReq, msgDesc );
if(  stat  <  IMS_OK )  return( stat );
/*  logvolset++;    */

return ( IMS_OK );
}   /*  MediaFit    */
