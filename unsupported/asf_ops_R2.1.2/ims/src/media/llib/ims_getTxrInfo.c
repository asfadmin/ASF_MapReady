static char *sccs = "@(#)ims_getTxrInfo.c	5.1  03/17/96";
/********************************************************************
*
*  Name: GetTXRInfo
*
*  Module Type: PROCEDURE    Language: C
*
*  Purpose: Collect info for use in the CEOS text record for a product.
*
*
*  Input Parameters:
*
*  Name          Type        Description
*  prodid        *char       Product ID.
*  prodtype      *char       Product type code.
*
*
*  Output Parameters:
*
*  Name          Type        Description
*  retstat       int         Return status (-1 bad, 1 = good)
*  sat           *char       Satellite.
*  rev           *int        Rev number.
*  ctime         *char       Product center time.
*  clat          *float      Center lat.
*  clon          *float      Center lon.
*
*  Modification History:
*
*  Date:   05 Oct 1989 10:16:24   Revision:   1.0   Author:   TOM
*  Date:   06 Feb 1990 15:24:28   Revision:   1.1   Author:   TOM
*  Date:   30 Apr 1990 15:17:46   Revision:   1.2   Author:   TOM
*  Date:   29 May 1990 14:08:06   Revision:   1.3   Author:   TOM
*  Date:   07 Jun 1990 11:15:56   Revision:   1.4   Author:   TOM
*  Date:   17 Jul 1990 15:15:42   Revision:   2.0   Author:   DBMAN
*  Date:   10 Aug 1990 15:50:44   Revision:   2.1   Author:   DBMAN
*  Date:   17 Aug 1993  9:58:48   Revision:   2.2   Author:   RODNEY
*  Date:   22 May 1995  R1B:  David Pass - for vdf formation of CEOS
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


int GetTXRInfo( sat, rev, ctime, clat, clon, pnt_vdfReq )
char            *sat;
int             *rev;
char            *ctime;
double          *clat;
double          *clon;
pnt_vdf_cat_request_t  pnt_vdfReq;
{



/* *********************************
pnt_vdfReq->pnt_save_prod = pnt_this_prod;
event = VDF_GET_TEXT_INFO;
status = ims_vdfCat( pnt_vdfReq, event );
if(  status  <  IMS_OK  ){
    return( status );
}
************************************ */

/* ******************************
    previously, calls to alot of different tables depending on
    prodtype.  this is now granules_table in dataset_policy.
    The data is in the granules table.
    ****  call now done in get_prod_list
********************************** */

(void) strcpy (sat, pnt_vdfReq->l_sat);
*rev = pnt_vdfReq->l_rev;
(void) strcpy (ctime, pnt_vdfReq->l_time);
*clat = pnt_vdfReq->l_lat;
*clon = pnt_vdfReq->l_lon;

return ( IMS_OK );
}   /*  GetTXRInfo  */
