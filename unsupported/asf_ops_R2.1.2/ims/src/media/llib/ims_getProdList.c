static char *sccs = "@(#)ims_getProdList.c	5.4  08/15/97";
/********************************************************************
*
*  Name: GetProdList
*
*  Module Type: PROCEDURE    Language: C
*
*  Purpose: Retrieve from the database, all the product information
*           for a given set of order lines.  Put each line item into
*           a node in the product list.
*
*
*  Input Parameters:
*
*  Name          Type         Description
*  msgDesc       c*           Message Facility descriptor.
*  orderid       DBINT        Order id.
*  items         DBSMALLINT[] Array of items to process.
*  n_items       int          Count of items.
*
*
*  Output Parameters:
*
*  Name              Type             Description
*  pnt_first_prod    pnt_product_t    Pointer to first product in list.
*
*
*
*  Modification History:
*
*  Author:   TOM      Revision:   1.1       Date:   10 Jan 1989  8:08:20
*  Author:   TOM      Revision:   1.2       Date:   14 Mar 1989 14:45:34
*  Author:   TOM      Revision:   1.3       Date:   18 Apr 1989 13:30:54
*  Author:   TOM      Revision:   1.4       Date:   19 Apr 1989  8:23:44
*  Author:   TOM      Revision:   1.5       Date:   21 Aug 1989 15:23:36
*  Author:   TOM      Revision:   1.6       Date:   14 Sep 1989 10:37:34
*  Author:   TOM      Revision:   1.7       Date:   05 Oct 1989 10:16:14
*  Author:   TOM      Revision:   1.7       Date:   05 Oct 1989 10:16:14
*  Author:   TOM      Revision:   1.11      Date:   19 Dec 1989 16:03:40
*  Author:   TOM      Revision:   1.12      Date:   06 Feb 1990 15:23:36
*  Author:   TOM      Revision:   1.13      Date:   28 Feb 1990 10:29:14
*  Author:   TOM      Revision:   1.14      Date:   06 Mar 1990 15:20:14
*  Author:   TOM      Revision:   1.15      Date:   30 Mar 1990  9:10:58
*  Author:   TOM      Revision:   1.16      Date:   30 Mar 1990 13:17:52
*  Author:   DBMAN    Revision:   2.0       Date:   17 Jul 1990 15:14:12
*  Author:   DBMAN    Revision:   2.1       Date:   10 Aug 1990 15:49:36
*  Author:   MARIAN   Revision:   2.2       Date:   07 Dec 1990 15:24:50
*  Author:   INGRID   Revision:   2.3       Date:   06 Apr 1992 12:26:24
*  Author:   INGRID   Revision:   2.5       Date:   14 May 1992 19:07:38
*  Author:   RODNEY   Revision:   2.6       Date:   14 Apr 1993 14:40:18
*  Author:   MIGUEL   Revision:   2.7       Date:   08 Aug 1994 11:10:46
*  Author:   RODNEY   Revision:   2.8       Date:   08 Aug 1994 12:47:52
*
*********************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <sys/types.h>

#include    <ims_dbms.h>
#include    <ims_const.h>
#include    <ims_msg.h>
#include    <ims_util.h>
#include    <ims_qi.h>
#include    <ims_media.h>

/***************************************************************
**
** GetProdList ()
**
**************************************************************** */

int GetProdList (
    IMS_MSG_STRUCT *msgDesc,
    DBINT orderid,
    DBSMALLINT items[],
    int n_items,
    int *ceosFlag,
    pnt_vdf_cat_request_t pnt_vdfReq,
    pnt_product_t *out_prod )
{

pnt_product_t   pnt_curr_prod;
pnt_product_t   pnt_next_prod;
pnt_product_t   pnt_first_prod;
char    str[257];
int     stat;
int flag;
int n_prods;

int leader_size     =   22000;
int trailer_size    =   1000;


/* Find lines which can go on common media by retrieving with sort */

/* *********************************
##  range of o is #ordline;
##  range of p is #ordproduct;
##  range of c is #ordprocess;
##  range of m is #ordmedia;
##  range of u is table_name;
*************************************** */

n_prods = 0;
pnt_first_prod = malloc(sizeof( product_t ));
pnt_curr_prod = NULL;
pnt_next_prod = pnt_first_prod;
pnt_next_prod->order_id = orderid;  /* this used for search */
pnt_next_prod->item_id = items[n_prods];  /* this used for search */
pnt_next_prod->selected = FALSE;
pnt_next_prod->written = FALSE;
pnt_next_prod->CEOS = TRUE;
pnt_next_prod->prodtype[0] = '\0';
pnt_next_prod->prodsize = 0.0;
pnt_next_prod->prodcreate[0] = '\0';
pnt_next_prod->prodftype[0] = '\0';
pnt_next_prod->prodid[0] = '\0';
pnt_next_prod->mediaid[0] = '\0';
pnt_next_prod->mediacode[0] = '\0';
pnt_next_prod->mediacap = 0;
pnt_next_prod->quantity = 1;
pnt_next_prod->file_num = 0;
pnt_next_prod->num_recs = 0;
pnt_next_prod->rec_len = 0;
pnt_next_prod->curr_vol = 1;
pnt_next_prod->phy_vol1 = 1;
pnt_next_prod->phy_vol2 = 1;
pnt_next_prod->rec_beg = 0;
pnt_next_prod->rec_end = 0;
pnt_next_prod->vdf_name[0] = '\0';
pnt_next_prod->format[0] = '\0';
pnt_next_prod->extCount = 0;
pnt_next_prod->extArray = (char **) NULL;
pnt_next_prod->pnt_next = NULL;

/* ******************************************
##  retrieve (
##      next_prod->lineid = o.#lineid,
##      next_prod->prodtype = p.#prodtype,
##      next_prod->prodcreate = p.#prodcreate,
##      next_prod->prodftype = p.#prodftype,
##      next_prod->prodid = o.#prodid,
##      next_prod->mediaid = uppercase(o.#mediaid),
##      next_prod->mediacode = o.#mediacode,
##      next_prod->mediacap = m.#mediacap,
##      next_prod->stepproc = o.#stepproc,
##      next_prod->quantity = o.#quantity)
##  where   o.#orderid = ordernum
##  and o.ordtype = ordertype
##  and p.prodpcnum = int4(left(shift(ascii(o.procnum),-2),3))
##  and m.#mediacode = o.#mediacode
##  and o.#lineid = u.#lineid
##  and o.#stepproc != "GCFR"
##  and o.#stepproc != "GCLR"
##  and o.#stepproc != "NGFR"
##  and o.#stepproc != "NGLR"
##      and     o.#stepproc != "DCMP"
##  sort by #mediacode
##  {
**************************************** */

flag = IMS_TRUE;
while (flag == IMS_TRUE)
{
    pnt_next_prod->quantity = 1;
    pnt_vdfReq->pnt_save_prod = pnt_next_prod;
    pnt_vdfReq->granule_table[0] = '\0';

    /*
    ** Get the product order information.
    */
    if ((stat = ims_vdfCat (pnt_vdfReq, VDF_GET_PRODUCT_LIST)) < IMS_OK)
    {
        return (stat);
    }
    if(  pnt_vdfReq->no_data ){
        /*
        **  could not find the order/item in the database
        */
        (void) ims_msg( msgDesc, IMS_ERROR,
            "Database did not have item %ld from order %ld",
            pnt_vdfReq->pnt_save_prod->item_id,
            pnt_vdfReq->pnt_save_prod->order_id );
        return( IMS_ERROR );
    }
    /*
    ** Get the product format information.
    */
    if ((stat = ims_vdfCat (pnt_vdfReq, VDF_GET_PRODUCT_FORMAT)) < IMS_OK)
    {
        return (stat);
    }

    /*
    ** Check the format of the granule.
    ** We do not want to gather CEOS information if a product
    ** is not in the CEOS format.
    */
    if ((strcmp (pnt_vdfReq->pnt_save_prod->format, "CEOS") != 0) &&
        (strcmp (pnt_vdfReq->pnt_save_prod->format, "CEOS_OLD") != 0))
    {
        *ceosFlag = IMS_FALSE;
    }
    else
    {
        /*
        ** Get the product CEOS information.
        */
        if ((stat = ims_vdfCat (pnt_vdfReq, VDF_GET_CEOS_INFO)) < IMS_OK)
        {
            return (stat);
        }

        /*
        ** If we have data than fill out the rest of the product information.
        */
        if (!pnt_vdfReq->no_data)
        {
            pnt_next_prod->selected = FALSE;
            pnt_next_prod->written = FALSE;

            /* This done here due to needing prodid  */
            pnt_vdfReq->pnt_save_prod = pnt_next_prod;

            /*
            ** Get the product text information.
            */
            if ((stat = ims_vdfCat (pnt_vdfReq, VDF_GET_TEXT_INFO)) < IMS_OK)
            {
                return (IMS_ERROR);
            }
        }
        else /* data not found  */
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Order item %d not found or has an error.",
                items[n_prods]);
            *out_prod = NULL;
            return ( IMS_ERROR );
        }
    }

    if (pnt_curr_prod != NULL)
    {
        pnt_curr_prod->pnt_next = pnt_next_prod;
    }

    /*
    ** Initialize the next structure.
    */
    n_prods++;
    pnt_curr_prod = pnt_next_prod;
    pnt_next_prod = (pnt_product_t) malloc (sizeof(product_t));
    pnt_next_prod->order_id = orderid;  /* this used for search */
    pnt_next_prod->item_id = items[n_prods];/* used for search */
    pnt_next_prod->selected = FALSE;
    pnt_next_prod->written = FALSE;
    pnt_next_prod->CEOS = TRUE;
    pnt_next_prod->prodtype[0] = '\0';
    pnt_next_prod->prodsize = 0.0;
    pnt_next_prod->prodcreate[0] = '\0';
    pnt_next_prod->prodftype[0] = '\0';
    pnt_next_prod->prodid[0] = '\0';
    pnt_next_prod->mediaid[0] = '\0';
    pnt_next_prod->mediacode[0] = '\0';
    pnt_next_prod->mediacap = 0;
    pnt_next_prod->quantity = 1;
    pnt_next_prod->file_num = 0;
    pnt_next_prod->num_recs = 0;
    pnt_next_prod->rec_len = 0;
    pnt_next_prod->curr_vol = 1;
    pnt_next_prod->phy_vol1 = 1;
    pnt_next_prod->phy_vol2 = 1;
    pnt_next_prod->rec_beg = 0;
    pnt_next_prod->rec_end = 0;
    pnt_next_prod->vdf_name[0] = '\0';
    pnt_next_prod->pnt_next = NULL;

    /*
    ** Check for the last item.
    */
    if (n_prods == n_items)
    {
        flag = IMS_FALSE;
    }
}

/*
** See if we have something.
*/
if (pnt_curr_prod == NULL)
{
    (void) ims_msg( msgDesc, IMS_ERROR,
        "GetProdList:  No products for this order id." );
    return ( IMS_ERROR );
}
pnt_curr_prod->pnt_next = NULL;

/*
** Skip the rest of this function if one of the products
** was not in CEOS format.
*/
if (*ceosFlag == IMS_FALSE)
{
    *out_prod = pnt_first_prod;
    return (IMS_OK);
}

pnt_curr_prod = pnt_first_prod;
while (pnt_curr_prod != NULL)
{
    pnt_curr_prod->num_recs += 1;

    pnt_curr_prod->prodsize =
        ((pnt_curr_prod->rec_len)*((pnt_curr_prod->num_recs)+1)) /
        1000000.0;

    pnt_curr_prod->prodsize +=  ((leader_size + trailer_size) /
        1000000.0);

    pnt_curr_prod = pnt_curr_prod->pnt_next;
}
*out_prod = pnt_first_prod;
return ( IMS_OK );
}       /*  GetProdList   */
