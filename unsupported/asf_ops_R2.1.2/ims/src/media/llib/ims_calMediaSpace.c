static char *sccs = "@(#)ims_calMediaSpace.c	6.1  03/13/98";
/********************************************************************
*
*  Name: ims_calMediaSpace
*
*  Module Type: PROCEDURE    Language: C
*
*  Purpose: Determine the space taken up on a media by a product.
*   The product is to be copied to an output media and the space
*   taken up will depend on the media type, the product size,
*   and the product type. This program assumes a tape product.
*
*
*  written by David Pass  4/7/97
*
*********************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <sys/types.h>

#include    <ims_const.h>
#include    <ims_msg.h>


int  ims_calMediaSpace( float  rec_gap, float  file_gap,
    int num_data_recs, int num_size_recs, char * media_fmt_type,
    double * prod_space  )
/*
**    rec_gap           kbytes for writting record
**    file_gap          kbytes for writting file
**    num_data_recs     no. of data records
**    num_size_recs     size of data records in bytes
**    media_fmt_type    either "CEOS" or "TAR"
**    prod_space        space used on disk by this product
*/
{
    static  int leader_size     =   22000;
    static  int trailer_size    =   1000;
    double  temp1, temp2;


    if(  media_fmt_type[0]  !=  'T' ){
        /*
        **  ceos type
        */
        temp2 = (  num_size_recs * ( num_data_recs+1 ) ) /  1000.0;
        temp2 +=  ((leader_size + trailer_size) /  1000.0);

        *prod_space = ( temp2 + ( num_data_recs + 10.0 ) * rec_gap +
            2.0 * file_gap) / 1000.0;
    }
    else{
        /*
        **  tar type: assume 10k records, no file marks
        */
        temp2 = (  num_size_recs * ( num_data_recs+1 ) ) /  1000.0;
        temp2 +=  ((leader_size + trailer_size) /  1000.0);

        temp1 = temp2/ 10000; /* *** divide by tar block size ***  */

        *prod_space = ( temp2 + temp1 * rec_gap )/ 1000.0;
    }
    return( IMS_OK ) ;
}   /*  ims_calMediaSpace  */
