/******************************************************************************
**
** File:        ims_product.h
**
** Author:      D. Pass
**
** Date:        5/18/95
**
******************************************************************************/

#ifndef _IMS_PRODUCT_H
#define _IMS_PRODUCT_H

static char *sccsProduct = "@(#)ims_product.h	5.1  03/17/96";

/*
** Structure for generating ceos and other products.
*/
typedef  struct  product__t *pnt_product_t;
typedef struct product__t{
    int     order_id;
    char    prodtype[33];
    float   prodsize;
    char    prodcreate[16];
    char    prodftype[16];
    char    prodid[16];
    int     item_id; /* no. of item in order - key other tables.  */
    char    mediaid[16];
    char    mediacode[16];
    int     mediacap;
    int     quantity;
    short   selected;
    short   written;
    short   CEOS;
    int     file_num;
    int     num_recs;
    int     rec_len;
    int     curr_vol;
    int     phy_vol1;
    int     phy_vol2;
    int     rec_beg;
    int     rec_end;
    char    vdf_name[256];
		char    name[31];
    char    format[11];
		short   version;
		short   status;
		int     extCount;
		char  **extArray;
    pnt_product_t pnt_next;
} product_t ;

#endif /* !_IMS_PRODUCT_H */
