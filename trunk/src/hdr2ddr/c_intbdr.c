/*******************************************************************************
NAME			       C_INTBDR

PURPOSE	     Initialize data for a band dependent record of a DDR file

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
K. Zanter	      Feb. 1988 	original development
B. Ailts	      Apr. 1988		Replaced newlas.h with las.h
					Changed the calling sequence -- now 
					only uses on structure - BDDR

PROJECT      LAS

ALGORITHM 
   Initialize the band record data with zeros if numeric values or blanks 
    if charcter strings.

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"
#include "las.h"

void FUNCTION int_c_intbdr(struct BDDR *bddr)
{
bddr->minval = 0.0;
bddr->maxval = 0.0;
bddr->bandno = 0;
bddr->valid = 0;
strcpy(bddr->source," ");
strcpy(bddr->instrument," ");
strcpy(bddr->direction," ");
strcpy(bddr->date," ");
strcpy(bddr->time," ");

return;
}  /*  c_intbdr  */
