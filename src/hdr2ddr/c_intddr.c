/*******************************************************************************
NAME			       C_INTDDR

PURPOSE	     Initialize for records 1 and 2 of a DDR file

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
K. Zanter	      Jan. 1988 	original development
B. Ailts	      Apr. 1988		Replaced newlas.h with las.h
					Changed the calling sequence -- now
					  only uses one structure - DDR
B. Davis	      JAN. 1990		initialized new "spare" field of ddr

PROJECT     LAS

ALGORITHM 
   Initialize the DDR structure with zeros and blanks as necessary.

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"

#include "las.h"
#include "proj.h"

void FUNCTION c_intddr(struct DDR *ddr)
{
int i;				/* loop index				      */

ddr->nl = 0;
ddr->ns = 0;
ddr->nbands = 0;
ddr->dtype = 0;
ddr->master_line = 1;
ddr->master_sample = 1;

/*  Set the validity flags to zero  
----------------------------------*/
for (i = 0; i < DDNVAL; i++)
    ddr->valid[i] = INVAL;

ddr->proj_code = 0;
ddr->zone_code = 0;
ddr->datum_code = 0;
ddr->spare = 0;

strcpy(ddr->system,c_getsys());
strcpy(ddr->proj_units," ");
strcpy(ddr->last_used_date," ");
strcpy(ddr->last_used_time," ");

/*  Set the 15 projection coeficients to zero
---------------------------------------------*/
for (i = 0; i < COEFCT; i++)
    ddr->proj_coef[i] = 0.0;

ddr->upleft[0] = 0.0;
ddr->upleft[1] = 0.0;
ddr->loleft[0] = 0.0;
ddr->loleft[1] = 0.0;
ddr->upright[0] = 0.0;
ddr->upright[1] = 0.0;
ddr->loright[0] = 0.0;
ddr->loright[1] = 0.0;
ddr->pdist_y = 0.0;
ddr->pdist_x = 0.0;
ddr->line_inc = 1.0;
ddr->sample_inc = 1.0;

return;
}


