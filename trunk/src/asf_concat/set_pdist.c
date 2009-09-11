/*******************************************************************************
NAME			       SET_PDIST

PURPOSE	     Calculates the projection distance fields of the output DDR files

EXTERNAL ASSOCIATIONS
	This routine is called by C_UPDDR routine
	This routine is used in conjunction with SET_PDIS_FLAG

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
B. Ailts	      Jan. 1988 	original development
B. Ailts	      Apr. 1988		Replace newlas.h with las.h
					Changed the calling sequence -- now only
					  uses one DDR structure
					Update to use current constants
					
PROJECT       LAS

ALGORITHM 
   If the pdist_flag for equals INVAL
      Assign INVAL the the projection distance validity flag of the output DDR
      Assign a zero the the projection distance value of the output DDR

   else if the pdist_flag for equals UDIFF
      Assign UDIFF the the projection distance validity flag of the output DDR
      Assign a zero the the projection distance value of the output DDR

   else 
      If the pdist_flag for equals USAME
         Assign UKNOW the the projection distance validity flag of the 
	   output DDR
      else 
         Assign VALID the the projection distance validity flag of the 
	   output DDR
      endif
      Calculate the projection distance in the y direction of the output DDR 
        by multiplying the input DDR projection distance in the y direction 
        by the input line increment value

      Calculate the projection distance in the x direction of the output DDR
        by multiplying the input DDR projection distance in the x direction 
        by the input sample increment value

   endif
   return
  

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "las.h"

void FUNCTION set_pdist(int pdist_flag,const struct DDR in_ddr[],
		struct DDR *out_ddr,double line_inc,double samp_inc)
{

/*  Check the input projection distance flag for INVAL, UDIFF, USAME, or VALID
------------------------------------------------------------------------------*/
if (pdist_flag == INVAL)
   {
   out_ddr->pdist_x = 0;	/*  Null out the projection distance in x dir.*/
   out_ddr->pdist_y = 0;	/*  Null out the projection distance in y dir.*/
   out_ddr->valid[DDPDV] = INVAL;	/*  Invalidate proj_dist validity flag*/
   }  /* if INVAL */

else if (pdist_flag == UDIFF)  
   /* unknown flag, values not equal */
   {
   out_ddr->pdist_x = 0;	/*  Null out the projection distance in x dir.*/
   out_ddr->pdist_y = 0;	/*  Null out the projection distance in y dir.*/
   out_ddr->valid[DDPDV] = UNKNOW;
   } /* if UDIFF */
   
else    /* USAME or VALID for flag, values equal */
   {
   /* set output corner validity flag */
   if (pdist_flag == USAME)
      out_ddr->valid[DDPDV] = UNKNOW;
   else
      out_ddr->valid[DDPDV] = VALID;
      
   /* set output ground distance values */
   out_ddr->pdist_y = in_ddr->pdist_y * line_inc;
   out_ddr->pdist_x = in_ddr->pdist_x * samp_inc;
   }

return;
}  /* set_pdist */
