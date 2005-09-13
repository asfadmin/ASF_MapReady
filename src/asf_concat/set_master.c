/*******************************************************************************
NAME			       SET_MASTER

PURPOSE	     Calculates the master line and sample fields of the output DDR 

EXTERNAL ASSOCIATIONS
	This routine is called by C_UPDDR routine

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
B. Ailts	      Jan. 1988 	original development
B. Ailts	      Apr. 1988		Replace newlas.h with las.h
					Update to use current constants
					Update to use current increment value
					specifications
					
PROJECT       LAS

ALGORITHM 
   Check to see if the image was windowed
   If a window was specified
      If the increment flag is valid
         If the starting line is greater than the line increment
	    output master line = input master line + (line increment value *
	             		 (starting line - 1))
	 else
	    output master line = input master line + (starting line *
	             		 (line increment value - 1))
         endif

         If the starting sample is greater than the sample increment
	    output master sample = input master sample + (sample increment 
	             		   value * (starting sample - 1))
	 else
	    output master sample = input master sample + (starting sample *
	             		   (sample increment value - 1))
         endif
      else
         output master line = 1
         output master sample = 1
   else
      ouptut master line = input master line
      output master sample = input master sample
   endif

   return
  
ALGORITHM REFERENCES	none
*******************************************************************************/
#include "las.h"

void FUNCTION set_master(const struct DDR in_ddr[],struct DDR *out_ddr,int inc_flag,int window[])
{
int  wind_flag;		/* window flag				*/

/* set windowing flag 
---------------------*/
wind_flag = FALSE;
   
if (window[0] != 1)
   wind_flag = TRUE;
if (window[1] != 1)
   wind_flag = TRUE;

/* set output master coordinate values 
--------------------------------------*/
if (wind_flag)
   {
   if (inc_flag == VALID)
      {
      /*  calculate the output DDR master line
      ----------------------------------------*/
      if (window[0] > in_ddr->line_inc)
	 out_ddr->master_line = in_ddr->master_line + (in_ddr->line_inc *
			        (window[0] - 1));

      else
	 out_ddr->master_line = in_ddr->master_line + (window[0] *
			        (in_ddr->line_inc - 1));

      /*  calculate the output DDR master sample
      ------------------------------------------*/
      if (window[1] > in_ddr->sample_inc)
	 out_ddr->master_sample = in_ddr->master_sample + (in_ddr->sample_inc *
			        (window[1] - 1));

      else
	 out_ddr->master_sample = in_ddr->master_sample + (window[1] *
			        (in_ddr->sample_inc - 1));
      } /*  if inc_flag == VALID  */

   else
      { /* if inc_flag != valid, set output DDR master line and sample to 1,1 */
      out_ddr->master_line = 1;
      out_ddr->master_sample = 1;
      } /* if inc_flag != valid */
   } /* if wind_flag  */

else  /* copy input to output */
   {
   out_ddr->master_line = in_ddr->master_line;
   out_ddr->master_sample = in_ddr->master_sample;
   }

return;
}  /* set_master */
