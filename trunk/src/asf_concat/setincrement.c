/*******************************************************************************
NAME			       SET_INCREMENT

PURPOSE	     Calculates the increment fields of the output DDR files

EXTERNAL ASSOCIATIONS
	This routine is called by C_UPDDR routine
	This routine is used in conjunction with SET_INC_FLAG

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
B. Ailts	      Jan. 1988 	original development
B. Ailts	      Apr. 1988		Replace newlas.h with las.h
					Update to use current constants
B. Ailts	      Dec. 1990		Updated error messages
					
PROJECT       LAS

ALGORITHM 
   If the inc_flag for equals INVAL
      Assign INVAL the the increment validity flag of the output DDR
      Assign a zero the the increment value of the output DDR

   Else if the inc_flag for equals UDIFF
      Assign UDIFF the the increment validity flag of the output DDR
      Assign a zero the the increment value of the output DDR

   Else 
      If the inc_flag for equals USAME
         Assign UKNOW the the increment validity flag of the output DDR
      Else 
         Assign VALID the the increment validity flag of the output DDR
      Endif
      Calculate the line increment value of the output DDR
      If line_inc is < and > 0 -- expansion
         Out_line_inc equals in_line_inc times line_inc
      Else if line_inc > 1 -- subsampling
         If in_line_inc > 1 -- subsampling
	    Out_line_inc equals line_inc times in_line_inc
	 Else if in_line_inc < 1 and > 0 -- expansion
	    Out_line_inc equals line_inc times in_line_inc
	 Else if in_line_inc < 0
	    Output an errmsg
	 Else
	    Out_line_inc equals in_line_inc
      Else if line_inc < 0
	 Output an errmsg
      Else
	 Out_line_inc equals in_line_inc

      Calculate the sample increment value of the output DDR
      If samp_inc is < and > 0 -- expansion
         Out_samp_inc equals in_samp_inc times samp_inc
      Else if samp_inc > 1 -- subsampling
         If in_samp_inc > 1 -- subsampling
	    Out_samp_inc equals samp_inc times in_samp_inc
	 Else if in_samp_inc < 1 and > 0 -- expansion
	    Out_samp_inc equals samp_inc times in_samp_inc
	 Else if in_samp_inc < 0
	    Output an errmsg
	 Else
	    Out_samp_inc equals in_samp_inc
      Else if samp_inc < 0
	 Output an errmsg
      Else
	 Out_samp_inc equals in_samp_inc
   Endif
   Return

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "las.h"

lasErr FUNCTION set_increment(int inc_flag,const struct DDR *in_ddr,struct DDR *out_ddr,double in_line_inc,double in_samp_inc)
{

char errtxt[ERRLEN + 1];	/*  temporary buffer for the error message */

if (inc_flag == INVAL)
   {  /* invalidate the increment values of output DDR  */
   out_ddr->line_inc = 0;
   out_ddr->sample_inc = 0;
   out_ddr->valid[DDINCV] = INVAL;
   }  /* if INVAL */

else if (inc_flag == UDIFF)  
   { /* unknown flag, values not equal */
   out_ddr->line_inc = 0;
   out_ddr->sample_inc = 0;
   out_ddr->valid[DDINCV] = UNKNOW;
   } /* if UDIFF */
   
else    /* UEQ or VALID for flag, values equal */
   {
   /* set output corner validity flag */
   if (inc_flag == USAME)
      out_ddr->valid[DDINCV] = UNKNOW;
   else
      out_ddr->valid[DDINCV] = VALID;
      
   /* recalculate line increment values 
   ------------------------------------*/
   if ((in_line_inc < 1) && (in_line_inc > 0))
      {
      out_ddr->line_inc = in_ddr->line_inc * in_line_inc;
      } /* if in_line_inc < -1 */
   else if (in_line_inc > 1)
      {
      if (in_ddr->line_inc > 1)
	 out_ddr->line_inc = in_line_inc * in_ddr->line_inc;
      else if ((in_ddr->line_inc < 1) && (in_ddr->line_inc > 0))
	    out_ddr->line_inc = in_ddr->line_inc * in_line_inc;
      else if (in_ddr->line_inc < 0)
         {
	 sprintf(errtxt,"%f is an invalid increment value",in_ddr->line_inc);
         c_errmsg(errtxt,"upddr-value",NON_FATAL);
         return(E_FAIL);
         }
      else
	 out_ddr->line_inc = in_line_inc;
      } /* if in_line_inc > 1  */
   else if (in_line_inc < 0)
      {
      sprintf(errtxt,"%f is an invalid increment value",in_line_inc);
      c_errmsg(errtxt,"upddr-value",NON_FATAL);
      return(E_FAIL);
      }
   else
      out_ddr->line_inc = in_ddr->line_inc;

   /* Calculate the sample increment values
   ----------------------------------------*/
   if ((in_samp_inc < 1) && (in_samp_inc > 0))
      out_ddr->sample_inc = in_ddr->sample_inc * in_samp_inc;
   else if (in_samp_inc > 1)
      {
      if (in_ddr->sample_inc > 1)
	 out_ddr->sample_inc = in_samp_inc * in_ddr->sample_inc;
      else if ((in_ddr->sample_inc < 1) && (in_ddr->sample_inc > 0))
	 out_ddr->sample_inc = in_ddr->sample_inc * in_samp_inc;
      else if (in_ddr->sample_inc < 0)
         {
         sprintf(errtxt,"%f is an invalid increment value",in_ddr->sample_inc);
         c_errmsg(errtxt,"upddr-value",NON_FATAL);
         return(E_FAIL);
         }
      else
	 out_ddr->sample_inc = in_samp_inc;
      } /* if in_samp_inc > 1  */
   else if (in_samp_inc < 0)
      {
      sprintf(errtxt,"%f is an invalid increment value",in_samp_inc);
      c_errmsg(errtxt,"upddr-value",NON_FATAL);
      return(E_FAIL);
      }
   else
      out_ddr->sample_inc = in_ddr->sample_inc;
   } /* if equal valid or unknown */

return(E_SUCC);
}  /* set_increment */
