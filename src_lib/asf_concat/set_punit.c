/*******************************************************************************
NAME			       SET_PUNIT

PURPOSE	     Compares the proj_units fields of two or more input DDR files

EXTERNAL ASSOCIATIONS
	This routine is called by C_UPDDR routine

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
B. Ailts	      Jan. 1988 	original development
B. Ailts	      Apr. 1988		Replace newlas.h with las.h
					Changed the calling sequence -- only
					  one structure
					Update to current constants
B. Ailts	      Dec. 1990		updated error messages
					
PROJECT       LAS

ALGORITHM 
   Check the punits_code validity flags for each input DDR
      Set the flags accordingly
   If one is invalid 
      Set the punits_code field of the output ddr to be INVAL

   If all are valid, check to see if the values are all equal
      If they are not equal 
	 Make the punits_code valid flag to be INVAL
	 Place a zero into the punits_code field
      Else
         Copy the first input value to the output value
         Set the punits_code valid flag to VALID
      Endif

   else if all the validity flags are unknown, check to see if the values are 	     equal
      If they are not equal 
	 Make the punits_code valid flag to be UNKNOW
	 Place a zero into the punits_code field
      Else
         Copy the first input value to the output value
         Set the punits_code valid flag to UNKNOW
      Endif

   Else the validity flags are both unknown and valid, check only the DDRs with
      valid flags set to valid and see if the values are equal
      If they are not equal 
	 Make the punits_code valid flag to be INVAL
	 Place a zero into the punits_code field
      Else
         Copy the first input value to the output value
         Set the punits_code valid flag to VALID
      Endif

ALGORITHM REFERENCES	none
*******************************************************************************/

#include "las.h"

void FUNCTION set_punit(const struct DDR in_ddr[],struct DDR *out_ddr,int nimg)

{ /* set_punit */

int first;			/*  first valid flag			   */
int pu_flag;			/*  processing projection unit flag	   */
int i;				/*  loop counter			   */
int index=0;			/*  index of first DDR with a valid proj unit*/
int invalid;			/*  invalid flag			   */
int unknown;			/*  unknown flag			   */
int valid;			/*  valid flag				   */

char errtxt[ERRLEN + 1];	/* buffer for the error message		   */

/*  Set the invalid, valid, and unknown flags
---------------------------------------------*/
invalid = FALSE;
valid = TRUE;
unknown = TRUE;

for (i = 0; (i < nimg) && (invalid != TRUE); i++)
   {
   if (in_ddr[i].valid[DDPUV] == INVAL)
      invalid = TRUE;
   if (in_ddr[i].valid[DDPUV] != VALID)
      valid = FALSE;
   if (in_ddr[i].valid[DDPUV] != UNKNOW)
      unknown = FALSE;
   }

if (invalid)  /* invalidate the output DDR projection units	*/
   {
   strcpy(out_ddr->proj_units," ");
   out_ddr->valid[DDPUV] = INVAL;
   } /* if invalid */

else if (valid)
   {
   pu_flag = VALID;
   for (i = 0; (i < nimg - 1) && (pu_flag != INVAL); i++)
      {
      if (strcmp(in_ddr[i].proj_units,in_ddr[i + 1].proj_units) != 0)
 	 {
	 pu_flag = INVAL;
 	 strcpy(out_ddr->proj_units," ");
	 out_ddr->valid[DDPUV] = INVAL;
	 sprintf(errtxt,"Projection units of image %d and %d are not equal",
	 i + 1,i + 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	 }
      } /* for i < nimg */

   if (pu_flag == VALID)
      {
      strcpy(out_ddr->proj_units,in_ddr[0].proj_units);
      out_ddr->valid[DDPUV] = VALID;
      }
   } /* else if valid */

else if (unknown)
   {
   pu_flag = USAME;
   for (i = 0; (i < nimg - 1) && (pu_flag != UDIFF); i++)
      {
      if (strcmp(in_ddr[i].proj_units,in_ddr[i + 1].proj_units) != 0)
	 {
	 pu_flag = UDIFF;
	 strcpy(out_ddr->proj_units," ");
	 out_ddr->valid[DDPUV] = UNKNOW;
	 sprintf(errtxt,"Projection units of image %d and %d are not equal",
	 i + 1,i + 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	 }
      } /* for i < nimg */

   if (pu_flag == USAME)
      {
      strcpy(out_ddr->proj_units,in_ddr[0].proj_units);
      out_ddr->valid[DDPUV] = UNKNOW;
      }
   } /* else if unknown */

else
   {
   pu_flag = VALID;
   first = TRUE;
   for (i = 0; (i < nimg) && (pu_flag != INVAL); i++)
      {
      if (in_ddr[i].valid[DDPUV] == VALID)
	 {
	 if (first)
	    {
	    first = FALSE;
	    index = i;
	    } /* if first */
	 else
	    {
	    if (strcmp(in_ddr[index].proj_units,in_ddr[i].proj_units) != 0)
	       {
	       strcpy(out_ddr->proj_units," ");
	       out_ddr->valid[DDPUV] = INVAL;
	       pu_flag = INVAL;
	       sprintf(errtxt,
	       "Projection units of image %d and %d are not equal",
	       index + 1,i + 1);
	       c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	       } /* if !first */
	    } /* else */
	 } /*  if in_ddr[i].valid[DDPUV] == valid */
     } /* for i < nimg */

   if (pu_flag == VALID)
      {
      strcpy(out_ddr->proj_units,in_ddr[index].proj_units);
      out_ddr->valid[DDPUV] = VALID;
      }
   } /* else */

return;
} /*  set_punit  */
