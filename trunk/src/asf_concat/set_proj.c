/*******************************************************************************
NAME			       SET_PROJ

PURPOSE	     Compares the projection code fields of two or more input DDR files

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
B. Ailts	      Dec. 1990		Updated error messages
					
PROJECT       LAS

ALGORITHM 
   Check the proj_code validity flags for each input DDR
      Set the flags accordingly
   If one is invalid 
      Set the proj_code field of the output ddr to be INVALID

   If all are valid, check to see if the values are all equal
      If they are not equal 
	 Make the proj_code valid flag to be INVALID
	 Place a zero into the proj_code field
      Else
         Copy the first input value to the output value
         Set the proj_code valid flag to VALID
      Endif

   else if all the validity flags are unknown, check to see if the values are 	     equal
      If they are not equal 
	 Make the proj_code valid flag to be UNKNOWN
	 Place a zero into the proj_code field
      Else
         Copy the first input value to the output value
         Set the proj_code valid flag to UNKNOWN
      Endif

   Else the validity flags are both unknown and valid, check only the DDRs with
      valid flags set to valid and see if the values are equal
      If they are not equal 
	 Make the proj_code valid flag to be INVALID
	 Place a zero into the proj_code field
      Else
         Copy the first input value to the output value
         Set the proj_code valid flag to VALID
      Endif

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "las.h"

void FUNCTION set_proj(const struct DDR in_ddr[],struct DDR *out_ddr,int *proj_flag,int nimg)

{ /* set_proj */

int first;			/* first valid flag			   */
int i;				/* loop counter				   */
int index=0;			/* index for first DDR with a valid proj flag*/
int invalid;			/* invalid flag				   */
int unknown;			/* unknown flag				   */
int valid;			/* valid flag				   */

char errtxt[ERRLEN + 1];	/* buffer for the error message		   */

/*  Set the invalid, valid, and unknown flags
---------------------------------------------*/
invalid = FALSE;
valid = TRUE;
unknown = TRUE;

for (i = 0; (i < nimg) && (invalid != TRUE); i++)
   {
   if (in_ddr[i].valid[DDPCV] == INVAL)
      invalid = TRUE;
   if (in_ddr[i].valid[DDPCV] != VALID)
      valid = FALSE;
   if (in_ddr[i].valid[DDPCV] != UNKNOW)
      unknown = FALSE;
   }

if (invalid)	/*  invalidate the projection code of output ddr	*/
   {
   out_ddr->proj_code = 0;
   out_ddr->valid[DDPCV] = INVAL;
   *proj_flag = INVAL;
   } /* if invalid */

else if (valid)	/* compare the projection codes of the input ddrs	*/
   {
   *proj_flag = VALID;
   for (i = 0; (i < nimg - 1) && (*proj_flag != INVAL); i++)
      {
      if (in_ddr[i].proj_code != in_ddr[i + 1].proj_code)
         {
	 *proj_flag = INVAL;
 	 out_ddr->proj_code = 0;
	 out_ddr->valid[DDPCV] = INVAL;
	 sprintf(errtxt,
	 "Projection code values for image %d and %d are not equal",
	 i + 1,i + 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	 }
      } /* for i < nimg */

   /*  If the projection codes are equal, copy the input projection code
       to the output projection code
   ---------------------------------------------------------------------*/
   if (*proj_flag == VALID)
      {
      out_ddr->proj_code = in_ddr[0].proj_code;
      out_ddr->valid[DDPCV] = VALID;
      }
   } /* else if valid */

else if (unknown)
   {
   *proj_flag = USAME;
   for (i = 0; (i < nimg - 1) && (*proj_flag != UDIFF); i++)
      {
      if (in_ddr[i].proj_code != in_ddr[i + 1].proj_code)
	 {
	 *proj_flag = UDIFF;
	 out_ddr->proj_code = 0;
	 out_ddr->valid[DDPCV] = UNKNOW;
	 sprintf(errtxt,
	 "Projection code values for image %d and %d are not equal",
	 i + 1,i + 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	 }
      } /* for i < nimg */

   /*  If the projection codes are equal, copy the input projection code
       to the output projection code
   ---------------------------------------------------------------------*/
   if (*proj_flag == USAME)
      {
      out_ddr->proj_code = in_ddr[0].proj_code;
      out_ddr->valid[DDPCV] = UNKNOW;
      }
   } /* else if unknown */

else  /*  input validity flags are both unknown and valid  */
   {
   *proj_flag = VALID;
   first = TRUE;
   for (i = 0; (i < nimg) && (*proj_flag != INVAL); i++)
      {
      if (in_ddr[i].valid[DDPCV] == VALID)
	 {
	 if (first)
	    {
	    first = FALSE;
	    index = i;
	    } /* if first */

	 else
	    {
	    if (in_ddr[index].proj_code != in_ddr[i].proj_code)
	       {
	       out_ddr->proj_code =0;
	       out_ddr->valid[DDPCV] = INVAL;
	       *proj_flag = INVAL;
	       sprintf(errtxt,
	       "Projection code values for image %d and %d are not equal",
		index + 1,i + 1);
	       c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	       } /* if !first */
	    } /* else */
	 } /*  if in_ddr[i].valid[DDPCV] == valid */
     } /* for i < nimg */

   /*  If the projection codes are equal, copy the input projection code
       to the output projection code
   ---------------------------------------------------------------------*/
   if (*proj_flag == VALID)
      {
      out_ddr->proj_code = in_ddr[index].proj_code;
      out_ddr->valid[DDPCV] = VALID;
      }
   } /* else */

return;
} /* set_proj */
