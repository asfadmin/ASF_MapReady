/*******************************************************************************
NAME			       SET_DATUM

PURPOSE	     Compare the datum fields of two or more input DDR files

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
   Check the datum validity flags for each input DDR
      Set the flags accordingly
   If one is invalid 
      Set the datum field of the output ddr to be INVALID

   If all are valid, check to see if the values are all equal
      If they are not equal 
	 Make the datum valid flag to be INVALID
	 Place a zero into the datum field
      Else
         Copy the first input value to the output value
         Set the datum valid flag to VALID
      Endif

   If all the validity flags are unknown, check to see if the values are equal
      If they are not equal 
	 Make the datum valid flag to be UNKNOWN
	 Place a zero into the datum field
      Else
         Copy the first input value to the output value
         Set the datum valid flag to UNKNOWN
      Endif

   If the validity flags are both unknown and valid, check only the DDRs with
      valid flags set to valid and see if the values are equal
      If they are not equal 
	 Make the datum valid flag to be INVALID
	 Place a zero into the datum field
      Else
         Copy the first input value to the output value
         Set the datum valid flag to VALID
      Endif

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "las.h"

void FUNCTION set_datum(const struct DDR in_ddr[],struct DDR *out_ddr,int nimg)
{ /* set_datum */

int datum_flag;		/* Processing datum flag		   */
int first;			/* First DDR with a valid datum flag	   */
int i;				/* loop counter				   */
int index=0;			/* Index of first DDR with a valid datum flg */
int invalid;			/* Invalid flag				   */
int unknown;			/* Unknown flag				   */
int valid;			/* Valid flag				   */

char errtxt[ERRLEN + 1];	/*  buffer for the error message	   */

/*  Set the invalid, valid, and unknown flags
---------------------------------------------*/
invalid = FALSE;
valid = TRUE;
unknown = TRUE;

for (i = 0; (i < nimg) && (invalid != TRUE); i++)
   {
   if (in_ddr[i].valid[DDDCV] == INVAL)
      invalid = TRUE;
   if (in_ddr[i].valid[DDDCV] != VALID)
      valid = FALSE;
   if (in_ddr[i].valid[DDDCV] != UNKNOW)
      unknown = FALSE;
   }

if (invalid)  /* invalidate the output datum code */
   {
   out_ddr->datum_code = 0;
   out_ddr->valid[DDDCV] = INVAL;
   } /* if invalid */

else if (valid)  /* Compare the input DDR datum code values  */
   {
   datum_flag = VALID;
   for (i = 0; (i < nimg - 1) && (datum_flag != INVAL); i++)
      {
      if (in_ddr[i].datum_code != in_ddr[i + 1].datum_code)
         {
	 datum_flag = INVAL;
 	 out_ddr->datum_code = 0;
	 out_ddr->valid[DDDCV] = INVAL;
	 sprintf(errtxt,"Datum values of image %d and %d are not equal",
	 i + 1,i+ 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	 }
      } /* for i < nimg */
   if (datum_flag == VALID)
      {
      out_ddr->datum_code = in_ddr[0].datum_code;
      out_ddr->valid[DDDCV] = VALID;
      }
   } /* else if valid */

else if (unknown)
   {
   datum_flag = USAME;
   for (i = 0; (i < nimg - 1) && (datum_flag != UDIFF); i++)
      {
      if (in_ddr[i].datum_code != in_ddr[i + 1].datum_code)
	 {
	 datum_flag = UDIFF;
	 out_ddr->datum_code = 0;
	 out_ddr->valid[DDDCV] = UNKNOW;
	 sprintf(errtxt,"Datum values of image %d and %d are not equal",
	 i + 1,i + 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	 }
      } /* for i < nimg */
   if (datum_flag == USAME)
      {
      out_ddr->datum_code = in_ddr[0].datum_code;
      out_ddr->valid[DDDCV] = UNKNOW;
      }
   } /* else if unknown */

else
   {  /* unknown and valid datum codes */
   /*  Compare only the valid datum codes
   --------------------------------------*/
   datum_flag = VALID;
   first = TRUE;
   for (i = 0; (i < nimg) && (datum_flag != INVAL); i++)
      {
      if (in_ddr[i].valid[DDDCV] == VALID)
	 {
	 if (first)
	    {
	    first = FALSE;
	    index = i;
	    } /* if first */
	 else
	    {
	    if (in_ddr[index].datum_code != in_ddr[i].datum_code)
	       {
	       out_ddr->datum_code =0;
	       out_ddr->valid[DDDCV] = INVAL;
	       datum_flag = INVAL;
	       sprintf(errtxt,"Datum values of image %d and %d are not equal",
	       index + 1,i + 1);
	       c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	       } /* if !first */
	    } /* else */
	 } /*  if in_ddr[i].valid[DDDCV] == valid */
     } /* for i < nimg */

   /*  Copy the first valid input datum code to the output datum code
   ------------------------------------------------------------------*/
   if (datum_flag == VALID)
      {
      out_ddr->datum_code = in_ddr[index].datum_code;
      out_ddr->valid[DDDCV] = VALID;
      }
   } /* else */

return;
}
