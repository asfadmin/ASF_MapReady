/*******************************************************************************
NAME			       SET_ZONE

PURPOSE	     Compares the zone code fields of two or more input DDR files

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
   Check the zone_code validity flags for each input DDR
      Set the flags accordingly
   If one is invalid 
      Set the zone_code field of the output ddr to be INVAL

   If all are valid, check to see if the values are all equal
      If they are not equal 
	 Make the zone_code valid flag to be INVAL
	 Place a zero into the zone_code field
      Else
         Copy the first input value to the output value
         Set the zone_code valid flag to VALID
      Endif

   else if all the validity flags are unknown, check to see if the values are 	     equal
      If they are not equal 
	 Make the zone_code valid flag to be UNKNOW
	 Place a zero into the zone_code field
      Else
         Copy the first input value to the output value
         Set the zone_code valid flag to UNKNOW
      Endif

   Else the validity flags are both unknown and valid, check only the DDRs with
      valid flags set to valid and see if the values are equal
      If they are not equal 
	 Make the zone_code valid flag to be INVAL
	 Place a zero into the zone_code field
      Else
         Copy the first input value to the output value
         Set the zone_code valid flag to VALID
      Endif

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "las.h"

void FUNCTION set_zone(const struct DDR in_ddr[],struct DDR *out_ddr,int *zone_flag,int nimg)

{ /* set_zone */

int first;			/*  First valid flag			   */
int i;				/*  loop counter			   */
int index=0;			/*  index of first DDR with a valid zone code*/
int invalid;			/*  invalid flag			   */
int unknown;			/*  unknown flag			   */
int valid;			/*  valid flag				   */

char errtxt[ERRLEN + 1];	/*  buffer for the error message	   */

/*  Set the valid, invalid, and unknown flags
---------------------------------------------*/
invalid = FALSE;
valid = TRUE;
unknown = TRUE;

for (i = 0; (i < nimg) && (invalid != TRUE); i++)
   {
   if (in_ddr[i].valid[DDZCV] == INVAL)
      invalid = TRUE;
   if (in_ddr[i].valid[DDZCV] != VALID)
      valid = FALSE;
   if (in_ddr[i].valid[DDZCV] != UNKNOW)
      unknown = FALSE;
   }

if (invalid)	/*  invalidate the output DDR zone code	*/
   {
   out_ddr->zone_code = 0;
   out_ddr->valid[DDZCV] = INVAL;
   *zone_flag = INVAL;
   } /* if invalid */

else if (valid)
   {
   *zone_flag = VALID;
   for (i = 0; (i < nimg - 1) && (*zone_flag != INVAL); i++)
      {
      if (in_ddr[i].zone_code != in_ddr[i + 1].zone_code)
         {
	 *zone_flag = INVAL;
 	 out_ddr->zone_code = 0;
	 out_ddr->valid[DDZCV] = INVAL;
	 sprintf(errtxt,"Zone values of image %d and %d are not equal",
	 i + 1,i + 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	 }
      } /* for i < nimg */

   if (*zone_flag == VALID)
      {
      out_ddr->zone_code = in_ddr[0].zone_code;
      out_ddr->valid[DDZCV] = VALID;
      }
   } 

else if (unknown)

   {
   *zone_flag = USAME;
   for (i = 0; (i < nimg - 1) && (*zone_flag != UDIFF); i++)
      {
      if (in_ddr[i].zone_code != in_ddr[i + 1].zone_code)
	 {
	 *zone_flag = UDIFF;
	 out_ddr->zone_code = 0;
	 out_ddr->valid[DDZCV] = UNKNOW;
	 sprintf(errtxt,"Zone values of image %d and %d are not equal",
	 i + 1,i + 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	 }
      } /* for i < nimg */

   if (*zone_flag == USAME)
      {
      out_ddr->zone_code = in_ddr[0].zone_code;
      out_ddr->valid[DDZCV] = UNKNOW;
      }
   } /* else if unknown */

else
   {
   *zone_flag = VALID;
   first = TRUE;
   for (i = 0; (i < nimg) && (*zone_flag != INVAL); i++)
      {
      if (in_ddr[i].valid[DDZCV] == VALID)
	 {
	 if (first)
	    {
	    first = FALSE;
	    index = i;
	    } /* if first */
	 else
	    {
	    if (in_ddr[index].zone_code != in_ddr[i].zone_code)
	       {
	       out_ddr->zone_code =0;
	       out_ddr->valid[DDZCV] = INVAL;
	       *zone_flag = INVAL;
	       sprintf(errtxt,
	       "Zone values of image %d and %d are not equal",index + 1,i + 1);
	       c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	       } /* if !first */
	    } /* else */
	 } /*  if in_ddr[i].valid[DDZCV] == valid */
     } /* for i < nimg */

   if (*zone_flag == VALID)
      {
      out_ddr->zone_code = in_ddr[index].zone_code;
      out_ddr->valid[DDZCV] = VALID;
      }
   } /* else */

return;
}
