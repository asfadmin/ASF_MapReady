/*******************************************************************************
NAME			       SET_PPAR

PURPOSE	     Compares the projection parameter fields of two or more input 
	     DDR files

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
					
PROJECT       LAS

ALGORITHM 
   Check the proj_coef validity flags for each input DDR
      Set the flags accordingly
   If one is invalid 
      Set the proj_coef field of the output ddr to be INVAL

   If all are valid, check to see if the values are all equal
      If they are not equal 
	 Make the proj_coef valid flag to be INVAL
	 Place a zero into the proj_coef field
      Else
         Copy the first input value to the output value
         Set the proj_coef valid flag to VALID
      Endif

   else if all the validity flags are unknown, check to see if the values 
	are equal
      If they are not equal 
	 Make the proj_coef valid flag to be UNKNOW
	 Place a zero into the proj_coef field
      Else
         Copy the first input value to the output value
         Set the proj_coef valid flag to UNKNOW
      Endif

   Else the validity flags are both unknown and valid, check only the DDRs with
      valid flags set to valid and see if the values are equal
      If they are not equal 
	 Make the proj_coef valid flag to be INVAL
	 Place a zero into the proj_coef field
      Else
         Copy the first input value to the output value
         Set the proj_coef valid flag to VALID
      Endif

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "las.h"
#include "proj.h"

void FUNCTION set_ppar(const struct DDR in_ddr[],struct DDR *out_ddr,
		int *ppar_flag,int *proj_flag,int nimg)
{ /* set_ppar */

int first;			/* first valid flag			   */
int i;				/* loop counter				   */
int k;				/* loop counter				   */
int index=0;			/* index of first DDR with a valid ppar_flag */
int invalid;			/* invalid flag				   */
int unknown;			/* unknown flag				   */
int valid;			/* valid flag	 		 	   */

/* Set the invalid, valid, and unknown flags
--------------------------------------------*/
invalid = FALSE;
valid = TRUE;
unknown = TRUE;

/* If the projections codes of the input ddrs are invalid the projection
coeficients can not be compared
--------------------------------------------------------------------------*/
if (*proj_flag == INVAL)
   invalid = TRUE;

/*  Check the valid flag of the projections coeficients
-------------------------------------------------------*/
for (i = 0; (i < nimg) && (invalid != TRUE); i++)
   {
   if (in_ddr[i].valid[DDPPV] == INVAL)
      invalid = TRUE;
   if (in_ddr[i].valid[DDPPV] != VALID)
      valid = FALSE;
   if (in_ddr[i].valid[DDPPV] != UNKNOW)
      unknown = FALSE;
   }

if (invalid)
   {
   for (k = 0; k < COEFCT; k++)
      out_ddr->proj_coef[k] = 0.;
   out_ddr->valid[DDPPV] = INVAL;
   } /* if invalid */

else if (valid)
   {
   *ppar_flag = VALID;
   for (i = 0; (i < nimg - 1) && (*ppar_flag != INVAL); i++)
      {
      com_ppar(in_ddr[i],in_ddr[i + 1],ppar_flag,INVAL,i + 1,i + 2);
      } /* for i < nimg */

   if (*ppar_flag == INVAL)
      {
      for (k = 0; k < COEFCT; k++)
         out_ddr->proj_coef[k] = 0;
      out_ddr->valid[DDPPV] = INVAL;
      } /* if INVAL */

   if (*ppar_flag == VALID)
      {
      for (k = 0; k < COEFCT; k++)
         out_ddr->proj_coef[k] = in_ddr[0].proj_coef[k];
      out_ddr->valid[DDPPV] = VALID;
      } /*  if VALID */
   } /* else if valid */

else if (unknown)
   {
   *ppar_flag = USAME;
   for (i = 0; (i < nimg - 1) && (*ppar_flag != UDIFF); i++)
      {
      com_ppar(in_ddr[i],in_ddr[i + 1],ppar_flag,UDIFF,i + 1,i + 2);
      } /* for i < nimg */

   if (*ppar_flag == UDIFF)
      {
      for (k = 0; k < COEFCT; k++)
      out_ddr->proj_coef[k] = 0;
      out_ddr->valid[DDPPV] = UNKNOW;
      }

   if (*ppar_flag == USAME)
      {
      for (k = 0; k < COEFCT; k++)
         out_ddr->proj_coef[k] = in_ddr[0].proj_coef[k];
      out_ddr->valid[DDPPV] = UNKNOW;
      }
   } /* else if unknown */

else
   {
   *ppar_flag = VALID;
   first = TRUE;
   for (i = 0; (i < nimg) && (*ppar_flag != INVAL); i++)
      {
      if (in_ddr[i].valid[DDPPV] == VALID)
	 {
	 if (first)
	    {
	    first = FALSE;
	    index = i;
	    } /* if first */
	 else
	    {
            com_ppar(in_ddr[i],in_ddr[index],ppar_flag,INVAL,index + 1,i + 1);
	    } /* else */
	 } /*  if in_ddr[i].valid[DDPPV] == valid */
      } /* for i < nimg */

   if (*ppar_flag == INVAL)
      {
      for (k = 0; k < COEFCT; k++)
         out_ddr->proj_coef[k] =0;
      out_ddr->valid[DDPPV] = INVAL;
      }

   if (*ppar_flag == VALID)
      {
      for (k = 0; k < COEFCT; k++)
         out_ddr->proj_coef[k] = in_ddr[index].proj_coef[k];
      out_ddr->valid[DDPPV] = VALID;
      }
   } /* else */

return;
} /* set_ppar */
