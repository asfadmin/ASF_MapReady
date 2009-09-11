/*******************************************************************************
NAME			       SET_PDIS_FLAG

PURPOSE	     Compares the projection distance fields of two or more input 
	     DDR files

EXTERNAL ASSOCIATIONS
	This routine is called by C_UPDDR routine
	This routine is used in conjunction with SET_PDIST

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
B. Ailts	      Jan. 1988 	original development
B. Ailts	      Apr. 1988		Replace newlas.h with las.h
					Changed the calling sequence -- only
					  one structure
					Update to use current constants
					Added a limit on precision of the
					comparisions
B. Ailts	      Dec. 1990 	Updated error messages
					
PROJECT       LAS

ALGORITHM 
   Check the projection distance validity flags for each input DDR
      Set the flags accordingly
   If of the projection distance valid flags is invalid 
      Set the projection distance flag argument to be INVAL

   If all are valid, check to see if the values are all equal within a precision
   of PREC
      If they are not equal 
	 Make the projection distance flag argument to be INVAL
      Else
         Set the projection distance flag argument to VALID
      Endif

   else if all the validity flags are unknown, check to see if the values are
    equal
      If they are not equal 
	 Make the projection distance flag argument to be UDIFF
      Else
         Set the projection distance flag argument to USAME
      Endif

   Else the validity flags are both unknown and valid, check only the DDRs with
      valid flags set to valid and see if the values are equal
      If they are not equal 
	 Make the projection distance flag argument to be INVAL
      Else
  	 Set the projection distance index argument to the number of the first 
          valid value
         Set the projection distance flag arguement to VALID
      Endif

ALGORITHM REFERENCES	none
*******************************************************************************/


#include "las.h"

/*  Below are the constansts used to designate the presicion that the
pdist_x and pdist_x values of each input DDR will be compared.  These
values are based on the reccomendations of a user representative that believed
that projection distance of a pixel need not be more accuate that two places
to the right of the decimal point.
-----------------------------------------------------------------------------*/
#define PREC 100.  /*  The precision of comparisions of the projection 
		       distance values*/
/*
#ifdef gould
#define GPREC .000000001
#endif
*/

void FUNCTION set_pdis_flag(const struct DDR ddr[],int *pdist_flag,int *pdist_index,int nimg)
{ /* set_pdist_flag */

int first;			/*  first valid flag			   */
int i;				/*  loop counter			   */
int invalid;			/*  invalid flag			   */
int unknown;			/*  unknown flag			   */
int valid;			/*  valid flag				   */

char errtxt[ERRLEN + 1];	/*  buffer for error message		   */

/*  Set the invalid, valid, and unknown flags
---------------------------------------------*/
invalid = FALSE;
valid = TRUE;
unknown = TRUE;

for (i = 0; (i < nimg) && (invalid != TRUE); i++)
   {
   if (ddr[i].valid[DDPDV] == INVAL)
      invalid = TRUE;
   if (ddr[i].valid[DDPDV] != VALID)
      valid = FALSE;
   if (ddr[i].valid[DDPDV] != UNKNOW)
      unknown = FALSE;
   }

if (invalid)
   {
   *pdist_flag = INVAL;
   *pdist_index = 0;
   }

else if (valid)
   {
   *pdist_flag = VALID;
   *pdist_index = 0;
   for (i = 0; (i < nimg - 1) && (*pdist_flag != INVAL); i++)
      {

/*  Compare each input DDR pdist_y and pdist_x to the precision set by the
constant PREC.  GPREC is added for the gould because of the truncation
that the gould does on floating point numbers.  The VAX and Suns round.
----------------------------------------------------------------------------*/
/*
#ifdef gould
      if ((abs(floor((ddr[i].pdist_x * PREC) + GPREC) - 
	       floor((ddr[i + 1].pdist_x * PREC) + GPREC)) >= 1) ||
          (abs(floor((ddr[i].pdist_y * PREC) + GPREC) - 
	       floor((ddr[i + 1].pdist_y * PREC) + GPREC)) >= 1))
#else
*/
      if ((fabs(floor(ddr[i].pdist_x * PREC) - 
	       floor(ddr[i + 1].pdist_x * PREC)) >= 1) ||
          (fabs(floor(ddr[i].pdist_y * PREC) - 
	       floor(ddr[i + 1].pdist_y * PREC)) >= 1))
/*
#endif
*/
	 {
	 *pdist_flag = INVAL;
	 sprintf(errtxt,
	 "Projection distance values of image %d and %d are not equal",
	 i + 1,i + 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	 }
      } /* for i < nimg */
   } /* else if valid */

else if (unknown)
   {
   *pdist_flag = USAME;
   *pdist_index = 0;
   for (i = 0; (i < nimg - 1) && (*pdist_flag != UDIFF); i++)
      {

/*  Compare each input DDR pdist_y and pdist_x to the precision set by the
constant PREC.  GPREC is added for the gould because of the truncation
that the gould does on floating point numbers.  The VAX and Suns round.
----------------------------------------------------------------------------*/
/*
#ifdef gould
      if ((abs(floor((ddr[i].pdist_x * PREC) + GPREC) - 
	       floor((ddr[i + 1].pdist_x * PREC) + GPREC)) >= 1) ||
          (abs(floor((ddr[i].pdist_y * PREC) + GPREC) - 
	       floor((ddr[i + 1].pdist_y * PREC) + GPREC)) >= 1))
#else
*/
      if ((fabs(floor(ddr[i].pdist_x * PREC) - 
	       floor(ddr[i + 1].pdist_x * PREC)) >= 1) ||
          (fabs(floor(ddr[i].pdist_y * PREC) - 
	       floor(ddr[i + 1].pdist_y * PREC)) >= 1))
/*
#endif
*/
	 {
	 *pdist_flag = UDIFF;
	 sprintf(errtxt,
	 "Projection distance values of image %d and %d are not equal",
	 i + 1,i + 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	 }
      } /* for i < nimg */
   } /* else if unknown */

else
   { /* combination of valid and unknown */
   *pdist_flag = VALID;
   first = TRUE;
   for (i = 0; (i < nimg) && (*pdist_flag != INVAL); i++)
      {
      if (ddr[i].valid[DDPDV] == VALID)
	 {
	 if (first)
	    {
	    first = FALSE;
	    *pdist_index = i;
	    } /* if first */
	 else
	    {

/*  Compare each input DDR pdist_y and pdist_x to the precision set by the
constant PREC.  GPREC is added for the gould because of the truncation
that the gould does on floating point numbers.  The VAX and Suns round.
----------------------------------------------------------------------------*/
/*
#ifdef gould
            if ((abs(floor((ddr[i].pdist_x * PREC) + GPREC) - 
	             floor((ddr[*pdist_index].pdist_x * PREC) + GPREC)) >= 1) ||
                (abs(floor((ddr[i].pdist_y * PREC) + GPREC) - 
	             floor((ddr[*pdist_index].pdist_y * PREC) + GPREC)) >= 1))
#else
*/
            if ((fabs(floor(ddr[i].pdist_x * PREC) - 
	             floor(ddr[*pdist_index].pdist_x * PREC)) >= 1) ||
                (fabs(floor(ddr[i].pdist_y * PREC) - 
	             floor(ddr[*pdist_index].pdist_y * PREC)) >= 1))
/*
#endif
*/
		{
	        *pdist_flag = INVAL;
	 	sprintf(errtxt,
	 	"Projection distance values of image %d and %d are not equal",
		*pdist_index + 1,i + 1);
	 	c_errmsg(errtxt,"upddr-equal",NON_FATAL);
	 	}
	    } /* else not first */
	 } /*  if ddr[i].valid[DDPDV] == valid */
      } /* for i < nimg */
   } /* else */

return;
} /* set_pdis_flag */
