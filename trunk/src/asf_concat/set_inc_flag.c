/*******************************************************************************
NAME			       SET_INC_FLAG

PURPOSE	     Compares the increment fields of two or more input DDR files

EXTERNAL ASSOCIATIONS
	This routine is called by C_UPDDR routine
	This routine is used in conjunction with SET_INCREMENT

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
B. Ailts	      Dec. 1990		Updated error messages
					
PROJECT       LAS

ALGORITHM 
   Check the increment validity flags for each input DDR
      Set the flags accordingly
   If of the increment valid flags is invalid 
      Set the increment flag argument to be INVAL

   If all are valid, check to see if the values are all equal within a precision
   of PREC
      If they are not equal 
	 Make the increment flag argument to be INVAL
      Else
         Set the increment flag argument to VALID
      Endif

   else if all the validity flags are unknown, check to see if the values are
   equal
      If they are not equal 
	 Make the increment flag argument to be UDIFF
      Else
         Set the increment flag argument to USAME
      Endif

   Else the validity flags are both unknown and valid, check only the DDRs with
      valid flags set to valid and see if the values are equal
      If they are not equal 
	 Make the increment flag argument to be INVAL
      Else
  	 Set the increment index argument to the number of the first valid value
         Set the increment flag arguement to VALID
      Endif

ALGORITHM REFERENCES	none
*******************************************************************************/

#include "las.h"

/*  Below are the constansts used to designate the presicion that the
line_inc and sample_inc values of each input DDR will be compared.  These
values are based on two assumptions.
  1.  double precision numbers have 15 valid digits
  2.  No line_inc or sample_inc will use more than 3 digits to the left of
      the decimal point.
-----------------------------------------------------------------------------*/
#define PREC 1000000000000.  /*  This is the precision of the comparisions    */
/*
#ifdef gould
#define GPREC .001
#endif
*/

void FUNCTION set_inc_flag(const struct DDR ddr[],int *inc_flag,int *inc_index,int nimg)

{ /* set_inc_flag */

int first;			/*  Flag of first valid increment flag    */
int i;				/*  loop counter			  */
int invalid;			/*  invalid flag			  */
int unknown;			/*  unknown flag			  */
int valid;			/*  valid flag				  */

char errtxt[ERRLEN + 1];	/* buffer for error message		  */

/*  Set the invalid, unknown, and valid flags
---------------------------------------------*/
invalid = FALSE;
valid = TRUE;
unknown = TRUE;

for (i = 0; (i < nimg) && (invalid != TRUE); i++)
   {
   if (ddr[i].valid[DDINCV] == INVAL)
      invalid = TRUE;
   if (ddr[i].valid[DDINCV] != VALID)
      valid = FALSE;
   if (ddr[i].valid[DDINCV] != UNKNOW)
      unknown = FALSE;
   }

if (invalid)
   {
   *inc_flag = INVAL;
   *inc_index = 0;
   }

else if (valid)
   {
   *inc_flag = VALID;
   *inc_index = 0;

/*  Compare each input DDR line_inc and samp_inc to the precision set by the
constant PREC.  GPREC is added to the gould because of the truncation
that the gould does on floating point numbers.  The VAX and Suns round.
----------------------------------------------------------------------------*/
   for (i = 0; (i < nimg - 1) && (*inc_flag != INVAL); i++)
      {
/*
#ifdef gould
      if ((abs(floor((ddr[i].line_inc * PREC) + GPREC) - 
              floor((ddr[i + 1].line_inc * PREC) + GPREC)) >= 1) ||
          (abs(floor((ddr[i].sample_inc * PREC) + GPREC) -
	      floor((ddr[i + 1].sample_inc * PREC) + GPREC)) >= 1))
#else
*/
      if ((fabs(floor(ddr[i].line_inc * PREC) - 
              floor(ddr[i + 1].line_inc * PREC)) >= 1) ||
          (fabs(floor(ddr[i].sample_inc * PREC) -
	      floor(ddr[i + 1].sample_inc * PREC)) >= 1))
/*
#endif */
	 {
	 *inc_flag = INVAL;
	 sprintf(errtxt,"Increment values of image %d and %d are not equal",
	 i + 1,i + 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
 	 }
      } /* for i < nimg */
   } /* else if valid */

else if (unknown)
   {
   *inc_flag = USAME;
   *inc_index = 0;

/*  Compare each input DDR line_inc and samp_inc to the precision set by the
constant PREC.  GPREC is added to the gould because of the truncation
that the gould does on floating point numbers.  The VAX and Suns round.
----------------------------------------------------------------------------*/
   for (i = 0; (i < nimg - 1) && (*inc_flag != UDIFF); i++)
      {
/*
#ifdef gould
      if ((abs(floor((ddr[i].line_inc * PREC) + GPREC) - 
              floor((ddr[i + 1].line_inc * PREC) + GPREC)) >= 1) ||
          (abs(floor((ddr[i].sample_inc * PREC) + GPREC) -
	      floor((ddr[i + 1].sample_inc * PREC) + GPREC)) >= 1))
#else
*/
      if ((fabs(floor(ddr[i].line_inc * PREC) - 
              floor(ddr[i + 1].line_inc * PREC)) >= 1) ||
          (fabs(floor(ddr[i].sample_inc * PREC) -
	      floor(ddr[i + 1].sample_inc * PREC)) >= 1))
/*
#endif
*/
         {
	 *inc_flag = UDIFF;
	 sprintf(errtxt,"Increment values of image %d and %d are not equal",
	 i + 1,i + 2);
	 c_errmsg(errtxt,"upddr-equal",NON_FATAL);
 	 }
      } /* for i < nimg */
   } /* else if unknown */

else
   { /* combination of valid and unknown */
   *inc_flag = VALID;
   first = TRUE;

/*  Compare each valid input DDR line_inc and samp_inc to the precision set 
by the constant PREC.  GPREC is added to the gould because of the 
truncation that the gould does on floating point numbers.  The VAX and 
Suns round.
----------------------------------------------------------------------------*/
   for (i = 0; (i < nimg) && (*inc_flag != INVAL); i++)
      {
      if (ddr[i].valid[DDINCV] == VALID)
	 {
	 if (first)
	    {
	    first = FALSE;
	    *inc_index = i;
	    } /* if first */
	 else
	    {
/*
#ifdef gould
            if ((abs(floor((ddr[i].line_inc * PREC) + GPREC) - 
                    floor((ddr[*inc_index].line_inc * PREC) + GPREC)) >= 1) ||
                (abs(floor((ddr[i].sample_inc * PREC) + GPREC) -
	            floor((ddr[*inc_index].sample_inc * PREC) + GPREC)) >= 1))
#else
*/
            if ((fabs(floor(ddr[i].line_inc * PREC) - 
                    floor(ddr[*inc_index].line_inc * PREC)) >= 1) ||
                (fabs(floor(ddr[i].sample_inc * PREC) -
	            floor(ddr[*inc_index].sample_inc * PREC)) >= 1))
/*
#endif
*/
	       {
	       *inc_flag = INVAL;
	       sprintf(errtxt,
	       "Increment values of image %d and %d are not equal",
		*inc_index + 1,i + 1);
	       c_errmsg(errtxt,"upddr-equal",NON_FATAL);
 	       }
	    } /* else not first */
	 } /*  if ddr[i].valid[DDINCV] == valid */
      } /* for i < nimg */
   } /* else */

return;
} /* set_inc_flag */
