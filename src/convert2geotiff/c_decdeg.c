/*****************************************************************************
NAME: DECDEG

SYNTAX: c_decdeg(angle, coform, type)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    angle      double *        Input and output of degrees     
    coform     char *          Type of angle being inputted    
    type       char *          Limits output angle is tested against

RETURN VALUE:  integer (either a 1 or 0 for sucess of failure)

DESCRIPTION: To convert angles to total degrees

PROGRAM HISTORY:
VERSION:   PROGRAMMER:      DATE:    REASON:
----------------------------------------------------------------------------
  0.0	D.Steinwand             Original development
  0.2	B.Ailts         9/87    Convert to C and add
	                        UNIX bridge routines
  0.4	B. Ailts        12/87   Place bridge routines in a seperate file
	                        replace DESC arguments with char. arrays
	                        change include file directory specifications
  0.6	B. Ailts        05/87   Changed return status to E_SUCC and E_FAIL
	                        Changed newlas.h to worgen.h
	                        Standardized error messages
  0.8	D. Etrheim      07/90   Standardized error message handling
  1.0   S. Watts        8/01    Removed TAE dependencies.
  
COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
    
ALGORITHM:	
    Recieve an anlge in seconds, minutes, or DMS
    Convert it to total degrees
    The angle is then checked to be sure it is within the limits of 
      its use(LAT, LON, or DEGREES)

ALGORITHM REFERENCES: None
******************************************************************************/

#include "ddr.h"


#define E_SUCC 0
#define E_FAIL -1


int c_decdeg
(
    double *angle,                /* Input and output of degrees            */
    char *coform,                 /* Type of angle being inputted           */
    char *type                   /* Limits output angle is tested against  */
)

{
  double in_coord;              /* Input coordinate value                 */
  float second;                 /* Number of seconds                      */ 
  float upper;                  /* Upper bound limits                     */
  float lower;                  /* Lower bound limits                     */

  long degree;                  /* Degrees in DMS coordinates             */
  long minute;                  /* Number of minutes                      */
  long sign;                    /* Positive or Negative anle              */

  /* Save the input coordinate value
  ----------------------------------*/
  in_coord = *angle;

  /* Get the upper/lower bounds to
     test the output angle against
  --------------------------------*/
  if (strncmp(type,"LAT",3) == 0) 
    {
    upper = 90.0;
    lower = -90.0;
    }
  else if (strncmp(type,"LON",3) == 0)
    {
    upper = 180.0;
    lower = -180.0;
    }
  else
    {
    upper = 360.0;
    lower = 0.0;
    }

  /* Compute the total degrees
     based on the unit type
  ----------------------------*/
  if (strncmp(coform,"MIN",3) == 0)
    *angle = *angle / 60.0;
  else if (strncmp(coform,"SEC",3) == 0)
    *angle = *angle / 3600.0;
  else if (strncmp(coform,"DMS",3) == 0)
    {
    /* Determine the sign of the input angle
    ----------------------------------------*/
    sign = 1;
    if (*angle < 0)
      {
      sign = -1;
      *angle = *angle * -1;
      }

    /* Break up the DMS value
    -------------------------*/
    degree = (long)(*angle / 1000000);
    *angle = *angle - (degree * 1000000);
    minute = (long)(*angle/1000);
    if (minute > 60)
      {
     printf("\nError: %lf - Minutes greater than 60\n",in_coord);
      return(E_FAIL);
      }
    second = *angle - (minute * 1000);
    if (second > 60)
      {
      printf("\nError: %lf - Seconds greater than 60\n",in_coord);
      return(E_FAIL);
      }

    *angle = sign * (degree + (minute/60.0) + (second/3600.0));
    }

  /* Make sure the final angle is within the bounds
  -------------------------------------------------*/
  if ((*angle > upper) || (*angle < lower))
    {
    printf("\nCalcuated coordinate out of bounds\n");
    return(E_FAIL);
    }

  return(E_SUCC);
}
