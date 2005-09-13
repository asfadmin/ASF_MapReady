/*******************************************************************************
NAME			      check_dms

PURPOSE	     Check packed DMS value entered--is it valid?

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
D. Steinwand		Sept, 1988	Original Development
					

PROJECT     LAS

ALGORITHM 
	Unpack the angle into separate DEG, MIN, SEC parts
	Check the range of each part, return if bad with E_FAIL
	return E_SUCC

ALGORITHM REFERENCES

*******************************************************************************/
#include "worgen.h"

FUNCTION int check_dms(angle)

double angle;		/* Angle value in DDDMMMSSS.SS */

{
int deg, min;		/* Degrees & minutes portion of angle */
float sec;		/* Seconds portion of angle */
int sign;		/* Sign of angle */


/* extract the degress, minutes, and seconds portions of angle
  -----------------------------------------------------------*/
if (angle < 0)
   sign = -1;
else
   sign = 1;
angle *= sign;
deg = angle / 1000000;
angle -= (deg * 1000000);
min = angle/1000;
sec = angle - (min * 1000);

if(deg > 180) return(E_FAIL);
if(min > 60) return(E_FAIL);
if(sec > 60.0) return(E_FAIL);

return(E_SUCC);
}
