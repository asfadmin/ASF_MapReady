/****************************************************************************
FUNCTION NAME:     modify -- edits points based on maximum residual value
SYNTAX:            modify(residual, tie_data, fit, coef
PARAMETERS:
        NAME:       TYPE:               PURPOSE:
        --------------------------------------------------------
        residual    struct RESIDUAL *   track largest residual errors
        tie_data    struct TPLDATA *    tie-point records
        fit         struct FIT *        fitting parameters
        coef        struct COEF *       coefficients and statistics
DESCRIPTION:  
   If the number of points used in the fit < 3, edit process done, return
   Find which is larger, the maximum x residual or the maximum y residual
   If the maximum of x or y found above is less than the maximum allowed
     residual, the editing process is complete, return SATISFIED
   Scan the tie point records for the point id of the tie point with the
     largest residual in x or y (found above)
   Set the active flag to "INACTIVE"
   Report to the user the point made inactive
   Adjust the degree of fit, if needed
   Return FALSE
 
RETURN VALUE:   SATISFIED if residual error is beneath tolerance,
                FALSE is a tie-point was removed from the list.
 
SPECIAL CONSIDERATIONS:
 
PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  1.0    3/82   R. Wenger     CSC      N/A    (Old name:  TIEPOINTS)
  4.0    5/85   S. Kondal     SAR      20-472 (Old name:  TIEFIT)
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 C conversion
  6.0	 7/94   T. Logan      ASF      Remove LAS & TAE dependencies, 
 				       port to Cray YMP
*****************************************************************************/
#include "2dmap.h"



 
int modify(residual, tie_data, fit, coef)
  struct RESIDUAL *residual; /* Residual errors for each tie point */
  struct TPLDATA *tie_data;  /* Tie Point Location data records    */
  struct FIT *fit;	     /* User entered fitting parameters    */
  struct COEF *coef;	     /* Coefficient & statistics records   */
{
  struct TPLDATA *tmp;      /* Temporary pointer to tie point data records */
  int count;		    /* Loop index */
  char pt_id[20];	    /* Point id of tie point with largest residual */
 
  if (coef->npts_fit <= 4)
    { printf("No more points can be deleted from the model\n"); exit(1); }
  if (fabs(residual->x_max_err) > fabs(residual->y_max_err))
    {
      strcpy(pt_id, residual->x_max_ptid);
      if (fabs(residual->x_max_err) < fit->max_res) return(TRUE);
    }
  else 
    {
      strcpy(pt_id, residual->y_max_ptid);
      if (fabs(residual->y_max_err) < fit->max_res) return(TRUE);
    }
  for (tmp = tie_data, count = 0; count < coef->npts; count++, tmp++)
    if (strcmp(pt_id, tmp->pt_id) == 0)
      {
	if ((tmp->active < 2) || (tmp->active > 9)) coef->npts_fit--;
	tmp->active = 8;
	/* printf("Point excluded:  %s \n",tmp->pt_id); */
	find_max_degree(coef->npts_fit, &(coef->degree), &(fit->degree));
      }
  return(FALSE);
}

/****************************************************************************
NAME:     find_max_degree
 
PURPOSE:  Finds maximum allowable degree for current number of tie points
 
PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  1.0    3/82   R. Wenger     CSC      N/A    (Old name:  TIEPOINTS)
  4.0    5/85   S. Kondal     SAR      20-472 (Old name:  TIEFIT)
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 C conversion
 
ALGORITHM DESCRIPTION:
	If current number of active points is less than 3, max degree = 0
	If current number of active points  > 2 and < 6, max degree = 1
	If current number of active points  > 5 and < 10, max degree = 2
	If current number of active points  > 9 and < 15, max degree = 3
	If current number of active points is greater than 14, max degree = 4
	If degree greater than max degree, degree = max degree
*****************************************************************************/
void find_max_degree(npts, degree, max_degree)
   int npts;		/* Number of tie points used in modeling process */
   int *degree;	/* Current degree of polynomial used */
   int *max_degree;	/* Maximum degree of polynomial allowable */
{
   if (npts < 4) *max_degree = 0;
   if ((npts > 3) && (npts < 6)) *max_degree = 1;
   if ((npts > 5) && (npts < 10)) *max_degree = 2;
   if ((npts > 9) && (npts < 15)) *max_degree = 3;
   if (npts > 14) *max_degree = 4;
   if (*degree > *max_degree) *degree = *max_degree;
   return;
}
