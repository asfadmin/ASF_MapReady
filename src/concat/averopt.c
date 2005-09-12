/*******************************************************************************
NAME:      		   AVEROPT	

PURPOSE:   	To determine which direction the average algorithm should
		run.
 

PROGRAM HISTORY:
PROGRAMMER	  DATE		REASON
----------	  ----		------
T. Mittan	MAR 1992	Allow average to be taken in either direction
P. Denny        MAR 2002        Pretty up the code (still ugly though)

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   AVEROPT must be run under TAE.

PROJECT:        LAS		                

ALGORITHM REFERENCES:     none
*******************************************************************************/
#include "asf.h"
#include "las.h"
#include "concat.h"

void averopt(int *laver,          /* left average array     */
             int out_window[][4], /* window in output image */
             int *limage          /* current image number   */
            )
{
  int i,j,   /* loop counters   */
      sl,    /* starting line   */
      nl;    /* number of lines */

/* check overlap for each image
-----------------------------*/
  for(i = 0; i < *limage; i++)
  {
    if (((out_window[i][SL]) < (out_window[*limage][SL] + 
      	  out_window[*limage][NL])) && ((out_window[i][SL] + out_window[i][NL])
      	  > out_window[*limage][SL])) 
    {
      if ((out_window[i][SS] <= out_window[*limage][SS]) &&
          ((out_window[i][SS] + out_window[i][NS]) <= (out_window[*limage][SS]
          + out_window[*limage][NS])))
      {
      	if (out_window[i][SL] < out_window[*limage][SL])
      	{
      	  sl = 0; 
      	  if ((out_window[i][SL] + out_window[i][NL]) < 
      	      (out_window[*limage][SL] + out_window[*limage][NL]))
      	    nl = out_window[i][SL] + out_window[i][NL] - out_window[*limage][SL];
          else
      	    nl = out_window[*limage][NL];
      	}
      	else
      	{
      	  sl = out_window[i][SL] - out_window[*limage][SL] ;
      	  if ((out_window[i][SL] + out_window[i][NL]) < 
      	      (out_window[*limage][SL] + out_window[*limage][NL]))
      	    nl = out_window[i][NL];
      	  else
      	    nl = out_window[*limage][SL] + out_window[*limage][NL] - out_window[i][SL];
      	}
      	for ( j = sl; j < sl + nl; j++)
      	  laver[j] = FALSE;
      }
      else if((out_window[i][SS] < (out_window[*limage][SS] + out_window[*limage][NS]))
	   && ((out_window[i][SS] + out_window[i][NS]) > (out_window[*limage][SS]
	      	      	      	      	      	      	+ out_window[*limage][NS]))) 
      {
      	if (out_window[i][SL] < out_window[*limage][SL])
      	{
      	  sl = 0; 
      	  if ((out_window[i][SL] + out_window[i][NL]) < 
      	      (out_window[*limage][SL] + out_window[*limage][NL]))
      	    nl = out_window[i][SL] + out_window[i][NL] - out_window[*limage][SL];
      	  else
      	    nl = out_window[*limage][NL];
      	}
      	else
      	{
      	  sl = out_window[i][SL] - out_window[*limage][SL];
      	  if ((out_window[i][SL] + out_window[i][NL]) < 
      	      (out_window[*limage][SL] + out_window[*limage][NL]))
      	    nl = out_window[i][NL];
      	  else
      	    nl = out_window[*limage][SL] + out_window[*limage][NL] - out_window[i][SL];
      	}
      	for (j=sl; j<(sl+nl); j++)
      	  laver[j] = TRUE;
      }
    }
  }
  return;
}
