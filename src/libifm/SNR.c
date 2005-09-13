/****************************************************************
FUNCTION NAME:  SNR

SYNTAX: void SNR(v,n,x,y,d,snr);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    v		float *		2D array
    n		int 		dimension of array
    x		int		peak x
    y		int		peak y
    d		int 		exclusion box size

DESCRIPTION:
 void SNR() determines the signal to noise ratio in a square
 array of type FLOAT.  The signal is given by a coordinate
 (x, y) and a box centered on that coordinate of size 'd'.  
 The noise is everything outside that box.

RETURN VALUE:
 The SNR value for the input array.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - Rob Fatland & Mike Shindle - original development
****************************************************************/
#include "asf.h"

#include "ifm.h"

float SNR(float *v, int n, int x, int y, int d)
{
  int    i, j, k, l; 
  int    area, radius, total1, total2;
  bool   *ok;
  float amp, pkPwrNorm, nsPwrNorm, sum1, sum2;
  float snr;

  /* if d is not odd, add one to it */
  if (!(d%2)) d++;
  /* init other variables */
  radius = d/2;
  area   = n*n;
  if (d >= n) Exit("SNR():  signal region too large");

  /* create a TRUE boolean array (TRUE = noise) 'ok' */
  i = sizeof(bool)*n*n;
  ok = (bool *)(MALLOC(i));
  for (i = 0; i < area; i++) *(ok + i) = TRUE; 

  /* turn off the 'ok' elements near the peak */
  for (i = x-radius; i <= x+radius; i++)
    for (j = y-radius; j <= y+radius; j++)
      if (i >= 0 && i <= n-1 && j >= 0 && j <= n-1 ) *(ok + n*j + i) = FALSE;

  /* accumulate both signal and noise sum powers */
  for (i = 0, total1 = 0, sum1 = 0.0, total2 = 0, sum2 = 0.0; i < area; i++) {
    amp  =  fabs(*(v+i));
    if (*(ok+i)) { sum2 += amp*amp; total2++; }  /* noise */
    else { sum1 += amp*amp; total1++; }  /* peak  */
  }

  if ((total2 + total1 != area))
    Exit("SNR(): inconsistent total integration");
  
  if (total1 == 0 || total2 == 0 || sum2 == 0.0)
    Exit("SNR(): bad noise power sum or integration region");

  pkPwrNorm = sum1/(double)(total1);
  nsPwrNorm = sum2/(double)(total2);
  if (nsPwrNorm == 0.0) {
    Alert("SNR got nsPwrNorm == 0.0 (zero noise power)"); snr = 0.0; }
  else if (pkPwrNorm == 0.0) {
    Alert("SNR got pkPwrNorm == 0.0 (zero peak power)"); snr = 0.0; }
  else if (pkPwrNorm/nsPwrNorm <= 0.0) {
    Alert("SNR got pkPwrNorm/nsPwrNorm <= 0.0"); snr = 0.0; }
  else snr = 10.0*log10(pkPwrNorm/nsPwrNorm);
  
  free(ok);
  return(snr);
}
