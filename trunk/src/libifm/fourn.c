/****************************************************************
FUNCTION NAME: fourn

SYNTAX: void fourn(data,nn,ndim,isign);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    data        float *         array to preform FFT on
    nn          int *           array containing the lengths of each
				        dimension
    ndim        int             number of dimensions. 
    isign       int             flag to determine use of inverse transform

DESCRIPTION:
	Replaces data by its ndim-dimensional discrete Fourier transform, if
	isign is input as 1. nn[1..ndim] is an integer array containing the
	lengths (number of complex values) of each dimension (the 0 position
	is not used) which must all be powers of 2. data is a real array of
	length twice the product of these lengths, in which the data are
	stored as in a multidimensional complex array: real and imaginary
	parts of each element are in consecutive locations, and the rightmost
	index of the array increases most rapidly as one proceeds along data.
	For a 2d array, this is equivalent to storing the array by rows. If
	isign is input as -1, data is replaced by its inverse transform times
	the product of the lengths of all dimensions.

RETURN VALUE:
        None. 

SPECIAL CONSIDERATIONS:
        Check Numerical Recipes in C for more information.

PROGRAM HISTORY:
        1.0 - Mike Shindle - original porting
****************************************************************/

#include "ifm.h"

void fourn(float *data, int *nn, int ndim, int isign)
{
  int   i1, i2, i3;
  int   i2rev,i3rev;
  int   ip1, ip2, ip3;
  int   ifp1, ifp2;
  int   ibit, idim;
  int   k1, k2;
  int   n, nprev, nrem, ntot;
  float tempi, tempr;
  float theta, wi, wpi, wpr, wr, wtemp;
  float pi;

  pi = 4.0 * atan(1.0);
  ntot=1;

  /* Compute total number of complex values. */
  for (idim = 1; idim <= ndim; idim++) 
     ntot *= nn[idim];
  nprev=1;

  /* Main loop over the dimensions */
  /* printf("fourn():\n"); */
  for (idim = ndim; idim >= 1; idim--) {
     n = nn[idim];
     /* printf("\tdimension = %d  n = %d\n", idim, n); */
     nrem = ntot / (n*nprev);
     ip1 = nprev << 1;
     ip2 = ip1*n;
     ip3 = ip2*nrem;
     i2rev = 1;

     /* this is the bit reversal section of the routine */
     for(i2 = 1; i2 <= ip2; i2 += ip1) {
        if (i2 < i2rev) {
          for(i1 = i2; i1 <= i2 + ip1 - 2; i1 += 2) {
            for (i3 = i1; i3 <= ip3; i3 += ip2) {
               i3rev = i2rev + i3 - i2;
               /* swap variables */ 
	         tempr = data[i3];
               data[i3]  = data[i3rev];
               data[i3rev] = tempr;
               
               tempr = data[i3+1];
               data[i3+1] = data[i3rev+1];
               data[i3rev+1] = tempr;

            }
          }
        }

        ibit = ip2 >> 1;
        while (ibit >= ip1 && i2rev > ibit) {
           i2rev -= ibit;
           ibit = ibit >> 1;
        }
        i2rev += ibit;
     }
     ifp1 = ip1;
     
     /* Here begins the Danielson-Lanczos section of the routine */
     while (ifp1 < ip2) {
        ifp2  = ifp1 << 1;
        theta = isign* 2.0 * pi /(ifp2/ip1);
        wtemp = sin(0.5*theta);
        wpr = -2.0*wtemp*wtemp;
        wpi = sin(theta);
        wr = 1.0;
        wi = 0.0;
        for (i3 = 1; i3 <= ifp1; i3 += ip1) {
           for (i1 = i3; i1 <= i3 + ip1 - 2; i1 += 2) {
              for (i2 = i1; i2 <= ip3; i2 += ifp2) {
                 k1 = i2;
                 k2 = k1 + ifp1;
                 tempr = wr * data[k2] - wi * data[k2+1];
                 tempi = wr * data[k2+1] + wi * data[k2];
                 data[k2] = data[k1] - tempr;
                 data[k2+1] = data[k1+1] - tempi;
                 data[k1] += tempr;
                 data[k1+1] += tempi;
              }
           }
           wtemp = wr;
           wr = wr * wpr - wi * wpi + wr;
           wi = wi * wpr + wtemp * wpi + wi;
        }
        ifp1 = ifp2;
     }
     nprev *= n;
  }
  return;
}
