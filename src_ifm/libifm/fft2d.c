/****************************************************************
FUNCTION NAME: fft2d

SYNTAX:  void fft2d(array,n,direction);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    array       Complex *       pointer to array of signal data to perform
				two dimensional FFT.
    n           int             number of values in array
    direction   int             flag to be passed to fourn().

DESCRIPTION:
	Gets the 2D FFT of the array "array" by calling the routine fourn().
	This routine acts as a buffer to fourn(), which has confusing index
	conventions and calling variables. This routine will copy 'array'
	into a strict double variable called 'myV'. This is for fourn() which
	requires a double instead of Complex. Fourn() was obtained from
	Numerical Recipes.

RETURN VALUE:
	None.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
	1.0 - Mike Shindle & Rob Fatland - Original Development.
****************************************************************/

#include "ifm.h"

void fft2d (FComplex *array, int n, int direction)
{
    int   i; 
    int   size;
    int   nn[3];
    float norm;
    float *myV;

    /* check to make sure n is nozero */ 
    if (n == 0)
      Exit("fft2d():  bad size/norm(0)");
    /* assign parameters */ 
    norm = (float)(n);
    size = n*n;

    /* set up my copy of the passed vector */
    myV  = (float *)(MALLOC((size + 1)*sizeof(FComplex)));
    
    /* assign values to myV */
    *myV = 0.0;
    for (i = 0; i < size; i++){
      *(myV + 2*i + 1)     = (array+i)->real;
      *(myV + 2*i + 1 + 1) = (array+i)->imag;
    }
    nn[0] = 0; 
    nn[1] = n;  
    nn[2] = n; 

    /* preform two dimensional fourier transform */
    fourn(myV, nn, 2, direction);

    /* get the values back into array */
    for (i = 0; i < size; i++){
      (array+i)->real = *(myV + 2*i + 1);
      (array+i)->imag = *(myV + 2*i + 1 + 1);
    }
    free(myV);

    /* normalize */
    for (i = 0; i < size; i++) 
      *(array+i) = Csdiv(norm, *(array+i));

    return;
}
