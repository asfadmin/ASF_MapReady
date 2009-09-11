#ifndef _FFT2D_H_
#define _FFT2D_H_
/*
fft2d.h:
	This file contains an external interface to asf_fft.a.
It contains the 2D/3D FFT routines.  See also fft.h
*/
/*******************************************************************
	This file extends the fftlib with 2d and 3d complex fft's and
	2d real fft's.  All fft's return results in-place.  Temporary buffers
	for transposing columns are maintained privately via calls to
	fft2dInit, fft2dFree, fft3dInit, and fft3dFree.
	Note that you can call fft2dInit and fft3dInit repeatedly
	with the same sizes, the extra calls will be ignored.
	So, you could make a macro to call fft2dInit every time you call fft2d.
	*** Warning *** fft2dFree and fft3dFree also call fftFree
	so you must re-init all 1d fft sizes you are going to continue using
*******************************************************************/
int fft2dInit(int M2, int M);
	/* init for fft2d, ifft2d, rfft2d, and rifft2d*/
	/* malloc storage for columns of 2d ffts then call fftinit for both row and column ffts sizes*/
/* INPUTS */
/* M2 = log2 of number of rows */
/* M = log2 of number of columns */
/*       of 2d matrix to be fourier transformed */
/* OUTPUTS */
/* private storage for columns of 2d ffts	*/
/* calls fftInit for cosine and bit reversed tables	*/

void fft2dFree(void);
/* free storage for columns of all 2d&3d ffts and call fftFree to free all BRLow and Utbl storage*/

void fft2d(float *data, int M2, int M);
/* Compute 2D complex fft and return results in-place	*/
/* INPUTS */
/* *data = input data array	*/
/* M2 = log2 of fft size number of rows */
/* M = log2 of fft size number of columns */
/* OUTPUTS */
/* *data = output data array	*/

void ifft2d(float *data, int M2, int M);
/* Compute 2D complex ifft and return results in-place	*/
/* INPUTS */
/* *data = input data array	*/
/* M2 = log2 of fft size number of rows */
/* M = log2 of fft size number of columns */
/* OUTPUTS */
/* *data = output data array	*/

int fft3dInit(int L, int M2, int M);
	/* init for fft3d, ifft3d*/
	/* malloc storage for 4 columns and 4 pages of 3d ffts*/
	/* then call fftinit for page, row and column ffts sizes*/
/* M = log2 of number of columns */
/* M2 = log2 of number of rows */
/* L = log2 of number of pages */
/*       of 3d matrix to be fourier transformed */
/* OUTPUTS */
/* private storage for columns and pages of 3d ffts	*/
/* calls fftInit for cosine and bit reversed tables	*/

void fft3dFree(void);
/* free storage for columns of all 2d&3d ffts and call fftFree to free all BRLow and Utbl storage*/

void fft3d(float *data, int M3, int M2, int M);
/* Compute 2D complex fft and return results in-place	*/
/* INPUTS */
/* *data = input data array	*/
/* M3 = log2 of fft size number of pages */
/* M2 = log2 of fft size number of rows */
/* M = log2 of fft size number of columns */
/* OUTPUTS */
/* *data = output data array	*/

void ifft3d(float *data, int M3, int M2, int M);
/* Compute 2D complex ifft and return results in-place	*/
/* INPUTS */
/* *data = input data array	*/
/* M3 = log2 of fft size number of pages */
/* M2 = log2 of fft size number of rows */
/* M = log2 of fft size number of columns */
/* OUTPUTS */
/* *data = output data array	*/

void rfft2d(float *data, int M2, int M);
/* Compute 2D real fft and return results in-place	*/
/* First performs real fft on rows using size from M to compute positive frequencies */
/* then performs transform on columns using size from M2 to compute wavenumbers */
/* If you think of the result as a complex pow(2,M2) by pow(2,M-1) matrix */
/* then the first column contains the positive wavenumber spectra of DC frequency */
/* followed by the positive wavenumber spectra of the nyquest frequency */
/* since these are two positive wavenumber spectra the first complex value */
/* of each is really the real values for the zero and nyquest wavenumber packed together */
/* All other columns contain the positive and negative wavenumber spectra of a positive frequency */
/* See rspect2dprod for multiplying two of these spectra together- ex. for fast convolution */
/* INPUTS */
/* *data = input data array	*/
/* M2 = log2 of fft size number of rows in */
/* M = log2 of fft size number of columns in */
/* OUTPUTS */
/* *data = output data array	*/

void rifft2d(float *data, int M2, int M);
/* Compute 2D real ifft and return results in-place	*/
/* The input must be in the order as outout from rfft2d */
/* INPUTS */
/* *data = input data array	*/
/* M2 = log2 of fft size number of rows out */
/* M = log2 of fft size number of columns out */
/* OUTPUTS */
/* *data = output data array	*/

void rspect2dprod(float *data1, float *data2, float *outdata, int N2, int N1);
/* When multiplying a pair of 2d spectra from rfft2d care must be taken to multiply the*/
/* four real values seperately from the complex ones. This routine does it correctly.*/
/* the result can be stored in-place over one of the inputs*/
/* *data1 = input data array	first spectra */
/* *data2 = input data array	second spectra */
/* N2 = fft size number of rows into rfft2d for both data1 and data2 */
/* N1 = fft size number of columns into rfft2d for both data1 and data2 */


#endif
