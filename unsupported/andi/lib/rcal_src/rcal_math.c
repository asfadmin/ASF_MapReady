/* rcal_math.c:  rob's commonly used math routines library functions */

#include LIBINC

/* dstnc:  euclidean distance between two vectors
     a:  double precision vector 1;
     b:  double precision vector 2;
     n:  number of elements in both vectors;
*/
double dstnc(a, b, n)
double *a, *b;
int    n;
{
  double s;
  int    i;
  for (i = 0, s = 0.0; i < n; i++) 
    s += ((*(a+i))-(*(b+i)))*((*(a+i))-(*(b+i)));
  return sqrt(s);
}

/* standard deviation and mean calculation 
     we got an input double precision vector 'a';
     we got a number of elements 'n';
     we got pointers to double *m and *s;
*/
int std_stat(a, n, m, s)
double *a, *m, *s;
int n;
{
  int i;
  double x, sum;

  if (!n) {
    Alert("std_stat():  0 elements");
    return FALSE;
  }

  /* mean */
  for (i = 0, sum = 0.0; i < n; i++){ sum += *(a+i); }
  *m = sum / (double)(n);

  /* standard deviation */
  for (i = 0, sum = 0.0; i < n; i++){
    x = *(a+i) - (*m);
    sum += x*x;
  }
  *s = sqrt(sum/(double)(n));
  return TRUE;
}


/* standard deviation and mean calculation
     we have the input Short Int Complex vector 'a';
     we have the number of elements 'n';
     we have pointers to double *m and *s;
*/
int cpx_stat(a, n, Im, Is, Qm, Qs, Dm, Ds)
SIComplex *a;
double *Im, *Is, *Qm, *Qs, *Dm, *Ds;
int n;
{
  int i;
  double x, sum1, sum2, sum3;
 
  if (!n) {
    Alert("std_stat():  0 elements");
    return FALSE;
  }
 
  /* means */
  sum1 = sum2 = sum3 = 0.0;
  for (i = 0; i < n; i++) {
    sum1 += a[i].real;
    sum2 += a[i].imag;
    sum3 += sqrt( (double) a[i].real*a[i].real + a[i].imag*a[i].imag );
  }
  *Im = sum1 / (double)(n);
  *Qm = sum2 / (double)(n);
  *Dm = sum3 / (double)(n);
printf("Three means = %2.2f %2.2f %2.2f\n", *Im, *Qm, *Dm);
 
  /* standard deviations */
  sum1 = sum2 = sum3 = 0.0;
  for (i = 0; i < n; i++) {
    x = a[i].real - (*Im);
    sum1 += x*x;
    x = a[i].imag - (*Qm);
    sum2 += x*x;
    x = sqrt( (double) a[i].real*a[i].real + a[i].imag*a[i].imag ) - (*Dm);
    sum3 += x*x;
  }
  *Is = sqrt(sum1/(double)(n));
  *Qs = sqrt(sum2/(double)(n));
  *Ds = sqrt(sum3/(double)(n));
  return TRUE;
}


/* centroid function; takes a listing of all possible values of x to be 
   stored in the array 'a'.  Takes the pdf of x, or in histogram
   terms, the number of occurences of x, to be in the array 'b'.
   The centroid is copied to the pointer 'c'.  The number of 
   entries is given by the integer n.

   Normalization of P(x) is attained by dividing by the sum of the
   contents of the probability array 'b';

   <x> = Sum over all values ( x times P(x) )

*/
int centroid(a, b, c, n)
double *a, *b, *c;
int n;
{
  int i;
  double sum, sum1, tmp;

  for (i = 0, sum = sum1 = 0.0; i < n; i++){ 
    tmp   = *(b+i);
    sum  += tmp;
    sum1 += *(a+i) * tmp; 
  }

  if (sum == 0.0) { 
    Alert ("centroid():  zero denominator");
    return FALSE;
  }
  else {
    *c = sum1 / sum; 
    return TRUE;
  }
}



/* matrix_product performs a generalized matrix multiplication in double precision 

   int matrix_product(a, b, r, n, m, l)
   double a[], b[], r[];
   int n, m, l;

   a * b = r 
      a    : matrix a
      b    : matrix b
      n    : rows of a
      m    : columns of a and rows of b
      l    : columns of b
*/
int matrix_product (a, b, r, n, m, l)
double a[], b[], r[];
int n, m, l;
{
  int i, j, k, ir = 0, ji, ik, ib;
  ik = -m;
  for (k = 0; k < l; k++){
    ik += m;
    for (j = 0; j < n; j++){
      ji = j - n;
      ib = ik;
      r[ir] = 0.0;
      for (i = 0; i < m; i++){
        ji += n;
        r[ir] += a[ji]*b[ib];
        ib++;
      }
      ir++; 
    }
  }
  return TRUE;
}

/* xprod_3dvectors takes a * b into r */
int xprod_3dvectors(a, b, r)
double a[3], b[3], r[3];
{
    r[0] = a[1]*b[2] - a[2]*b[1];
    r[1] = a[2]*b[0] - a[0]*b[2];
    r[2] = a[0]*b[1] - a[1]*b[0];
    return TRUE;
}

/* line_correlate:
   This subroutine determines the correlation coefficient between
   two real arrays of length n.  The correlation is returned as a number
   between -1 and 1 stored in "c"  */
int line_correlate(a, b, n, c)
double *a, *b;
int n;
double *c;
{
  double mean_a,mean_b,tot,tot2,tot3, tmp;
  int i,j;

  if (!n) Exit ("line_correlate():  passed length n = 0");

  /* get mean values  */
  tot = 0.0;
  for (i=0;i<n;i++) tot += *(a+i);
  mean_a = tot/(double)(n);
  tot = 0.0;
  for (i=0;i<n;i++) tot += *(b+i);
  mean_b = tot/(double)(n);

  /* find correlation coefficient */
  tot = tot2 = tot3 = 0.0;
  for (i=0;i<n;i++) tot  += ((*(a+i))-mean_a) * ((*(b+i))-mean_b);
  for (i=0;i<n;i++) tot2 += ((*(a+i))-mean_a) * ((*(a+i))-mean_a);
  for (i=0;i<n;i++) tot3 += ((*(b+i))-mean_b) * ((*(b+i))-mean_b);
  
  if (tot2*tot3 == 0.0) {
    Exit ("line_correlate():  0 denom; halting");
  }

  *c = tot/sqrt(tot2*tot3);

  return TRUE;
}



/* 
 * findoffset():  routine uses correlation to find the best offset between 
 * two rectangular arrays v1 and v2 of dimension x1,y1 and x2,y2 respectively.  
 * It is assumed that x2 < x1 and y2 < y1.  The routine works by zero-padding 
 * v2 until it is the same size as v1, and then using 2D Fourier transform 
 * techniques to find the offset.  For generality, it is assumed that the arrays
 * are complex.
 */


/* 
 * this routine needs to have the subtraction of the mean installed and also
 *   a reasonable calculation of the 'quality' variable 'corr' 
 */

void findoffset (v1, v2, x1, y1, x2, y2, xoff, yoff, corr)
Complex *v1,*v2;
int      x1, y1, x2, y2, *xoff, *yoff;
float    *corr;
{
  Complex  *v1a, *v2a, *v3a, *v4a;
  Complex  c1, c2, c3, c4;
  int      i, j, k, l; 
  int      ibox, jbox, i1, j1, ib, jb, i2, j2;
  int      xsize, ysize, x_trunc, y_trunc;
  int      nn[3];
  float    maxval, val, powers[14];
  float    a, b;
  float    tot,pow1,pow2;

 

  /* fill powers of 2 array */
  powers[0] = 1.0;
  for (j = 1; j < 14; j++) 
     powers[j] =  2.0* powers[j-1];

  /* find smallest power of 2 greater than x1, y1 */
  xsize = 1; i = 0;
  while (xsize < x1) { i++; xsize = (int) powers[i]; }
  ysize = 1; i = 0;
  while (ysize < y1) { i++; ysize = (int) powers[i]; }


  /*
   * important note:  fft2d() assumes it is passed a square Complex array; however, both
   *   fft2d_rect() and fourn() allow rectangular arrays.  There is the question of which
   *   of these to use.  
   *
   * fft2d_rect() is a friendly interface to fourn() and also normalizes the xformed vector.
   * fourn() is the actual fft routine and is called in either case; calling it directly
   *   eliminates the middle man.
   *
   * there is an additional problem with fourn(), regarding the indexing.  In all the 
   * complex number manipulation routines we have the typedef Complex.  However, as fourn()
   * is taken from _Numerical_Recipes_in_C_, it assumes it is passed a float pointer 'float *f'.
   * This points to an array of floats (column vector) assumed to be of the following format:
   *
   *    *(f    ) = indeterminate...
   *    *(f + 1) = first real value
   *    *(f + 2) = first imaginary value
   *    *(f + 3) = second real value
   *    *(f + 4) = second imaginary value
   *        .
   *        .
   *        .
   *       etc...
   *
   * Thus fourn() can be called by passing the complex array Complex *C in a dirty way as:
   *
   *   fourn( ..., (float *)(C) - 1, ...);
   *
   * I do not think that fourn() will access the indeterminate zeroth element of this array, but
   * it is still an opportunity for 'illegal' memory access.  To ensure that this never happens,
   * '*C' is malloc'd in this routine (the vectors *v1a, *v2a, *v3a, *v4a) with an additional
   * unused zeroth element.  All the indexing to these vectors must thus add 1, as in
   *
   *   c1 = *(v3a + 1 + j*xsize + i);
   *              ^^^ 
   *
   * I decided to call fourn() directly and include the equivalent fft2d_rect() calls commented
   * out adjacent.  The indexing shift is now a much safer
   *
   *   fourn( ..., (float *)(C) + 1, ...);
   *
   * because the zeroth element of C is never touched.  I hope this is clear as mud now.
   */

  /* malloc fft buffers */
  v1a = (Complex *) malloc((1 + xsize*ysize)*sizeof(Complex));
  v2a = (Complex *) malloc((1 + xsize*ysize)*sizeof(Complex));
  v3a = (Complex *) malloc((1 + xsize*ysize)*sizeof(Complex));
  v4a = (Complex *) malloc((1 + xsize*ysize)*sizeof(Complex));

  /* set v2 transfer index limits to size of v2 */
  x_trunc = x2;
  y_trunc = y2;  

  /* truncate v2 if it is bigger than v1 either in x or in
   *  y; for example, set x_trunc = x1 - 1 for transferring 
   *  v2 to v2a under the index i < x_trunc
   */
  if (x2 >= x1 ) {
    Alert("findoffset():  region 2 wider than region 1   ");
    Alert("findoffset():  truncating region 2 x-dimension");
    x_trunc = x1 - 1;
  }
  if (y2 >= y1 ) {
    Alert("findoffset():  region 2 higher than region 1  ");
    Alert("findoffset():  truncating region 2 y-dimension");
    y_trunc = y1 - 1;
  }

  /*  
   * copy v1 to v1a, placing data values in the upper left corner
   * and zero-padding the remainder.
   */

  /* zero v1a */
  for (j = 0; j < xsize*ysize + 1; j++) 
      (*(v1a + j)).real = (*(v1a+j)).imag = 0.0;

  /* copy v1 values -> v1a */
  for (j = 0; j < y1; j++) 
    for (i=0;i<x1;i++) 
      *(v1a + j*xsize + i + 1) = *(v1 + j*x1 + i);

  /* find and remove mean for v1a */
  tot = 0.0;
  for (j = 1; j < xsize*ysize + 1; j++) tot +=   (*(v1a + j)).real; 
  tot /= ((float) (xsize*ysize));
  for (j = 1; j < xsize*ysize + 1; j++) (*(v1a + j)).real -= tot; 

  /* 
   * Copy v2 to v2a, placing v2 data values in upper left
   * corner of v2a, and zero-padding the remainder.  v2a is
   * the same size as v1a
   */

  /* zero v2a */
  for (j = 0; j < xsize*ysize + 1; j++) 
      (*(v2a + j)).real = (*(v2a+j)).imag = 0.0;

  /* copy v2 values */
  for (j = 0; j < y_trunc; j++) 
    for (i = 0; i < x_trunc; i++) 
      *(v2a + j*xsize + i + 1) = *(v2 + j*x2 + i);

  /* find and remove mean for v2a */
  tot = 0.0;
  for (j = 1; j < xsize*ysize + 1; j++) tot +=   (*(v2a + j)).real; 
  tot /= ((float) (xsize*ysize));
  for (j = 1; j < xsize*ysize + 1; j++) (*(v2a + j)).real -= tot; 

#if 0 
  /* for debugging only copy v1a to v2a and see what
     correlation at 0,0 offset is */
  for (j = 1; j < xsize*ysize + 1; j++)  *(v2a + j) = *(v1a + j); 

  for (j = 1; j < xsize*ysize + 1; j++)  
   printf("%d   %f   %f   %f   %f\n", j,(*(v2a + j)).real,
   (*(v2a+j)).imag ,   (*(v1a + j)).real, (*(v1a + j)).imag); 
#endif

  /* Get total power in v1a and v2a */
  pow1 = pow2 = 0.0;
  for (j = 1; j < xsize*ysize + 1; j++)  {
   pow1 += (*(v1a + j)).real *  (*(v1a + j)).real
   +  (*(v1a + j)).imag*  (*(v1a + j)).imag;
   pow2 += (*(v2a + j)).real *  (*(v2a + j)).real
   +  (*(v2a + j)).imag *  (*(v2a + j)).imag;
   }

   
  /* diag; print xfer vectors 
   * printMatrix (v1a+1, xsize, ysize, COMPLEX, "v1a");
   * printMatrix (v2a+1, xsize, ysize, COMPLEX, "v2a");
   */


  /* 
   * copy v2a -> v3a with reversed indices; don't move DC 
   *   this is a subtle operation, hinging on the periodic 
   *   nature of DFT sequences 
   */
  for (j = 0; j < ysize; j++) {
    for (i = 0; i < xsize; i++) {
      i1 = (xsize - i) % xsize;
      j1 = (ysize - j) % ysize;
      c1 = *(v2a + 1 + j*xsize + i);
      *(v3a + 1 + j1*xsize + i1) = c1;
    }
  }

  /* diag 
   * printMatrix (v3a+1, xsize, ysize, COMPLEX, "v2a -> rev indx v3a");
   */

  /* conjugate v3a */
  for (j = 0; j < ysize; j++) {
    for (i = 0; i < xsize; i++) {
      c1.real  = (*(v3a + 1 + j*xsize + i)).real;
      c1.imag  = (*(v3a + 1 + j*xsize + i)).imag;
      c1 = Cconj(c1);
      *(v3a + 1 + j*xsize + i) = c1;
    }
  }

  /* diag 
   * printMatrix (v3a+1, xsize, ysize, COMPLEX, "v3a conjugated");
   */
  
  /* take forward transforms of v1a and v3a */
  nn[0] = 0; nn[1] = ysize; nn[2] = xsize;
  /* fourn commented out; use fft2d_rect
   * fourn((float *)(v1a)+1, nn, 2, -1);
   * fourn((float *)(v3a)+1, nn, 2, -1);
   */
  /* equivalent calls to fft2d_rect(): */
  fft2d_rect (v1a+1, xsize, ysize, -1);
  fft2d_rect (v3a+1, xsize, ysize, -1);

  /* diag; print fft of xfer vectors 
   * printMatrix (v1a+1, xsize, ysize, COMPLEX, "fft of v1a");
   * printMatrix (v3a+1, xsize, ysize, COMPLEX, "fft of v3a");
   */

  /* complex element by element freq. domain multiply v1a by v3a, result in v4a */
  for (j = 0; j < ysize; j++) {
    for (i = 0;i < xsize; i++) {
      c1 = *(v1a + j*xsize + i + 1);
      c2 = *(v3a + j*xsize + i + 1);
      c3 = Cmul(c1, c2);
      *(v4a + j*xsize + i + 1) = c3;
    }
  }

  /* diag
   * printMatrix (v4a+1, xsize, ysize, COMPLEX, "el. product v1a x v3a");
   */

  /* inverse fourier transform */
  /* call to fourn() commented out; use fft2d_rect
   * fourn((float *)(v4a)+1, nn, 2, 1);
   */
  /* equivalent call to fft2d_rect(): */
  fft2d_rect (v4a+1, xsize, ysize, 1);

  /* diag 
   * printMatrix (v4a+1, xsize, ysize, COMPLEX, "inv fft ");
   */

  /* find max value */
  maxval = 0.0;
  for (j = 0; j < ysize; j++) {
    for (i = 0;i < xsize; i++) {

      val = Cabs(*(v4a + j*xsize + i + 1));

      if (val > maxval) { 
        *xoff  = i;
        *yoff  = j;
        maxval = val;
      }
    }
  }

  if (*xoff > xsize/2) *xoff -= xsize;
  if (*yoff > ysize/2) *yoff -= ysize;

  *corr = sqrt((float) xsize*ysize)*maxval/sqrt(pow1*pow2);

  free(v1a);
  free(v2a);
  free(v3a);
  free(v4a);

  return;
}



/* 
 * Complex complex COMPLEX arithmetic operations 
 */

Complex Cmul(a, b)
Complex a, b;
{
  Complex x;
  x.real = a.real*b.real - a.imag*b.imag;
  x.imag = a.real*b.imag + a.imag*b.real;
  return x;
}

Complex Cdiv(a, b)
Complex a, b;
{
  Complex x;
  float   u, v, denom;

  if (b.real == 0.0 && b.imag == 0.0) 
    Exit ("rmath:  Cdiv():  divide by zero");
  denom = b.real*b.real + b.imag*b.imag;
  u = b.real / denom;
  v = -b.imag / denom;
  x.real = a.real*u - a.imag*v;
  x.imag = a.real*v + a.imag*u;
  return x;
}

Complex Cadd(a, b)
Complex a, b;
{
  Complex x;
  x.real = a.real + b.real;
  x.imag = a.imag + b.imag;
  return x;
}

Complex Csub(a, b)
Complex a, b;
{
  Complex x;
  x.real = a.real - b.real;
  x.imag = a.imag - b.imag;
  return x;
}

Complex Csmul(s, a)
float s;
Complex a;
{
  Complex x;
  x.real = s*a.real;
  x.imag = s*a.imag;
  return x;
}

Complex Csdiv(s, a)
float s;
Complex a;
{
  Complex x;
  if (s == 0.0) 
    Exit ("rcal_math.c:  Csdiv():  scalar divisor == 0.0");
  x.real = a.real/s;
  x.imag = a.imag/s;
  return x;
}

Complex Cconj(a)
Complex a;
{
  Complex x;
  x.real = a.real;
  x.imag = -a.imag;
  return x;
}

Complex Czero()
{
  Complex x;
  x.real = 0.0;
  x.imag = 0.0;
  return x;
}

Complex Cone()
{
  Complex x;
  x.real = 1.0;
  x.imag = 0.0;
  return x;
}

Complex Cneg_one()
{
  Complex x;
  x.real = -1.0;
  x.imag =  0.0;
  return x;
}

Complex Ci()
{
  Complex x;
  x.real = 0.0;
  x.imag = 1.0;
  return x;
}

Complex Cneg_i()
{
  Complex x;
  x.real =  0.0;
  x.imag = -1.0;
  return x;
}

Complex Cnorm(a)
Complex a;
{
  Complex x;
  float   f;
  if ((f = (float)(Cabs(a))) == 0.0)
    Exit ("rcal_math.c:  Cnorm():  can't normalize zero");
  x.real = a.real/f;
  x.imag = a.imag/f;
  return x;
}

Complex Cphasor(d)
double d;
{
  Complex x;
  x.real = (float)(cos(d));
  x.imag = (float)(sin(d));
  return x;
}

Complex Cneg(a)
Complex a;
{
  Complex x;
  x.real = -a.real;
  x.imag = -a.imag;
  return x;
}

double Cabs(a)
Complex a;
{
  double x;
  x = (a.real == 0.0 && a.imag == 0.0) ? 
      0.0 : sqrt (a.real*a.real + a.imag*a.imag);
  return x;
}

double Cphase(a)
Complex a;
{
  double real, imag;
  real = (double)(a.real);
  imag = (double)(a.imag);
  return (atan2(imag, real));
}

void Cprintf(a)
Complex a;
{
  printf (" Complex (%f, %f) \n", a.real, a.imag);
  return;
}

/* The following 3 FFT routines are graciously provided by John Villasenor ;
 * the fourn() routine was taken from _Numerical_Recipes_in_C_.
 */

/* 
 * I wrote a buffer routine (actually for my class) to spare people from
 * the indexing.  From your program, you can take a 2D FFT of an N by N
 * complex array by the command
 *
 * fft2d(array,N,direction)
 *
 * where direction = -1 for forward, 1 for reverse.
 *
 * You can also use the routine fft1d included below.
 *
 * Note - if your 2D sequence is not square, you need to modify
 * the fft2d routine.
 * John
 */

/* 
 * Multidimensional FFT routine from numerical recipes
 */

#define SWAP2(a,b) tempr=(a);(a)=(b);(b)=tempr

void fourn(data,nn,ndim,isign)
float data[];
int nn[],ndim,isign;
{
    int i1,i2,i3,i2rev,i3rev,ip1,ip2,ip3,ifp1,ifp2;
    int ibit,idim,k1,k2,n,nprev,nrem,ntot;
    float tempi,tempr;
    double theta,wi,wpi,wpr,wr,wtemp;

    ntot=1;
    for (idim=1;idim<=ndim;idim++) ntot *= nn[idim];
    nprev=1;

    for (idim = ndim; idim >= 1; idim--) {
        n     = nn[idim];
        nrem  = ntot / (n*nprev);
        ip1   = nprev << 1;
        ip2   = ip1*n;
        ip3   = ip2*nrem;
        i2rev = 1;
        for (i2 = 1; i2 <= ip2; i2 += ip1) {
            if (i2 < i2rev) {
                for (i1 = i2;i1 <= i2 + ip1 - 2; i1 += 2) {
                    for (i3 = i1; i3 <= ip3; i3 += ip2) {
                        i3rev = i2rev + i3 - i2;
                        SWAP2(data[i3], data[i3rev]);
                        SWAP2(data[i3+1], data[i3rev+1]);
                    }
                }
            }
            ibit = ip2 >> 1;
            while (ibit >= ip1 && i2rev > ibit) {
                i2rev -=  ibit;
                ibit  >>= 1;
            }
            i2rev += ibit;
        }
        ifp1 = ip1;
        while (ifp1 < ip2) {
            ifp2  = ifp1 << 1;
            theta = isign*6.28318530717959/(ifp2/ip1);
            wtemp = sin(0.5*theta);
            wpr   = -2.0*wtemp*wtemp;
            wpi   = sin(theta);
            wr    = 1.0;
            wi    = 0.0;
            for (i3 = 1; i3 <= ifp1; i3 += ip1) {
                for (i1 = i3;i1 <= i3 + ip1 - 2; i1 += 2) {
                    for (i2 = i1; i2 <= ip3; i2 += ifp2) {
                        k1         = i2;
                        k2         = k1 + ifp1;
                        tempr      = wr*data[k2] - wi*data[k2 + 1];
                        tempi      = wr*data[k2 + 1] + wi*data[k2];
                        data[k2]   = data[k1] - tempr;
                        data[k2+1] = data[k1 + 1] - tempi;
                        data[k1]   += tempr;
                        data[k1+1] += tempi;
                    }
                }
                wr = (wtemp=wr)*wpr - wi*wpi + wr;
                wi = wi*wpr + wtemp*wpi + wi;
            }
            ifp1 = ifp2;
        }
        nprev *= n;
    }
}

#undef SWAP2



/* 
 *  Gets the 2D FFT of the array "array" by calling the routine fourn.c
 *  This routine basically acts as a buffer from fourn.c, which has
 *  confusing indexing conventions and calling variables.
 *  If you don't want to use this routine, you can use fourn.c directly
 *  using:
 *      nn[0] = 0; nn[1] = N ; nn[2] = N;
 *      fourn((float *)(array)-1,nn,2,-1);
 */

void fft2d (array, n, direction)
Complex *array;
int n, direction;

{
    int   i; 
    int   size;
    int   nn[3];
    float norm;
    float *myV;

    norm = (float)(n);
    size = n*n;
    if (size == 0 || norm == 0.0)
      Exit ("rcal_math.c:  fft2d():  bad size/norm (0)");

    /* set up my copy of the passed vector */
    myV  = (float *)(malloc((size + 1)*sizeof(Complex)));
    *myV = 0.0;
    for (i = 0; i < size; i++){
      *(myV + 2*i + 1)     = (array+i)->real;
      *(myV + 2*i + 1 + 1) = (array+i)->imag;
    }

    nn[0] = 0; 
    nn[1] = n;  
    nn[2] = n; 
    fourn(myV, nn, 2, direction);

    /* get the values back into array */
    for (i = 0; i < size; i++){
      (array+i)->real = *(myV + 2*i + 1);
      (array+i)->imag = *(myV + 2*i + 1 + 1);
    }
    free(myV);

    /* normalize */
    for (i = 0; i < size; i++) 
      *(array+i) = Csdiv (norm, *(array+i));
   
    return;
}


/* 
 * Since not all ffts are square, the following allows for a rectangular
 * 2-D FFT.  It is a simple modification of fft2d().
 */

void fft2d_rect (array, m, n, direction)
Complex *array;
int     m, n, direction;

{
    int   i; 
    int   size;
    int   nn[3];
    float norm;
    float *myV;

    size = m*n;
    norm = (float)(sqrt((double)(size)));
    if (size == 0 || norm == 0.0)
      Exit ("rcal_math.c:  fft2d_rect():  bad size/norm (0)");

    /* set up my copy of the passed vector */
    myV  = (float *)(malloc((size + 1)*sizeof(Complex)));
    *myV = 0.0;
    for (i = 0; i < size; i++){
      *(myV + 2*i + 1)     = (array+i)->real;
      *(myV + 2*i + 1 + 1) = (array+i)->imag;
    }

    /* m is the x-dimension, n is the y-dimension of 'array' */
    nn[0] = 0; 
    nn[1] = n; 
    nn[2] = m; 

    /* note bizarre casting, shift inf fourn() call */
    fourn(myV, nn, 2, direction);

    /* get the values back into array */
    for (i = 0; i < size; i++){
      (array+i)->real = *(myV + 2*i + 1);
      (array+i)->imag = *(myV + 2*i + 1 + 1);
    }
    free(myV);

    /* normalize */
    for (i = 0; i < size; i++) 
      *(array+i) = Csdiv (norm, *(array+i));

    return;
}


/* 
 *  Gets the 1D FFT of the array "array" by calling the routine fourn.c
 *  This routine basically acts as a buffer from fourn.c, which has
 *  confusing indexing conventions and calling variables.
 */

void fft1d(array, n, direction)
Complex array[];
int n, direction;

{
   int i, size = n;
   int nn[3];
   float nroot;

   nroot= sqrt( (float) n);

   nn[0] = 0; nn[1] = n ; nn[2] = 0; 
   fourn((float *)(array) - 1, nn, 1, direction);

   for (i = 0;i < size; i++) {
      array[i].real /= nroot;
      array[i].imag /= nroot;
    }
   
}

/* oversamp1d() works with a (passed) 1 dimensional array of floating point values
 *   which correspond to real amplitudes; i.e. the data is assumed basebanded and
 *   real valued.  The result of oversampling the input vector 'vin' is returned 
 *   in 'vout'.
 *
 *   Note that the 'high frequency component' of the transform of the input vector
 *   'vin' is actually an ambiguity; 1/2 power is from positive frequency, 1/2 is
 *   from negative.  This is fine but if the elements are copied out in a naiive 
 *   fashion to the oversampling array, there will be an imbalance in the terms;
 *   they have to be dealt with in a 'halving' manner.  Hence the involved section
 *   of code following the initial copying.
 *
 */

void oversamp1d(vin, vout, dim, os)
float *vin;   /* float input vector              */
float *vout;  /* float oversample output vector  */
int   dim;    /* dimension of target box         */
int   os;     /* oversample factor               */
{
  int     i, j, ii, jj, vsize, osize, indx, oindx;
  int     direction, forward_fft, inverse_fft;
  float   *vinp, chksum;
  Complex *v, *vp, *vo, a, b;

  /* follow custom shown above fourn() routine */
  forward_fft = -1;
  inverse_fft =  1;

  /* determine sizes of 2 arrays (total number of elements) */
  vsize = dim;
  osize = vsize*os;

  /* create two complex vectors 'v' and 'vo' */
  v =  (Complex *)(malloc(vsize*sizeof(Complex)));
  vo = (Complex *)(malloc(osize*sizeof(Complex)));
 
  /* copy the input vector to 'v' */
  for (i = 0, vinp = vin, vp = v; i < vsize; i++) {
    (*vp).real = *vinp;
    (*vp).imag = 0.0;
    vp++;
    vinp++;
  }

  /* 1-D fft vector 'v' */
  direction = forward_fft;
  fft1d (v, dim, direction); 

  /* make sure 'vo' = 0.0 */
  for (i = 0; i < osize; i++) (*(vo+i)).real = (*(vo+i)).imag = 0.0;

  /* 
   * Frequency domain oversampling; we need to zero pad matrix 'v' by
   * copying 'v' into 'vo' with extra zeros; note that ambiguity splitting
   * (.5 values) are taken care of after the main copying code.  
   *
   * As an example of how a copy proceeds, we have:
   *
   *  a b c d e f g h
   *
   *  with dim = 8 and os = 2 becomes
   * 
   *  a   b   c   d   .5e 0   0   0   0   0   0   0   .5e f   g   h
   */

  /* 'left' side */
  for (i = 0; i < dim/2; i++) { 
    *(vo + i) = *(v + i);
  }

  /* 'right' side */
  for (i = dim/2 + 1; i < dim; i++) { 
    oindx = dim*(os-1) + i;
    *(vo + oindx) = *(v + i);
  }

  /* copy ambiguous high frequency elements:  element 'dim/2' */
  a = *(v + dim/2);
  b = Csmul (.5, a);
  *(vo + dim/2)              = b;
  *(vo + dim/2 + dim*(os-1)) = b;
  /* end high freqency ambiguity compensation */

  /* inverse FFT 'vo' */
  direction = inverse_fft;
  fft1d (vo, dim*os, direction); 

  /* diagnostic */
  /* for (i = 0, chksum = 0.0; i < osize; i++) chksum += (*(vo+i)).imag;    */
  /* printf ("check that oversamp1d() got zeros for imaginary values: \n"); */
  /* printf ("  sum(vo[i][j].imag), %d terms, = %f\n", osize, chksum);      */

  /* copy real part of 'vo' into 'vout' */
  for (i = 0; i < osize; i++) *(vout+i) = (*(vo+i)).real;

  /* free 'v' and 'vo' and return */
  free (v);
  free (vo);
  return;
}


/* oversamp2d() works with a (passed) 2 dimensional array of floating point values
 *   which correspond to real amplitudes; i.e. the data is assumed basebanded and
 *   real valued.  The result of oversampling the input vector 'vin' is returned 
 *   in 'vout'.
 *
 *   Note that the 'high frequency component' of the transform of the input vector
 *   'vin' is actually an ambiguity; 1/2 power is from positive frequency, 1/2 is
 *   from negative.  This is fine but if the elements are copied out in a naiive 
 *   fashion to the oversampling array, there will be an imbalance in the terms;
 *   they have to be dealt with in a 'halving' manner.  Hence the involved section
 *   of code following the initial copying.
 *
 *   Note that since we use 1-D vectors to represent 2-D matrices, I 
 *   somewhat incorrectly use the terms 'vector', 'matrix', and 'array' 
 *   interchangeably, depending on circumstance.
 */

void oversamp2d(vin, vout, dim, os)
float *vin;   /* float input vector              */
float *vout;  /* float oversample output vector  */
int   dim;    /* dimension of target box         */
int   os;     /* oversample factor               */
{
  int     i, j, ii, jj, vsize, osize, indx, oindx;
  int     direction, forward_fft, inverse_fft;
  float   *vinp, chksum;
  Complex *v, *vp, *vo, *vop, a, b;

  /* follow custom shown above fourn() routine */
  forward_fft = -1;
  inverse_fft =  1;

  /* determine sizes of 2 arrays (total number of elements) */
  vsize = dim*dim;
  osize = vsize*os*os;

  /* create two complex vectors 'v' and 'vo' */
  v =  (Complex *)(malloc(vsize*sizeof(Complex)));
  vo = (Complex *)(malloc(osize*sizeof(Complex)));
 
  /* copy the input vector to 'v' */
  for (i = 0, vinp = vin, vp = v; i < vsize; i++) {
    (*vp).real = *vinp;
    (*vp).imag = 0.0;
    vp++;
    vinp++;
  }

  /* 2-D fft vector 'v' */
  direction = forward_fft;
  fft2d (v, dim, direction); 

  /* make sure 'vo' = 0.0 */
  for (i = 0; i < osize; i++) (*(vo+i)).real = (*(vo+i)).imag = 0.0;

  /* 
   * Frequency domain oversampling; we need to zero pad matrix 'v' by
   * copying 'v' into 'vo' with extra zeros; note that ambiguity splitting
   * (.5 and .25 values) are taken care of after the main copying code.  
   *
   * As an example of how a copy proceeds, we have:
   *
   *  a b c d
   *  e f g h
   *  i j k l
   *  m n p q
   *
   *  with dim = 4 and os = 2 becomes
   * 
   *  a    b    .5c  0    0    0    .5c  d
   *  e    f    .5g  0    0    0    .5g  h
   *  .5i  .5j  .25k 0    0    0    .25k .5l 
   *  0    0    0    0    0    0    0    0 
   *  .5i  .5j  .25k 0    0    0    .25k .5l
   *  m    n    .5p  0    0    0    .5p  q
   */

  /* 'ulh' corner */
  for (i = 0; i < dim/2; i++) { for (j = 0; j < dim/2; j++) {
    indx  = j*dim + i;
    oindx = j*dim*os + i;
    *(vo + oindx) = *(v + indx);
  } }

  /* 'llh' corner */
  for (i = 0; i < dim/2; i++) { for (j = dim/2 + 1; j < dim; j++) {
    indx  = j*dim + i;
    oindx = (dim*(os-1) + j)*(dim*os) + i;
    *(vo + oindx) = *(v + indx);
  } }

  /* 'urh' corner */
  for (i = dim/2 + 1; i < dim; i++) { for (j = 0; j < dim/2; j++) {
    indx  = j*dim + i;
    oindx = j*dim*os + dim*(os-1) + i;
    *(vo + oindx) = *(v + indx);
  } }

  /* 'lrh' corner */
  for (i = dim/2 + 1; i < dim; i++) { for (j = dim/2 + 1; j < dim; j++) {
    indx  = j*dim + i;
    oindx = (dim*(os-1) + j)*(dim*os) + dim*(os-1) + i;
    *(vo + oindx) = *(v + indx);
  } }

  /* copy ambiguous high frequency elements:  row 'dim/2' */
  j = dim/2;
  for (i = 0; i < dim; i++) {
    indx = j*dim + i;
    a = *(v + indx);
    b = Csmul (.5, a);
      
    /* two regions handled separately:  i < dim/2, i > dim/2 */

    if (i < dim/2) {

      /* top part */
      ii = i;
      jj = j;
      oindx = ii + jj*dim*os;
      *(vo + oindx) = b;

      /* bottom part */
      ii = i;
      jj = dim*os - dim/2;
      oindx = ii + jj*dim*os;
      *(vo + oindx) = b;

    }

    else if (i > dim/2) {

      /* top part */
      ii = dim*(os - 1) + i;
      jj = j;
      oindx = ii + jj*dim*os;
      *(vo + oindx) = b;

      /* bottom part */
      ii = dim*(os-1) + i;
      jj = dim*os - dim/2;
      oindx = ii + jj*dim*os;
      *(vo + oindx) = b;

    }

    else { /* i == dim/2 handled below */ }

  } /* end row 'dim/2' */

  /* copy ambiguous high frequency elements:  column 'dim/2' */
  i = dim/2;
  for (j = 0; j < dim; j++) {
    indx = j*dim + i;
    a = *(v + indx);
    b = Csmul (.5, a);
      
    /* two regions handled separately:  j < dim/2, j > dim/2 */

    if (j < dim/2) {

      /* left part */
      ii = i;
      jj = j;
      oindx = ii + jj*dim*os;
      *(vo + oindx) = b;

      /* right part */
      ii = dim*os - dim/2;
      jj = j;
      oindx = ii + jj*dim*os;
      *(vo + oindx) = b;

    }

    else if (j > dim/2) {

      /* left part */
      ii = i;
      jj = dim*(os - 1) + j;
      oindx = ii + jj*dim*os;
      *(vo + oindx) = b;

      /* right part */
      ii = dim*os - dim/2;
      jj = dim*(os-1) + j;
      oindx = ii + jj*dim*os;
      *(vo + oindx) = b;

    }

    else { /* j == dim/2 handled below */ }

  } /* end column 'dim/2' */


  /* copy ambiguous high frequency elements:  element (dim/2, dim/2) */
  i = dim/2;
  j = dim/2;
  indx = j*dim + i;
  a = *(v + indx);
  b = Csmul (.25, a);

  /* ulh */
  ii = dim/2;
  jj = dim/2;
  oindx = ii + jj*dim*os;
  *(vo + oindx) = b;

  /* llh */
  ii = dim/2;
  jj = dim*os - dim/2;
  oindx = ii + jj*dim*os;
  *(vo + oindx) = b;

  /* urh */
  ii = dim*os - dim/2;
  jj = dim/2;
  oindx = ii + jj*dim*os;
  *(vo + oindx) = b;

  /* lrh */
  ii = dim*os - dim/2;
  jj = dim*os - dim/2;
  oindx = ii + jj*dim*os;
  *(vo + oindx) = b;

  /* 
   * end high freqency ambiguity compensation 
   */
  

  /* inverse FFT 'vo' */
  direction = inverse_fft;
  fft2d (vo, dim*os, direction); 

  /* diagnostic */
  /* for (i = 0, chksum = 0.0; i < osize; i++) chksum += (*(vo+i)).imag; */
  /* printf ("check that oversamp2d() got zeros for imaginary values: \n"); */
  /* printf ("  sum(vo[i][j].imag), %d terms, = %f\n", osize, chksum); */

  /* copy real part of 'vo' into 'vout' */
  for (i = 0; i < osize; i++) *(vout+i) = (*(vo+i)).real;

  /* free 'v' and 'vo' and return */
  free (v);
  free (vo);
  return;
}

/* oversamp2dCpx() is the same as oversamp2d, expecting Complex pointers.
 *   The high frequency voodoo is not necessary. 
 *   This operation is non-destructive to the input matrix *vin.
 */
void oversamp2dCpx(vin, vout, dim, os)
Complex *vin;   /* float input vector              */
Complex *vout;  /* float oversample output vector  */
int     dim;    /* dimension of input box          */
int     os;     /* oversample factor               */
{
  int     i, j, k, ii, jj, vsize, osize, indx, oindx;
  int     forward_fft, inverse_fft;
  Complex *vp, *vtmp;

  /* follow custom shown above fourn() routine */
  forward_fft = -1;
  inverse_fft =  1;

  /* determine sizes, copy vin to vtmp */ 
  vsize = dim*dim;
  osize = vsize*os*os;
  vtmp = (Complex *)(malloc(vsize*sizeof(Complex)));
  copyVector(vin, vtmp, COMPLEX, COMPLEX, vsize);

  /* 2-D fft vector 'vin' */
  fft2d (vtmp, dim, forward_fft); 

  writeVector(vtmp, "FFT.small", COMPLEX, vsize);

  /* make sure 'vout' = 0.0 */
  for (i = 0; i < osize; i++) *(vout+i) = Czero();

  /* copy vtmp to 4 corners of vout */
  /* ulh, llh, urh, lrh corners */
  k = dim*(os-1);
  for (i = 0; i < dim/2; i++) { for (j = 0; j < dim/2; j++) {
    indx  = rIndex(i, j, dim);
    oindx = rIndex(i, j, dim*os);
    *(vout + oindx) = *(vtmp + indx);
  } }
  for (i = 0; i < dim/2; i++) { for (j = dim/2; j < dim; j++) {
    indx  = rIndex(i, j, dim);
    oindx = rIndex(i, j + k, dim*os);
    *(vout + oindx) = *(vtmp + indx);
  } }
  for (i = dim/2; i < dim; i++) { for (j = 0; j < dim/2; j++) {
    indx  = rIndex(i, j, dim);
    oindx = rIndex(i + k, j, dim*os);
    *(vout + oindx) = *(vtmp + indx);
  } }
  for (i = dim/2; i < dim; i++) { for (j = dim/2; j < dim; j++) {
    indx  = rIndex(i, j, dim);
    oindx = rIndex(i + k, j + k, dim*os);
    *(vout + oindx) = *(vtmp + indx);
  } }

  writeVector(vout, "FFT.big", COMPLEX, osize);

  /* inverse FFT 'vo' */
  fft2d (vout, dim*os, inverse_fft); 

  free (vtmp);
  return;
}

/* explodeMatrix explodes a complex matrix as per oversamp2dCpx() */
void explodeMatrix(vin, vout, dim, os)
Complex *vin;   /* float input vector              */
Complex *vout;  /* float oversample output vector  */
int     dim;    /* dimension of input box          */
int     os;     /* oversample factor               */
{
  int     i, j, k, ii, jj, vsize, osize, indx, oindx;

  /* determine sizes */
  vsize = dim*dim;
  osize = vsize*os*os;

  /* make sure 'vout' = 0.0 */
  for (i = 0; i < osize; i++) *(vout+i) = Czero();

  /* copy vin to 4 corners of vout */
  /* ulh, llh, urh, lrh corners */
  k = dim*(os-1);
  for (i = 0; i < dim/2; i++) { for (j = 0; j < dim/2; j++) {
    indx  = rIndex(i, j, dim);
    oindx = rIndex(i, j, dim*os);
    *(vout + oindx) = *(vin + indx);
  } }
  for (i = 0; i < dim/2; i++) { for (j = dim/2; j < dim; j++) {
    indx  = rIndex(i, j, dim);
    oindx = rIndex(i, j + k, dim*os);
    *(vout + oindx) = *(vin + indx);
  } }
  for (i = dim/2; i < dim; i++) { for (j = 0; j < dim/2; j++) {
    indx  = rIndex(i, j, dim);
    oindx = rIndex(i + k, j, dim*os);
    *(vout + oindx) = *(vin + indx);
  } }
  for (i = dim/2; i < dim; i++) { for (j = dim/2; j < dim; j++) {
    indx  = rIndex(i, j, dim);
    oindx = rIndex(i + k, j + k, dim*os);
    *(vout + oindx) = *(vin + indx);
  } }

  return;
}

/*****************************************************************
 * 
 * also courtesy John Villasenor:
 *
 * 1D and 2D Discrete Cosine Transforms 
 *
 *****************************************************************/
 

/* 
 *  Gets the 1D DCT of the array "array" 
 */

void dct1d(array,n,direction)
float array[];
int n,direction;

{
   Complex w,x;
   Complex *data;
   int i,nhalf;
   int nn[3];
   float nroot,nfloat,n2float,arg;

   nfloat = (float) n;
   n2float = nfloat * 2.0;
   nhalf = n/2;
   nroot= sqrt( ((float) n)/2.);
 
   if (direction == -1 ) {

   data = (Complex *) malloc(n*sizeof(Complex));

   for (i=0;i<nhalf;i++) {
     (*(data + i)).real = array[2*i];
     (*(data + i)).imag = 0.0;
     (*(data + i + nhalf)).real = array[n-1-2*i];
     (*(data + i + nhalf)).imag = 0.0;
     
   }

   nn[0] = 0; nn[1] = n ; nn[2] = 0; 
   fourn((float *)(data)-1,nn,1,-1);

   for (i=0;i<n;i++) {
    arg = -PI * ( (float) i) /n2float;
    w.real = cos(arg);
    w.imag = sin(arg);
    x = Cmul(w,(*(data + i)) ); 
    array[i] = x.real/nroot;
    }

    free(data);
    array[0] /= sqrt(2.);

  }  /* end if direction */


  if (direction == 1) {

   data = (Complex *) malloc(2*n*sizeof(Complex));

   array[0] *= sqrt(2.0);

   for (i=0;i<n;i++) {
    arg = PI * ( (float) i) /n2float;
    w.real = cos(arg);
    w.imag = sin(arg);
    (*(data + i)).real = w.real * array[i]; 
    (*(data + i)).imag = w.imag * array[i]; 
   }

    (*(data + n)).real = 0.0;
    (*(data + n)).imag = 0.0;

   for (i=n+1;i<2*n;i++) {
    arg = PI * ( (float) i) /n2float;
    w.real = -cos(arg);
    w.imag = -sin(arg);
    (*(data + i)).real = w.real * array[2*n-i]; 
    (*(data + i)).imag = w.imag * array[2*n-i]; 
   }

   nn[0] = 0; nn[1] = 2*n ; nn[2] = 0; 
   fourn((float *)(data)-1,nn,1,1);

   for (i=0;i<n;i++) {
    array[i] = 0.5*(*(data +i)).real/nroot;
    }

   free(data);

  } /* end if direction  */
}
/*****************************************************************/

/* 
 *  Gets the 2D DCT of the array "array" by transforming rows then
 *  columns.  Array is of size n by n.
 */


void dct2d(array,n,direction)
float array[];
int n,direction;

{
   float *data;
   int i,j;
   data = (float *) malloc(n*sizeof(float));

/* rows */
   for (i=0;i<n;i++) {
     for (j=0;j<n;j++) (*(data + j)) = array[n*i + j];
     dct1d(data,n,direction);
     for (j=0;j<n;j++) array[n*i+j] = (*(data + j));
   }
/* columns */
   for (i=0;i<n;i++) {
     for (j=0;j<n;j++) (*(data + j)) = array[i + n*j];
     dct1d(data,n,direction);
     for (j=0;j<n;j++) array[i+n*j] = (*(data + j));
   }
  free(data);
   if (direction == -1 )printf("Forward 2D DCT complete\n");
   if (direction == 1 )printf("Inverse 2D DCT complete\n");
}

/* sum adjacent products in a rectangular complex array */
void complexPhaseFn1(v, nX, nY, pCol, pRow) 
Complex *v;
int      nX, nY;
Complex *pCol, *pRow;
{
  int     i, j, ind1, ind2;
  Complex c1, c2, c3;

  *pRow = Czero();
  *pCol = Czero();

  /* determine pCol from a sum of products in the column -> direction */
  for (j = 0; j < nY; j++) {
    for (i = 1; i < nX; i++) {
      ind1   = rIndex(i-1, j, nX);
      ind2   = rIndex(  i, j, nX);
      c1     = Cconj(*(v+ind1));
      c2     = *(v+ind2);
      c3     = Cmul(c1, c2);
      *pCol  = Cadd(*pCol, c3);
    }
  }

  /*                                                  |           */
  /* determine pRow from a sum of products in the row V direction */
  for (i = 0; i < nX; i++) {
    for (j = 1; j < nY; j++) {
      ind1   = rIndex(i, j-1, nX);
      ind2   = rIndex(i,   j, nX);
      c1     = Cconj(*(v+ind1));
      c2     = *(v+ind2);
      c3     = Cmul(c1, c2);
      *pRow  = Cadd(*pRow, c3);
    }
  }

  return;
}

/* incrementally multiply out a complex array by two phasors, the
 * time domain equivalent of basebanding.
 */
void complexPhaseFn2(v, nX, nY, phase1, phase2)
Complex  *v;
int      nX, nY;
float    phase1, phase2;
{
  int     i, j, ind;
  float   phiCol, phiRow, phiSum;
  double  dPhiSum;
  Complex cFactor;

  /* loop over columns */
  for (i = 0; i < nX; i++) {
  
    /* get column term of exponent */
    phiCol = (float)(i)*phase1;

    /* loop over rows */
    for (j = 0; j < nY; j++) {

      /* get column term of exponent */
      phiRow  = (float)(j)*phase2;

      /* get phase sum -> double */
      phiSum  = phiRow + phiCol;
      dPhiSum = (double)(phiSum);

      /* convert phase to complex number */
      cFactor = Cphasor(dPhiSum);

      /* conjugate multiplier */
      cFactor = Cconj(cFactor);

      /* multiply out to get new vector element */
      ind        =  rIndex(i, j, nX);
      *(v + ind) = Cmul(cFactor, (*(v + ind)));

    } /* end row loop */

  } /* end column loop */

  return;
}


void convertPowerToDB(v, n, floor)
float *v;
int    n;
float  floor;
{
  int i;
  float x;
  for (i = 0; i < n; i++){
    x = *(v+i);
    if (x <= 0.0) x = EPS;
    x = 10.0*log10(x);
    if (x < floor) x = floor;
    *(v+i) = x;
  }
  return;
}

int twoExponent(n)
int n;
{
  int i;
  i = 0; n--;
  while (n) { i++; n /= 2; }
  return (i);
}

double dBToLinear(dB) double dB;
{ return (pow(10.0, dB/10.0)); }

double linearTodB(l) double l;
{ return (10.0*log10(l)); }

