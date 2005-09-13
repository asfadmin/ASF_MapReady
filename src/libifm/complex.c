    return 0.0;
  }

  tmp = c*c - b*b*sin(alpha)*sin(alpha);

  if (tmp <= 0.0) {
    Alert("lawOfCosinesNearSide():  non-Euclidean triangle");
    return 0.0;
  }

  return b*cos(alpha) + sqrt(tmp);
}

double lawOfCosinesFarSide(double alpha, double a, double b)
{
  double tmp;
  if (a <= 0.0 || b <= 0.0) {
    Alert("lawOfCosinesFarSide():  side length(s) non-positive");
    return 0.0;
  }

  tmp = a*a + b*b - 2.0*a*b*cos(alpha);

  /* only the 0.0 case is realistic in this check */
  if (tmp <= 0.0) {
    Alert("lawOfCosinesFarSide():  bad triangle parameters");
    return 0.0;
  }

  return sqrt(tmp);
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
    Exit ("r_math.c:  Csdiv():  scalar divisor == 0.0");
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
  Complex zero;
  zero.real = 0.0;
  zero.imag = 0.0;
  return zero;
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

Complex Cnormalize(a)
Complex a;
{
  Complex x;
  float   f;
  if ((f = (float)(Cabs(a))) == 0.0)
    Exit ("r_math.c:  Cnormalize():  can't normalize zero");
  x.real = a.real/f;
  x.imag = a.imag/f;
  return x;
}

Complex Cphasor(d)
double d;
{
  Complex x;
  d      = fmod(d, TWOPI);
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

/* I made this less careful than 'Cnorm()' and hence faster */
double Cabs(a)
Complex a;
{
  double x;
  x = sqrt (a.real*a.real + a.imag*a.imag);
  return x;
}

double Cnorm(a)
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
  if (real == 0.0 && imag == 0.0) return 0.0;
  return (atan2(imag, real));
}

Complex Csqrt(Complex a)
{
  Complex x;
  float   m;
  m = (float)(sqrt(sqrt(a.real*a.real + a.imag*a.imag)));
  x = Csmul(m, Cphasor(Cphase(a)/2.0));
  return x;
}

void Cprintf(a)
Complex a;
{
  printf (" Complex (%f, %f) \n", a.real, a.imag);
  return;
}
