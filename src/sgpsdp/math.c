#include "math.h"
#include "sgpsdp.h" 
#include "vector.h"

////////////////  Math - PASCAL routines converted ... ////////////////////

double sqr (double arg)
{
	return arg*arg;
}

double RadToDeg (double arg)
{
	return (double)(arg/(2.0*PI)*360.0);
}

double DegToRad (double arg)
{
	return (double)(arg/360.0*(2.0*PI));
}

double Fmod2p (double arg)
{
	double modu, ret;
	double twopi = 2.0*PI;
	modu = arg - (int)(arg/twopi) * twopi;
	if (modu >= 0.0)
		ret = modu;
	else
		ret = modu + twopi;
	return ret;
}

double Modulus(double arg1, double arg2)
{
	double modu;
	modu = arg1 - (long)(arg1/arg2) * arg2;
	if (modu >= 0.0)	return modu;
	else			return modu + arg2;
}

double AcTan(double sinx, double cosx)
{
	// The AcTan - bug. Here some e-mail excerpts ...

// The "AcTan bug" was introduced in Dr. Kelso's translation of the
// original FORTRAN code to his Pascal version. The code has a
// two-argument arctangent function, which returns a value from 0( to
// 360( in the FORTRAN version, but goes from -90( to 270( in the
// Pascal version. It may have been that this change was made so that
// AcTan could be used when determining latitude, and because some
// test cases may have shown absolutely no effect from the change.
// Indeed, sgp4 results appear to not be affected at all by this
// change. And some sdp4 cases are also not affected (probably because
// the calls to AcTan were not in the fourth quadrant, which is the
// only place that there is a difference). However, sdp4 results can
// be affected ...

// But this ACTAN function is completely unnecessary. I never coded it. As you 
// pointed out, every decent programming language has a 2-argument arctangent 
// function that is quadrant-preserving. For my FORTRAN, it's ATAN2(Y,X), where 
// Y is the sine of the angle, and X is the cosine. It returns values from -pi 
// to +pi. A simple change will return values from 0 to 2*pi: 
// Angle = Pi - ATAN2(-Y,-X) 

// atan2 definnition in C++ :
// atan2 returns a value in the range -pi to +pi radians, 
// using the signs of both parameters to determine the quadrant 
// of the return value. 
	double ret;

	if ( (sinx == 0.0) && (cosx == 0.0) )
		ret = 270.0;
	else	{
		ret = atan2(sinx, cosx);
		if (ret <= -PI/2.000000001)	// This will adapt this version with the 'ugly' 
			ret += 2.0*PI;			// one below. Both functions return exactly the same values
	}
// the old version self made ...
/*
	if (cosx == 0.0)	{
		if (sinx > 0.0)
			ret = PI/2.0;
		else
			ret = 3.0*PI/2.0;
	}
	else if (cosx > 0.0) {
	// --------- correction to match FORTRAN version ----------}
//		if (sinx > 0)						// Add
			ret = atan(sinx/cosx);
//		else								// Add
//			ret = 2*PI + atan(sinx/cosx);	// Add
	// --------------------------------------------------------}
 	}
	else
		ret = PI + atan(sinx/cosx);
	}
*/
	return ret;

}

long Round (double arg)
{
	double fFrac, fInt;
	fFrac = modf(arg, &fInt);
	if (fFrac >= 0.5) fInt ++;
	return (long) fInt;
}

void Magnitude (VECTOR *pVector)
{
	pVector->w = sqrt(sqr(pVector->x) + sqr(pVector->y) + sqr(pVector->z));
}

double Dot (VECTOR v1, VECTOR v2)
{
	double fRet;
	fRet = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
	return fRet;
}
//////////////// End of conversion of Math - PASCAL routines ////////////////////
double VecDot( double *X, double *Y, int N)
{
 /* Purpose 
	calculate the dot (inner) product of X and Y
    Inputs
	X, Y are the input vectors of length N
 */
  int i;
  double M;
  M = 0.0;;
  for ( i=0; i<N; i++ ) M += X[i] * Y[i];
  return M;
}
/*-----------------------------------------------------------------------*/
void VecCross( double *X, double *Y, double *Z, int N)
{
  /*
    Purpose
	    calculate the vector cross product z of x and y.
    Inputs
	    X and Y of length N
    Output
	    Z of length N
  */
  int j,k,m;
  for( m = 0; m < N ; m++ ) {
    j = (m+1)%N; k = (m+2)%N;
    Z[m] = X[j] * Y[k] - X[k] * Y[j];
  }
}
/*-----------------------------------------------------------------------*/
double VecMag( double *X, int N)
{
/*  
   Purpose
	   calculate the magnitude of the vector X of length N
   Input 
	   X
   returns the magnitude of X
*/
   return sqrt( VecDot( X, X, N ));
}
/*------------------------------------------------------------------------*/
void UnitVec( double *X, double *Y, int N )
{
  /*
     Purpose
	     calculate a unit vector in the direction of the input
	     vector
     Input 
	     X, a vector
	     N  the length of X and Y
     Output
	     Y, a unit vector in the direction of X
  */
  int i;
  double M;

  M = VecMag( X, N );
  if (M == 0.0) for (i=0;i<N;i++) Y[i] = 0.0;
  else          for (i=0;i<N;i++) Y[i] = X[i]/M;
}
/*-----------------------------------------------------------------------*/
void VecDiff( double *X, double *Y, double *Z, int N)
{
/*  
   Purpose
	    Calculate the vector difference of the input vectors
   Input
	    X, Y are vectors of length N
   Output  
	    Z = X - Y
*/
  int i;
  for (i=0;i<N;i++) Z[i] = X[i] - Y[i];
}
/*-----------------------------------------------------------------------*/
void VecSum( double *X, double *Y, double *Z, int N)
{
  /* purpose sum of two vectors */
  /* input X and Y are vectors of length N
     output Z another vector of length N
  */
  int i;
  for (i=0;i<N;i++) Z[i] = X[i] + Y[i];
}
/*-----------------------------------------------------------------------*/
void VecScale( double u, double *X, double *Y, int N)
{
/*
  Purpose
	  scale a vector
  Input
	  u: a scalar
	  X: the vector to be scaled 
	  N: the length of X
  Output
	  Y: the scaled vector
*/
  int i;
  for(i=0;i<N;i++)Y[i] = X[i] * u;
}

//////////////////////////////////////////////////////////////////////////////
///////////////////Construction area for a CVector class /////////////////////
//////////////////////////////////////////////////////////////////////////////
/*
CVector::CVector() 
{
	m_iDepth = 3;
	m_vector.x = m_vector.y = m_vector.z = m_vector.w = 0.0;
	return;
}

CVector::CVector(VECTOR vIn) 
{
	m_iDepth = 3;
	SetVector(vIn);
	return;
}

CVector::~CVector () 
{
	return;
}

VECTOR CVector::GetVector()
{
	return m_vector;
}

void CVector::SetVector(VECTOR vIn)
{
	m_vector.x = vIn.x;
	m_vector.y = vIn.y;
	m_vector.z = vIn.z;
	m_vector.w = vIn.w;
}

int CVector::GetDepth()
{
	return m_iDepth;
}

void CVector::SetDepth(int iDepth)
{
	m_iDepth = iDepth;
}

double CVector::Dot( VECTOR vIn )
{
// Purpose 
//	calculate the dot (inner) product of X and Y
// Inputs
//	X, Y are the input vectors of length m_iDepth

	double *X = (double *)&m_vector;
	double *Y = (double *)&vIn;

	int i;
	double M;
  
	M = 0.0;;
	for ( i=0; i<m_iDepth; i++ ) M += X[i] * Y[i];
	return M;
}

VECTOR CVector::Cross( VECTOR vIn )
{
//    Purpose
//	    calculate the vector cross product z of x and y.
//    Inputs
//	    m_vector and vIn of length m_iDepth
//    Output
//	    Z of length m_iDepth

	static VECTOR vector;
	double *X = (double *)&m_vector;
	double *Y = (double *)&vIn;
	double *Z = (double *)&vector;
	
	int j,k,m;
	for( m = 0; m < m_iDepth ; m++ ) {
		j = (m+1)%m_iDepth; k = (m+2)%m_iDepth;
		Z[m] = X[j] * Y[k] - X[k] * Y[j];
	}
	return vector;
}

double CVector::Mag()
{  
//   Purpose
//	   calculate the magnitude of the vector X of length m_iDepth
//   Input 
//	   X
//   returns the magnitude of X

	return sqrt( Dot( m_vector ));
}

VECTOR CVector::Unit( VECTOR vIn )
{
//     Purpose
//	     calculate a unit vector in the direction of the input
//	     vector
//     Input 
//	     X, a vector
//	     m_iDepth  the length of X and Y
//    Output
//	     Y, a unit vector in the direction of X

	static VECTOR vector;
	double *X = (double *)&m_vector;
	double *Y = (double *)&vector;
	
	int i;
	double M;

	M = Mag( );
	if (M == 0.0) for (i=0;i<m_iDepth;i++) Y[i] = 0.0;
	else          for (i=0;i<m_iDepth;i++) Y[i] = X[i]/M;
	return vector;
}

VECTOR CVector::Diff( VECTOR vIn )
{  
//   Purpose
//	    Calculate the vector difference of the input vectors
//  Input
//	    X, Y are vectors of length m_iDepth
//   Output  
//	    Z = X - Y

	static VECTOR vector;
	double *X = (double *)&m_vector;
	double *Y = (double *)&vIn;
	double *Z = (double *)&vector;
	
	int i;
	for (i=0;i<m_iDepth;i++) Z[i] = X[i] - Y[i];
	return vector;
}

VECTOR CVector::Sum( VECTOR vIn )
{
// purpose sum of two vectors
// input X and Y are vectors of length m_iDepth
//     output Z another vector of length m_iDepth

	static VECTOR vector;
	double *X = (double *)&m_vector;
	double *Y = (double *)&vIn;
	double *Z = (double *)&vector;
	
	int i;
	for (i=0;i<m_iDepth;i++) Z[i] = X[i] + Y[i];
	return vector;
}

VECTOR CVector::Scale( double u )
{
//  Purpose
//	  scale a vector
//  Input
//	  u: a scalar
//	  X: the vector to be scaled 
//	  m_iDepth: the length of X
//  Output
//	  Y: the scaled vector

	static VECTOR vector;
	double *X = (double *)&m_vector;
	double *Y = (double *)&vector;
	
	int i;
	for(i=0;i<m_iDepth;i++)Y[i] = X[i] * u;
	return vector;
}
*/
