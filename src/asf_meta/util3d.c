/***************** Misc. 3D Utility Routines ******************
	originally part of JPL's earthloc software.
	Translated to C by Orion Lawlor, ASF, 6/97
	
void cart2sph(double x,double y,double z,double *r,double *theta,double *phi);
	Converts cartesian x,y,z to spherical r, theta, phi.
	
void sph2cart(double r,double theta,double phi,double *x,double *y,double *z);
	Converts spherical r, theta, and phi back to cartesian x,y,and z.
	
double vecMagnitude(const double *v);
	Returns the length (vector magnitude) of v.

void vecScale(double *v,double scale);
	Scales v by scale by multiplying each component.

void vecNormalize(double *v);
	Changes v into a vector that has a length of one, by dividing v by its magnitude.

void vecCross(double A[3],double B[3],double *aXb);
	Calculate the right-handed cross product of A and B.
	
void vecMul(double matrix[3][3],double vector[3],double *outVec);
	Mulitplies the given vector by the given matrix.
*/
#include "asf.h"
#include "geolocate.h"
double atan2_check(double y, double x)
{
	if (y==0.0 && x==0.0)
		return 0;
	else
		return atan2(y,x);
}

void cart2sph(const vector v,double *r,double *theta,double *phi)
{
       *r=sqrt(v.x*v.x+v.y*v.y+v.z*v.z);     
       *theta=asin(v.z/(*r));          
       *phi=atan2_check(v.y,v.x);
}
void sph2cart(double r,double theta,double phi,vector *v)
{
       v->x=r*cos(theta)*cos(phi);     
       v->y=r*cos(theta)*sin(phi);          
       v->z=r*sin(theta);
}


vector vecNew(double x,double y,double z)
{
	vector ret;
	ret.x=x;ret.y=y;ret.z=z;
	return ret;
}
void vecAdd(const vector a,const vector b, vector *c)
{
	c->x=a.x+b.x;
	c->y=a.y+b.y;
	c->z=a.z+b.z;
}
void vecSub(const vector a,const vector b, vector *c)
{
	c->x=a.x-b.x;
	c->y=a.y-b.y;
	c->z=a.z-b.z;
}
void vecScale(vector *v,double scale)
{
	v->x*=scale;
	v->y*=scale;
	v->z*=scale;
}
double vecMagnitude(const vector v)
{
	return sqrt(v.x*v.x+v.y*v.y+v.z*v.z);
}
void vecNormalize(vector *v)
{
	vecScale(v,1.0/vecMagnitude(*v));
}
double vecDot(const vector a,const vector b)
{
	return a.x*b.x+a.y*b.y+a.z*b.z;
}
void vecCross(const vector a,const vector b,vector *aXb)
{
	aXb->x=a.y*b.z-a.z*b.y;
	aXb->y=a.z*b.x-a.x*b.z;
	aXb->z=a.x*b.y-a.y*b.x;
}
void vecMul(const vector matrix[3],const vector inVec,vector *outVec)
{
	vector product=matrix[0];
	vecScale(&product,inVec.x);
	(*outVec)=product;
	product=matrix[1];
	vecScale(&product,inVec.y);
	vecAdd(product,*outVec,outVec);
	product=matrix[2];
	vecScale(&product,inVec.z);
	vecAdd(product,*outVec,outVec);
}
double vecAngle(const vector a, const vector b)
{
    vecNormalize(&a);
    vecNormalize(&b);
    return acos(vecDot(a,b));
}
