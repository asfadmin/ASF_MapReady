/*
Orion's Standard Library
Orion Sky Lawlor, 11/3/1999
NAME:		osl/vector3d.h

DESCRIPTION:	C++ 3-Dimentional vector library

This file provides various utility routines for easily
manipulating 3-D vectors-- included are arithmetic,
dot/cross product, magnitude and normalization terms. 
Most routines are provided right in the header file (for inlining).
*/

#ifndef __OSL_VECTOR3D_H
#define __OSL_VECTOR3D_H

#include <math.h> /* for sqrt */

#ifdef __CHARMC__ /* for migration */
#  include "pup.h"
#endif
#undef min /* Microsoft Windows header file pollution */
#undef max

namespace osl {

/// Vector3d is a cartesian vector in 3-space-- an x, y, and z.
///  For cross products, the space is assumed to be right-handed (x cross y = +z)
template <class real>
class Vector3dT {
	typedef Vector3dT<real> vec;
public:
	real x,y,z;
	Vector3dT(void) {}//Default consructor
	/// Simple 1-value constructors
	explicit Vector3dT(int init) {x=y=z=(real)init;}
	explicit Vector3dT(float init) {x=y=z=(real)init;}
	explicit Vector3dT(double init) {x=y=z=(real)init;}
	
	/// 3-value constructor
	Vector3dT(const real Nx,const real Ny,const real Nz) {x=Nx;y=Ny;z=Nz;}
	/// real array constructor
	Vector3dT(const real *arr) {x=arr[0];y=arr[1];z=arr[2];}

	/// Constructors from other types of Vector:
	Vector3dT(const Vector3dT<float> &src) 
	  {x=(real)src.x; y=(real)src.y; z=(real)src.z;}
	Vector3dT(const Vector3dT<double> &src) 
	  {x=(real)src.x; y=(real)src.y; z=(real)src.z;}
	Vector3dT(const Vector3dT<int> &src) 
	  {x=(real)src.x; y=(real)src.y; z=(real)src.z;}

	// Copy constructor & assignment operator are by default
	
	/// This lets you typecast a vector to a real array
	operator real *() {return (real *)&x;}
	operator const real *() const {return (const real *)&x;}

//Basic mathematical operators	
	int operator==(const vec &b) const {return (x==b.x)&&(y==b.y)&&(z==b.z);}
	int operator!=(const vec &b) const {return (x!=b.x)||(y!=b.y)||(z!=b.z);}
	vec operator+(const vec &b) const {return vec(x+b.x,y+b.y,z+b.z);}
	vec operator-(const vec &b) const {return vec(x-b.x,y-b.y,z-b.z);}
	vec operator*(const real scale) const 
		{return vec(x*scale,y*scale,z*scale);}
	vec operator/(const real &div) const
		{real scale=1.0/div;return vec(x*scale,y*scale,z*scale);}
	vec operator-(void) const {return vec(-x,-y,-z);}
	void operator+=(const vec &b) {x+=b.x;y+=b.y;z+=b.z;}
	void operator-=(const vec &b) {x-=b.x;y-=b.y;z-=b.z;}
	void operator*=(const real scale) {x*=scale;y*=scale;z*=scale;}
	void operator/=(const real div) {real scale=1.0/div;x*=scale;y*=scale;z*=scale;}

//Vector-specific operations
	/// Return the square of the magnitude of this vector
	real magSqr(void) const {return x*x+y*y+z*z;}
	/// Return the magnitude (length) of this vector
	real mag(void) const {return sqrt(magSqr());}
	
	/// Return the square of the distance to the vector b
	real distSqr(const vec &b) const 
		{return (x-b.x)*(x-b.x)+(y-b.y)*(y-b.y)+(z-b.z)*(z-b.z);}
	/// Return the distance to the vector b
	real dist(const vec &b) const {return sqrt(distSqr(b));}
	
	/// Return the dot product of this vector and b
	real dot(const vec &b) const {return x*b.x+y*b.y+z*b.z;}
	/// Return the cosine of the angle between this vector and b
	real cosAng(const vec &b) const {return dot(b)/(mag()*b.mag());}
	
	/// Return the "direction" (unit vector) of this vector
	vec dir(void) const {return (*this)/mag();}
	/// Return the right-handed cross product of this vector and b
	vec cross(const vec &b) const {
		return vec(y*b.z-z*b.y,z*b.x-x*b.z,x*b.y-y*b.x);
	}
	/// Make this vector have unit length
	void normalize(void) { *this=this->dir();}
	
	/// Return the largest coordinate in this vector
	real max(void) const {
		real big=x;
		if (big<y) big=y;
		if (big<z) big=z;
		return big;
	}
	/// Make each of this vector's coordinates at least as big
	///  as the given vector's coordinates.
	void enlarge(const vec &by) {
		if (x<by.x) x=by.x;
		if (y<by.y) y=by.y;
		if (z<by.z) z=by.z;     
	}
#ifdef __CK_PUP_H
	void pup(PUP::er &p) {p(x);p(y);p(z);}
#endif
};

/** Utility wrapper routines */
template<class real>
inline real dist(const Vector3dT<real> &a,const Vector3dT<real> &b)
	{ return a.dist(b); }

template<class real>
inline real dot(const Vector3dT<real> &a,const Vector3dT<real> &b)
	{ return a.dot(b); }

template<class real>
inline Vector3dT<real> cross(const Vector3dT<real> &a,const Vector3dT<real> &b)
	{ return a.cross(b); }

typedef Vector3dT<double> Vector3d;
typedef Vector3dT<float> Vector3f;
typedef Vector3dT<int> Vector3i;

/// Allow "3.0*vec" to compile:
template <class scalar_t,class real>
inline Vector3dT<real> operator*(const scalar_t scale,const Vector3dT<real> &v)
		{return Vector3dT<real>(v.x*scale,v.y*scale,v.z*scale);}

/// Polar3d is a Point expressed in a 3D spherical coordinate system--
///  theta is the angle (right-handed about +z) in the x-y axis (longitude);
///  phi is the angle up (toward +z) from the x-y axis (latitude);
///  r is the distance of the Point from the origin.
class Polar3d {
public:
	double theta, phi;///< Angles in radians
	double r;///< Distance from origin
//Nothing too useful can be done here, except convert to/from a Vector3d (see below)
	Polar3d() {} //Default constructor
	Polar3d(double Nt,double Np,double Nr) {theta=Nt;phi=Np;r=Nr;}
	Polar3d(const Vector3d &v);
};

/// Describes how much of a bounding volume is visible
typedef enum {
	boundview_none=0, ///< Object is entirely outside view volume
	boundview_part=1, ///< Object is partially inside view volume
	boundview_all=2   ///< Object is entirely inside view volume
} boundview_t;

/// A Halfspace3d is the portion of a 3d plane lying on
///  one side of the plane (p1,p2,p3).
/// Representation is
///    (n dot p)+d==0 for a plane point p; 
///    (n dot x)+d>0 for inside x
class Halfspace3d {
public:
	Vector3d n;///< Plane normal, dotted with points
	double d;///< Plane offset (signed distance from origin if n is unit)
	
	typedef const Vector3d cv;
	Halfspace3d() {}
	Halfspace3d(cv &p1,cv &p2,cv &p3) {init(p1,p2,p3);}
	Halfspace3d(cv &p1,cv &p2,cv &p3,cv &in) {initCheck(p1,p2,p3,in);}
	//Norm Points into the halfspace; p0 is on the line
	Halfspace3d(cv &norm,cv &p0) {n=norm;d=-n.dot(p0);}
	Halfspace3d(cv &n_,double d_) {n=n_;d=d_;}

	//Set this halfspace to (p1,p2,p3).
	// inside Points are on the right-handed thumb side of p1,p2,p3
	void init(cv &p1,cv &p2,cv &p3) {
		n=(p2-p1).cross(p3-p1);
		d=-n.dot(p1);
	}
	
	//Set this halfspace to (p1,p2,p3) with in inside.
	void initCheck(cv &p1,cv &p2,cv &p3,cv &in)
	{ init(p1,p2,p3); if (side(in)<0) {n=-n;d=-d;} }
	
	/// Returns + if inside halfspace, - if outside (and 0 on line).
	double side(cv &pt) const
	{return n.dot(pt)+d;}
	
	/// Normalize this halfspace's direction vector.
	void normalize(void) {
		double scale=1.0/n.mag();
		n*=scale; d*=scale;
	}
	
	/**
	  Return intersection of this halfspace with this sphere.
	  Sphere can be outside, inside, or middle.
	  WARNING: assumes n is a unit vector (use "normalize")
	*/
	boundview_t hitSphere(cv &pt,double r) {
		double s=side(pt);
		if (s<-r) return boundview_none;
		if (s>r) return boundview_all;
		return boundview_part;
	}
	
	/// Return any point on our plane.
	///   Rationale:  
	///       n dot getPlane() + d = n dot n / n.magSqr() * (-d) +d = 0
	Vector3d getPlane(void) const {return (-d/n.magSqr())*n;}
	
	/// Return a value t such that pos+t*dir lies on our plane.
	double intersectDir(cv &pos,cv &dir) const
		{return -(d+n.dot(pos))/n.dot(dir);}
	double intersect(cv &start,cv &end) const
		{return intersectDir(start,end-start);}
	
	/// Returns the Point that lies on our plane and 
	/// the line starting at start and going in dir
	Vector3d intersectDirPt(cv &start,cv &dir) const
		{return start+dir*intersectDir(start,dir);}
	Vector3d intersectPt(cv &start,cv &end) const
		{return intersectDirPt(start,end-start);}
	
#ifdef __CK_PUP_H
	void pup(PUP::er &p) {n.pup(p); p(d);}
#endif
};

}; //end namespace osl

#endif //__OSL_VECTOR3D_H


