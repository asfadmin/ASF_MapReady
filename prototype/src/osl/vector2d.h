/*
Orion's Standard Library
Orion Sky Lawlor, 2/22/2000
NAME:		osl/vector2d.h

DESCRIPTION:	C++ 2-Dimentional vector library (no templates)

This file provides various utility routines for easily
manipulating 2-D vectors-- included are arithmetic,
dot product, magnitude and normalization terms. 
All routines are provided right in the header file (for inlining).

Converted from osl/vector3d.h.

*/

#ifndef __OSL_VECTOR2D_H
#define __OSL_VECTOR2D_H

#include <math.h>
#undef min /* Microsoft Windows header file pollution */
#undef max

namespace osl {
namespace io { class Serializer; }; /* forward declaration */

class Vector2d;
//Polar2d is a Point expressed in a 2D polar coordinate system--
// theta is the angle (counterclockwise) up from the x axis
// r is the distance of the Point from the origin.
class Polar2d {
public:
	double r;//Distance from origin
	double theta;//Angle in radians
//Nothing too useful can be done here, except convert to/from a Vector2d (see below)
	Polar2d() {} //Default constructor
	Polar2d(double Nr,double Nt) {r=Nr;theta=Nt;}
	Polar2d(const Vector2d &v);//Has to be declared below
	
	void io(osl::io::Serializer &s);
};

/// Templated superclass, for vectors of other types:
///   Vector's coordinates are of type T
///   Vector operations return type Child
template <class T,class Child>
class Vector2dT {
public:
	typedef Vector2dT<T,Child> this_t;

	T x,y;
	Vector2dT(void) {}//Default consructor
	/// Simple 1-value constructor
	explicit Vector2dT(const T init) {x=y=init;}
	/// 2-value constructor
	Vector2dT(const T Nx,const T Ny) {x=Nx;y=Ny;}
	/// 2-value array constructor
	Vector2dT(const T *src) {x=src[0];y=src[1];}
	/// Copy constructor
	Vector2dT(const this_t &copy) {x=copy.x;y=copy.y;}
	
	/// Cast-to-T * operators (treat vector as array)
	operator T *() {return &x;}
	operator const T *() const {return &x;}

/**
Arithmetic operations: these are carefully restricted to just those
 that make unambiguous sense (to me... now...  ;-)
Counterexamples: vector*vector makes no sense (use .dot()) because
T/vector is meaningless (and we'd want a*b/b==a for b!=0), 
ditto for vector&vector (dot?), vector|vector (projection?), 
vector^vector (cross?),T+vector, vector+=T, etc.
*/
	this_t &operator=(const this_t &b) {x=b.x;y=b.y;return *this;}
	int operator==(const this_t &b) const {return (x==b.x)&&(y==b.y);}
	int operator!=(const this_t &b) const {return (x!=b.x)||(y!=b.y);}
	Child operator+(const this_t &b) const {return Child(x+b.x,y+b.y);}
	Child operator+(const Child &b) const {return Child(x+b.x,y+b.y);}
	Child operator-(const this_t &b) const {return Child(x-b.x,y-b.y);}
	Child operator*(const T scale) const 
		{return Child(x*scale,y*scale);}
	Child operator/(const T &div) const
		{T scale=1.0/div;return Child(x*scale,y*scale);}
	Child operator-(void) const {return Child(-x,-y);}
	void operator+=(const this_t &b) {x+=b.x;y+=b.y;}
	void operator-=(const this_t &b) {x-=b.x;y-=b.y;}
	void operator*=(const T scale) {x*=scale;y*=scale;}
	void operator/=(const T div) {T scale=1/div;x*=scale;y*=scale;}


// Vector specific operations
	/// Return the square of the magnitude of this vector
	T magSqr(void) const {return x*x+y*y;}
	/// Return the square of the distance to the vector b
	T distSqr(const this_t &b) const 
		{return (x-b.x)*(x-b.x)+(y-b.y)*(y-b.y);}

	/// Return the CCW perpendicular vector
	Child perp(void) const {return Child(-y,x);}
	/// Return the signed area in parallelogram with given sides
	T area(const this_t &o) const {return x*o.y-y*o.x;}

	/// Perform a counterclockwise test between these three vertices--
	///  returns >0 if this->a->b is counterclockwise.
	T ccw(this_t a,this_t b) const{
		a-=*this;
		b-=*this;
		return a.x*b.y-a.y*b.x;
	}
	
	/// Return the largest coordinate in this vector
	T max(void) const {return (x>y)?x:y;}
	/// Make each of this vector's coordinates at least as big
	///  as the given vector's coordinates.
	void enlarge(const this_t &by)
	{if (by.x>x) x=by.x; if (by.y>y) y=by.y;}
	
	/// Set coordinates so we are less, on each axis, than this value:
	void shrink(const this_t &by) 
	{if (x>by.x) x=by.x; if (y>by.y) y=by.y; }
};

/// Vector2d is a cartesian vector in 2-space-- an x and y.
class Vector2d : public Vector2dT<double,Vector2d> {
public:
	Vector2d(void) {}//Default consructor
	/// Simple 1-value constructor
	explicit Vector2d(const double init) {x=y=init;}
	/// Simple 1-value constructor
	explicit Vector2d(int init) {x=y=init;}
	/// 2-value constructor
	Vector2d(const double Nx,const double Ny) {x=Nx;y=Ny;}
	/// 2-value array constructor
	Vector2d(const double *src) {x=src[0];y=src[1];}
	Vector2d(const float *src) {x=src[0];y=src[1];}
	Vector2d(const int *src) {x=src[0];y=src[1];}
	/// Copy constructor
	Vector2d(const Vector2d &copy) {x=copy.x;y=copy.y;}
	/// Polar coordinates constructor
	Vector2d(const Polar2d &p) 
	{x=p.r*cos(p.theta);y=p.r*sin(p.theta);}

//Vector-specific operations
	/// Return the magnitude (length) of this vector
	double mag(void) const {return sqrt(magSqr());}
	
	/// Return the distance to the vector b
	double dist(const Vector2d &b) const {return sqrt(distSqr(b));}
	
	/// Return the dot product of this vector and b
	double dot(const Vector2d &b) const {return x*b.x+y*b.y;}
	/// Return the cosine of the angle between this vector and b
	double cosAng(const Vector2d &b) const {return dot(b)/(mag()*b.mag());}
	
	/// Return the "direction" (unit vector) of this vector
	Vector2d dir(void) const {return (*this)/mag();}

	/// Return this vector scaled by that
	Vector2d &scale(const Vector2d &b) {x*=b.x;y*=b.y;return *this;}

	/// Return the counterclockwise angle this vector makes 
	///  from the x axis, in radians.
	double angle(void) const {return atan2(y,x);}
	
	void io(osl::io::Serializer &s);
};

/// Allows "3.0*vector2d" to work.
inline Vector2d operator*(const double scale,const Vector2d &v)
	{return Vector2d(v.x*scale,v.y*scale);}	

/// Since this uses actual Vector2d fields & methods,
///  it has to be declared down here.
inline Polar2d::Polar2d(const Vector2d &v)
{
	r=v.mag();
	theta=atan2(v.y,v.x);
}

#define OSL_LINESEG_EPSILON 1.0e-10
#define OSL_LINESEG_EPSILON_M1 (1-OSL_LINESEG_EPSILON)
/// A simple line segment class, used for intersections.
class LineSeg {
	Vector2d s,d;//My startPoint and direction: s at t=0; s+d at t=1.
public:
	/// Build line from start and end Points
	LineSeg(const Vector2d &Ns,const Vector2d &Ne):s(Ns),d(Ne-s) {}
	
	/// Return my location at parameter value t.
	Vector2d at(double t) const {return s+t*d;}
	
	/// Return my start & end Points
	Vector2d start(void) const {return s;}
	Vector2d end(void) const {return s+d;}
	
	/// Return my direction vector
	Vector2d dir(void) const {return d;}
	
	/// Return my right-handed normal (perp. vector)
	Vector2d perp(void) const {return d.perp();}
	
	/// Return the parameter (t value) of the given Point
	double param(const Vector2d &p) const;
	
	/// Shift this line by this distance
	void translate(const Vector2d &by) {s+=by;}
	
	/// Return true and set the appropriate t values if these lines intersect.
	///  withEndPts determines whether endpoint-only intersections are accepted.
	bool intersects(const LineSeg &l2,bool withEndPts,double *t1Val=0,double *t2Val=0) const;
	
	/// Set the intersection, and return true if these lines intersect
	bool intersects(const LineSeg &l2,bool withEndPts,Vector2d *ret) const
	{
		double t;
		if (!intersects(l2,withEndPts,&t)) {
			*ret=at(t);
			return false;
		} else {
			*ret=at(t);
			return true;
		}
	}
	
	/// Compute t values at intersection, or return false if parallel.
	bool intersection(const LineSeg &l2,double &t1,double &t2) const;
};

/// A Halfspace2d is the portion of a 2D plane lying on
///  one side of the line (p1,p2).
///  n dot p+d==0 on line
class Halfspace2d {
public:
	Vector2d n;///< Line normal
	double d;
	
	typedef const Vector2d cv;
	Halfspace2d() {}
	Halfspace2d(cv &p1,cv &p2) {init(p1,p2);}
	Halfspace2d(cv &p1,cv &p2,cv &in) {initCheck(p1,p2,in);}

	/// Set this halfspace to (p1,p2).
	///  p1,p2, inside Point should be right-handed.
	void init(cv &p1,cv &p2) {
		n.x=p1.y-p2.y;
		n.y=p2.x-p1.x;
		d=p1.x*p2.y-p2.x*p1.y;
	}
	
	/// Set this halfspace to (p1,p2), with in inside.
	void initCheck(cv &p1,cv &p2,cv &in)
	{ init(p1,p2);if (side(in)<0) {n=-n;d=-d;} }
	
	/// Returns + if inside halfspace, - if outside (and 0 on line).
	double side(cv &pt) const
	{return n.dot(pt)+d;}
	
	/** Return a value t such that pos+t*dir lies on our line.
	   (just solve  (pos+t*dir).dot(n)+d=0  for t) */
	double intersectDir(cv &pos,cv &dir) const
		{return -(d+n.dot(pos))/n.dot(dir);}
	double intersect(cv &start,cv &end) const
		{return intersectDir(start,end-start);}
	
	/** Returns the Point that lies on our line 
	  and the line from start in dir*/
	Vector2d intersectDirPt(cv &start,cv &dir) const
		{return start+dir*intersectDir(start,dir);}
	Vector2d intersectPt(cv &start,cv &end) const
		{return intersectDirPt(start,end-start);}
};

/// A 2d Pixel location
class Point : public Vector2dT<int,Point>{
public:
	Point(void) {}
	Point(int val) :Vector2dT<int,Point>(val,val) {}
	Point(int x_,int y_) :Vector2dT<int,Point>(x_,y_) { }
	Point(const Vector2d &v) :Vector2dT<int,Point>((int)v.x,(int)v.y) { }
	
	void io(osl::io::Serializer &s);
	
	/// Return true if this point does not lie in these bounds.
	///  "unsigned" allows us to avoid one test per axis.
	inline bool oob(unsigned int wid,unsigned int ht) const {
		/// subtle: if x is negative, (unsigned int)x is big.
		return ((unsigned int)x>=wid) || ((unsigned int)y>=ht);
	}
};

}; //end namespace osl

#endif //__OSL_VECTOR2D_H


