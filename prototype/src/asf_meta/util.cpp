/*
Metadata utility routines.

Orion Sky Lawlor, olawlor@acm.org, 2006/06/20
*/
#include "asf/plugin.h"
#include "asf_meta/util.h"
#include "asf/units.h"
using namespace asf;

vector asf::vecNew(double x,double y,double z) {return vector(x,y,z);}
void asf::vecAdd(const vector a,const vector b, vector *c) {*c=a+b;}
void asf::vecSub(const vector a,const vector b, vector *c) {*c=a-b;}
void asf::vecScale(vector *v,double scale) {*v *= scale;}
double asf::vecMagnitude(const vector v) {return v.mag();}
void asf::vecNormalize(vector *v) {v->normalize();}
double asf::vecDot(const vector a,const vector b) {return a.dot(b);}
void asf::vecCross(const vector a,const vector b,vector *aXb) {*aXb=a.cross(b);}
void asf::vecMul(const vector *matrix,const vector src,vector *dest) {
	*dest=
		src.x*matrix[0]+
		src.y*matrix[1]+
		src.z*matrix[2];
}

double asf::atan2_check(double y, double x)
{
	if (y==0.0 && x==0.0)
		return 0;
	else
		return atan2(y,x);
}

void asf::cart2sph(const vector v,double *r,double *theta,double *phi)
{
       *r=sqrt(v.x*v.x+v.y*v.y+v.z*v.z);     
       *theta=asin(v.z/(*r));          
       *phi=atan2_check(v.y,v.x);
}
void asf::sph2cart(double r,double theta,double phi,vector *v)
{
       v->x=r*cos(theta)*cos(phi);     
       v->y=r*cos(theta)*sin(phi);          
       v->z=r*sin(theta);
}



/** Rotate this state vector about the Z axis counterclockwise by angle phi
degrees, then add Coriolis velocity corresponding to a counterclockwise
frame rotation rate  of omega radians per second. This is the core of a
fixed-Earth to inertial (and back) transform.  */
asf::meta_state_t asf::meta_state_t::rotate_coriolis(double phi_deg,double omega) const
{
	double phi=RADIANS_FROM_DEGREES*phi_deg;
	double sp,cp; /* rotation terms */
	if (phi!=0) {sp=sin(phi), cp=cos(phi);}
	else {sp=0.0; cp=1.0;} /* <- common no-rotation case */
	asf::meta_state_t ret;
	ret.pos.x = pos.x*cp - pos.y*sp; /* <- 2x2 rotation matrix */
	ret.pos.y = pos.x*sp + pos.y*cp;
	ret.pos.z = pos.z; /* Z is unchanged */
	
	/** This is the coriolis velocity of the rotating reference frame */
	asf::meta2D_t coriolis(-ret.pos.y*omega,+ret.pos.x*omega);
	ret.vel.x = vel.x*cp - vel.y*sp + coriolis.x;
	ret.vel.y = vel.x*sp + vel.y*cp + coriolis.y;
	ret.vel.z = vel.z;
	return ret;
}


/** Interpolate state vectors A and B, which are at times 
(in seconds) timeA and timeB, to state vector OUT, at time timeOut.
*/
void asf::interp_stVec(const stateVector *st1,double time1, 
				const stateVector *st2, double time2,
				stateVector *stOut,double timeOut)
{
	int i;
	double deltaT=time2-time1; /* We scale the time between input vectors to 1.0 */
	double t=(timeOut-time1)/deltaT;
	double t2=t*t;  /* t squared */
	double t3=t2*t; /* t cubed */
	for (i=0;i<3;i++)
	{
	/*For each coordinate axis, set up a cubic position interpolation function r(t)
	such that r(0.0)=st1.pos, r(1.0)=st2.pos, r'(0.0)=st1.vel, and r'(1.0)=st2.vel.
	Velocities are quadratically interpolated consistent with positions.
	FIXME: better results might be possible by fixing the acceleration vector
	  to always point downward (toward the Earth's center of mass)
	*/
		double A,B,Av,Bv;
		/* Scaled inputs to function fit */
		A=st1->pos[i];
		B=st2->pos[i];
		Av=st1->vel[i]*deltaT;
		Bv=st2->vel[i]*deltaT;
		/* Coefficients of cubic position function */
		double coef0=A;
		double coef1=Av;
		double coef2=3*B-3*A-2*Av-Bv;
		double coef3=2*A-2*B+Av+Bv;
		/* Evaluate position function */
		stOut->pos[i]=coef0+coef1*t+coef2*t2+coef3*t3;
		/* Evaluate velocity as derivative of position function */
		stOut->vel[i]=(coef1+2.0*coef2*t+3.0*coef3*t2)/deltaT;
	}
}


/** Return true if this year is a Gregorian leap year.
  Gregorian leap years occur every 4 years, except years 
  divisible by 100 aren't leap years unless also divisible by 400. */
 bool asf::gregorian_leap_year(int year) {
	if ((year%400)==0) return true;
	if ((year%100)==0) return false;
	if ((year%4)==0) return true;
	else return false;
}

/** Return the number of days in this year AD in the Gregorian calendar */
int days_in_gregorian_year(int year)
{
	if (gregorian_leap_year(year))
		return 366;
	else
		return 365;
}

/**
 Return the Julian day of midnight on this day of this month of this year.
*/
 double asf::julian_day_from_ymd(int year,int month,int day) {
/* Compute based on the awesome Fliegel and van Flandern (1968) formulas:
	http://aa.usno.navy.mil/faq/docs/JD_Formula.html
	http://scienceworld.wolfram.com/astronomy/JulianDate.html
For online converters and snarky commentary, see
	http://www.fourmilab.ch/documents/calendar/
*/
	int Y=year, M=month, D=day;
	int MOFF=(M-14)/12;
	return D-32075
		+1461*(Y+4800+MOFF)/4
		+367*(M-2-MOFF*12)/12
		-3*((Y+4900+MOFF)/100)/4
		-0.5; /* the -0.5 converts from noon to midnight */
}

/** Return the julian day number of this (1-based) dayOfYear of year */
 double asf::julian_day_from_yd(int year,int day) {
	return julian_day_from_ymd(year,1,1)+(day-1); /* because day is 1-based */
}

/** Return the julian day of midnight, January 1 of targetYear (backup) */
 double asf::julian_day_from_year(int targetYear)
{
#if 1
	return julian_day_from_ymd(targetYear,1,1);
#else
/* Just count days since 1975 */
	double totalDays=2442413.5; /* Julian day of January 1, 1975. */
	int year;
	if (targetYear<1975)
		for (year=1975;year>targetYear;year--)
			totalDays-=days_in_gregorian_year(year);
	else
		for (year=1975;year<targetYear;year++)
			totalDays+=days_in_gregorian_year(year);
	return totalDays;
#endif
}

/** Look up this enum value's description.  Aborts if not found. */
const enum_value_description_t *asf::lookup_enum_value(int value,const enum_value_description_t *table)
{
	int i=0;
	while (value!=table[i].value) {
		i++; /* Keep on looking */
		if (-1==table[i].value) { /* hit end of table */
			char buf[40];
			sprintf(buf,"%d",value);
			asf::die("Can't locate enum name for enum value "+std::string(buf));
		}
	}
	return &table[i];
}

/** Look up this value's description by name.  Aborts if not found. */
const enum_value_description_t *asf::lookup_enum_name(const char *name,const enum_value_description_t *table) 
{
	const enum_value_description_t *ret=lookup_enum_name_NULL(name,table);
	if (ret==NULL) asf::die("Can't locate enum value for enum name "+std::string(name));
	return ret;
}

/** Look up this value's description by name.  Returns NULL if not found. */
const enum_value_description_t *asf::lookup_enum_name_NULL(const char *name,const enum_value_description_t *table)
{
	int i=0;
	while (0!=strcmp(table[i].name,name)) {
		i++; /* Keep on looking */
		if (-1==table[i].value) return NULL; /* end of table */
	}
	return &table[i];
}


/** These tables are generated from enum.h by enum_parse.pl */
#include "enum_table.cpp"

