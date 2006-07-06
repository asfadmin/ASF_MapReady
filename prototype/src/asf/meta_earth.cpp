/*
Meta.h routines specific to the planet Earth.
This is a suprisingly small amount of stuff:
	- Rotation rates
	- The horribly involved UT1 and GHA calculations

Orion Sky Lawlor, olawlor@acm.org, 2006/06/19
*/
#include "asf/meta.h"
#include "asf/units.h"

using namespace asf; /* <- avoid having to type asf:: everywhere... */

/** Length in days of a "tropical year": the year relative to the equinoxes (days/year) */
const static double tropical_year_length_earth=365.24218967;

/** The rotation rate of the Earth relative to inertial coordinates (radians/second) */
const double asf::sidereal_rotation_rate_radians_earth=
	(2 * M_PI/(24*60*60))*(1 + 1.0/tropical_year_length_earth);

/** Given a Julian Day, return the yearly-smoothed Earth rotation 
    difference UT2 - UT1, in seconds.  
    UT2 is smoother than UT1. 
*/
double ut2_minus_ut1(double JD) {
	double T=2000 + (JD - 2451544.53)/365.242189; /* Besselian year */
	double YR=2*M_PI*T; /* year in radians */
	return 0.022*sin(YR) - 0.012*cos(YR) 
			- 0.006*sin(2*YR) + 0.007*cos(2*YR);
}

/**
 This table stores can convert between these three time standards:
 	TAI: Atomic time.
	UTC: TAI plus "leap seconds" to match UT1 within about 1 second.
	UT1: The rotation of the earth relative to the mean sun.
 
 We do this by storing the count of leap seconds, TAI-UTC,
 and a measurement of the Earth's rotation loss, UT1-TAI.
 When summed together, these give "DUT", which is UT1-UTC.
 This approach is better than just storing DUT, because DUT has both
 slowly-varying (rotational) components and discrete (leap second)
 components.
*/
struct meta_ut1_table_entry {
/** UTC Year + "year fraction".  I've defined the year fraction:
	0.0 means January 1; 0.5 means July 1
   since that's when leap seconds happen.  
   "float" is enough until about 1 million AD.
   FIXME: eventually the table should be switched to use a
   Modified Julian Day (MJD) count, not years.
*/
	float year;

/** This is TAI-UTC: a count of the leap seconds that separate 
TAI time and UTC time.  This value should never be interpolated--
instead, the new value applies starting at "year".
*/
	int tai_utc;

/** This is UT1-TAI, the additional rotation of the Earth.
This value can be interpolated safely, to within a few milliseconds 
(a few meters).
*/
	double ut1_tai;

/** Return DUT=UT1-UTC */
	inline double ut1_utc(void) const {return ut1_tai+tai_utc;}
};

static const meta_ut1_table_entry meta_ut1_table[]={
#include "meta_ut1_table.cpp" /* written by script in gha_calculation directory */
};

/**
  Return the ut1_table_entry that corresponds to this UTC time.
*/
meta_ut1_table_entry lookup_ut1_table_entry(int year,int dayOfYear, double secOfDay)
{
	/* Convert the request date to a year plus "year fraction".
	  FIXME: "year fraction" is slightly less accurate than MJD.
	*/
	double start=julian_day_from_ymd(year,1,1); /* defined as 0.0 year */
	double middle=julian_day_from_ymd(year,7,1); /* July 1 is defined as 0.5 year (to make leap seconds work right) */
	double end=julian_day_from_ymd(year+1,1,1); /* defined as 0.999999999999 year */
	double cur=start+dayOfYear-1;
	double y; /* UTC year + "year fraction" */
	if (cur<middle) y=year+0.5*(cur-start)/(middle-start);
	else /* cur>=middle */ y=year+0.5+0.5*(cur-middle)/(end-middle);
	
	/* Find and interpolate the gha_table_entries that bracket our year */
	const int table_length=sizeof(meta_ut1_table)/sizeof(meta_ut1_table_entry);
	for (int e=0;e<table_length-1;e++) {
		const meta_ut1_table_entry &L=meta_ut1_table[e];
		const meta_ut1_table_entry &R=meta_ut1_table[e+1];
		if (L.year<=y && y<R.year) {
			/* Interpolate L and R entries. 
			  FIXME: is cubic interpolation needed here?
			*/
			meta_ut1_table_entry ret;
			ret.year=y;
			double frac=(y-L.year)/(R.year-L.year); /* 0.0 at L; 1.0 at R */
			ret.ut1_tai=L.ut1_tai+frac*(R.ut1_tai-L.ut1_tai); /* linear interpolation */
			ret.tai_utc=L.tai_utc; /* copy leap second from left */
			return ret;		
		}
	}
	/* Else year not found in table-- abort! */
	fprintf(stderr,
		"Year %.3f not found in UT1 table, which only covers years from %.0f AD to %.0f AD\n",
		y,meta_ut1_table[0].year,meta_ut1_table[table_length-1].year);
	asf::die("UT1 requested for out-of-bounds year.  One of the ASF developers needs to re-run meta_gha_calculation/generate_ut1_table.sh.");
	return *(meta_ut1_table_entry *)0;
}

/**
  Return UT1 seconds of day, which correspond exactly with the 
  rotation of the earth relative to the sun.
*/
ASF_COREDLL double asf::UT1_from_UTC(int year,int dayOfYear, double secOfDay)
{
	double ut1_utc=lookup_ut1_table_entry(year,dayOfYear,secOfDay).ut1_utc();
	return ut1_utc+secOfDay; /* UT1 = (UT1-UTC)+UTC */
}

/**
  Return TAI seconds of day, which correspond exactly with atomic clocks, 
  given the UTC date and seconds of day. 
*/
ASF_COREDLL double asf::TAI_from_UTC (int year,int dayOfYear, double secOfDay)
{
	double tai_utc=lookup_ut1_table_entry(year,dayOfYear,secOfDay).tai_utc;
	return tai_utc+secOfDay; /* TAI = (TAI-UTC)+UTC */
}

/** 
Given a julian centuries since 2000, return the "nutation"
of Earth, in radians, in dpsi (longitude nutation) and deps 
(latitude nutation).  The output of this routine is a few 
dozen microradians, which *is* enough to screw up a geolocation
calculation if you just leave it out.  "Nutation" is caused
by the heavier side of the Earth being pulled toward the Sun
and Moon, which causes a wobble in the Earth's regular rotation.

This subroutine was converted from Fortran by f2c from the original at
	ftp://maia.usno.navy.mil/conv2000/chapter5/NU2000B.f
*/ 
static void nu2000c(double t, double *dpsi, double *deps)
{
    const static double arcseconds_per_revolution = 1.296e6;
    const static double radians_per_arcsecond=4.848136811095359935899141e-6;

    /* Initialized data */
    const static signed char nals[385]	/* was [5][77] */ = {
	0,0,0,0,1,0,0,2,-2,2,0,
	0,2,0,2,0,0,0,0,2,0,1,0,0,0,0,1,2,-2,2,1,0,0,0,0,0,0,2,0,1,1,0,2,
	0,2,0,-1,2,-2,2,0,0,2,-2,1,-1,0,2,0,2,-1,0,0,2,0,1,0,0,0,1,-1,0,0,
	0,1,-1,0,2,2,2,1,0,2,0,1,-2,0,2,0,1,0,0,0,2,0,0,0,2,2,2,0,-2,2,-2,
	2,-2,0,0,2,0,2,0,2,0,2,1,0,2,-2,2,-1,0,2,0,1,2,0,0,0,0,0,0,2,0,0,
	0,1,0,0,1,-1,0,0,2,1,0,2,2,-2,2,0,0,-2,2,0,1,0,0,-2,1,0,-1,0,0,1,
	-1,0,2,2,1,0,2,0,0,0,1,0,2,2,2,-2,0,2,0,0,0,1,2,0,2,0,0,2,2,1,0,
	-1,2,0,2,0,0,0,2,1,1,0,2,-2,1,2,0,2,-2,2,-2,0,0,2,1,2,0,2,0,1,0,
	-1,2,-2,1,0,0,0,-2,1,-1,-1,0,2,0,2,0,0,-2,1,1,0,0,2,0,0,1,2,-2,1,
	1,-1,0,0,0,-2,0,2,0,2,3,0,2,0,2,0,-1,0,2,0,1,-1,2,0,2,0,0,0,1,0,
	-1,-1,2,2,2,-1,0,2,0,0,0,-1,2,2,2,-2,0,0,0,1,1,1,2,0,2,2,0,0,0,1,
	-1,1,0,1,0,1,1,0,0,0,1,0,2,0,0,-1,0,2,-2,1,1,0,0,0,2,-1,0,0,1,0,0,
	0,2,1,2,-1,0,2,4,2,-1,1,0,1,1,0,-2,2,-2,1,1,0,2,2,1,-2,0,2,2,2,-1,
	0,0,0,2,1,1,2,-2,2 };
    const static int cls[462]	/* was [6][77] */ = { -172064161,-174666,
	33386,92052331,9086,15377,-13170906,-1675,-13696,5730336,
	-3015,-4587,-2276413,-234,2796,978459,-485,1374,2074554,
	207,-698,-897492,470,-291,1475877,-3633,11817,73871,
	-184,-1924,-516821,1226,-524,224386,-677,-174,711159,73,
	-872,-6750,0,358,-387298,-367,380,200728,18,318,
	-301461,-36,816,129025,-63,367,215829,-494,111,-95929,
	299,132,128227,137,181,-68982,-9,39,123457,11,19,
	-53311,32,-4,156994,10,-168,-1235,0,82,63110,63,27,
	-33228,0,-9,-57976,-63,-189,31429,0,-75,-59641,-11,
	149,25543,-11,66,-51613,-42,129,26366,0,78,45893,50,
	31,-24236,-10,20,63384,11,-150,-1220,0,29,-38571,-1,
	158,16452,-11,68,32481,0,0,-13870,0,0,-47722,0,-18,
	477,0,-25,-31046,-1,131,13238,-11,59,28593,0,-1,
	-12338,10,-3,20441,21,10,-10758,0,-3,29243,0,-74,
	-609,0,13,25887,0,-66,-550,0,11,-14053,-25,79,8551,
	-2,-45,15164,10,11,-8001,0,-1,-15794,72,-16,6850,-42,
	-5,21783,0,13,-167,0,13,-12873,-10,-37,6953,0,-14,
	-12654,11,63,6415,0,26,-10204,0,25,5222,0,15,16707,
	-85,-10,168,-1,10,-7691,0,44,3268,0,19,-11024,0,-14,
	104,0,2,7566,-21,-11,-3250,0,-5,-6637,-11,25,3353,0,
	14,-7141,21,8,3070,0,4,-6302,-11,2,3272,0,4,5800,
	10,2,-3045,0,-1,6443,0,-7,-2768,0,-4,-5774,-11,-15,
	3041,0,-5,-5350,0,21,2695,0,12,-4752,-11,-3,2719,0,
	-3,-4940,-11,-21,2720,0,-9,7350,0,-8,-51,0,4,4065,
	0,6,-2206,0,1,6579,0,-24,-199,0,2,3579,0,5,-1900,
	0,1,4725,0,-6,-41,0,3,-3075,0,-2,1313,0,-1,-2904,
	0,15,1233,0,7,4348,0,-10,-81,0,2,-2878,0,8,1232,0,
	4,-4230,0,5,-20,0,-2,-2819,0,7,1207,0,3,-4056,0,5,
	40,0,-2,-2647,0,11,1129,0,5,-2294,0,-10,1266,0,-4,
	2481,0,-7,-1062,0,-3,2179,0,-2,-1129,0,-2,3276,0,1,
	-9,0,0,-3389,0,5,35,0,-2,3339,0,-13,-107,0,1,
	-1987,0,-6,1073,0,-2,-1981,0,0,854,0,0,4026,0,-353,
	-553,0,-139,1660,0,-5,-710,0,-2,-1521,0,9,647,0,4,
	1314,0,0,-700,0,0,-1283,0,0,672,0,0,-1331,0,8,663,
	0,4,1383,0,-2,-594,0,-2,1405,0,4,-610,0,2,1290,0,
	0,-556,0,0
    };

    /* Local variables */
    double d,d1,f;
    int i;
    double de, el, dp, om, elp, carg, sarg, depspl, depsls,
	     dpsipl, dpsils;

/* + 
- - - - - - - - 
 N U 2 0 0 0 B 
- - - - - - - - 

Nutation, IAU 2000B (truncated) model. 

Annexe to IERS Conventions 2000, Chapter 5 

This revision:  2002 November 25  (Hacked by OSL 2006/06)

Luni-Solar argument multipliers: 
	     L     L'	 F     D     Om 

Luni-Solar nutation coefficients, unit 1e-7 arcsec: 
longitude (sin, t*sin, cos), obliquity (cos, t*cos, sin) 

Each row of coefficients in CLS belongs with the corresponding row of 
fundamental-argument multipliers in NALS. 
*/
    /* Compute julian centuries from julian days */
    //double t = (*jd - 2451545.0) / 36525.0;

/*  Fundamental (Delaunay) arguments from Simon et al. (1994) */

/*  Mean anomaly of the Moon. */
    d1 = t * 1717915923.2178 + 485868.249036;
    el = fmod(d1, arcseconds_per_revolution) * radians_per_arcsecond;
/*  Mean anomaly of the Sun. */
    d1 = t * 129596581.0481 + 1287104.79305;
    elp = fmod(d1, arcseconds_per_revolution) * radians_per_arcsecond;
/*  Mean argument of the latitude of the Moon. */
    d1 = t * 1739527262.8478 + 335779.526232;
    f = fmod(d1, arcseconds_per_revolution) * radians_per_arcsecond;
/*  Mean elongation of the Moon from the Sun. */
    d1 = t * 1602961601.209 + 1072260.70369;
    d = fmod(d1, arcseconds_per_revolution) * radians_per_arcsecond;
/*  Mean longitude of the ascending node of the Moon. */
    d1 = 450160.398036 - t * 6962890.5431;
    om = fmod(d1, arcseconds_per_revolution) * radians_per_arcsecond;
    
/*  Initialize the nutation values. */
    dp = 0.;
    de = 0.;
/*  Summation of luni-solar nutation series (in reverse order). */
    for (i = 77; i >= 1; --i) {
/*     Argument and functions. */
	d1 = (double) nals[i * 5 - 5] * el + 
	     (double) nals[i * 5 - 4] * elp + 
	     (double) nals[i * 5 - 3] * f + 
	     (double) nals[i * 5 - 2] * d + 
	     (double) nals[i * 5 - 1] * om;
	sarg = sin(d1);
	carg = cos(d1);
/*     Term. */
	dp = dp + (cls[i * 6 - 6] + cls[i * 6 - 5] * t) * sarg + cls[i * 6 - 4] * carg;
	de = de + (cls[i * 6 - 3] + cls[i * 6 - 2] * t) * carg + cls[i * 6 - 1] * sarg;
    }
/*  Convert from 0.1 microarcsec units to radians. */
    dpsils = dp * 1.0e-7*radians_per_arcsecond;
    depsls = de * 1.0e-7*radians_per_arcsecond;
    
/*  ------------------ */
/*  PLANETARY NUTATION */
/*  ------------------ */
/*  Fixed terms to allow for long-period nutation. 
n.b.  These fixed terms account for the omission of the 
long-period planetary terms in the truncated model. 
*/
    dpsipl = -6.5449846949787363e-10;
    depspl = 1.8810770827049998e-9;
/*  ----- */
/*  TOTAL */
/*  ----- */
/*  Add planetary and luni-solar components. */
    *dpsi = dpsipl + dpsils;
    *deps = depspl + depsls;
}



/** Convert Universal Time Coordinates (UTC) to rotation of the Earth, measured
as the number of degrees Greenwich, England (at latitude 0) makes with the 
vernal equinox (at celestial latitude 0).

This is calculated via UT1 according to IAU 2000 scheme outlined here:
	http://www.iers.org/documents/publications/tn/tn30/tn30_066.pdf
It also matches the calculation in the Fortran code here:
	ftp://maia.usno.navy.mil/conv2000/chapter5/
		see GMST2000.f and 
*/
ASF_COREDLL double asf::utc2gha (int year,int dayOfYear, int hour,int min, double sec)
{
#if 1
	/// Return radians given arcseconds
	const double radians_per_arcsecond=4.848136811095359935899141e-6;
	const double centuries_per_day=1.0/36525.0; /* julian centuries per day */
	const double reference_JD=2451545; /* Julian day of IAU reference epoch in 2000 */
	
	double utc_sec=(hour*60+min)*60+sec;
	double ut1_sec=UT1_from_UTC(year,dayOfYear,utc_sec);
	double sec_per_day=24*60*60; // Seconds per julian day
	double julian_day=julian_day_from_year(year)+dayOfYear-1;
	double fractional_day=ut1_sec*(1.0/sec_per_day);
	/* Julian days since IAU reference time */
	double since_ref=(julian_day-reference_JD)+fractional_day; /* rearranged for roundoff! */

// See ftp://maia.usno.navy.mil/conv2000/chapter5/ERA2000.f
	/* Earth Rotation Angle, in radians, according to IAU 2000 scheme */
	double ERA=2*M_PI*(0.7790572732640 + 1.00273781191135448 * since_ref);

// See ftp://maia.usno.navy.mil/conv2000/chapter5/GMST2000.f
	/// Polynomial describing precession of equinoxes as a function of centuries
	const double c0=0.014506; /* arcseconds of equinox offset at year 2000 */
	const double c1=4612.15739966; /* arcseconds of equinox precession per century */
	const double c2=1.39667721; /* arcseconds per century squared */
	const double c3=0.00009344; /* cubed */
	const double c4=0.0000188;  /* to the fourth */
	/* Julian centuries since epoch.  Theoretically this should be TT, terrestrial time,
	  although there isn't much precession in the 60+ leapsecond difference so far! */
	const double t=centuries_per_day*since_ref; 
	/* Greenwich Mean Sidereal Time (GMST), in radians */
	double GMST=ERA+radians_per_arcsecond*(c0+t*(c1+t*(c2+t*(c3+t*c4))));
	
	/* Nutation */
	double nut_lon,nut_lat;
	nu2000c(t,&nut_lon,&nut_lat);
	double EECT=0.0; // I'm ignoring EECT, since it's under half a millidegree.
	
	/* Equation of the equinoxes (EE), from EE2000.f */
	double e0=84381.448, e1=-46.8402, e2=-0.00059, e3=0.001813;
	double EPSA = radians_per_arcsecond*(e0+t*(e1+t*(e2+t*e3)));
	double EE=nut_lon*cos(EPSA)+EECT;
	
	/* Greenwich (apparent) Sidereal Time (GST), in radians, from GST2000.f */
	double GST=GMST+EE;
	
	/// Wraparound
	double deg=fmod(DEGREES_FROM_RADIANS*GST,360.0);
	if (deg<0) deg+=360.0; /* silly fmod--can return negative values... */
	return deg;

#else 
/* Old silly pre-UT1 version.  OK for late 1995; poor for 2000's. */
	/** Constant, linear, and quadratic terms for converting fractional centuries
	  to the rotation of the earth, in degrees */
	const double /*ta=99.6910 */
		ta=99.691716, /* Position in degrees */
		tb=36000.7689, /* Rotational velocity in degrees per century */
		tc=0.0004; /* Slowdown rate, degrees per century squared */
	const double jd1900=2415021.0; /* Julian day for noon 1/1/1900 */
	double greenwich_hour_angle; /*Greenwich Hour Angle.*/
	double julian_century_days=100*365.25; /* defined length of a julian century */
	double fracDay,day,century,subDayDegrees;
	fracDay=julianDay+hour/24.0+min/1440.0+sec/86400.0;
	day=julian_day_from_year(year)+fracDay-jd1900;
	century=day/julian_century_days; /*Julian century with respect to jd1900*/
	subDayDegrees=(360.0/86400.0)*(hour*3600.0+min*60.0+sec);
	greenwich_hour_angle=(ta+tb*century+tc*century*century+subDayDegrees);
	return fmod(greenwich_hour_angle,360.0);
#endif
}


