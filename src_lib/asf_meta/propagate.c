#include <assert.h>
#include <unistd.h>

#include "asf.h"
#include "dateUtil.h"
#include "orbital_state_vector.h"
#include "propagate.h"

/* allocation routine for meta_state_vectors */
meta_state_vectors *meta_state_vectors_init(int num_of_vectors);



void printStVec(const char *desc,stateVector s,FILE *f)
{
	fprintf(f,"%s:\n"
		"pos.x=%.2f m\n"
		"pos.y=%.2f m\n"
		"pos.z=%.2f m\n"
		"vel.x=%.2f m/s\n"
		"vel.y=%.2f m/s\n"
		"vel.z=%.2f m/s\n\n",
		desc,
		s.pos.x,s.pos.y,s.pos.z,
		s.vel.x,s.vel.y,s.vel.z);
}

/*******************************************************************************
 * Get the GHA corresponding to the given number of seconds since 1900.*/
double sec2gha(double sec)
{
	julian_date jd;
	hms_time hms;
	sec2date(sec,&jd,&hms);
	return utc2gha(jd.year,jd.jd,hms.hour,hms.min,hms.sec);
}

/*******************************************************************************
 * Write the given seconds-since-1900 value as
 * Year
 * month
 * day
 * seconds
 * to the given file.*/
void printYMDS_date(double sec,FILE *dest)
{
	julian_date jd;
	ymd_date ymd;
	hms_time hms;
	double secOfDay;
	sec2date(sec,&jd,&hms);
	date_jd2ymd(&jd,&ymd);
	secOfDay=date_hms2sec(&hms);
	fprintf(dest,"%d\n",ymd.year);
	fprintf(dest,"%d\n",ymd.month);
	fprintf(dest,"%d\n",ymd.day);
	fprintf(dest,"%f\n",secOfDay);
}

static void
cartesian_to_keplerian (cartesian_orbit_t *co_in, keplerian_orbit_t *ko,
			double *mean_anomaly)
{
  double  a, u, r, v, es, ec, e, E, M_deg, V2, H, i, i_deg;
  double  w_deg, cu, su, somega, comega, omega;
  double  pi, mu;
  double rx = co_in->x, ry = co_in->y, rz = co_in->z;
  double vx = co_in->vx, vy = co_in->vy, vz = co_in->vz;

  pi = M_PI;
  mu = 3.9860045e+5;

  /* determine semi major axis 'a' */
  r = sqrt ((rx * rx) + (ry * ry) + (rz * rz));
  V2 = (vx * vx) + (vy * vy) + (vz * vz);
  a = (mu * r) / ((2.0 * mu) - (r * V2));

 /* determine eccentricity 'e' */
  es = ((rx * vx) + (ry * vy) + (rz * vz)) / sqrt (mu * a);
  ec = 1.0 - (r / a);
  e = sqrt ((es * es) + (ec * ec));

  /* determine mean anomaly'M' */
  E = 2.0 * atan2 ((e - ec), es);
  M_deg = (180.0 / pi) * (E - es);
  if (M_deg < 0.0)
    M_deg = M_deg + 360.0;

  /* determine angle of inclination 'i' */
  H = sqrt (mu * a * (1.0 - (e * e)));
  i = acos (((rx * vy) - (ry * vx)) / H);
  i_deg = i * (180.0 / pi);

  /* determine omega */
  somega = ((ry * vz) - (rz * vy)) / (sin (i) * H);
  comega = ((rx * vz) - (rz * vx)) / (sin (i) * H);
  omega = (180.0 / pi) * 2.0 * atan2 ((1.0 - comega), somega);
  if (omega < 0.0)
    omega = omega + 360.0;
  
  /* determine w_deg */
  su = rz / (r * sin (i));
  cu = ((ry / r) * somega) + ((rx / r) * comega);
  if (rz == 0.0)
    cu = 1.0;
  u = 2 * atan2 ((1.0 - cu), su);
  v = 2 * atan (sqrt ((1.0 + e) / (1.0 - e)) * tan (E / 2.0));
  w_deg = (180.0 / pi) * (u - v);
  if (w_deg < 0.0)
    w_deg = w_deg + 360.0;
    
  ko->a = a;
  ko->e = e;
  ko->i = i_deg;
  ko->cap_omega = omega;
  ko->omega = w_deg;
  *mean_anomaly = M_deg;

  return;
}

static double 
kepler_equation (double mean_anomaly, double eccentricity)
{
  double tol1 = 1e-5, tol2 = 1e-8, ea, sin_ea, cos_ea, dea, a;
  double ma = mean_anomaly, ecc = eccentricity;
  double abm = fabs (mean_anomaly);
  int k = 0;

  assert (abm <= M_PI);

  ea = abm + ecc * sin(abm) / (1 - sin(abm + ecc) + sin(abm));

  do {
    sin_ea = sin(ea);
    cos_ea = cos(ea);
    k++;
    if (k>10) break;

    do {
      dea = (abm + ecc * sin_ea - ea) / (1 - ecc * cos_ea
            + (0.5 * (abm + ecc * sin_ea - ea) * ecc * sin_ea)
            / (1 - ecc * cos_ea));
      ea += dea;
      if ( fabs(dea) > tol1 ) break;
      a = 1 - 0.5 * dea *dea;
      sin_ea = a * sin_ea + dea * cos_ea;
      cos_ea = a * cos_ea - dea * sin_ea;
    } while ( fabs(dea) > tol2 );
  } while ( fabs(dea) > tol2 );

  if ( ma < 0.0 ) ea = -ea;

  return ea;
}

/* Convert keplerian orbit ko with eccentric anomaly ea to cartesian
   orbit co.  */
static void 
keplerian_to_cartesian (keplerian_orbit_t *ko, double ea, 
			cartesian_orbit_t *co) 
{
  double td[8];
  double radius;

  td[0]=cos(ko->cap_omega)*cos(ko->omega)-sin(ko->cap_omega)*sin(ko->omega)
    *cos(ko->i);
  td[1]=sin(ko->cap_omega)*cos(ko->omega)+cos(ko->cap_omega)*sin(ko->omega)
    *cos(ko->i);
  td[2]=sin(ko->omega)*sin(ko->i);
  td[3]=-cos(ko->cap_omega)*sin(ko->omega)-sin(ko->cap_omega)*cos(ko->omega)
    *cos(ko->i);
  td[4]=-sin(ko->cap_omega)*sin(ko->omega)+cos(ko->cap_omega)*cos(ko->omega)
    *cos(ko->i);
  td[5]=cos(ko->omega)*sin(ko->i);
  td[6]=sqrt(1 - pow(ko->e, 2)) * ko->a;
  td[7]=sqrt(GM / pow(ko->a, 3));

  radius = ko->a * (1 - ko->e * cos(ea));

  co->x = ko->a * td[0] * (cos(ea) - ko->e) + td[6] * td[3] * sin(ea);
  co->y = ko->a * td[1] * (cos(ea) - ko->e) + td[6] * td[4] * sin(ea);
  co->z = ko->a * td[2] * (cos(ea) - ko->e) + td[6] * td[5] * sin(ea);
  co->vx = ko->a * td[7] 
    * (td[6] * cos(ea) * td[3] - ko->a * sin(ea) * td[0]) / radius;
  co->vy = ko->a * td[7] 
    * (td[6] * cos(ea) * td[4] - ko->a * sin(ea) * td[1]) / radius;
  co->vz = ko->a * td[7] 
    * (td[6] * cos(ea) * td[5] - ko->a * sin(ea) * td[2]) / radius;
}

static void
derivatives (double cjt, cartesian_orbit_t *co, orbit_derivatives_t *od)
{
  double r2 = pow (co->x, 2) + pow (co->y, 2) + pow (co->z, 2);
  double r1 = sqrt (r2);
  double r3 = r1 * r2;
  static const double j2 = 0.0010826271;
  static const double re = 6378.140;
  double r5 = r2 * r3;
  double d1 = -1.5 * j2 * re * re * GM / r5;
  double d2 = 1 - 5.0 * co->z * co->z / r2;

  od->dx = co->vx;
  od->dy = co->vy;
  od->dz = co->vz;

  od->dvx = co->x * d1 * d2 - (GM * co->x / r3);
  od->dvy = co->y * d1 * d2 - (GM * co->y / r3);
  od->dvz = co->z * d1 * (d2 + 2.0) - (GM * co->z / r3);
}

#define SYSTEM_SIZE 6

/* Starts with a cartesian state vector *co and a time *t, and
   modifies them in place st *t == tout and *co reflects the new
   position at tout.  */
static void
runge_kutta (cartesian_orbit_t *co, double *t, double tout)
{
  static double *ch = NULL, *alpha = NULL;
  static double beta[12][13];
  static const int dim_1 = 13;
  static const int dim_2 = 12;
  static const double fact = 0.7;
  static const double small = 1.0e-8;
  static const double h = 60.0;	/* Initial step size guess.  */
  static const double sp = 1e99;
  double x[SYSTEM_SIZE];	/* To be returned in co.  */
  int idx;      
  double sdt;
  double dt;
  double spp;
  double tpr;


  if ( ch == NULL ) {
    int idx, idx2;
    ch = calloc (dim_1, sizeof (double));
    alpha = calloc (dim_1, sizeof (double));

    for ( idx = 0 ; idx < dim_2 ; idx++ ) {
      for ( idx2 = 0 ; idx2 < dim_1 ; idx2++ ) {
	beta[idx][idx2] = 0.0;
      }
    }
  
    ch[5] = 34.0 / 105.0;
    ch[6] = 9.0/35.0;
    ch[7] = ch[6];   
    ch[8] = 9.0/280.0;
    ch[9] = ch[8] ;
    ch[11] = 41.0/840.0;
    ch[12] = ch[11];
    alpha[1] = 2.0/27.0;
    alpha[2] = 1.0/9.0;
    alpha[3] = 1.0/6.0; 
    alpha[4] = 5.0/12.0;
    alpha[5] = 0.5;
    alpha[6] = 5.0/6.0;
    alpha[7] = 1.0/6.0;
    alpha[8] = 2.0/3.0;
    alpha[9] = 1.0/3.0;
    alpha[10] = 1.0;
    alpha[12] = 1.0;
    beta[0][1] = 2.0/27.0;
    beta[0][2] = 1.0/36.0;
    beta[0][3] = 1.0/24.0;
    beta[0][4] = 5.0/12.0;
    beta[0][5] = 0.05;
    beta[0][6] = -25.0/108.0;
    beta[0][7] = 31.0/300.0;
    beta[0][8] = 2.0;
    beta[0][9] = -91.0/108.0;
    beta[0][10] = 2383.0/4100.0;
    beta[0][11] = 3.0/205.0;
    beta[0][12] = -1777.0/4100.0;   
    beta[1][2] = 1.0/12.0;
    beta[2][3] = 1.0/8.0;
    beta[2][4] = -25.0/16.0;
    beta[3][4] = -beta[2][4];
    beta[3][5] = 0.25;
    beta[3][6] = 125.0/108.0;
    beta[3][8] = -53.0/6.0;
    beta[3][9] = 23.0/108.0;
    beta[3][10] = -341.0/164.0;
    beta[3][12] = beta[3][10];
    beta[4][5] = 0.2;
    beta[4][6] = -65.0/27.0;
    beta[4][7] = 61.0/225.0;
    beta[4][8] = 704.0/45.0;
    beta[4][9] = -976.0/135.0; 
    beta[4][10] = 4496.0/1025.0;
    beta[4][12] = beta[4][10];
    beta[5][6] = 125.0/54.0;
    beta[5][7] = -2.0/9.0;
    beta[5][8] = -107.0/9.0;
    beta[5][9] = 311.0/54.0;
    beta[5][10] = -301.0/82.0;
    beta[5][11] = -6.0/41.0;
    beta[5][12] = -289.0/82.0;
    beta[6][7] = 13.0/900.0;
    beta[6][8] = 67.0/90.0;
    beta[6][9] = -19.0/60.0;
    beta[6][10] = 2133.0/4100.0;
    beta[6][11] = -3.0/205.0;
    beta[6][12] = 2193.0/4100.0;
    beta[7][8] = 3.0;
    beta[7][9] = 17.0/6.0;
    beta[7][10] = 45.0/82.0;
    beta[7][11] = -3.0/41.0;
    beta[7][12] = 51.0/82.0;
    beta[8][9] = -1.0/12.0;
    beta[8][10] = 45.0/164.0;
    beta[8][11] = 3.0/41.0;
    beta[8][12] = 33.0/164.0;
    beta[9][10] = 18.0/41.0;
    beta[9][11] = 6.0/41.0;
    beta[9][12] = 12.0/41.0;
    beta[11][12] = 1.0;
  }

  /* Initialization. */
  sdt = tout - *t >= 0 ? 1 : -1;
  dt = fabs(h) * sdt;
  spp = fabs(sp) * sdt;
  tpr = *t;
  for ( idx = 0 ; idx < SYSTEM_SIZE ; idx++ ) {
    x[idx] = ((double *) co)[idx];
  }

  do {
    double t_dummy = *t;
    double x_dummy[SYSTEM_SIZE];
    double f[13][SYSTEM_SIZE];
    static const int ntimes = 13;
    static const double relative_err = 1e-12;
    static const double absolute_err = 1e-12;
    int kk, ii, jj;		/* Loop indicies.  */
    double err;
    static const double order_cp = 0.125;

    tpr += spp;
    
    for ( idx = 0 ; idx < SYSTEM_SIZE ; idx++ ) {
      x_dummy[idx] = x[idx];
    }
    
    if ( fabs(dt) > fabs(tout - *t) ) {
      dt = tout - *t;
    }

    if ( fabs (*t - tout) < small ) {
      break;
    }
    
    derivatives (*t, (cartesian_orbit_t *) x, (orbit_derivatives_t *) (f[0]));
    
    for ( kk = 1 ; kk < ntimes ; kk++ ) {
      int kk_max = kk;
      for ( ii = 0 ; ii < SYSTEM_SIZE ; ii++ ) {
	double temp = 0.0;
	for ( jj = 0 ; jj < kk_max ; jj++ ) {
	  temp += beta[jj][kk] * f[jj][ii];
	}
	x[ii] = x_dummy[ii] + dt * temp;
      }
      *t = t_dummy + alpha[kk] * dt;
      
      derivatives (*t, (cartesian_orbit_t *) x, 
		   (orbit_derivatives_t *) (f[kk]));
    }
  
    for ( ii = 0 ; ii < SYSTEM_SIZE ; ii++ ) {
      double temp = 0.0;
      for ( jj = 0 ; jj < ntimes ; jj++ ) {
	temp += ch[jj] * f[jj][ii];
      }
      x[ii] = x_dummy[ii] + dt * temp;
    }
    
    err = relative_err;
    for ( ii = 0 ; ii < SYSTEM_SIZE ; ii++ ) {
      double ter = fabs ((f[0][ii] + f[10][ii] - f[11][ii] 
			  - f[12][ii]) * ch[11] * dt);
      double tol = fabs (x[ii]) * relative_err + absolute_err;
      double constant = ter / tol;
      if ( constant > err ) {
	err = constant;
      }
    }
    
    dt = fact * dt * pow ((1 / err), order_cp);
    
    if ( err > 1.0 ) {
      *t = t_dummy;
      for ( idx = 0 ; idx < SYSTEM_SIZE ; idx++ ) {
	x[idx] = x_dummy[idx];
      }
    }
  } while ( (sdt > 0 && *t < tpr) || (sdt < 0 && *t > tpr) );
  
  for ( idx = 0 ; idx < SYSTEM_SIZE ; idx++ ) {
    ((double *)co)[idx] = x[idx];
  }
}

/*****************************************************************************
 * Propagate the given (fixed-earth) state vector from the given time to the
 * next given time.*/
stateVector propagate(stateVector source,double sourceSec,double destSec)
{
	stateVector ret;
	char inBuf[256];
	double ignored;
	int i;
	FILE *asapIn,*asapOut;
/*Convert input state vector to inertial coordinates*/
	fixed2gei(&source,sec2gha(sourceSec));
	OrbitalStateVector *osv 
	  = orbital_state_vector_new (source.pos.x, source.pos.y, 
				      source.pos.z, source.vel.x, 
				      source.vel.y, source.vel.z);
	orbital_state_vector_propagate (osv, destSec - sourceSec);
	ret.pos.x = osv->position->x;
	ret.pos.y = osv->position->y;
	ret.pos.z = osv->position->z;
	ret.vel.x = osv->velocity->x;
	ret.vel.y = osv->velocity->y;
	ret.vel.z = osv->velocity->z;
	vector_free (osv->position);
	vector_free (osv->velocity);
	free (osv);

	/*Convert out state vector to fixed-earth coordinates*/
	gei2fixed(&ret,sec2gha(destSec));
	return ret;
}
/*******************************************************************************
 * FANCY C VERSION OF PROPAGATE (DON'T HAVE TO CALL THE SCRIPT PROPAGATE WHICH
 * USES FORTRAN77 COMPILED PROGRAM 'ASAP')... NOT YET IN WORKING ORDER
********************************************************************************
 * Propagate the given (fixed-earth) state vector from the given time to the
 * next given time.*
*stateVector propagate(stateVector source,double sourceSec,double destSec)
*{
*	stateVector ret;
*	cartesian_orbit_t co[1];
*	keplerian_orbit_t ko[1];
*	double mean_anomaly;
*	static const double d2r = M_PI / 180.0;
*	double ecc_anomaly;
*	double t_step;
*	double t_current;
*	double t_next;
*
*	** Convert input state vector to inertial coordinates.  **
*	fixed2gei(&source,sec2gha(sourceSec));
*
*	** Recompute the cartesian elements, replacing the mean
*           anomaly with the eccentric anomaly.  **
*	co->x  = source.pos.x;
*	co->y  = source.pos.y;
*	co->z  = source.pos.z;
*	co->vx = source.vel.x / 1000.0;
*	co->vy = source.vel.y / 1000.0;
*	co->vz = source.vel.z / 1000.0;
*
*	** Convert cartesian coordinates to Keplerian elements. **
*	cartesian_to_keplerian (co, ko, &mean_anomaly);
*	co->x /= 1000;
*	co->y /= 1000;
*	co->z /= 1000;
*
*	** Convert to radians.  **
*	ko->i *= d2r;
*	ko->cap_omega *= d2r;
*	ko->omega *= d2r;
*	mean_anomaly *= d2r;
*
*	** Calculate eccentric anomaly using the Kepler equation. **
*	ecc_anomaly = kepler_equation (mean_anomaly, ko->e);
*
*	** Convert Keplerian elements to cartesian coordinates. **
*	keplerian_to_cartesian (ko, ecc_anomaly, co);
*
*	t_step = destSec - sourceSec;
*	t_current = sourceSec;
*	do {
*	  t_next = t_current + t_step;
*
*	  if ( (t_step > 0) && (t_next > destSec) ) {
*	    t_next = destSec;
*	  }
*	  else if ( (t_step < 0) && (t_next < destSec) ) {
*	    t_next = destSec;
*	  }
*	  runge_kutta (co, &t_current, t_next);
*	}
*	while ( t_next != destSec ); 
*	   
*	ret.pos.x = co->x * 1000;
*	ret.pos.y = co->y * 1000;
*	ret.pos.z = co->z * 1000;
*	ret.vel.x = co->vx * 1000;
*	ret.vel.y = co->vy * 1000;
*	ret.vel.z = co->vz * 1000;
*
*	gei2fixed(&ret,sec2gha(destSec));
*	return ret;
*}
*******************************************************************************/

/*******************************************************************************
 * Propagate the state vectors in the given meta_parameters structure so they
 * start at the image start. Make nStVec of them, data_int seconds apart.*/
void propagate_state(meta_parameters *meta,int nStVec, double data_int)
{
	julian_date img_jd;
	hms_time img_time;
	double imgSec;
	
	double secToClosest=100000000000.0;
	stateVector closestSt;
	double closestSec=-1;
	
	int startNo;/*Number of starting state vector*/
	int outNo;/*Number of output state vector*/
	meta_state_vectors *new_st;/*Freshly-created state vector structure.*/

/*Search the list of state vectors for the one nearest to the image start.*/
	for (startNo=0;startNo<meta->state_vectors->vector_count;startNo++)
	{
		state_loc *loc=&(meta->state_vectors->vecs[startNo]);
		/*Compute the distance between this state vector and image start.*/
		double absSec=fabs(loc->time);
		if (secToClosest>absSec)
		{/*This state vector is closer than any state vector so far.*/
			secToClosest=absSec;
			closestSt=loc->vec;
			closestSec=loc->time;
		}
	}
/*
	if (!quietflag) printf("   Nearest state vector is %.2f seconds from image start.\n",secToClosest);
*/
	if (closestSec==-1) {
	  sprintf(errbuf,"   ERROR: Couldn't find any nearby state vectors in propagate_state!\n");
	  printErr(errbuf);
	}
	
/*Compute Seconds since 1900 of Start of Image*/
	img_jd.year = meta->state_vectors->year;
	img_jd.jd   = meta->state_vectors->julDay;
	date_sec2hms(meta->state_vectors->second,&img_time);
	
	imgSec=date2sec(&img_jd,&img_time);

/*Propagate nearest state vector to create each output state vector.*/
	new_st = meta_state_vectors_init(nStVec);
	new_st->year   = img_jd.year;
	new_st->julDay = img_jd.jd;
	new_st->second = date_hms2sec(&img_time);

	for (outNo=0;outNo<nStVec;outNo++)
	{
		double outTime=outNo*data_int;
		new_st->vecs[outNo].time=outTime;
		new_st->vecs[outNo].vec=propagate(closestSt,imgSec+closestSec,imgSec+outTime);
	}

/*Blow away the old state vector structure.*/
	FREE(meta->state_vectors);
	meta->state_vectors = new_st;
}
