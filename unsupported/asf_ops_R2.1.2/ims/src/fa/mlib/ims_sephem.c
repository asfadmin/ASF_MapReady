static char *sccs = "@(#)ims_sephem.c	5.2 25 Apr 1996";


#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/*
#define RAD(x) (x / 57.29577)
#define ARAD(x) (x * 57.29577)
*/


#define RAD(x) (x)  /* These functions are already in radians */
#define ARAD(x) (x)

double ims_fa_abs(double x);


/* sephem
 * Corey Porter
 * 12/18/95
 */

#include <ims_cpfort.h>

void setsun(double t,double *es)
{
  double rjd = 2.41502e6;
  double dts = 8.64e4;
  double dt = t / dts - rjd;
  double dt1 = dt * 1e-4;
  double dt2 = dt1 * dt1;
  double dt3 = dt1 * dt2;

  es[0] = 1.496e8;
  es[1] = 1.675104e-2 - 1.1444e-5 * dt1 - 9.4e-9 * dt2;
  es[2] = 0.4093197474 - 6.21791e-5 * dt1 - 2.1468e-9 * dt2
         + 1.7977e-10 * dt3;
  es[3] = 0.0;
  es[4] = 4.908229653 + 8.2149855e-7 * dt + 5.9167e-7 * dt2 + 1.22e-9 * dt3;
}

void a_ephem(
  double isun,
  double imoon,
  double isrp,
  double t,
  double *es,
  double *em)
{
  int i;

  double rjd = 2.41502e6;
	double pi = 3.14159265358979323846264338327950288419716939937511;
	double tpi = 6.2831853071795864769252867665590057683943387987502;
  double ans = 1.720196977e-2;
  double anm = 0.22997150294101;
  double dts = 8.64e4;
  double dt = t / dts - rjd;
  double dt1 = dt * 1e-4;
  double dt2 = dt1 * dt1;
  double dt3 = dt1 * dt2;

  double se,ce,si,ci,so,co,ca,alpha,sa,sw,sd,sso,swso,cwso,sdso,cdso,del;

  if(isun == 1 || isrp == 1)
  {
    es[5] = dmod(6.256583575+ans*dt-1.9548e-7*dt2-1.22e-9*dt3,tpi);
    es[6] = ans/dts;
  }

  if(imoon == 1)
  {
    em[0] = 3.844e5;
    em[1] = 5.4900489e-2;
    em[2] = 8.980410805e-2;
    em[3] = 4.523601515 - 9.242202942e-4*dt+2.71748e-6*dt2+8.73e-10*dt3;
    em[4] = 5.835151539+1.944368001e-3*dt-1.35071e-5*dt2-4.538e-9*dt3 -em[3];
    em[5] = 4.719966572+anm*dt-1.4835e-6*dt2+6.80678e-10*dt3-em[3]-em[4];

    for(i=3;i<6;i++)
		{
			em[i] = dmod(em[i],tpi);
		}

    em[6] = anm / dts;

    se = sin(RAD(es[2]));
    ce = cos(RAD(es[2]));
    si = sin(RAD(em[2]));
    ci = cos(RAD(em[2]));
    so = sin(RAD(em[3]));
    co = cos(RAD(em[3]));
    ca = si*se*co-ce*ci;
    alpha = ARAD(acos(ca));
    em[2] = pi - alpha;
    sa = sin(RAD(alpha));
    sw = so*si/sa;
    sd = so*se/sa;
    sso = d_sign(1.0,so);
    swso = sw*so*sso;
    cwso = (sw*ce*co+sd*ci)*sso;
    em[3] = ARAD(atan2(swso,cwso));
    sdso = sd*so*sso;
    cdso = (sd*co*ci+sw*ce)*sso;
    del = ARAD(atan2(sdso,cdso));
    em[4] += del;
  }
}

/* 
 * I have no idea if the following function is at all correct.
 * The original version was so full of assigned GOTO's and other
 * wonderous marvels that make FORTRAN the joy that it is that I
 * couldn't really tell what in the world was going on, so I more 
 * or less fudged it.  There is one piece of code in particular that
 * I could not quite understand (from the original FORTRAN):

      IF (K.GT.10) STOP

 * What in the world is this supposed to mean?  STOP?  Is that like 
 * RETURN?  I have no idea.  At any rate, I've marked the spot where this
 * was supposed to be with a lot of ?'s
 */

void a_kepler(double am,double e,double *ea,double *se,double *ce)
{
  double tol1 = 1e-5, 
         tol2 = 1e-8,
         half = 0.5,
         zero = 0,
         one  = 1,
				 pi   = 3.14159265358979323846264338327950288419716939937511,
				 tpi  = 6.2831853071795864769252867665590057683943387987502,
         k    = 0;

  double abm,sm,ese,f,d,c,a,sen,cen;

  
	while((abm = ims_fa_abs(am)) > pi)
	{
		if(am > pi) am -= tpi;
		else       am += tpi;
	}

  sm = sin(RAD(abm));
  *ea = abm + e * sm / (one - sin(RAD(abm+e)) +sm);

  *se = sin(RAD(*ea));
  *ce = cos(RAD(*ea));
  k++;

  if(k > 10) 
  {
  	/* ?????????????? */
    /* there should be some big error condition here */
		fprintf(stderr,"some huge massive FORTRAN-type error...???\n");
	return;
  }

  for(;;)
  {
    ese = e * *se;
    f = abm + ese - *ea;
    d = one - e * (*ce);
    c = f / (d + half * f * ese / d);
    *ea += c;
    if(ims_fa_abs(c) > tol1)
    {
      *se = sin(RAD(*ea));
      *ce = cos(RAD(*ea));
      k++;
			if(k > 10)
			{
				fprintf(stderr,"huge FORTRAN error!!! Run for your life!\n");
				return;
			}
    }
    else if(ims_fa_abs(c) < tol2)
    {
      break;
    }
		else
		{
    	a = one - half * c * c;
    	sen = a * *se + c * *ce;
    	cen = a * *ce - c * *se;
    	*se = sen;
    	*ce = cen;
		}
  }

  sen = *se + c * *ce;
  cen = *ce - c * *se;
  *se = sen;
  *ce = cen;
  if(am <= zero)
  {
    *se = -*se;
    *ea = -*ea;
  }
}

void coord(double *x,double gm,double *y)
{
  double cc,sc,cw,sw,ci,si,td[8],ce,se,r;
  int i;

  cc = cos(RAD(x[3]));
  sc = sin(RAD(x[3]));
  cw = cos(RAD(x[4]));
  sw = sin(RAD(x[4]));
  ci = cos(RAD(x[2]));
  si = sin(RAD(x[2]));

  td[0] = cc * cw - sc * sw * ci;
  td[1] = sc * cw + cc * sw * ci;
  td[2] = sw * si;
  td[3] = -cc*sw-sc*cw*ci;
  td[4] = -sc*sw+cc*cw*ci;
  td[5] = cw * si;
  td[6] = sqrt(1 - x[1] * x[1]) * x[0];
  td[7] = sqrt(gm /(x[0] * x[0] * x[0]));
  ce = cos(RAD(x[5]));
  se = sin(RAD(x[5]));
  r  = x[0] * (1-x[1]*ce);

  for(i=0;i<3;i++)
  {
    y[i] = x[0] * td[i] * (ce - x[1]) + td[6] * td[i+3] * se;
    y[i+3]=x[0] * td[7] * (td[6] * ce * td[i+3] - x[0] * se * td[i])/r;
  }
}

void sephem(double tj,double *x)
{
  double es[7],em[7],ea,se,ce;
  double isun,imoon,isrp,tjsec,ge;

  tjsec = tj * 86400;

  setsun(tjsec,es);
  isun = 1;
  imoon = 0;
  isrp = 0;

  a_ephem(isun,imoon,isrp,tjsec,es,em);
  a_kepler(es[5],es[1],&ea,&se,&ce);
  es[5] = ea;
  ge = 3.9860045e5;
  coord(es,ge,x);
}

/*
** Found bugs in the other abs() function.
*/

double ims_fa_abs(double x)
{
   if (x < 0)
	  return(x * -1);
   return(x);
}
