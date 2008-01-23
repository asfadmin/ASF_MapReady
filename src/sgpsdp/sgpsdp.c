#include "math.h"
#include "string.h"
#include "stdlib.h"
#include "assert.h"
#include "sgpsdp.h" 

double	ae;
double	tothrd;
double	xkmper;	// 6378.135 {Earth equatorial radius - kilometers (WGS '72)}
double	f;		// 1/298.26 {Earth flattening (WGS '72)}
double	ge;		// 398600.8 {Earth gravitational constant (WGS '72)}
double	J2;		// 1.0826158E-3 {J2 harmonic (WGS '72)}
double	J3;		// -2.53881E-6  {J3 harmonic (WGS '72)}
double	J4;		// -1.65597E-6  {J4 harmonic (WGS '72)}
double	ck2;	// J2/2;
double	ck4;	// -3*J4/8;
double	xj3;	// J3;
double	qo;		// ae + 120/xkmper;
double	s;		// ae + 78/xkmper;	
double	e6a;	// 1E-6;
double	dpinit;	// 1        {Deep-space initialization code}	
double	dpsec;	// 2        {Deep-space secular code}
double	dpper;	// 3        {Deep-space periodic code}
long	secday;	// seconds per day
double	omega_E;// earth rotation per sideral day
double  sr;		// = 696000.0;		{Solar radius - kilometers (IAU 76)}
//double  AU;		// = 1.49597870E8;	{Astronomical unit - kilometers (IAU 76)}

int     iflag,ideep;
double	xke,xmnpda;

double	eqsq,siniq,cosiq,rteqsq,ao,cosq2,sinomo,cosomo;
double	bsq,xlldot,omgdt,xnodot,xnodp;
double	xll,omgasm,xnodes,_em,xinc,xn,t;
double	qoms2t;

// couple forward decls
static void ConvertSatState(CSGP4_SDP4 *self, VECTOR *pos,VECTOR *vel);
static void ConvertData(CSGP4_SDP4 *self);
static void InitSatellite(CSGP4_SDP4 *self);
static void SetSatellite (CSGP4_SDP4 *self, char *cLine0, char *cLine1, char *cLine2, int bInitSatellite);
static void SetSatellite2(CSGP4_SDP4 *self, SATELLITE *pSatellite, int bInitSatellite);

static int SGP4(CSGP4_SDP4 *self, double tsince, int *iflag, VECTOR *pos, VECTOR *vel);
static int SDP4(CSGP4_SDP4 *self, double tsince, int *iflag, VECTOR *pos, VECTOR *vel);

static void Call_dpper(CSGP4_SDP4 *self,double *e,double *xincc,double *omgadf,double *xnode,double *xmam);
static void Call_dpsec(CSGP4_SDP4 *self,double *xmdf,double *omgadf,double *xnode,double *emm,double *xincc, double *xnn,double *tsince);
static void Call_dpinit(CSGP4_SDP4 *self,double *eosq,double *sinio,double *cosio,double *betao,double *aodp, double *theta2,double *sing,double *cosg,double *betao2,double *xmdot, double *omgdot,double *xnodott,double *xnodpp);
static int Deep(CSGP4_SDP4 *self, int ideep);
static void Init(CSGP4_SDP4 *self);

static double JulianDateFromSystemTime (SYSTEMTIME st);		// returns the julian date from SYSTEMTIME
//double JulianDate (CTime ct);			// returns the julian date from CTime
static double JulianDate (double st);			// returns the julian date from the Norad2Line epoch style
static double JulianDateOfYear (int yr);		// returns the JD of a given year
static int EpocheYear (int iYear);			// returns 99 for 1999 and 32 for 2032
static int DayOfYear (int, int, int);		        // returns the day of yeart of a given date.
static double FractionOfDay(int, int, int);	        // returns the fraction of a day.
static double ThetaG(double jd);			// returns the sideral GMT

// Calculates if the Satellite is in an eclipse or visible ...
static double DepthOfEclipse(CSGP4_SDP4 *self, double time, VECTOR r1);
// Calculates the actual Sun position ...
static VECTOR CalculateSolarPosition(double time);
static int Eclipse(double *r, VECTOR *vSun, VECTOR *vMoon, int *em, int *ee, char **which);
static double MoonPhase(double *Sun, double *Moon, double *Observer);
static void SunMoon(double T, double *Sun, double *Moon);
static int GetEclipsed(CSGP4_SDP4 *self);
static VECTOR CalculateLatLonAlt2(VECTOR vPos, double time);

CSGP4_SDP4 *SGP4_SDP4_new_from_tle(char *cLine0, char *cLine1, char *cLine2)
{
  CSGP4_SDP4 *self = malloc(sizeof(CSGP4_SDP4));
  Init(self);
  SetSatellite (self, cLine0, cLine1, cLine2, TRUE);
  return self;
}

// Calculates the User pos and vel, and return m_vUPos and m_vUVel
void CalculateUserPosVel(CSGP4_SDP4 *self, VECTOR *geodetic, double time)
{
//Procedure Calculate_User_PosVel(var geodetic : vector;
//                                        time : double;
//                         var obs_pos,obs_vel : vector);
// Reference:  The 1992 Astronomical Almanac, page K11
	double mfactor = 2.0*PI*omega_E/secday;
	double	lat,lon,alt,theta,c,s,achcp;
	lat = geodetic->x;
	lon = geodetic->y;
	alt = geodetic->z;
	theta = Modulus(ThetaG(time) + lon,2.0*PI);
	geodetic->w = theta;						// {LMST}
	c = 1.0/sqrt(1.0 + f*(f - 2.0)*sqr(sin(lat)));
	s = sqr(1.0 - f)*c;
	achcp = (xkmper*c + alt)*cos(lat);
	self->m_vUPos.x = achcp*cos(theta);			// {kilometers}
	self->m_vUPos.y = achcp*sin(theta);
	self->m_vUPos.z = (xkmper*s + alt)*sin(lat);
	self->m_vUPos.w = sqrt(sqr(self->m_vUPos.x) + sqr(self->m_vUPos.y) + sqr(self->m_vUPos.z));
	self->m_vUVel.x = -mfactor*self->m_vUPos.y;		// {kilometers/second}
	self->m_vUVel.y =  mfactor*self->m_vUPos.x;
	self->m_vUVel.z =  0.0;
	self->m_vUVel.w = sqrt(sqr(self->m_vUVel.x) + sqr(self->m_vUVel.y));
}
// Calculates the Observer data for a given Satellite and returns m_vObs
static int CalculateObs(CSGP4_SDP4 *self, VECTOR pos, VECTOR vel, VECTOR geodetic, double time) {
// Procedure Calculate_Obs(pos,vel,geodetic : vector;
//                                    time : double;
//                             var obs_set : vector);
	int	visible;
	double	lat,lon,alt,theta,sin_lat,cos_lat,sin_theta,cos_theta;
	double	el,azim,top_s,top_e,top_z;
	VECTOR	range,rgvel;
	CalculateUserPosVel(self,&geodetic,time);	//,obs_pos,obs_vel);
	rgvel.x = vel.x - self->m_vUVel.x;
	rgvel.y = vel.y - self->m_vUVel.y;
	rgvel.z = vel.z - self->m_vUVel.z;
	range.x = pos.x - self->m_vUPos.x;
	range.y = pos.y - self->m_vUPos.y;
	range.z = pos.z - self->m_vUPos.z;
	range.w = sqrt(sqr(range.x) + sqr(range.y) + sqr(range.z));
	lat = geodetic.x;
	lon = geodetic.y;
	alt = geodetic.z;
	theta = geodetic.w;
	sin_lat = sin(lat);
	cos_lat = cos(lat);
	sin_theta = sin(theta);
	cos_theta = cos(theta);
	top_s = sin_lat*cos_theta*range.x + sin_lat*sin_theta*range.y - cos_lat*range.z;
	top_e = -sin_theta*range.x + cos_theta*range.y;
	top_z = cos_lat*cos_theta*range.x + cos_lat*sin_theta*range.y + sin_lat*range.z;
	azim = atan(-top_e/top_s);	//{Azimuth}
	if (top_s > 0.0)	azim += PI;
	if (azim < 0.0)	    azim += 2.0*PI;
	el = asin(top_z/range.w);
	self->m_vObs.x = azim;            //{Azimuth (radians)}
	self->m_vObs.y = el;				//{Elevation (radians)}
	self->m_vObs.z = range.w;			//{Range (kilometers)}
	self->m_vObs.w = range.x*rgvel.x + range.y*rgvel.y + range.z*rgvel.z;
	self->m_vObs.w /= range.w;	    //{Range Rate (kilometers/second)}
//{ Corrections for atmospheric refraction }
//{ Reference:  Astronomical Algorithms by Jean Meeus, pp. 101-104 }
//{ Note:  Correction is meaningless when apparent elevation is below horizon }
//	m_vObs.y += DegToRad((1.02/tan(DegToRad(RadToDeg(el)+10.3/(RadToDeg(el)+5.11))))/60.0);
	// Suggested check for 90 degrees limit from Dmitri Lisin 01/18/2003 (Izmiran : Russian academy of science)
	double fEle, fTan;
	fEle = RadToDeg(el)+5.11;
	if (fabs (fEle) < 0.0000000001)
		fEle = 0.0000000001;
	// Now we check that the variable for the tan function is not too close to 90.0 deg.
	fTan = fabs(DegToRad(RadToDeg(el)+10.3/fEle));
	while (fTan > 90.0) fTan -= 90.0;
	if (fabs(fTan-90.0) < 0.0000000001)
		fTan =  0.0000000001;
	else
		fTan =  0.0;
	self->m_vObs.y += DegToRad((1.02/tan(DegToRad(RadToDeg(el)+10.3/fEle))+fTan)/60.0);
	if (self->m_vObs.y >= 0)	visible = TRUE;
	else	{
		self->m_vObs.y = el;	//{Reset to true elevation}
		visible = FALSE;
	}
	return visible;
}
// Calculates the Observer data in topocentric data and return m_vRad
static void CalculateRADec(CSGP4_SDP4 *self, VECTOR pos, VECTOR vel, VECTOR geodetic, double time)
{
//Procedure Calculate_RADec(pos,vel,geodetic : vector;
//                                      time : double;
//                               var obs_set : vector);
//{ Reference:  Methods of Orbit Determination by Pedro Ramon Escobal, pp. 401-402}
	double	phi,theta,sin_theta,cos_theta,sin_phi,cos_phi,az,el,Lxh,Lyh,Lzh;
	double	Sx,Ex,Zx,Sy,Ey,Zy,Sz,Ez,Zz,Lx,Ly,Lz,cos_delta,sin_alpha,cos_alpha;
	self->m_vRad.x = 0.0; self->m_vRad.y = 0.0; self->m_vRad.z = 0.0; self->m_vRad.w = 0.0;
	int visible = CalculateObs(self,pos,vel,geodetic,time);	//,obs_set);
	if (visible)	{
		az = self->m_vObs.x;
		el = self->m_vObs.y;
//		az = m_vRad.x;
//		el = m_vRad.y;
		phi   = geodetic.x;
		theta = Modulus(ThetaG(time) + geodetic.y,2.0*PI);
		sin_theta = sin(theta);
		cos_theta = cos(theta);
		sin_phi = sin(phi);
		cos_phi = cos(phi);
		Lxh = -cos(az)*cos(el);
		Lyh =  sin(az)*cos(el);
		Lzh =  sin(el);
		Sx = sin_phi*cos_theta;
		Ex = -sin_theta;
		Zx = cos_theta*cos_phi;
		Sy = sin_phi*sin_theta;
		Ey = cos_theta;
		Zy = sin_theta*cos_phi;
		Sz = -cos_phi;
		Ez = 0.0;
		Zz = sin_phi;
		Lx = Sx*Lxh + Ex*Lyh + Zx*Lzh;
		Ly = Sy*Lxh + Ey*Lyh + Zy*Lzh;
		Lz = Sz*Lxh + Ez*Lyh + Zz*Lzh;
		self->m_vRad.y= asin(Lz);						//{Declination (radians)}
		cos_delta  = sqrt(1 - sqr(Lz));
		sin_alpha  = Ly/cos_delta;
		cos_alpha  = Lx/cos_delta;
		self->m_vRad.x = AcTan(sin_alpha,cos_alpha);	//{Right Ascension (radians)}
		self->m_vRad.x = Modulus(self->m_vRad.x,2.0*PI);
    }
}

/////////////////////////////////////////////////////////////////////////////////////
int SGP4(CSGP4_SDP4 *self, double tsince,int *iFlag, VECTOR *pos, VECTOR *vel)
{
//  label
//    9,10,90,100,110,130,140;
//  const
	static double a1		= 0.0;static double a3ovk2	= 0.0;static double ao		= 0.0;
	static double aodp		= 0.0;static double aycof	= 0.0;static double betao	= 0.0;
	static double betao2	= 0.0;static double c1		= 0.0;static double c1sq	= 0.0;
	static double c2		= 0.0;static double c3		= 0.0;static double c4		= 0.0;
	static double c5		= 0.0;static double coef	= 0.0;static double coef1	= 0.0;
	static double cosio		= 0.0;static double d2		= 0.0;static double d3		= 0.0;
	static double d4		= 0.0;static double del1	= 0.0;static double delmo	= 0.0;
	static double delo		= 0.0;static double eeta	= 0.0;static double eosq	= 0.0;
	static double eta		= 0.0;static double etasq	= 0.0;static int    isimp	= 0;
	static double omgcof	= 0.0;static double omgdot	= 0.0;static double perige	= 0.0;
	static double pinvsq	= 0.0;static double psisq	= 0.0;static double qoms24	= 0.0;
	static double s4		= 0.0;static double sinio	= 0.0;static double sinmo	= 0.0;
	static double t2cof		= 0.0;static double t3cof	= 0.0;static double t4cof	= 0.0;
	static double t5cof		= 0.0;static double temp	= 0.0;static double temp1	= 0.0;
	static double temp2		= 0.0;static double temp3	= 0.0;static double theta2	= 0.0;
	static double theta4	= 0.0;static double tsi		= 0.0;static double x1m5th	= 0.0;
	static double x1mth2	= 0.0;static double x3thm1	= 0.0;static double x7thm1	= 0.0;
	static double xhdot1	= 0.0;static double xlcof	= 0.0;static double xmcof	= 0.0;
	static double xmdot		= 0.0;static double xnodcf	= 0.0;static double xnodot	= 0.0;
	static double xnodp		= 0.0;
// var
    int i;
    double cosuk,sinuk,rfdotk,vx,vy,vz,ux,uy,uz,xmy,xmx;
    double cosnok,sinnok,cosik,sinik,rdotk,xinck,xnodek,uk,rk;
    double cos2u,sin2u,u,sinu,cosu,betal,rfdot,rdot,r,pl,elsq;
    double esine,ecose,epw,temp6,temp5,temp4,cosepw,sinepw;
    double capu,ayn,xlt,aynl,xll,axn,xn,beta,xl,e,a,tfour;
    double tcube,delm,delomg,templ,tempe,tempa,xnode,tsq,xmp;
    double omega,xnoddf,omgadf,xmdf,x,y,z,xdot,ydot,zdot;
	double ee;
// Recover original mean motion (xnodp) and semimajor axis (aodp) 
// from input elements. 
	int iflag = *iFlag;
  if (iflag == 0)
    goto SGP100;
  a1 = pow(xke/self->m_Sat.fMeanMotion,tothrd);
  cosio = cos(self->m_Sat.fInclination);
  theta2 = cosio*cosio;
  x3thm1 = 3.0*theta2 - 1.0;
  eosq = self->m_Sat.fEccentricity*self->m_Sat.fEccentricity;
  betao2 = 1.0 - eosq;
  betao = sqrt(betao2);
  del1 = 1.5*ck2*x3thm1/(a1*a1*betao*betao2);
  ao = a1*(1.0 - del1*(0.5*tothrd + del1*(1.0 + 134.0/81.0*del1)));
  delo = 1.5*ck2*x3thm1/(ao*ao*betao*betao2);
  xnodp = self->m_Sat.fMeanMotion/(1.0 + delo);
  aodp = ao/(1.0 - delo);
// Initialization 
// For perigee less than 220 kilometers, the isimp flag is set and
//  the equations are truncated to linear variation in sqrt a and
//  quadratic variation in mean anomaly.  Also, the c3 term, the
//  delta omega term, and the delta m term are dropped.
  isimp = 0;
  if ((aodp*(1.0 - self->m_Sat.fEccentricity)/ae) < (220.0/xkmper + ae))
    isimp = 1;
// For perigee below 156 km, the values of s and qoms2t are altered. 
  s4 = s;
  qoms24 = qoms2t;
  perige = xkmper*(aodp*(1.0 - self->m_Sat.fEccentricity) - ae);
  if (perige >= 156.0)
    goto SGP10;
  s4 = perige - 78.0;
  if (perige > 98.0)
    goto SGP9;
  s4 = 20.0;
SGP9:
  qoms24 = pow((120.0 - s4)*ae/xkmper,4.0);
  s4 = s4/xkmper + ae;
SGP10:
  pinvsq = 1.0/(aodp*aodp*betao2*betao2);
  tsi = 1.0/(aodp - s4);
  eta = aodp*self->m_Sat.fEccentricity*tsi;
  etasq = eta*eta;
  eeta = self->m_Sat.fEccentricity*eta;
  psisq = fabs(1.0 - etasq);
  coef = qoms24*pow(tsi,4.0);
  coef1 = coef/pow(psisq,3.5);
  c2 = coef1*xnodp*(aodp*(1.0 + 1.5*etasq + eeta*(4.0 + etasq))
      + 0.75*ck2*tsi/psisq*x3thm1*(8.0 + 3.0*etasq*(8.0 + etasq)));
  c1 = self->m_Sat.fRadiationCoefficient*c2;
  sinio = sin(self->m_Sat.fInclination);
  a3ovk2 = -xj3/ck2*pow(ae,3.0);
  c3 = coef*tsi*a3ovk2*xnodp*ae*sinio/self->m_Sat.fEccentricity;
  x1mth2 = 1.0 - theta2;
  c4 = 2.0*xnodp*coef1*aodp*betao2*(eta*(2.0 + 0.5*etasq)
      + self->m_Sat.fEccentricity*(0.5 + 2.0*etasq) - 2.0*ck2*tsi/(aodp*psisq)
      *(-3.0*x3thm1*(1.0 - 2.0*eeta + etasq*(1.5 - 0.5*eeta))
      + 0.75*x1mth2*(2.0*etasq - eeta*(1.0 + etasq))*cos(2.0*self->m_Sat.fPeregee)));
  c5 = 2.0*coef1*aodp*betao2*(1.0 + 2.75*(etasq + eeta) + eeta*etasq);
  theta4 = theta2*theta2;
  temp1 = 3.0*ck2*pinvsq*xnodp;
  temp2 = temp1*ck2*pinvsq;
  temp3 = 1.25*ck4*pinvsq*pinvsq*xnodp;
  xmdot = xnodp + 0.5*temp1*betao*x3thm1
         + 0.0625*temp2*betao*(13.0 - 78.0*theta2 + 137.0*theta4);
  x1m5th = 1.0 - 5.0*theta2;
  omgdot = -0.5*temp1*x1m5th + 0.0625*temp2*(7.0 - 114.0*theta2 +395.0*theta4)
          + temp3*(3.0 - 36.0*theta2 + 49.0*theta4);
  xhdot1 = -temp1*cosio;
  xnodot = xhdot1 + (0.5*temp2*(4.0 - 19.0*theta2)
          + 2.0*temp3*(3.0 - 7.0*theta2))*cosio;
  omgcof = self->m_Sat.fRadiationCoefficient*c3*cos(self->m_Sat.fPeregee);
  xmcof = -tothrd*coef*self->m_Sat.fRadiationCoefficient*ae/eeta;
  xnodcf = 3.5*betao2*xhdot1*c1;
  t2cof = 1.5*c1;
  xlcof = 0.125*a3ovk2*sinio*(3.0 + 5.0*cosio)/(1.0 + cosio);
  aycof = 0.25*a3ovk2*sinio;
  delmo = pow(1.0 + eta*cos(self->m_Sat.fMeanAnomaly),3.0);
  sinmo = sin(self->m_Sat.fMeanAnomaly);
  x7thm1 = 7.0*theta2 - 1.0;
  if (isimp == 1)
    goto SGP90;
  c1sq = c1*c1;
  d2 = 4.0*aodp*tsi*c1sq;
  temp = d2*tsi*c1/3.0;
  d3 = (17.0*aodp + s4)*temp;
  d4 = 0.5*temp*aodp*tsi*(221.0*aodp + 31.0*s4)*c1;
  t3cof = d2 + 2.0*c1sq;
  t4cof = 0.25*(3.0*d3 + c1*(12.0*d2 + 10.0*c1sq));
  t5cof = 0.2*(3.0*d4 + 12.0*c1*d3 + 6.0*d2*d2 + 15.0*c1sq*(2.0*d2 + c1sq));
SGP90:
  iflag = 0;
// Update for secular gravity and atmospheric drag. 
SGP100:
  xmdf = self->m_Sat.fMeanAnomaly + xmdot*tsince;
  omgadf = self->m_Sat.fPeregee + omgdot*tsince;
  xnoddf = self->m_Sat.fRightAscending + xnodot*tsince;
  omega = omgadf;
  xmp = xmdf;
  tsq = tsince*tsince;
  xnode = xnoddf + xnodcf*tsq;
  tempa = 1.0 - c1*tsince;
  tempe = self->m_Sat.fRadiationCoefficient*c4*tsince;
  templ = t2cof*tsq;
  if (isimp == 1)
    goto SGP110;
  delomg = omgcof*tsince;
  delm = xmcof*(pow(1.0 + eta*cos(xmdf),3.0) - delmo);
  temp = delomg + delm;
  xmp = xmdf + temp;
  omega = omgadf - temp;
  tcube = tsq*tsince;
  tfour = tsince*tcube;
  tempa = tempa - d2*tsq - d3*tcube - d4*tfour;
  tempe = tempe + self->m_Sat.fRadiationCoefficient*c5*(sin(xmp) - sinmo);
  templ = templ + t3cof*tcube + tfour*(t4cof + tsince*t5cof);
SGP110:
  a = aodp*sqr(tempa);
  e = self->m_Sat.fEccentricity - tempe;
  ee = e*e;
  if ( ee > 1.0) return FALSE;	// error, no good satellite datas ...
  xl = xmp + omega + xnode + xnodp*templ;
  beta = sqrt(1.0 - ee);
  xn = xke/pow(a,1.5);
// Long period periodics 
  axn = e*cos(omega);
  temp = 1.0/(a*beta*beta);
  xll = temp*xlcof*axn;
  aynl = temp*aycof;
  xlt = xl + xll;
  ayn = e*sin(omega) + aynl;
// Solve Kepler's Equation 
  capu = Fmod2p(xlt - xnode);
  temp2 = capu;
  for (i = 1;i <= 10;i++)
    {
    sinepw = sin(temp2);
    cosepw = cos(temp2);
    temp3 = axn*sinepw;
    temp4 = ayn*cosepw;
    temp5 = axn*cosepw;
    temp6 = ayn*sinepw;
    epw = (capu - temp4 + temp3 - temp2)/(1.0 - temp5 - temp6) + temp2;
    if (fabs(epw - temp2) <= e6a)
      goto SGP140;
// SGP130: Wird nicht angesprochen ...??? ...
    temp2 = epw;
    }; //for i
// Short period preliminary quantities 
SGP140:
  ecose = temp5 + temp6;
  esine = temp3 - temp4;
  elsq = axn*axn + ayn*ayn;
  temp = 1.0 - elsq;
  pl = a*temp;
  r = a*(1.0 - ecose);
  temp1 = 1.0/r;
  rdot = xke*sqrt(a)*esine*temp1;
  rfdot = xke*sqrt(pl)*temp1;
  temp2 = a*temp1;
  betal = sqrt(temp);
  temp3 = 1.0/(1.0 + betal);
  cosu = temp2*(cosepw - axn + ayn*esine*temp3);
  sinu = temp2*(sinepw - ayn - axn*esine*temp3);
  u = AcTan(sinu,cosu);
  sin2u = 2*sinu*cosu;
  cos2u = 2*cosu*cosu - 1.0;
  temp = 1.0/pl;
  temp1 = ck2*temp;
  temp2 = temp1*temp;
// Update for short periodics 
  rk = r*(1.0 - 1.5*temp2*betal*x3thm1) + 0.5*temp1*x1mth2*cos2u;
  uk = u - 0.25*temp2*x7thm1*sin2u;
  xnodek = xnode + 1.5*temp2*cosio*sin2u;
  xinck = self->m_Sat.fInclination + 1.5*temp2*cosio*sinio*cos2u;
  rdotk = rdot - xn*temp1*x1mth2*sin2u;
  rfdotk = rfdot + xn*temp1*(x1mth2*cos2u + 1.5*x3thm1);
// Orientation vectors 
  sinuk = sin(uk);
  cosuk = cos(uk);
  sinik = sin(xinck);
  cosik = cos(xinck);
  sinnok = sin(xnodek);
  cosnok = cos(xnodek);
  xmx = -sinnok*cosik;
  xmy = cosnok*cosik;
  ux = xmx*sinuk + cosnok*cosuk;
  uy = xmy*sinuk + sinnok*cosuk;
  uz = sinik*sinuk;
  vx = xmx*cosuk - cosnok*sinuk;
  vy = xmy*cosuk - sinnok*sinuk;
  vz = sinik*cosuk;
// Position and velocity 
  x = rk*ux;  pos->x = x;
  y = rk*uy;  pos->y = y;
  z = rk*uz;  pos->z = z;
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
  xdot = rdotk*ux + rfdotk*vx;  vel->x = xdot;
  ydot = rdotk*uy + rfdotk*vy;  vel->y = ydot;
  zdot = rdotk*uz + rfdotk*vz;  vel->z = zdot;
  return TRUE; 
}; //Procedure SGP4

static int Deep(CSGP4_SDP4 *self, int ideep)
{
//  const
    static double zns    =  1.19459E-5;     static double c1ss   =  2.9864797E-6;   static double zes    =  0.01675;
    static double znl    =  1.5835218E-4;   static double c1l    =  4.7968065E-7;   static double zel    =  0.05490;
    static double zcosis =  0.91744867;     static double zsinis =  0.39785416;     static double zsings = -0.98088458;
    static double zcosgs =  0.1945905;      //static double zcoshs =  1;              static double zsinhs =  0;
    static double q22    =  1.7891679E-6;   static double q31    =  2.1460748E-6;   static double q33    =  2.2123015E-7;
    static double g22    =  5.7686396;      static double g32    =  0.95240898;     static double g44    =  1.8014998;
    static double g52    =  1.0508330;      static double g54    =  4.4108898;      static double root22 =  1.7891679E-6;
    static double root32 =  3.7393792E-7;   static double root44 =  7.3636953E-9;   static double root52 =  1.1428639E-7;
    static double root54 =  2.1765803E-9;   static double thdt   =  4.3752691E-3;
//  label
    //dpinit
//    5,10,20,30,40,45,50,55,60,65,70,80,
    //dpsec
//    90,100,105,110,120,125,130,140,150,152,154,160,165,170,175,180,
    //dpper
//    205,210,218,220,230;
//const  Typed constants to retain values between ENTRY calls 
    static int iresfl	= 0;static int isynfl	= 0;
    static int iret		= 0;static int iretn	= 0;
    static int ls		= 0;
    static double a1      = 0.0;static double    a2      = 0.0;static double    a3      = 0.0;
    static double a4      = 0.0;static double    a5      = 0.0;static double    a6      = 0.0;
    static double a7      = 0.0;static double    a8      = 0.0;static double    a9      = 0.0;
    static double a10     = 0.0;static double    ainv2   = 0.0;static double    alfdp   = 0.0;
    static double aqnv    = 0.0;static double    atime   = 0.0;static double    betdp   = 0.0;
    static double bfact   = 0.0;static double    c       = 0.0;static double    cc      = 0.0;
    static double cosis   = 0.0;static double    cosok   = 0.0;static double    cosq    = 0.0;
    static double ctem    = 0.0;static double    d2201   = 0.0;static double    d2211   = 0.0;
    static double d3210   = 0.0;static double    d3222   = 0.0;static double    d4410   = 0.0;
    static double d4422   = 0.0;static double    d5220   = 0.0;static double    d5232   = 0.0;
    static double d5421   = 0.0;static double    d5433   = 0.0;static double    dalf    = 0.0;
    static double day     = 0.0;static double    dbet    = 0.0;static double    del1    = 0.0;
    static double del2    = 0.0;static double    del3    = 0.0;static double    delt    = 0.0;
    static double dls     = 0.0;static double    e3      = 0.0;static double    ee2     = 0.0;
    static double eoc     = 0.0;static double    eq      = 0.0;static double    f2      = 0.0;
    static double f220    = 0.0;static double    f221    = 0.0;static double    f3      = 0.0;
    static double f311    = 0.0;static double    f321    = 0.0;static double    f322    = 0.0;
    static double f330    = 0.0;static double    f441    = 0.0;static double    f442    = 0.0;
    static double f522    = 0.0;static double    f523    = 0.0;static double    f542    = 0.0;
    static double f543    = 0.0;static double    fasx2   = 0.0;static double    fasx4   = 0.0;
    static double fasx6   = 0.0;static double    ft      = 0.0;static double    g200    = 0.0;
    static double g201    = 0.0;static double    g211    = 0.0;static double    g300    = 0.0;
    static double g310    = 0.0;static double    g322    = 0.0;static double    g410    = 0.0;
    static double g422    = 0.0;static double    g520    = 0.0;static double    g521    = 0.0;
    static double g532    = 0.0;static double    g533    = 0.0;static double    gam     = 0.0;
    static double omegaq  = 0.0;static double    pe      = 0.0;static double    pgh     = 0.0;
    static double ph      = 0.0;static double    pinc    = 0.0;static double    pl      = 0.0;
    static double preep   = 0.0;static double    s1      = 0.0;static double    s2      = 0.0;
    static double s3      = 0.0;static double    s4      = 0.0;static double    s5      = 0.0;
    static double s6      = 0.0;static double    s7      = 0.0;static double    savtsn  = 0.0;
    static double se      = 0.0;static double    se2     = 0.0;static double    se3     = 0.0;
    static double sel     = 0.0;static double    ses     = 0.0;static double    sgh     = 0.0;
    static double sgh2    = 0.0;static double    sgh3    = 0.0;static double    sgh4    = 0.0;
    static double sghl    = 0.0;static double    sghs    = 0.0;static double    sh      = 0.0;
    static double sh2     = 0.0;static double    sh3     = 0.0;static double    sh1     = 0.0;
    static double shs     = 0.0;static double    si      = 0.0;static double    si2     = 0.0;
    static double si3     = 0.0;static double    sil     = 0.0;static double    sini2   = 0.0;
    static double sinis   = 0.0;static double    sinok   = 0.0;static double    sinq    = 0.0;
    static double sinzf   = 0.0;static double    sis     = 0.0;static double    sl      = 0.0;
    static double sl2     = 0.0;static double    sl3     = 0.0;static double    sl4     = 0.0;
    static double sll     = 0.0;static double    sls     = 0.0;static double    sse     = 0.0;
    static double ssg     = 0.0;static double    ssh     = 0.0;static double    ssi     = 0.0;
    static double ssl     = 0.0;static double    stem    = 0.0;static double    step2   = 0.0;
    static double stepn   = 0.0;static double    stepp   = 0.0;static double    temp    = 0.0;
    static double temp1   = 0.0;static double    thgr    = 0.0;static double    x1      = 0.0;
    static double x2      = 0.0;static double    x2li    = 0.0;static double    x2omi   = 0.0;
    static double x3      = 0.0;static double    x4      = 0.0;static double    x5      = 0.0;
    static double x6      = 0.0;static double    x7      = 0.0;static double    x8      = 0.0;
    static double xfact   = 0.0;static double    xgh2    = 0.0;static double    xgh3    = 0.0;
    static double xgh4    = 0.0;static double    xh2     = 0.0;static double    xh3     = 0.0;
    static double xi2     = 0.0;static double    xi3     = 0.0;static double    xl      = 0.0;
    static double xl2     = 0.0;static double    xl3     = 0.0;static double    xl4     = 0.0;
    static double xlamo   = 0.0;static double    xldot   = 0.0;static double    xli     = 0.0;
    static double xls     = 0.0;static double    xmao    = 0.0;static double    xnddt   = 0.0;
    static double xndot   = 0.0;static double    xni     = 0.0;static double    xno2    = 0.0;
    static double xnodce  = 0.0;static double    xnoi    = 0.0;static double    xnq     = 0.0;
    static double xomi    = 0.0;static double    xpidot  = 0.0;static double    xqncl   = 0.0;
    static double z1      = 0.0;static double    z11     = 0.0;static double    z12     = 0.0;
    static double z13     = 0.0;static double    z2      = 0.0;static double    z21     = 0.0;
    static double z22     = 0.0;static double    z23     = 0.0;static double    z3      = 0.0;
    static double z31     = 0.0;static double    z32     = 0.0;static double    z33     = 0.0;
    static double zcosg   = 0.0;static double    zcosgl  = 0.0;static double    zcosh   = 0.0;
    static double zcoshl  = 0.0;static double    zcosi   = 0.0;static double    zcosil  = 0.0;
    static double ze      = 0.0;static double    zf      = 0.0;static double    zm      = 0.0;
    static double zmo     = 0.0;static double    zmol    = 0.0;static double    zmos    = 0.0;
    static double zn      = 0.0;static double    zsing   = 0.0;static double    zsingl  = 0.0;
    static double zsinh   = 0.0;static double    zsinhl  = 0.0;static double    zsini   = 0.0;
    static double zsinil  = 0.0;static double    zx      = 0.0;static double    zy      = 0.0;

//	switch (ideep)	{
    if (ideep == dpinit) { // Entrance for deep space initialization 
             thgr = ThetaG(self->m_Sat.fJulianEpoch);
			 eq = self->m_Sat.fEccentricity;
             xnq = xnodp;
             aqnv = 1.0/ao;
             xqncl = self->m_Sat.fInclination;
             xmao = self->m_Sat.fMeanAnomaly;
             xpidot = omgdt + xnodot;
             sinq = sin(self->m_Sat.fRightAscending);
             cosq = cos(self->m_Sat.fRightAscending);
             omegaq = self->m_Sat.fPeregee;
    // Initialize lunar solar terms 
//DEEP5: Unused ...
			 day = self->m_Sat.fJulianEpoch - 2433281.5 + 18261.5;  //Days since 1900 Jan 0.5
             if (day == preep)
               goto DEEP10;
             preep = day;
             xnodce = 4.5236020 - 9.2422029E-4*day;
             stem = sin(xnodce);
             ctem = cos(xnodce);
             zcosil = 0.91375164 - 0.03568096*ctem;
             zsinil = sqrt(1.0 - zcosil*zcosil);
             zsinhl = 0.089683511*stem/zsinil;
             zcoshl = sqrt(1.0 - zsinhl*zsinhl);
             c = 4.7199672 + 0.22997150*day;
             gam = 5.8351514 + 0.0019443680*day;
             zmol = Fmod2p(c - gam);
             zx = 0.39785416*stem/zsinil;
             zy = zcoshl*ctem + 0.91744867*zsinhl*stem;
             zx = AcTan(zx,zy);
             zx = gam + zx - xnodce;
             zcosgl = cos(zx);
             zsingl = sin(zx);
             zmos = 6.2565837 + 0.017201977*day;
             zmos = Fmod2p(zmos);
    // Do solar terms 
DEEP10:		 savtsn = 1E20;
             zcosg = zcosgs;
             zsing = zsings;
             zcosi = zcosis;
             zsini = zsinis;
             zcosh = cosq;
             zsinh = sinq;
             cc = c1ss;
             zn = zns;
             ze = zes;
             zmo = zmos;
             xnoi = 1.0/xnq;
             ls = 30; //assign 30 to ls
DEEP20:		 a1 = zcosg*zcosh + zsing*zcosi*zsinh;
             a3 = -zsing*zcosh + zcosg*zcosi*zsinh;
             a7 = -zcosg*zsinh + zsing*zcosi*zcosh;
             a8 = zsing*zsini;
             a9 = zsing*zsinh + zcosg*zcosi*zcosh;
             a10 = zcosg*zsini;
             a2 = cosiq*a7 +  siniq*a8;
             a4 = cosiq*a9 +  siniq*a10;
             a5 = -siniq*a7 +  cosiq*a8;
             a6 = -siniq*a9 +  cosiq*a10;
             x1 = a1*cosomo + a2*sinomo;
             x2 = a3*cosomo + a4*sinomo;
             x3 = -a1*sinomo + a2*cosomo;
             x4 = -a3*sinomo + a4*cosomo;
             x5 = a5*sinomo;
             x6 = a6*sinomo;
             x7 = a5*cosomo;
             x8 = a6*cosomo;
             z31 = 12.0*x1*x1 - 3.0*x3*x3;
             z32 = 24.0*x1*x2 - 6.0*x3*x4;
             z33 = 12.0*x2*x2 - 3.0*x4*x4;
             z1 = 3.0*(a1*a1 + a2*a2) + z31*eqsq;
             z2 = 6.0*(a1*a3 + a2*a4) + z32*eqsq;
             z3 = 3.0*(a3*a3 + a4*a4) + z33*eqsq;
             z11 = -6.0*a1*a5 + eqsq*(-24.0*x1*x7 - 6.0*x3*x5);
             z12 = -6.0*(a1*a6 + a3*a5)
                  + eqsq*(-24.0*(x2*x7 + x1*x8) - 6.0*(x3*x6 + x4*x5));
             z13 = -6.0*a3*a6 + eqsq*(-24.0*x2*x8 - 6.0*x4*x6);
             z21 = 6.0*a2*a5 + eqsq*(24.0*x1*x5 - 6.0*x3*x7);
             z22 = 6.0*(a4*a5 + a2*a6)
                  + eqsq*(24.0*(x2*x5 + x1*x6) - 6.0*(x4*x7 + x3*x8));
             z23 = 6.0*a4*a6 + eqsq*(24.0*x2*x6 - 6.0*x4*x8);
             z1 = z1 + z1 + bsq*z31;
             z2 = z2 + z2 + bsq*z32;
             z3 = z3 + z3 + bsq*z33;
             s3 = cc*xnoi;
             s2 = -0.5*s3/rteqsq;
             s4 = s3*rteqsq;
             s1 = -15.0*eq*s4;
             s5 = x1*x3 + x2*x4;
             s6 = x2*x3 + x1*x4;
             s7 = x2*x4 - x1*x3;
             se = s1*zn*s5;
             si = s2*zn*(z11 + z13);
             sl = -zn*s3*(z1 + z3 - 14.0 - 6.0*eqsq);
             sgh = s4*zn*(z31 + z33 - 6.0);
             sh = -zn*s2*(z21 + z23);
             if (xqncl < 5.2359877E-2)
               sh = 0.0;
             ee2 = 2.0*s1*s6;
             e3 = 2.0*s1*s7;
             xi2 = 2.0*s2*z12;
             xi3 = 2.0*s2*(z13 - z11);
             xl2 = -2.0*s3*z2;
             xl3 = -2.0*s3*(z3 - z1);
             xl4 = -2.0*s3*(-21.0 - 9.0*eqsq)*ze;
             xgh2 = 2.0*s4*z32;
             xgh3 = 2.0*s4*(z33 - z31);
             xgh4 = -18.0*s4*ze;
             xh2 = -2.0*s2*z22;
             xh3 = -2.0*s2*(z23 - z21);
             if (ls == 30) goto DEEP30;
             else if (ls == 40) goto DEEP40;
             else	return TRUE;
    // Do lunar terms 
DEEP30:		 sse = se;
             ssi = si;
             ssl = sl;
             ssh = sh/siniq;
             ssg = sgh - cosiq*ssh;
             se2 = ee2;
             si2 = xi2;
             sl2 = xl2;
             sgh2 = xgh2;
             sh2 = xh2;
             se3 = e3;
             si3 = xi3;
             sl3 = xl3;
             sgh3 = xgh3;
             sh3 = xh3;
             sl4 = xl4; 
             sgh4 = xgh4;
             zcosg = zcosgl;
             zsing = zsingl;
             zcosi = zcosil;
             zsini = zsinil;
             zcosh = zcoshl*cosq + zsinhl*sinq;
             zsinh = sinq*zcoshl - cosq*zsinhl;
             zn = znl;
             cc = c1l;
             ze = zel;
             zmo = zmol;
             ls = 40; //assign 40 to ls
             goto DEEP20;
DEEP40:		 sse = sse + se;
             ssi = ssi + si;
             ssl = ssl + sl;
             ssg = ssg + sgh - cosiq/siniq*sh;
             ssh = ssh + sh/siniq;
    // Geopotential resonance initialization for 12 hour orbits 
             iresfl = 0;
             isynfl = 0;
             if ((xnq < 0.0052359877) && (xnq > 0.0034906585))
               goto DEEP70;
             if ((xnq < 8.26E-3) || (xnq > 9.24E-3))
               return TRUE;
             if (eq < 0.5)
               return TRUE;
             iresfl = 1;
             eoc = eq*eqsq;
             g201 = -0.306 - (eq - 0.64)*0.440;
             if (eq > 0.65)
               goto DEEP45;
             g211 = 3.616 - 13.247*eq + 16.290*eqsq;
             g310 = -19.302 + 117.390*eq - 228.419*eqsq + 156.591*eoc;
             g322 = -18.9068 + 109.7927*eq - 214.6334*eqsq + 146.5816*eoc;
             g410 = -41.122 + 242.694*eq - 471.094*eqsq + 313.953*eoc;
             g422 = -146.407 + 841.880*eq - 1629.014*eqsq + 1083.435*eoc;
             g520 = -532.114 + 3017.977*eq - 5740.0*eqsq + 3708.276*eoc;
             goto DEEP55;
DEEP45:		 g211 = -72.099 + 331.819*eq - 508.738*eqsq + 266.724*eoc;
             g310 = -346.844 + 1582.851*eq - 2415.925*eqsq + 1246.113*eoc;
             g322 = -342.585 + 1554.908*eq - 2366.899*eqsq + 1215.972*eoc;
             g410 = -1052.797 + 4758.686*eq - 7193.992*eqsq + 3651.957*eoc;
             g422 = -3581.69 + 16178.11*eq - 24462.77*eqsq + 12422.52*eoc;
             if (eq > 0.715)
               goto DEEP50;
             g520 = 1464.74 - 4664.75*eq + 3763.64*eqsq;
             goto DEEP55;
DEEP50:		 g520 = -5149.66 + 29936.92*eq - 54087.36*eqsq + 31324.56*eoc;
DEEP55:		 if (eq >= (0.7))
               goto DEEP60;
             g533 = -919.2277 + 4988.61*eq - 9064.77*eqsq + 5542.21*eoc;
             g521 = -822.71072 + 4568.6173*eq - 8491.4146*eqsq + 5337.524*eoc;
             g532 = -853.666 + 4690.25*eq - 8624.77*eqsq + 5341.4*eoc;
               goto DEEP65;
DEEP60:		 g533 = -37995.78 + 161616.52*eq - 229838.2*eqsq + 109377.94*eoc;
             g521 = -51752.104 + 218913.95*eq - 309468.16*eqsq + 146349.42*eoc;
             g532 = -40023.88 + 170470.89*eq - 242699.48*eqsq + 115605.82*eoc;
DEEP65:		 sini2 = siniq*siniq;
             f220 = 0.75*(1.0 + 2.0*cosiq + cosq2);
             f221 = 1.5*sini2;
             f321 = 1.875*siniq*(1.0 - 2.0*cosiq - 3.0*cosq2);
             f322 = -1.875*siniq*(1.0 + 2.0*cosiq - 3.0*cosq2);
             f441 = 35.0*sini2*f220;
             f442 = 39.3750*sini2*sini2;
             f522 = 9.84375*siniq*(sini2*(1.0 - 2.0*cosiq - 5.0*cosq2)
                   + 0.33333333*(-2.0 + 4.0*cosiq + 6.0*cosq2));
             f523 = siniq*(4.92187512*sini2*(-2.0 - 4.0*cosiq + 10.0*cosq2)
                   + 6.56250012*(1.0 + 2.0*cosiq - 3.0*cosq2));
             f542 = 29.53125*siniq*(2.0 - 8.0*cosiq + cosq2*(-12.0 + 8.0*cosiq + 10.0*cosq2));
             f543 = 29.53125*siniq*(-2.0 - 8.0*cosiq + cosq2*(12.0 + 8.0*cosiq - 10.0*cosq2));
             xno2 = xnq*xnq;
             ainv2 = aqnv*aqnv;
             temp1 = 3.0*xno2*ainv2;
             temp = temp1*root22;
             d2201 = temp*f220*g201;
             d2211 = temp*f221*g211;
             temp1 = temp1*aqnv;
             temp = temp1*root32;
             d3210 = temp*f321*g310;
             d3222 = temp*f322*g322;
             temp1 = temp1*aqnv;
             temp = 2.0*temp1*root44;
             d4410 = temp*f441*g410;
             d4422 = temp*f442*g422;
             temp1 = temp1*aqnv;
             temp = temp1*root52;
             d5220 = temp*f522*g520;
             d5232 = temp*f523*g532;
             temp = 2.0*temp1*root54;
             d5421 = temp*f542*g521;
             d5433 = temp*f543*g533;
             xlamo = xmao + self->m_Sat.fRightAscending + self->m_Sat.fRightAscending - thgr - thgr;
             bfact = xlldot + xnodot + xnodot - thdt - thdt;
             bfact = bfact + ssl + ssh + ssh;
               goto DEEP80;
    // Synchronous resonance terms initialization 
DEEP70:		 iresfl = 1;
             isynfl = 1;
             g200 = 1.0 + eqsq*(-2.5 + 0.8125*eqsq);
             g310 = 1.0 + 2.0*eqsq;
             g300 = 1.0 + eqsq*(-6.0 + 6.60937*eqsq);
             f220 = 0.75*(1.0 + cosiq)*(1.0 + cosiq);
             f311 = 0.9375*siniq*siniq*(1.0 + 3*cosiq) - 0.75*(1.0 + cosiq);
             f330 = 1.0 + cosiq;
             f330 = 1.875*f330*f330*f330;
             del1 = 3.0*xnq*xnq*aqnv*aqnv;
             del2 = 2.0*del1*f220*g200*q22;
             del3 = 3.0*del1*f330*g300*q33*aqnv;
             del1 = del1*f311*g310*q31*aqnv;
             fasx2 = 0.13130908;
             fasx4 = 2.8843198;
             fasx6 = 0.37448087;
             xlamo = xmao + self->m_Sat.fRightAscending + self->m_Sat.fPeregee - thgr;
             bfact = xlldot + xpidot - thdt;
             bfact = bfact + ssl + ssg + ssh;
DEEP80:		 xfact = bfact - xnq;
    // Initialize integrator 
             xli = xlamo;
             xni = xnq;
             atime = 0.0;
             stepp = 720.0;
             stepn = -720.0;
             step2 = 259200.0;
             } //dpinit
	else if (ideep == dpsec) { // Entrance for deep space secular effects 
             xll = xll + ssl*t;
             omgasm = omgasm + ssg*t;
             xnodes = xnodes + ssh*t;
             _em = self->m_Sat.fEccentricity + sse*t;
             xinc = self->m_Sat.fInclination + ssi*t;
             if (xinc >= 0.0)
               goto DEEP90;
             xinc = -xinc;
             xnodes = xnodes  +  PI;
             omgasm = omgasm - PI;
DEEP90:		 if (iresfl == 0) 
               return TRUE;
DEEP100:	 if (atime == 0.0)
               goto DEEP170;
             if ((t >= 0.0) && (atime < 0.0))
               goto DEEP170;
             if ((t < 0.0) && (atime >= 0.0))
               goto DEEP170;
//DEEP105:	 Unused ...
			 if (fabs(t) >= fabs(atime))
               goto DEEP120;
             delt = stepp;
             if (t >= 0.0)
               delt = stepn;
//DEEP110:	 Unused ...
			 iret = 100; //assign 100 to iret
             goto DEEP160;
DEEP120:	 delt = stepn;
             if (t > 0.0)
               delt = stepp;
DEEP125:	 if (fabs(t - atime) < stepp)
               goto DEEP130;
             iret = 125; //assign 125 to iret
             goto DEEP160;
DEEP130:	 ft = t - atime;
             iretn = 140; //assign 140 to iretn
             goto DEEP150;
DEEP140:	 xn = xni + xndot*ft + xnddt*ft*ft*0.5;
             xl = xli + xldot*ft + xndot*ft*ft*0.5;
             temp = -xnodes + thgr + t*thdt;
             xll = xl - omgasm + temp;
             if (isynfl == 0)
               xll = xl + temp + temp;
             return TRUE;
    // Dot terms calculated 
DEEP150:	 if (isynfl == 0)
               goto DEEP152;
             xndot = del1*sin(xli - fasx2) + del2*sin(2.0*(xli - fasx4))
                    + del3*sin(3.0*(xli - fasx6));
             xnddt = del1*cos(xli - fasx2)
                    + 2.0*del2*cos(2.0*(xli - fasx4))
                    + 3.0*del3*cos(3.0*(xli - fasx6));
             goto DEEP154;
DEEP152:	 xomi = omegaq + omgdt*atime;
             x2omi = xomi + xomi;
             x2li = xli + xli;
             xndot = d2201*sin(x2omi + xli - g22)
                    + d2211*sin(xli - g22)
                    + d3210*sin(xomi + xli - g32)
                    + d3222*sin(-xomi + xli - g32)
                    + d4410*sin(x2omi + x2li - g44)
                    + d4422*sin(x2li - g44)
                    + d5220*sin(xomi + xli - g52)
                    + d5232*sin(-xomi + xli - g52)
                    + d5421*sin(xomi + x2li - g54)
                    + d5433*sin(-xomi + x2li - g54);
             xnddt = d2201*cos(x2omi + xli - g22)
                    + d2211*cos(xli - g22)
                    + d3210*cos(xomi + xli - g32)
                    + d3222*cos(-xomi + xli - g32)
                    + d5220*cos(xomi + xli - g52)
                    + d5232*cos(-xomi + xli - g52)
                    + 2.0*(d4410*cos(x2omi + x2li - g44)
                    + d4422*cos(x2li - g44)
                    + d5421*cos(xomi + x2li - g54)
                    + d5433*cos(-xomi + x2li - g54));
DEEP154:	 xldot = xni + xfact;
             xnddt = xnddt*xldot;
               if (iretn == 140) goto DEEP140;
               else if (iretn == 165) goto DEEP165;
			   else return TRUE;
//             } //case
    // Integrator 
DEEP160:	 iretn = 165; //assign 165 to iretn
             goto DEEP150;
DEEP165:	 xli = xli + xldot*delt + xndot*step2;
             xni = xni + xndot*delt + xnddt*step2;
             atime = atime + delt;
             if(iret == 100) goto DEEP100;
             else if(iret == 125) goto DEEP125;
             else return TRUE;
//             }; //case
    // Epoch restart 
DEEP170:	 if (t >= 0)
               goto DEEP175;
             delt = stepn;
             goto DEEP180;
DEEP175:	 delt = stepp;
DEEP180:	 atime = 0.0;
             xni = xnq;
             xli = xlamo;
             goto DEEP125;
             } //dpsec
    else if(ideep ==  dpper) { // Entrance for lunar-solar periodics 
             sinis = sin(xinc);
             cosis = cos(xinc);
             if (fabs(savtsn - t) < 30.0)
               goto DEEP210;
             savtsn = t;
             zm = zmos + zns*t;
//DEEP205:	 Unused ...
			 zf = zm + 2.0*zes*sin(zm);
             sinzf = sin(zf);
             f2 = 0.5*sinzf*sinzf - 0.25;
             f3 = -0.5*sinzf*cos(zf);
             ses = se2*f2 + se3*f3;
             sis = si2*f2 + si3*f3;
             sls = sl2*f2 + sl3*f3 + sl4*sinzf;
             sghs = sgh2*f2 + sgh3*f3 + sgh4*sinzf;
             shs = sh2*f2 + sh3*f3;
             zm = zmol + znl*t;
             zf = zm + 2.0*zel*sin(zm);
             sinzf = sin(zf);
             f2 = 0.5*sinzf*sinzf - 0.25;
             f3 = -0.5*sinzf*cos(zf);
             sel = ee2*f2 + e3*f3;
             sil = xi2*f2 + xi3*f3;
             sll = xl2*f2 + xl3*f3 + xl4*sinzf;
             sghl = xgh2*f2 + xgh3*f3 + xgh4*sinzf;
             sh1 = xh2*f2 + xh3*f3;
             pe = ses + sel;
             pinc = sis + sil;
             pl = sls + sll;
DEEP210:	 pgh = sghs + sghl;
             ph = shs + sh1;
             xinc = xinc + pinc;
             _em = _em + pe;
             if (xqncl < 0.2)
               goto DEEP220;
             goto DEEP218;
    // Apply periodics directly 
DEEP218:	 ph = ph/siniq;
             pgh = pgh - cosiq*ph;
             omgasm = omgasm + pgh;
             xnodes = xnodes + ph;
             xll = xll + pl;
             goto DEEP230;
    // Apply periodics with Lyddane modification 
DEEP220:	 sinok = sin(xnodes);
             cosok = cos(xnodes);
             alfdp = sinis*sinok;
             betdp = sinis*cosok;
             dalf = ph*cosok + pinc*cosis*sinok;
             dbet = -ph*sinok + pinc*cosis*cosok;
             alfdp = alfdp + dalf;
             betdp = betdp + dbet;
             xls = xll + omgasm + cosis*xnodes;
             dls = pl + pgh - pinc*xnodes*sinis;
             xls = xls + dls;
             xnodes = AcTan(alfdp,betdp);
             xll = xll + pl;
             omgasm = xls - xll - cos(xinc)*xnodes;
DEEP230: ;	 }; //dpper
//    } //case
 return TRUE;
}; //Procedure Deep

static void Call_dpinit(CSGP4_SDP4 *self,
                 double *eosq,double *sinio,double *cosio,double *betao,double *aodp,
                 double *theta2,double *sing,double *cosg,double *betao2,double *xmdot,
                 double *omgdot,double *xnodott,double *xnodpp)
{
  eqsq   = *eosq;    siniq  = *sinio;   cosiq  = *cosio;   rteqsq = *betao;
  ao     = *aodp;    cosq2  = *theta2;  sinomo = *sing;    cosomo = *cosg;
  bsq    = *betao2;  xlldot = *xmdot;   omgdt  = *omgdot;  xnodot = *xnodott;
  xnodp  = *xnodpp;
  Deep(self,1);
  *eosq   = eqsq;    *sinio  = siniq;   *cosio  = cosiq;   *betao  = rteqsq;
  *aodp   = ao;      *theta2 = cosq2;   *sing   = sinomo;  *cosg   = cosomo;
  *betao2 = bsq;     *xmdot  = xlldot;  *omgdot = omgdt;   *xnodott = xnodot;
  *xnodpp  = xnodp;
}; //Procedure Call_dpinit

static void Call_dpsec(CSGP4_SDP4 *self,
                double *xmdf,double *omgadf,double *xnode,double *emm,double *xincc,
                double *xnn,double *tsince)
{

  xll    = *xmdf;    omgasm = *omgadf;  xnodes = *xnode;   /*_em     = emm;
  *xinc   = *xincc;*/   xn     = *xnn;  t      = *tsince;
  Deep(self,2);
  *xmdf   = xll;     *omgadf = omgasm;  *xnode  = xnodes;  *emm    = _em;
  *xincc  = xinc;    *xnn    = xn;      *tsince = t;
}; // Procedure Call_dpsec 

static void Call_dpper(CSGP4_SDP4 *self,double *e,double *xincc,double *omgadf,double *xnode,double *xmam)
{
  _em     = *e;       xinc   = *xincc;   omgasm = *omgadf;  xnodes = *xnode;
  xll    = *xmam;
  Deep(self,3);
  *e      = _em;      *xincc  = xinc;    *omgadf = omgasm;  *xnode  = xnodes;
  *xmam   = xll;
}; //Procedure Call_dpper

int SDP4(CSGP4_SDP4 *self, double tsince, int *iFlag, VECTOR *pos, VECTOR *vel)
{
//  label
//    9,10,90,100,130,140;
//  const
    static double a1       = 0.0;static double   a3ovk2   = 0.0;static double   ao       = 0.0;
    static double aodp     = 0.0;static double   aycof    = 0.0;static double   betao    = 0.0;
    static double betao2   = 0.0;static double   c1       = 0.0;static double   c2       = 0.0;
    static double c4       = 0.0;static double   coef     = 0.0;static double   coef1    = 0.0;
    static double cosg     = 0.0;static double   cosio    = 0.0;static double   del1     = 0.0;
    static double delo     = 0.0;static double   eeta     = 0.0;static double   eosq     = 0.0;
    static double eta      = 0.0;static double   etasq    = 0.0;static double   omgdot   = 0.0;
    static double perige   = 0.0;static double   pinvsq   = 0.0;static double   psisq    = 0.0;
    static double qoms24   = 0.0;static double   s4       = 0.0;static double   sing     = 0.0;
    static double sinio    = 0.0;static double   t2cof    = 0.0;static double   temp1    = 0.0;
    static double temp2    = 0.0;static double   temp3    = 0.0;static double   theta2   = 0.0;
    static double theta4   = 0.0;static double   tsi      = 0.0;static double   x1m5th   = 0.0;
    static double x1mth2   = 0.0;static double   x3thm1   = 0.0;static double   x7thm1   = 0.0;
    static double xhdot1   = 0.0;static double   xlcof    = 0.0;static double   xmdot    = 0.0;
    static double xnodcf   = 0.0;static double   xnodot   = 0.0;static double   xnodp    = 0.0;
//  var
    int i;
    double a,axn,ayn,aynl,beta,betal,capu,cos2u,cosepw,cosik;
    double cosnok,cosu,cosuk,e,ecose,elsq,em,epw,esine,omgadf;
    double pl,r,rdot,rdotk,rfdot,rfdotk,rk,sin2u,sinepw,sinik;
    double sinnok,sinu,sinuk,temp,temp4,temp5,temp6,tempa;
    double tempe,templ,tsq,u,uk,ux,uy,uz,vx,vy,vz,xinc,xinck;
    double xl,xll,xlt,xmam,xmdf,xmx,xmy,xn,xnoddf,xnode,xnodek;
    double x,y,z,xdot,ydot,zdot;
	double ee;
	int iflag = *iFlag;
  if (iflag == 0)
    goto SDP100;
// Recover original mean motion (xnodp) and semimajor axis (aodp) 
// from input elements. 
  a1 = pow(xke/self->m_Sat.fMeanMotion,tothrd);
  cosio = cos(self->m_Sat.fInclination);
  theta2 = cosio*cosio;
  x3thm1 = 3.0*theta2 - 1.0;
  eosq = self->m_Sat.fEccentricity*self->m_Sat.fEccentricity;
  betao2 = 1.0 - eosq;
  betao = sqrt(betao2);
  del1 = 1.5*ck2*x3thm1/(a1*a1*betao*betao2);
  ao = a1*(1.0 - del1*(0.5*tothrd + del1*(1.0 + 134.0/81.0*del1)));
  delo = 1.5*ck2*x3thm1/(ao*ao*betao*betao2);
  xnodp = self->m_Sat.fMeanMotion/(1.0 + delo);
  aodp = ao/(1.0 - delo);
// Initialization 
// For perigee below 156 km, the values of s and qoms2t are altered. 
  s4 = s;
  qoms24 = qoms2t;
  perige = (double)(aodp*(1.0 - self->m_Sat.fEccentricity) - ae)*xkmper;
  if (perige >= 156.0) goto SDP10;
  s4 = perige - 78.0;
  if (perige > 98.0) goto SDP9;
  s4 = 20.0;
SDP9:
  qoms24 = pow((120.0 - s4)*ae/xkmper,4.0);
  s4 = s4/xkmper + ae;
SDP10:
  pinvsq = 1.0/(aodp*aodp*betao2*betao2);
  sing = sin(self->m_Sat.fPeregee);
  cosg = cos(self->m_Sat.fPeregee);
  tsi = 1.0/(aodp - s4);
  eta = aodp*self->m_Sat.fEccentricity*tsi;
  etasq = eta*eta;
  eeta = self->m_Sat.fEccentricity*eta;
  psisq = fabs(1.0 - etasq);
  coef = qoms24*pow(tsi,4.0);
  coef1 = coef/pow(psisq,3.5);
  c2 = coef1*xnodp*(aodp*(1.0 + 1.5*etasq + eeta*(4.0 + etasq))
      + 0.75*ck2*tsi/psisq*x3thm1*(8.0 + 3.0*etasq*(8.0 + etasq)));
  c1 = self->m_Sat.fRadiationCoefficient*c2;
  sinio = sin(self->m_Sat.fInclination);
  a3ovk2 = -xj3/ck2*pow(ae,3.0);
  x1mth2 = 1.0 - theta2;
  c4 = 2.0*xnodp*coef1*aodp*betao2*(eta*(2.0 + 0.5*etasq)
      + self->m_Sat.fEccentricity*(0.5 + 2.0*etasq) - 2.0*ck2*tsi/(aodp*psisq)
      *(-3.0*x3thm1*(1.0 - 2.0*eeta + etasq*(1.5 - 0.5*eeta))
      + 0.75*x1mth2*(2.0*etasq - eeta*(1.0 + etasq))*cos(2.0*self->m_Sat.fPeregee)));
  theta4 = theta2*theta2;
  temp1 = 3.0*ck2*pinvsq*xnodp;
  temp2 = temp1*ck2*pinvsq;
  temp3 = 1.25*ck4*pinvsq*pinvsq*xnodp;
  xmdot = xnodp + 0.5*temp1*betao*x3thm1
         + 0.0625*temp2*betao*(13.0 - 78.0*theta2 + 137.0*theta4);
  x1m5th = 1.0 - 5.0*theta2;
  omgdot = -0.5*temp1*x1m5th + 0.0625*temp2*(7.0 - 114.0*theta2 + 395.0*theta4)
          + temp3*(3.0 - 36.0*theta2 + 49.0*theta4);
  xhdot1 = -temp1*cosio;
  xnodot = xhdot1 + (0.5*temp2*(4.0 - 19.0*theta2)
          + 2.0*temp3*(3.0 - 7.0*theta2))*cosio;
  xnodcf = 3.5*betao2*xhdot1*c1;
  t2cof = 1.5*c1;
  xlcof = 0.125*a3ovk2*sinio*(3.0 + 5.0*cosio)/(1.0 + cosio);
  aycof = 0.25*a3ovk2*sinio;
  x7thm1 = 7.0*theta2 - 1.0;
// SDP90:Never used
  iflag = 0;
  Call_dpinit(self,&eosq,&sinio,&cosio,&betao,&aodp,&theta2,&sing,&cosg,
              &betao2,&xmdot,&omgdot,&xnodot,&xnodp);
// Update for secular gravity and atmospheric drag 
SDP100:
  xmdf = self->m_Sat.fMeanAnomaly + xmdot*tsince;
  omgadf = self->m_Sat.fPeregee + omgdot*tsince;
  xnoddf = self->m_Sat.fRightAscending + xnodot*tsince;
  tsq = tsince*tsince;
  xnode = xnoddf + xnodcf*tsq;
  tempa = 1.0 - c1*tsince;
  tempe = self->m_Sat.fRadiationCoefficient*c4*tsince;
  templ = t2cof*tsq;
  xn = xnodp;
  Call_dpsec(self,&xmdf,&omgadf,&xnode,&em,&xinc,&xn,&tsince);
  a = pow(xke/xn,tothrd)*sqr(tempa);
  e = em - tempe;
  ee = e*e;
  if ( ee > 1.0) return FALSE;	// wrong satellite datas
  xmam = xmdf + xnodp*templ;
  Call_dpper(self,&e,&xinc,&omgadf,&xnode,&xmam);
  xl = xmam + omgadf + xnode;
  beta = sqrt(1.0 - e*e);
  xn = xke/pow(a,1.5);
// Long period periodics 
  axn = e*cos(omgadf);
  temp = 1.0/(a*beta*beta);
  xll = temp*xlcof*axn;
  aynl = temp*aycof;
  xlt = xl + xll;
  ayn = e*sin(omgadf) + aynl;
// Solve Kepler's Equation 
  capu = Fmod2p(xlt - xnode);
  temp2 = capu;
  for (i = 1;i < 10;i++)
    {
    sinepw = sin(temp2);
    cosepw = cos(temp2);
    temp3 = axn*sinepw;
    temp4 = ayn*cosepw;
    temp5 = axn*cosepw;
    temp6 = ayn*sinepw;
    epw = (capu - temp4 + temp3 - temp2)/(1.0 - temp5 - temp6) + temp2;
    if (fabs(epw - temp2) <= e6a) goto SDP140;
//SDP130: Never used
    temp2 = epw;
    }; //for i
// Short period preliminary quantities 
SDP140:
  ecose = temp5 + temp6;
  esine = temp3 - temp4;
  elsq = axn*axn + ayn*ayn;
  temp = 1.0 - elsq;
  pl = a*temp;
  r = a*(1.0 - ecose);
  temp1 = 1.0/r;
  rdot = xke*sqrt(a)*esine*temp1;
  rfdot = xke*sqrt(pl)*temp1;
  temp2 = a*temp1;
  betal = sqrt(temp);
  temp3 = 1.0/(1.0 + betal);
  cosu = temp2*(cosepw - axn + ayn*esine*temp3);
  sinu = temp2*(sinepw - ayn - axn*esine*temp3);
  u = AcTan(sinu,cosu);
  sin2u = 2.0*sinu*cosu;
  cos2u = 2.0*cosu*cosu - 1.0;
  temp = 1.0/pl;
  temp1 = ck2*temp;
  temp2 = temp1*temp;
// Update for short periodics 
  rk = r*(1.0 - 1.5*temp2*betal*x3thm1) + 0.5*temp1*x1mth2*cos2u;
  uk = u - 0.25*temp2*x7thm1*sin2u;
  xnodek = xnode + 1.5*temp2*cosio*sin2u;
  xinck = xinc + 1.5*temp2*cosio*sinio*cos2u;
  rdotk = rdot - xn*temp1*x1mth2*sin2u;
  rfdotk = rfdot + xn*temp1*(x1mth2*cos2u + 1.5*x3thm1);
// Orientation vectors 
  sinuk = sin(uk);
  cosuk = cos(uk);
  sinik = sin(xinck);
  cosik = cos(xinck);
  sinnok = sin(xnodek);
  cosnok = cos(xnodek);
  xmx = -sinnok*cosik;
  xmy = cosnok*cosik;
  ux = xmx*sinuk + cosnok*cosuk;
  uy = xmy*sinuk + sinnok*cosuk;
  uz = sinik*sinuk;
  vx = xmx*cosuk - cosnok*sinuk;
  vy = xmy*cosuk - sinnok*sinuk;
  vz = sinik*cosuk;
// Position and velocity 
  x = rk*ux;  pos->x = x;
  y = rk*uy;  pos->y = y;
  z = rk*uz;  pos->z = z;
  xdot = rdotk*ux + rfdotk*vx;  vel->x = xdot;
  ydot = rdotk*uy + rfdotk*vy;  vel->y = ydot;
  zdot = rdotk*uz + rfdotk*vz;  vel->z = zdot;
  return TRUE;
};	//Procedure SDP4


int SGP(CSGP4_SDP4 *self, double time)
{
	double tsince;
	int ret;
	VECTOR pos,vel;
	self->m_bLatLonAlt = FALSE;
	self->m_fTime = time;
	tsince = (time - self->m_Sat.fJulianEpoch) * xmnpda;
	if (ideep == 0)
		ret = SGP4(self,tsince,&iflag,&pos,&vel);	// Calculation for near earth
	else
		ret = SDP4(self,tsince,&iflag,&pos,&vel);	// Calculation for the deep space
	if (ret == FALSE) return FALSE;	// Ooops, there was a mistake within the calculation !!!
	ConvertSatState(self, &pos, &vel);	// here the pos and vel vector are 
									// to be copied to m_vPOS and m_vVEL !!!
	return TRUE;
} // Procedure SGP

int GetVersion(char *pBuffer, int iMaxLen)		// returns a pointer to the version of this dll
{
	static char	szVersion[] = {
			"NORAD SGP4 / SDP4 algorithm (WGS72) for GPS.\n"
			"Pascal Version 2.5 by Dr.TS Kelso in 1995 (tkelso@celestrak.com)\n"
			"Converted to C++ by Varol Okan in 1996\n"
			"Converted to C by ASF in 2008\n"
			"DLL-Version 1.6 (18.Oct.2001)\n\n"
			"This version resolves the AcTan, and Lyddan - bug.\0"};

	strncpy(pBuffer, szVersion, iMaxLen);
    return strlen(szVersion);
}

static void SetSatellite (CSGP4_SDP4 *self, char *cLine0, char *cLine1, char *cLine2, int bInitSatellite)
{
	memcpy(self->m_cLine0, cLine0, 22);
	memcpy(self->m_cLine1, cLine1, 69);
	memcpy(self->m_cLine2, cLine2, 69);
	self->m_cLine0[22] = self->m_cLine1[69] = self->m_cLine2[69] = 0;
	ConvertData(self);
	if (bInitSatellite)
		InitSatellite(self);
	return;
}

SATELLITE *GetSatellite(CSGP4_SDP4 *self) {
  return &(self->m_Sat);
}

VECTOR GetUserPos(CSGP4_SDP4 *self) {
  return self->m_vUPos;
}

VECTOR GetUserVel(CSGP4_SDP4 *self) {
  return self->m_vUVel;
}

VECTOR GetObserver(CSGP4_SDP4 *self) {
  return self->m_vObs;
}

VECTOR GetRADec(CSGP4_SDP4 *self) {
  return self->m_vRad;
}
	
VECTOR GetPos(CSGP4_SDP4 *self)	{
  return self->m_vPOS;
}

VECTOR GetVel(CSGP4_SDP4 *self) {
  return self->m_vVEL;
}

double GetTime(CSGP4_SDP4 *self) {
  return self->m_fTime;
}

double GetLat(CSGP4_SDP4 *self) 
{
  if (self->m_bLatLonAlt)
    return self->m_vLLA.x;
  else {
    assert(0);
    return 0;
  }
}

double GetLon(CSGP4_SDP4 *self)
{
  if (self->m_bLatLonAlt)
    return self->m_vLLA.y;
  else {
    assert(0);
    return 0;
  }
}

double GetAlt(CSGP4_SDP4 *self)
{
  if (self->m_bLatLonAlt)
    return self->m_vLLA.z;
  else {
    assert(0);
    return 0;
  }
}

static double GetFloat(int iStart, int iEnd, char *cLine) 
{
  char cs[256];
  double ret;
  --iStart;
  strncpy(cs, cLine+iStart, iEnd-iStart);
  cs[iEnd-iStart]='\0';
  //cs = csLine.Mid(iStart, iEnd-iStart);
  ret = atof(cs);
  return ret;
}

static long GetInt(int iStart, int iEnd, char *cLine) 
{
  char cs[256];
  long ret;
  --iStart;
  strncpy(cs, cLine+iStart, iEnd-iStart);
  cs[iEnd-iStart]='\0';
  //cs = csLine.Mid(iStart, iEnd-iStart);
  ret = atoi(cs);
  return ret;
}

static char *GetString(int iStart, int iEnd, char *cLine) 
{
  static char cBuffer[100];
  --iStart;
  strncpy(cBuffer, cLine+iStart, iEnd-iStart);
  cBuffer[iEnd-iStart]='\0';
  return (char *) &cBuffer;
}


static void ConvertSatState(CSGP4_SDP4 *self, VECTOR *pos,VECTOR *vel)
{
  pos->x *= xkmper;
  pos->y *= xkmper;
  pos->z *= xkmper;
  pos->w = sqrt(sqr(pos->x)+sqr(pos->y)+sqr(pos->z));
  vel->x *= xkmper/60.0;	// kilometers/second
  vel->y *= xkmper/60.0;	// kilometers/second
  vel->z *= xkmper/60.0;	// kilometers/second
  vel->w = sqrt(sqr(vel->x)+sqr(vel->y)+sqr(vel->z));
  // write to the public member ...
  self->m_vPOS.x = pos->x;
  self->m_vPOS.y = pos->y;
  self->m_vPOS.z = pos->z;
  self->m_vPOS.w = pos->w;
  self->m_vVEL.x = vel->x;
  self->m_vVEL.y = vel->y;
  self->m_vVEL.z = vel->z;
  self->m_vVEL.w = vel->w;

} //Procedure Convert_Sat_State

static void ConvertData(CSGP4_SDP4 *self)
{
	memcpy(self->m_Sat.cSatelliteName, self->m_cLine0, 22);
// Now everything is in butter !
//	CString	csHelp;
//	double	dEpochDay;
	int		iTemp;
	char	*pChar;
	self->m_Sat.iSatelliteNumber	= GetInt	(3 , 7,self->m_cLine1);	// = CString >catnr<
	self->m_Sat.iLaunchYear		= GetInt	(10,11,self->m_cLine1);
	self->m_Sat.iLaunchNumber	= GetInt	(12,14,self->m_cLine1);
	pChar				= GetString	(15,17,self->m_cLine1);
	memcpy(self->m_Sat.cLaunchPiece, pChar, 3);
	self->m_Sat.iEpochYear		= GetInt	(19,20,self->m_cLine1);	// = epoch 19, 14 lang
	self->m_Sat.fEpochDay		= GetFloat	(21,32,self->m_cLine1);	// = epoch 19, 14 lang
	self->m_Sat.fBalisticCoefficient= GetFloat	(34,43,self->m_cLine1);	// xndt2o is never used so far ...
	self->m_Sat.fSecondMeanMotion 	= 0.0;				// xndd60 is never used so far ...
	self->m_Sat.iSecondMeanMotion 	= 0;				// xndd60 is never used so far ...
//	m_Sat.fSecondMeanMotion 	= GetFloat	(45,50,m_cLine1);	// xndd60 is never used so far ...
//	iexp 	= GetInt	(51,52,m_cLine1);				// iexp is never used so far ...
//	m_Sat.fBalisticCoefficient *= pow(10.0, iexp);				// --- '' ---
	
//	m_Sat.fRadiationCoefficient = GetFloat	(54,61,m_cLine1)*1e-5;	// = bstar 54, 6 lang
	// the input format is 12345-x convert to 0.12345^x
	iTemp	= GetInt	(60,61,self->m_cLine1);	// get the exponent, and make 0.xxxxx of it
	self->m_Sat.fRadiationCoefficient	= GetFloat	(54,59,self->m_cLine1)*pow(10,-5+iTemp);

	pChar						= GetString	(63,63,self->m_cLine1);
	memcpy(&(self->m_Sat.cEmphemeristType), pChar, 2);
	self->m_Sat.iElementNumber	= GetInt	(65,68,self->m_cLine1);

	self->m_Sat.fInclination	= GetFloat	(9 ,16,self->m_cLine2);	// = xincl
	self->m_Sat.fRightAscending	= GetFloat	(18,25,self->m_cLine2);	// = xnodeo
	self->m_Sat.fEccentricity	= GetFloat	(27,33,self->m_cLine2)*1e-7;	// = eo
	self->m_Sat.fPeregee		= GetFloat	(35,42,self->m_cLine2);	// = omegao
	self->m_Sat.fMeanAnomaly	= GetFloat	(44,51,self->m_cLine2);	// = xmo
	self->m_Sat.fMeanMotion		= GetFloat	(53,63,self->m_cLine2);	// = xno
	self->m_Sat.iRevAtEpoch		= GetInt	(64,68,self->m_cLine2);

	self->m_Sat.fJulianEpoch = JulianDate (self->m_Sat.iEpochYear *1000.0+ self->m_Sat.fEpochDay);
//	m_Sat.fEpochFraction = modf(m_Sat.fEpochDay, &dEpochDay);
//	m_Sat.iEpochDay = (int) dEpochDay;
	self->m_Sat.iEpochDay = (int) self->m_Sat.fEpochDay;
	self->m_Sat.fEpochFraction = self->m_Sat.fEpochDay - (double)self->m_Sat.iEpochDay;

	self->m_Sat.iEpochYear += 1900;
	self->m_Sat.iLaunchYear += 1900;
}

static void Init(CSGP4_SDP4 *self)
{
// Normal variables for the 72'model !!!
	ae       = 1.0;
	tothrd   = 2.0/3.0;
	xkmper   = 6378.135;	// Earth equatorial radius - kilometers (WGS '72)
	f        = 1.0/298.26;	// Earth flattening (WGS '72)
	ge       = 398600.8;	// Earth gravitational constant (WGS '72)
	J2       = 1.0826158E-3;// J2 harmonic (WGS '72)
	J3       = -2.53881E-6;	// J3 harmonic (WGS '72)
	J4       = -1.65597E-6;	// J4 harmonic (WGS '72)
	ck2      = (double)J2/2.0;
	ck4      = -3.0*J4/8.0;
	xj3      = (double)J3;
	qo       = ae + 120.0/xkmper;
	s        = ae + 78.0/xkmper;
	e6a      = 1E-6;
	dpinit   = 1.0;			// Deep-space initialization code
	dpsec    = 2.0;			// Deep-space secular code
	dpper    = 3.0;			// Deep-space periodic code
	xmnpda	 = 1440.0;		// Minutes per day
	secday	 = 86400;		// seconds per day
	omega_E	 = 1.00273790934;	// earth rotation per sideral day
	xke    = sqrt(3600.0*ge/(xkmper*xkmper*xkmper));  //sqrt(ge) ER^3/min^2
	qoms2t = sqr(sqr(qo-s));              //(qo-s)^4 ER^4
}

static void InitSatellite(CSGP4_SDP4 *self)
{
// Convert sat datas to proper units ...
//	m_Sat.fRadiationCoefficient *= pow(10.0, ibexp)/ae;		// Already done
//	m_Sat.fBalisticCoefficient *= pow(10.0, iexp);			//  earlier !!!
	self->m_Sat.fRightAscending = DegToRad(self->m_Sat.fRightAscending);
	self->m_Sat.fPeregee = DegToRad(self->m_Sat.fPeregee);
	self->m_Sat.fMeanAnomaly = DegToRad(self->m_Sat.fMeanAnomaly);
	self->m_Sat.fInclination = DegToRad(self->m_Sat.fInclination);
	self->m_Sat.fMeanMotion *= 2.0*PI/xmnpda;
//	m_Sat.fSecondMeanMotion *= 2.0*PI/sqr(xmnpda);				// Never used variables ...
//	m_Sat.fBalisticCoefficient *= 2.0*PI/(xmnpda*xmnpda*xmnpda);// Never used variables ...
// determination if it is deep space or not ...
	double a1,a0,del1, del0,temp;
	a1 = pow (xke/self->m_Sat.fMeanMotion, tothrd);
	temp = (double)(1.5*ck2*(3.0*sqr(cos(self->m_Sat.fInclination))-1.0)/pow(1.0-self->m_Sat.fEccentricity*self->m_Sat.fEccentricity,1.5));
	del1 = temp /(a1*a1);
	a0 = a1*(1-del1*(0.5*tothrd+del1*(1.0+134.0/81.0*del1)));
	del0 = temp/(a0*a0);
	xnodp = self->m_Sat.fMeanMotion/(1.0+del0);
	if (2.0*PI/xnodp >= 225.0) ideep = 1;
	else ideep = 0;
	iflag = 1;

	self->m_bEclipsed = FALSE;
	sr = 696000.0;		// {Solar radius - kilometers (IAU 76)}
	//AU = 1.49597870E8;	// {Astronomical unit - kilometers (IAU 76)}
}

static void SetSatellite2(CSGP4_SDP4 *self, SATELLITE *pSatellite, int bInitSatellite)
{
	int iEpochYear;
	SATELLITE *pSat = (SATELLITE *)pSatellite;
	memcpy(self->m_Sat.cSatelliteName, pSat->cSatelliteName, 22);
// Now everything is in butter!
//	CString		csHelp;
	self->m_Sat.iSatelliteNumber		= pSat->iSatelliteNumber;
	self->m_Sat.iLaunchYear			= pSat->iLaunchYear;
	self->m_Sat.iLaunchNumber		= pSat->iLaunchNumber;
	memcpy(self->m_Sat.cLaunchPiece, pSat->cLaunchPiece, 3);
	self->m_Sat.iEpochYear			= pSat->iEpochYear;
	self->m_Sat.iEpochDay			= pSat->iEpochDay;
	self->m_Sat.fEpochDay			= pSat->fEpochDay;
	self->m_Sat.fEpochFraction		= pSat->fEpochFraction;
	self->m_Sat.fBalisticCoefficient	= pSat->fBalisticCoefficient;
	self->m_Sat.fSecondMeanMotion		= pSat->fSecondMeanMotion;
	self->m_Sat.iSecondMeanMotion		= pSat->iSecondMeanMotion;
	self->m_Sat.fRadiationCoefficient = pSat->fRadiationCoefficient;
	memcpy(self->m_Sat.cEmphemeristType, pSat->cEmphemeristType,2);
	self->m_Sat.iElementNumber		= pSat->iElementNumber;

	self->m_Sat.fInclination		= pSat->fInclination;
	self->m_Sat.fRightAscending		= pSat->fRightAscending;
	self->m_Sat.fEccentricity		= pSat->fEccentricity;
	self->m_Sat.fPeregee			= pSat->fPeregee;
	self->m_Sat.fMeanAnomaly		= pSat->fMeanAnomaly;
	self->m_Sat.fMeanMotion			= pSat->fMeanMotion;
	self->m_Sat.iRevAtEpoch			= pSat->iRevAtEpoch;

	iEpochYear = (int)(self->m_Sat.iEpochYear/100.0)*100;	// convert 1996 to 1900
	iEpochYear = self->m_Sat.iEpochYear - iEpochYear;	// but i need 96
	self->m_Sat.fJulianEpoch = JulianDate (iEpochYear*1000.0+self->m_Sat.iEpochDay+self->m_Sat.fEpochFraction);
	if (bInitSatellite)
		InitSatellite(self);
	return;
};



static double JulianDateFromSystemTime (SYSTEMTIME st)
{
	double fDay, fDate;
	int iYear  = (int)st.wYear;
	int iMonth = (int)st.wMonth;
	int iDay   = (int)st.wDay;
	int iHour  = (int)st.wHour;
	int iMin   = (int)st.wMinute;
	int iSec   = (int)st.wSecond;
	
	int iEpocheYear = EpocheYear(iYear) * 1000; // 1999 to 99000 and 2032 to 32000.
	int iDayOfYear = DayOfYear(iYear, iMonth, iDay);
	double fFraction = FractionOfDay(iHour, iMin, iSec);

	fDate = iEpocheYear + iDayOfYear + fFraction;
	fDay = JulianDate(fDate);
	return fDay;	// The JulianDate of the CTime Date
}
/*
double CSGP4_SDP4::JulianDate (CTime ct)
{
	// CTime is defined between 1970 and 2038 (Microsoft)
	double fDay, fDate;
	int iYear  = (int)ct.GetYear();
	int iMonth = (int)ct.GetMonth();
	int iDay   = (int)ct.GetDay();
	int iHour  = (int)ct.GetHour();
	int iMin   = (int)ct.GetMinute();
	int iSec   = (int)ct.GetSecond();

	int iEpocheYear = EpocheYear(iYear) * 1000; // 1999 to 99000 and 2032 to 32000.
	int iDayOfYear = DayOfYear(iYear, iMonth, iDay);
	double fFraction = FractionOfDay(iHour, iMin, iSec);

	fDate = iEpocheYear + iDayOfYear + fFraction;
	fDay = JulianDate(fDate);
	return fDay;	// The JulianDate of the CTime Date
}
*/

static double JulianDate (double st)
// Astronomical Formulae for Calculators, Jean Meeus, pages 23-25
// Calculate Julian Date of 0.0 Jan year
{
// input like : 99200.27104438 => 1999, 200.27104438 days.
/*	double dYear;
	int iYear = (int)(st/1000.0);
	st -= iYear * 1000;
	if (iYear < 50) iYear +=1999;
	else iYear +=1899;
old time ... */
// Modification to support Y2K }
// Valid 1957 through 2056 }
	double fYear, fDay;
	int iYear = (int)(st/1000.0);
	fDay = st - iYear * 1000.0; // = 99200,271 - 99000.0 = 200,271
	if (iYear < 57) iYear += 2000;
	else iYear += 1900;
	fYear = JulianDateOfYear(iYear);
	return fYear + fDay; 
}

static double JulianDateOfYear (int yr)
{
	long A,B;
	int iYear = yr - 1;
	double fYear;
	A = (long)(iYear/100.0);
	B = 2 - A + (long)(A/4.0);
	fYear = (long)(365.25 * iYear)
          + 428.0 + 1720994.5 + B;
	return fYear;
}

static int EpocheYear (int iYear)
{	// returns 99 for 1999 and 23 for 2023
	int iEpocheYear;
	if (iYear > 2000)
		iEpocheYear = iYear - 2000;
	else 
		iEpocheYear = iYear - 1900;
	return iEpocheYear;
}

static int DayOfYear (int yr, int mo, int dy)
{
	// yr=1999 / mo=12 / dy=24 => 12/24/1999=christmas 1999
	// December is never used since the last Month of a year
	// is added by getting the days from that month.
    int days[] = {31,28,31,30,31,30,31,31,30,31,30,31};
	int i, iDay;

	iDay = 0;
	for (i=0; i < mo - 1; i++)	// mo-2 because zero based and just add until last month
		iDay += days[i];
	iDay += dy;

	if	(((yr %4) == 0) &&	// every four years
		(((yr %100) != 0) || ((yr %400) == 0)) &&	// but not at centuries other then every 400 years
		(mo > 2)) iDay += 1;		// Schaltjahr ???
	return iDay;
}

static double FractionOfDay (int hr, int min, int sec)
{
	double fFraction;
	fFraction = (double) (60.0*60.0*hr	+ 
						  60.0*min		+
						  (double)sec    )/(24.0*60.0*60.0);
//						  msec/100)/(24*60*60);Not precise enough for milliseconds
//  Fraction_of_Day := (hr + (min + (sec + msec/100)/60)/60)/24;
	return fFraction;
}


static SYSTEMTIME CalendarDate (double dJulian)
{
//  { Astronomical Formulae for Calculators, Jean Meeus, pages 26-27 }
    int		month, alpha;
    double	Z,A,B,C,D,E,F;
    double	day,year;
	double	hour, minute, second;	//, milliseconds;

	static SYSTEMTIME ret;
	F = modf(dJulian + 0.5,&Z);
	if (Z < 2299161)
		A = Z;
	else	{
		alpha = (int)((Z - 1867216.25)/36524.25);
		A = Z + 1.0 + alpha - (int)(alpha/4);
	}
	B   = A + 1524.0;
	modf((B - 122.1)/365.25, &C);
	modf(365.25 * C, &D);
	modf((B - D)/30.6001, &E);
	day = B - D - (int)(30.6001 * E) + F;
	
	if (E < 13.5)	month = round(E - 1.0);
	else			month = round(E - 13.0);
	if (month > 2.5)year  = C - 4716.0;
	else			year  = C - 4715.0;

	A = modf(day, &Z);		// A = fraction of day ...
	A = modf(A*24.0, &hour);
	A = modf(A*60.0, &minute);
	A = modf(A*60.0, &second);
//	A = modf(A*100,&milliseconds);

	ret.wYear			= (int)year;
	ret.wMonth			= (int)month;
	ret.wDay			= (int)day;
	ret.wHour			= (int)hour;
	ret.wMinute			= (int)minute;
	ret.wSecond			= (int)++second;
//	ret.wMilliseconds	= (int)milliseconds; Not enough precission in the double, Julian Date, for milliseconds ...
	ret.wMilliseconds	= 0;
	ret.wDayOfWeek		= 0;
	return ret;
}

static double SideralTime(double jd)
{
// Reference:  The 1992 Astronomical Almanac, page B6.
	double modulo;
	double UT,TU,GMST;
	UT   = modf(jd + 0.5, &TU);
	jd   -= UT;
	TU   = (double)(jd - 2451545.0)/36525.0;
	GMST = 24110.54841 + TU * (8640184.812866 + TU * (0.093104 - TU * 6.2E-6));
	GMST = (GMST + secday*omega_E*UT);	//%secday;

	modf (GMST/secday, &modulo);
	modulo = GMST - modulo* secday;
	if (modulo < 0.0)
		modulo += secday;
	return PI * 2.0 * modulo/secday;
}

static double ThetaG(double jd)
{
// Reference:  The 1992 Astronomical Almanac, page B6. 
	double	UT,TU,GMST;
  UT	= modf(jd + 0.5, &jd);
  TU	= (jd - 2451545.0)/36525.0;
  GMST	= 24110.54841 + TU * (8640184.812866 + TU * (0.093104 - TU * 6.2E-6));
  GMST	= Modulus(GMST + secday*omega_E*UT,secday);
  return (2.0*PI*GMST/secday);
}





static VECTOR CalculateSolarPosition(double time)	//, solar_vector : vector);
{
	static VECTOR solar_vector;
	double DeltaET_year;
	double mjd,year,T,M,L,e,C,O,Lsa,nu,R,eps;
	mjd  = time - 2415020.0;
	year = 1900.0 + mjd/365.25;
	DeltaET_year = 26.465 + 0.747622*(year - 1950.0) + 1.886913*sin(2.0 * PI * (year - 1975.0)/33.0);

	T = (mjd + DeltaET_year/secday)/36525.0;
	M = (Modulus(358.47583 + Modulus(35999.04975*T,360.0) - (0.000150 + 0.0000033*T)*sqr(T),360.0)) * PI/180.0;
	L = (Modulus(279.69668 + Modulus(36000.76892*T,360.0) + 0.0003025*sqr(T),360.0)) * PI/180.0;
	e = 0.01675104 - (0.0000418 + 0.000000126*T)*T;
	C = ((1.919460 - (0.004789 + 0.000014*T)*T)*sin(M) + (0.020094 - 0.000100*T)*sin(2*M) + 0.000293*sin(3*M)) * PI/180.0;
	O = (Modulus(259.18 - 1934.142*T,360.0)) * PI/180.0;
	Lsa = Modulus(L + C - (0.00569 - 0.00479*sin(O)) * PI/180.0, PI * 2.0);
	nu  = Modulus(M + C,PI * 2.0);
	R   = 1.0000002*(1 - sqr(e))/(1 + e*cos(nu));
	eps = (23.452294 - (0.0130125 + (0.00000164 - 0.000000503*T)*T)*T + 0.00256*cos(O)) * PI/180.0;
	R   = AU*R;
	solar_vector.x = R*cos(Lsa);
	solar_vector.y = R*sin(Lsa)*cos(eps);
	solar_vector.z = R*sin(Lsa)*sin(eps);
	solar_vector.w = R;
	// end; {Procedure Calculate_Solar_Position}
	return solar_vector;
}

static double DepthOfEclipse(CSGP4_SDP4 *self, double time, VECTOR r1)
{
	double r1_r1,r1_r2,r2_r2,k,d,ds;
	VECTOR r2;
	Magnitude(&r1);
	r2 = CalculateSolarPosition(time);
//	solar_pos = r2;
	r1_r1 = sqr(r1.w);
	r1_r2 = -Dot(r1,r2);
	r2_r2 = sqr(r2.w);
	k = r1_r2/r2_r2;
	// {Calculate perpendicular distance from anti-solar vector}
	d = sqrt(r1_r1 - sqr(r1_r2)/r2_r2);
	// {Calculate shadow distance ds}
	ds = xkmper + k * (sr - xkmper);
	// {If d < ds, then satellite is in eclipse}
	if ( (k > 0.0) && (d < ds) )
		self->m_bEclipsed = TRUE;
	else
		self->m_bEclipsed = FALSE;
	return d - ds;
	// Depth_of_Eclipse := d - ds
	// end; {Function Depth_of_Eclipse}
}

static int GetEclipsed(CSGP4_SDP4 *self)
{
	return self->m_bEclipsed;
}

void CalculateLatLonAlt(CSGP4_SDP4 *self, double jdTime)
{
	self->m_vLLA = CalculateLatLonAlt2(self->m_vPOS, jdTime);
	self->m_bLatLonAlt = TRUE;
}

static VECTOR CalculateLatLonAlt2(VECTOR vPOS, double time)
{
// Reference:  The 1992 Astronomical Almanac, page K12. 
	static VECTOR vLLA;
	double lat,lon,alt;
	double theta,r,e2,phi,c;
	double arg1, arg2;

	vLLA.x = vLLA.y = vLLA.z = vLLA.w = 0.0;
	lat = lon = alt = 0.0;
	theta = r = e2 = phi = c = 0.0;

//	theta = atan2(vPOS.y,vPOS.x);
	theta = AcTan(vPOS.y,vPOS.x);
	
	arg1 = ThetaG(time);
	arg1 = theta - arg1;
	arg2 = 2.0* PI;

//	lon = Modulus(theta - ThetaG(time),2.0*PI);
	lon = Modulus(arg1, arg2);

	r = sqrt(sqr(vPOS.x) + sqr(vPOS.y));
	e2 = f*(2.0 - f);
	lat = AcTan(vPOS.z,r);
	do	{
		phi = lat;
		c = 1.0/sqrt(1.0 - e2*sqr(sin(phi)));
		lat = AcTan( vPOS.z + xkmper*c*e2*sin(phi),r);
	}	while (fabs(lat - phi) > 1E-10);//1E-7); For speeding up calculation 7 digit
										//is exact enough (123.45
	alt = r/cos(lat) - xkmper*c;

	vLLA.x = lat*180.0/PI;   // radians
	vLLA.y = lon*180.0/PI;   // radians
	vLLA.z = alt;			// kilometers
	vLLA.w = theta*180.0/PI; // radians
	return vLLA;
}

/*----------------------------------------------------------------------*/
//int CSGP4_SDP4::Eclipse( double *r, double *sun, double *moon, int *em, int *ee, char **which )
static int Eclipse(double *r, VECTOR *vSun, VECTOR *vMoon, int *em, int *ee, char **which )
{
  /* function to compute if object at location r is eclipsed by
     either the earth or moon.  ECI coordinates for all vectors.
     Inputs:
	r is the location of s/c
	sun is location of sun
	moon is location of moon
     Outputs
	which points to  message about result of the test
	em is zero if not eclipsed by moon
	ee is zero if not eclipsed by earth
     Returns 0 if no eclipse.
  */
	double us[3], um[3], ue[3];
	double alpha, beta, beta_sun;
	double mdist, edist, sdist;
	double x[3];
	double *sun = (double *)&vSun;
	double *moon= (double *)&vMoon; 
  *em = 0;               /* assume no eclipse */
  *ee = 0;
  /* find distances and directions to sun, moon and earth */
  VecDiff( sun, r, x, 3 ); sdist = VecMag( x, 3 );
  UnitVec( x, us, 3 );   /* direction towards the sun */            
  VecDiff( moon, r, x, 3 ); mdist = VecMag( x, 3 );
  UnitVec( x, um, 3 );   /* direction towards the moon */            
  VecScale( -1.0, r, ue, 3 ); /* vector to earth center */
  UnitVec( ue, ue, 3 );  /* direction to earth center */
  edist = VecMag( r, 3 );
  /* cannot have eclipse if sun is closest */
  *which = "No Eclipse";
  if (( sdist <= edist ) && (sdist <= mdist )) return 0;
  beta_sun = asin( RSUN / sdist );    /* half angle of sun */
  /* look for eclipse by earth */
  beta = asin( REarth / edist );      /* half angle of earth */
  alpha = acos( VecDot( ue, us, 3 )); /* angle from earth to sun */
  if ( alpha < (beta + beta_sun)) {   /* some kind of eclipse */
    *ee = 1;
    if (( beta >= beta_sun ) && ( alpha <= (beta - beta_sun))){ /* total */
      *which = "Total Eclipse by Earth"; return 1; }
    else {   /* partial */
      *which = "Partial Eclipse by Earth"; return 1; }
  }
  /* look for eclipse by moon */
  beta = asin( RMOON / mdist );      /* half angle of moon */
  alpha = acos( VecDot( um, us, 3 ));/* angle from moon to sun */
  if ( alpha < (beta + beta_sun)) {  /* some kind of eclipse */
    *em = 1;
    if (( beta >= beta_sun ) && ( alpha <= (beta - beta_sun))){ /* total */
      *which = "Total Eclipse by Moon"; return 1; }
    else {   /* partial */
      *which = "Partial Eclipse by Moon"; return 1; }
  }
  return 0;
}
/*----------------------------------------------------------------------*/
static double MoonPhase(double *Sun, double *Moon, double *Observer )
{
  /* Calculate the phase of the moon in degrees as seen by
     the observer.  180 deg is full.
  */
  double MO[3], UMO[3], USO[3], SO[3], C;
  VecDiff( Moon, Observer, MO, 3 );
  VecDiff( Sun, Observer, SO, 3);
  UnitVec( MO, UMO, 3 );
  UnitVec( SO, USO, 3 );
  C = VecDot( USO, UMO, 3 );
  return acos( C ) * RTD;
}
/**********************************************************************/
static void SunMoon(double T, double *Sun, double *Moon )
{
  /* locate the sun and moon in geocentric inertial coordinates
   input time in Julian days and get back sun and moon in meters.
  */

   /* sun ephemeris based on Slabinski's suneph subroutine 
   which is adapted from Newcomb's theory of the sun, A.P.A.E, 2nd ed.
   Vol 6 part 1 (Washington: Navy Dept., 1898), pp. 9-21 and
   described in Comsat Tech Memo CD-03-72.
   */

  static double ABERTN = 99.3664E-06;  /* constant of aberation in rad */
  static double T0 = 2415020.0;        /* epoch for this fourier series */
  double DT;
  double MEARTH, COSME, SINME, NURA;
  double LONSUN, COSL, SINL, RS, E, OB, SOB, COB;
  double LONPRT, NMOON, MR, MRi, LMMean, F, MM;
  double cosmm, cosf, sinmm, sinf, sin2d; 
  double cos2d, sin2f, cos2f, sin2mm, cos2mm; 
  double SL, CL, S, cosd, sind, LATMN, LONGMN;
  double MVENUS, MMARS, MJUP, DMNSUN;
  double myTrue[3];
  /* convert Julian time to ephemeris time wrt to epoch 1900 Jan 0.5 ET*/
  DT = (T - T0 + 6.31E-04 + 2.85E-08 * (T-2446066.5))/36525.0;
  /* compute earth mean anomaly, eqn 14 */
  MEARTH = fmod(6.25658358 + 628.30194572 * DT, M2PI );
  COSME = cos( MEARTH ); SINME = sin( MEARTH );
  /* Mean anomalies for perturbing bodies */
  MVENUS = fmod(3.70795199 + DT * 1021.32292286, M2PI);
  MMARS  = fmod(5.57772322 + DT *  334.05561740, M2PI);
  MJUP   = fmod(3.93187774 + DT *   52.96346477, M2PI);
  /* mean elongation of the moon from the sun, eqn 15 */
  DMNSUN = fmod(6.12152394 + DT * 7771.37719393, M2PI);
  /* celestial longitude of the mean asc node of the moon's orbit, eqn 16 */
  NMOON  = fmod(4.52360151 - DT *   33.757146246, M2PI);
  NURA = -83.55E-06 * sin( NMOON );   /* nutation of ra ?  eqn 25 */
  /* perturbation expressions are from Newcomb, Tables of the sun
     A.P.A.E. vol. 6, pp.14-18 (1898)*/
  LONPRT =  23.4553E-06 * cos( 5.220309 + MVENUS - MEARTH ) /*eqn 13 */
	  + 26.7908E-06 * cos(2.588556 + 2.0 * (MVENUS - MEARTH))
	  + 12.1058E-06 * cos(5.514251 + 2.0 * MVENUS - 3.0 * MEARTH)
	  +  9.9047E-06 * cos(6.001983 + 2.0 * (MEARTH - MMARS))
	  + 34.9454E-06 * cos(3.133418 + MEARTH - MJUP)
	  + 12.6051E-06 * cos(4.593997 - MJUP)
	  + 13.2402E-06 * cos(1.520967 + 2.0 * (MEARTH - MJUP))
	  + 31.0281E-06 * cos(4.035026 + DT * 0.352556)
	  + 31.2898E-06 * sin( DMNSUN ) + NURA - ABERTN;
  LONSUN = fmod(4.88162793 + DT * (628.33195099 + DT * 5.2844E-06), M2PI)
	   + SINME * (33495.79E-06 - 83.58E-06 * DT 
	   + COSME * (701.41E-06 - 3.50E-06 * DT + COSME * 20.44E-06))
	   + LONPRT;
  COSL = cos( LONSUN );
  SINL = sin( LONSUN );
  RS = AU * (1.0002806 - COSME * (1675.104E-05 - 4.180E-05 * DT
		       + COSME * 28.060E-05));
  /* true obliquity of the ecliptic, eqn 1 & 26 */
  OB = 0.40931970 - 2.27111E-04 * DT + 4.465E-05 * cos ( NMOON );
  COB = cos( OB ); SOB = sin( OB );
  E = NURA * COB;                   /* eqn of equinoxes, eqn 2 */
  /* convert to cartesian coordinates, eqns. 6-10 scaled to meters */
  Sun[0] = RS * (COSL + SINL * COB * E );  
  Sun[1] = RS * (SINL * COB - COSL * E );
  Sun[2] = RS * SINL * SOB; 

  /* lunar ephemeris is based on Lrown's Lunar Theory Ammended.
  The expressions are adapted from Woolard, theory of the rotation the
  Earth, A.P.A.E Vol.15, Part 1 (1953), Tables 4, 6, and 7.

  The moon vector is computed at the Earth's center in the geometric 
  direction of the moon.  Neglecting the light travel time of 1.3 sec 
  between Earth and moon gives a negligible error of 0.0002 deg for
  the moons's direction.
  */

  LMMean = fmod( 4.71996657 + DT * (8399.70914492 - DT * 1.9780E-05), M2PI);
  F      = fmod( 0.19636505 + DT * (8433.46629117 - DT * 5.6044E-05), M2PI);
  MM     = fmod( 5.16800034 + DT * (8328.69110367 + DT * 1.60425E-04),M2PI);
  DMNSUN -= (2.5065E-05 * DT * DT);   /* eqn 33 */
  NMOON  += (3.6264E-05 * DT * DT);   
  /* fundamental frequency sines and cosines */
  cosmm = cos(MM);     sinmm = sin(MM); 
  cosd  = cos(DMNSUN); sind  = sin(DMNSUN);
  cosf  = cos(F);      sinf  = sin(F);
  /* double frequency sines and cosines */
  sin2d   = 2.0 * sind * cosd;    cos2d  =  cosd * cosd  -  sind * sind;
  sin2f   = 2.0 * sinf * cosf;    cos2f  =  cosf * cosf  -  sinf * sinf;
  sin2mm  = 2.0 * sinmm * cosmm;  cos2mm = cosmm * cosmm - sinmm * sinmm;
  /* lunar longitude */
  LONGMN = sind * (-607.0 + 84.0 * COSME - 131.0 * cosmm )
       + sin2d * (11490.0 + 134.0 * cos2d + 239.0 * cos2f 
		     + 683.0 * COSME
         +  cosmm * (23098.0 + 392.0 * cos2d + 918.0 * COSME 
		  	      - 80.0 * cos2f + 138.0 * cos2mm)
         +  cos2mm * (1096.0 + 300.0 * cos2d))
       +  sin2f * (-1996.0 - 295.0 * cos2d 
	    + cosmm * (-411.0 - 20.0 * cos2d))
       +  SINME * (-3238.0 + 90.0 * cosd - 919.0 * cos2d - 72.0 * COSME
         +  cosmm * (-1248.0 - 1222.0 * cos2d) - 84.0 * cos2mm)
       + sin2mm * ( 3876.0 + 10.0 * COSME
	 + cos2d * ( -956.0 - 296.0 * cos2d ) )
       + (sin2mm * cosmm + cos2mm * sinmm ) * ( 175.0 -59.0 * cos2d )
       + 68.9 * sin( 6.042967 + 2.318888 * DT ) - 49.0 * sin( NMOON )
       + sinmm * ( sin2d * ( -1052.0 * SINME -72.0 * sin2f )
		   + COSME * ( 184.0 - 804.0 * cos2d )
		   + cos2d * ( -21305.0 - 352.0 * cos2d )
		   + cos2f * (  -27.0 + 8.0 * cos2d )
		   + 49.0 * cosd + 109936.0 ); 
  LONGMN = LONGMN * 1.0E-06 + LMMean;
  /* lunar latitude */
  LATMN = cosf * ( -49.0E-06 * sind
	     + sin2d * ( 3592.0E-06 +  119.0E-06 * COSME + 2008.0E-06 * cosmm
				+   85.0E-06 * cos2mm )
             + SINME  * (  -55.0E-06 -   24.0E-06 * cos2d - 112.0E-06 * cosmm )
	     + sinmm  * ( 9744.0E-06 - 1540.0E-06 * cos2d +  10.0E-06 * COSME )
	     + sin2mm * (  455.0E-06 -   49.0E-06 * cos2d ) )
         + sinf * (-3.0E-06 * cosd - 2454.0E-06 * cos2d - 60.0E-06 * cos2f
			  + 6.0E-06 * sinmm * SINME
             + sin2d * (-157.0E-06 * SINME+248.0E-06 * sinmm-79.0E-06 * sin2mm )
	     + COSME * (  -7.0E-06 -   53.0E-06 * cos2d )
	     + cosmm * (  50.0E-06 + 72.0E-06 * cos2d + 4.0E-06 * COSME )
	     + cos2mm * (147.0E-06 - 87.0E-06 * cos2d )
	     + 89473.0E-06 );
 SL = sin( LATMN ); CL = cos( LATMN );
 /* the following numerator is the inverse of the constant of sine 
    parallax for the moon, 3422.451 arc-sec, but iin radians. */
 MRi = 9999237.0 - 2858.0 * cosd
	+ sin2d * ( 6481.0 * SINME + sin2mm * (-1715.0 + 2144.0 * cos2d))
	+ sinmm * (-2083.0 * sin2f +  SINME * ( 6142.0 - 4067.0 * cos2d )
	  + sin2d * (91230.0 + 3260.0 * cos2d + 3027.0 * COSME))
	+ cos2d * (  82488.0 + 1526.0 * cos2d)
	+ COSME  * ( -1169.0 + 4727.0 * cos2d +   67.0 * cos2mm)
	+ cosmm  * (541309.0 - 2089.0 * cos2f + 3634.0 * cos2mm
	  + cos2d  * (109264.0         + 3764.0 * cos2d)
	  + SINME  * (  5691.0 * sin2d + 1350.0 * sinmm)
	  + COSME  * (   596.0         + 4091.0 * cos2d ))
        + cos2mm * ( 28598.0 + cos2d * ( -61.0 + 2204.0 * cos2d ));
  MR = REarth * 60.26816E+07 / MRi;
  myTrue[0] =  CL * cos( LONGMN );
  S = CL * sin( LONGMN );
  myTrue[1] =  ( S * COB - SL * SOB);
  Moon[2] = MR *  ( S * SOB + SL * COB );
  /* convert x and y components from true equinox to mean equinox of date
     so components are based on true equator and mean equinox. */
  Moon[0] = MR * ( myTrue[0] + myTrue[1] * NURA );
  Moon[1] = MR * ( myTrue[1] - myTrue[0] * NURA );
  return;
}
/*------------------------------------------------------------------------*/
