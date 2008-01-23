
typedef struct	tagVECTOR {
	double x,y,z,w;
} VECTOR;

typedef struct tagSATELLITE {
	char	cSatelliteName[23];
	int		iSecondMeanMotion;
	int		iSatelliteNumber;
	int		iLaunchYear;
	int		iLaunchNumber;
	char	cLaunchPiece[3];
	int		iEpochYear;
	double	fEpochDay;
	int		iEpochDay;
	double	fEpochFraction;
	double	fBalisticCoefficient;
	double	fSecondMeanMotion;
	double	fRadiationCoefficient;
	char	cEmphemeristType[2];
	int		iElementNumber;

	double	fInclination;
	double	fRightAscending;
	double	fEccentricity;
	double	fPeregee;
	double	fMeanAnomaly;
	double	fMeanMotion;
	int		iRevAtEpoch;
	double  fJulianEpoch;
} SATELLITE;

#ifndef PI
#define PI 3.141592653589793
#endif

#ifdef TRUE
#undef TRUE
#endif
#define TRUE 1

#ifdef FALSE
#undef FALSE
#endif
#define FALSE 1

//#define M_PI PI
#define M2PI (2 * M_PI)
#define RTD (360 / M2PI)

#define REarth 6378140.0                /* meters of earth mean radius */
#define Ellipticity_of_Earth 0.00335281 /* Wertz p 99 eqn 4.13 */
#define AU 1.49597870E+11               /* meters to an astronomical unit*/
#define RMOON 1738.0E+03                /* moon radius in meters */
#define RSUN 696000.0E+03               /* sun radius in meters */
#define GMe 3.986005E+14                /* m^3/s^2 Wertz p 827 */ 
#define GMs 1.32712438E+20              /* m^3/s^2 Wertz p 827 */ 
#define GMm 4.902792965E+12             /* m^3/s^2 Note 74 */
#define m_to_ft 3.2808398950131234      /* feet per meter */
#define ft_to_nm 1.6457883369330454E-04 /* nautical miles per foot */
#define nm_to_m  1852.0                 /* meters per nautical mile */
#define speed_of_light 2.99792458E+08   /* m/s */
#define psi0_by_c 4.570E-06             /* N/m^2 Note 74: solar torque const */
#define rho0 2.1E-10                    /* kg/m^3 Note 73 */
#define h0 50000.0                      /* meters: scale factor for atmo dens*/
#define r0 240000.0                     /* altitude for rho0 */
#define lbm_to_kg 0.45359237            /* kg per lbm */ 
#define slug_to_kg 14.593902937206365   /* kg per slug */
#define gee 9.80665                     /* m/s^2 */

#define EARTHGEO 6.6103 /* radius of geosync if earth = 1.0 */
#define EARTHGEO_METERS (EARTHGEO*REarth) /* radius of geosync meters */

typedef struct tagCSGP4_SDP4 {
	char		m_cLine0[23], m_cLine1[70], m_cLine2[70];

	SATELLITE	m_Sat;
	VECTOR		m_vLLA;
	VECTOR		m_vPOS;
	VECTOR		m_vVEL;
	VECTOR		m_vUPos;
	VECTOR		m_vUVel;
	VECTOR		m_vObs;
	VECTOR		m_vRad;
	VECTOR		m_vSolarPosition;
	double		m_fTime;
	int		m_bLatLonAlt;
	int	m_bEclipsed;

} CSGP4_SDP4;

// this is a Windows struct that is duplicated here
typedef struct tagSYSTEMTIME {
  int wYear;
  int wMonth;
  int wDayOfWeek;
  int wDay;
  int wHour;
  int wMinute;
  int wSecond;
  int wMilliseconds;
} SYSTEMTIME;

/* math.c */
double Modulus(double a1,double a2);	// returns the remainder 
long Round (double arg);
double Fmod2p (double arg);
double AcTan(double sinx, double cosx);
double sqr (double arg);
void Magnitude (VECTOR *pVector);
double Dot (VECTOR v1, VECTOR v2);
double VecDot( double *X, double *Y, int N);
void VecCross( double *X, double *Y, double *Z, int N);
double VecMag( double *X, int N);
void UnitVec( double *X, double *Y, int N );
void VecDiff( double *X, double *Y, double *Z, int N);
void VecSum( double *X, double *Y, double *Z, int N);
void VecScale( double u, double *X, double *Y, int N);
double RadToDeg (double arg);			// converts radians to degree
double DegToRad (double arg);			// converts degree to radians

/* sgpsdp.c */
CSGP4_SDP4 *SGP4_SDP4_new_from_tle(char *cLine0, char *cLine1, char *cLine2);
int SGP(CSGP4_SDP4 *self, double time);
void CalculateLatLonAlt(CSGP4_SDP4 *self, double jdTime);

SATELLITE *GetSatellite(CSGP4_SDP4 *self);
VECTOR GetUserPos(CSGP4_SDP4 *self);
VECTOR GetUserVel(CSGP4_SDP4 *self);
VECTOR GetObserver(CSGP4_SDP4 *self);
VECTOR GetRADec(CSGP4_SDP4 *self);
VECTOR GetPos(CSGP4_SDP4 *self);
VECTOR GetVel(CSGP4_SDP4 *self);
double GetTime(CSGP4_SDP4 *self);
double GetLat(CSGP4_SDP4 *self);
double GetLon(CSGP4_SDP4 *self);
double GetAlt(CSGP4_SDP4 *self);

int GetVersion(char *pBuffer, int iMaxLen);

/*

class DLLEXPORT CSGP4_SDP4 //: public CObject
{                      
protected:	// the public accesible data members ...
	char		m_cLine0[23], m_cLine1[70], m_cLine2[70];

	SATELLITE	m_Sat;
	VECTOR		m_vLLA;
	VECTOR		m_vPOS;
	VECTOR		m_vVEL;
	VECTOR		m_vUPos;
	VECTOR		m_vUVel;
	VECTOR		m_vObs;
	VECTOR		m_vRad;
	VECTOR		m_vSolarPosition;
	double		m_fTime;
protected:
	BOOL		m_bLatLonAlt;
	// here are some constants
	BOOL	m_bEclipsed;
public:
	double		Modulus(double a1,double a2);	// returns the remainder 
	void		ConvertData();
	void		Init();
	long		round (double arg);
	double		Fmod2p (double arg);
	double		AcTan(double sinx, double cosx);
	double		sqr (double arg);
	void		Magnitude (VECTOR *pVector);
	double		Dot (VECTOR v1, VECTOR v2);

protected:
	// the two routines to calculate the satellite position ...
	BOOL SGP4(double tsince, int *iflag, VECTOR *pos, VECTOR *vel);
	BOOL SDP4(double tsince, int *iflag, VECTOR *pos, VECTOR *vel);
	// some subroutines used by SDP4 / SGP4
	void Call_dpper(CSGP4_SDP4 *self,double *e,double *xincc,double *omgadf,double *xnode,double *xmam);
	void Call_dpsec(CSGP4_SDP4 *self,double *xmdf,double *omgadf,double *xnode,double *emm,double *xincc,
					double *xnn,double *tsince);
	void Call_dpinit(CSGP4_SDP4 *self,double *eosq,double *sinio,double *cosio,double *betao,double *aodp,
					 double *theta2,double *sing,double *cosg,double *betao2,double *xmdot,
					 double *omgdot,double *xnodott,double *xnodpp);
	BOOL Deep(int ideep);
public:
	// the first constructor is for direct use with the Norad2Line elements
	CSGP4_SDP4 (char *m_cLine0, char *m_cLine1, char *m_cLine2);
	// this will be the preffered constructor, because there is no calculation for the Satellite needed
	CSGP4_SDP4 (SATELLITE *pSat);
	// this constructor is usefull for accessing the time/math - routines, without any sat data
	CSGP4_SDP4 ();      // the default constructor ...
	// and the destructor of course !!!
	~CSGP4_SDP4 ();
	// This both routines set a new satellite as datas
	void SetSatellite (char *m_cLine0, char *m_cLine1, char *m_cLine2, bool bInitSatellite = true);
	void SetSatellite (SATELLITE *pSat, bool bInitSatellite = true);
	void InitSatellite();

	// This routine will automatically determine, which routine to use (SDP4 / SGP4)
	// It is the main function of this DLL ...
	BOOL SGP(double time);
	// This routine will convert the pos and vel vector to units (km and km/sec)
	void ConvertSatState(VECTOR *pos,VECTOR *vel);
	// And this one calculate the Lattitude / Longitude and altitude of pos at time in ->pLLA
	void CalculateLatLonAlt(double time);
	VECTOR CalculateLatLonAlt(VECTOR vPos, double time);
	// Calculates the User pos and vel, and return m_vUPos and m_vUVel
	void CalculateUserPosVel(VECTOR *geodetic, double time);
	// Calculates the Observer data for a given Satellite and returns m_vObs
	BOOL CalculateObs(VECTOR pos, VECTOR vel, VECTOR geodetic, double time);
	// Calculates the Observer data in topocentric data and return m_vRad
	void CalculateRADec(VECTOR pos, VECTOR vel, VECTOR geodetic, double time);
 
	// Here ar the accesible routines ...
	double		GetFloat (int iStart, int iEnd, char *cLine);	// extracts a float form a string
	long		GetInt   (int iStart, int iEnd, char *cLine);	// extracts a int from a string
	char	   *GetString(int iStart, int iEnd, char *cLine);	// extracts a string from the string
	double		RadToDeg (double arg);			// converts radians to degree
	double		DegToRad (double arg);			// converts degree to radians
	double		JulianDate (SYSTEMTIME st);		// returns the julian date from SYSTEMTIME
	double		JulianDate (CTime ct);			// returns the julian date from CTime
	double		JulianDate (double st);			// returns the julian date from the Norad2Line epoch style
	double 		JulianDateOfYear (int yr);		// returns the JD of a given year
	int 		EpocheYear (int iYear);			// returns 99 for 1999 and 32 for 2032
	int 		DayOfYear (int, int, int);		// returns the day of yeart of a given date.
	double 		FractionOfDay(int, int, int);	// returns the fraction of a day.
	double		ThetaG(double jd);				// returns the sideral GMT
	SYSTEMTIME	CalendarDate (double dJulian);	// returns the date from a julian date
	double		SideralTime(double jd);			// returns the sideral time at jd
	VECTOR		GetPos();		// returns the calculated Position vector
	VECTOR		GetVel();		// returns the calculated Velocity vector
	double		GetLat();		// returns the calculated Lattitude 
	double		GetLon();		// returns the calculated Longitude
	double		GetAlt();		// returns the calculated Altitude
	double		GetTime();		// returns the actual time
	VECTOR		GetUserPos();	// returns the calculated Position vector
	VECTOR		GetUserVel();	// returns the calculated Velocity vector
	VECTOR		GetObserver();	// returns the calculated Position vector
	VECTOR		GetRADec();		// returns the calculated Velocity vector
	BOOL		GetEclipsed();	// returns the boolean variable, calculated by DepthOfEclipse ...
	static int	GetVersion(char *pBuffer, int iMaxLen);	// returns a pointer to the version of this dll
	void	   *GetSatellite();	// returns a pointer to th actual satellite

	VECTOR 		CalculateSolarPosition(double time);	// Calculates the actual Sun position ...
	double 		DepthOfEclipse(double time, VECTOR r1);	// Calculates if the Satellite is in an eclipse or visible ...
	int 		Eclipse( double *r, VECTOR *vSun, VECTOR *vMoon, int *em, int *ee, char **which );
	double 		MoonPhase( double *Sun, double *Moon, double *Observer );
	void 		SunMoon( double T, double *Sun, double *Moon );

	double VecDot( double *X, double *Y, int N);
	void VecCross( double *X, double *Y, double *Z, int N);
	double VecMag( double *X, int N);
	void UnitVec( double *X, double *Y, int N );
	void VecDiff( double *X, double *Y, double *Z, int N);
	void VecSum( double *X, double *Y, double *Z, int N);
	void VecScale( double u, double *X, double *Y, int N);
};




*/
