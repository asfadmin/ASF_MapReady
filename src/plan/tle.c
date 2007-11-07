#include "plan_internal.h"
#include <time.h>

#define GM 3.9860045e+14         // Gravitational constant [m^3 / s^2].
#define ECC 8.1827385e-02        // GEM6 model eccentricity of the Earth.

typedef struct {
  double a;                     // Semi-major axis.
  double e;                     // Eccentricity.
  double i;                     // Inclination.
  double cap_omega;             // Longitude of ascending node.
  double omega;                 // Argument of perigee.
  double ea;                    // Eccentric anomaly.
} keplerian_orbit_t;

typedef struct {
  double x, y, z, vx, vy, vz;
} cartesian_orbit_t;

typedef struct {
  int year;                     // Year
  int day;                      // Julian day
  double time;                  // Time of the day (seconds)
  double inclination;           // Orbital inclination
  double cap_omega;             // Right ascension of the ascending node
  double eccentricity;          // Eccentricity
  double omega;                 // Argument of perigee
  double mean_anomaly;          // Mean anomaly
  double mean_motion;           // Mean motion
  int rev_at_epoch;             // Revolution number at epoch
} tle_t;

static double
kepler_equation (double mean_anomaly, double eccentricity)
{
  double tol1 = 1e-5, tol2 = 1e-8, ea, sin_ea, cos_ea, dea, a;
  double ma = mean_anomaly*D2R, ecc = eccentricity;
  double abm = fabs (mean_anomaly*D2R);
  int k = 0;

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

  return ea*R2D;
}

static void
keplerian_to_cartesian (keplerian_orbit_t *ko, cartesian_orbit_t *co)
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

  radius = ko->a * (1 - ko->e * cos(ko->ea));

  co->x = ko->a * td[0] * (cos(ko->ea) - ko->e) + td[6] * td[3] * sin(ko->ea);
  co->y = ko->a * td[1] * (cos(ko->ea) - ko->e) + td[6] * td[4] * sin(ko->ea);
  co->z = ko->a * td[2] * (cos(ko->ea) - ko->e) + td[6] * td[5] * sin(ko->ea);
  co->vx = ko->a * td[7]
    * (td[6] * cos(ko->ea) * td[3] - ko->a * sin(ko->ea) * td[0]) / radius;
  co->vy = ko->a * td[7]
    * (td[6] * cos(ko->ea) * td[4] - ko->a * sin(ko->ea) * td[1]) / radius;
  co->vz = ko->a * td[7]
    * (td[6] * cos(ko->ea) * td[5] - ko->a * sin(ko->ea) * td[2]) / radius;
}

static void read_tle_file(const char *tleFile, const char *satellite,
                          tle_t *tle)
{
  FILE *fp;
  char line[512], *s;

  s = (char *) MALLOC(sizeof(char)*25);
  fp = FOPEN(tleFile, "r");
  while (fgets(line, 512, fp)) {
    if (strncmp(line, satellite, strlen(satellite))==0) {

      fgets(line, 512, fp);

      // Epoch year
      strncpy(s, &line[18], 3);
      s[2] = 0;
      tle->year = atoi(s);
      if (tle->year > 90)
	tle->year += 1900;
      else
	tle->year += 2000;
      
      // Epoch day
      strncpy(s, &line[20], 13);
      s[12] = 0;
      tle->day = atoi(s);

      // Epoch time
      tle->time = (atof(s) - tle->day) * 86400;

      fgets(line, 512, fp);
      
      // Orbital inclination
      strncpy(s, &line[8], 9);
      s[8] = 0;
      tle->inclination = atof(s);

      // Right ascension of the ascending node
      strncpy(s, &line[17], 9);
      s[8] = 0;
      tle->cap_omega = atof(s);

      // Eccentricity
      sprintf(s, "0.");
      strncpy(&s[2], &line[26], 8);
      s[9] = 0;
      tle->eccentricity = atof(s);

      // Argument of the perigee
      strncpy(s, &line[34], 9);
      s[8] = 0;
      tle->omega = atof(s);

      // Mean anomaly
      strncpy(s, &line[43], 9);
      s[8] = 0;
      tle->mean_anomaly = atof(s);

      // Mean motion
      strncpy(s, &line[52], 12);
      s[11] = 0;
      tle->mean_motion = atof(s);
      
      // Revolution number at epoch
      strncpy(s, &line[63], 6);
      s[5] = 0;
      tle->rev_at_epoch = atoi(s);
    }
  }
  
  // Clean up
  FCLOSE(fp);
  FREE(s);
}

void read_tle(const char *tle_filename, const char *satellite,
              stateVector *st, double *st_time)
{
  tle_t *tle = MALLOC(sizeof(tle_t));
  keplerian_orbit_t *ko = MALLOC(sizeof(keplerian_orbit_t));
  cartesian_orbit_t *co = MALLOC(sizeof(cartesian_orbit_t));

  // Read TLE file
  read_tle_file(tle_filename, satellite, tle);

  printf("tle: year= %d\n", tle->year);
  printf("tle: jday= %d\n", tle->day);
  printf("tle: time= %f\n", tle->time);

  // Keplerian elements
  double period = 86400 / tle->mean_motion;
  double velocity = pow(2*M_PI*GM/period, 1.0/3.0);
  ko->a = GM/velocity/velocity;
  ko->e = tle->eccentricity;
  ko->i = tle->inclination;
  ko->cap_omega = tle->cap_omega;
  ko->omega = 90.0;
  ko->ea = kepler_equation(tle->mean_anomaly, tle->eccentricity);

  // Cartesian coordinates
  keplerian_to_cartesian(ko, co);

  st->pos.x = co->x;
  st->pos.y = co->y;
  st->pos.z = co->z;
  st->vel.x = co->vx;
  st->vel.y = co->vy;
  st->vel.z = co->vz;

  // mucking around with the time
  julian_date jd;
  hms_time t;

  jd.year = tle->year;
  jd.jd = tle->day;

  double tm = tle->time;
  int h = (int)floor(tm/24.);
  tm -= h*24;
  int m = (int)floor(tm/60.);
  tm -= m*60;

  t.hour = h;
  t.min = m;
  t.sec = tm;

  *st_time = date2sec(&jd, &t);

  FREE(co);
  FREE(ko);
  FREE(tle);
}
