#include <stdio.h>
#include <dateUtil.h>
#include <assert.h>
#include "asf_meta.h"
#include "sgpsdp.h"

double secs_to_jul(double t)
{
  julian_date jd;
  hms_time hms;
  ymd_date ymd;

  sec2date(t, &jd, &hms);
  date_jd2ymd(&jd, &ymd);

  assert(ymd.year>=1900);
  assert(ymd.day>=1 && ymd.day<=31);
  assert(ymd.month>=1&&ymd.month<=12);

  double jdoy = Julian_Date_of_Year(ymd.year);
  double doy = DOY(ymd.year,ymd.month,ymd.day);
  double fod = hms.sec/(60.*60.*24.) + hms.min/(60.*24.) + hms.hour/24.0;

  double ret = jdoy + doy + fod;
  return ret;
}

double jul_to_secs(double jul)
{
  julian_date jd;
  hms_time hms;
  ymd_date ymd;

  time_t jtime = (jul-2440587.5)*86400.;
  struct tm *stm = gmtime(&jtime);

  hms.hour = stm->tm_hour;
  hms.min = stm->tm_min;
  hms.sec = stm->tm_sec;

  ymd.year = stm->tm_year+1900;
  ymd.day = stm->tm_mday;
  ymd.month = stm->tm_mon;

  assert(ymd.year>=1900);
  assert(ymd.day>=1 && ymd.day<=31);
  assert(ymd.month>=1 && ymd.month<=12);

  date_ymd2jd(&ymd,&jd);
  return date2sec(&jd,&hms);
}

double time_to_secs(int year, int doy, double fod)
{
  julian_date jd;
  hms_time hms;

  jd.year=year;
  jd.jd=doy;

  hms.hour=hms.min=0;
  hms.sec=0.0;

  double t=date2sec(&jd,&hms);
  return t+fod*60*60*24;
}

stateVector tle_propagate(sat_t *sat, double t)
{
  sat->jul_utc = secs_to_jul(t);
  sat->tsince = (sat->jul_utc - sat->jul_epoch) * xmnpda;

  SGP4(sat, sat->tsince);

  Convert_Sat_State(&sat->pos, &sat->vel);
  Magnitude(&sat->vel);
  sat->velo = sat->vel.w;

  //geodetic_t obs_geodetic;
  //obs_set_t obs_set;
  //obs_geodetic.lat = obs_geodetic.lon = obs_geodetic.alt = 0;
  //obs_geodetic.theta = 0;
  //Calculate_Obs(sat->jul_utc, &sat->pos, &sat->vel, &obs_geodetic, &obs_set);

  geodetic_t sat_geodetic;
  Calculate_LatLonAlt(sat->jul_utc, &sat->pos, &sat_geodetic);

  while (sat_geodetic.lon < -PI)
    sat_geodetic.lon += twopi;
  while (sat_geodetic.lon > PI)
    sat_geodetic.lon -= twopi;

  //sat->az = Degrees (obs_set.az);
  //sat->el = Degrees (obs_set.el);
  //sat->range = obs_set.range;
  //sat->range_rate = obs_set.range_rate;
  sat->ssplat = Degrees (sat_geodetic.lat);
  sat->ssplon = Degrees (sat_geodetic.lon);
  sat->alt = sat_geodetic.alt;
  sat->ma = Degrees (sat->phase);
  sat->ma *= 256.0/360.0;
  sat->phase = Degrees (sat->phase);

  sat->footprint = 12756.33 * acos (xkmper / (xkmper+sat->alt));
  double age = sat->jul_utc - sat->jul_epoch;

  // I really don't know if we need the -1 in here or not!!
  int revnum = sat->tle.revnum; // - 1;

  double orbit = (sat->tle.xno * xmnpda/twopi +
                  age * sat->tle.bstar * ae) * age + sat->tle.xmo/twopi;

  sat->orbit = (long) floor(orbit) + revnum;
  sat->orbit_part = orbit + revnum - (double)sat->orbit;

  stateVector st;

  st.pos.x = sat->pos.x * 1000;
  st.pos.y = sat->pos.y * 1000;
  st.pos.z = sat->pos.z * 1000;
  st.vel.x = sat->vel.x * 1000;
  st.vel.y = sat->vel.y * 1000;
  st.vel.z = sat->vel.z * 1000;

  julian_date jd;
  hms_time hms;
  sec2date(t,&jd,&hms);
  double gha = utc2gha(jd.year,jd.jd,hms.hour,hms.min,hms.sec);
  gei2fixed(&st,gha);

  return st;
}

void read_tle(const char *tle_filename, const char *satellite, sat_t *sat)
{
  FILE *fp = fopen(tle_filename, "r");
  char tle_str[3][80];
  if (fp) {

    while (1) {

      if (!fgets(tle_str[0], 80, fp)) break;
      if (!fgets(tle_str[1], 80, fp)) break;
      if (!fgets(tle_str[2], 80, fp)) break;

      if (strncmp(tle_str[0], satellite, strlen(satellite)) == 0) {
        // found
        int ret = Get_Next_Tle_Set (tle_str, &sat->tle);

        if (ret != 1) {
          printf("Error reading '%s' from %s\n", satellite, tle_filename);
          break;
        }
        else {
          // good satellite data obtained
          sat->flags = 0;
          select_ephemeris(sat);

          sat->jul_utc = 0.0;
          sat->tsince = 0.0;
          sat->az = 0.0;
          sat->el = 0.0;
          sat->range = 0.0;
          sat->range_rate = 0.0;
          sat->ra = 0.0;
          sat->dec = 0.0;
          sat->ssplat = 0.0;
          sat->ssplon = 0.0;
          sat->alt = 0.0;
          sat->velo = 0.0;
          sat->ma = 0.0;
          sat->footprint = 0.0;
          sat->phase = 0.0;
          sat->aos = 0.0;
          sat->los = 0.0;

          sat->jul_epoch = Julian_Date_of_Epoch(sat->tle.epoch);

          // calculate the satellite's position at t=0 (epoch time)
          SGP4 (sat, 0.0);

          // scale position and velocity to km and km/sec
          Convert_Sat_State (&sat->pos, &sat->vel);

          // get the velocity of the satellite
          Magnitude (&sat->vel);
          sat->velo = sat->vel.w;

          geodetic_t sat_geodetic;
          Calculate_LatLonAlt (sat->jul_utc, &sat->pos, &sat_geodetic);

          while (sat_geodetic.lon < -PI)
            sat_geodetic.lon += twopi;
	
          while (sat_geodetic.lon > (PI))
            sat_geodetic.lon -= twopi;

          sat->ssplat = Degrees (sat_geodetic.lat);
          sat->ssplon = Degrees (sat_geodetic.lon);
          sat->alt = sat_geodetic.alt;
          sat->ma = Degrees (sat->phase);
          sat->ma *= 256.0/360.0;
          sat->footprint = 2.0 * xkmper * acos (xkmper/sat->pos.w);
          double age = 0.0;

          double orbit = (sat->tle.xno * xmnpda/twopi +
                          age * sat->tle.bstar * ae) * age + 
                          sat->tle.xmo/twopi;
          sat->orbit = (long) floor(orbit) + sat->tle.revnum/*-1*/;
          sat->orbit_part = orbit + sat->tle.revnum/*-1*/ - (double)sat->orbit;

          // orbit type
          sat->otype = ORBIT_TYPE_UNKNOWN;

          fclose(fp);
          return;
        }
      }
    }

    // reached end of file without finding TLE
    printf("Couldn't find '%s' in %s.\n", satellite, tle_filename);
  }
  else { // failed to open TLE file
    printf("Couldn't read: %s\n", tle_filename);
  }
}

const char *get_tle_info(const char *tle_filename, const char *satellite)
{
    sat_t sat;
    read_tle(tle_filename, satellite, &sat);

    static char tle_info[256];

    double t = time_to_secs(sat.tle.epoch_year,
                            sat.tle.epoch_day,
                            sat.tle.epoch_fod);

    double now = seconds_from_long(current_date());
    int age = (now - t) / (24*60*60);

    if (age==0) {
      sprintf(tle_info,
              "Satellite: %s\n"
              "Date: %s\n"
              "Current",
              satellite, date_str_long(t));
    }
    else {
      sprintf(tle_info,
              "Satellite: %s\n"
              "Date: %s\n"
              "%d day%s old",
              satellite, date_str_long(t), age, age==1?"":"s");
    }

    return tle_info;
}
