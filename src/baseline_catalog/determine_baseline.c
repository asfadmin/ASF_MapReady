#include "asf_baseline.h"

void determine_baseline(char *sensor, int track, struct srf_orbit *srf, int nOrbits,
			struct base_pair *pairs, int *nPairs)
{
  GEOLOCATE_REC *g;
  stateVector stVec1, stVec2;
  julian_date jd1, jd2;
  hms_time hms1, hms2;
  vector target, up, relPos, upBeam, alongBeam, beamNormal;
  char time1[30], time2[30];
  int  i, j, k=0, beginYear, endYear, sign;
  double lat, phi, earthRadius, Bp, Bn, dt, re, rp;

  pairs = (struct base_pair *) MALLOC(sizeof(struct base_pair)*SIZE);

  for (i=0; i<nOrbits; i++) { // Master image
    for (j=0; j<nOrbits; j++) { // Slave image
      
      if(srf[i].orbit != srf[j].orbit && srf[i].frame == srf[j].frame) {
	sprintf(pairs[k].sensor, "%s", sensor);
	pairs[k].track = track;
	pairs[k].frame = srf[i].frame;
	if (srf[i].orbit_dir == 'A')
	  sprintf(pairs[k].orbit_dir, "Ascending");
	else if (srf[i].orbit_dir == 'D')
	  sprintf(pairs[k].orbit_dir, "Descending");
	else
	  sprintf(pairs[k].orbit_dir, "n/a");
	pairs[k].master = srf[i].orbit;
	pairs[k].m_seq = srf[i].seq;
	sprintf(pairs[k].m_time, "%s", srf[i].time);
	pairs[k].slave = srf[j].orbit;
	pairs[k].s_seq = srf[j].seq;
	sprintf(pairs[k].s_time, "%s", srf[j].time);
	pairs[k].c_lat = srf[i].c_lat;
	pairs[k].c_lon = srf[i].c_lon;
	pairs[k].ns_lat = srf[i].ns_lat;
	pairs[k].ns_lon = srf[i].ns_lon;
	pairs[k].fs_lat = srf[i].fs_lat;
	pairs[k].fs_lon = srf[i].fs_lon;
	pairs[k].ne_lat = srf[i].ne_lat;
	pairs[k].ne_lon = srf[i].ne_lon;
	pairs[k].fe_lat = srf[i].fe_lat;
	pairs[k].fe_lon = srf[i].fe_lon;

	// Get the information out of the structs
	stVec1.pos.x = srf[i].x;
	stVec1.pos.y = srf[i].y;
	stVec1.pos.z = srf[i].z;
	stVec1.vel.x = srf[i].vx;
	stVec1.vel.y = srf[i].vy;
	stVec1.vel.z = srf[i].vz;
	stVec2.pos.x = srf[j].x;
	stVec2.pos.y = srf[j].y;
	stVec2.pos.z = srf[j].z;
	stVec2.vel.x = srf[j].vx;
	stVec2.vel.y = srf[j].vy;
	stVec2.vel.z = srf[j].vz;
	sprintf(time1, "%s", srf[i].time);
	sprintf(time2, "%s", srf[j].time);
	
	// Scale state vector position to [m] and velocity to [m/s] 
	vecScale(&stVec1.pos, 1000);
	vecScale(&stVec2.pos, 1000);
	
	// Get inertial state vectors into earth-fixed.
	sscanf(time1, "%4d-%3dT%2d:%2d:%lf", 
	       &jd1.year, &jd1.jd, &hms1.hour, &hms1.min, &hms1.sec);
	gei2fixed(&stVec1, 
		  utc2gha(jd1.year, jd1.jd, hms1.hour, hms1.min, hms1.sec));
	sscanf(time2, "%4d-%3dT%2d:%2d:%lf", 
	       &jd2.year, &jd2.jd, &hms2.hour, &hms2.min, &hms2.sec);
	gei2fixed(&stVec2,
		  utc2gha(jd2.year, jd2.jd, hms2.hour, hms2.min, hms2.sec));
	
	// Target is the patch of ground at beam center.
	// Initialize the transformation.
	g = init_geolocate(&stVec1);
	g->lambda = 0.0565646;
	lat = srf[i].c_lat*D2R;
	rp = g->rp;
	re = g->re;
	g->earth_radius = 
	  (re*rp) / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
	getLoc(g, srf[i].range, srf[i].doppler, 
	       &lat, &phi, &earthRadius);
	free_geolocate(g);
	sph2cart(earthRadius, lat, phi, &target);
	
	// Create beam plane unit vectors
	vecSub(stVec1.pos, target, &alongBeam);
	vecNormalize(&alongBeam);
	up = stVec1.pos;
	vecNormalize(&up);
	vecCross(up, alongBeam, &beamNormal);
	vecNormalize(&beamNormal);
	vecCross(alongBeam, beamNormal, &upBeam);
	vecNormalize(&upBeam);
	
	// Now we have the second satellite sitting in the plane of the first beam,
	// so we just separate that position into components along-beam and 
	// across-beam, and return.
	vecSub(stVec2.pos, stVec1.pos, &relPos);
	Bp = vecDot(alongBeam, relPos);
	Bn = vecDot(upBeam, relPos);
	
	// Calculate temporal baseline
	dt = 0;
	if (jd1.year < jd2.year) {
	  beginYear = jd1.year;
	  endYear = jd2.year;
	  sign = 1;
	}
	else {
	  beginYear = jd2.year;
	  endYear = jd1.year;
	  sign = -1;
	}
	while (beginYear<endYear)
	  dt += date_getDaysInYear(beginYear++)*sign;
	dt += jd2.jd-jd1.jd;
	
	// Assign baseline values
	pairs[k].b_perp = (int) (Bn+0.5);
	pairs[k].b_par = (int) (Bp+0.5);
	if (dt > 0)
	  pairs[k].b_temp = (int) (dt+0.5);
	else
	  pairs[k].b_temp = (int) (dt-0.5);

	k++;
      }
    }
  }
  *nPairs = k;
}
