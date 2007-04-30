#include "asf_baseline.h"

void determine_baseline(char *sensor, int track, struct base_info *srf, 
			struct base_pair *pairs, int nPairs)
{
  struct base_info *current_srf, master, slave, *p, *q;
  GEOLOCATE_REC *g;
  stateVector stVec1, stVec2;
  julian_date jd1, jd2;
  hms_time hms1, hms2;
  vector target, up, relPos, upBeam, alongBeam, beamNormal;
  char time1[30], time2[30];
  int  i, beginYear, endYear, sign, new;
  double lat, phi, earthRadius, Bp, Bn, dt, re, rp;



  // Line up all the needed information
  for (i=0; i<nPairs; i++) {
    
    current_srf = srf;
    new = TRUE;
    int found = FALSE;
    while (current_srf->next) {
      if (current_srf->next && !new)
	current_srf = current_srf->next;
      if (new)
	new = FALSE;
      if (pairs[i].master == current_srf->orbit && 
	  pairs[i].frame == current_srf->frame) {
	master = *current_srf;
	sprintf(pairs[i].sensor, "%s", sensor);
	pairs[i].track = track;
	if (current_srf->orbit_dir == 'A')
	  sprintf(pairs[i].orbit_dir, "Ascending");
	else if (current_srf->orbit_dir == 'D')
	  sprintf(pairs[i].orbit_dir, "Descending");
	else
	  sprintf(pairs[i].orbit_dir, "n/a");
	sprintf(pairs[i].m_time, "%s", current_srf->time);
	pairs[i].c_lat = current_srf->c_lat;
	pairs[i].c_lon = current_srf->c_lon;
	pairs[i].ns_lat = current_srf->ns_lat;
	pairs[i].ns_lon = current_srf->ns_lon;
	pairs[i].fs_lat = current_srf->fs_lat;
	pairs[i].fs_lon = current_srf->fs_lon;
	pairs[i].ne_lat = current_srf->ne_lat;
	pairs[i].ne_lon = current_srf->ne_lon;
	pairs[i].fe_lat = current_srf->fe_lat;
	pairs[i].fe_lon = current_srf->fe_lon;
	found = TRUE;
	break;
      }
    }
   
    if (!found) {
      asfPrintWarning("orbit: %d, frame: %d not found!!!!!!!!!1\n", 
		    pairs[i].master, pairs[i].frame);
      continue;
    }
    found = FALSE;
    current_srf = srf;
    new = TRUE;
    while (current_srf->next) {
      if (current_srf->next && !new)
	current_srf = current_srf->next;
      if (new)
	new = FALSE;
      if (pairs[i].slave == current_srf->orbit &&
	  pairs[i].frame == current_srf->frame) {
	slave = *current_srf;
	pairs[i].s_seq = current_srf->seq;
	sprintf(pairs[i].s_time, "%s", current_srf->time);
	found = TRUE;
	break;
      }
    } 

    if (!found) {
      asfPrintWarning("orbit: %d, frame: %d not found!!!!!!!!!1\n", 
		    pairs[i].master, pairs[i].frame);
      continue;
    }

    // Get the information out of the structs
    stVec1.pos.x = master.x;
    stVec1.pos.y = master.y;
    stVec1.pos.z = master.z;
    stVec1.vel.x = master.vx;
    stVec1.vel.y = master.vy;
    stVec1.vel.z = master.vz;
    stVec2.pos.x = slave.x;
    stVec2.pos.y = slave.y;
    stVec2.pos.z = slave.z;
    stVec2.vel.x = slave.vx;
    stVec2.vel.y = slave.vy;
    stVec2.vel.z = slave.vz;
    sprintf(time1, "%s", master.time);
    sprintf(time2, "%s", slave.time);
    
    // Scale state vector position to [m] and velocity to [m/s] 
    vecScale(&stVec1.pos, 1000);
    vecScale(&stVec2.pos, 1000);
    
    // Get inertial state vectors into earth-fixed.
    sscanf(time1, "%4d-%3dT%2d:%2d:%lf", 
	   &jd1.year, &jd1.jd, &hms1.hour, &hms1.min, &hms1.sec);
    gei2fixed(&stVec1, utc2gha(jd1.year, jd1.jd, hms1.hour, hms1.min, hms1.sec));
    sscanf(time2, "%4d-%3dT%2d:%2d:%lf", 
	   &jd2.year, &jd2.jd, &hms2.hour, &hms2.min, &hms2.sec);
    gei2fixed(&stVec2, utc2gha(jd2.year, jd2.jd, hms2.hour, hms2.min, hms2.sec));
    
    // Target is the patch of ground at beam center.
    // Initialize the transformation.
    g = init_geolocate(&stVec1);
    g->lambda = 0.0565646;
    lat = master.c_lat*D2R;
    rp = g->rp;
    re = g->re;
    g->earth_radius = 
      (re*rp) / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
    getLoc(g, master.range, master.doppler, &lat, &phi, &earthRadius);
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
    
    // Now we have the second satellite sitting in the plane of the first's beam,
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
    
    // Assign return values
    pairs[i].b_perp = (int) (Bn+0.5);
    pairs[i].b_par = (int) (Bp+0.5);
    if (dt > 0)
      pairs[i].b_temp = (int) (dt+0.5);
    else
      pairs[i].b_temp = (int) (dt-0.5);
  }

  // Clean up the scan results file structures
  p = srf;
  while (p) {
    q = p->next;
    FREE(p);
    p = q;
  }
}
