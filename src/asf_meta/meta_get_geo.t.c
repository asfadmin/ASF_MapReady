#include "CUnit/Basic.h"
#include "asf_meta.h"

static int within_tol(double a, double b, double tol)
{
  if (fabs(a) < tol) {
    if (fabs(b) < tol) {
      return TRUE;
    } else {
      printf("Out of tol, expected %g got %g\n", a, b);
      return FALSE;
    }
  } else {
    if (fabs((a-b)/b) < tol) {
      return TRUE;
    } else {
      printf("Out of tol, expected %g got %g\n", a, b);
      return FALSE;
    }
  }
}

static void test_mgll(meta_parameters *meta, double line, double samp,
                      double lat, double lon)
{
  // force calculation of these, if the metadata has a valid value
  // then the function call will use that as a shortcut
  //meta->sar->satellite_height = MAGIC_UNSET_DOUBLE;
  //meta->sar->earth_radius = MAGIC_UNSET_DOUBLE;

  double lat1, lon1, line1, samp1;

  meta_get_latLon(meta, line, samp, 0, &lat1, &lon1);
  CU_ASSERT(within_tol(lat,lat1,.01));
  CU_ASSERT(within_tol(lon,lon1,.01));

  meta_get_lineSamp(meta, lat1, lon1, 0, &line1, &samp1);
  CU_ASSERT(within_tol(line,line1,.2));
  CU_ASSERT(within_tol(samp,samp1,.2));

  if (!within_tol(lat,lat1,.01)   || !within_tol(lon,lon1,.01) ||
      !within_tol(line,line1,.2) || !within_tol(samp,samp1,.2)) {
    printf("(%g,%g) -> (%g,%g) -> (%g,%g)\n", 
           line, samp, lat1, lon1, line1, samp1);
  }

  double time, slant, dop;
  meta_get_timeSlantDop(meta,line,samp,&time,&slant,&dop);
  double time1 = meta_get_time(meta,line,samp);
  CU_ASSERT(within_tol(time,time1,.0001));
  double slant1 = meta_get_slant(meta,line,samp);
  CU_ASSERT(within_tol(slant,slant1,.1));

  // Why doesn't this work??
  //double dop1 = meta_get_dop(meta,line,samp);
  //CU_ASSERT(within_tol(dop,dop1,.0001));

  double sr = slant;
  double incid = meta_incid(meta,line,samp);

  double ht = meta_get_sat_height(meta,line,samp);
  double er = meta_get_earth_radius(meta,line,samp);

  double look = meta_look(meta,line,samp);

  CU_ASSERT(ht>er);
  CU_ASSERT(er+sr>ht);
  CU_ASSERT(within_tol(2.*er*sr*cos(PI-incid),sr*sr+er*er-ht*ht,.01));
  CU_ASSERT(within_tol(2.*sr*ht*cos(look),sr*sr+ht*ht-er*er,.01));

  slant1 = slant_from_incid(incid,er,ht);
  CU_ASSERT(within_tol(slant1,sr,.001));

  double look1 = look_from_incid(incid,er,ht);
  CU_ASSERT(within_tol(look1,look,.001));

  double look2, yaw;
  stateVector st = meta_get_stVec(meta, time);
  GEOLOCATE_REC *g = init_geolocate_meta(&st,meta);
  getLookYaw(g,sr,dop,&look2,&yaw);
  CU_ASSERT(within_tol(look,look2,.01));
  free_geolocate(g);

  meta_timeSlantDop2latLon(meta,time,sr,dop,0,&lat1,&lon1);
  CU_ASSERT(within_tol(lat,lat1,.01));
  CU_ASSERT(within_tol(lon,lon1,.01));

  slant1 = meta_get_slant(meta,line,0);
  CU_ASSERT(within_tol(slant1,meta->sar->slant_range_first_pixel,.001));
}

static void corner_test(const char *filename)
{
  meta_parameters *meta = meta_read(filename);
  meta_general *mg = meta->general;
  int nl = mg->line_count;
  int ns = mg->sample_count;
  meta_location *ml = meta->location;
  test_mgll(meta, 0, 0, ml->lat_start_near_range, ml->lon_start_near_range);
  test_mgll(meta, nl, 0, ml->lat_end_near_range, ml->lon_end_near_range);
  if (strcmp(meta->general->sensor, "ALOS")==0 && meta->general->orbit_direction=='A') {
    test_mgll(meta, nl, ns, ml->lat_start_far_range, ml->lon_start_far_range);
    test_mgll(meta, 0, ns, ml->lat_end_far_range, ml->lon_end_far_range);
  } else {
    test_mgll(meta, 0, ns, ml->lat_start_far_range, ml->lon_start_far_range);
    test_mgll(meta, nl, ns, ml->lat_end_far_range, ml->lon_end_far_range);
  }
  test_mgll(meta, nl/2, ns/2, mg->center_latitude, mg->center_longitude);
  meta_free(meta);
}

void test_meta_get_latLon()
{
  corner_test("test_input/ers1.meta");
  corner_test("test_input/palsar_fbd.meta");
}


void test_meta_get_lineSamp()
{
}

