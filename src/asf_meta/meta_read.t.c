#include "CUnit/Basic.h"
#include "asf_meta.h"

int is_valid_ll2s_transform(meta_parameters *meta);

static int within_tol(double a, double b)
{
  static const double tol = .00001;
  if (fabs(a) < tol) {
    return fabs(b) < tol;
  } else {
    return fabs((a-b)/b) < tol;
  }
}

static void test_palsar_fbd_values(meta_parameters *meta)
{
  meta_general *mg = meta->general;
  CU_ASSERT(mg!=NULL);
  CU_ASSERT(strcmp(mg->sensor, "ALOS")==0);
  CU_ASSERT(strcmp(mg->sensor_name, "SAR")==0);
  CU_ASSERT(strcmp(mg->processor, "JAXA")==0);
  CU_ASSERT(mg->data_type == REAL32);
  CU_ASSERT(mg->band_count == 2);
  CU_ASSERT(strcmp(mg->bands, "HH,HV")==0);
  CU_ASSERT(mg->orbit == 4161);
  CU_ASSERT(mg->frame == 1270);
  CU_ASSERT(mg->orbit_direction == 'A');
  CU_ASSERT(mg->line_count == 13);
  CU_ASSERT(mg->sample_count == 14);
  CU_ASSERT(mg->missing_lines == MAGIC_UNSET_INT);
  CU_ASSERT(isnan(mg->no_data));
  CU_ASSERT(!meta_is_valid_double(mg->no_data));
  CU_ASSERT(within_tol(mg->x_pixel_size, 5000));
  CU_ASSERT(within_tol(mg->y_pixel_size, 5000));
  CU_ASSERT(strcmp(mg->acquisition_date, "05-Nov-2006, 07:54:51")==0);
  CU_ASSERT(mg->image_data_type == POLARIMETRIC_IMAGE);

  meta_sar *ms = meta->sar;
  CU_ASSERT(ms!=NULL);
  CU_ASSERT(ms->image_type == 'R');
  CU_ASSERT(ms->multilook == 1);
  CU_ASSERT(ms->look_direction == 'R');
  CU_ASSERT(ms->azimuth_look_count == 4);
  CU_ASSERT(ms->range_look_count == 1);
  CU_ASSERT(strcmp(ms->polarization, "dual-pol")==0);
  CU_ASSERT(within_tol(ms->prf, 1901.1406844));
  CU_ASSERT(within_tol(ms->azimuth_processing_bandwidth, 1507.4234619));
  CU_ASSERT(within_tol(ms->range_sampling_rate, 16000000));
  CU_ASSERT(within_tol(ms->wavelength, .2360571));
  CU_ASSERT(within_tol(ms->azimuth_time_per_pixel, 0.73204225396));
  CU_ASSERT(within_tol(ms->slant_range_first_pixel, 959888.17474));

  meta_state_vectors *sv = meta->state_vectors;
  CU_ASSERT(sv!=NULL);
  CU_ASSERT(sv->num == 9);
  CU_ASSERT(sv->year == 2006);
  CU_ASSERT(sv->julDay == 309);
  CU_ASSERT(within_tol(sv->second, 29400));
  CU_ASSERT(within_tol(sv->vecs[0].time, -226.99321985));
  CU_ASSERT(within_tol(sv->vecs[4].time, 13.006780148));
  CU_ASSERT(within_tol(sv->vecs[8].time, 253.00678015));
  CU_ASSERT(within_tol(sv->vecs[0].vec.pos.x, -4036112.634));
  CU_ASSERT(within_tol(sv->vecs[1].vec.pos.y, -2066475.038));
  CU_ASSERT(within_tol(sv->vecs[2].vec.pos.z, 5845116.379));
  CU_ASSERT(within_tol(sv->vecs[6].vec.vel.x, 5347.021));
  CU_ASSERT(within_tol(sv->vecs[7].vec.vel.y, 4811.737));
  CU_ASSERT(within_tol(sv->vecs[8].vec.vel.z, 1464.557));

  meta_transform *mt = meta->transform;
  CU_ASSERT(mt!=NULL);
  CU_ASSERT(strcmp(mt->type,"ground")==0);
  CU_ASSERT(mt->parameter_count == 25);
  CU_ASSERT(is_valid_ll2s_transform(meta));
  CU_ASSERT(within_tol(mt->l[0],4.9998944122));
  CU_ASSERT(within_tol(mt->l[24],2798.7429814));
  CU_ASSERT(within_tol(mt->s[0],0.92978217885));
  CU_ASSERT(within_tol(mt->s[24],2667.73047));
  CU_ASSERT(within_tol(mt->y[0],-1.5831374102e-34));
  CU_ASSERT(within_tol(mt->y[24],-144.78983784));
  CU_ASSERT(within_tol(mt->x[0],3.3677672314e-34));
  CU_ASSERT(within_tol(mt->x[24],63.932119174));
  CU_ASSERT(within_tol(mt->source_pixel_size, 12.5));
  CU_ASSERT(!meta_is_valid_double(mt->target_pixel_size));
  CU_ASSERT(within_tol(mt->origin_pixel, 2800));
  CU_ASSERT(within_tol(mt->origin_line, 2650));
  CU_ASSERT(within_tol(mt->origin_lat, 63.93013135));
  CU_ASSERT(within_tol(mt->origin_lon, -144.78949137));

  meta_location *ml = meta->location;
  CU_ASSERT(ml!=NULL);
  CU_ASSERT(within_tol(ml->lat_start_near_range, 63.5908));
  CU_ASSERT(within_tol(ml->lon_start_near_range, -145.3895));
  CU_ASSERT(within_tol(ml->lat_start_far_range, 63.6821));
  CU_ASSERT(within_tol(ml->lon_start_far_range, -143.9921));
  CU_ASSERT(within_tol(ml->lat_end_near_range, 64.1778 ));
  CU_ASSERT(within_tol(ml->lon_end_near_range, -145.6022));
  CU_ASSERT(within_tol(ml->lat_end_far_range, 64.2709));
  CU_ASSERT(within_tol(ml->lon_end_far_range, -144.1758));

  meta_calibration *mc = meta->calibration;
  CU_ASSERT(mc!=NULL);
  CU_ASSERT(mc->type == alos_cal);

  CU_ASSERT(meta->projection==NULL);
}

static void test_ers1_values(meta_parameters *meta)
{
  meta_general *mg = meta->general;
  CU_ASSERT(mg!=NULL);
  CU_ASSERT(strcmp(mg->sensor, "ERS1")==0);
  CU_ASSERT(strcmp(mg->sensor_name, "SAR")==0);
  CU_ASSERT(mg->orbit == 22590);
  CU_ASSERT(mg->frame == 290);
  CU_ASSERT(mg->orbit_direction == 'D');
  CU_ASSERT(mg->line_count == 10);
  CU_ASSERT(mg->sample_count == 9);
  CU_ASSERT(within_tol(mg->center_latitude, 63.8448));
  CU_ASSERT(within_tol(mg->center_longitude, -144.9802));
  CU_ASSERT(within_tol(mg->bit_error_rate, 5.1e-6));
  CU_ASSERT(strcmp(mg->mode, "STD")==0);
  CU_ASSERT(strcmp(mg->bands, "AMP")==0);
  CU_ASSERT(mg->band_count == 1);
  CU_ASSERT(strcmp(mg->acquisition_date, "09-Nov-1995, 20:55:30")==0);
  CU_ASSERT(mg->image_data_type == AMPLITUDE_IMAGE);
  CU_ASSERT(mg->radiometry == r_AMP);
  CU_ASSERT(mg->data_type == REAL32);
  CU_ASSERT(strcmp(mg->basename, "E122590290G1U014.D")==0);
  CU_ASSERT(mg->missing_lines == MAGIC_UNSET_INT);
  CU_ASSERT(isnan(mg->no_data));
  CU_ASSERT(!meta_is_valid_double(mg->no_data));
  CU_ASSERT(within_tol(mg->x_pixel_size, 10000));
  CU_ASSERT(within_tol(mg->y_pixel_size, 10000));

  meta_sar *ms = meta->sar;
  CU_ASSERT(ms!=NULL);
  CU_ASSERT(ms->image_type == 'G');
  CU_ASSERT(ms->multilook == 1);
  CU_ASSERT(ms->look_direction == 'R');
  CU_ASSERT(ms->azimuth_look_count == 5);
  CU_ASSERT(ms->range_look_count == 1);
  CU_ASSERT(strcmp(ms->polarization, "VV")==0);
  CU_ASSERT(within_tol(ms->prf, 1679.9023438));
  CU_ASSERT(within_tol(ms->azimuth_processing_bandwidth, 1343.921875));
  CU_ASSERT(within_tol(ms->range_sampling_rate, 18959999.1));
  CU_ASSERT(within_tol(ms->wavelength, .05656));
  CU_ASSERT(within_tol(ms->azimuth_time_per_pixel, -1.509016447));
  CU_ASSERT(within_tol(ms->slant_range_first_pixel, 836838.0737));

  meta_state_vectors *sv = meta->state_vectors;
  CU_ASSERT(sv!=NULL);
  CU_ASSERT(sv->num == 3);
  CU_ASSERT(sv->year == 1995);
  CU_ASSERT(sv->julDay == 313);
  CU_ASSERT(within_tol(sv->second, 75322.359375));
  CU_ASSERT(within_tol(sv->vecs[0].time, 0));
  CU_ASSERT(within_tol(sv->vecs[1].time, 7.7261657715));
  CU_ASSERT(within_tol(sv->vecs[2].time, 15.452331543));
  CU_ASSERT(within_tol(sv->vecs[0].vec.pos.x, -2423929.812));
  CU_ASSERT(within_tol(sv->vecs[1].vec.pos.y, -2129578.323));
  CU_ASSERT(within_tol(sv->vecs[2].vec.pos.z, 6342249.023));
  CU_ASSERT(within_tol(sv->vecs[0].vec.vel.z, -3160.138));
  CU_ASSERT(within_tol(sv->vecs[1].vec.vel.x, -6502.118));
  CU_ASSERT(within_tol(sv->vecs[2].vec.vel.y, -2049.095));

  meta_location *ml = meta->location;
  CU_ASSERT(ml!=NULL);
  CU_ASSERT(within_tol(ml->lat_start_near_range, 63.2752));
  CU_ASSERT(within_tol(ml->lon_start_near_range, -144.2827));
  CU_ASSERT(within_tol(ml->lat_start_far_range, 63.5193));
  CU_ASSERT(within_tol(ml->lon_start_far_range, -146.2513));
  CU_ASSERT(within_tol(ml->lat_end_near_range, 64.1602));
  CU_ASSERT(within_tol(ml->lon_end_near_range, -143.6801));
  CU_ASSERT(within_tol(ml->lat_end_far_range, 64.4100));
  CU_ASSERT(within_tol(ml->lon_end_far_range, -145.7110));

  meta_calibration *mc = meta->calibration;
  CU_ASSERT(mc!=NULL);
  CU_ASSERT(mc->type == asf_cal);
  CU_ASSERT(mc->asf->sample_count == 7942);
  CU_ASSERT(within_tol(mc->asf->a0, 4.6944799));
  CU_ASSERT(within_tol(mc->asf->a1, 0.0013470502));
  CU_ASSERT(within_tol(mc->asf->a2, 0));
  CU_ASSERT(within_tol(mc->asf->noise[0], 0.2723663));
  CU_ASSERT(within_tol(mc->asf->noise[100], 0.2351995));
  CU_ASSERT(within_tol(mc->asf->noise[200], 0.2528235));
  CU_ASSERT(within_tol(mc->asf->noise[255], 0.4099058));

  CU_ASSERT(meta->transform==NULL);
  CU_ASSERT(meta->projection==NULL);
}

typedef void test_function(meta_parameters *);

static void test_meta(const char *meta_file, test_function *test_fn)
{
  meta_parameters *meta = meta_read(meta_file);
  test_fn(meta);
  meta_write(meta, "tmp.meta");
  meta_free(meta);
  meta = meta_read("tmp.meta");
  unlink("tmp.meta");
  test_fn(meta);
  meta_parameters *mc = meta_copy(meta);
  meta_free(meta);
  test_fn(mc);
  meta_free(mc);
}

static void test_ers1()
{
  test_meta("test_input/ers1.meta", test_ers1_values);
  test_meta("test_input/palsar_fbd.meta", test_palsar_fbd_values);
}

void test_meta_read()
{
  test_ers1();
}

