#include "CUnit/Basic.h"
#include "asf_meta.h"

static int within_tol(double a, double b)
{
  static const double tol = .00001;
  if (fabs(a) < tol) {
    return fabs(b) < tol;
  } else {
    return fabs((a-b)/b) < tol;
  }
}

static void test_ers1()
{
  meta_parameters *meta = meta_read("test_input/ers1.meta");

  meta_general *mg = meta->general;
  CU_ASSERT(mg);
  CU_ASSERT(strcmp(mg->sensor, "ERS1")==0);
  CU_ASSERT(strcmp(mg->sensor_name, "SAR")==0);
  CU_ASSERT(mg->orbit == 22590);
  CU_ASSERT(mg->orbit_direction == 'D');
  CU_ASSERT(mg->line_count == 10);
  CU_ASSERT(mg->sample_count == 9);
  CU_ASSERT(within_tol(mg->center_latitude, 63.8448));
  CU_ASSERT(within_tol(mg->center_longitude, -144.9802));
  CU_ASSERT(within_tol(mg->bit_error_rate, 5.1e-6));

  meta_sar *ms = meta->sar;
  CU_ASSERT(ms);
  CU_ASSERT(ms->image_type == 'G');
  CU_ASSERT(ms->multilook == 1);
  CU_ASSERT(ms->look_direction == 'R');
  CU_ASSERT(ms->azimuth_look_count == 5);
  CU_ASSERT(ms->range_look_count == 1);
  CU_ASSERT(strcmp(ms->polarization, "VV")==0);
  CU_ASSERT(within_tol(ms->prf, 1679.9023438));
  CU_ASSERT(within_tol(ms->azimuth_processing_bandwidth, 1343.921875));
  CU_ASSERT(within_tol(ms->range_sampling_rate, 18959999.1));


  meta_free(meta);
}

void test_meta_read()
{
  test_ers1();
}

