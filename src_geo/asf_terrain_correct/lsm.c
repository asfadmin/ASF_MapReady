#include "lsm.h"

#include <glib.h>
#include <stdio.h>

#include <gsl/gsl_min.h>

#include "float_image.h"
#include "asf_nan.h"

// #define KH_DEBUG

int
lsm_mask_value_is_layover(int mask_value)
{
  return 
    mask_value == MASK_ACTIVE_LAYOVER_VALUE ||
    mask_value == MASK_PASSIVE_LAYOVER_VALUE;
}

int
lsm_mask_value_is_shadow(int mask_value)
{
  return 
    mask_value == MASK_ACTIVE_SHADOW_VALUE ||
    mask_value == MASK_PASSIVE_SHADOW_VALUE;
}

int
lsm_image_mask_value_is_layover(FloatImage *mask, int x, int y)
{
  return lsm_mask_value_is_layover(float_image_get_pixel(mask, x, y));
}

int
lsm_image_mask_value_is_shadow(FloatImage *mask, int x, int y)
{
  return lsm_mask_value_is_shadow(float_image_get_pixel(mask, x, y));
}

FloatImage *
lsm_generate_mask(DEMGeomInfo *dgi)
{
  g_assert(dgi);

  g_assert(dgi->cp_target_z);
  g_assert(dgi->slant_range_value);
  g_assert(dgi->imaging_time);
  g_assert(dgi->satellite_height);
  g_assert(dgi->dem_height);
  g_assert(dgi->nadir_distance);

  g_assert(dgi->size_x > 0);
  g_assert(dgi->size_y > 0);

  int x, y;

  int nshad = 0;
  int nlay = 0;
  int nnorm = 0;

  FloatImage *mask = float_image_new (dgi->size_x, dgi->size_y);
  FloatImage *A_vals = float_image_new (dgi->size_x, dgi->size_y);
  FloatImage *R_vals = float_image_new (dgi->size_x, dgi->size_y);

  for (y = 0; y < dgi->size_y; ++y) {

    FILE * f = NULL;

#ifdef KH_DEBUG
    if (y%100 == 0 && y>0) {
      char fname[64];
      sprintf(fname, "lsm%04d.dat", y);
      f = fopen(fname, "wt");
    }
#endif

    for (x = 0; x < dgi->size_x; ++x) {
      lsm_mask_value_t pixel_value;

      float_image_set_pixel(A_vals, x, y, -1);

      if (y == 0 || y == dgi->size_y - 1 || 
	  x == 0 || x == dgi->size_x - 1) {
	pixel_value = MASK_NORMAL_VALUE;
	++nnorm;
      } 
      else {
	if (dem_geom_info_get_slant_range_value(dgi, x, y) < 0) {
	  pixel_value = MASK_NORMAL_VALUE;
	  ++nnorm;
	}
	else {
	  double dx = ( dem_geom_info_get_x(dgi, x + 1, y) -
			dem_geom_info_get_x(dgi, x - 1, y) );
	  
	  double dy = ( dem_geom_info_get_y(dgi, x, y + 1) -
			dem_geom_info_get_y(dgi, x, y - 1) );
	  
	  if (dx == 0 || dy == 0) {
	    pixel_value = MASK_NORMAL_VALUE;
	    ++nnorm;
#ifdef KH_DEBUG
	    //printf(" Hmm - dx,dy = %g,%g at %d,%d \n", dx, dy, x, y);
#endif
	  }
	  else {

	    double z = dem_geom_info_get_dem_height(dgi, x, y);
	    double d = dem_geom_info_get_nadir_distance(dgi, x, y);

	    double zw = dem_geom_info_get_dem_height(dgi, x - 1, y);
	    double ze = dem_geom_info_get_dem_height(dgi, x + 1, y);
	    double zn = dem_geom_info_get_dem_height(dgi, x, y - 1);
	    double zs = dem_geom_info_get_dem_height(dgi, x, y + 1);

	    double tw = dem_geom_info_get_imaging_time(dgi, x - 1, y);
	    double te = dem_geom_info_get_imaging_time(dgi, x + 1, y);
	    double tn = dem_geom_info_get_imaging_time(dgi, x, y - 1);
	    double ts = dem_geom_info_get_imaging_time(dgi, x, y + 1);

	    double dd = (ze - zw)/dx * (te - tw)/dx + 
                	(zn - zs)/dy * (tn - ts)/dy;

	    double z_s = dem_geom_info_get_satellite_height(dgi, x, y);

	    double R = d - (z_s - z) * dd;
	    double A = (z_s - z) + d * dd;

	    if (A <= 0) {
	      ++nshad;
	      pixel_value = MASK_ACTIVE_SHADOW_VALUE;
	    }
	    else if (R <= 0) {
	      ++nlay;
	      pixel_value = MASK_ACTIVE_LAYOVER_VALUE;
	    }
	    else {
	      ++nnorm;
	      pixel_value = MASK_NORMAL_VALUE;
	    }

	    float_image_set_pixel(A_vals, x, y, A);
	    float_image_set_pixel(R_vals, x, y, R <= 0 ? 1.0 : 0.0);

	    if (f)
	      fprintf(f, "%d %g %g %g %g %d \n", x, d, z, A, R, pixel_value);
	  }
	}
      }

      float_image_set_pixel(mask, x, y, pixel_value);
    }

    if (f)
      fclose(f);
  }

  printf("Out of %d pixels:\n %12d shadow\n %12d layover\n %12d other\n",
	 nshad + nlay + nnorm, nshad, nlay, nnorm);

  int pscount = 0;
  int plcount = 0;

  for (y = 0; y < dgi->size_y; ++y) {
    int prev = MASK_NORMAL_VALUE;
    for (x = 0; x < dgi->size_x; ++x) {

      int current = (int) float_image_get_pixel(mask, x, y);
      if (current != prev) {
	
	// 2 cases: entering a region of layover/shadow, or leaving
	if (current == MASK_ACTIVE_LAYOVER_VALUE) {
	  // entering layover region

	  // scan ahead to find the extent of the layover
	  double sr_curr = dem_geom_info_get_slant_range_value(dgi, x, y);
	  double sr_next = sr_curr, sr_prev = sr_curr;

	  int xx = x;
	  while (++xx < dgi->size_x) {
	    if ( (int) float_image_get_pixel(mask, xx, y) !=
		 MASK_ACTIVE_LAYOVER_VALUE)
	      break;

	    sr_next = dem_geom_info_get_slant_range_value(dgi, xx, y);

	    if (sr_next > sr_prev)
	      break;

	    sr_prev = sr_next;
	  }

	  double sr_found = sr_prev;
	  int ahead = xx;

	  // scan back to find matching point
	  xx = x;
	  while (--xx > 0) {
	    sr_prev = dem_geom_info_get_slant_range_value(dgi, xx, y);

	    if (sr_prev < sr_found)
	      break;

	    // can't have a larger passive region than active
	    if (x - xx > ahead - x)
	      break;
	  }

	  int back = xx;

#ifdef KH_DEBUG
	  if (y > 4000) {
	    g_print("Extremum found %d pixels ahead (%d): %g\n",
		    ahead - x, ahead, sr_found);

	    g_print("Matching point was %d pixels behind (%d): %g\n",
		    x - back, back, sr_prev);

	    int bk = back - 5;
	    if (bk < 0) bk = 0;

	    int ah = ahead + 5;
	    if (ah > dgi->size_x - 1) ah = dgi->size_x - 1;

	    if (ah - bk < 50) {
	      int kkk;
	      for (kkk = bk; kkk < ah; ++kkk) {
		printf("%2s %5d %12g\n",
		       kkk == back || kkk == ahead ? "*" : "",
		       kkk, dem_geom_info_get_slant_range_value(dgi, kkk, y));
	      }
	    } else {
	      printf("Too many to list.\n");
	    }
	  }
#endif

	  for (xx = back; xx < x; ++xx) {
	    if ((int)float_image_get_pixel(mask, xx, y) == MASK_NORMAL_VALUE) {
	      float_image_set_pixel(mask, xx, y, MASK_PASSIVE_LAYOVER_VALUE);
	      ++plcount;
	    }
	  }

	  x = ahead + 1;
	}
	else if (prev == MASK_ACTIVE_SHADOW_VALUE) {
	  // leaving shadow region

#ifdef KH_DEBUG
	  g_print("Ho! Leaving shadow ... y = %d, x = %d\n", y, x);
#endif
	  // scan back to find the extent of the shadow
	  int xx = x;
	  while (--xx > dgi->size_y) {
	    if ( (int) float_image_get_pixel(mask, xx, y) !=
		 MASK_ACTIVE_SHADOW_VALUE)
	      break;
	  }

	  double A_found = float_image_get_pixel(A_vals, xx, y);

#ifdef KH_DEBUG
	  if (y > 4000) {
	    g_print("Extend found %d pixels behind (%d): %g\n",
		    x - xx, xx, A_found);
	  }
#endif

	  int back = xx;

	  double A_next = -1;

	  // scan ahead to find the A crossing
	  xx = x;
	  while (++xx < dgi->size_x - 1) {
	    A_next = float_image_get_pixel(A_vals, xx, y);

	    if (A_next > A_found)
	      break;

	    // can't have a larger passive region than active
	    if (xx - x > x - back)
	      break;
	  }

	  int ahead = xx;

#ifdef KH_DEBUG
	  if (y > 4000) {
	    g_print("Matching point was %d pixels ahead (%d): %g\n",
		    xx - x, xx, A_next);
	  }

	  if (y > 4000) {
	    int bk = back - 5;
	    if (bk < 0) bk = 0;

	    int ah = ahead + 5;
	    if (ah > dgi->size_x - 1) ah = dgi->size_x - 1;

	    if (ah - bk < 50) {
	      int kkk;
	      for (kkk = bk; kkk < ah; ++kkk) {
		printf("%2s %5d %12g\n",
		       kkk == back || kkk == ahead ? "*" : "",
		       kkk, float_image_get_pixel(A_vals, kkk, y));
	      }
	    } else {
	      printf("Too many to list.\n");
	    }
	  }
#endif

	  for (xx = x; xx < ahead; ++xx) {
	    if ((int)float_image_get_pixel(mask, xx, y) == MASK_NORMAL_VALUE) {
	      float_image_set_pixel(mask, xx, y, MASK_PASSIVE_SHADOW_VALUE);
	      ++pscount;
	    }
	  }
	}
	else if (prev == MASK_ACTIVE_LAYOVER_VALUE) {
	  // leaving layover region

	}
	else if (current == MASK_ACTIVE_SHADOW_VALUE) {
	  // entering shadow region

	}
	else {
	  printf("Impossible: current = %d, prev = %d\n", current, prev);
	  g_assert(FALSE);
	}
      }

      prev = current;
    }
  }
  
  printf("Added %d passive shadow, %d passive layover pixels.\n", 
	 pscount, plcount);

#ifdef KH_DEBUG
  float_image_set_pixel(A_vals, 0, 0, 0);
  float_image_set_pixel(A_vals, 1, 1, 1);

  float_image_set_pixel(R_vals, 0, 0, 0);
  float_image_set_pixel(R_vals, 1, 1, 1);

  float_image_set_pixel(mask, 0, 0, 0);
  float_image_set_pixel(mask, 1, 1, 1);

  float_image_export_as_jpeg (A_vals, "a_vals.jpg", 800, NAN);
  float_image_free(A_vals);

  float_image_export_as_jpeg (R_vals, "r_vals.jpg", 800, NAN);
  float_image_free(R_vals);

  float_image_export_as_jpeg (mask, "lsm1k.jpg", 800, NAN);
  float_image_free(mask);
#endif

  return mask;
}

void
lsm_test()
{
  int size_x = 800;
  int size_y = 800;

  DEMGeomInfo * dgi = dem_geom_info_new(size_x, size_y);

  int x, y;

  for (y = 0; y < size_y; ++y) {
    for (x = 0; x < size_x; ++x) {
      double imaging_time = (double)x / (double)size_x * 20;
      double slant_range_value = 1;
      double satellite_height = 5e+06;

      double h = (x - size_x/2.0) / (size_x/2.0);
      double f = 1 + (double)y / (double)size_y * 20;
      double dem_height = 1000.0 * f * exp ( - f * h*h );

//      printf("h = %g dh = %g\n", h, dem_height);

      Vector * poca = vector_new(0, -y * 1e+03, satellite_height);
      Vector * cp_target = vector_new(-x * 1e+03, -y * 1e+03, 0);

      dem_geom_info_set(dgi, x, y, cp_target, imaging_time,
			slant_range_value, dem_height, poca);

      vector_free(poca);
      vector_free(cp_target);
    }
  }

  float_image_export_as_jpeg(dgi->dem_height, "dem.jpg", 800, NAN);

  lsm_generate_mask(dgi);
  dem_geom_info_free(dgi);

  exit(0);
}
