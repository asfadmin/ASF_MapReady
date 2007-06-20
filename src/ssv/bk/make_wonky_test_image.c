// This program produces an image with a variety of interesting
// anomolies that we hope we can effectively deal with in the viewer.

#include <math.h>
#include <stdlib.h>

#include <float_image.h>

#define SOURCE_IMAGE "E116306133G1S009.img"
#define SIW 7951
#define SIH 8192
#define OUTPUT_IMAGE "E116306133G1S009_wonky.img"
#define SOURCE_META "E116306133G1S009.meta"
#define OUTPUT_META "E116306133G1S009_wonky.meta"

#ifndef NAN
#  error "We wants NANs. Giiiive us the NANs now (maybe define _GNU_SOURCE)."
#endif

#define SP(x, y, val) (float_image_set_pixel (new_i, x, y, val))

int
main (void)
{
  FloatImage *new_i
    = float_image_new_from_file (SIW, SIH, SOURCE_IMAGE, 0,
				 FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  guint ii, jj;			// Index variables.

  // Create a block of magical pixels in the upper left corner.
  const gfloat special_float = 42.0;
  for ( ii = 0 ; ii < 50 ; ii++ ) {
    for ( jj = 0 ; jj < 50 ; jj++ ) {
      SP (ii, jj, special_float);
    }
  }
  // And another in the lower right.
  for ( ii = SIW - 50 ; ii < SIW ; ii++ ) {
    for ( jj = SIH - 50 ; jj < SIH ; jj++ ) {
      SP (ii, jj, special_float);
    }
  }

  // Create a not-too-outrageously bright patch.
  const float bs_value = 520.0;
  const size_t bstl_x = 60, bstl_y = 60, bs_size = 20; 
  for ( ii = bstl_x ; ii < bstl_x + bs_size ; ii++ ) {
    for ( jj = bstl_y ; jj < bstl_y + bs_size ; jj++ ) {
      SP (ii, jj, bs_value);
    }
  }

  // Create a dark spot.
  const float ds_value = -200.0;
  const size_t dstl_x = 60, dstl_y = 400, ds_size = 20;
  g_assert (dstl_y > bstl_y + bs_size);
  for ( ii = dstl_x ; ii < dstl_x + ds_size ; ii++ ) {
    for ( jj = dstl_y ; jj < dstl_y + ds_size ; jj++ ) {
      SP (ii, jj, ds_value);
    }
  }

  // Create a little spot of Infinities.
  const size_t inf_x = 120, inf_y = 60, inf_size = 100;
  for ( ii = inf_x ; ii < inf_x + inf_size ; ii++ ) {
    for ( jj = inf_y ; jj < inf_y + inf_size ; jj++ ) {
      SP (ii, jj, INFINITY);
    }
  }

  // A little negative infinity for symmetry.
  const size_t ninf_x = 420, ninf_y = 60, ninf_size = 100;
  for ( ii = ninf_x ; ii < ninf_x + ninf_size ; ii++ ) {
    for ( jj = ninf_y ; jj < ninf_y + ninf_size ; jj++ ) {
      SP (ii, jj, -INFINITY);
    }
  }

  // A little patch o' NaNs.
  const size_t nan_x = 720, nan_y = 60, nan_size = 100;
  for ( ii = nan_x ; ii < nan_x + nan_size ; ii++ ) {
    for ( jj = nan_y ; jj < nan_y + nan_size ; jj++ ) {
      SP (ii, jj, NAN);
    }
  }

  // A couple patches of nearby huge values (for taming with a pixmap
  // range including both).
  const float hva = 42.0e27, hvb = 42.0e28;
  const size_t hva_x = 920, hva_y = 60, hva_size = 100;
  for ( ii = hva_x ; ii < hva_x + hva_size ; ii++ ) {
    for ( jj = hva_y ; jj < hva_y + hva_size ; jj++ ) {
      SP (ii, jj, hva);
    }
  }
  const size_t hvb_x = 1120, hvb_y = 60, hvb_size = 100;
  for ( ii = hvb_x ; ii < hvb_x + hvb_size ; ii++ ) {
    for ( jj = hvb_y ; jj < hvb_y + hvb_size ; jj++ ) {
      SP (ii, jj, hvb);
    }
  }  

  int return_code = float_image_store (new_i, OUTPUT_IMAGE,
				       FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  g_assert (return_code == 0);

  float_image_free (new_i);

  GString *system_command = g_string_new ("");
  g_string_append_printf (system_command, "cp %s %s", SOURCE_META,
                          OUTPUT_META);
  int exit_code = system (system_command->str);
  g_assert (exit_code == 0);
  g_string_free (system_command, TRUE);

  exit (EXIT_SUCCESS);
}
