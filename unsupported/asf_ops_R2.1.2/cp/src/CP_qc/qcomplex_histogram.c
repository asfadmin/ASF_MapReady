#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qcomplex_histogram_c[] = "@(#)qcomplex_histogram.c	1.11 97/02/12 09:05:16";

/* 
 * This function fills up the histogram array with data values 
 * from the entire picture (complex pictures only) 
 */

void
create_histogram_from_complex_picture(byte_data) 
   byte *byte_data;
{
   /* 
    * to be set up: _low_pixel_stretch_value, _high_pixel_stretch_value,
    * _histogram_min, _histogram_max, _data_mean, _data_sdev 
    */
   int   total_size, i;

   float tmp1, tmp2, accumulator = 0.0;

   if ((_x_zoom_offset >= 0) || (_y_zoom_offset >= 0))
      return;

   /* First off, clear out the histogram array */
   for (i = 0; i < 256 ; i++)
      _processed_histogram_array[i] = 0;

   total_size = ((_image_size_x / _image_scale_factor) *
		 (_image_size_y / _image_scale_factor));
    
   _processed_histogram_size = 256;

   /* First, put together _histogram_array */
   for (i = 0; i < total_size; i++) {
      /* exclude zero into histogram */
      if (byte_data[i] != 0) {
         _processed_histogram_array [ (int) byte_data[i] ]++;
      }
   }

   /* Next, find the low stretch value... */
   for (i = 0; i < 255 ; i++ ) {
      if (_processed_histogram_array[i] != 0) {
	 _low_pixel_stretch_value = i;
	 break;
      }
   }

   /* Then, find the high stretch value... */
   for (i = 255; i > -1 ; i-- ) {
      if (_processed_histogram_array[i] != 0) {
	 _high_pixel_stretch_value = i;
	 break;
      }
   }

   _histogram_max = -1;      /* initialize to the opposite */
   _histogram_min = 999999;

   /* Start at one, until we can cut out the excess black pixels ... */
   for (i = 1; i < 256 ; i++) {
      if (_processed_histogram_array[i] > _histogram_max) {
	 _histogram_max = _processed_histogram_array[i];
	 _histogram_max_location = i;
      }

      if (_processed_histogram_array[i] < _histogram_min) {
	 _histogram_min = _processed_histogram_array[i];
	 _histogram_min_location = i;
      }
    }

   /* Finally, compute the mean / standard deviation... */
   tmp1 = 0.0;
   tmp2 = 0.0;

   /* exclude the zero element of histogram into maen and sdev */
   for (i = 1; i < 256; i++) {
      tmp1 += (float) (_processed_histogram_array[i] * (i * i));
      tmp2 += (float) (_processed_histogram_array[i] * i);
      /* This is used to compute the mean .... */
      accumulator += (float) _processed_histogram_array[i];
   }

   _data_mean = tmp2 / accumulator;

   _data_sdev = fsqrt ((tmp1 / accumulator) - (_data_mean * _data_mean));

   /* Now figure out the data min and max */
   for (i = 0; i < 256 ; i++) {
      if (_processed_histogram_array[i] != 0) {
	 _data_min = i;
	 break;
      }
   }

   for (i = 255; i >= 0 ; i--) {
      if (_processed_histogram_array[i] != 0) {
 	 _data_max = i;
         break;
      }
   }
}

