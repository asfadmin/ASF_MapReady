#include "asf_raster.h"
#include "asf_meta.h"
#include "asf.h"
#include "float_image.h"

#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <assert.h>
#include <stdlib.h>
#include <time.h>

#define DEG_TO_RAD 0.0174532925199432958
#define ANGLE 5 * DEG_TO_RAD
#define X_ORIGIN 127.5
#define Y_ORIGIN 127.5

#define SET_PIXEL(x, y, value) float_image_set_pixel (outbuf, x, y, value)

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#define SQR(X) ((X)*(X))

static const double tol = 0.1;
int within_tol(double a, double b)
{
    return fabs(b) < tol ? fabs(a) < tol : fabs((a-b)/b) < tol;
}



void rotation_test(interpolate_type_t mode, weighting_type_t f)
{
  // Interpolation check implemented after Helmut Dersch's experiment
  // http://www.path.unimelb.edu.au/~dersch/interpolator/interpolator.html

  int ii, kk, nn, nl, ns, ret;
  FloatImage *inbuf, *outbuf;
  float value, xShift, yShift, x0, y0, x1, y1, a00, a01, a10, a11;
  meta_parameters *meta;
  char cmd[255], file[255];

  meta = meta_read("test256_float");
  nl = meta->general->line_count;
  ns = meta->general->sample_count;

  if (mode == BILINEAR) {
    printf("Bilinear interpolation (rotation test)\n");
    sprintf(file, "bilinear.img");
  }
  else if (mode == BICUBIC) {
    printf("Bicubic interpolation (rotation test)\n");
    sprintf(file, "bicubic_5.img");
  }
  else if (mode == SINC && f == NO_WEIGHT) {
    printf("Sinc interpolation (rotation test)\n");
    sprintf(file, "sinc.img");
  }
  else if (mode == SINC && f == HAMMING) {
    printf("Sinc interpolation with Hamming weighting (rotation test)\n");
    sprintf(file, "sinc_hamming.img");
  }
  else if (mode == SINC && f == KAISER) {
    printf("Sinc interpolation with Kaiser weighting (rotation test)\n");
    sprintf(file, "sinc_kaiser.img");
  }
  else if (mode == SINC && f == LANCZOS) {
    printf("Sinc interpolation with Lanczos weighting (rotation test)\n");
    sprintf(file, "sinc_lanczos.img");
  }
  inbuf = float_image_new_from_file(nl, ns, "test256_float.img", 0,
					FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  // 36 rotations five degrees each
  for (nn=1; nn<=1; nn++) {
    if (nn == 1)
      inbuf = float_image_new_from_file(nl, ns, "test256_float.img", 0,
					FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
    else {
      inbuf = float_image_new_from_file(nl, ns, file, 0,
					FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
    }
    outbuf = float_image_new (nl, ns);
    a00 = cos(ANGLE);
    a01 = -sin(ANGLE);
    a10 = sin(ANGLE);
    a11 = cos(ANGLE);
    x0 = a00*X_ORIGIN + a01*Y_ORIGIN;
    y0 = a10*X_ORIGIN + a11*Y_ORIGIN;
    xShift = X_ORIGIN - x0;
    yShift = Y_ORIGIN - y0;
    for (ii=3; ii<nl-2; ii++) {
      x0 = a01*(float)ii + xShift;
      y0 = a11*(float)ii + yShift;
      for (kk=3; kk<ns-2; kk++) {
        x1 = x0 + a00*(float)kk;
        y1 = y0 + a10*(float)kk;
	if (sqrt(SQR(x1-X_ORIGIN) + SQR(y1-Y_ORIGIN))>127.5) SET_PIXEL(ii, kk, 0.0);
	else {
	  if (mode == BILINEAR) {
	    value = interpolate(BILINEAR, inbuf, y1, x1, NO_WEIGHT, 8);
	  }
          else if (mode == BICUBIC) {
            value = interpolate(BICUBIC, inbuf, y1, x1, NO_WEIGHT, 8);
          }
	  else if (mode == SINC && f == NO_WEIGHT) {
	    value = interpolate(SINC, inbuf, y1, x1, NO_WEIGHT, 8);
	  }
	  else if (mode == SINC && f == HAMMING) {
	    value = interpolate(SINC, inbuf, y1, x1, HAMMING, 8);
	  }
	  else if (mode == SINC && f == KAISER) {
	    value = interpolate(SINC, inbuf, y1, x1, KAISER, 8);
	  }
	  else if (mode == SINC && f == LANCZOS) {
	    value = interpolate(SINC, inbuf, y1, x1, LANCZOS, 8);
	  }
	  SET_PIXEL(ii, kk, value);
	}
      }
    }
    // Let's see how many circles are left
    if (mode == BILINEAR) {
     ret = float_image_store(outbuf, file, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
     sprintf(cmd, "cp test256_float.meta bilinear.meta");
    }
    else if (mode == BICUBIC) {
      sprintf(file, "bicubic_%i.img", (int)(nn*ANGLE*180/M_PI +0.5));
      ret = float_image_store(outbuf, file, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
      sprintf(cmd, "cp test256_float.meta bicubic_%i.meta",
	      (int)(nn*ANGLE*180/M_PI +0.5));
    }
    else if (mode == SINC && f == NO_WEIGHT) {
      ret = float_image_store(outbuf, file, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
      sprintf(cmd, "cp test256_float.meta sinc.meta");
    }
    else if (mode == SINC && f == HAMMING) {
      ret = float_image_store(outbuf, file, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
      sprintf(cmd, "cp test256_float.meta sinc_hamming.meta");
    }
    else if (mode == SINC && f == KAISER) {
      ret = float_image_store(outbuf, file, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
      sprintf(cmd, "cp test256_float.meta sinc_kaiser.meta");
    }
    else if (mode == SINC && f == LANCZOS) {
      ret = float_image_store(outbuf, file, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
      sprintf(cmd, "cp test256_float.meta sinc_lanczos.meta");
    }
    system(cmd);
    float_image_free(inbuf);
    float_image_free(outbuf);
  }
}

int main(int argc, char * argv [])
{

  /*
  rotation_test(BILINEAR, NO_WEIGHT);
  rotation_test(BICUBIC, NO_WEIGHT);
  rotation_test(SINC, NO_WEIGHT);
  rotation_test(SINC, HAMMING);
  rotation_test(SINC, KAISER);
  rotation_test(SINC, LANCZOS);
  */
  
  return 0;
}
