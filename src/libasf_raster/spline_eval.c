#include <asf_raster.h>

double gsl_spline_eval_check(gsl_spline *s, double x, gsl_interp_accel *a)
{
  double x_orig = x;
  int len = s->size;
  if (x < s->x[0] || x > s->x[len-1]) {
    if (x < s->x[0]) {
       asfPrintStatus("Spline extrapolation: %.12f < %.12f\n",
                      x, s->x[0]);
       x = s->x[0];
    }
    else if (x > s->x[len-1]) {
       asfPrintStatus("Spline extrapolation: %.12f > %.12f\n",
                      x, s->x[len-1]);
       x = s->x[len-1];
    }
    if (fabs(x_orig - x) > .001) {
      asfPrintWarning("Attempt to extrapolate with spline: %.10f (%.10f, %.10f)\n",
                      x_orig, s->x[0], s->x[len-1]);
    }
  }

  return gsl_spline_eval(s, x, a);
}

