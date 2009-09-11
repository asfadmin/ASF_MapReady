#include "asf.h"
#include <assert.h>

static int sign(double x)
{
  return x>0 ? 1 : -1;
}

// Simple Newton's method solver
int solve1d(solve1d_fn *f, void *params, int min_x, int max_x, double acc,
            double *root)
{
  double x0 = min_x, x1 = max_x;
  double f0 = f(params, x0);
  double f1 = f(params, x1);
  int niter = 0;

  if (sign(f0)==sign(f1)) {
    // what should we do here?
    // look around for a crossing
    printf("Looking for a region containing a root...\n");
    int i,n=100;
    double d = (max_x - min_x)/(double)n;
    double x_prev = min_x;
    double f_prev = f0;
    int found = FALSE;
    for (i=1; i<n; ++i) {
      double x_next = x_prev + d;
      double f_next = f(params, x_next);
      if (sign(f_next) != sign(f_prev)) {
        // found an interval with a root
        found = TRUE;
        x0 = x_prev;
        x1 = x_next;
        f0 = f_prev;
        f1 = f_next;
        break;
      }
    }
    if (!found) {
      printf("Failed to find an interval containing a root.\n");
      return FALSE;
    }
    else {
      printf("Searching for a root in (%f,%f)\n",x0,x1);
    }
  }
  else {
    x0 = min_x;
    x1 = max_x;
  }

  assert(sign(f0)!=sign(f1));

  while (niter < 1000) {
    double xm = (x0+x1)/2.;
    double fm = f(params, xm);

    if (sign(fm)==sign(f0)) {
      // look in xm,x1 region
      x0 = xm;
      f0 = fm;
    } 
    else {
      // look in x0,xm region
      x1 = xm;
      f1 = fm;
    }

    if (fabs(x1-x0) < acc) {
      //printf("Converged after %d iterations.\n", niter);
      *root = xm;
      return TRUE;
    }

    ++niter;
  }

  //printf("Failed to converge after %d iterations.\n", niter);
  return FALSE;
}

/* Simple test program, that also illustrates how the thing is used */
typedef struct test_params
{
    double d;
} test_params_t;

static double test_fn(void *params, double x)
{
  test_params_t *p = (test_params_t*)params;
  return x*x*x - p->d;
}

void test_solve1d()
{
  int i;
  for (i=0; i<100; ++i) {
    test_params_t p;
    p.d = (double)i;
    double x;
    if (solve1d(test_fn, (void*)(&p), 0, 200, 0.000005, &x)) {
      printf("%d %.5f %.5f %.8f\n", i, x, pow(i,1./3.),x-pow(i,1./3.));
    }
    else {
      printf("%d -- failed\n", i);
    }
  }
}
