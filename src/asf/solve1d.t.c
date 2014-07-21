#include "CUnit/Basic.h"
#include "asf.h"

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
      // solution to f(x) = (x^3 - i) should be cube root of i
      CU_ASSERT(fabs(x-pow((double)i,1./3.))<.001);
    }
    else {
      // failed to solve
      CU_ASSERT(0);
    }
  }
}

