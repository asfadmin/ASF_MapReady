#include "CUnit/Basic.h"
#include "asf.h"

static int within_tol(double a, double b)
{
  return fabs(a-b) < .0001;
}

void test_complex()
{
  complexFloat c = complex_new(1,2);
  CU_ASSERT(c.real==1);
  CU_ASSERT(c.imag==2);
  complexFloat d = complex_add(c,complex_zero());
  CU_ASSERT(d.real==1);
  CU_ASSERT(d.imag==2);
  d = complex_sub(c,complex_new(-1,-2));
  CU_ASSERT(d.real==2);
  CU_ASSERT(d.imag==4);
  CU_ASSERT(within_tol(complex_amp(c),sqrt(5)));
  CU_ASSERT(within_tol(complex_amp(d),sqrt(20)));
  CU_ASSERT(within_tol(complex_amp_sqr(c),5));
  CU_ASSERT(within_tol(complex_amp_sqr(d),20));
  CU_ASSERT(within_tol(complex_amp(complex_scale(d,.5)),sqrt(5)));
  d = complex_conj(c);
  CU_ASSERT(d.real==1);
  CU_ASSERT(d.imag==-2);
  complexFloat e = complex_conj(complex_add(d,complex_conj(c)));
  CU_ASSERT(e.real==2);
  CU_ASSERT(e.imag==4);
  complexVector v = complex_vector_new(c,d,e);
  CU_ASSERT(v.A.real==1);
  CU_ASSERT(v.A.imag==2);
  CU_ASSERT(v.B.real==1);
  CU_ASSERT(v.B.imag==-2);
  CU_ASSERT(v.C.real==2);
  CU_ASSERT(v.C.imag==4);
  complexVector vc = complex_vector_conj(v);
  CU_ASSERT(vc.A.real==1);
  CU_ASSERT(vc.A.imag==-2);
  CU_ASSERT(vc.B.real==1);
  CU_ASSERT(vc.B.imag==2);
  CU_ASSERT(vc.C.real==2);
  CU_ASSERT(vc.C.imag==-4);
}

