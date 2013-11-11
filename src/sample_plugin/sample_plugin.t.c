#include "asf_meta.h"
#include "CUnit/Basic.h"

void test_sample_plugin1()
{
  int a = 0;
  int b = 1;

  //CU_ASSERT(a==b);
  CU_ASSERT(a+1==b);
}

void test_sample_plugin2()
{
  double a = 0;
  double b = 1;

  CU_ASSERT(FLOAT_COMPARE_TOLERANCE(a+1,b,.00001));
}

