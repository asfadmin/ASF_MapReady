#include "CUnit/Basic.h"

void xml_test(int *num_ok, int *num_bad);
void test_xml()
{
  int n_ok, n_bad;
  xml_test(&n_ok, &n_bad);

  CU_ASSERT(n_bad==0);
}

