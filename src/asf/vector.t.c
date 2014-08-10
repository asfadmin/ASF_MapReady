#include "CUnit/Basic.h"
#include "vector.h"
#include "asf.h"

static int within_tol(double a, double b)
{
  return fabs(a-b) < .0001;
}

void test_vector()
{
  Vector *v = vector_new(1,2,3);
  CU_ASSERT(v->x == 1);
  CU_ASSERT(v->y == 2);
  CU_ASSERT(v->z == 3);

  Vector *vc = vector_copy(v);
  CU_ASSERT(vc->x == 1);
  CU_ASSERT(vc->y == 2);
  CU_ASSERT(vc->z == 3);

  Vector *u = vector_new(4,5,6);
  vector_add(v,u);
  CU_ASSERT(v->x == 5);
  CU_ASSERT(v->y == 7);
  CU_ASSERT(v->z == 9);

  vector_subtract(v,u);
  CU_ASSERT(v->x == 1);
  CU_ASSERT(v->y == 2);
  CU_ASSERT(v->z == 3);

  vector_multiply(v,2);
  CU_ASSERT(v->x == 2);
  CU_ASSERT(v->y == 4);
  CU_ASSERT(v->z == 6);

  Vector *w = vector_cross(v,u);
  CU_ASSERT(w->x == -6);
  CU_ASSERT(w->y == 12);
  CU_ASSERT(w->z == -6);

  vector_free(v);
  vector_free(u);
  vector_free(w);

  v = vector_new(3,4,0);
  u = vector_new(0,3,4);
  CU_ASSERT(v->x==u->y);
  CU_ASSERT(v->y==u->z);

  w = vector_cross(v,u);
  CU_ASSERT(w->x == 16);
  CU_ASSERT(w->y == -12);
  CU_ASSERT(w->z == 9);


  CU_ASSERT(within_tol(vector_magnitude(v),5));
  CU_ASSERT(within_tol(vector_magnitude(u),5));

  vector_free(v);
  vector_free(u);
  vector_free(w);

  v = vector_new(1,0,0);
  u = vector_new(0,0,1);

  double a = vector_angle(v,u)/D2R;
  CU_ASSERT(within_tol(a,90));
  CU_ASSERT(within_tol(vector_angle(u,v),90.*D2R));

  w = vector_cross(v,u);
  CU_ASSERT(within_tol(vector_magnitude(w),1));
  CU_ASSERT(w->x==0);
  CU_ASSERT(w->y==-1);
  CU_ASSERT(w->z==0);
}

