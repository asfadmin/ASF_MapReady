#include "asf_geocode.h"
#include "CUnit/Basic.h"

int geoid_get_width(void);
int geoid_get_height(void);
float *geoid_get_height_array(void);
float geoid_height_at(int x, int y);

void test_geoid(void)
{
    float f = get_geoid_height(0,0);
    if (fabs(f - 17.16) > .0001)
      asfPrintWarning("Unexpected value at 0,0 should be 17.15: %f\n", f);

    int w = geoid_get_width();
    int h = geoid_get_height();
    float *geoid_heights = geoid_get_height_array();

    int i, j;
    for (i=0; i<w; ++i) {
      for (j=0; j<h; ++j) {
        float a0 = *(geoid_heights + j*w + i);
        float a1 = geoid_height_at(i,j);
        if (fabs(a0-a1) > .0001)
          asfPrintWarning("test_geoid #1 failed: %d %d %f %f\n", i, j, a0, a1);
        CU_ASSERT(fabs(a0-a1)<.0001);

        if (i<w-1 && j<h-1) {
          double ii = i+.5;
          double jj = j+.5;

          double lat = 90.0 - jj/4.0;
          double lon = ii/4.0;

          float b0 = get_geoid_height(lat,lon);
          float b1 = .25*(geoid_height_at(i,j)+geoid_height_at(i+1,j)+
                          geoid_height_at(i,j+1)+geoid_height_at(i+1,j+1));
          if (fabs(b0-b1) > .0001)
            asfPrintWarning("test_geoid #2 failed: %d %d %f %f\n", i, j, b0, b1);
          CU_ASSERT(fabs(b0-b1)<.0001);

          ii = i+.25;
          jj = j+.75;
          lat = 90.0 - jj/4.0;
          lon = ii/4.0;
          float c0 = get_geoid_height(lat,lon);

          float u = .75*geoid_height_at(i,j) + .25*geoid_height_at(i+1,j);
          float v = .75*geoid_height_at(i,j+1) + .25*geoid_height_at(i+1,j+1);
          float c1 = .25*u + .75*v;
          if (fabs(c0-c1) > .0001)
            asfPrintError("test_geoid #3 failed: %d %d %f %f\n", i, j, c0, c1);
          CU_ASSERT(fabs(c0-c1)<.0001);
        }
      }
    }
}

