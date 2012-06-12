#include "asf_geocode.h"
#include "asf.h"

#include <assert.h>
#include <stdio.h>

//#define geoid_height_at(x,y) *(geoid_heights + y*w + x)
static const int w=1440;
static const int h=721;
static float *geoid_heights = NULL;

static float geoid_height_at(int x, int y)
{
    if (x<0) x=0;
    if (x>=w) x=w-1;
    if (y<0) y=0;
    if (y>=h) y=h-1;
    assert(y*w+x < w*h);

    return *(geoid_heights + y*w + x);
}

static int read_geoid(void)
{
    int x,y;
    FILE *f=fopen_share_file("WW15MGH.DAC","rb");
    for (y=0;y<h;y++)
	for (x=0;x<w;x++) {
            signed char hi=fgetc(f);
            unsigned char lo=fgetc(f);
            float height=((hi<<8)+lo)*(1.0/100);
            *(geoid_heights + y*w + x) = height;
	}
    fclose(f);
    return 0;
}

float get_geoid_height(double lat, double lon)
{
    if (!geoid_heights) {
        geoid_heights = MALLOC(sizeof(float)*w*h);
        read_geoid();
    }

    if (lon < 0) lon += 360;

    if (lat > 90 || lat < -90 || lon < 0 || lon >= 360) {
        printf("Illegal lat, lon passed to get_geoid_height: %f,%f\n",lat,lon);
        return 0;
    }

    // Y: 721 records = 180*4+1, 
    //    from 90 degrees North down to 90 degrees South every 0.25 degrees.
    double y = (90.-lat)*4.;

    // X: 1440 elements = 360*4, 
    //    from 0 degrees East to 359.75 degrees East every 0.25 degrees.
    double x = lon*4.;

    // bilinear interp
    int y0 = (int)y;
    int y1 = y0 + 1;
    double yf = y-y0;

    int x0 = (int)x;
    int x1 = x0 + 1;
    double xf = x-x0;

    return
         xf     * yf     * geoid_height_at(x1,y1) +
         (1-xf) * yf     * geoid_height_at(x0,y1) +
         xf     * (1-yf) * geoid_height_at(x1,y0) +
         (1-xf) * (1-yf) * geoid_height_at(x0,y0);
}

void test_geoid(void)
{
    float f = get_geoid_height(0,0);
    if (fabs(f - 17.16) > .0001)
      asfPrintWarning("Unexpected value at 0,0 should be 17.15: %f\n", f);

    int i, j;
    for (i=0; i<w; ++i) {
      for (j=0; j<h; ++j) {
        float a0 = *(geoid_heights + j*w + i);
        float a1 = geoid_height_at(i,j);
        if (fabs(a0-a1) > .0001)
          asfPrintWarning("test_geoid #1 failed: %d %d %f %f\n", i, j, a0, a1);

        if (i<w-1 && j<h-1) {
          double i1 = i+.5;
          double j1 = j+.5;

          double lat = 90.0 - j1/4.0;
          double lon = i1/4.0;

          float b0 = get_geoid_height(lat,lon);
          float b1 = .25*(geoid_height_at(i,j)+geoid_height_at(i+1,j)+
                          geoid_height_at(i,j+1)+geoid_height_at(i+1,j+1));
          if (fabs(b0-b1) > .0001)
            asfPrintWarning("test_geoid #2 failed: %d %d %f %f\n", i, j, b0, b1);

          i1 = i+.25;
          j1 = j+.75;
          lat = 90.0 - j1/4.0;
          lon = i1/4.0;
          float c0 = get_geoid_height(lat,lon);

          float u = .75*geoid_height_at(i,j) + .25*geoid_height_at(i+1,j);
          float v = .75*geoid_height_at(i,j+1) + .25*geoid_height_at(i+1,j+1);
          float c1 = .25*u + .75*v;  
          if (fabs(c0-c1) > .0001)
            asfPrintError("test_geoid #3 failed: %d %d %f %f\n", i, j, c0, c1);
        }
      }
    }
}

