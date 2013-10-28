#include "asf_geocode.h"
#include "asf.h"

#include <assert.h>
#include <stdio.h>

//#define geoid_height_at(x,y) *(geoid_heights + y*w + x)
static const int w=1440;
static const int h=721;
static float *geoid_heights = NULL;

#ifdef TEST
float geoid_height_at(int x, int y);
#else
static
#endif
float geoid_height_at(int x, int y)
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
    int y_0 = (int)y;
    int y_1 = y_0 + 1;
    double yf = y-y_0;

    int x_0 = (int)x;
    int x_1 = x_0 + 1;
    double xf = x-x_0;

    return
         xf     * yf     * geoid_height_at(x_1,y_1) +
         (1-xf) * yf     * geoid_height_at(x_0,y_1) +
         xf     * (1-yf) * geoid_height_at(x_1,y_0) +
         (1-xf) * (1-yf) * geoid_height_at(x_0,y_0);
}

// These are for the test code -- do not use!
int geoid_get_width(void);
int geoid_get_height(void);
float *geoid_get_height_array(void);
int geoid_get_width(void) { return w; }
int geoid_get_height(void) { return h; }
float *geoid_get_height_array(void) { return geoid_heights; }

