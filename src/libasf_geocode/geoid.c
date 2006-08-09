#include "asf_geocode.h"
#include "asf.h"

#include <stdio.h>

#define geoid_height_at(x,y) *(geoid_heights + y*w + x)

static int read_geoid(float *geoid_heights, int w, int h)
{
    int x,y;
    FILE *f=fopen_share_file("WW15MGH.DAC","rb");
    for (y=0;y<h;y++)
	for (x=0;x<w;x++) {
            signed char hi=fgetc(f);
            unsigned char lo=fgetc(f);
            float height=((hi<<8)+lo)*(1.0/100);
            geoid_height_at(x,y) = height;
	}
    fclose(f);
    return 0;
}

float get_geoid_height(double lat, double lon)
{
    const int w=1440;
    const int h=721;

    static float *geoid_heights = NULL;

    if (!geoid_heights) {
        geoid_heights = MALLOC(sizeof(float)*w*h);
        read_geoid(geoid_heights, w, h);
    }

    if (lon < 0) lon += 360;

    if (lat > 90 || lat < -90 || lon < 0 || lon >= 360) {
        printf("Illegal lat, lon passed to get_geoid_height: %f,%f\n",lat,lon);
        return 0;
    }

    // Y: 721 records = 180*4+1, 
    //    from 90 degrees North down to 90 degrees South every 0.25 degrees.

    int y = (int)(90-lat)*4;

    // X: 1440 elements = 360*4, 
    //    from 0 degrees East to 359.75 degrees East every 0.25 degrees.

    int x = (int)lon*4;

    return geoid_height_at(x,y);
}

