#include "project.h"

#include <stdio.h>

#define DEG_TO_RAD 0.0174532925199432958

static int nfail = 0;
static int nok = 0;

#define TEST_POLY(inx, iny, outx, outy) { \
                       double x,y; \
                       project_poly(inx,iny,&x,&y); \
                       if (!within_tol(x,outx) || !within_tol(y, outy)) { \
                         printf("Fail: proj_poly(" #inx "," #iny "," \
                                #outx "," #outy "): got (%f,%f)\n", \
                                x, y); \
                         ++nfail; \
                       } else { ++nok; } \
		       }

#define TEST_UTM(lon0, lat, lon, outx, outy) { \
                       double x, y; \
                       project_utm(lon0, \
                                   lat * DEG_TO_RAD, \
                                   lon * DEG_TO_RAD, &x, &y); \
                       if (!within_tol(x,outx) || !within_tol(y, outy)) { \
                         printf("Fail: project_utm(" #lon0 "," #lat "," #lon \
                                "," #outx "," #outy "): got (%f,%f)\n", \
                                x, y); \
                         ++nfail; \
                       } else { ++nok; } \
		       }

#define TEST_PS(lat0, lon0, lat, lon, outx, outy) { \
                       double x, y; \
                       project_ps(lat0, lon0, \
                                   lat * DEG_TO_RAD, \
                                   lon * DEG_TO_RAD, &x, &y); \
                       if (!within_tol(x,outx) || !within_tol(y, outy)) { \
                         printf("Fail: project_ps(" #lat0 "," #lon0 "," \
                                #lat "," #lon "," #outx "," #outy \
                                "): got (%f,%f)\n", \
                                x, y); \
                         ++nfail; \
                       } else { ++nok; } \
		       }
                        
static const double tol = 0.001;
int within_tol(double a, double b)
{
    return fabs(b) < tol ? fabs(a) < tol : fabs((a-b)/b) < tol;
}

void test_poly()
{
    TEST_POLY(0, -90, 0, 0);
    TEST_POLY(33, -95, -467100.408, 3663659.262);
    TEST_POLY(77, -86, 100412.759, 8553464.807);
}

void test_utm()
{
    TEST_UTM(-112, 45.25919444, -111.5, 460769.27, 5011648.45); 
}

void test_ps()
{
    TEST_PS(71, -96, 39.101252222, -121.33955, -2529570, -5341800); 
}

int main(int argc, char * argv [])
{
    test_poly();
    test_utm();
    test_ps();

    if (nfail == 0)
	printf("%d tests passed!\n", nok);
    else
	printf("%d ok, %d failures.\n", nok, nfail);

    return nfail;
}
