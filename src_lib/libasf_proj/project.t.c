#include "project.h"
#include "asf_meta.h"

#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <assert.h>
#include <stdlib.h>

#define DEG_TO_RAD 0.0174532925199432958

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

static const int ARR_TEST_SIZE = 10;

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

#define TEST_LAMAZ(lat0, lon0, lat, lon, outx, outy) { \
                       double x, y; \
                       project_lamaz(lat0, lon0, \
                                   lat * DEG_TO_RAD, \
                                   lon * DEG_TO_RAD, &x, &y); \
                       if (!within_tol(x,outx) || !within_tol(y, outy)) { \
                         printf("Fail: project_lamaz(" #lat0 "," #lon0 "," \
                                #lat "," #lon "," #outx "," #outy \
                                "): got (%f,%f)\n", \
                                x, y); \
                         ++nfail; \
                       } else { ++nok; } \
		       } { \
                       double x, y; \
                       proj_lamaz lamaz; \
                       lamaz.center_lon = lon0; \
                       lamaz.center_lat = lat0; \
                       project_lamaz_s(&lamaz, \
                                   lat * DEG_TO_RAD, \
                                   lon * DEG_TO_RAD, &x, &y); \
                       if (!within_tol(x,outx) || !within_tol(y, outy)) { \
                         printf("Fail: project_lamaz_s(" #lat0 "," #lon0 "," \
                                #lat "," #lon "," #outx "," #outy \
                                "): got (%f,%f)\n", \
                                x, y); \
                         ++nfail; \
                       } else { ++nok; } \
		       }

#define TEST_LAMCC(lat1, lat2, lat_0, lon_0, lat, lon, outx, outy) { \
                       double x, y; \
                       project_lamcc(lat1, lat2, lat_0, lon_0, \
                                   lat * DEG_TO_RAD, \
                                   lon * DEG_TO_RAD, &x, &y); \
                       if (!within_tol(x,outx) || !within_tol(y, outy)) { \
                         printf("Fail: project_lamcc(" #lat1 "," #lat2 "," \
                                #lat_0 "," #lon_0 "," \
                                #lat "," #lon "," #outx "," #outy \
                                "): got (%f,%f)\n", \
                                x, y); \
                         ++nfail; \
                       } else { ++nok; } \
		       } { \
                       double x, y; \
                       proj_lamcc lamcc; \
                       lamcc.plat1 = lat1; \
                       lamcc.plat2 = lat2; \
                       lamcc.lat0 = lat_0; \
                       lamcc.lon0 = lon_0; \
                       project_lamcc_s(&lamcc, \
                                   lat * DEG_TO_RAD, \
                                   lon * DEG_TO_RAD, &x, &y); \
                       if (!within_tol(x,outx) || !within_tol(y, outy)) { \
                         printf("Fail: project_lamcc_s(" #lat1 "," #lat2 "," \
                                #lat_0 "," #lon_0 "," \
                                #lat "," #lon "," #outx "," #outy \
                                "): got (%f,%f)\n", \
                                x, y); \
                         ++nfail; \
                       } else { ++nok; } \
		       }

#define TEST_ALB(lat1, lat2, lat0, lon0, lat, lon, outx, outy) { \
                       double x, y; \
                       project_albers(lat1, lat2, lat0, lon0, \
                                   lat * DEG_TO_RAD, \
                                   lon * DEG_TO_RAD, &x, &y); \
                       if (!within_tol(x,outx) || !within_tol(y, outy)) { \
                         printf("Fail: project_albers(" #lat1 "," #lat2 "," \
                                #lat0 "," #lon0 "," \
                                #lat "," #lon "," #outx "," #outy \
                                "): got (%f,%f)\n", \
                                x, y); \
                         ++nfail; \
                       } else { ++nok; } \
		       } { \
                       double x, y; \
                       proj_albers pa; \
                       pa.std_parallel1 = lat1; \
                       pa.std_parallel2 = lat2; \
                       pa.orig_latitude = lat0; \
                       pa.center_meridian = lon0; \
                       project_albers_s(&pa, \
                                   lat * DEG_TO_RAD, \
                                   lon * DEG_TO_RAD, &x, &y); \
                       if (!within_tol(x,outx) || !within_tol(y, outy)) { \
                         printf("Fail: project_albers_s(" #lat1 "," #lat2 "," \
                                #lat0 "," #lon0 "," \
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

int check(char * name, double x, double y, double x_correct, double y_correct)
{
    if (!within_tol(x, x_correct) || !within_tol(y, y_correct))
    {
	printf("Fail: %s.  Expected: (%f,%f) Got: (%f,%f)\n",
	       name, x_correct, y_correct, x, y);

	++nfail;

	return FALSE;
    }
    else
    {
	++nok;

	return TRUE;
    }
}

void test_poly()
{
    TEST_POLY(0, -90, 0, 0);
    TEST_POLY(33, -95, -467100.408, 3663659.262);
    TEST_POLY(77, -86, 100412.759, 8553464.807);
}

/************************************** Universal Transverse Mercator Tests */
void testa_utm(double lon0_deg, double lat_deg, double lon_deg,
	       double x_correct, double y_correct)
{
    char name[256];
    double lon0, lat, lon;

    lon0 = lon0_deg * DEG_TO_RAD;
    lat = lat_deg * DEG_TO_RAD;
    lon = lon_deg * DEG_TO_RAD;

    /* normal function call check */
    {
	double x, y;
	double lat_out, lon_out;
	int ok;

	sprintf(name, "project_utm(%f,%f,%f)",
		lon0_deg, lat_deg, lon_deg);
	
	project_utm(lon0, lat, lon, &x, &y);
	
	ok = check(name, x, y, x_correct, y_correct);

	/* inverse call should result in original point */
	/* skip if forward test did not pass */
	if (ok)
	{
	    sprintf(name, "project_utm_inv(%f,%f,%f)",
		    lon0_deg, x, y);
	    
	    project_utm_inv(lon0, x, y, &lat_out, &lon_out);
	    check(name, lat, lon, lat_out, lon_out);
	}
    }

    /* passing as an array function call check */
    {
	double *xarr, *yarr;
	int i, ok;

	sprintf(name, "project_utm_arr(%f,%f,%f)",
		lon0_deg, lat_deg, lon_deg);

	xarr = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
	yarr = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);

	for (i = 0; i < ARR_TEST_SIZE; ++i)
	{
	    xarr[i] = lat;
	    yarr[i] = lon;
	}
	
	project_utm_arr(lon0, xarr, yarr, ARR_TEST_SIZE);

	ok = 1;
	for (i = 1; i < ARR_TEST_SIZE; ++i)
	{
	    if (!within_tol(xarr[i], xarr[0]) || !within_tol(yarr[i], yarr[0]))
	    {
		ok = 0;
		printf("Fail: %s. Result [%d] (%f,%f) "
		       "disagrees with [0] (%f,%f).\n",
		       name, i, xarr[i], yarr[i], xarr[0], yarr[0]);

		++nfail;
		break;
	    }
	}

	if (ok) ++nok;
	check(name, xarr[0], yarr[0], x_correct, y_correct);

	free(yarr);
	free(xarr);
    }
}

void testa_random_utm()
{
    /* check that randomly generated values agree between array
       version and by-point version */

    double *x, *xp, *xa;
    double *y, *yp, *ya;

    int i, ok=1;

    int reference_lon;
    int reference_lat;

    /* do these dynamically, might be too big for the stack */
    x = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    y = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);

    xp = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    yp = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);

    xa = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    ya = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);

    srand(102);

    /* any longitude, and latitudes (-60,60) should be ok */
    reference_lon = rand() % 360 - 180;
    reference_lat = rand() % 120 - 60;

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	/* latitude: close to the reference latitude (within 2 degrees) */
	x[i] = xa[i] = DEG_TO_RAD * ( reference_lat + 
	    ( (double)rand() / (double)RAND_MAX * 1 - .5 ) );

	/* longitude: close to the reference longitude (within 2 degrees) */
	y[i] = ya[i] = DEG_TO_RAD * ( reference_lon + 
	    ( (double)rand() / (double)RAND_MAX * 1 - .5 ) );
    }

    project_utm_arr(reference_lon * DEG_TO_RAD, xa, ya, ARR_TEST_SIZE);

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	project_utm(reference_lon * DEG_TO_RAD, x[i], y[i], &xp[i], &yp[i]); 
    }

    /* do they agree? */
    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	if (!within_tol(xa[i], xp[i]) || !within_tol(ya[i], yp[i]))
	{
	    printf("Fail: project_utm{_arr} - Random[%d] (%d,%f,%f)"
		   " Array Result: (%f,%f) Point Result: (%f,%f)\n",
		   i, reference_lon,
		   x[i], y[i], xa[i], ya[i], xp[i], yp[i]);
	    ++nfail;
	    ok = 0;
	    break;
	}
    }
    
    if (ok)
	++nok;

    /* now reverse-transform via both methods.  First by-point */
    ok = 1;

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	project_utm_inv(reference_lon * DEG_TO_RAD, 
			xp[i], yp[i], &xp[i], &yp[i]);
    }

    /* agrees with original values ? */
    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	if (!within_tol(xp[i], x[i]) || !within_tol(yp[i], y[i]))
	{
	    printf("Fail: project_utm/project_utm_inv doesn't match."
		   " Random[%d] (%d,%f,%f) Got: (%f,%f)\n",
		   i, reference_lon, x[i], y[i], xp[i], yp[i]);
	    ++nfail;
	    ok = 0;
	    break;
	}
    }

    if (ok)
	++nok;

    /* Second, by array */
    ok = 1;

    project_utm_arr_inv(reference_lon * DEG_TO_RAD,
			xa, ya, ARR_TEST_SIZE);

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	if (!within_tol(xa[i], x[i]) || !within_tol(ya[i], y[i]))
	{
	    printf("Fail: project_utm_arr/project_utm_arr_inv doesn't match."
		   " Random[%d] (%d,%f,%f) Got: (%f,%f)\n",
		   i, reference_lon, x[i], y[i], xa[i], ya[i]);
	    ++nfail;
	    ok = 0;
	    break;
	}
    }

    free(ya);
    free(xa);
    free(yp);
    free(xp);
    free(x);
    free(y);
}

void test_random_utm(int N)
{
    int i;
    for (i = 0; i < N; ++i)
	testa_random_utm();
}


void test_utm()
{
    testa_utm(-112, 45.25919444, -111.5, 460769.27, 5011648.45);

    test_random_utm(25);
}

/************************************* Polar Stereographic Projection Tests */
void testa_ps(double lat0_deg, double lon0_deg, double lat_deg, double lon_deg,
	      double x_correct, double y_correct)
{
    char name[256];

    double lat0, lon0, lat, lon;

    lat0 = lat0_deg * DEG_TO_RAD;
    lon0 = lon0_deg * DEG_TO_RAD;
    lat = lat_deg * DEG_TO_RAD;
    lon = lon_deg * DEG_TO_RAD;

    /* normal function call check */
    {
	double x, y;
	double lat_out, lon_out;
	int ok;

	sprintf(name, "project_ps(%f,%f,%f,%f)",
		lat0_deg, lon0_deg, lat_deg, lon_deg);
	
	project_ps(lat0, lon0, 1, lat, lon, &x, &y);
	
	ok = check(name, x, y, x_correct, y_correct);

	/* inverse call should result in original point */
	/* skip if forward test did not pass */
	if (ok)
	{
	    sprintf(name, "project_ps_inv(%f,%f,%f,%f)",
		    lat0_deg, lon0_deg, x, y);
	    
	    project_ps_inv(lat0, lon0, 1, x, y, &lat_out, &lon_out);

	    check(name, lat, lon, lat_out, lon_out);
	}
    }

    /* passing as a structure function call check */
    {
	proj_ps ps;
	double x, y;

	sprintf(name, "project_ps_s(%f,%f,%f,%f)",
		lat0_deg, lon0_deg, lat_deg, lon_deg);
	
	ps.slat = lat0;
	ps.slon = lon0;
	
	project_ps_s(&ps, 1, lat, lon, &x, &y);

	check(name, x, y, x_correct, y_correct);
    }
    
    /* passing as an array function call check */
    {
	double *xarr, *yarr;
	int i, ok;

	sprintf(name, "project_ps_arr(%f,%f,%f,%f)",
		lat0_deg, lon0_deg, lat_deg, lon_deg);

	xarr = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
	yarr = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);

	for (i = 0; i < ARR_TEST_SIZE; ++i)
	{
	    xarr[i] = lat;
	    yarr[i] = lon;
	}
	
	project_ps_arr(lat0, lon0, 1, xarr, yarr, ARR_TEST_SIZE);

	ok = 1;
	for (i = 1; i < ARR_TEST_SIZE; ++i)
	{
	    if (!within_tol(xarr[i], xarr[0]) || !within_tol(yarr[i], yarr[0]))
	    {
		ok = 0;
		printf("Fail: %s. Result [%d] (%f,%f) "
		       "disagrees with [0] (%f,%f).\n",
		       name, i, xarr[i], yarr[i], xarr[0], yarr[0]);

		++nfail;
		break;
	    }
	}

	if (ok) ++nok;
	check(name, xarr[0], yarr[0], x_correct, y_correct);

	free(yarr);
	free(xarr);
    }
}

void testa_random_ps()
{
    /* check that randomly generated values agree between array
       version and by-point version */

    double *x, *xp, *xa;
    double *y, *yp, *ya;

    int i, ok=1;

    int reference_lat;
    int reference_lon;

    /* do these dynamically, might be too big for the stack */
    x = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    y = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);

    xp = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    yp = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);

    xa = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    ya = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);

    srand(101);

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	/* latitude: between pi/4 and pi/2 (northern latitudes) */
	x[i] = xa[i] = (double)rand() / (double)RAND_MAX * .7 + .7;

	/* longitude: between -pi and pi */
	y[i] = ya[i] = (double)rand() / (double)RAND_MAX * 6.26 - 3.14;
    }

    /* should be able to use any reference latitude > 45 and any ref lon */
    reference_lat = rand() % 45 + 45;
    reference_lon = rand() % 360 - 180;

    project_ps_arr(reference_lat * DEG_TO_RAD, reference_lon * DEG_TO_RAD,
		   1, xa, ya, ARR_TEST_SIZE);

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	project_ps(reference_lat * DEG_TO_RAD, reference_lon * DEG_TO_RAD, 1, 
		   x[i], y[i], &xp[i], &yp[i]); 
    }

    /* do they agree? */
    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	if (!within_tol(xa[i], xp[i]) || !within_tol(ya[i], yp[i]))
	{
	    printf("Fail: project_ps{_arr} - Random[%d] (%d,%d,%f,%f)"
		   " Array Result: (%f, %f) Point Result: (%f,%f)\n",
		   i, reference_lat, reference_lon,
		   x[i], y[i], xa[i], ya[i], xp[i], yp[i]);
	    ++nfail;
	    ok = 0;
	    break;
	}
    }
    
    if (ok)
	++nok;

    /* now reverse-transform via both methods.  First by-point */
    ok = 1;

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	project_ps_inv(reference_lat * DEG_TO_RAD, 
		       reference_lon * DEG_TO_RAD, 1,
		       xp[i], yp[i], &xp[i], &yp[i]);
    }

    /* agrees with original values ? */
    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	if (!within_tol(xp[i], x[i]) || !within_tol(yp[i], y[i]))
	{
	    printf("Fail: project_ps/project_ps_inv doesn't match."
		   " Random[%d] (%d,%d,%f,%f) Got: (%f,%f)\n",
		   i, reference_lat, reference_lon,
		   x[i], y[i], xp[i], yp[i]);
	    ++nfail;
	    ok = 0;
	    break;
	}
    }

    if (ok)
	++nok;

    /* Second, by array */
    ok = 1;

    project_ps_arr_inv(reference_lat * DEG_TO_RAD, reference_lon * DEG_TO_RAD,
		       1, xa, ya, ARR_TEST_SIZE);

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	if (!within_tol(xa[i], x[i]) || !within_tol(ya[i], y[i]))
	{
	    printf("Fail: project_ps_arr/project_ps_arr_inv doesn't match."
		   " Random[%d] (%d,%d,%f,%f) Got: (%f,%f)\n",
		   i, reference_lat, reference_lon,
		   x[i], y[i], xa[i], ya[i]);
	    ++nfail;
	    ok = 0;
	    break;
	}
    }

    free(ya);
    free(xa);
    free(yp);
    free(xp);
    free(x);
    free(y);
}

void test_random_ps(int N)
{
    int i;
    for (i = 0; i < N; ++i)
	testa_random_ps();
}


void test_ps()
{
    testa_ps(71, -96, 39.101252222, -121.33955, -2529570, -5341800); 
    
    test_random_ps(25);
}

/********************************************* Lambert Azimuthal Equal Area  */
void test_lamaz()
{
    TEST_LAMAZ(0, 0, 0, 0, 0, 0);
}

/************************************************** Lambert Conformal Conic  */
void test_lamcc()
{
    /*TEST_LAMCC(28.3833333, 30.2833333, 27.833333, -99, 28.5, -96, 0, 0);*/
}

/************************************************** Albers Equal-Area Conic  */
void test_alb()
{
    TEST_ALB(5, 10, 0, 0, 0, 0, 0, 0);
}

void perf_test2()
{
    const int N = 10000;
    double *x, *xo;
    double *y, *yo;
    double lat0, lon0;

    int i, n=0;
    struct timeval tv1, tv2;
    struct timezone tz;
    int elapsed1, elapsed2;

    lat0 = 71 * DEG_TO_RAD;
    lon0 = -96 * DEG_TO_RAD;

    x = (double *) malloc(sizeof(double) * N);
    y = (double *) malloc(sizeof(double) * N);

    xo = (double *) malloc(sizeof(double) * N);
    yo = (double *) malloc(sizeof(double) * N);

    /* use same seed each time */
    srand(10101);

    for (i = 0; i < N; ++i)
    {
	x[i] = xo[i] = (double)rand() / (double)RAND_MAX * .707 + .707;
	y[i] = yo[i] = (double)rand() / (double)RAND_MAX * .707 + .707;
    }

    gettimeofday(&tv1, &tz);    
    project_ps_arr(lat0, lon0, 1, x, y, N);
    gettimeofday(&tv2, &tz);

    elapsed1 = tv2.tv_sec - tv1.tv_sec;

    gettimeofday(&tv1, &tz);    
    for (i = 0; i < N; ++i)
    {
	project_ps(lat0, lon0, 1, xo[i], yo[i], &xo[i], &yo[i]); 
    }
    gettimeofday(&tv2, &tz);
    elapsed2 = tv2.tv_sec - tv1.tv_sec;

    /* do they agree? */
    for (i = 0; i < N; ++i)
    {
	if (!within_tol(x[i], xo[i]) || !within_tol(y[i], yo[i]))
	{
/*
	    printf("x[%d] = %f, xo[%d] = %f, y[%d] = %f, yo[%d] = %f\n",
		   i, x[i], i, xo[i], i, y[i], i, yo[i]);
*/
	    ++n;
	}
    }
    
    printf("This Should Be 0 : %d\n", n);
    printf("Elapsed #1 (array): %d sec\n", elapsed1);
    printf("Elapsed #2 (point): %d sec\n", elapsed2);
}

void perf_test()
{
    double x, y;
    int i;
    struct timeval tv1, tv2;
    struct timezone tz;
    
    gettimeofday(&tv1, &tz);    
    for (i = 0; i < 1000000; ++i)
    {
	project_lamaz(46, -112, .765, -1.1305, &x, &y);
    }
    gettimeofday(&tv2, &tz);

    printf("Elapsed: %d sec\n", (int)(tv2.tv_sec - tv1.tv_sec));
}

int main(int argc, char * argv [])
{
    test_poly();
    test_utm();
    test_ps();
    test_lamaz();
    test_lamcc();
    test_alb();

    /* perf_test2(); */

    if (nfail == 0)
	printf("%d tests passed!\n", nok);
    else
	printf("%d ok, %d failures.\n", nok, nfail);

    return nfail;
}
