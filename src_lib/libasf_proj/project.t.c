#include "libasf_proj.h"
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

static const int ARR_TEST_SIZE = 50;
static const int NUM_REPS = 2;

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
int testa_utm_arr(project_parameters_t * pps,  char * name,
		  double lat, double lon,
		  double x_correct, double y_correct,
		  int we_allocate)
{
    double *xarr, *yarr;
    double *xarr_out, *yarr_out;
    int i, ok;
    
    xarr = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    yarr = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    
    if (we_allocate)
    {
	xarr_out = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
	yarr_out = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    }
    else
    {
	xarr_out = NULL;
	yarr_out = NULL;
    }

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	xarr[i] = lat;
	yarr[i] = lon;
    }
    
    project_utm_arr(pps, xarr, yarr, &xarr_out, &yarr_out, ARR_TEST_SIZE);

    ok = 1;
    for (i = 1; i < ARR_TEST_SIZE; ++i)
    {
	if (!within_tol(xarr_out[i], xarr_out[0]) ||
	    !within_tol(yarr_out[i], yarr_out[0]))
	{
	    ok = 0;
	    printf("Fail: %s. Result [%d] (%f,%f) "
		   "disagrees with [0] (%f,%f).\n",
		   name, i, xarr_out[i], yarr_out[i], 
		   xarr_out[0], yarr_out[0]);
	    ++nfail;
	    break;
	}
    }
    
    if (ok) ++nok;
    check(name, xarr_out[0], yarr_out[0], x_correct, y_correct);
    
    free(yarr_out);
    free(xarr_out);
    
    free(yarr);
    free(xarr);

    return ok;
}

void testa_utm(double lon0_deg, double lat_deg, double lon_deg,
	       double x_correct, double y_correct)
{
    char name[256];
    double lon0, lat, lon;
    project_parameters_t pps;
    int ok;

    lon0 = lon0_deg * DEG_TO_RAD;
    lat = lat_deg * DEG_TO_RAD;
    lon = lon_deg * DEG_TO_RAD;

    pps.utm.lon0 = lon0;

    /* normal function call check */
    {
	double x, y;
	double lat_out, lon_out;

	sprintf(name, "project_utm(%f,%f,%f)",
		lon0_deg, lat_deg, lon_deg);
	
	project_utm(&pps, lat, lon, &x, &y);
	
	ok = check(name, x, y, x_correct, y_correct);

	/* inverse call should result in original point */
	/* skip if forward test did not pass */
	if (ok)
	{
	    sprintf(name, "project_utm_inv(%f,%f,%f)",
		    lon0_deg, x, y);
	    
	    project_utm_inv(&pps, x, y, &lat_out, &lon_out);
	    check(name, lat, lon, lat_out, lon_out);
	}
    }

    /* passing as an array function call check -- 2 ways */
    sprintf(name, "project_utm_arr(%f,%f,%f) - preallocated\n",
	    lon0_deg, lat_deg, lon_deg);

    ok = testa_utm_arr(&pps, name, lat, lon, x_correct, y_correct, TRUE);

    if (ok)
    {
	sprintf(name, "project_utm_arr(%f,%f,%f) - NULLed\n",
		lon0_deg, lat_deg, lon_deg);
	
	testa_utm_arr(&pps, name, lat, lon, x_correct, y_correct, FALSE);
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
    project_parameters_t pps;

    /* do these dynamically, might be too big for the stack */
    x = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    y = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);

    xp = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    yp = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);

    xa = NULL;
    ya = NULL;

    srand(102);

    /* any longitude, and latitudes (-60,60) should be ok */
    reference_lon = rand() % 360 - 180;
    reference_lat = rand() % 120 - 60;

    pps.utm.lon0 = reference_lon * DEG_TO_RAD;

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	/* latitude: close to the reference latitude (within 2 degrees) */
	x[i] = DEG_TO_RAD * ( reference_lat + 
	    ( (double)rand() / (double)RAND_MAX * 1 - .5 ) );

	/* longitude: close to the reference longitude (within 2 degrees) */
	y[i] = DEG_TO_RAD * ( reference_lon + 
	    ( (double)rand() / (double)RAND_MAX * 1 - .5 ) );
    }

    project_utm_arr(&pps, x, y, &xa, &ya, ARR_TEST_SIZE);

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	project_utm(&pps, x[i], y[i], &xp[i], &yp[i]); 
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
	project_utm_inv(&pps,xp[i], yp[i], &xp[i], &yp[i]);
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

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	xp[i] = xa[i];
	yp[i] = ya[i];
    }

    project_utm_arr_inv(&pps, xp, yp, &xa, &ya, ARR_TEST_SIZE);

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

    test_random_utm(NUM_REPS);
}

/************************************* Polar Stereographic Projection Tests */
int testa_ps_arr(project_parameters_t * pps, char * name,
		 double lat, double lon,
		 double x_correct, double y_correct,
		 int we_allocate)		  
{
    double *xarr_in, *yarr_in;
    double *xarr, *yarr;
    int i, ok;
    
    xarr_in = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    yarr_in = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    
    if (we_allocate)
    {
	xarr = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
	yarr = (double *) malloc(sizeof(double) * ARR_TEST_SIZE);
    }
    else
    {
	xarr = NULL;
	yarr = NULL;
    }

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	xarr_in[i] = lat;
	yarr_in[i] = lon;
    }
    
    project_ps_arr(pps, xarr_in, yarr_in, &xarr, &yarr, ARR_TEST_SIZE);
    
    ok = 1;
    for (i = 1; i < ARR_TEST_SIZE; ++i)
    {
	if (!within_tol(xarr[i], xarr[0]) || 
	    !within_tol(yarr[i], yarr[0]))
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

    return ok;
}

void testa_ps(double lat0_deg, double lon0_deg, double lat_deg, double lon_deg,
	      double x_correct, double y_correct)
{
    char name[256];

    double lat0, lon0, lat, lon;
    project_parameters_t pps;
    int ok;

    lat0 = lat0_deg * DEG_TO_RAD;
    lon0 = lon0_deg * DEG_TO_RAD;
    lat = lat_deg * DEG_TO_RAD;
    lon = lon_deg * DEG_TO_RAD;

    pps.ps.slat = lat0;
    pps.ps.slon = lon0;
    pps.ps.is_north_pole = 1;
    pps.ps.false_easting = 0;
    pps.ps.false_northing = 0;
    pps.ps.scale_factor = 1;

    /* normal function call check */
    {
	double x, y;
	double lat_out, lon_out;

	sprintf(name, "project_ps(%f,%f,%f,%f)",
		lat0_deg, lon0_deg, lat_deg, lon_deg);
	
	project_ps(&pps, lat, lon, &x, &y);
	
	ok = check(name, x, y, x_correct, y_correct);

	/* inverse call should result in original point */
	/* skip if forward test did not pass */
	if (ok)
	{
	    sprintf(name, "project_ps_inv(%f,%f,%f,%f)",
		    lat0_deg, lon0_deg, x, y);
	    
	    project_ps_inv(&pps, x, y, &lat_out, &lon_out);

	    check(name, lat, lon, lat_out, lon_out);
	}
    }

    /* array function call tests */
    sprintf(name, "project_ps_arr(%f,%f,%f,%f) - preallocated",
		lat0_deg, lon0_deg, lat_deg, lon_deg);

    ok = testa_ps_arr(&pps, name, lat, lon, x_correct, y_correct, TRUE);

    if (ok)
    {
	sprintf(name, "project_ps_arr(%f,%f,%f,%f) - NULLed",
		lat0_deg, lon0_deg, lat_deg, lon_deg);
	
	testa_ps_arr(&pps, name, lat, lon, x_correct, y_correct, FALSE);
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

    project_parameters_t pps;

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
	x[i] = (double)rand() / (double)RAND_MAX * .7 + .7;

	/* longitude: between -pi and pi */
	y[i] = (double)rand() / (double)RAND_MAX * 6.26 - 3.14;
    }

    /* should be able to use any reference latitude > 45 and any ref lon */
    reference_lat = rand() % 45 + 45;
    reference_lon = rand() % 360 - 180;

    pps.ps.slat = reference_lat * DEG_TO_RAD;
    pps.ps.slon = reference_lon * DEG_TO_RAD;
    pps.ps.is_north_pole = 1;

    project_ps_arr(&pps, x, y, &xa, &ya, ARR_TEST_SIZE);

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	project_ps(&pps, x[i], y[i], &xp[i], &yp[i]); 
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
	project_ps_inv(&pps, xp[i], yp[i], &xp[i], &yp[i]);
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

    for (i = 0; i < ARR_TEST_SIZE; ++i)
    {
	xp[i] = xa[i];
	yp[i] = ya[i];
    }

    project_ps_arr_inv(&pps, xp, yp, &xa, &ya, ARR_TEST_SIZE);

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
    
    test_random_ps(NUM_REPS);
}

/********************************************* Lambert Azimuthal Equal Area  */
void test_lamaz()
{
}

/************************************************** Lambert Conformal Conic  */
void test_lamcc()
{
}

/************************************************** Albers Equal-Area Conic  */
void test_alb()
{
}

typedef int project_f_t(project_parameters_t*, double, double,
		     double*, double*);
typedef int project_f_arr_t(project_parameters_t*, double*, double*,
			 double**, double**, long);
typedef int project_f_inv_t(project_parameters_t*, double, double,
			 double*, double*);
typedef int project_f_arr_inv_t(project_parameters_t*, double*, double*,
			     double**, double**, long);
typedef double gen_lat_t(void);
typedef double gen_lon_t(void);

void testa_random(char * name,
                  project_parameters_t * pps,
		  project_f_t *project_f, 
		  project_f_arr_t *project_f_arr,
		  project_f_inv_t *project_f_inv,
		  project_f_arr_inv_t *project_f_arr_inv,
		  gen_lat_t *gen_lat,
		  gen_lon_t *gen_lon)
{
    {
	double lat, lon, x, y, lat_out, lon_out, lat_deg, lon_deg;
	int ok;
	
	lat_deg = gen_lat();
	lon_deg = gen_lon();
	
	lat = lat_deg * DEG_TO_RAD;
	lon = lon_deg * DEG_TO_RAD;
	
	x = y = lat_out = lon_out = HUGE_VAL;
	ok = FALSE;
	
	if (project_f(pps, lat, lon, &x, &y))
	{
	    if (project_f_inv(pps, x, y, &lat_out, &lon_out))
	    {
		ok = within_tol(lat, lat_out) && within_tol(lon, lon_out);
	    }
	}
	
	if (ok)
	{
	    ++nok;
	}
	else
	{
	    printf("Fail: %s(%f,%f):"
		   "(%f,%f) forward-> (%f,%f) inverse-> (%f,%f)\n",
		   name, lat_deg, lon_deg, lat, lon, 
		   x, y, lat_out, lon_out);
	    
	    ++nfail;
	}
    }
    {
	double *lat_rad, *lon_rad;
	double *px, *py;
	double *lat_out, *lon_out;
	int i, ok = FALSE;

	lat_rad = (double *) malloc (sizeof(double) * ARR_TEST_SIZE);
	lon_rad = (double *) malloc (sizeof(double) * ARR_TEST_SIZE);

	px = py = lat_out = lon_out = NULL;

	for (i = 0; i < ARR_TEST_SIZE; ++i)
	{
	    lat_rad[i] = gen_lat() * DEG_TO_RAD;
	    lon_rad[i] = gen_lon() * DEG_TO_RAD;
	}

	if (project_f_arr(pps, lat_rad, lon_rad, &px, &py, ARR_TEST_SIZE))
	{
	    if (project_f_arr_inv(pps, px, py, &lat_out, &lon_out, 
				  ARR_TEST_SIZE))
	    {
		ok = TRUE;
		for (i = 0; i < ARR_TEST_SIZE; ++i)
		{
		    if (!within_tol(lat_rad[i], lat_out[i]) ||
			!within_tol(lon_rad[i], lon_out[i]))
		    {
			printf("Fail: %s_arr:"
			       "(%f,%f) forward-> (%f,%f) inverse-> (%f,%f)\n",
			       name,
			       lat_rad[i], lon_rad[i], 
			       px[i], py[i],
			       lat_out[i], lon_out[i]);

			ok = FALSE;
		    }
		}
	    }
	}

	if (ok)
	{
	    ++nok;
	}
	else
	{
	    printf("Fail: %s_arr\n", name);
	    ++nfail;
	}

	free(lon_out);
	free(lat_out);
	free(px);
	free(py);
	free(lon_rad);
	free(lat_rad);
    }
}

double gen_all_lon()
{
    return (double) ( rand() % 360 - 180 );
}

double gen_lamaz_lat()
{
    /* mid-latitudes ? */
    return (double) ( rand() % 30 + 30 );
}

double gen_ps_lat()
{
    /* northern latitudes */
    return (double) ( rand() % 45 + 45 );
}

double gen_utm_lat()
{
    /* any latitude, -90 to +90 */
    return (double) ( rand() % 180 - 90 );
}

double gen_utm_lon()
{
    static int ref_lon = -1;

    if (ref_lon == -1)
    {
	ref_lon = gen_all_lon();
	return (double) ref_lon;
    }
    else
    {
	double r = (double)rand() / (double)RAND_MAX / 2.0 - 0.25;
	return (double) ref_lon + r;
    }
}

void testa_random_lamaz()
{
    project_parameters_t pps;
    char name[256];
    double center_lat, center_lon;

    center_lat = gen_lamaz_lat();
    center_lon = gen_all_lon();

    pps.lamaz.center_lat = center_lat * DEG_TO_RAD;
    pps.lamaz.center_lon = center_lon * DEG_TO_RAD;
    pps.lamaz.false_easting = 0;
    pps.lamaz.false_northing = 0;

    sprintf(name, "lamaz(%f,%f)", center_lat, center_lon);

    testa_random(name, &pps,
		 project_lamaz, project_lamaz_arr,
		 project_lamaz_inv, project_lamaz_arr_inv,
		 gen_lamaz_lat, gen_all_lon);
}

void testa_random_ps2()
{
    project_parameters_t pps;
    char name[256];
    int slat, slon;

    slat = (int)floor(gen_ps_lat());
    slon = (int)floor(gen_all_lon());

    pps.ps.slat = slat * DEG_TO_RAD;
    pps.ps.slon = slon * DEG_TO_RAD;
    pps.ps.is_north_pole = 1;
    pps.ps.false_easting = 0;
    pps.ps.false_northing = 0;
    pps.ps.
    sprintf(name, "ps(%d,%d)", slat, slon);

    testa_random(name, &pps,
		 project_ps, project_ps_arr,
		 project_ps_inv, project_ps_arr_inv,
		 gen_ps_lat, gen_all_lon);
}

void testa_random_utm2()
{
    project_parameters_t pps;
    char name[256];
    int slon;

    slon = gen_utm_lon();
    pps.utm.zone = slon;

    sprintf(name, "utm(%d)", slon);

    testa_random(name, &pps,
		 project_utm, project_utm_arr,
		 project_utm_inv, project_utm_arr_inv,
		 gen_utm_lat, gen_utm_lon);
}

void test_random_all()
{
    int i;

    srand(12345);
    /* testa_random_lamaz(); */

    for (i = 0; i < NUM_REPS; ++i)
    {
	testa_random_ps2();
	testa_random_utm2();
    }
}

void perf_test_ps()
{
    const int N = ARR_TEST_SIZE * NUM_REPS * 10;
    double *x, *x1, *xo;
    double *y, *y1, *yo;
    double lat0, lon0;
    project_parameters_t pps;

    int i, n=0;
    struct timeval tv1, tv2;
    struct timezone tz;
    int elapsed1, elapsed2;

    lat0 = 71 * DEG_TO_RAD;
    lon0 = -96 * DEG_TO_RAD;

    x = (double *) malloc(sizeof(double) * N);
    y = (double *) malloc(sizeof(double) * N);

    x1 = (double *) malloc(sizeof(double) * N);
    y1 = (double *) malloc(sizeof(double) * N);

    xo = (double *) malloc(sizeof(double) * N);
    yo = (double *) malloc(sizeof(double) * N);

    pps.ps.slat = lat0;
    pps.ps.slon = lon0;
    pps.ps.is_north_pole = 1;

    /* use same seed each time */
    srand(10101);

    for (i = 0; i < N; ++i)
    {
	x[i] = (double)rand() / (double)RAND_MAX * .707 + .707;
	y[i] = (double)rand() / (double)RAND_MAX * .707 + .707;
    }

    gettimeofday(&tv1, &tz);    
    project_ps_arr(&pps, x, y, &x1, &y1, N);
    gettimeofday(&tv2, &tz);

    elapsed1 = tv2.tv_sec - tv1.tv_sec;

    gettimeofday(&tv1, &tz);    
    for (i = 0; i < N; ++i)
    {
	project_ps(&pps, x[i], y[i], &xo[i], &yo[i]); 
    }
    gettimeofday(&tv2, &tz);
    elapsed2 = tv2.tv_sec - tv1.tv_sec;

    /* do they agree? */
    for (i = 0; i < N; ++i)
    {
	if (!within_tol(x1[i], xo[i]) || !within_tol(y1[i], yo[i]))
	{
/*
	    printf("x[%d] = %f, xo[%d] = %f, y[%d] = %f, yo[%d] = %f\n",
		   i, x[i], i, xo[i], i, y[i], i, yo[i]);
*/
	    ++n;
	}
    }
    
    if (n > 0)
    {
	++nfail;

	printf("Fail: perf_test_ps results don't agree! wrong: %d of %d\n",
	       n, N);
    }
    else
    {
	++nok;

	printf("perf_test_ps results -- (iters: %d)\n", N);
	printf("  Elapsed #1 (array): %d sec\n", elapsed1);
	printf("  Elapsed #2 (point): %d sec\n", elapsed2);
    }

    free(yo);
    free(xo);
    free(y1);
    free(x1);
    free(y);
    free(x);
}

int main(int argc, char * argv [])
{
    test_poly();
    test_utm();
    test_ps();
    test_lamaz();
    test_lamcc();
    test_alb();

    perf_test_ps();

    test_random_all();

    if (nfail == 0)
	printf("%d tests passed!\n", nok);
    else
	printf("%d ok, %d failures.\n", nok, nfail);

    return nfail;
}
