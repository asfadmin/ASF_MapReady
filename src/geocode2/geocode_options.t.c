#include "geocode_options.h"
#include "parse_options.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "asf_reporting.h"
#include "log.h"

static int noisy = 0;
static int nok = 0;
static int nfail = 0;
static projection_type_t pt;
static int l;
static char **p;
static double height;
static double pixel_size;

extern void set_options_testing();

static const double tol = 0.00001;
int within_tol(double a, double b)
{
    if (ISNAN(a)) return ISNAN(b);
    if (ISNAN(b)) return ISNAN(a);
    return fabs(b) < tol ? fabs(a) < tol : fabs((a-b)/b) < tol;
}

static void print_args(int len, char **p)
{
    int i;
    printf("length = %d\n", len);
    for (i = 0; i < len; ++i)
    {
	printf("p[%d] = %s\n", i, p[i]);
    }
}

void print_proj(project_parameters_t *pps, projection_type_t pt)
{
    switch (pt)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:
	    printf("UTM: %f %f %d\n",
		   pps->utm.lon0,
		   pps->utm.lat0,
		   pps->utm.zone);
	    break;

	case POLAR_STEREOGRAPHIC:
	    printf("PS: %f %f %d %f %f\n", 
		   pps->ps.slon,
		   pps->ps.slat,
		   pps->ps.is_north_pole,
		   pps->ps.false_easting,
		   pps->ps.false_northing);
	    break;

	case ALBERS_EQUAL_AREA:
	    printf("albers: %f %f %f %f %f %f\n",
		   pps->albers.center_meridian,
		   pps->albers.orig_latitude,
		   pps->albers.std_parallel1,
		   pps->albers.std_parallel2,
		   pps->albers.false_easting,
		   pps->albers.false_northing);
	    break;

	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	    printf("lamaz: %f %f %f %f\n",
		   pps->lamaz.center_lat,
		   pps->lamaz.center_lon,
		   pps->lamaz.false_easting,
		   pps->lamaz.false_northing);
	    break;

	case LAMBERT_CONFORMAL_CONIC:
	    printf("lamcc: %f %f %f %f %f %f\n",
		   pps->lamcc.plat1,
		   pps->lamcc.plat2,
		   pps->lamcc.lat0,
		   pps->lamcc.lon0,
		   pps->lamcc.false_easting,
		   pps->lamcc.false_northing);
	    break;

	default:
	    printf("print_proj: illegal projection type!\n");
    }
}

int projcmp(project_parameters_t* pps, project_parameters_t* pps2,
	    projection_type_t pt)
{
    switch (pt)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:
	    return 
		within_tol(pps->utm.lon0, pps2->utm.lon0) &&
		within_tol(pps->utm.lat0, pps2->utm.lat0) &&
		pps->utm.zone == pps->utm.zone;

	case POLAR_STEREOGRAPHIC:
	    return
		within_tol(pps->ps.slon, pps2->ps.slon) &&
		within_tol(pps->ps.slat, pps2->ps.slat) &&
		pps->ps.is_north_pole == pps2->ps.is_north_pole &&
		within_tol(pps->ps.false_easting, pps2->ps.false_easting) &&
		within_tol(pps->ps.false_northing, pps2->ps.false_northing);

	case ALBERS_EQUAL_AREA:
	    return
		within_tol(pps->albers.center_meridian,
			   pps2->albers.center_meridian) &&
		within_tol(pps->albers.orig_latitude,
			   pps2->albers.orig_latitude) &&
		within_tol(pps->albers.std_parallel1,
			   pps2->albers.std_parallel1) &&
		within_tol(pps->albers.std_parallel2,
			   pps2->albers.std_parallel2) &&
		within_tol(pps->albers.false_easting,
			   pps2->albers.false_easting) &&
		within_tol(pps->albers.false_northing,
			   pps2->albers.false_northing);

	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	    return
		within_tol(pps->lamaz.center_lat, pps2->lamaz.center_lat) &&
		within_tol(pps->lamaz.center_lon, pps2->lamaz.center_lon) &&
		within_tol(pps->lamaz.false_easting,
			   pps2->lamaz.false_easting) &&
		within_tol(pps->lamaz.false_northing,
			   pps2->lamaz.false_northing);

	case LAMBERT_CONFORMAL_CONIC:
	    return
		within_tol(pps->lamcc.plat1, pps2->lamcc.plat1) &&
		within_tol(pps->lamcc.plat2, pps2->lamcc.plat2) &&
		within_tol(pps->lamcc.lat0, pps2->lamcc.lat0) &&
		within_tol(pps->lamcc.lon0, pps2->lamcc.lon0) &&
		within_tol(pps->lamcc.false_easting,
			   pps2->lamcc.false_easting) &&
		within_tol(pps->lamcc.false_northing,
			   pps2->lamcc.false_northing);
	    break;

	default:
	    return FALSE;
    }    
}

void test_file(project_parameters_t* pps, projection_type_t pt)
{
    static char * opts [] =
	{ "--read-proj-file", "tmp" };
    projection_type_t pt2;

    p = opts;
    l = 2;

    write_args(pt, pps, "tmp");
    
    project_parameters_t * pps2 = get_geocode_options(&l, &p, &pt2, &height,
						      &pixel_size);

    if (l == 0 && pt == pt2 && projcmp(pps, pps2, pt))
    {
	++nok;
    }
    else
    {
	printf("Fail: read-proj-file %d %d %d\n", l, pt, pt2);
	printf("Passed in:\n");
	print_proj(pps, pt);
	printf("From File:\n");
	print_proj(pps2, pt2);
	++nfail;
    }
}

void no_arg_test()
{
    static char * opts [] =
	{ "unrelated" };

    p = opts; l = 1;
    project_parameters_t * pps = get_geocode_options(
	&l, &p, &pt, &height, &pixel_size);

    if (!pps && l == 1 && strcmp(p[0], "unrelated") == 0)
    {
	++nok;
    }
    else
    {
	printf("Fail: no_arg_test\n");
	++nfail;
    }
}

void utm_test_1()
{
    static char * opts [] =
	{ "-p", "utm", "-z", "4", "other", "stuff" };

    if (noisy) printf("UTM Test 1\n");

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(
	&l, &p, &pt, &height, &pixel_size);

    if (noisy) printf("...\n");
    if (pps && pps->utm.zone == 4 && pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	l == 2 && strcmp(p[0], "other") == 0 && strcmp(p[1], "stuff") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_1 %p %d %d %s %s\n", pps, pps->utm.zone,
	       l, p[0], p[1]);
	++nfail;
    }
}

void utm_test_2()
{
    static char * opts [] =
	{ "-p", "utm", "extra", "stuff" };

    if (noisy) printf("UTM Test 2\n");

    p = opts; l =  4;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps &&
	pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	strcmp(p[0], "extra") == 0 && strcmp(p[1], "stuff") == 0 && l == 2)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_2\n");
	++nfail;
    }
}

void utm_test_3()
{
    static char * opts [] =
	{ "-p" };

    p = opts; l =  1;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: utm_test_3\n");
	++nfail;
    }
}

void utm_test_4()
{
    static char * opts [] =
	{ "-p", "utm", "-z", "more", "args" };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: utm_test_4\n");
	++nfail;
    }
}

void utm_test_5()
{
    static char * opts [] =
	{ "-p", "utm", "--zone", "5", "--another-arg" };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps && pps->utm.zone == 5 && pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	l == 1 && strcmp(p[0], "--another-arg") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_5\n");
	++nfail;
    }
}

void utm_test_6()
{
    static char * opts [] =
	{ "-p", "utm", "-z", "illegal" };

    p = opts; l =  4;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: utm_test_6\n");
	++nfail;
    }
}

void utm_test_7()
{
    static char * opts [] =
	{ "--projection", "utm", "-z", "123f" };

    p = opts; l =  4;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: utm_test_7\n");
	++nfail;
    }
}

void utm_test_8()
{
    static char * opts [] =
	{ "--projection", "utm", "-z", "23" };

    p = opts; l =  4;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->utm.zone == 23 && pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	l == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_8\n");
	++nfail;
    }
}

void utm_test_9()
{
    static char * opts [] =
	{ "--projection", "utm", "-z", "-12" };

    p = opts; l =  4;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->utm.zone == -12 && pt == UNIVERSAL_TRANSVERSE_MERCATOR)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_9\n");
	++nfail;
    }
}

void utm_test_10()
{
    static char * opts [] =
	{ "--projection", "utm", "-z", "-13", "--lat0", "45.6" };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->utm.zone == -13 && 
	pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	within_tol(pps->utm.lat0, 45.6) &&
	ISNAN(pps->utm.lon0))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_10\n");
	++nfail;
    }
}

void utm_test_11()
{
    static char * opts [] =
	{ "--projection", "utm", "-z", "6", "-lon0", "45.6" };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->utm.zone == 6 && 
	pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	within_tol(pps->utm.lon0, 45.6) &&
	ISNAN(pps->utm.lat0))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_11\n");
	++nfail;
    }
}

void utm_test_12()
{
    static char * opts [] =
	{ "--projection", "utm",
	  "-lat0", "10.101",
	  "--lon0", "-146", "dork!" };

    p = opts; l = 7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->utm.zone == 6 && 
	pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	within_tol(pps->utm.lon0, -146) &&
	within_tol(pps->utm.lat0, 10.101) &&
	l == 1 && strcmp(p[0], "dork!") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_12\n");
	++nfail;
    }
}

void utm_test_13()
{
    static char * opts [] =
	{ "--projection", "utm",
	  "1", "2", "3", "4", "5" };

    p = opts; l = 7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps && pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	l == 5 && strcmp(p[0], "1") == 0 &&
	strcmp(p[1], "2") == 0 && strcmp(p[2], "3") == 0 &&
	strcmp(p[3], "4") == 0 && strcmp(p[4], "5") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	print_args(l, p);
	printf("Fail: utm_test_13\n");
	++nfail;
    }
}


void test_utm_options()
{
    utm_test_1();
    utm_test_2();
    utm_test_3();
    utm_test_4();
    utm_test_5();
    utm_test_6();
    utm_test_7();
    utm_test_8();
    utm_test_9();
    utm_test_10();
    utm_test_11();
    utm_test_12();
    utm_test_13();
}

void ps_test_1()
{
    static char * opts [] =
	{ "--projection", "ps", "-slat", "61", "-slon", "-59" };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slat == 61 && pps->ps.slon == -59 && 
	pps->ps.is_north_pole == 1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_1\n");
	++nfail;
    }
}

void ps_test_2()
{
    static char * opts [] =
	{ "--projection", "ps", "-slon", "59", "-slat", "-61", "f1", "f2"  };

    p = opts; l =  8;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slat == -61 && pps->ps.slon == 59 && 
	pps->ps.is_north_pole == 1 && pt == POLAR_STEREOGRAPHIC &&
	l == 2 && strcmp(p[0], "f1") == 0 && strcmp(p[1], "f2") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_2\n");
	++nfail;
    }
}


void ps_test_3()
{
    static char * opts [] =
	{ "-p", "ps", "--slon", "59", "--slat", "-61"  };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slat == -61 && pps->ps.slon == 59 && 
	pps->ps.is_north_pole == 1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_3\n");
	++nfail;
    }
}

void ps_test_4()
{
    static char * opts [] =
	{ "-p", "ps", "--center_longitude", "0", "--center_latitude", "61"  };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slat == 61 && pps->ps.slon == 0 && 
	pps->ps.is_north_pole == 1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_4\n");
	++nfail;
    }
}

void ps_test_5()
{
    static char * opts [] =
	{ "-p", "ps", "-slon", "59", "--center_latitude", "-61"  };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slat == -61 && pps->ps.slon == 59 && 
	pps->ps.is_north_pole == 1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_5\n");
	++nfail;
    }
}

void ps_test_6()
{
    static char * opts [] =
	{ "-p", "ps", "--slon", "123zap", "--slat", "-61"  };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: ps_test_6\n");
	++nfail;
    }
}

void ps_test_7()
{
    static char * opts [] =
	{ "-p", "ps", "-slon", "59", "--center_latitude", "-61", "-n"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slat == -61 && pps->ps.slon == 59 && 
	pps->ps.is_north_pole == 1 && pt == POLAR_STEREOGRAPHIC)
    {
	test_file(pps, pt);
	++nok;
    }
    else
    {
	printf("Fail: ps_test_7\n");
	++nfail;
    }
}

void ps_test_8()
{
    static char * opts [] =
	{ "-p", "ps", "-slon", "59", "--center_latitude", "-61", "-s"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slat == -61 && pps->ps.slon == 59 && 
	pps->ps.is_north_pole == -1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_8\n");
	++nfail;
    }
}

void ps_test_9()
{
    static char * opts [] =
	{ "-p", "ps", "-slon", "159", "-slat", "61", "--south_pole"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slat == 61 && pps->ps.slon == 159 && 
	pps->ps.is_north_pole == -1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_9\n");
	++nfail;
    }
}

void ps_test_10()
{
    static char * opts [] =
	{ "--projection", "ps", "-slat", "19", "-slon", "-101",
	  "--north_pole"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slon == -101 && pps->ps.slat == 19 && 
	pps->ps.is_north_pole == 1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_10\n");
	++nfail;
    }
}

void ps_test_11()
{
    static char * opts [] =
	{ "--projection", "ps", "-slat", "19", "-slon", "-101",
	  "--south_pole"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slon == -101 && pps->ps.slat == 19 && 
	pps->ps.is_north_pole == -1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_11\n");
	++nfail;
    }
}

void ps_test_12()
{
    static char * opts [] =
	{ "--projection", "ps", "--slat", "19", "--slon", "-101",
	  "--south_pole"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slon == -101 && pps->ps.slat == 19 && 
	pps->ps.is_north_pole == -1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_12\n");
	++nfail;
    }
}

void ps_test_13()
{
    static char * opts [] =
	{ "-p", "ps", "-slon", "-101", "-s"  };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slon == -101 && pps->ps.slat == -90 && 
	pps->ps.is_north_pole == -1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_13\n");
	++nfail;
    }
}

void ps_test_14()
{
    static char * opts [] =
	{ "-p", "ps", "-slon", "-101", "-n"  };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slon == -101 && pps->ps.slat == 90 && 
	pps->ps.is_north_pole == 1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_14\n");
	++nfail;
    }
}

void ps_test_15()
{
    static char * opts [] =
	{ "-p", "ps", "-slat", "-11", "-n"  };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->ps.slat == -11 && ISNAN(pps->ps.slon) && 
	pps->ps.is_north_pole == 1 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_15\n");
	++nfail;
    }
}

void ps_test_16()
{
    static char * opts [] =
	{ "-p", "ps", "-slat", "-n"  };

    p = opts; l =  4;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: ps_test_16\n");
	++nfail;
    }
}

void ps_test_17()
{
    static char * opts [] =
	{ "--projection", "ps", "--slon", "-n"  };

    p = opts; l =  4;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: ps_test_17\n");
	++nfail;
    }
}

void ps_test_18()
{
    static char * opts [] =
	{ "--projection", "ps", "--slat", "7", "--slon"  };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: ps_test_18\n");
	++nfail;
    }
}

void ps_test_19()
{
    static char * opts [] =
	{ "--projection", "ps", "--slon", "7", "--slat"  };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: ps_test_19\n");
	++nfail;
    }
}

void ps_test_20()
{
    static char * opts [] =
	{ "-p", "ps", "-slat", "-81", "-n", "-fn", "5.5", "-fe", "8.1"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->ps.slat, -81) &&
	ISNAN(pps->ps.slon) && 
	pps->ps.is_north_pole == 1 &&
	within_tol(pps->ps.false_easting, 8.1) && 
	within_tol(pps->ps.false_northing, 5.5) && 
	pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_20\n");
	++nfail;
    }
}

void ps_test_21()
{
    static char * opts [] =
	{ "-p", "ps", "-slat", "-11.101", "--north_pole", "-slon", "60",
	  "--false_northing", "-55000000", "--false_easting", "81000000"  };

    p = opts; l =  11;

    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->ps.slat, -11.101) &&
	within_tol(pps->ps.slon, 60) && 
	pps->ps.is_north_pole == 1 &&
	within_tol(pps->ps.false_easting, 81000000) && 
	within_tol(pps->ps.false_northing, -55000000) && 
	pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_21\n");
	++nfail;
    }
}

void ps_test_22()
{
    static char * opts [] =
	{ "-p", "ps", "-n",
	  "--false_northing", "-55000000", "--false_easting", "81000000",
	  "-slat", "11.101", "-slon", "60"  };

    p = opts; l =  11;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->ps.slat, 11.101) &&
	within_tol(pps->ps.slon, 60) && 
	pps->ps.is_north_pole == 1 &&
	within_tol(pps->ps.false_easting, 81000000) && 
	within_tol(pps->ps.false_northing, -55000000) &&
	pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_22\n");
	++nfail;
    }
}

void ps_test_23()
{
    static char * opts [] =
	{ "hey", "-p", "ps", "-n",
	  "--false_easting", "81000000",
	  "-slat", "51.101", "-slon", "60"  };

    p = opts; l = 10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->ps.slat, 51.101) &&
	within_tol(pps->ps.slon, 60) && 
	pps->ps.is_north_pole == 1 &&
	within_tol(pps->ps.false_easting, 81000000) && 
	within_tol(pps->ps.false_northing, 0) &&
	l == 1 && strcmp(p[0], "hey") == 0 &&
	pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_23 %f %f %d %f %f %d %s\n", pps->ps.slat,
	       pps->ps.slon, pps->ps.is_north_pole, pps->ps.false_easting,
	       pps->ps.false_northing, l, p[0]);
	++nfail;
    }
}

void ps_test_24()
{
    static char * opts [] =
	{ "-p", "ps", "-n",
	  "--false_northing", "81000000",
	  "-slat", "1.101", "-slon", "60", "phat"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->ps.slat, 1.101) &&
	within_tol(pps->ps.slon, 60) && 
	pps->ps.is_north_pole == 1 &&
	within_tol(pps->ps.false_northing, 81000000) && 
	within_tol(pps->ps.false_easting, 0) && pt == POLAR_STEREOGRAPHIC &&
	l == 1 && strcmp(p[0], "phat") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: ps_test_24\n");
	++nfail;
    }
}

void ps_test_25()
{
    static char * opts [] =
	{ "--projection", "ps", "--slat", "11fff9", "--slon", "-101",
	  "--south_pole", "more", "stuff"  };

    p = opts; l = 9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: ps_test_12\n");
	++nfail;
    }
}

void test_ps_options()
{
    ps_test_1();
    ps_test_2();
    ps_test_3();
    ps_test_4();
    ps_test_5();
    ps_test_6();
    ps_test_7();
    ps_test_8();
    ps_test_9();
    ps_test_10();
    ps_test_11();
    ps_test_12();
    ps_test_13();
    ps_test_14();
    ps_test_15();
    ps_test_16();
    ps_test_17();
    ps_test_18();
    ps_test_19();
    ps_test_20();
    ps_test_21();
    ps_test_22();
    ps_test_23();
    ps_test_24();
    ps_test_25();
}

void lamcc_test_1()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "17", "--plat2", "18",
	  "--lat0", "45", "--lon0", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->lamcc.plat1 == 17 && pps->lamcc.plat2 == 18 &&
	pps->lamcc.lat0 == 45 && pps->lamcc.lon0 == 0 &&
	pt == LAMBERT_CONFORMAL_CONIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_1\n");
	++nfail;
    }    
}

void lamcc_test_2()
{
    static char * opts [] =
	{ "-p", "lamcc", "--lat_1", "17", "--lat_2", "18",
	  "--slat", "45", "--slon", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->lamcc.plat1 == 17 && pps->lamcc.plat2 == 18 &&
	pps->lamcc.lat0 == 45 && pps->lamcc.lon0 == 0 &&
	pt == LAMBERT_CONFORMAL_CONIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_2\n");
	++nfail;
    }    
}

void lamcc_test_3()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "17", "--plat2", "18",
	  "--center_latitude", "45", "--center_longitude", "0.03"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->lamcc.plat1 == 17 && pps->lamcc.plat2 == 18 &&
	pps->lamcc.lat0 == 45 && within_tol(pps->lamcc.lon0, 0.03) &&
	pt == LAMBERT_CONFORMAL_CONIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_3\n");
	++nfail;
    }    
}

void lamcc_test_4()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "17", "--plat2", "18",
	  "-slat", "45.8", "-slon", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->lamcc.plat1 == 17 && pps->lamcc.plat2 == 18 &&
	within_tol(pps->lamcc.lat0, 45.8) && pps->lamcc.lon0 == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_4\n");
	++nfail;
    }    
}

void lamcc_test_5()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "17", "--plat2", "18",
	  "--lat_0", "45", "--lon_0", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->lamcc.plat1 == 17 && pps->lamcc.plat2 == 18 &&
	pps->lamcc.lat0 == 45 && pps->lamcc.lon0 == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_5\n");
	++nfail;
    }    
}

void lamcc_test_6()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "17z", "--plat2", "18",
	  "--lat_0", "45", "--lon_0", "0"  };

    p = opts; l = 10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: lamcc_test_6\n");
	++nfail;
    }    
}

void lamcc_test_7()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "17", "--plat2", "1o8",
	  "--lat_0", "45", "--lon_0", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: lamcc_test_7\n");
	++nfail;
    }    
}

void lamcc_test_8()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "17", "--plat2", "18",
	  "--lat_0", "4r5", "--lon_0", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: lamcc_test_8\n");
	++nfail;
    }    
}

void lamcc_test_9()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "17", "--plat2", "18",
	  "--lat_0", "45", "--lon_0", "lookyhere!"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: lamcc_test_9\n");
	++nfail;
    }    
}

void lamcc_test_10()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "--plat2", "18",
	  "--lat_0", "45", "--lon_0", "0"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: lamcc_test_10\n");
	++nfail;
    }    
}

void lamcc_test_11()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "17", "--plat2",
	  "--lat_0", "45", "--lon_0", "0"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: lamcc_test_11\n");
	++nfail;
    }    
}

void lamcc_test_12()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "17", "--plat2", "0",
	  "--lat_0", "45", "--lon_0"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: lamcc_test_12\n");
	++nfail;
    }    
}

void lamcc_test_13()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat1", "17", "--plat2", "0",
	  "--lon_0", "45", "--lat_0"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: lamcc_test_13\n");
	++nfail;
    }    
}

void lamcc_test_14()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--lat_0", "17", "--plat2", "0",
	  "--lon_0", "45", "--plat1"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: lamcc_test_14\n");
	++nfail;
    }    
}

void lamcc_test_15()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--lat_0", "17", "--lon_0", "45",
	  "--plat1", "0", "--plat2"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: lamcc_test_15\n");
	++nfail;
    }    
}

void lamcc_test_16()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--lat_0", "17", "--lon_0", "45",
	  "--plat1", "0"  };

    p = opts; l =  8;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->lamcc.plat1 == 0 && ISNAN(pps->lamcc.plat2) &&
	pps->lamcc.lat0 == 17 && pps->lamcc.lon0 == 45)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_16 %f %f %f %f\n",
	       pps->lamcc.plat1,
	       pps->lamcc.plat2,
	       pps->lamcc.lat0,
	       pps->lamcc.lon0);
	++nfail;
    }    
}

void lamcc_test_17()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--plat2", "17.3", "--lon_0", "45",
	  "--plat1", "0"  };

    p = opts; l =  8;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->lamcc.plat1 == 0 && ISNAN(pps->lamcc.lat0) &&
	within_tol(pps->lamcc.plat2, 17.3) && pps->lamcc.lon0 == 45)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_17 %f\n", pps->lamcc.plat2);
	++nfail;
    }    
}

void lamcc_test_18()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--lat_0", "17", "--lon_0", "45",
	  "--plat2", "0"  };

    p = opts; l =  8;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->lamcc.plat2 == 0 && ISNAN(pps->lamcc.plat1) &&
	pps->lamcc.lat0 == 17 && pps->lamcc.lon0 == 45)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_18\n");
	++nfail;
    }    
}

void lamcc_test_19()
{
    static char * opts [] =
	{ "--projection", "lamcc", "--lat_0", "17", "--plat2", "45",
	  "--plat1", "0"  };

    p = opts; l =  8;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (pps->lamcc.plat1 == 0 && ISNAN(pps->lamcc.lon0) &&
	pps->lamcc.lat0 == 17 && pps->lamcc.plat2 == 45)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_19\n");
	++nfail;
    }    
}

void lamcc_test_20()
{
    static char * opts [] =
	{ "--projection", "lamcc"  };

    p = opts; l =  2;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (ISNAN(pps->lamcc.plat1) && ISNAN(pps->lamcc.lon0) &&
	ISNAN(pps->lamcc.lat0) && ISNAN(pps->lamcc.plat2))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_20\n");
	++nfail;
    }    
}

void lamcc_test_21()
{
    static char * opts [] =
	{ "-p", "lamcc", "--lat_0", "17.778", "--plat2", "45.001",
	  "--plat1", "-1.123", "--lon0", "-111.111"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_21\n");
	++nfail;
    }    
}

void lamcc_test_22()
{
    static char * opts [] =
	{ "-p", "lamcc", "--lat_0", "17.778", "--plat2", "45.001",
	  "--plat1", "-1.123", "--lon0", "-111.111", "-fe", "1",
	  "-fn", "7888" };

    p = opts; l =  14;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778) &&
	within_tol(pps->lamcc.false_easting, 1) &&
	within_tol(pps->lamcc.false_northing, 7888))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_22\n");
	++nfail;
    }    
}

void lamcc_test_23()
{
    static char * opts [] =
	{ "-p", "lamcc", "--lat_0", "17.778", "--plat2", "45.001",
	  "--plat1", "-1.123", "--lon0", "-111.111", "-fn", "1",
	  "-fn", "7888" };

    p = opts; l =  14;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (!pps)
	++nok;
    else
    {
	printf("Fail: lamcc_test_23\n");
	++nfail;
    }    
}

void lamcc_test_24()
{
    static char * opts [] =
	{ "-p", "lamcc", "--lat_0", "17.778", "--plat2", "45.001",
	  "--plat1", "-1.123", "--lon0", "-111.111",
	  "-fe", "7888" };

    p = opts; l =  12;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778) &&
	within_tol(pps->lamcc.false_easting, 7888) &&
	within_tol(pps->lamcc.false_northing, 0))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_24\n");
	++nfail;
    }    
}

void lamcc_test_25()
{
    static char * opts [] =
	{ "-p", "lamcc", "--lat_0", "17.778", "--plat2", "45.001",
	  "--plat1", "-1.123", "--lon0", "-111.111",
	  "-fn", "-7888" };

    p = opts; l =  12;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778) &&
	within_tol(pps->lamcc.false_northing, -7888) &&
	within_tol(pps->lamcc.false_easting, 0))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_25\n");
	++nfail;
    }    
}

void lamcc_test_26()
{
    static char * opts [] =
	{ "-p", "lamcc", "--lat_0", "17.778", "--plat2", "45.001",
	  "--plat1", "-1.123", "--lon0", "-111.111", "-fe", "1",
	  "-fn", "7888", "--scale_factor", "1.1" };

    p = opts; l =  16;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778) &&
	within_tol(pps->lamcc.scale_factor, 1.1) &&
	within_tol(pps->lamcc.false_easting, 1) &&
	within_tol(pps->lamcc.false_northing, 7888))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_26\n");
	++nfail;
    }    
}

void lamcc_test_27()
{
    static char * opts [] =
	{ "-p", "lamcc", "--lat_0", "17.778", "--plat2", "45.001",
	  "--plat1", "-1.123", "--lon0", "-111.111", "-fe", "1",
	   "--scale_factor", "91.1", "--false_northing", "7888", };

    p = opts; l =  16;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778) &&
	within_tol(pps->lamcc.scale_factor, 91.1) &&
	within_tol(pps->lamcc.false_easting, 1) &&
	within_tol(pps->lamcc.false_northing, 7888))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_27\n");
	++nfail;
    }    
}

void lamcc_test_28()
{
    static char * opts [] =
	{ "-p", "lamcc", "--lat_0", "17.778", "--plat2", "45.001",
	  "--plat1", "-1.123", "--lon0", "-111.111", "-fn", "21",
	  "--false_easting", "7888", "-sf", "1.12", "final" };

    p = opts; l =  17;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778) &&
	within_tol(pps->lamcc.scale_factor, 1.12) &&
	within_tol(pps->lamcc.false_easting, 7888) &&
	within_tol(pps->lamcc.false_northing, 21) &&
	l == 1 &&
	strcmp(p[0], "final") == 0 &&
	pt == LAMBERT_CONFORMAL_CONIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_28 %f,%f,%f,%f,%f,%f,%f\n",
	       pps->lamcc.plat1,
	       pps->lamcc.lon0,
	       pps->lamcc.plat2,
	       pps->lamcc.lat0,
	       pps->lamcc.scale_factor,
	       pps->lamcc.false_easting,
	       pps->lamcc.false_northing);
	++nfail;
    }    
}

void lamcc_test_29()
{
    static char * opts [] =
	{ "-p", "lamcc", "--lat_0", "17.778", "--plat2", "45.001",
	  "--plat1", "-1.123", "--lon0", "-111.111", "-fe", "1",
	  "-fn", "7888", "--scale_factor", "1.1" };

    p = opts; l =  16;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778) &&
	within_tol(pps->lamcc.scale_factor, 1.1) &&
	within_tol(pps->lamcc.false_easting, 1) &&
	within_tol(pps->lamcc.false_northing, 7888))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_29\n");
	++nfail;
    }    
}

void lamcc_test_30()
{
    static char * opts [] =
	{ "-p", "lamcc", "--plat2", "45.001", "--scale_factor", "1.1",
	  "-fn", "7888", "--lon0", "-111.111", "-fe", "1",
	   "--plat1", "-1.123", "--lat_0", "17.778"  };

    p = opts; l =  16;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778) &&
	within_tol(pps->lamcc.scale_factor, 1.1) &&
	within_tol(pps->lamcc.false_easting, 1) &&
	within_tol(pps->lamcc.false_northing, 7888))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamcc_test_30\n");
	++nfail;
    }    
}

void test_lamcc_options()
{
    lamcc_test_1();
    lamcc_test_2();
    lamcc_test_3();
    lamcc_test_4();
    lamcc_test_5();
    lamcc_test_6();
    lamcc_test_7();
    lamcc_test_8();
    lamcc_test_9();
    lamcc_test_10();
    lamcc_test_11();
    lamcc_test_12();
    lamcc_test_13();
    lamcc_test_14();
    lamcc_test_15();
    lamcc_test_16();
    lamcc_test_17();
    lamcc_test_18();
    lamcc_test_19();
    lamcc_test_21();
    lamcc_test_22();
    lamcc_test_23();
    lamcc_test_24();
    lamcc_test_25();
    lamcc_test_26();
    lamcc_test_27();
    lamcc_test_28();
    lamcc_test_29();
    lamcc_test_30();
}

void lamaz_test_1()
{
    static char * opts [] =
	{ "-p", "lamaz", "--lat0", "17.778", "--lon0", "45.001",
	  "-fn", "7888", "-fe", "110000" };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 110000) &&
	within_tol(pps->lamaz.false_northing, 7888))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamaz_test_1\n");
	++nfail;
    }    
}

void lamaz_test_2()
{
    static char * opts [] =
	{ "-p", "lamaz", "--lat_0", "17.778", "--center_longitude", "45.001",
	  "-fn", "7888", "-fe", "110000" };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 110000) &&
	within_tol(pps->lamaz.false_northing, 7888))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamaz_test_2\n");
	++nfail;
    }    
}

void lamaz_test_3()
{
    static char * opts [] =
	{ "-p", "lamaz", "-slat", "17.778", "--slon", "45.001",
	  "-fn", "7888", "-fe", "110000" };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 110000) &&
	within_tol(pps->lamaz.false_northing, 7888))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamaz_test_3\n");
	++nfail;
    }    
}

void lamaz_test_4()
{
    static char * opts [] =
	{ "-p", "lamaz", "--slat", "17.778", "-slon", "45.001",
	  "-fn", "7888", "-fe", "110000", "additional_arg" };

    p = opts; l =  11;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 110000) &&
	within_tol(pps->lamaz.false_northing, 7888) &&
	l == 1 &&
	strcmp(p[0], "additional_arg") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamaz_test_4\n");
	++nfail;
    }    
}

void lamaz_test_5()
{
    static char * opts [] =
	{ "yo",
	  "-p", "lamaz", "--center_latitude", "17.778", "--lon_0", "45.001",
	  "-fn", "7888", "-fe", "110000", "dude" };

    p = opts; l =  12;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 110000) &&
	within_tol(pps->lamaz.false_northing, 7888) &&
	l == 2 && strcmp(p[0], "yo") == 0 && strcmp(p[1], "dude") == 0 &&
	pt == LAMBERT_AZIMUTHAL_EQUAL_AREA)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamaz_test_5\n");
	++nfail;
    }    
}

void lamaz_test_6()
{
    static char * opts [] =
	{ "stuff", "at", "the", "beginning", "-p", "lamaz",
	  "--center_latitude", "17.778", "--lon_0", "45.001",
	  "stuff", "at", "the", "end" };

    p = opts; l =  14;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 0) &&
	within_tol(pps->lamaz.false_northing, 0) &&
	l == 8 &&
	pt == LAMBERT_AZIMUTHAL_EQUAL_AREA &&
	strcmp(p[0], "stuff") == 0 &&
	strcmp(p[1], "at") == 0 &&
	strcmp(p[2], "the") == 0 &&
	strcmp(p[3], "beginning") == 0 &&
	strcmp(p[4], "stuff") == 0 &&
	strcmp(p[5], "at") == 0 &&
	strcmp(p[6], "the") == 0 &&
	strcmp(p[7], "end") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamaz_test_6\n");
	++nfail;
    }    
}

void lamaz_test_7()
{
    static char * opts [] =
	{ "--log", "logfile", "-p", "lamaz", "input", "output"  };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (ISNAN(pps->lamaz.center_lon) &&
	ISNAN(pps->lamaz.center_lat) &&
	within_tol(pps->lamaz.false_easting, 0) &&
	within_tol(pps->lamaz.false_northing, 0) &&
	l == 4 &&
	pt == LAMBERT_AZIMUTHAL_EQUAL_AREA &&
	strcmp(p[0], "--log") == 0 &&
	strcmp(p[1], "logfile") == 0 &&
	strcmp(p[2], "input") == 0 &&
	strcmp(p[3], "output") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamaz_test_7\n");
	++nfail;
    }    
}

void test_lamaz_options()
{
    lamaz_test_1();
    lamaz_test_2();
    lamaz_test_3();
    lamaz_test_4();
    lamaz_test_5();
    lamaz_test_6();
    lamaz_test_7();
}

void albers_test_1()
{
    static char * opts [] =
	{ "-p", "albers"  };

    p = opts; l =  2;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size);

    if (ISNAN(pps->albers.center_meridian) &&
	ISNAN(pps->albers.orig_latitude) &&
	within_tol(pps->albers.false_easting, 0) &&
	within_tol(pps->albers.false_northing, 0) &&
	pt == ALBERS_EQUAL_AREA &&
	l == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_1\n");
	++nfail;
    }
}

void albers_test_2()
{
    static char * opts [] =
	{ "--quiet", "-p", "albers", "--plat2", "45.001",
	  "-fn", "7888", "--lon0", "-111.111", "-fe", "1",
	   "--plat1", "-1.123", "--lat_0", "17.778", "f1", "f2"  };

    if (noisy) printf("albers_test_2\n");

    p = opts; l =  17;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size);

    if (noisy) printf("... albers_test_2 %d %s %s %s\n", l, p[0], p[1], p[2]);

    if (within_tol(pps->albers.std_parallel1, -1.123) &&
	within_tol(pps->albers.center_meridian, -111.111) &&
	within_tol(pps->albers.std_parallel2, 45.001) &&
	within_tol(pps->albers.orig_latitude, 17.778) &&
	within_tol(pps->albers.false_easting, 1) &&
	within_tol(pps->albers.false_northing, 7888) &&
	l == 3 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0,
	strcmp(p[1], "f1") == 0,
	strcmp(p[2], "f2") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_2\n");
	++nfail;
    }    
}

void albers_test_3()
{
    static char * opts [] =
	{ "--quiet", "--projection", "albers", "-fe", "1",
	  "-fn", "7888", "--plat2", "-45.001", "--lon0", "-111.111", 
	   "--lat_0", "+17.778", "--plat1", "-1.123", "foo", "bar"  };

    if (noisy) printf("albers_test_3\n");

    p = opts; l =  17;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size);

    if (within_tol(pps->albers.std_parallel1, -1.123) &&
	within_tol(pps->albers.center_meridian, -111.111) &&
	within_tol(pps->albers.std_parallel2, -45.001) &&
	within_tol(pps->albers.orig_latitude, 17.778) &&
	within_tol(pps->albers.false_easting, 1) &&
	within_tol(pps->albers.false_northing, 7888) &&
	l == 3 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0,
	strcmp(p[1], "foo") == 0,
	strcmp(p[2], "bar") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_3\n");
	++nfail;
    }    
}

void albers_test_4()
{
    static char * opts [] =
	{ "--quiet", "--projection", "albers", "-fe", "1",
	  "-fn", "7888", "--second-standard-parallel", "-45.001",
	  "--lon0", "-111.111", "--lat_0", "+17.778",
	  "--first-standard-parallel", "-1.123", "foo", "bar", "baz"  };

    if (noisy) printf("albers_test_4\n");

    p = opts; l =  18;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size);

    if (within_tol(pps->albers.std_parallel1, -1.123) &&
	within_tol(pps->albers.center_meridian, -111.111) &&
	within_tol(pps->albers.std_parallel2, -45.001) &&
	within_tol(pps->albers.orig_latitude, 17.778) &&
	within_tol(pps->albers.false_easting, 1) &&
	within_tol(pps->albers.false_northing, 7888) &&
	l == 4 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0 &&
	strcmp(p[1], "foo") == 0 &&
	strcmp(p[2], "bar") == 0 &&
	strcmp(p[3], "baz") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_4\n");
	++nfail;
    }    
}

void albers_test_5()
{
    static char * opts [] =
	{ "--quiet", "--projection", "albers", "-fe", "1",
	  "-fn", "7888", "--second_standard_parallel", "45.001",
	  "--central-meridian", "111.111", "--lat_0", "+17.778",
	  "--first_standard_parallel", "-11.123", "--height", "14",
	  "foo", "bar", "baz"  };

    if (noisy) printf("albers_test_5\n");

    p = opts; l = 20;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size);

    if (within_tol(pps->albers.std_parallel1, -11.123) &&
	within_tol(pps->albers.center_meridian, 111.111) &&
	within_tol(pps->albers.std_parallel2, 45.001) &&
	within_tol(pps->albers.orig_latitude, 17.778) &&
	within_tol(pps->albers.false_easting, 1) &&
	within_tol(pps->albers.false_northing, 7888) &&
	l == 4 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0 &&
	strcmp(p[1], "foo") == 0 &&
	strcmp(p[2], "bar") == 0 &&
	strcmp(p[3], "baz") == 0 &&
	within_tol(height, 14))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_5 %f\n", height);
	++nfail;
    }    
}

void albers_test_6()
{
    static char * opts [] =
	{ "--quiet", "--height", "14.9", "--projection", "albers", "-fe", "1",
	  "-fn", "7888", "--second_standard_parallel", "45.001",
	  "--central-meridian", "111.111", "--lat_0", "+17.778",
	  "--first_standard_parallel", "-11.123",
	  "foo", "bar", "baz"  };

    if (noisy) printf("albers_test_6\n");

    p = opts; l = 20;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size);

    if (within_tol(pps->albers.std_parallel1, -11.123) &&
	within_tol(pps->albers.center_meridian, 111.111) &&
	within_tol(pps->albers.std_parallel2, 45.001) &&
	within_tol(pps->albers.orig_latitude, 17.778) &&
	within_tol(pps->albers.false_easting, 1) &&
	within_tol(pps->albers.false_northing, 7888) &&
	l == 4 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0 &&
	strcmp(p[1], "foo") == 0 &&
	strcmp(p[2], "bar") == 0 &&
	strcmp(p[3], "baz") == 0 &&
	within_tol(height, 14.9))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_6 %f %d\n", height, l);
	++nfail;
    }    
}

void albers_test_7()
{
    static char * opts [] =
	{ "--quiet", "--height", "14.9", "--write-proj-file", "tmp",
	  "--pixel_size", "100",
	  "--projection", "albers", "-fe", "1",
	  "-fn", "7888", "--second_standard_parallel", "45.001",
	  "--central-meridian", "111.111", "--lat_0", "+17.778",
	  "--first_standard_parallel", "-11.123",
	  "foo", "bar", "baz"  };

    if (noisy) printf("albers_test_7\n");

    p = opts; l = 24;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size);

    if (within_tol(pps->albers.std_parallel1, -11.123) &&
	within_tol(pps->albers.center_meridian, 111.111) &&
	within_tol(pps->albers.std_parallel2, 45.001) &&
	within_tol(pps->albers.orig_latitude, 17.778) &&
	within_tol(pps->albers.false_easting, 1) &&
	within_tol(pps->albers.false_northing, 7888) &&
	l == 4 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0 &&
	strcmp(p[1], "foo") == 0 &&
	strcmp(p[2], "bar") == 0 &&
	strcmp(p[3], "baz") == 0 &&
	within_tol(height, 14.9) &&
	within_tol(pixel_size, 100))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_7 %f %d\n", height, l);
	print_args(l,p);
	++nfail;
    }    
}

void albers_test_8()
{
    static char * opts [] =
	{ "--quiet", 
	  "--projection", "albers", "-fe", "1",
	  "-fn", "7888", "--second_standard_parallel", "45.001",
	  "--central-meridian", "111.111", "--lat_0", "+17.778",
	  "--first_standard_parallel", "-11.123",
	  "--height", "14.9", "--write-proj-file", "bork",
	  "--pixel_size", "100", "foo", "bar", "baz"  };

    if (noisy) printf("albers_test_8\n");

    p = opts; l = 24;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size);

    if (within_tol(pps->albers.std_parallel1, -11.123) &&
	within_tol(pps->albers.center_meridian, 111.111) &&
	within_tol(pps->albers.std_parallel2, 45.001) &&
	within_tol(pps->albers.orig_latitude, 17.778) &&
	within_tol(pps->albers.false_easting, 1) &&
	within_tol(pps->albers.false_northing, 7888) &&
	l == 4 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0 &&
	strcmp(p[1], "foo") == 0 &&
	strcmp(p[2], "bar") == 0 &&
	strcmp(p[3], "baz") == 0 &&
	within_tol(height, 14.9) &&
	within_tol(pixel_size, 100))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_8 %f %d\n", height, l);
	print_args(l,p);
	++nfail;
    }    
}

void albers_test_9()
{
    static char * opts [] =
	{ "--quiet", "dude",
	  "-h", "14.9",
	  "--projection", "albers", "-fe", "1",
	  "-fn", "7888", "--second_standard_parallel", "45.001",
	  "--central-meridian", "111.111", "--lat_0", "+17.778",
	  "--first_standard_parallel", "-11.123",
	  "--pixel-size", "10.01", "foo", 
	  "--write-proj-file", "bork",
	  "bar", "baz"  };

    if (noisy) printf("albers_test_9\n");

    p = opts; l = 25;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size);

    if (within_tol(pps->albers.std_parallel1, -11.123) &&
	within_tol(pps->albers.center_meridian, 111.111) &&
	within_tol(pps->albers.std_parallel2, 45.001) &&
	within_tol(pps->albers.orig_latitude, 17.778) &&
	within_tol(pps->albers.false_easting, 1) &&
	within_tol(pps->albers.false_northing, 7888) &&
	l == 5 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0 &&
	strcmp(p[1], "dude") == 0 &&
	strcmp(p[2], "foo") == 0 &&
	strcmp(p[3], "bar") == 0 &&
	strcmp(p[4], "baz") == 0 &&
	within_tol(height, 14.9) &&
	within_tol(pixel_size, 10.01))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_9 %f %d\n", height, l);
	print_args(l,p);
	++nfail;
    }    
}

void albers_test_10()
{
    static char * opts [] =
	{ "--quiet", "dude",
	  "-h", "14.9",
	  "--projection", "albers", "-fe", "1",
	  "-fn", "7888", "--second_standard_parallel", "45.001",
	  "--central-meridian", "111.111", "--lat_0", "+17.7878",
	  "-wpf", "bork",
	  "--first_standard_parallel", "-11.123",
	  "--pixel-size", "10.01", "foo", 
	  "bar", "baz", "bork" };

    if (noisy) printf("albers_test_10\n");

    p = opts; l = 26;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size);

    if (within_tol(pps->albers.std_parallel1, -11.123) &&
	within_tol(pps->albers.center_meridian, 111.111) &&
	within_tol(pps->albers.std_parallel2, 45.001) &&
	within_tol(pps->albers.orig_latitude, 17.7878) &&
	within_tol(pps->albers.false_easting, 1) &&
	within_tol(pps->albers.false_northing, 7888) &&
	l == 6 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0 &&
	strcmp(p[1], "dude") == 0 &&
	strcmp(p[2], "foo") == 0 &&
	strcmp(p[3], "bar") == 0 &&
	strcmp(p[4], "baz") == 0 &&
	strcmp(p[5], "bork") == 0 &&
	within_tol(height, 14.9) &&
	within_tol(pixel_size, 10.01))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_10 %f %d\n", height, l);
	print_args(l,p);
	++nfail;
    }    
}

void albers_test_11()
{
    static char * opts [] =
	{ "--quiet", "dude",
	  "--projection", "albers", "--false_easting", "1000000",
	  "--false_northing", "78880000", 
	  "--second_standard_parallel", "45.001",
	  "--central-meridian", "111.111", "--center_latitude", "7.78078",
	  "-wpf", "bork", 
	  "--first_standard_parallel", "-11.123",
	  "-ps", "1001.1001", "foo", "-h", "1499.009", 
	  "bar", "baz", "bork" };

    if (noisy) printf("albers_test_11\n");

    p = opts; l = 26;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size);

    if (within_tol(pps->albers.std_parallel1, -11.123) &&
	within_tol(pps->albers.center_meridian, 111.111) &&
	within_tol(pps->albers.std_parallel2, 45.001) &&
	within_tol(pps->albers.orig_latitude, 7.78078) &&
	within_tol(pps->albers.false_easting, 1000000) &&
	within_tol(pps->albers.false_northing, 78880000) &&
	l == 6 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0 &&
	strcmp(p[1], "dude") == 0 &&
	strcmp(p[2], "foo") == 0 &&
	strcmp(p[3], "bar") == 0 &&
	strcmp(p[4], "baz") == 0 &&
	strcmp(p[5], "bork") == 0 &&
	within_tol(height, 1499.009) &&
	within_tol(pixel_size, 1001.1001))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_11 %f %d\n", height, l);
	print_args(l,p);
	++nfail;
    }    
}

void test_albers_options()
{
    albers_test_1();
    albers_test_2();
    albers_test_3();
    albers_test_4();
    albers_test_5();
    albers_test_6();
    albers_test_7();
    albers_test_8();
    albers_test_9();
    albers_test_10();
    albers_test_11();
}

const char * random_string(int length)
{
    static const char * vowels = "aeiou";
    static const char * consonants = "bcdfghjklmnpqrstvwxyz";

    static char str[64];
    int i;

    for (i = 0; i < length; ++i)
    {
	if (i%2==0)
	    str[i] = consonants[ rand() % strlen(consonants) ];
	else
	    str[i] = vowels[ rand() % strlen(vowels) ];
    }

    str[length] = '\0';
    return str;
}

void test_random()
{
    projection_type_t pt;
    int argc = 0;
    char *argv[256];
    char *extra_stuff[256];

    srand(121);

    /* choose a random projection type */
    pt = (int) (rand() % 5);

    if (rand() % 10 > 4)
    {
	/* add some stuff to the beginning */
	int i, n = rand() % 10 + 1;
	argc += n;

	for (i = 0; i < n; ++i)
	{
	    argv[i] = strdup(random_string(rand() % 50 + 5));
	    extra_stuff[i] = strdup(argv[i]);
	}
    }

    if (rand() % 2 == 0)
	argv[argc++] = "-p";
    else
	argv[argc++] = "--projection";

    switch (pt)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:
	    argv[argc++] = "utm";
	    break;
	case POLAR_STEREOGRAPHIC:
	    argv[argc++] = "ps";
	    break;
	case ALBERS_EQUAL_AREA:
	    argv[argc++] = "albers";
	    break;
	case LAMBERT_CONFORMAL_CONIC:
	    argv[argc++] = "lamcc";
	    break;
	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	    argv[argc++] = "lamaz";
	    break;
	default:
	    break;
    }
    /* todo...*/
}


int main(int argc, char * argv [])
{
    quietflag = 1;
    set_options_testing(1);
    noisy = 0;

    if (noisy) printf("No arg test\n");
    no_arg_test();
    if (noisy) printf("UTM test\n");
    test_utm_options();
    if (noisy) printf("PS test\n");
    test_ps_options();
    if (noisy) printf("Lamcc test\n");
    test_lamcc_options();
    if (noisy) printf("Lamaz test\n");
    test_lamaz_options();
    if (noisy) printf("Albers test\n");
    test_albers_options();

/*    test_random(); */

    if (nfail == 0)
	printf("%d tests passed!\n", nok);
    else
	printf("%d ok, %d failures.\n", nok, nfail);

    return nfail;    
}
