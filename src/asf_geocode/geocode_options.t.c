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
static datum_type_t datum;
static resample_method_t rm;

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
						      &pixel_size, &datum, &rm);

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
	&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "utm", "-z", "4", "--datum", "WGS84", "other", "stuff" };

    if (noisy) printf("UTM Test 1\n");

    p = opts; l =  8;
    project_parameters_t * pps = get_geocode_options(
	&l, &p, &pt, &height, &pixel_size, &datum, &rm);

    if (noisy) printf("...\n");
    if (pps && pps->utm.zone == 4 && pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	l == 2 && strcmp(p[0], "other") == 0 && strcmp(p[1], "stuff") == 0 &&
	datum == WGS84_DATUM)
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
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "utm", "-z", "-13", "--center-latitude", "45.6" };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "utm", "-z", "6", "--central-meridian", "45.6" };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	  "--center-latitude", "10.101",
	  "--central-meridian", "-146", "dork!" };

    p = opts; l = 7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height,
						     &pixel_size, &datum, &rm);

    if (pps->utm.zone == MAGIC_UNSET_INT && 
	pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	within_tol(pps->utm.lon0, -146) &&
	within_tol(pps->utm.lat0, 10.101) &&
	ISNAN(height) &&
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
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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

void utm_test_14()
{
    static char * opts [] =
	{ "--projection", "utm",
	  "1", "2", "3", "4", "5" };

    p = opts; l = 7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

    if (pps && pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	l == 5 && strcmp(p[0], "1") == 0 &&
	strcmp(p[1], "2") == 0 && strcmp(p[2], "3") == 0 &&
	strcmp(p[3], "4") == 0 && strcmp(p[4], "5") == 0 &&
	logflag == 0 && quietflag == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_14\n");
	print_args(l, p);
	++nfail;
    }
}

void utm_test_15()
{
    static char * opts [] =
	{ "--projection", "utm",
	  "1", "2", "3", "4", "-quiet", "5" };

    p = opts; l = 8;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

    if (pps && pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	l == 5 && strcmp(p[0], "1") == 0 &&
	strcmp(p[1], "2") == 0 && strcmp(p[2], "3") == 0 &&
	strcmp(p[3], "4") == 0 && strcmp(p[4], "5") == 0 &&
	logflag == 0 && quietflag == 1)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_15\n");
	print_args(l, p);
	++nfail;
    }
}

void utm_test_16()
{
    static char * opts [] =
	{ "--projection", "utm",
	  "1", "2", "3", "-log", "logfile", "4", "5" };

    p = opts; l = 9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

    if (pps && pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	l == 5 && strcmp(p[0], "1") == 0 &&
	strcmp(p[1], "2") == 0 && strcmp(p[2], "3") == 0 &&
	strcmp(p[3], "4") == 0 && strcmp(p[4], "5") == 0 &&
	logflag == 1 && quietflag == 0 && strcmp(logFile, "logfile") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_16\n");
	print_args(l, p);
	++nfail;
    }
}

void utm_test_17()
{
    static char * opts [] =
	{ "--projection", "utm",
	  "1", "-quiet", "2", "3", "-log", "log.file", "4", "5" };

    p = opts; l = 10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

    if (pps && pt == UNIVERSAL_TRANSVERSE_MERCATOR &&
	l == 5 && strcmp(p[0], "1") == 0 &&
	strcmp(p[1], "2") == 0 && strcmp(p[2], "3") == 0 &&
	strcmp(p[3], "4") == 0 && strcmp(p[4], "5") == 0 &&
	logflag == 1 && quietflag == 1 && strcmp(logFile, "log.file") == 0)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: utm_test_17\n");
	print_args(l, p);
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
    utm_test_14();
    utm_test_15();
    utm_test_16();
    utm_test_17();
}

void ps_test_1()
{
    static char * opts [] =
	{ "--projection", "ps", "--first-standard-parallel", "61", "--central-meridian", "-59" };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "ps", "--central-meridian", "59", "--first-standard-parallel", "-61", "f1", "f2"  };

    p = opts; l =  8;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "ps", "--central-meridian", "59", "--first-standard-parallel", "-61"  };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "ps", "--central-meridian", "0", "--first-standard-parallel", "61"  };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "ps", "--central-meridian", "59", "--first-standard-parallel", "-61"  };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "ps", "--central-meridian", "123zap", "--first-standard-parallel", "-61"  };

    p = opts; l =  6;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "ps", "--central-meridian", "59", "--first-standard-parallel", "-61", "-n"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "ps", "--central-meridian", "59", "--first-standard-parallel", "-61", "-s"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

    if (pps->ps.slat == -61 && pps->ps.slon == 59 && 
	pps->ps.is_north_pole == 0 && pt == POLAR_STEREOGRAPHIC)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	print_args(l, p);
	printf("Fail: ps_test_8\n");
	++nfail;
    }
}

void ps_test_9()
{
    static char * opts [] =
	{ "-p", "ps", "--central-meridian", "159",
	  "--first-standard-parallel", "61", "--south-pole"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

    if (pps->ps.slat == 61 && pps->ps.slon == 159 && 
	pps->ps.is_north_pole == 0 && pt == POLAR_STEREOGRAPHIC)
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
	{ "--projection", "ps", "--first-standard-parallel", "19", "--central-meridian", "-101",
	  "--north-pole"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "ps", "--first-standard-parallel", "19", "--central-meridian", "-101",
	  "--south-pole"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

    if (pps->ps.slon == -101 && pps->ps.slat == 19 && 
	pps->ps.is_north_pole == 0 && pt == POLAR_STEREOGRAPHIC)
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
	{ "--projection", "ps", "--first-standard-parallel", "19",
	  "--central-meridian", "-101",
	  "--south-pole"  };

    p = opts; l =  7;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

    if (pps->ps.slon == -101 && pps->ps.slat == 19 && 
	pps->ps.is_north_pole == 0 && pt == POLAR_STEREOGRAPHIC)
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
	{ "-p", "ps", "--central-meridian", "-101", "-s"  };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size,
						     &datum, &rm);

    if (pps->ps.slon == -101 && ISNAN(pps->ps.slat) && 
	pps->ps.is_north_pole == 0 && pt == POLAR_STEREOGRAPHIC &&
	ISNAN(height))
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
	{ "-p", "ps", "--central-meridian", "-101", "-n"  };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height,
						     &pixel_size, &datum, &rm);

    if (pps->ps.slon == -101 && ISNAN(pps->ps.slat) && 
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
	{ "-p", "ps", "--first-standard-parallel", "-11", "-n"  };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "ps", "--first-standard-parallel", "-n"  };

    p = opts; l =  4;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "ps", "--central-meridian", "-n"  };

    p = opts; l =  4;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "ps", "--first-standard-parallel", "7", "--central-meridian"  };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "ps", "--central-meridian", "7", "--first-standard-parallel"  };

    p = opts; l =  5;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "ps", "--first-standard-parallel", "-81", "-n",
	  "--false-northing", "5.5", "--false-easting", "8.1"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "ps", "--first-standard-parallel", "-11.101", "--north-pole",
	  "--central-meridian", "60",
	  "--false-northing", "-55000000", "--false-easting", "81000000"  };

    p = opts; l =  11;

    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	  "--false-northing", "-55000000", "--false-easting", "81000000",
	  "--first-standard-parallel", "11.101", "--central-meridian", "60"  };

    p = opts; l =  11;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	  "--false-easting", "81000000",
	  "--first-standard-parallel", "51.101",
	  "--central-meridian", "60"  };

    p = opts; l = 10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height,
						     &pixel_size, &datum, &rm);

    if (within_tol(pps->ps.slat, 51.101) &&
	within_tol(pps->ps.slon, 60) && 
	pps->ps.is_north_pole == 1 &&
	within_tol(pps->ps.false_easting, 81000000) && 
	ISNAN(pps->ps.false_northing) &&
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
	  "--false-northing", "81000000",
	  "--first-standard-parallel", "1.101", "--central-meridian", "60",
	  "phat"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height,
						     &pixel_size, &datum, &rm);

    if (within_tol(pps->ps.slat, 1.101) &&
	within_tol(pps->ps.slon, 60) && 
	pps->ps.is_north_pole == 1 &&
	within_tol(pps->ps.false_northing, 81000000) && 
	ISNAN(pps->ps.false_easting) && pt == POLAR_STEREOGRAPHIC &&
	l == 1 && strcmp(p[0], "phat") == 0 &&
	ISNAN(pixel_size) && ISNAN(height))
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
	{ "--projection", "ps", "--first-standard-parallel", "11fff9", "--central-meridian", "-101",
	  "--south-pole", "more", "stuff"  };

    p = opts; l = 9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "17",
	  "--second-standard-parallel", "18",
	  "--center-latitude", "45", "--central-meridian", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "lamcc", "--first-standard-parallel", "17",
	  "--second-standard-parallel", "18",
	  "--center-latitude", "45", "--central-meridian", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "17", "--second-standard-parallel", "18",
	  "--center-latitude", "45", "--central-meridian", "0.03"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "17", "--second-standard-parallel", "18",
	  "--center-latitude", "45.8", "--central-meridian", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "17",
	  "--second-standard-parallel", "18",
	  "--center-latitude", "45", "--central-meridian", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "17z", "--second-standard-parallel", "18",
	  "--center-latitude", "45", "--central-meridian", "0"  };

    p = opts; l = 10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "17", "--second-standard-parallel", "1o8",
	  "--center-latitude", "45", "--central-meridian", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "17", "--second-standard-parallel", "18",
	  "--center-latitude", "4r5", "--central-meridian", "0"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "17", "--second-standard-parallel", "18",
	  "--center-latitude", "45", "--central-meridian", "lookyhere!"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "--second-standard-parallel", "18",
	  "--center-latitude", "45", "--central-meridian", "0"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "17", "--second-standard-parallel",
	  "--center-latitude", "45", "--central-meridian", "0"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "17", "--second-standard-parallel", "0",
	  "--center-latitude", "45", "--central-meridian"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--first-standard-parallel", "17", "--second-standard-parallel", "0",
	  "--central-meridian", "45", "--center-latitude"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--center-latitude", "17", "--second-standard-parallel", "0",
	  "--central-meridian", "45", "--first-standard-parallel"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--center-latitude", "17", "--central-meridian", "45",
	  "--first-standard-parallel", "0", "--second-standard-parallel"  };

    p = opts; l =  9;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--center-latitude", "17", "--central-meridian", "45",
	  "--first-standard-parallel", "0"  };

    p = opts; l =  8;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--second-standard-parallel", "17.3", "--central-meridian", "45",
	  "--first-standard-parallel", "0"  };

    p = opts; l =  8;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--center-latitude", "17", "--central-meridian", "45",
	  "--second-standard-parallel", "0"  };

    p = opts; l =  8;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "--projection", "lamcc", "--center-latitude", "17", "--second-standard-parallel", "45",
	  "--first-standard-parallel", "0"  };

    p = opts; l =  8;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "lamcc", "--center-latitude", "17.778", "--second-standard-parallel", "45.001",
	  "--first-standard-parallel", "-1.123", "--central-meridian", "-111.111"  };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "lamcc", "--center-latitude", "17.778", "--second-standard-parallel", "45.001",
	  "--first-standard-parallel", "-1.123", "--central-meridian", "-111.111", "--false-easting", "1",
	  "--false-northing", "7888" };

    p = opts; l =  14;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "lamcc", "--center-latitude", "17.778", "--second-standard-parallel", "45.001",
	  "--first-standard-parallel", "-1.123", "--central-meridian", "-111.111", "--false-northing", "1",
	  "--false-northing", "7888" };

    p = opts; l =  14;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	{ "-p", "lamcc", "--center-latitude", "17.778",
	  "--second-standard-parallel", "45.001",
	  "--first-standard-parallel", "-1.123",
	  "--central-meridian", "-111.111",
	  "--false-easting", "7888",
	  "--datum", "WGS84" };

    p = opts; l =  14;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height,
						     &pixel_size, &datum, &rm);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778) &&
	within_tol(pps->lamcc.false_easting, 7888) &&
	ISNAN(pps->lamcc.false_northing) &&
	ISNAN(height) &&
	ISNAN(pixel_size) &&
	datum == WGS84_DATUM)
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
	{ "-p", "lamcc", "--center-latitude", "17.778",
	  "--second-standard-parallel", "45.001",
	  "--first-standard-parallel", "-1.123",
	  "--central-meridian", "-111.111",
	  "--false-northing", "-7888",
	  "--datum", "nad27", 
	  "--height", "466" };

    p = opts; l =  16;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height,
						     &pixel_size, &datum, &rm);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778) &&
	within_tol(pps->lamcc.false_northing, -7888) &&
	ISNAN(pps->lamcc.false_easting) &&
	within_tol(height, 466) &&
	ISNAN(pixel_size) &&
	datum == NAD27_DATUM)
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
	{ "-p", "lamcc", "--center-latitude", "17.778",
	  "--second-standard-parallel", "45.001",
	  "--first-standard-parallel", "-1.123",
	  "--central-meridian", "-111.111", "--false-easting", "1",
	  "--false-northing", "7888", "--scale-factor", "1.1" };

    p = opts; l =  16;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	printf("Fail: lamcc_test_26\n");
	++nfail;
    }    
}

void lamcc_test_27()
{
    static char * opts [] =
	{ "-p", "lamcc", "--center-latitude", "17.778",
	  "--second-standard-parallel", "45.001",
	  "--first-standard-parallel", "-1.123",
	  "--central-meridian", "-111.111", "--false-easting", "1",
	  "--false-northing", "7888", };

    p = opts; l =  14;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	printf("Fail: lamcc_test_27\n");
	++nfail;
    }    
}

void lamcc_test_28()
{
    static char * opts [] =
	{ "-p", "lamcc", "--center-latitude", "17.778",
	  "--second-standard-parallel", "45.001",
	  "--first-standard-parallel", "-1.123",
	  "--central-meridian", "-111.111", "--false-northing", "21",
	  "--false-easting", "7888", "final" };

    p = opts; l =  15;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

    if (within_tol(pps->lamcc.plat1, -1.123) &&
	within_tol(pps->lamcc.lon0, -111.111) &&
	within_tol(pps->lamcc.plat2, 45.001) &&
	within_tol(pps->lamcc.lat0, 17.778) &&
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
	{ "-p", "lamcc", "--center-latitude", "17.778",
	  "--second-standard-parallel", "45.001",
	  "--first-standard-parallel", "-1.123",
	  "--central-meridian", "-111.111", "--false-easting", "1",
	  "--false-northing", "7888" };

    p = opts; l =  14;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	printf("Fail: lamcc_test_29\n");
	++nfail;
    }    
}

void lamcc_test_30()
{
    static char * opts [] =
	{ "-p", "lamcc", "--second-standard-parallel", "45.001",
	  "--false-northing", "7888", "--central-meridian", "-111.111",
	  "--false-easting", "1",
	  "--first-standard-parallel", "-1.123",
	  "--center-latitude", "17.778"  };

    p = opts; l =  14;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	{ "-p", "lamaz", "--center-latitude", "17.778",
	  "--central-meridian", "45.001",
	  "--false-northing", "7888", "--false-easting", "110000" };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "lamaz", "--center-latitude", "17.778",
	  "--central-meridian", "45.001",
	  "--false-northing", "7888", "--false-easting", "110000" };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "lamaz", "--center-latitude", "17.778",
	  "--central-meridian", "45.001",
	  "--false-northing", "7888", "--false-easting", "110000" };

    p = opts; l =  10;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	{ "-p", "lamaz", "--center-latitude", "17.778",
	  "--central-meridian", "45.001",
	  "--false-northing", "7888", "--false-easting", "110000",
	  "additional_arg" };

    p = opts; l =  11;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	  "-p", "lamaz", "--center-latitude", "17.778", "--central-meridian", "45.001",
	  "--false-northing", "7888", "--false-easting", "110000", "dude" };

    p = opts; l =  12;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

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
	  "--center-latitude", "17.778", "--central-meridian", "45.001",
	  "stuff", "at", "the", "end" };

    p = opts; l =  14;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height,
						     &pixel_size, &datum, &rm);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	ISNAN(pps->lamaz.false_easting) &&
	ISNAN(pps->lamaz.false_northing) &&
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
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height, &pixel_size, &datum, &rm);

    if (ISNAN(pps->lamaz.center_lon) &&
	ISNAN(pps->lamaz.center_lat) &&
	ISNAN(pps->lamaz.false_easting) &&
	ISNAN(pps->lamaz.false_northing) &&
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

void lamaz_test_8()
{
    static char * opts [] =
	{ "-p", "lamaz", "--center-latitude", "17.778",
	  "--central-meridian", "45.001",
	  "--false-northing", "7888", "--false-easting", "110000",
	  "-log", "logfilename" };

    p = opts; l = 12;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 110000) &&
	within_tol(pps->lamaz.false_northing, 7888) &&
	strcmp(logFile, "logfilename") == 0 &&
	logflag == 1)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamaz_test_8: %s\n", logFile);
	++nfail;
    }    
}

void lamaz_test_9()
{
    static char * opts [] =
	{ "-p", "lamaz", "--center-latitude", "17.778",
	  "--central-meridian", "45.001",
	  "--false-northing", "7888", "--false-easting", "110000",
	  "-log", "tmp123.log" };

    p = opts; l = 12;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 110000) &&
	within_tol(pps->lamaz.false_northing, 7888) &&
	strcmp(logFile, "tmp123.log") == 0 &&
	logflag == 1)
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: lamaz_test_9: %s\n", logFile);
	++nfail;
    }    
}

void lamaz_test_10()
{
    static char * opts [] =
	{ "-p", "lamaz", "--center-latitude", "17.778",
	  "--central-meridian", "45.001",
	  "--false-northing", "7888", "--false-easting", "110000",
	  "-log" };

    p = opts; l = 11;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 110000) &&
	within_tol(pps->lamaz.false_northing, 7888))
    {
	++nok;
    }
    else
    {
	printf("Fail: lamaz_test_10\n");
	++nfail;
    }    
}

void lamaz_test_11()
{
    static char * opts [] =
	{ "-p", "lamaz", "--center-latitude", "17.778",
	  "--central-meridian", "45.001",
	  "--false-northing", "7888", "--false-easting", "110000",
	  "-quiet" };

    p = opts; l = 11;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 110000) &&
	within_tol(pps->lamaz.false_northing, 7888) &&
	quietflag == 1)
    {
	++nok;
    }
    else
    {
	printf("Fail: lamaz_test_11\n");
	++nfail;
    }    
}

void lamaz_test_12()
{
    static char * opts [] =
	{ "-log" ,"mylogfile", "-p", "lamaz", "--center-latitude", "17.778",
	  "--central-meridian", "45.001",
	  "--false-northing", "7888", "--false-easting", "110000" };

    p = opts; l = 12;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 110000) &&
	within_tol(pps->lamaz.false_northing, 7888) &&
	logflag == 1 && strcmp(logFile, "mylogfile") == 0 &&
	quietflag == 0)
    {
	++nok;
    }
    else
    {
	printf("Fail: lamaz_test_12 %s %d\n", logFile, logflag);
	print_args(l,p);
	++nfail;
    }    
}

void lamaz_test_13()
{
    static char * opts [] =
	{ "-log" ,"mylogfile", "-quiet",
	  "-p", "lamaz", "--center-latitude", "17.778",
	  "--central-meridian", "45.001",
	  "--false-northing", "7888", "--false-easting", "110000" };

    p = opts; l = 13;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

    if (within_tol(pps->lamaz.center_lon, 45.001) &&
	within_tol(pps->lamaz.center_lat, 17.778) &&
	within_tol(pps->lamaz.false_easting, 110000) &&
	within_tol(pps->lamaz.false_northing, 7888) &&
	logflag == 1 && strcmp(logFile, "mylogfile") == 0 &&
	quietflag == 1)
    {
	++nok;
    }
    else
    {
	printf("Fail: lamaz_test_13 %s %d %d\n", logFile, logflag, quietflag);
	print_args(l,p);
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
    lamaz_test_8();
    lamaz_test_9();
    lamaz_test_10();
    lamaz_test_11();
    lamaz_test_12();
    lamaz_test_13();
}

void albers_test_1()
{
    static char * opts [] =
	{ "-p", "albers"  };

    p = opts; l =  2;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt, &height,
						     &pixel_size, &datum, &rm);

    if (ISNAN(pps->albers.center_meridian) &&
	ISNAN(pps->albers.orig_latitude) &&
	ISNAN(pps->albers.false_easting) &&
	ISNAN(pps->albers.false_northing) &&
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
	{ "--quiet", "-p", "albers", "--second-standard-parallel", "45.001",
	  "--false-northing", "7888", "--central-meridian", "-111.111", "--false-easting", "1",
	   "--first-standard-parallel", "-1.123", "--center-latitude", "17.778", "f1", "f2"  };

    if (noisy) printf("albers_test_2\n");

    p = opts; l =  17;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	{ "--quiet", "--projection", "albers", "--false-easting", "1",
	  "--false-northing", "7888",
	  "--second-standard-parallel", "-45.001",
	  "--central-meridian", "-111.111", 
	  "--center-latitude", "+17.778",
	  "--first-standard-parallel", "-1.123", "foo", "bar"  };

    if (noisy) printf("albers_test_3\n");

    p = opts; l =  17;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	{ "--quiet", "--projection", "albers", "--false-easting", "1",
	  "--false-northing", "7888", "--second-standard-parallel", "-45.001",
	  "--central-meridian", "-111.111", "--center-latitude", "+17.778",
	  "--first-standard-parallel", "-1.123", "foo", "bar", "baz"  };

    if (noisy) printf("albers_test_4\n");

    p = opts; l =  18;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	{ "--quiet", "--projection", "albers", "--false-easting", "1",
	  "--false-northing", "7888", "--second-standard-parallel", "45.001",
	  "--central-meridian", "111.111", "--center-latitude", "+17.778",
	  "--first-standard-parallel", "-11.123", "--height", "14",
	  "foo", "bar", "baz"  };

    if (noisy) printf("albers_test_5\n");

    p = opts; l = 20;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	{ "--quiet", "--height", "14.9", "--projection", "albers", "--false-easting", "1",
	  "--false-northing", "7888", "--second-standard-parallel", "45.001",
	  "--central-meridian", "111.111", "--center-latitude", "+17.778",
	  "--first-standard-parallel", "-11.123",
	  "foo", "bar", "baz"  };

    if (noisy) printf("albers_test_6\n");

    p = opts; l = 20;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	  "--pixel-size", "100",
	  "--projection", "albers", "--false-easting", "1",
	  "--false-northing", "7888", "--second-standard-parallel", "45.001",
	  "--central-meridian", "111.111", "--center-latitude", "+17.778",
	  "--first-standard-parallel", "-11.123",
	  "foo", "bar", "baz"  };

    if (noisy) printf("albers_test_7\n");

    p = opts; l = 24;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	  "--projection", "albers", "--false-easting", "1",
	  "--false-northing", "7888", "--second-standard-parallel", "45.001",
	  "--central-meridian", "111.111", "--center-latitude", "+17.778",
	  "--first-standard-parallel", "-11.123",
	  "--height", "14.9", "--write-proj-file", "bork",
	  "--pixel-size", "100", "foo", "bar", "baz"  };

    if (noisy) printf("albers_test_8\n");

    p = opts; l = 24;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	  "--projection", "albers", "--false-easting", "1",
	  "--false-northing", "7888", "--second-standard-parallel", "45.001",
	  "--central-meridian", "111.111", "--center-latitude", "+17.778",
	  "--first-standard-parallel", "-11.123",
	  "--pixel-size", "10.01", "foo", 
	  "--write-proj-file", "bork",
	  "bar", "baz"  };

    if (noisy) printf("albers_test_9\n");

    p = opts; l = 25;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	  "--projection", "albers", "--false-easting", "1",
	  "--false-northing", "7888", "--second-standard-parallel", "45.001",
	  "--central-meridian", "111.111", "--center-latitude", "+17.7878",
	  "--write-proj-file", "bork",
	  "--first-standard-parallel", "-11.123",
	  "--pixel-size", "10.01", "foo", 
	  "bar", "baz", "bork" };

    if (noisy) printf("albers_test_10\n");

    p = opts; l = 26;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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
	  "--projection", "albers", "--false-easting", "1000000",
	  "--false-northing", "78880000", 
	  "--second-standard-parallel", "45.001",
	  "--central-meridian", "111.111", "--center-latitude", "7.78078",
	  "--write-proj-file", "bork", 
	  "--first-standard-parallel", "-11.123",
	  "-ps", "1001.1001", "foo", "-h", "1499.009", 
	  "bar", "baz", "bork" };

    if (noisy) printf("albers_test_11\n");

    p = opts; l = 26;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

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

void albers_test_12()
{
    static char * opts [] =
	{ "--quiet", "dude",
	  "--projection", "albers", "--false-easting", "1000000",
	  "--false-northing", "78880000", 
	  "--second-standard-parallel", "45.0101",
	  "--central-meridian", "111.111", "--center-latitude", "77.78078",
	  "--write-proj-file", "bork2", 
	  "--first-standard-parallel", "-11.123",
	  "-ps", "1001.1001", "foo", "-h", "1499.009", 
	  "bar", "baz", "bork", "a", "b", "c" };

    if (noisy) printf("albers_test_12\n");

    p = opts; l = 29;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size, &datum, &rm);

    if (within_tol(pps->albers.std_parallel1, -11.123) &&
	within_tol(pps->albers.center_meridian, 111.111) &&
	within_tol(pps->albers.std_parallel2, 45.0101) &&
	within_tol(pps->albers.orig_latitude, 77.78078) &&
	within_tol(pps->albers.false_easting, 1000000) &&
	within_tol(pps->albers.false_northing, 78880000) &&
	l == 9 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0 &&
	strcmp(p[1], "dude") == 0 &&
	strcmp(p[2], "foo") == 0 &&
	strcmp(p[3], "bar") == 0 &&
	strcmp(p[4], "baz") == 0 &&
	strcmp(p[5], "bork") == 0 &&
	strcmp(p[6], "a") == 0 &&
	strcmp(p[7], "b") == 0 &&
	strcmp(p[8], "c") == 0 &&
	within_tol(height, 1499.009) &&
	within_tol(pixel_size, 1001.1001))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_12 %f %d\n", height, l);
	print_args(l,p);
	++nfail;
    }    
}

void albers_test_13()
{
    static char * opts [] =
	{ "--quiet", "dude", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
	  "--projection", "albers", "--false-easting", "1000000",
	  "--false-northing", "78880000", 
	  "--second-standard-parallel", "45.0101",
	  "--central-meridian", "111.111", "--center-latitude", "77.78078",
	  "--write-proj-file", "bork", 
	  "--first-standard-parallel", "-11.123", "aaa", "bbb",
	  "-ps", "1001.1001", "foo", "-h", "1499.009", 
	  "bar", "baz", "bork", "k", "l", "m", "n", "o", "p", "q", "r" };

    if (noisy) printf("albers_test_13\n");

    p = opts; l = 46;
    project_parameters_t * pps = get_geocode_options(&l, &p, &pt,
						     &height, &pixel_size,
						     &datum, &rm);

    if (within_tol(pps->albers.std_parallel1, -11.123) &&
	within_tol(pps->albers.center_meridian, 111.111) &&
	within_tol(pps->albers.std_parallel2, 45.0101) &&
	within_tol(pps->albers.orig_latitude, 77.78078) &&
	within_tol(pps->albers.false_easting, 1000000) &&
	within_tol(pps->albers.false_northing, 78880000) &&
	l == 26 &&
	pt == ALBERS_EQUAL_AREA &&
	strcmp(p[0], "--quiet") == 0 &&
	strcmp(p[1], "dude") == 0 &&
	strcmp(p[2], "a") == 0 &&
	strcmp(p[3], "b") == 0 &&
	strcmp(p[4], "c") == 0 &&
	strcmp(p[5], "d") == 0 &&
	strcmp(p[6], "e") == 0 &&
	strcmp(p[7], "f") == 0 &&
	strcmp(p[8], "g") == 0 &&
	strcmp(p[9], "h") == 0 &&
	strcmp(p[10], "i") == 0 &&
	strcmp(p[11], "j") == 0 &&
	strcmp(p[12], "aaa") == 0 &&
	strcmp(p[13], "bbb") == 0 &&
	strcmp(p[14], "foo") == 0 &&
	strcmp(p[15], "bar") == 0 &&
	strcmp(p[16], "baz") == 0 &&
	strcmp(p[17], "bork") == 0 &&
	strcmp(p[18], "k") == 0 &&
	strcmp(p[19], "l") == 0 &&
	strcmp(p[20], "m") == 0 &&
	strcmp(p[21], "n") == 0 &&
	strcmp(p[22], "o") == 0 &&
	strcmp(p[23], "p") == 0 &&
	strcmp(p[24], "q") == 0 &&
	strcmp(p[25], "r") == 0 &&
	within_tol(height, 1499.009) &&
	within_tol(pixel_size, 1001.1001))
    {
	++nok;
	test_file(pps, pt);
    }
    else
    {
	printf("Fail: albers_test_13 %f %d\n", height, l);
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
    albers_test_12();
    albers_test_13();
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
