#include "geocode_options.h"
#include "asf.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "asf_reporting.h"

#include <stdlib.h>
#include <string.h>

static int print_warn = 1;

static int parse_double(const char * str, double * val)
{
    char *p;
    *val = strtod(str, &p);
    
    if (*str == '\0' || *p != '\0')
    {
	return FALSE;
    }
    
    return TRUE;
}

static int parse_int(const char * str, int * val)
{
    char *p;
    *val = (int) strtol(str, &p, 10);
    
    if (*str == '\0' || *p != '\0')
    {
	return FALSE;
    }
    
    return TRUE;
}

static void no_arg(char * option)
{
    if (print_warn)
	asfPrintWarning("No argument specified for option: %s\n", option);
}

static void bad_arg(char * option, char * arg)
{
    if (print_warn)
	asfPrintWarning("Bad argument specified for option %s: %s\n", 
			option, arg);
}

static void pole_arg(void)
{
    if (print_warn)
	asfPrintWarning("Pole specified twice.\n"); 
}

static void double_arg(const char * option)
{
    if (print_warn)
	asfPrintWarning("Option occurs twice: %s\n", option); 
}

static void missing_arg(const char * option)
{
    if (print_warn)
	asfPrintWarning(
	    "Projection requires option that was not specified: %s\n", option);
}

void set_options_testing(int is_testing)
{
    print_warn = !is_testing;
}

project_parameters_t * get_geocode_options(int *argc, char **argv[],
					   projection_type_t * proj_type)
{
    int i, j;
    project_parameters_t *pps = malloc(sizeof(project_parameters_t));
    int start_arg = 0, end_arg = 0;

    for (i = 0; i < *argc; ++i)
    {
	if (strcmp((*argv)[i], "--projection") == 0 ||
	    strcmp((*argv)[i], "-p") == 0)
	{
	    start_arg = i;
	    ++i;

	    if (i == *argc)
	    {
		no_arg((*argv)[i-1]);
		return NULL;
	    }

	    if (strcmp((*argv)[i], "utm") == 0)
	    {
		int specified_zone = 0;
		int specified_lat0 = 0;
		int specified_lon0 = 0;

		*proj_type = UNIVERSAL_TRANSVERSE_MERCATOR;

		while (1)
		{
		    ++i;

		    if (i == *argc)
			break;

		    if (strcmp((*argv)[i], "--zone") == 0 || 
			strcmp((*argv)[i], "-z") == 0)
		    {
			int val;
			++i;
			
			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_zone)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_zone = 1;

			if (parse_int((*argv)[i], &val))
			{
			    pps->utm.zone = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--lat0") == 0 ||
			     strcmp((*argv)[i], "-lat0") == 0 ||
			     strcmp((*argv)[i], "--center_latitude") == 0 ||
			     strcmp((*argv)[i], "--slat") == 0 ||
			     strcmp((*argv)[i], "-slat") == 0 ||
			     strcmp((*argv)[i], "--lat_0") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_lat0)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_lat0 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->utm.lat0 = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--lon0") == 0 ||
			     strcmp((*argv)[i], "-lon0") == 0 ||
			     strcmp((*argv)[i], "--center_longitude") == 0 ||
			     strcmp((*argv)[i], "--slon") == 0 ||
			     strcmp((*argv)[i], "-slon") == 0 ||
			     strcmp((*argv)[i], "--lon_0") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_lon0)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_lon0 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->utm.lon0 = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else
		    {
			break;
		    }
		}

		if (!specified_lon0)
		{
		    pps->utm.lon0 = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_lat0)
		{
		    pps->utm.lat0 = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_zone)
		{
		    pps->utm.zone = MAGIC_UNSET_INT;
		}

		break;
	    }
	    else if (strcmp((*argv)[i], "ps") == 0)
	    {
		int specified_slat = 0;
		int specified_slon = 0;
		int specified_pole = 0;
		int specified_false_northing = 0;
		int specified_false_easting = 0;

		pps->ps.is_north_pole = 1;
		*proj_type = POLAR_STEREOGRAPHIC;

		while (1)
		{
		    ++i;

		    if (i == *argc)
			break;

		    if (strcmp((*argv)[i], "-slat") == 0 ||
			strcmp((*argv)[i], "--slat") == 0 ||
			strcmp((*argv)[i], "--center_latitude") == 0 ||
			strcmp((*argv)[i], "--lat_ts") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_slat)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_slat = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->ps.slat = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "-slon") == 0 ||
			strcmp((*argv)[i], "--slon") == 0 ||
			strcmp((*argv)[i], "--center_longitude") == 0 ||
			strcmp((*argv)[i], "--lon_0") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_slon)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_slon = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->ps.slon = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--false_easting") == 0 ||
			     strcmp((*argv)[i], "-fe") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_false_easting)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_false_easting = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->ps.false_easting = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--false_northing") == 0 ||
			     strcmp((*argv)[i], "-fn") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_false_northing)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_false_northing = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->ps.false_northing = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--north_pole") == 0 ||
			strcmp((*argv)[i], "-n") == 0)
		    {

			if (specified_pole)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_pole = 1;

			pps->ps.is_north_pole = 1;
		    }


		    else if (strcmp((*argv)[i], "--south_pole") == 0 ||
			strcmp((*argv)[i], "-s") == 0)
		    {
			if (specified_pole)
			{
			    pole_arg();
			    return NULL;
			}

			specified_pole = 1;

			pps->ps.is_north_pole = -1;
		    }
		    else
		    {
			break;
		    }
		}

		if (!specified_slat)
		{
		    if (pps->ps.is_north_pole == 1)
			pps->ps.slat = 90;
		    else
			pps->ps.slat = -90;
		}

		if (!specified_slon)
		{
		    pps->ps.slon = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_false_easting)
		{
		    pps->ps.false_easting = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_false_northing)
		{
		    pps->ps.false_northing = MAGIC_UNSET_DOUBLE;
		}

		break;
	    }
	    else if (strcmp((*argv)[i], "lamcc") == 0)
	    {
		int specified_plat1 = 0;
		int specified_plat2 = 0;
		int specified_lat0 = 0;
		int specified_lon0 = 0;
		int specified_false_easting = 0;
		int specified_false_northing = 0;
		int specified_scale_factor = 0;
		*proj_type = LAMBERT_CONFORMAL_CONIC;

		while (1)
		{
		    ++i;
		    if (i == *argc)
			break;

		    if (strcmp((*argv)[i], "--plat1") == 0 ||
			strcmp((*argv)[i], "--lat_1") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_plat1)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_plat1 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->lamcc.plat1 = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--plat2") == 0 ||
			     strcmp((*argv)[i], "--lat_2") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_plat2)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_plat2 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->lamcc.plat2 = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--lat0") == 0 ||
			     strcmp((*argv)[i], "-lat0") == 0 ||
			     strcmp((*argv)[i], "--center_latitude") == 0 ||
			     strcmp((*argv)[i], "--slat") == 0 ||
			     strcmp((*argv)[i], "-slat") == 0 ||
			     strcmp((*argv)[i], "--lat_0") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_lat0)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_lat0 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->lamcc.lat0 = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--lon0") == 0 ||
			     strcmp((*argv)[i], "-lon0") == 0 ||
			     strcmp((*argv)[i], "--center_longitude") == 0 ||
			     strcmp((*argv)[i], "--slon") == 0 ||
			     strcmp((*argv)[i], "-slon") == 0 ||
			     strcmp((*argv)[i], "--lon_0") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_lon0)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_lon0 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->lamcc.lon0 = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--false_easting") == 0 ||
			     strcmp((*argv)[i], "-fe") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_false_easting)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_false_easting = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->lamcc.false_easting = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--false_northing") == 0 ||
			     strcmp((*argv)[i], "-fn") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_false_northing)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_false_northing = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->lamcc.false_northing = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--scale_factor") == 0 ||
			     strcmp((*argv)[i], "-sf") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_scale_factor)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_scale_factor = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->lamcc.scale_factor = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else
		    {
			break;
		    }
		}

		if (!specified_plat1)
		{
		    pps->lamcc.plat1 = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_plat2)
		{
		    pps->lamcc.plat2 = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_lat0)
		{
		    pps->lamcc.lat0 = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_lon0)
		{
		    pps->lamcc.lon0 = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_false_easting)
		{
		    pps->lamcc.false_easting = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_false_northing)
		{
		    pps->lamcc.false_northing = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_scale_factor)
		{
		    pps->lamcc.scale_factor = MAGIC_UNSET_DOUBLE;
		}

		break;
	    }
	    else if (strcmp((*argv)[i], "lamaz") == 0)
	    {
		int specified_lat0 = 0;
		int specified_lon0 = 0;
		int specified_false_easting = 0;
		int specified_false_northing = 0;

		*proj_type = LAMBERT_AZIMUTHAL_EQUAL_AREA;

		while (1)
		{
		    ++i;
		    if (i == *argc)
			break;

		    else if (strcmp((*argv)[i], "--lat0") == 0 ||
			     strcmp((*argv)[i], "-lat0") == 0 ||
			     strcmp((*argv)[i], "--center_latitude") == 0 ||
			     strcmp((*argv)[i], "--slat") == 0 ||
			     strcmp((*argv)[i], "-slat") == 0 ||
			     strcmp((*argv)[i], "--lat_0") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_lat0)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_lat0 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->lamaz.center_lat = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--lon0") == 0 ||
			     strcmp((*argv)[i], "-lon0") == 0 ||
			     strcmp((*argv)[i], "--center_longitude") == 0 ||
			     strcmp((*argv)[i], "--slon") == 0 ||
			     strcmp((*argv)[i], "-slon") == 0 ||
			     strcmp((*argv)[i], "--lon_0") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_lon0)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_lon0 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->lamaz.center_lon = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--false_easting") == 0 ||
			     strcmp((*argv)[i], "-fe") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_false_easting)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_false_easting = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->lamaz.false_easting = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--false_northing") == 0 ||
			     strcmp((*argv)[i], "-fn") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_false_northing)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_false_northing = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->lamaz.false_northing = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else
		    {
			break;
		    }
		}

		if (!specified_lat0)
		{
		    pps->lamaz.center_lat = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_lon0)
		{
		    pps->lamaz.center_lon = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_false_easting)
		{
		    pps->lamaz.false_easting = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_false_northing)
		{
		    pps->lamaz.false_northing = MAGIC_UNSET_DOUBLE;
		}

		break;
	    }
	    else if (strcmp((*argv)[i], "albers") == 0)
	    {
		int specified_plat1 = 0;
		int specified_plat2 = 0;
		int specified_lat0 = 0;
		int specified_lon0 = 0;
		int specified_false_easting = 0;
		int specified_false_northing = 0;

		*proj_type = ALBERS_EQUAL_AREA;

		while (1)
		{
		    ++i;
		    if (i == *argc)
			break;

		    if (strcmp((*argv)[i], "--plat1") == 0 ||
			strcmp((*argv)[i], "--lat_1") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_plat1)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_plat1 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->albers.std_parallel1 = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--plat2") == 0 ||
			     strcmp((*argv)[i], "--lat_2") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_plat2)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_plat2 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->albers.std_parallel2 = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--lat0") == 0 ||
			     strcmp((*argv)[i], "-lat0") == 0 ||
			     strcmp((*argv)[i], "--center_latitude") == 0 ||
			     strcmp((*argv)[i], "--slat") == 0 ||
			     strcmp((*argv)[i], "-slat") == 0 ||
			     strcmp((*argv)[i], "--lat_0") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_lat0)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_lat0 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->albers.orig_latitude = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--lon0") == 0 ||
			     strcmp((*argv)[i], "-lon0") == 0 ||
			     strcmp((*argv)[i], "--center_longitude") == 0 ||
			     strcmp((*argv)[i], "--slon") == 0 ||
			     strcmp((*argv)[i], "-slon") == 0 ||
			     strcmp((*argv)[i], "--lon_0") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_lon0)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_lon0 = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->albers.center_meridian = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--false_easting") == 0 ||
			     strcmp((*argv)[i], "-fe") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_false_easting)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_false_easting = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->albers.false_easting = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else if (strcmp((*argv)[i], "--false_northing") == 0 ||
			     strcmp((*argv)[i], "-fn") == 0)
		    {
			double val;
			++i;

			if (i == *argc)
			{
			    no_arg((*argv)[i-1]);
			    return NULL;
			}

			if (specified_false_northing)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_false_northing = 1;

			if (parse_double((*argv)[i], &val))
			{
			    pps->albers.false_northing = val;
			}
			else
			{
			    bad_arg((*argv)[i-1], (*argv)[i]);
			    return NULL;
			}
		    }

		    else
		    {
			break;
		    }
		}

		if (!specified_plat1)
		{
		    pps->albers.std_parallel1 = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_plat2)
		{
		    pps->albers.std_parallel2 = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_lat0)
		{
		    pps->albers.orig_latitude = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_lon0)
		{
		    pps->albers.center_meridian = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_false_easting)
		{
		    pps->albers.false_easting = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_false_northing)
		{
		    pps->albers.false_northing = MAGIC_UNSET_DOUBLE;
		}

		break;
	    }
	    else
	    {
		asfPrintWarning("Unknown projection: %s\n", (*argv)[i]);
		return NULL;
	    }
	}
    }

    end_arg = i;

    if (start_arg == end_arg)
    {
	asfPrintWarning("No projection Specified");
	return NULL;
    }
    else
    {
	i = start_arg;
	j = end_arg;

	while (j < *argc && i < end_arg)
	{
	    char * tmp = (*argv)[i];
	    (*argv)[i] = (*argv)[j];
	    (*argv)[j] = tmp;

	    ++i;
	    ++j;
	}

	*argc -= end_arg - start_arg;

	apply_defaults(*proj_type, pps);
	sanity_check(*proj_type, pps);

	return pps;
    }
}

static int calc_utm_zone(double lon)
{
    return((int)(((lon + 180.0) / 6.0) + 1.0));
}

static void verify_valid_latitude(double lat)
{
    if (ISNAN(lat))
	return;

    if (lat > 90 || lat < -90)
    {
	if (print_warn)
	    asfPrintWarning("Invalid Latitude: %f\n", lat);
    }
}

static void verify_valid_longitude(double lon)
{
    if (ISNAN(lon))
	return;

    if (lon > 360 || lon < -360)
    {
	if (print_warn)
	    asfPrintWarning("Invalid Longitude: %f\n", lon);
    }
}

void sanity_check(projection_type_t pt, project_parameters_t * pps)
{
    switch (pt)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:

	    if ((abs(pps->utm.zone) < 1) || (abs(pps->utm.zone) > 60))
	    {
		asfPrintError("Illegal zone number: %d\n", pps->utm.zone);
	    }

	    verify_valid_latitude(pps->utm.lat0);
	    verify_valid_longitude(pps->utm.lon0);

	    break;

	case POLAR_STEREOGRAPHIC:

	    verify_valid_latitude(pps->ps.slat);
	    verify_valid_longitude(pps->ps.slon);

	    break;

	case ALBERS_EQUAL_AREA:

	    verify_valid_latitude(pps->albers.std_parallel1);
	    verify_valid_latitude(pps->albers.std_parallel2);
	    verify_valid_latitude(pps->albers.orig_latitude);
	    verify_valid_longitude(pps->albers.center_meridian);

	    break;

	case LAMBERT_AZIMUTHAL_EQUAL_AREA:

	    verify_valid_latitude(pps->lamaz.center_lat);
	    verify_valid_longitude(pps->lamaz.center_lon);

	    break;

	case LAMBERT_CONFORMAL_CONIC:

	    verify_valid_latitude(pps->lamcc.plat1);
	    verify_valid_latitude(pps->lamcc.plat2);
	    verify_valid_latitude(pps->lamcc.lat0);
	    verify_valid_longitude(pps->lamcc.lon0);

	    break;

	default:
	    asfPrintError("sanity_check: illegal projection type!");
    }
}

void apply_defaults(projection_type_t pt, project_parameters_t * pps)
{
    switch (pt)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:
	    /* set the zone based on the specified longitude */
	    if (!ISNAN(pps->utm.lon0))
	    {
		if (pps->utm.zone == MAGIC_UNSET_INT)
		    pps->utm.zone = calc_utm_zone(pps->utm.lon0);
	    }

	    /* false easting & false northing are fixed for utm */
	    if (!ISNAN(pps->utm.lat0))
	    {
		pps->utm.false_northing = pps->utm.lat0 > 0 ? 0 : 10000000;
		pps->utm.false_easting = 500000;
	    }
	    else
	    {
		pps->utm.false_easting = MAGIC_UNSET_DOUBLE;
		pps->utm.false_northing = MAGIC_UNSET_DOUBLE;
	    }

	    break;

	case POLAR_STEREOGRAPHIC:
	    break;

	case ALBERS_EQUAL_AREA:
	    break;

	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	    break;

	case LAMBERT_CONFORMAL_CONIC:
	    break;

	default:
	    asfPrintError("apply_defaults: illegal projection type!");
    }
}

