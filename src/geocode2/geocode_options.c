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

static void pole_arg()
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

project_parameters_t * get_geocode_options(int argc, char *argv[])
{
    int i;
    project_parameters_t *pps = malloc(sizeof(project_parameters_t));

    for (i = 0; i < argc; ++i)
    {
	if (strcmp(argv[i], "--projection") == 0 || strcmp(argv[i], "-p") == 0)
	{
	    ++i;

	    if (i == argc)
	    {
		no_arg(argv[i-1]);
		return NULL;
	    }

	    if (strcmp(argv[i], "utm") == 0)
	    {
		pps->utm.zone = -1;

		++i;

		if (i < argc &&
		    (strcmp(argv[i], "--zone") == 0 || 
		     strcmp(argv[i], "-z") == 0))
		{
		    double val;
		    ++i;

		    if (parse_double(argv[i], &val))
		    {
			pps->utm.zone = val;
		    }
		    else
		    {
			bad_arg(argv[i-1], argv[i]);
			return NULL;
		    }
		}

		return pps;
	    }
	    else if (strcmp(argv[i], "ps") == 0)
	    {
		int specified_slat = 0;
		int specified_slon = 0;
		int specified_pole = 0;
		int specified_false_northing = 0;
		int specified_false_easting = 0;

		pps->ps.is_north_pole = 1;

		while (1)
		{
		    ++i;

		    if (i == argc)
			break;

		    if (strcmp(argv[i], "-slat") == 0 ||
			strcmp(argv[i], "--slat") == 0 ||
			strcmp(argv[i], "--center_latitude") == 0 ||
			strcmp(argv[i], "--lat_ts") == 0)
		    {
			double val;
			++i;

			if (i == argc)
			{
			    no_arg(argv[i-1]);
			    return NULL;
			}

			if (specified_slat)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_slat = 1;

			if (parse_double(argv[i], &val))
			{
			    pps->ps.slat = val;
			}
			else
			{
			    bad_arg(argv[i-1], argv[i]);
			    return NULL;
			}
		    }

		    else if (strcmp(argv[i], "-slon") == 0 ||
			strcmp(argv[i], "--slon") == 0 ||
			strcmp(argv[i], "--center_longitude") == 0 ||
			strcmp(argv[i], "--lon_0") == 0)
		    {
			double val;
			++i;

			if (i == argc)
			{
			    no_arg(argv[i-1]);
			    return NULL;
			}

			if (specified_slon)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_slon = 1;

			if (parse_double(argv[i], &val))
			{
			    pps->ps.slon = val;
			}
			else
			{
			    bad_arg(argv[i-1], argv[i]);
			    return NULL;
			}
		    }

		    else if (strcmp(argv[i], "--false_easting") == 0 ||
			     strcmp(argv[i], "-fe") == 0)
		    {
			double val;
			++i;

			if (i == argc)
			{
			    no_arg(argv[i-1]);
			    return NULL;
			}

			if (specified_false_easting)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_false_easting = 1;

			if (parse_double(argv[i], &val))
			{
			    pps->ps.false_easting = val;
			}
			else
			{
			    bad_arg(argv[i-1], argv[i]);
			    return NULL;
			}
		    }

		    else if (strcmp(argv[i], "--false_northing") == 0 ||
			     strcmp(argv[i], "-fn") == 0)
		    {
			double val;
			++i;

			if (i == argc)
			{
			    no_arg(argv[i-1]);
			    return NULL;
			}

			if (specified_false_northing)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_false_northing = 1;

			if (parse_double(argv[i], &val))
			{
			    pps->ps.false_northing = val;
			}
			else
			{
			    bad_arg(argv[i-1], argv[i]);
			    return NULL;
			}
		    }

		    else if (strcmp(argv[i], "--north_pole") == 0 ||
			strcmp(argv[i], "-n") == 0)
		    {

			if (specified_pole)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_pole = 1;

			pps->ps.is_north_pole = 1;
		    }


		    else if (strcmp(argv[i], "--south_pole") == 0 ||
			strcmp(argv[i], "-s") == 0)
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
		    pps->ps.slon = 0;
		}

		if (!specified_false_easting)
		{
		    pps->ps.false_easting = MAGIC_UNSET_DOUBLE;
		}

		if (!specified_false_northing)
		{
		    pps->ps.false_northing = MAGIC_UNSET_DOUBLE;
		}

		return pps;
	    }
	    else if (strcmp(argv[i], "lamcc") == 0)
	    {
		int specified_plat1 = 0;
		int specified_plat2 = 0;
		int specified_lat0 = 0;
		int specified_lon0 = 0;
		int specified_false_easting = 0;
		int specified_false_northing = 0;
		int specified_scale_factor = 0;

		while (1)
		{
		    ++i;
		    if (i == argc)
			break;

		    if (strcmp(argv[i], "--plat1") == 0 ||
			strcmp(argv[i], "--lat_1") == 0)
		    {
			double val;
			++i;

			if (i == argc)
			{
			    no_arg(argv[i-1]);
			    return NULL;
			}

			if (specified_plat1)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_plat1 = 1;

			if (parse_double(argv[i], &val))
			{
			    pps->lamcc.plat1 = val;
			}
			else
			{
			    bad_arg(argv[i-1], argv[i]);
			    return NULL;
			}
		    }

		    else if (strcmp(argv[i], "--plat2") == 0 ||
			     strcmp(argv[i], "--lat_2") == 0)
		    {
			double val;
			++i;

			if (i == argc)
			{
			    no_arg(argv[i-1]);
			    return NULL;
			}

			if (specified_plat2)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_plat2 = 1;

			if (parse_double(argv[i], &val))
			{
			    pps->lamcc.plat2 = val;
			}
			else
			{
			    bad_arg(argv[i-1], argv[i]);
			    return NULL;
			}
		    }

		    else if (strcmp(argv[i], "--lat0") == 0 ||
			     strcmp(argv[i], "--center_latitude") == 0 ||
			     strcmp(argv[i], "--slat") == 0 ||
			     strcmp(argv[i], "-slat") == 0 ||
			     strcmp(argv[i], "--lat_0") == 0)
		    {
			double val;
			++i;

			if (i == argc)
			{
			    no_arg(argv[i-1]);
			    return NULL;
			}

			if (specified_lat0)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_lat0 = 1;

			if (parse_double(argv[i], &val))
			{
			    pps->lamcc.lat0 = val;
			}
			else
			{
			    bad_arg(argv[i-1], argv[i]);
			    return NULL;
			}
		    }

		    else if (strcmp(argv[i], "--lon0") == 0 ||
			     strcmp(argv[i], "--center_longitude") == 0 ||
			     strcmp(argv[i], "--slon") == 0 ||
			     strcmp(argv[i], "-slon") == 0 ||
			     strcmp(argv[i], "--lon_0") == 0)
		    {
			double val;
			++i;

			if (i == argc)
			{
			    no_arg(argv[i-1]);
			    return NULL;
			}

			if (specified_lon0)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_lon0 = 1;

			if (parse_double(argv[i], &val))
			{
			    pps->lamcc.lon0 = val;
			}
			else
			{
			    bad_arg(argv[i-1], argv[i]);
			    return NULL;
			}
		    }

		    else if (strcmp(argv[i], "--false_easting") == 0 ||
			     strcmp(argv[i], "-fe") == 0)
		    {
			double val;
			++i;

			if (i == argc)
			{
			    no_arg(argv[i-1]);
			    return NULL;
			}

			if (specified_false_easting)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_false_easting = 1;

			if (parse_double(argv[i], &val))
			{
			    pps->lamcc.false_easting = val;
			}
			else
			{
			    bad_arg(argv[i-1], argv[i]);
			    return NULL;
			}
		    }

		    else if (strcmp(argv[i], "--false_northing") == 0 ||
			     strcmp(argv[i], "-fn") == 0)
		    {
			double val;
			++i;

			if (i == argc)
			{
			    no_arg(argv[i-1]);
			    return NULL;
			}

			if (specified_false_northing)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_false_northing = 1;

			if (parse_double(argv[i], &val))
			{
			    pps->lamcc.false_northing = val;
			}
			else
			{
			    bad_arg(argv[i-1], argv[i]);
			    return NULL;
			}
		    }

		    else if (strcmp(argv[i], "--scale_factor") == 0 ||
			     strcmp(argv[i], "-sf") == 0)
		    {
			double val;
			++i;

			if (i == argc)
			{
			    no_arg(argv[i-1]);
			    return NULL;
			}

			if (specified_scale_factor)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_scale_factor = 1;

			if (parse_double(argv[i], &val))
			{
			    pps->lamcc.scale_factor = val;
			}
			else
			{
			    bad_arg(argv[i-1], argv[i]);
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

		return pps;
	    }
	    else if (strcmp(argv[i], "lamaz") == 0)
	    {

	    }
	    else if (strcmp(argv[i], "albers") == 0)
	    {

	    }
	    else
	    {
		asfPrintWarning("Unknown projection: %s\n", argv[i]);
		return NULL;
	    }
	}
    }

    asfPrintWarning("No projection Specified");
    return NULL;
}
