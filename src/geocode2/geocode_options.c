#include "geocode_options.h"
#include "asf.h"
#include "asf_meta.h"
#include "asf_reporting.h"

#include <stdlib.h>
#include <string.h>

/*
  --projection <name> <<projection specific options>>

  --projection utm --zone <zone>
  --projection ps --center_latitude 14.0 --center_longitude 

*/
static int print_warn = 1;

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
		    int val;
		    ++i;

		    if (parse_int(argv[i], &val))
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
			int val;
			++i;

			if (specified_slat)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_slat = 1;

			if (parse_int(argv[i], &val))
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
			int val;
			++i;

			if (specified_slon)
			{
			    double_arg(argv[i]);
			    return NULL;
			}

			specified_slon = 1;

			if (parse_int(argv[i], &val))
			{
			    pps->ps.slon = val;
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

		return pps;
	    }
	    else if (strcmp(argv[i], "lamcc") == 0)
	    {

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
	    }
	}
    }

    asfPrintWarning("No projection Specified");
}
