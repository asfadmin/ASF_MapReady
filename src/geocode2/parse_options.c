#include "parse_options.h"
#include "geocode_options.h"
#include "asf.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "asf_reporting.h"

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

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

/* turns out didn't need this one
static void missing_arg(const char * option)
{
    if (print_warn)
	asfPrintWarning(
	    "Projection requires option that was not specified: %s\n", option);
}
*/

void set_options_testing(int is_testing)
{
    print_warn = !is_testing;
}

static void readline(FILE * f, char * buffer, size_t n)
{
    char * p;
    char * newline;
    
    p = fgets(buffer, n, f);
    
    if (!p)
    {
	strcpy(buffer, "");
    }
    else
    {
	newline = strrchr(buffer, '\n');
	if (newline)
	    *newline = '\0';
    }
}

static int parse_val(char * inbuf, char * key, double * val)
{
    char * p, * eq, * buf;
    int match = FALSE;

    buf = strdup(inbuf);

    p = eq = strchr(buf, '=');
    if (!eq)
	return FALSE;

    *eq = '\0';
    --p;

    while (isspace((int)(*p)))
	*p-- = '\0';
 
    if (strcasecmp(buf, key) == 0)
    {
	p = eq + 1;
	while (isspace((int)(*p)))
	    ++p;

	if (*p)
	{
	    double d;
	    if (parse_double(p, &d))
	    {
		*val = d;
		match = TRUE;
	    }
	    else
	    {
		asfPrintWarning("Illegal value found for key '%s': %s\n",
				key, p);
	    }
	}
    }

    free(buf);
    return match;
}

static void get_fields(FILE * fp, ...)
{
    va_list ap;
    char * keys[32];
    double * vals[32];
    char buf[256];
    unsigned int nkeys = 0;

    va_start(ap, fp);
    while (nkeys < sizeof(keys))
    {
	keys[nkeys] = va_arg(ap, char *);
	if (!keys[nkeys])
	    break;

	vals[nkeys] = va_arg(ap, double *);
	++nkeys;
    }
    va_end(ap);

    while (!feof(fp))
    {
	unsigned int i;
	int found = FALSE;

	readline(fp, buf, sizeof(buf));

	if (strlen(buf) > 0)
	{
	    for (i = 0; i < nkeys; ++i)
	    {
		if (parse_val(buf, keys[i], vals[i]))
		{
		    found = TRUE;
		    break;
		}
	    }

	    if (!found)
		asfPrintWarning("Unknown key found in projection file: %s\n",
				buf);
	}
    }
}

static const char * bracketed_projection_name(projection_type_t proj_type)
{
    switch (proj_type)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:
	    return "[Universal Transverse Mercator]";

	case POLAR_STEREOGRAPHIC:
	    return "[Polar Stereographic]";

	case ALBERS_EQUAL_AREA:
	    return "[Albers Conical Equal Area]";

	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	    return "[Lambert Azimuthal Equal Area]";

	case LAMBERT_CONFORMAL_CONIC:
	    return "[Lambert Conformal Conic]";

	default:
	    asfPrintError("projection_name: illegal projection type!");
	    return "";
    }
}

#ifndef TEST_PARSE_OPTIONS
static
#endif
void write_args(projection_type_t proj_type, project_parameters_t *pps,
		char * file)
{
    FILE * fp = FOPEN(file, "wt");

    fprintf(fp, "%s\n", bracketed_projection_name(proj_type));

    switch (proj_type)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:
	    fprintf(fp, "Scale Factor=%.10f\n", pps->utm.scale_factor);
	    fprintf(fp, "Central Meridian=%.10f\n", pps->utm.lon0);
	    fprintf(fp, "Latitude of Origin=%.10f\n", pps->utm.lat0);
	    fprintf(fp, "False Easting=%.10f\n", pps->utm.false_easting);
	    fprintf(fp, "False Northing=%.10f\n", pps->utm.false_northing);
	    fprintf(fp, "Zone=%d\n", pps->utm.zone);
	    break;

	case POLAR_STEREOGRAPHIC:
	    fprintf(fp, "First Standard Parallel=%.10f\n", pps->ps.slat);
	    fprintf(fp, "Central Meridian=%.10f\n", pps->ps.slon);
	    fprintf(fp, "False Easting=%.10f\n", pps->ps.false_easting);
	    fprintf(fp, "False Northing=%.10f\n", pps->ps.false_northing);
	    fprintf(fp, "Northern Projection=%d\n", pps->ps.is_north_pole);
	    break;

	case ALBERS_EQUAL_AREA:
	    fprintf(fp, "First standard parallel=%.10f\n",
		    pps->albers.std_parallel1);
	    fprintf(fp, "Second standard parallel=%.10f\n",
		    pps->albers.std_parallel2);
	    fprintf(fp, "Central Meridian=%.10f\n",
		    pps->albers.center_meridian);
	    fprintf(fp, "Latitude of Origin=%.10f\n",
		    pps->albers.orig_latitude);
	    fprintf(fp, "False Easting=%.10f\n", pps->albers.false_easting);
	    fprintf(fp, "False Northing=%.10f\n", pps->albers.false_northing);
	    break;

	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	    fprintf(fp, "Central Meridian=%.10f\n", pps->lamaz.center_lon);
	    fprintf(fp, "Latitude of Origin=%.10f\n", pps->lamaz.center_lat);
	    fprintf(fp, "False Easting=%.10f\n", pps->lamaz.false_easting);
	    fprintf(fp, "False Northing=%.10f\n", pps->lamaz.false_northing);
	    break;

	case LAMBERT_CONFORMAL_CONIC:
	    fprintf(fp, "First standard parallel=%.10f\n", pps->lamcc.plat1);
	    fprintf(fp, "Second standard parallel=%.10f\n", pps->lamcc.plat2);
	    fprintf(fp, "Central Meridian=%.10f\n", pps->lamcc.lon0);
	    fprintf(fp, "Latitude of Origin=%.10f\n", pps->lamcc.lat0);
	    fprintf(fp, "False Easting=%.10f\n", pps->lamcc.false_easting);
	    fprintf(fp, "False Northing=%.10f\n", pps->lamcc.false_northing);
	    fprintf(fp, "Scale Factor=%.10f\n",
		    ISNAN(pps->lamcc.scale_factor) ? 
		        1.0 : 
		        pps->lamcc.scale_factor);
	    break;

	default:
	    asfPrintError("write_args: illegal projection type!");
	    break;
    }

    fclose(fp);
}

static void parse_proj_args_file(char * file, project_parameters_t * pps,
				 projection_type_t * proj_type)
{
    FILE * fp;
    char buf[256];

    fp = fopen(file, "rt");
    if (!fp)
    {
	asfPrintError("Couldn't open projection file: %s\n", file);
	return; /* not reached */
    }

    readline(fp, buf, sizeof(buf));

    if (strcmp(buf, bracketed_projection_name(ALBERS_EQUAL_AREA)) == 0)
    {
	*proj_type = ALBERS_EQUAL_AREA;
	get_fields(fp,
		   "First standard parallel", &pps->albers.std_parallel1,
		   "Second standard parallel", &pps->albers.std_parallel2,
		   "Central Meridian", &pps->albers.center_meridian,
		   "Latitude of Origin", &pps->albers.orig_latitude,
		   "False Easting", &pps->albers.false_easting,
		   "False Northing", &pps->albers.false_northing,
		   NULL);
    }
    else if (strcmp(buf, bracketed_projection_name(
			LAMBERT_AZIMUTHAL_EQUAL_AREA)) == 0)
    {
	*proj_type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
	get_fields(fp,
		   "Central Meridian", &pps->lamaz.center_lon,
		   "Latitude of Origin", &pps->lamaz.center_lat,
		   "False Easting", &pps->lamaz.false_easting,
		   "False Northing", &pps->lamaz.false_northing,
		   NULL);
    }
    else if (strcmp(buf, bracketed_projection_name(
			LAMBERT_CONFORMAL_CONIC)) == 0)
    {
	*proj_type = LAMBERT_CONFORMAL_CONIC;
	get_fields(fp,
		   "First standard parallel", &pps->lamcc.plat1,
		   "Second standard parallel", &pps->lamcc.plat2,
		   "Central Meridian", &pps->lamcc.lon0,
		   "Latitude of Origin", &pps->lamcc.lat0,
		   "False Easting", &pps->lamcc.false_easting,
		   "False Northing", &pps->lamcc.false_northing,
		   "Scale Factor", &pps->lamcc.scale_factor,
		   NULL);
    }
    else if (strcmp(buf, bracketed_projection_name(POLAR_STEREOGRAPHIC)) == 0)
    {
	double is_north_pole;
	*proj_type = POLAR_STEREOGRAPHIC;
	get_fields(fp,
		   "First standard parallel", &pps->ps.slat,
		   "Central Meridian", &pps->ps.slon,
		   "False Easting", &pps->ps.false_easting,
		   "False Northing", &pps->ps.false_northing,
  		   "Northern Projection", &is_north_pole,
		   NULL);
	pps->ps.is_north_pole = (int) is_north_pole;
    }
    else if (strcmp(buf, bracketed_projection_name(
			UNIVERSAL_TRANSVERSE_MERCATOR)) == 0)
    {
	double zone;
	*proj_type = UNIVERSAL_TRANSVERSE_MERCATOR;
	get_fields(fp,
		   "Scale Factor", &pps->utm.scale_factor,
		   "Central Meridian", &pps->utm.lon0,
		   "Latitude of Origin", &pps->utm.lat0,
		   "False Easting", &pps->utm.false_easting,
		   "False Northing", &pps->utm.false_northing,
		   "Zone", &zone,
		   NULL);
	pps->utm.zone = (int) zone;
    }
    else
    {
	asfPrintError("Unknown Projection in file '%s': %s\n",
		      file, buf);
    }

    fclose(fp);
}

static int matches_write_proj_file(char * param)
{
    return
	strcmp(param, "--write-proj-file") == 0;
}

static int matches_read_proj_file(char * param)
{
    return
	strcmp(param, "--read-proj-file") == 0;
}

static int parse_double_option(int *i, int argc, char *argv[], int *specified,
			       double *value)
{
    double val;
    int ok;

    ++(*i);
    if (*i == argc)
    {
	no_arg(argv[*i-1]);
	ok = FALSE;
    }
    else if (*specified)
    {
	double_arg(argv[*i]);
	ok = FALSE;
    }
    else
    {
	*specified = TRUE;
	if (parse_double(argv[*i], &val))
	{
	    *value = val;
	    ok = TRUE;
	}
	else
	{
	    bad_arg(argv[*i-1], argv[*i]);
	    ok = FALSE;
	}
    }

    return ok;
}

static int parse_string_option(int *i, int argc, char *argv[], int *specified,
			       char *value)
{
    int ok;

    ++(*i);
    if (*i == argc)
    {
	no_arg(argv[*i-1]);
	ok = FALSE;
    }
    else if (*specified)
    {
	double_arg(argv[*i]);
	ok = FALSE;
    }
    else
    {
	*specified = TRUE;
	strcpy(value, argv[*i]);
        ok = TRUE;
    }

    return ok;
}

static int parse_int_option(int *i, int argc, char *argv[], int *specified,
			    int *value)
{
    int val;
    int ok;

    ++(*i);
    if (*i == argc)
    {
	no_arg(argv[*i-1]);
	ok = FALSE;
    }
    else if (*specified)
    {
	double_arg(argv[*i]);
	ok = FALSE;
    }
    else
    {
	*specified = TRUE;
	if (parse_int(argv[*i], &val))
	{
	    *value = val;
	    ok = TRUE;
	}
	else
	{
	    bad_arg(argv[*i-1], argv[*i]);
	    ok = FALSE;
	}
    }

    return ok;
}

static int parse_zone_option(int *i, int argc,
			     char *argv[], int *specified,
			     int *value, int *ok)
{
    if (strcmp(argv[*i], "--zone") == 0 ||
	strcmp(argv[*i], "-z") == 0)
    {
	*ok = parse_int_option(i, argc, argv, specified, value);
	return TRUE;
    }
    else
    {
	return FALSE;
    }
}

static int parse_first_standard_parallel_option(int *i, int argc,
						char *argv[], int *specified,
						double *value, int *ok)
{
    if (strcmp(argv[*i], "--first-standard-parallel") == 0)
    {
	*ok = parse_double_option(i, argc, argv, specified, value);
	return TRUE;
    }
    else
    {
	return FALSE;
    }
}

static int parse_second_standard_parallel_option(int *i, int argc,
						 char *argv[], int *specified,
						 double *value, int *ok)
{
    if (strcmp(argv[*i], "--second-standard-parallel") == 0)
    {
	*ok = parse_double_option(i, argc, argv, specified, value);
	return TRUE;
    }
    else
    {
	return FALSE;
    }
}

static int parse_center_latitude_option(int *i, int argc,
					char *argv[], int *specified,
					double *value, int *ok)
{
    if (strcmp(argv[*i], "--center-latitude") == 0)
    {
	*ok = parse_double_option(i, argc, argv, specified, value);
	return TRUE;
    }
    else
    {
	return FALSE;
    }
}

static int parse_central_meridian_option(int *i, int argc,
					 char *argv[], int *specified,
					 double *value, int *ok)
{
    if (strcmp(argv[*i], "--central-meridian") == 0)
    {
	*ok = parse_double_option(i, argc, argv, specified, value);
	return TRUE;
    }
    else
    {
	return FALSE;
    }
}

static int parse_false_easting_option(int *i, int argc,
				      char *argv[], int *specified,
				      double *value, int *ok)
{
    if (strcmp(argv[*i], "--false-easting") == 0)
    {
	*ok = parse_double_option(i, argc, argv, specified, value);
	return TRUE;
    }
    else
    {
	return FALSE;
    }
}

static int parse_false_northing_option(int *i, int argc,
				      char *argv[], int *specified,
				      double *value, int *ok)
{
    if (strcmp(argv[*i], "--false-northing") == 0)
    {
	*ok = parse_double_option(i, argc, argv, specified, value);
	return TRUE;
    }
    else
    {
	return FALSE;
    }
}

static int parse_scale_factor_option(int *i, int argc,
				     char *argv[], int *specified,
				     double *value, int *ok)
{
    if (strcmp(argv[*i], "--scale-factor") == 0)
    {
	*ok = parse_double_option(i, argc, argv, specified, value);
	return TRUE;
    }
    else
    {
	return FALSE;
    }
}

static int parse_write_proj_file_option(int *i, int argc, char *argv[],
					char **write_file, int *ok)
{
    if (matches_write_proj_file(argv[*i]))
    {
	++(*i);

	if (*i == argc)
	{
	    no_arg(argv[*i-1]);
	    *ok = FALSE;
	}
	else
	{
	    *write_file = argv[*i];
	    *ok = TRUE;
	}

	return TRUE;
    }
    else
    {
	return FALSE;
    }
}

static int parse_read_proj_file_option(int *i, int argc, char *argv[],
				       projection_type_t *proj_type,
				       project_parameters_t *pps, int *ok)
{
    if (matches_read_proj_file(argv[*i]))
    {
	++(*i);

	if (*i == argc)
	{
	    no_arg(argv[*i-1]);
	    *ok = FALSE;
	}
	else
	{
	    parse_proj_args_file(argv[*i], pps, proj_type);
	    *ok = TRUE;
	}

	++(*i);
	return TRUE;
    }
    else
    {
	return FALSE;
    }
}

static void remove_args(int start, int end, int *argc, char **argv[])
{
    int i, j, nargs;

    nargs = end - start + 1;
    i = start;
    j = start + nargs;

    while (j < *argc)
    {
	char * tmp = (*argv)[i];
	(*argv)[i] = (*argv)[j];
	(*argv)[j] = tmp;
	
	++i;
	++j;
    }

    *argc -= nargs;
}

static void extract_double_option(int *argc, char **argv[], double *val,
				 char *arg, int *found)
{
    int i;

    for (i = 0; i < *argc; ++i)
    {
	if (strcmp((*argv)[i], arg) == 0)
	{
	    if (parse_double_option(&i, *argc, *argv, found, val))
	    {
		remove_args(i-1, i, argc, argv);
	    }
	}
    }
}

static void extract_double_options(int *argc, char **argv[],
				   double *val, ... )
{
    va_list ap;
    char * arg = NULL;
    int found = FALSE;

    *val = MAGIC_UNSET_DOUBLE;

    va_start(ap, val);
    do
    {
	arg = va_arg(ap, char *);

	if (arg)
	    extract_double_option(argc, argv, val, arg, &found);
    }
    while (arg);
}

static void extract_string_option(int *argc, char **argv[], char *val,
				  char *arg, int *found)
{
    int i;

    for (i = 0; i < *argc; ++i)
    {
	if (strcmp((*argv)[i], arg) == 0)
	{
	    if (parse_string_option(&i, *argc, *argv, found, val))
	    {
		remove_args(i-1, i, argc, argv);
	    }
	}
    }
}

/* assumes the space has been allocated by caller */
static int extract_string_options(int *argc, char **argv[],
				  char *val, ... )
{
    va_list ap;
    char * arg = NULL;
    int found = FALSE;

    *val = '\0';

    va_start(ap, val);
    do
    {
	arg = va_arg(ap, char *);

	if (arg)
	    extract_string_option(argc, argv, val, arg, &found);
    }
    while (arg);

    return found;
}

static void extract_flag(int *argc, char **argv[],
			 char *arg, int *found)
{
    int i;

    for (i = 0; i < *argc; ++i)
    {
	if (strcmp((*argv)[i], arg) == 0)
	{
	    *found = TRUE;
	    remove_args(i, i, argc, argv);
	}
    }
}

static int extract_flags(int *argc, char **argv[], ... )
{
    va_list ap;
    char * arg = NULL;
    int found = FALSE;

    va_start(ap, argv);
    do
    {
	arg = va_arg(ap, char *);

	if (arg)
	    extract_flag(argc, argv, arg, &found);
    }
    while (arg);

    return found;
}

void parse_log_options(int *argc, char **argv[])
{
    if (extract_string_options(argc, argv, logFile, "-log", NULL))
	logflag = 1;
    else
	logflag = 0;

    if (extract_flags(argc, argv, "-quiet", NULL))
	quietflag = 1;
    else
	quietflag = 0;
}

void parse_other_options(int *argc, char **argv[],
			 double *height, double *pixel_size)
{
    extract_double_options(argc, argv, height, "--height", "-h", NULL);
    extract_double_options(argc, argv, pixel_size,
			   "--pixel-size", "-ps", NULL);
}

project_parameters_t * parse_projection_options(int *argc, char **argv[],
						projection_type_t * proj_type)
{
    int i;
    project_parameters_t *pps = malloc(sizeof(project_parameters_t));
    int start_arg = 0, end_arg = 0;
    char * write_file = NULL;
    int ok = TRUE;

    for (i = 0; i < *argc; ++i)
    {
	if (parse_read_proj_file_option(&i, *argc, *argv, proj_type, pps, &ok))
	{
	    if (!ok)
		return NULL;

	    break;
	}
					
	else if (strcmp((*argv)[i], "--projection") == 0 ||
		 strcmp((*argv)[i], "-p") == 0)
	{
	    if (++i == *argc)
	    {
		no_arg((*argv)[i-1]);
		return NULL;
	    }

	    if (strcmp((*argv)[i], "utm") == 0)
	    {
		int specified_zone = FALSE;
		int specified_lat0 = FALSE;
		int specified_lon0 = FALSE;

		*proj_type = UNIVERSAL_TRANSVERSE_MERCATOR;

		while (TRUE)
		{
		    if (!ok)
			return NULL;

		    if (++i == *argc)
			break;

		    if (parse_write_proj_file_option(
			    &i, *argc, *argv, &write_file, &ok))
			continue;

		    if (parse_zone_option(
			    &i, *argc, *argv, &specified_zone,
			    &pps->utm.zone, &ok))
			continue;

		    if (parse_center_latitude_option(
			    &i, *argc, *argv, &specified_lat0,
			    &pps->utm.lat0, &ok))
			continue;

		    if (parse_central_meridian_option(
			    &i, *argc, *argv, &specified_lon0,
			    &pps->utm.lon0, &ok))
			continue;

		    break;
		}

		if (!specified_lon0)
		    pps->utm.lon0 = MAGIC_UNSET_DOUBLE;

		if (!specified_lat0)
		    pps->utm.lat0 = MAGIC_UNSET_DOUBLE;

		if (!specified_zone)
		    pps->utm.zone = MAGIC_UNSET_INT;

		break;
	    }
	    else if (strcmp((*argv)[i], "ps") == 0)
	    {
		int specified_slat = FALSE;
		int specified_slon = FALSE;
		int specified_pole = FALSE;
		int specified_false_northing = FALSE;
		int specified_false_easting = FALSE;

		pps->ps.is_north_pole = 1;
		*proj_type = POLAR_STEREOGRAPHIC;

		while (TRUE)
		{
		    if (!ok)
			return NULL;

		    if (++i == *argc)
			break;

		    if (parse_write_proj_file_option(
			    &i, *argc, *argv, &write_file, &ok))
			continue;

		    if (parse_center_latitude_option(
			    &i, *argc, *argv, &specified_slat,
			    &pps->ps.slat, &ok))
			continue;

		    if (parse_central_meridian_option(
			    &i, *argc, *argv, &specified_slon,
			    &pps->ps.slon, &ok))
			continue;

		    if (parse_false_easting_option(
			    &i, *argc, *argv, &specified_false_easting,
			    &pps->ps.false_easting, &ok))
			continue;

		    if (parse_false_northing_option(
			    &i, *argc, *argv, &specified_false_northing,
			    &pps->ps.false_northing, &ok))
			continue;

		    if (strcmp((*argv)[i], "--north-pole") == 0 ||
			strcmp((*argv)[i], "-n") == 0)
		    {
			if (specified_pole)
			{
			    double_arg((*argv)[i]);
			    return NULL;
			}

			specified_pole = 1;

			pps->ps.is_north_pole = 1;
			continue;
		    }

		    if (strcmp((*argv)[i], "--south-pole") == 0 ||
			strcmp((*argv)[i], "-s") == 0)
		    {
			if (specified_pole)
			{
			    pole_arg();
			    return NULL;
			}

			specified_pole = 1;

			pps->ps.is_north_pole = -1;
			continue;
		    }

		    break;
		}

		if (!specified_slat)
		{
		    if (pps->ps.is_north_pole == 1)
			pps->ps.slat = 90;
		    else
			pps->ps.slat = -90;
		}

		if (!specified_slon)
		    pps->ps.slon = MAGIC_UNSET_DOUBLE;

		if (!specified_false_easting)
		    pps->ps.false_easting = MAGIC_UNSET_DOUBLE;

		if (!specified_false_northing)
		    pps->ps.false_northing = MAGIC_UNSET_DOUBLE;

		break;
	    }
	    else if (strcmp((*argv)[i], "lamcc") == 0)
	    {
		int specified_plat1 = FALSE;
		int specified_plat2 = FALSE;
		int specified_lat0 = FALSE;
		int specified_lon0 = FALSE;
		int specified_false_easting = FALSE;
		int specified_false_northing = FALSE;
		int specified_scale_factor = FALSE;
		*proj_type = LAMBERT_CONFORMAL_CONIC;

		while (TRUE)
		{
		    if (!ok)
			return NULL;

		    if (++i == *argc)
			break;

		    if (parse_write_proj_file_option(
			    &i, *argc, *argv, &write_file, &ok))
			continue;

		    if (parse_first_standard_parallel_option(
			    &i, *argc, *argv, &specified_plat1,
			    &pps->lamcc.plat1, &ok))
			continue;

		    if (parse_second_standard_parallel_option(
			    &i, *argc, *argv, &specified_plat2,
			    &pps->lamcc.plat2, &ok))
			continue;

		    if (parse_center_latitude_option(
			    &i, *argc, *argv, &specified_lat0,
			    &pps->lamcc.lat0, &ok))
			continue;

		    if (parse_central_meridian_option(
			    &i, *argc, *argv, &specified_lon0,
			    &pps->lamcc.lon0, &ok))
			continue;

		    if (parse_false_easting_option(
			    &i, *argc, *argv, &specified_false_easting,
			    &pps->lamcc.false_easting, &ok))
			continue;

		    if (parse_false_northing_option(
			    &i, *argc, *argv, &specified_false_northing,
			    &pps->lamcc.false_northing, &ok))
			continue;

		    if (parse_scale_factor_option(
			    &i, *argc, *argv, &specified_scale_factor,
			    &pps->lamcc.scale_factor, &ok))
			continue;

		    break;
		}

		if (!specified_plat1)
		    pps->lamcc.plat1 = MAGIC_UNSET_DOUBLE;

		if (!specified_plat2)
		    pps->lamcc.plat2 = MAGIC_UNSET_DOUBLE;

		if (!specified_lat0)
		    pps->lamcc.lat0 = MAGIC_UNSET_DOUBLE;

		if (!specified_lon0)
		    pps->lamcc.lon0 = MAGIC_UNSET_DOUBLE;

		if (!specified_false_easting)
		    pps->lamcc.false_easting = MAGIC_UNSET_DOUBLE;

		if (!specified_false_northing)
		    pps->lamcc.false_northing = MAGIC_UNSET_DOUBLE;

		if (!specified_scale_factor)
		    pps->lamcc.scale_factor = MAGIC_UNSET_DOUBLE;

		break;
	    }
	    else if (strcmp((*argv)[i], "lamaz") == 0)
	    {
		int specified_lat0 = FALSE;
		int specified_lon0 = FALSE;
		int specified_false_easting = FALSE;
		int specified_false_northing = FALSE;

		*proj_type = LAMBERT_AZIMUTHAL_EQUAL_AREA;

		while (TRUE)
		{
		    if (!ok)
			return NULL;

		    if (++i == *argc)
			break;

		    if (parse_write_proj_file_option(
			    &i, *argc, *argv, &write_file, &ok))
			continue;

		    if (parse_center_latitude_option(
			    &i, *argc, *argv, &specified_lat0,
			    &pps->lamaz.center_lat, &ok))
			continue;

		    if (parse_central_meridian_option(
			    &i, *argc, *argv, &specified_lon0,
			    &pps->lamaz.center_lon, &ok))
			continue;

		    if (parse_false_easting_option(
			    &i, *argc, *argv, &specified_false_easting,
			    &pps->lamaz.false_easting, &ok))
			continue;

		    if (parse_false_northing_option(
			    &i, *argc, *argv, &specified_false_northing,
			    &pps->lamaz.false_northing, &ok))
			continue;

		    break;
		}

		if (!specified_lat0)
		    pps->lamaz.center_lat = MAGIC_UNSET_DOUBLE;

		if (!specified_lon0)
		    pps->lamaz.center_lon = MAGIC_UNSET_DOUBLE;

		if (!specified_false_easting)
		    pps->lamaz.false_easting = MAGIC_UNSET_DOUBLE;

		if (!specified_false_northing)
		    pps->lamaz.false_northing = MAGIC_UNSET_DOUBLE;

		break;
	    }
	    else if (strcmp((*argv)[i], "albers") == 0)
	    {
		int specified_plat1 = FALSE;
		int specified_plat2 = FALSE;
		int specified_lat0 = FALSE;
		int specified_lon0 = FALSE;
		int specified_false_easting = FALSE;
		int specified_false_northing = FALSE;

		*proj_type = ALBERS_EQUAL_AREA;

		while (TRUE)
		{
		    if (!ok)
			return NULL;

		    if (++i == *argc)
			break;

		    if (parse_write_proj_file_option(
			    &i, *argc, *argv, &write_file, &ok))
			continue;

		    if (parse_first_standard_parallel_option(
			    &i, *argc, *argv, &specified_plat1,
			    &pps->albers.std_parallel1, &ok))
			continue;

		    if (parse_second_standard_parallel_option(
			    &i, *argc, *argv, &specified_plat2,
			    &pps->albers.std_parallel2, &ok))
			continue;

		    if (parse_center_latitude_option(
			    &i, *argc, *argv, &specified_lat0,
			    &pps->albers.orig_latitude, &ok))
			continue;

		    if (parse_central_meridian_option(
			    &i, *argc, *argv, &specified_lon0,
			    &pps->albers.center_meridian, &ok))
			continue;

		    if (parse_false_easting_option(
			    &i, *argc, *argv, &specified_false_easting,
			    &pps->albers.false_easting, &ok))
			continue;

		    if (parse_false_northing_option(
			    &i, *argc, *argv, &specified_false_northing,
			    &pps->albers.false_northing, &ok))
			continue;

		    break;
		}

		if (!specified_plat1)
		    pps->albers.std_parallel1 = MAGIC_UNSET_DOUBLE;

		if (!specified_plat2)
		    pps->albers.std_parallel2 = MAGIC_UNSET_DOUBLE;

		if (!specified_lat0)
		    pps->albers.orig_latitude = MAGIC_UNSET_DOUBLE;

		if (!specified_lon0)
		    pps->albers.center_meridian = MAGIC_UNSET_DOUBLE;

		if (!specified_false_easting)
		    pps->albers.false_easting = MAGIC_UNSET_DOUBLE;

		if (!specified_false_northing)
		    pps->albers.false_northing = MAGIC_UNSET_DOUBLE;

		break;
	    }
	    else
	    {
		asfPrintWarning("Unknown projection: %s\n", (*argv)[i]);
		return NULL;
	    }
	}

	++start_arg;
    }

    end_arg = i - 1;

    if (start_arg >= end_arg)
    {
	/* don't need this -- caller detects and prints a better message */
	/* asfPrintWarning("No projection specified\n"); */
	return NULL;
    }
    else
    {
	remove_args(start_arg, end_arg, argc, argv);

	for (i = 0; i < *argc; ++i)
	{
	    if (parse_write_proj_file_option(
		    &i, *argc, *argv, &write_file, &ok))
	    {
		remove_args(i-1, i, argc, argv);
		break;
	    }
	}

	if (write_file)
	    write_args(*proj_type, pps, write_file);

	return pps;
    }
}
