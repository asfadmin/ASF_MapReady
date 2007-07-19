#include "asf.h"
#include "asf_nan.h"
#include "asf_meta.h"

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <unistd.h>
#include <dirent.h>

// To avoid having to change link flags around or link against glib we
// have this compatability function.
static char *
static_strdup (const char *s)
{
  char *result = malloc (sizeof (char) * (strlen (s) + 1));

  int idx = 0;
  while ( s[idx] != '\0') {
    result[idx] = s[idx];
    idx++;
  }

  result[idx] = '\0';

  return result;
}


// To avoid having to change link flags around or link against glib we
// have this function.  Its like strcasecmp but only returns true or
// false (true if strings are not equal disregarding case, false
// otherwise).
static int
static_strcaseneq (const char *s1, const char *s2)
{
    return !!strcmp_case(s1,s2);
}

static void readline(FILE * f, char * buffer, size_t n)
{
  char * p;
  p = fgets(buffer, n, f);
  
  if (!p)
  {
      strcpy(buffer, "");
  }
  else
  {
      p = buffer + strlen(buffer) - 1;
      while(isspace(*p)) *p-- = '\0';
  }
}

static int parse_val(char * inbuf, char * key, double * val)
{
  char * p, * eq, * buf;
  int match = FALSE;
  
  buf = static_strdup(inbuf);
  
  p = eq = strchr(buf, '=');
  if (!eq)
    return FALSE;
  
  *eq = '\0';
  --p;
  
  while (isspace((int)(*p)))
    *p-- = '\0';
  
  if (static_strcaseneq(buf, key) == 0)
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
      *(vals[nkeys]) = MAGIC_UNSET_DOUBLE;
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
	  
	  ////Changed to ignore fields that aren't understood
	  //if (!found)
	  //	asfPrintWarning("Unknown key found in projection file: %s\n",
	  //	buf);
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

FILE *fopen_proj_file(const char *file, const char *mode)
{
    FILE *fp;

    // first, just try opening the file as-is
    fp = fopen(file, mode);
    if (fp) return fp;

    char filename[1024];

    // failed -- try with a ".proj" extension
    sprintf(filename, "%s.proj", file);
    fp = fopen(filename, mode);
    if (fp) return fp;

    char share_dir[1024];

    // go through files in the share dir
    sprintf(share_dir, "%s/projections", get_asf_share_dir());

    struct dirent *dp;
    DIR *dfd;

    if ((dfd = opendir(share_dir)) == NULL)
        return NULL; // couldn't open share dir

    while ((dp = readdir(dfd)) != NULL) {
        if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0)
            continue;

        if (strlen(share_dir)+strlen(dp->d_name)+2 > sizeof(filename)) {
            asfPrintWarning("fopen_proj_file> buffersize exceeded.\n");
            return NULL;
        }

        sprintf(filename, "%s/%s/%s", share_dir, dp->d_name, file);
        fp = fopen(filename, mode);
        if (fp) {
            closedir(dfd);
            //printf("Found proj file: %s\n", filename);
            return fp; // found!
        }
            
        // failed, try adding extension
        sprintf(filename, "%s/%s/%s.proj", share_dir, dp->d_name, file);
        fp = fopen(filename, mode);
        if (fp) {
            closedir(dfd);
            //printf("Found proj file: %s\n", filename);
            return fp;
        }
    }

    closedir(dfd);
    
    // failed
    return NULL;
}

void read_proj_file(char * file, project_parameters_t * pps,
		    projection_type_t * proj_type)
{
  FILE * fp;
  char buf[256];
  
  fp = fopen_proj_file(file, "r");
  if (!fp)
  {
      asfPrintError("Couldn't open projection file: %s\n", file);
      return; /* not reached */
  }
  
  readline(fp, buf, sizeof(buf));
  
  if (strcmp(buf, bracketed_projection_name(ALBERS_EQUAL_AREA)) == 0 ||
      strcmp(buf, "[Albers Equal Area Conic]") == 0)
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
  else if 
    (strcmp(buf, bracketed_projection_name(LAMBERT_AZIMUTHAL_EQUAL_AREA)) == 0)
    {
      *proj_type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
      get_fields(fp,
		 "Central Meridian", &pps->lamaz.center_lon,
		 "Latitude of Origin", &pps->lamaz.center_lat,
		 "False Easting", &pps->lamaz.false_easting,
		 "False Northing", &pps->lamaz.false_northing,
		 NULL);
    }
  else if 
    (strcmp(buf, bracketed_projection_name(LAMBERT_CONFORMAL_CONIC)) == 0)
    {
      *proj_type = LAMBERT_CONFORMAL_CONIC;
      get_fields(fp,
		 "First standard parallel", &pps->lamcc.plat1,
		 "Second standard parallel", &pps->lamcc.plat2,
		 "Central Meridian", &pps->lamcc.lon0,
		 "Latitude of Origin", &pps->lamcc.lat0,
		 "False Easting", &pps->lamcc.false_easting,
		 "False Northing", &pps->lamcc.false_northing,
		 /* "Scale Factor", &pps->lamcc.scale_factor, */
		 NULL);
    }
  else if (strcmp(buf, bracketed_projection_name(POLAR_STEREOGRAPHIC)) == 0)
    {
      //char area[10];
      double is_north_pole;
      *proj_type = POLAR_STEREOGRAPHIC;
      get_fields(fp,
		 "Standard parallel", &pps->ps.slat,
		 "Standard Parallel", &pps->ps.slat,
		 "Central Meridian", &pps->ps.slon,
		 "False Easting", &pps->ps.false_easting,
		 "False Northing", &pps->ps.false_northing,
		 "Hemisphere", &is_north_pole,
		 NULL);
      pps->ps.is_north_pole = pps->ps.slat > 0;
    }
  else if 
    (strcmp(buf, bracketed_projection_name(UNIVERSAL_TRANSVERSE_MERCATOR)) == 0)
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
      
      if (pps->utm.zone == 0 || ISNAN(zone))
	pps->utm.zone = MAGIC_UNSET_INT;
    }
  else
    {
      asfPrintError("Unknown Projection in file '%s': %s\n",
		    file, buf);
    }
  
  fclose(fp);
}

