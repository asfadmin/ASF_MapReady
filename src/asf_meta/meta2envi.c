#include <assert.h>

#include "asf_meta.h"
#include "envi.h"
#include "asf_nan.h"
#include "time.h"
#include "earth_radius2datum.h"

char *get_str_element(char *str, int num)
{
  int ii;
  char *p, *q, *end;
  char *line = STRDUP(str);
  p = line; 
  q = line;
  end = strchr(line, '}');
  end[0] = '\0';
  while (q[0] == ' ')
    q++;
  for (ii=1; ii<=num; ii++) {
    p = q;
    q = strchr(p, ',');
    if (q) {
      q++;
      while (q[0] == ' ')
	q++;
    }
  }
  q = strchr(p, ',');
  if (q)
    q[0] = '\0';

  return p;
}

envi_header* read_envi(char *envi_name)
{
  envi_header *envi=NULL;
  FILE *fp;
  char line[255]="", key[25]="", value[25]="", *map_info_ptr=NULL;
  char *proj_info_ptr, proj_info[255]="", bla[25], map_info[255]="";
  int projection_key=-1;

  // Allocate memory for ESRI header structure
  envi = (envi_header *)CALLOC(1,sizeof(envi_header));
  envi->band_name = NULL;

  // Read .hdr and fill meta structures
  fp = FOPEN(envi_name, "r");
  while (NULL != fgets(line, 255, fp)) {
    sscanf(line, "%s = %s", key, value);
    //asfPrintStatus("key: %s\n", key);
    if (strncmp(key, "description", 11)==0) {
      if (strchr(line, '}') == NULL) {
        fgets(line, 255, fp);
        char *p = strchr(line, '}');
        p[0] = '\0';
        sprintf(envi->description, "%s", p);
      }
      else {
        char *q = strchr(line, '{');
        char *p = strchr(line, '}');
        p[0] = '\0';
        q++;
        if (q) {
          while (q[0] == ' ' || q[0] == '-')
            q++;
          sprintf(envi->description, "%s", q);
        }
        else
          sprintf(envi->description, "%s", line);
      }
    }
    if (strncmp(key, "samples", 6)==0) envi->samples = atoi(value);
    else if (strncmp(key, "lines", 5)==0) envi->lines = atoi(value);
    else if (strncmp(key, "bands", 5)==0) envi->bands = atoi(value);
    else if (strncmp(key, "header", 6)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "offset", 6)==0)
    envi->header_offset = atoi(value);
    }
    // ignore file type for the moment
    else if (strncmp(key, "data", 4)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "type", 4)==0)
    envi->data_type = atoi(value);
    }
    else if (strncmp(key, "interleave", 10)==0)
      sprintf(envi->interleave, "%s", value);
    else if (strncmp(key, "sensor", 6)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "type", 4)==0)
	sprintf(envi->sensor_type, "%s", value);
    }
    else if (strncmp(key, "byte", 4)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "order", 5)==0)
    envi->byte_order = atoi(value);
    }
    else if (strncmp(key, "map", 3)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "info", 4)==0) {
    map_info_ptr = strstr(line, ",");
    // check for UTM case - does not have projection key
    if (strstr(line, "UTM"))
      projection_key = 3;
    if (strstr(line, "Geographic Lat/Lon"))
      projection_key = 1;
    sprintf(map_info, "%s", map_info_ptr+1);
      }
    }
    else if (strncmp(key, "projection", 10)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "info", 4)==0) {
    proj_info_ptr = strstr(line, ",");
        sprintf(proj_info, "%s", proj_info_ptr+1);
    sscanf(value, "{%i,", &projection_key);
      }
    }
    else if (strncmp(key, "wavelength", 10)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "units", 5)==0)
    sprintf(envi->wavelength_units, "%s", value);
    }
    else if (strncmp(key, "band", 4)==0) {
      if (strchr(line, '}') == NULL) {
        fgets(line, 255, fp);
        char *p = strchr(line, '}');
        p[0] = '\0';
        envi->band_name = trim_spaces(line);
      }
      else {
        char *q = strchr(line, '{');
        char *p = strchr(line, '}');
        p[0] = '\0';
        q++;
        envi->band_name = trim_spaces(q);
      }
    }
    // ignore wavelength for the moment
    // ignore data ignore for the moment
    // ignore default stretch for the moment
  }
  FCLOSE(fp);

  switch(projection_key)
    {
    case 1:
      sprintf(envi->projection, "Geographic Lat/Lon");
      sscanf(map_info, "%i, %i, %lf, %lf, %lf, %lf",
	     &envi->ref_pixel_x, &envi->ref_pixel_y, &envi->pixel_easting,
	     &envi->pixel_northing, &envi->proj_dist_x, &envi->proj_dist_y);
      envi->pixel_size_x = fabs(envi->proj_dist_x);
      envi->pixel_size_y = fabs(envi->proj_dist_y);
      sprintf(envi->datum, "%s", get_str_element(map_info, 7));
      sscanf(proj_info, "%lf, %lf, 0.0, 0.0, %s}",
	     &envi->semimajor_axis, &envi->semiminor_axis, bla);
      break;
    case 3:
      sprintf(envi->projection, "UTM");
      sscanf(map_info, "%i, %i, %lf, %lf, %lf, %lf, %i",
	 &envi->ref_pixel_x, &envi->ref_pixel_y,
	 &envi->pixel_easting, &envi->pixel_northing,
	 &envi->proj_dist_x, &envi->proj_dist_y,
	 &envi->projection_zone);
      envi->pixel_size_x = fabs(envi->proj_dist_x);
      envi->pixel_size_y = fabs(envi->proj_dist_y);
      sprintf(envi->hemisphere, "%s", get_str_element(map_info, 8));
      sprintf(envi->datum, "%s", get_str_element(map_info, 9));
      break;
    case 4:
      sprintf(envi->projection, "Lambert Conformal Conic");
      sscanf(map_info, "%i, %i, %lf, %lf, %lf, %lf",
         &envi->ref_pixel_x, &envi->ref_pixel_y,
         &envi->pixel_easting, &envi->pixel_northing,
         &envi->proj_dist_x, &envi->proj_dist_y);
      sprintf(envi->datum, "%s", get_str_element(map_info, 7));
      envi->pixel_size_x = fabs(envi->proj_dist_x);
      envi->pixel_size_y = fabs(envi->proj_dist_y);
      sscanf(proj_info, "%lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf, %s}",
         &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
         &envi->center_lon, &envi->false_easting, &envi->false_northing,
	 &envi->standard_parallel1, &envi->standard_parallel2, bla);
      break;
    case 9:
      sprintf(envi->projection, "Albers Conical Equal Area");
      sscanf(map_info, "%i, %i, %lf, %lf, %lf, %lf",
         &envi->ref_pixel_x, &envi->ref_pixel_y,
         &envi->pixel_easting, &envi->pixel_northing,
         &envi->proj_dist_x, &envi->proj_dist_y);
      sprintf(envi->datum, "%s", get_str_element(map_info, 7));
      envi->pixel_size_x = fabs(envi->proj_dist_x);
      envi->pixel_size_y = fabs(envi->proj_dist_y);
      sscanf(proj_info, "%lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf, %s}",
         &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
         &envi->center_lon, &envi->false_easting, &envi->false_northing, 
	 &envi->standard_parallel1, &envi->standard_parallel2, bla);
      break;
    case 11:
      sprintf(envi->projection, "Lambert Azimuthal Equal Area");
      sscanf(map_info, "%i, %i, %lf, %lf, %lf, %lf",
         &envi->ref_pixel_x, &envi->ref_pixel_y,
         &envi->pixel_easting, &envi->pixel_northing,
         &envi->proj_dist_x, &envi->proj_dist_y);
      sprintf(envi->datum, "%s", get_str_element(map_info, 7));
      envi->pixel_size_x = fabs(envi->proj_dist_x);
      envi->pixel_size_y = fabs(envi->proj_dist_y);
      sscanf(proj_info, "%lf, %lf, %lf, %lf, %s}",
         &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
         &envi->center_lon, bla);
      break;
    case 31:
      sprintf(envi->projection, "Polar Stereographic");
      sscanf(map_info, "%d, %d, %lf, %lf, %lf, %lf",
         &envi->ref_pixel_x, &envi->ref_pixel_y,
         &envi->pixel_easting, &envi->pixel_northing,
         &envi->proj_dist_x, &envi->proj_dist_y);
      envi->pixel_size_x = fabs(envi->proj_dist_x);
      envi->pixel_size_y = fabs(envi->proj_dist_y);
      sprintf(envi->datum, "%s", get_str_element(map_info, 7));
      sscanf(proj_info, "%lf, %lf, %lf, %lf, %s}",
         &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
         &envi->center_lon, bla);
      break;
    case -1: // projection not set
      sprintf(envi->projection, "not map projected");
      break;
    default:
      sprintf(errbuf, "\n   ERROR: unsupported map projection\n\n");
      printErr(errbuf);
      break;
    }
  if (strlen(envi->hemisphere) == 0) {
    if (envi->center_lat > 0.0)
      strcpy(envi->hemisphere, "North");
    else
      strcpy(envi->hemisphere, "South");
  }
  
  return envi;
}

int datatype2envi(int data_type)
{
  switch (data_type) 
  {
    case ASF_BYTE: return 1;
    case INTEGER16: return 2;
    case INTEGER32: return 3;
    case REAL32: return 4;
    case REAL64: return 5;
    case COMPLEX_REAL32: return 6;
    default: return -1;
   }
}

envi_header* meta2envi(meta_parameters *meta)
{
  envi_header *envi;

  /* Allocate memory for ENVI header */
  envi = (envi_header *)MALLOC(sizeof(envi_header));

  /* Initialize the values */
  envi->samples = MAGIC_UNSET_INT;
  envi->lines = MAGIC_UNSET_INT;
  envi->bands = 1;
  envi->header_offset = 0; /* Currently only generic binary files supported */
  sprintf(envi->file_type, "ENVI Standard"); /* check !!! */
  envi->data_type = 1; /* 8-bit byte as default */
  sprintf(envi->interleave, "bsq"); /* Taken as the default value for now,
                                       since we don't have multiband imagery */
  sprintf(envi->sensor_type, "Unknown");
  envi->byte_order = 1; /* big endian data */
  envi->ref_pixel_x = 0;
  envi->ref_pixel_y = 0;
  envi->pixel_easting = MAGIC_UNSET_DOUBLE;
  envi->pixel_northing = MAGIC_UNSET_DOUBLE;
  envi->proj_dist_x = MAGIC_UNSET_DOUBLE;
  envi->proj_dist_y = MAGIC_UNSET_DOUBLE;
  sprintf(envi->hemisphere, "%s", MAGIC_UNSET_STRING);
  sprintf(envi->projection, "%s", MAGIC_UNSET_STRING);
  strcpy(envi->datum, "");
  envi->standard_parallel1 = MAGIC_UNSET_DOUBLE;
  envi->standard_parallel2 = MAGIC_UNSET_DOUBLE;
  envi->center_lat = MAGIC_UNSET_DOUBLE;
  envi->center_lon = MAGIC_UNSET_DOUBLE;
  sprintf(envi->wavelength_units, "Unknown");
  envi->wavelength = MAGIC_UNSET_DOUBLE;
  envi->data_ignore = MAGIC_UNSET_DOUBLE;
  envi->pixel_size_x = MAGIC_UNSET_DOUBLE;
  envi->pixel_size_y = MAGIC_UNSET_DOUBLE;
  envi->semimajor_axis = MAGIC_UNSET_DOUBLE;
  envi->semiminor_axis = MAGIC_UNSET_DOUBLE;
  sprintf(envi->default_stretch, "%s", MAGIC_UNSET_STRING);
  envi->data_type = datatype2envi(meta->general->data_type);
  if (envi->data_type == -1) {
      sprintf(errbuf,"\n   ERROR: Unsupported data type\n\n");
      printErr(errbuf);
  }

  /* Fill the values in from the metadata */
  envi->samples = meta->general->sample_count;
  envi->lines = meta->general->line_count;
  envi->bands = meta->general->band_count;
  if (strcmp(meta->general->sensor, "RSAT-1")==0)
    sprintf(envi->sensor_type, "RADARSAT");
  else if (strncmp(meta->general->sensor, "ERS1", 4)==0 ||
	   strncmp(meta->general->sensor, "ERS-1", 5)==0)
    sprintf(envi->sensor_type, "ERS-1");
  else if (strncmp(meta->general->sensor, "ERS2", 4)==0 ||
	   strncmp(meta->general->sensor, "ERS-2", 5)==0)
    sprintf(envi->sensor_type, "ERS-2");
  else if (strncmp(meta->general->sensor, "JERS1", 5)==0 ||
	   strncmp(meta->general->sensor, "JERS-1", 6)==0)
    sprintf(envi->sensor_type, "JERS-1");
  else if(strncmp(meta->general->sensor, "SIR-C", 5)==0)
    sprintf(envi->sensor_type, "SIR-C");
  else if (strncmp(meta->general->sensor, "AIRSAR", 6)==0)
    sprintf(envi->sensor_type, "AIRSAR");
  else if (strncmp(meta->general->sensor, "UAVSAR", 6)==0)
    sprintf(envi->sensor_type, "UAVSAR");
  else if (strncmp(meta->general->sensor, "ALOS", 4)==0)
    sprintf(envi->sensor_type, "ALOS");
  // All the data we generate now is big_endian by default
  envi->byte_order = 1;
  if (meta->projection)
  {
    switch (meta->projection->type)
      {
      default:
        asfPrintError("Unsupported projection: %d\n", meta->projection->type);
        break;
      case LAT_LONG_PSEUDO_PROJECTION:
	sprintf(envi->projection, "Geographic Lat/Lon");
	break;
      case UNIVERSAL_TRANSVERSE_MERCATOR: 
	sprintf(envi->projection, "UTM"); 
	envi->projection_zone = meta->projection->param.utm.zone;
	envi->center_lat = meta->projection->param.utm.lat0;
	envi->center_lon = meta->projection->param.utm.lon0;
	break;
      case POLAR_STEREOGRAPHIC:
	sprintf(envi->projection, "Polar Stereographic"); 
	envi->center_lat = meta->projection->param.ps.slat;
	envi->center_lon = meta->projection->param.ps.slon;
	break;
      case ALBERS_EQUAL_AREA:
	sprintf(envi->projection, "Albers Conical Equal Area"); 
	envi->standard_parallel1 = meta->projection->param.albers.std_parallel1;
	envi->standard_parallel2 = meta->projection->param.albers.std_parallel2;
	envi->center_lat = meta->projection->param.albers.orig_latitude;
	envi->center_lon = meta->projection->param.albers.center_meridian;
	break;
      case LAMBERT_CONFORMAL_CONIC: 
	sprintf(envi->projection, "Lambert Conformal Conic"); 
	envi->standard_parallel1 = meta->projection->param.lamcc.plat1;
	envi->standard_parallel2 = meta->projection->param.lamcc.plat2;
	envi->center_lat = meta->projection->param.lamcc.lat0;
	envi->center_lon = meta->projection->param.lamcc.lon0;
	break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA: 
	sprintf(envi->projection, "Lambert Azimuthal Equal Area");
	envi->center_lat = meta->projection->param.lamaz.center_lat;
	envi->center_lon = meta->projection->param.lamaz.center_lon;
	break;
      case EQUI_RECTANGULAR:
	sprintf(envi->projection, "Equi Rectangualr");
	envi->center_lat = meta->projection->param.eqr.orig_latitude;
	envi->center_lon = meta->projection->param.eqr.central_meridian;
	break;
      case STATE_PLANE: 
      case SCANSAR_PROJECTION: 
	break;
      case MERCATOR:
      case UNKNOWN_PROJECTION:
	// I haven't tested this at all.
	assert (0);
	break;
      }
    envi->ref_pixel_y = 1;
    envi->ref_pixel_x = 1;
    envi->pixel_easting = meta->projection->startX;
    envi->pixel_northing = meta->projection->startY;
    envi->proj_dist_x = meta->projection->perX;
    envi->proj_dist_y = fabs(meta->projection->perY);
    if (strcmp(envi->projection,"UTM") != 0)
      envi->projection_zone = 0;
    if (meta->projection->hem == 'N') 
      sprintf(envi->hemisphere, "North");
    else if (meta->projection->hem == 'S')
      sprintf(envi->hemisphere, "South");
    envi->semimajor_axis = meta->projection->re_major;
    envi->semiminor_axis = meta->projection->re_minor;
    switch (meta->projection->datum) {
      case NAD27_DATUM:
        strcpy(envi->datum, "North America 1927");
        break;
      case NAD83_DATUM:
        strcpy(envi->datum, "North America 1983");
        break;
      case ED50_DATUM:
        strcpy(envi->datum, "European 1950");
        break;
      case WGS72_DATUM:
        strcpy(envi->datum, "WGS-72");
        break;
      case WGS84_DATUM:
        strcpy(envi->datum, "WGS-84");
        break;
      case HUGHES_DATUM:
	strcpy(envi->datum, "Hughes");
	break;
      default:
        // Keep quiet for now, this would get annoying
        // printf("Datum %s not supported by ENVI, using WGS84\n");
        strcpy(envi->datum, "WGS-84");
        break;
    }
  }
  if (meta->sar) {
    envi->wavelength = meta->sar->wavelength;
    sprintf(envi->wavelength_units, "meters");
  }
  envi->pixel_size_x = meta->general->x_pixel_size;
  envi->pixel_size_y = meta->general->y_pixel_size;
  envi->band_name = meta->general->bands;

  return envi;
}

meta_parameters* envi2meta(envi_header *envi)
{
  meta_parameters *meta;

  /* Allocate memory for metadata structure */
  meta = raw_init();

  /* Fill metadata with valid ENVI header data */
  strcpy(meta->general->basename, envi->description);
  strcpy(meta->general->sensor, envi->sensor_type);
  meta->general->line_count = envi->lines;
  meta->general->sample_count = envi->samples;
  // We have to assume that we are not dealing with a subset.
  // Hence, the start line/sample are set to 0.
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->band_count = envi->bands;
  strcpy(meta->general->bands, envi->band_name);
  meta->general->re_major = envi->semimajor_axis;
  meta->general->re_minor = envi->semiminor_axis;
  // Another assumption is that we are dealing with geocoded data that has
  // regular zero fill.
  meta->general->no_data = 0.0;

  switch (envi->data_type)
    {
    case 1: meta->general->data_type = ASF_BYTE; break;
    case 2: meta->general->data_type = INTEGER16; break;
    case 3: meta->general->data_type = INTEGER32; break;
    case 4: meta->general->data_type = REAL32; break;
    case 5: meta->general->data_type = REAL64; break;
    case 6: meta->general->data_type = COMPLEX_REAL32; break;
    default:
      sprintf(errbuf,"\n   ERROR: Unsupported data type\n\n");
      printErr(errbuf);
      break;
    }

  if (strncmp(envi->sensor_type, "RADARSAT", 8)==0)
    sprintf(meta->general->sensor, "RSAT-1");
  else 
    sprintf(meta->general->sensor, "%s", envi->sensor_type);

  if (strcmp(envi->projection, "not map projected") != 0) {
    if (!meta->projection) meta->projection = meta_projection_init();
    
    if (strncmp(envi->projection, "UTM", 3)==0) {
      meta->projection->type = UNIVERSAL_TRANSVERSE_MERCATOR;
      meta->projection->param.utm.zone = envi->projection_zone;
      // Fill in the default values for UTM
      meta->projection->param.utm.false_easting = 500000.0;
      if (strcmp_case(envi->hemisphere, "NORTH") == 0)
	meta->projection->param.utm.false_northing = 0.0;
      else
	meta->projection->param.utm.false_northing = 10000000.0;
      meta->projection->param.utm.lat0 = 0.0;
      meta->projection->param.utm.lon0 =
	(double) (envi->projection_zone - 1) * 6.0 - 177.0;
      meta->projection->param.utm.scale_factor = 0.9996;
    }
    else if (strncmp(envi->projection, "Polar Stereographic", 18)==0) {
      meta->projection->type = POLAR_STEREOGRAPHIC;
      meta->projection->param.ps.slat = envi->center_lat;
      meta->projection->param.ps.slon = envi->center_lon;
      meta->projection->param.ps.is_north_pole = (envi->center_lat > 0) ? 1 : 0;
      meta->projection->param.ps.false_easting = envi->false_easting;
      meta->projection->param.ps.false_northing = envi->false_northing;
    }
    else if (strncmp(envi->projection, "Albers Conical Equal Area", 25)==0) {
      meta->projection->type = ALBERS_EQUAL_AREA;
      meta->projection->param.albers.std_parallel1 = envi->standard_parallel1;
      meta->projection->param.albers.std_parallel2 = envi->standard_parallel2;
      meta->projection->param.albers.center_meridian = envi->center_lon;
      meta->projection->param.albers.orig_latitude = envi->center_lat;
      meta->projection->param.albers.false_easting = envi->false_easting;
      meta->projection->param.albers.false_northing = envi->false_northing;
    }
    else if (strncmp(envi->projection, "Lambert Conformal Conic", 23)==0) {
      meta->projection->type = LAMBERT_CONFORMAL_CONIC;
      meta->projection->param.lamcc.plat1 = envi->standard_parallel1;
      meta->projection->param.lamcc.plat2 = envi->standard_parallel2;
      meta->projection->param.lamcc.lat0 = envi->center_lat;
      meta->projection->param.lamcc.lon0 = envi->center_lon;
      meta->projection->param.lamcc.false_easting = envi->false_easting;
      meta->projection->param.lamcc.false_northing = envi->false_northing;
    } 
    else if (strncmp(envi->projection, "Lambert Azimuthal Equal Area", 28)==0) {
      meta->projection->type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
      meta->projection->param.lamaz.center_lat = envi->center_lat;
      meta->projection->param.lamaz.center_lon = envi->center_lon;
      meta->projection->param.lamaz.false_easting = envi->false_easting;
      meta->projection->param.lamaz.false_northing = envi->false_northing;
    }
    else if (strncmp(envi->projection, "Geographic Lat/Lon", 18)==0)
      meta->projection->type = LAT_LONG_PSEUDO_PROJECTION;
    else {
      sprintf(errbuf,"\n   ERROR: Unsupported projection type\n\n");
      printErr(errbuf);
    }
    meta->projection->startX = envi->pixel_easting;
    meta->projection->startY = envi->pixel_northing;
    meta->projection->perX = envi->proj_dist_x;
    meta->projection->perY = -envi->proj_dist_y;
    if (strncmp(envi->hemisphere, "North", 5)==0)
      meta->projection->hem = 'N';
    else if (strncmp(envi->hemisphere, "South", 5)==0)
      meta->projection->hem = 'S';
    if (strcmp(envi->datum, "North America 1927") == 0)
      meta->projection->datum = NAD27_DATUM;
    else if (strcmp(envi->datum, "North America 1983") == 0)
      meta->projection->datum = NAD83_DATUM;
    else if (strcmp(envi->datum, "European 1950") == 0)
      meta->projection->datum = ED50_DATUM;
    else if (strcmp(envi->datum, "WGS-72") == 0)
      meta->projection->datum = WGS72_DATUM;
    else if (strcmp(envi->datum, "WGS-84") == 0)
      meta->projection->datum = WGS84_DATUM;
    else if (strcmp(envi->datum, "Hughes") == 0)
      meta->projection->datum = HUGHES_DATUM;
    meta->projection->spheroid = datum_spheroid(meta->projection->datum);
    meta->projection->re_major = envi->semimajor_axis;
    meta->projection->re_minor = envi->semiminor_axis;
    if (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
      double re_major=MAGIC_UNSET_DOUBLE, re_minor=MAGIC_UNSET_DOUBLE;
      if (meta->projection->datum == WGS72_DATUM)
	datum2earth_radius(5, &re_major, &re_minor);
      else if (meta->projection->datum == WGS84_DATUM)
	datum2earth_radius(8, &re_major, &re_minor);
      meta->general->re_major = re_major;
      meta->general->re_minor = re_minor;
      meta->projection->re_major = re_major;
      meta->projection->re_minor = re_minor;
    }
    sprintf(meta->projection->units, "meters");
    double center_x = meta->projection->startX + 
      meta->general->sample_count/2.0 * meta->projection->perX;
    double center_y = meta->projection->startY +
      meta->general->line_count/2.0 * meta->projection->perY;
    double center_lat, center_lon, height;
    proj_to_latlon(meta->projection, center_x, center_y, 0.0,
		   &center_lat, &center_lon, &height);
    meta->general->center_latitude = center_lat * R2D;
    meta->general->center_longitude = center_lon * R2D;
    if (meta->projection->hem == MAGIC_UNSET_CHAR) {
      if (center_lat > 0.0)
	meta->projection->hem = 'N';
      else
	meta->projection->hem = 'S';
    }
  }
  if (meta->sar)
    meta->sar->wavelength = envi->wavelength;
  meta->general->x_pixel_size = envi->pixel_size_x;
  meta->general->y_pixel_size = envi->pixel_size_y;

  return meta;
}

// Write ENVI header file
void write_envi_header(const char *headerFile, const char *dataFile,
		       meta_parameters *meta, envi_header *envi)
{
  FILE *fp;
  time_t t;
  char t_stamp[15];
  char datum_str[64];

  if (meta->projection && strlen(envi->datum) > 0) {
    sprintf(datum_str, "%s", envi->datum);
  }

  t = time(NULL);
  strftime(t_stamp, 12, "%d-%b-%Y", localtime(&t));

  fp = FOPEN(headerFile, "w");
  fprintf(fp, "ENVI\n");
  fprintf(fp, "description = {\n  Generated by meta2envi (%s)}\n", t_stamp);
  fprintf(fp, "samples = %i\n", envi->samples);
  fprintf(fp, "lines   = %i\n", envi->lines);
  fprintf(fp, "bands   = %i\n", envi->bands);
  fprintf(fp, "header offset = %i\n", envi->header_offset);
  fprintf(fp, "file type = %s\n", envi->file_type);
  fprintf(fp, "data type = %i\n", envi->data_type);
  fprintf(fp, "interleave = %s\n", envi->interleave);
  fprintf(fp, "sensor type = %s\n", envi->sensor_type);
  fprintf(fp, "byte order = %i\n", envi->byte_order);
  if (meta->projection) {
    switch (meta->projection->type)
      {
      default:
        asfPrintError("Unsupported projection: %d\n", meta->projection->type);
        break;
      case LAT_LONG_PSEUDO_PROJECTION:
	fprintf(fp, 
		"map info = {%s, %d, %d, %.5f, %.5f, %f, %f, %s, units=Degrees}\n", 
		envi->projection, envi->ref_pixel_x, envi->ref_pixel_y,
		envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
		envi->proj_dist_y, datum_str);
	fprintf(fp, 
		"projection info = {1, %.3f, %.3f, 0.0, 0.0, %s}\n", 
		envi->semimajor_axis, envi->semiminor_axis, datum_str);
	break;
      case UNIVERSAL_TRANSVERSE_MERCATOR:
	fprintf(fp, 
		"map info = {%s, %i, %i, %.3f, %.3f, %.3f, %.3f, %i, %s, %s}\n", 
		envi->projection, envi->ref_pixel_x, envi->ref_pixel_y, 
		envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
		envi->proj_dist_y, envi->projection_zone, envi->hemisphere,
		datum_str);
	/* not required according to GDAL
	fprintf(fp, 
		"projection info = {3, %.3f, %.3f, %.4f, %.4f, " 
                "0.0, 0.0, 0.99996%s, %s}\n", 
		envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
		envi->center_lon, datum_str, envi->projection);
	*/
	break;
      case POLAR_STEREOGRAPHIC:
	fprintf(fp, 
		"map info = {%s, %i, %i, %.3f, %.3f, %.3f, %.3f, %s}\n", 
		envi->projection, envi->ref_pixel_x, envi->ref_pixel_y, 
		envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
		envi->proj_dist_y, datum_str);
	fprintf(fp, 
		"projection info = {31, %.3f, %.3f, %.4f, %.4f, " 
                "0.0, 0.0, %s, %s}\n", 
		envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
		envi->center_lon, datum_str, envi->projection);
	break;
      case ALBERS_EQUAL_AREA:
	fprintf(fp, 
		"map info = {%s, %i, %i, %.3f, %.3f, %.3f, %.3f, %s}\n", 
		envi->projection, envi->ref_pixel_x, envi->ref_pixel_y, 
		envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
		envi->proj_dist_y, datum_str);
	fprintf(fp, 
		"projection info = {9, %.3f, %.3f, %.4f, %.4f, " 
                "0.0, 0.0, %.4f, %.4f, %s, %s}\n", 
		envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
		envi->center_lon, envi->standard_parallel1, 
		envi->standard_parallel2, datum_str, envi->projection);
	break;
      case LAMBERT_CONFORMAL_CONIC:
	fprintf(fp, 
		"map info = {%s, %i, %i, %.3f, %.3f, %.3f, %.3f, %s}\n", 
		envi->projection, envi->ref_pixel_x, envi->ref_pixel_y, 
		envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
		envi->proj_dist_y, datum_str);
	fprintf(fp, 
		"projection info = {4, %.3f, %.3f, %.4f, %.4f, " 
                "0.0, 0.0, %.4f, %.4f, %s, %s}\n", 
		envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
		envi->center_lon, envi->standard_parallel1,
		envi->standard_parallel2, datum_str, envi->projection);
	break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	fprintf(fp, 
		"map info = {%s, %i, %i, %.3f, %.3f, %.3f, %.3f, %s}\n", 
		envi->projection, envi->ref_pixel_x, envi->ref_pixel_y, 
		envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
		envi->proj_dist_y, datum_str);
	fprintf(fp, 
		"projection info = {11, %.3f, %.3f, %.4f, %.4f, " 
                "0.0, 0.0, %s, %s}\n", 
		envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
		envi->center_lon, datum_str, envi->projection);
	break;
      case STATE_PLANE:
      case SCANSAR_PROJECTION: 
      case MERCATOR:
      case EQUI_RECTANGULAR:
      case UNKNOWN_PROJECTION:
	break;
      }
  }
  fprintf(fp, "wavelength units = %s\n", envi->wavelength_units);
  if (envi->band_name)
    fprintf(fp, "band names = {\n %s}\n", envi->band_name);
  /*** wavelength, data ignore and default stretch currently not used ***/
  FCLOSE(fp);

  return;
}
