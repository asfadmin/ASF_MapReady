#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "asf.h"
#include "asf_meta.h"
#include "libasf_proj.h"
#include "asf_geocode.h"

#include <ctype.h>

typedef int project_t(project_parameters_t *pps, double lat, double lon,
      double height, double *x, double *y, double *z, datum_type_t dtm);
typedef int unproject_t(project_parameters_t *pps, double x, double y,
      double z, double *lat, double *lon, double *height, datum_type_t dtm);

void usage()
{
  printf(
"Usage:\n"
"     asf_proj2proj <source proj file> <target proj file> <infile> <outfile>\n"
"\n"
"  source proj file: Searched for in the current directory, then the ASF\n"
"                    share dir.  Use ""latlon"" for the lat/lon projection.\n"
"  target proj file: Searched for in the current directory, then the ASF\n"
"                    share dir.  Use ""latlon"" for the lat/lon projection.\n"
"  infile:           File containing points to project.\n"
"  outfile:          Created by the tool, contains the projected data.\n"
"\n"
"The input file may use comma, semicolon, or whitespace to separate values.\n"
"The input file should contain one point (2 or 3 values, as below) per line.\n"
"The input file can use 2 or 3 values per line, the third value is the\n"
"height, 0 is used if only 2 values are present.\n\n");
  exit(1);
}

static int
project_lat_long_pseudo (project_parameters_t *pps, double lat, double lon,
       double height, double *x, double *y, double *z, datum_type_t datum)
{
  /* Silence compiler warning about unused argument.  */
  pps = pps; datum = datum;

  *y = lon * R2D;
  *x = lat * R2D;
  if (z) *z = height;

  return TRUE;
}

static int
project_lat_long_pseudo_inv (project_parameters_t *pps, double x, double y,
           double z, double *lat, double *lon,
           double *height, datum_type_t datum)
{
  /* Silence compiler warning about unused argument.  */
  pps = pps; datum = datum;

  *lon = y * D2R;
  *lat = x * D2R;
  if (height) *height = z;

  return TRUE;
}

static void determine_projection_fns(int projection_type,
                                     project_t **project,
                                     unproject_t **unproject)
{
  switch ( projection_type ) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      if (project) *project = project_utm;
      if (unproject) *unproject = project_utm_inv;
      break;
    case POLAR_STEREOGRAPHIC:
      if (project) *project = project_ps;
      if (unproject) *unproject = project_ps_inv;
      break;
    case ALBERS_EQUAL_AREA:
      if (project) *project = project_albers;
      if (unproject) *unproject = project_albers_inv;
      break;
    case LAMBERT_CONFORMAL_CONIC:
      if (project) *project = project_lamcc;
      if (unproject) *unproject = project_lamcc_inv;
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      if (project) *project = project_lamaz;
      if (unproject) *unproject = project_lamaz_inv;
      break;
    case LAT_LONG_PSEUDO_PROJECTION:
      if (project) *project = project_lat_long_pseudo;
      if (unproject) *unproject = project_lat_long_pseudo_inv;
      break;
    default:
      if (project) *project = NULL;
      if (unproject) *unproject = NULL;
      break;
  }
}

int main(int argc, char *argv[])
{
  if (argc!=5) usage();

  char buf[256];

  project_parameters_t source_pp, target_pp;
  projection_type_t source_proj, target_proj;
  datum_type_t source_datum, target_datum;

  const char *source_proj_file = argv[1];
  const char *target_proj_file = argv[2];
  const char *in_file = argv[3];
  const char *out_file = argv[4];

  char *err = NULL;
  if (strcmp_case(source_proj_file, "latlon")==0) {
    source_proj = LAT_LONG_PSEUDO_PROJECTION;
    source_datum = WGS84_DATUM;
  }
  else {
    parse_proj_args_file(source_proj_file, &source_pp, &source_proj,
                       &source_datum, &err);
    if (err)
      asfPrintError(err);
  }

  if (strcmp_case(target_proj_file, "latlon")==0) {
    target_proj = LAT_LONG_PSEUDO_PROJECTION;
    target_datum = WGS84_DATUM;
  }
  else {
    parse_proj_args_file(target_proj_file, &target_pp, &target_proj,
                       &target_datum, &err);
    if (err)
      asfPrintError(err);
  }

  if (!fileExists(in_file))
    asfPrintError("Input file not found: %s\n", in_file);

  project_t *source_proj_fn, *target_proj_fn;
  unproject_t *source_unproj_fn, *target_unproj_fn;
    
  determine_projection_fns(source_proj, &source_proj_fn, &source_unproj_fn);
  determine_projection_fns(target_proj, &target_proj_fn, &target_unproj_fn);

  FILE *ifp = FOPEN(in_file, "r");

  FILE *ofp;
  if (strcmp_case(out_file,"stdout")==0)
    ofp = stdout;
  else
    ofp = FOPEN(out_file, "w");

  int line_num = 1;
  int valid_lines = 0;

  while (NULL!=fgets(buf,255,ifp)) {

    // strip trailing whitespace
    while (isspace(buf[strlen(buf)-1]))
        buf[strlen(buf)-1]='\0';

    // line too long?
    int len = strlen(buf);
    if (len>200) {
      asfPrintStatus("Lengthy line %d ignored.\n", line_num);
      fprintf(ofp, "%s\n", buf); // write what we got of the line
    }

    // blank line?  pass through without comment
    else if (len==0) {
      fprintf(ofp, "\n");
    }

    // normal line-- try to parse 2/3 floating point numbers
    else {
      double x, y, z=0;
      char *p2,*p3,*p4;
      x = strtod(buf, &p2);
      if (errno==EINVAL) {
        asfPrintStatus("Invalid line %d ignored.\n", line_num);
        fprintf(ofp, "%s\n", buf);
      } 
      else {
        if (*p2==',' || *p2==';') ++p2;
        y = strtod(p2, &p3);
        if (errno==EINVAL || p2==p3) {
          asfPrintStatus("Invalid line %d ignored.\n", line_num);
          fprintf(ofp, "%s\n", buf);
        }
        else {
          if (*p3==',' || *p3==';') ++p3;
          if (*p3!='\0')
            z = strtod(p3, &p4);
          
          //printf("Parsed values: x=%f, y=%f, z=%f\n", x, y, z);
          
          // now... project!
          // first to lat/lon, then to target projection
          double lat, lon, ht;
          source_unproj_fn(&source_pp, x, y, z, &lat, &lon, &ht,
                           source_datum);
          
          //printf("Lat/Lon: %f %f\n", lat, lon);
          
          if (valid_lines==0 &&
              target_proj==UNIVERSAL_TRANSVERSE_MERCATOR && 
              target_pp.utm.zone==0)
          {
            fill_in_utm(lat*R2D, lon*R2D, &target_pp);
            asfPrintStatus("Zone: %d\n", target_pp.utm.zone);
          }
          
          target_proj_fn(&target_pp, lat, lon, ht, &x, &y, &z,
                         target_datum);
          
          //printf("Projected: x=%f, y=%f, z=%f\n", x, y, z);
          // use different accuracies for lat/lon vs. projected
          if (target_proj==LAT_LONG_PSEUDO_PROJECTION)
            fprintf(ofp, "%.4f %.4f %.2f\n", x, y, z);
          else
            fprintf(ofp, "%.2f %.2f %.2f\n", x, y, z);

          ++valid_lines;
        }
      }
    }
    ++line_num;
  }

  fclose(ifp);
  fclose(ofp);

  if (valid_lines==0)
    asfPrintStatus("No valid lines found in %s.\n", in_file);
  else
    asfPrintStatus("Done.  Projected %d point%s from %s.\n", valid_lines,
                   valid_lines==1?"":"s", in_file);

  return valid_lines>0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
