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

#include "common.c"

void usage()
{
  printf(
"Usage:\n"
"     asf_proj2proj <source proj file> <target proj file> <infile> <outfile>\n"
"\n"
"  source proj file: Searched for in the current directory, then the ASF\n"
"                    share dir.  Use \"latlon\" for the lat/lon projection.\n"
"  target proj file: Searched for in the current directory, then the ASF\n"
"                    share dir.  Use \"latlon\" for the lat/lon projection.\n"
"  infile:           File containing points to project.\n"
"  outfile:          Created by the tool, contains the projected data.\n"
"                    Use \"stdout\" to have the output sent to stdout.\n"
"\n"
"The input file may use comma, semicolon, or whitespace to separate values.\n"
"The input file should contain one point (2 or 3 values, as below) per line.\n"
"The input file can use 2 or 3 values per line, the third value is the\n"
"height, 0 is used if only 2 values are present.\n"
"\n"
  );
  exit(1);
}

int main(int argc, char *argv[])
{
  if (argc!=5) usage();

  char buf[256];

  project_parameters_t source_pp, target_pp;
  projection_type_t source_proj, target_proj;
  datum_type_t source_datum, target_datum;
  spheroid_type_t source_spheroid, target_spheroid;

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
			 &source_datum, &source_spheroid, &err);
    if (err)
      asfPrintError(err);
  }

  if (strcmp_case(target_proj_file, "latlon")==0) {
    target_proj = LAT_LONG_PSEUDO_PROJECTION;
    target_datum = WGS84_DATUM;
  }
  else {
    parse_proj_args_file(target_proj_file, &target_pp, &target_proj,
			 &target_datum, &target_spheroid, &err);
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

    // blank line?  comment line?  pass through without comment
    else if (len==0 || buf[0]=='#') {
      fprintf(ofp, "%s\n", buf);
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
