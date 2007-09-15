#include "asf_vector.h"
#include "asf_meta.h"
#include "asf.h"

#define VERSION 1.0

void usage()
{
  printf("\n"
   "USAGE:\n"
   "   multimatch2vector <multimatch> <shape>\n");
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   multimatch    Text file output from JPL's multimatch program\n"
   "   shape         Basename of the output file.\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program converts information from a multimatch output file into\n"
   "   ArcGIS shape files.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  FILE *fp;
  DBFHandle dbase;
  SHPHandle shape;
  char multimatchFile[255], shapeFile[255], line[512];
  float ref_x, ref_y, ref_z, ref_x_pixel_size, ref_y_pixel_size, ref_start_x;
  float ref_start_y, search_x, search_y, search_z, search_x_pixel_size;
  float search_y_pixel_size, search_start_x, search_start_y, dx, dy, dz;
  float speed, direction;
  double lat, lon;
  int n=0, ref_utm_zone, search_utm_zone;
  
  if (argc != 3)
    usage();

  strcpy(multimatchFile, argv[1]);
  strcpy(shapeFile, argv[2]);
	 
  asfSplashScreen(argc, argv);
  
  // Open shop for business
  shape_init(shapeFile, MULTIMATCH);
  open_shape(shapeFile, &dbase, &shape);
  fp = FOPEN(multimatchFile, "r");

  // Reference data header
  fgets(line, 512, fp); // Data file type
  fgets(line, 512, fp); // Data file dimensions
  fgets(line, 512, fp); // Post spacing
  sscanf(line, "%f %f", &ref_x_pixel_size, &ref_y_pixel_size);
  fgets(line, 512, fp); // Starting corner position (s,c)
  sscanf(line, "%f %f", &ref_start_x, &ref_start_y);
  fgets(line, 512, fp); // Peg position (WGS-84)
  fgets(line, 512, fp); // UTM zone
  sscanf(line, "%d", &ref_utm_zone);
  fgets(line, 512, fp); // blank line for value

  // Search data header
  fgets(line, 512, fp); // Data file type
  fgets(line, 512, fp); // Data file dimensions
  fgets(line, 512, fp); // Post spacing
  sscanf(line, "%f %f", &search_x_pixel_size, &search_y_pixel_size);
  fgets(line, 512, fp); // Starting corner position (s,c)
  sscanf(line, "%f %f", &search_start_x, &search_start_y);
  fgets(line, 512, fp); // Peg position (WGS-84)
  fgets(line, 512, fp); // UTM zone
  sscanf(line, "%d", &search_utm_zone);
  fgets(line, 512, fp); // blank line for value

  // Read matching information
  while (fgets(line, 512, fp)) {
    sscanf(line, "%f %f %f %f %f %f %f %f %f",
	   &ref_x, &ref_y, &ref_z, &search_x, &search_y, &search_z,
	   &dx, &dy, &dz);
    UTM2latLon(ref_start_x + ref_x_pixel_size*ref_x, 
	       ref_start_y + ref_y_pixel_size*ref_y, 
	       ref_z, ref_utm_zone, &lat, &lon);
    speed = sqrt(dx*dx + dy*dy);
    direction = atan2(dx, dy)*R2D;
    if (direction < 0)
      direction += 360.0;
    sprintf(line, "%.4lf,%.4lf,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.3f,%.3f,%3.f,"
	    "%.4f,%.1f", lat, lon, ref_x, ref_y, ref_z, search_x, search_y, 
	    search_z, dx, dy, dz, direction, speed);
    multimatch2shape(line, dbase, shape, n);
    n++;
  }
  FCLOSE(fp);

  // Clean up
  close_shape(dbase, shape);
  write_esri_proj_file(shapeFile);
  
  return(0);
}
