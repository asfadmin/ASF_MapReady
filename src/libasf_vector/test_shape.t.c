#include <stdio.h>
#include "asf.h"
#include "asf_vector.h"

void gi_antenna_shapefile()
{
  FILE *fp;
  double lat, lon;
  char id[255], pointFile[255];
    
  // Assign some values
  lat=64.8599; 
  lon=-147.8473;
  sprintf(id, "GI");
  sprintf(pointFile, "gi_antenna.txt");

  // Create a point file with location of GI antenna
  fp = FOPEN(pointFile, "w");
  fprintf(fp, "%s %.4lf %.4lf\n", id, lat, lon);
  printf("%s %.4lf %.4lf\n", id, lat, lon);
  FCLOSE(fp);
  
  // Create a shapefile from point file
  write_point_shapefile("test_write", "gi_antenna.txt");
  write_point_shapefile("gi_antenna", "gi_antenna.txt");
}

void read_antenna_shapefile()
{
  FILE *fp;
  double lat, lon;
  char id[255], pointFile[255];

  sprintf(pointFile, "test_read.txt");

  // Read shapefile
  read_shapefile("gi_antenna", pointFile);

  // Read point file and dump it on screen
  fp = FOPEN(pointFile, "r");
  fscanf(fp, "%s\t%lf\t%lf", id, &lat, &lon);
  printf("ID: %s, Lat: %.4lf, Lon: %.4lf\n", id, lat, lon);
  FCLOSE(fp);
}

void cook_inlet_polygon()
{
  FILE *fp;
  double lat[4], lon[4];
  int i;

  // Assign some numbers
  lat[0]=62.5;lat[1]=62.5;lat[2]=55.0;lat[3]=55.0;
  lon[0]=-160.0;lon[1]=-147.5;lon[2]=-147.5;lon[3]=-160.0;

  //  Generate point file
  fp = FOPEN("cook_inlet.txt", "w");
  for (i=0; i<4; i++) {
    fprintf(fp, "%d\t%.4lf\t%.4lf\n", i+1, lat[i], lon[i]);
    printf("%d %.4lf %.4lf\n", i+1, lat[i], lon[i]);
  }
  FCLOSE(fp);

  // Create shapefile
  write_polygon_shapefile("cook_inlet", "cook_inlet.txt", "Cook Inlet polygon");
}

void delta_test_polygon()
{
  FILE *fp;
  double lat[4], lon[4];
  int i;

  // Assign some numbers
  lat[0]=63.415;lat[1]=63.440;lat[2]=63.455;lat[3]=63.420;
  lon[0]=-144.535;lon[1]=-144.500;lon[2]=-144.565;lon[3]=-144.570;

  //  Generate point file
  fp = FOPEN("delta_test.txt", "w");
  for (i=0; i<4; i++) {
    fprintf(fp, "%d\t%.4lf\t%.4lf\n", i+1, lat[i], lon[i]);
    printf("%d %.4lf %.4lf\n", i+1, lat[i], lon[i]);
  }
  FCLOSE(fp);

  // Create shapefile
  write_polygon_shapefile("delta_polygon", "delta_test.txt", "Delta polygon test");
}

void read_delta_polygon()
{
  FILE *fp;
  double lat, lon;
  char id[25], line[255], pointFile[255];

  sprintf(pointFile, "delta_polygon.txt");

  // Read shapefile
  read_shapefile("delta_polygon", pointFile);

  // Read point file and dump it on screen
  fp = FOPEN(pointFile, "r");
  while (fgets(line, 255, fp) != NULL) {
    sscanf(line, "%s\t%lf\t%lf", id, &lat, &lon);
    printf("ID: %s, Lat: %.4lf, Lon: %.4lf\n", id, lat, lon);
  }
  FCLOSE(fp);
}

void create_delta_mask()
{
  create_mask("delta", "delta_polygon", "delta_mask");
}

void invert_delta_mask()
{
  invert_mask("delta_mask.img", "delta_mask_invert.img");
}

int main(int argc, char * argv [])
{
  printf("\nGenerating GI antenna shapefile ...\n");
  gi_antenna_shapefile();
  printf("\nReading GI antenna shapefile ...\n");
  read_antenna_shapefile(); 
  printf("\nGenerating Delta polygon shapefile ...\n");
  delta_test_polygon();
  printf("\nReading polygon shapefile ...\n");
  read_delta_polygon();
  printf("\nGenerating mask file for Delta ...\n");
  create_delta_mask();
  printf("\nInverting mask file for Delta ...\n");
  invert_delta_mask();
  printf("\nGenerating Cook Inlet polygon shapefile ...\n");
  cook_inlet_polygon();

  printf("\n*** Test completed ****\n\n");

  return 0;
}
