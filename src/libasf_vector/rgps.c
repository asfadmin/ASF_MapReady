#include "asf_vector.h"
#include "asf.h"

// Convert RGPS cell information to kml
void rgps2kml(cell_t cell, double *lat, double *lon, FILE *fp)
{
  int ii;

  // Write information in kml file
  fprintf(fp, "<Placemark>\n");
  fprintf(fp, "  <description><![CDATA[\n");
  fprintf(fp, "<strong>Cell</strong>: %ld<br>\n", cell.cell_id);
  fprintf(fp, "<strong>Vertices</strong>: %d<br>\n", cell.nVertices);
  for (ii=0; ii<cell.nVertices; ii++) {
    fprintf(fp, "<strong>%d</strong> - ", ii+1);
    fprintf(fp, "<strong>Lat</strong>: %9.4f, ", lat[ii]);
    fprintf(fp, "<strong>Lon</strong>: %9.4f<br>\n", lon[ii]);
  }
  fprintf(fp, "<strong>Date</strong>: %s<br>\n", cell.date);
  fprintf(fp, "<strong>Source image</strong>: %s<br>\n", cell.sourceImage);
  fprintf(fp, "<strong>Target image</strong>: %s<br>\n", cell.targetImage);
  fprintf(fp, "<strong>Stream</strong>: %s<br>\n", cell.stream);
  fprintf(fp, "<strong>Area</strong>: %.1lf<br>\n", cell.area);
  fprintf(fp, "<strong>Multi-year ice</strong>: %.3lf<br>\n",
      cell.multi_year_ice);
  fprintf(fp, "<strong>Open water</strong>: %.3lf<br>\n", cell.open_water);
  fprintf(fp, "<strong>Incidence angle</strong>: %.4lf<br>\n",
      cell.incidence_angle);
  fprintf(fp, "<strong>Cell x</strong>: %.3lf<br>\n", cell.cell_x);
  fprintf(fp, "<strong>Cell y</strong>: %.3lf<br>\n", cell.cell_y);
  fprintf(fp, "<strong>dudx</strong>: %.4lf<br>\n", cell.dudx);
  fprintf(fp, "<strong>dudy</strong>: %.4lf<br>\n", cell.dudy);
  fprintf(fp, "<strong>dvdx</strong>: %.4lf<br>\n", cell.dvdx);
  fprintf(fp, "<strong>dvdy</strong>: %.4lf<br>\n", cell.dvdy);
  fprintf(fp, "<strong>dtp</strong>: %.4lf<br>\n", cell.dtp);
  fprintf(fp, "<strong>temperature</strong>: %.1lf<br>\n", cell.temperature);
  fprintf(fp, "<strong>u wind</strong>: %.3lf<br>\n", cell.u_wind);
  fprintf(fp, "<strong>v wind</strong>: %.3lf<br>\n", cell.v_wind);
  fprintf(fp, "]]></description>\n");
  fprintf(fp, "<name>%ld</name>\n", cell.cell_id);
  fprintf(fp, "<LookAt>\n");
  fprintf(fp, "<longitude>%.4lf</longitude>\n", lon[0]);
  fprintf(fp, "<latitude>%.4lf</latitude>\n", lat[0]);
  fprintf(fp, "<range>400000</range>\n");
  fprintf(fp, "<heading>-90</heading>\n");
  fprintf(fp, "</LookAt>\n");
  fprintf(fp, "<visibility>1</visibility>\n");
  fprintf(fp, "<open>1</open>\n");
  write_kml_style_keys(fp);
  fprintf(fp, "<Polygon>\n");
  fprintf(fp, "<extrude>1</extrude>\n");
  fprintf(fp, "<altitudeMode>%s</altitudeMode>\n", altitude_mode());
  fprintf(fp, "<outerBoundaryIs>\n");
  fprintf(fp, "<LinearRing>\n");
  fprintf(fp, "<coordinates>\n");

  for (ii=0; ii<=cell.nVertices; ii++)
    fprintf(fp, "%.12f,%.12f,7000\n", lon[ii], lat[ii]);

  fprintf(fp, "</coordinates>\n");
  fprintf(fp, "</LinearRing>\n");
  fprintf(fp, "</outerBoundaryIs>\n");
  fprintf(fp, "</Polygon>\n");
  fprintf(fp, "</Placemark>\n");

  return;
}

void rgps_grid2kml(grid_attr_t grid, FILE *fp)
{
  // Write information in kml file
  fprintf(fp, "<Placemark>\n");
  fprintf(fp, "<description><![CDATA[\n");
  fprintf(fp, "<strong>Grid ID</strong>: %ld<br>\n", grid.grid_id);
  fprintf(fp, "<strong>Date</strong>: %s<br>\n", grid.date);
  fprintf(fp, "<strong>Day of the year</strong>: %.4lf<br>\n", grid.day);
  fprintf(fp, "<strong>Grid x</strong>: %.3lf<br>\n", grid.grid_x);
  fprintf(fp, "<strong>Grid y</strong>: %.3lf<br>\n", grid.grid_y);
  fprintf(fp, "<strong>Source image</strong>: %s<br>\n", grid.sourceImage);
  fprintf(fp, "<strong>Target image</strong>: %s<br>\n", grid.targetImage);
  fprintf(fp, "<strong>Stream</strong>: %s<br>\n", grid.stream);
  fprintf(fp, "<strong>Quality</strong>: %d<br>\n", grid.quality);
  fprintf(fp, "]]></description>\n");
  fprintf(fp, "<LookAt>\n");
  fprintf(fp, "<longitude>%9.4f</longitude>\n", grid.lon);
  fprintf(fp, "<latitude>%9.4f</latitude>\n", grid.lat);
  fprintf(fp, "<range>400000</range>\n");
  fprintf(fp, "</LookAt>\n");
  fprintf(fp, "<Style>\n");
  fprintf(fp, "<IconStyle>\n");
  fprintf(fp, "<scale>0.5</scale>\n");
  fprintf(fp, "<Icon>\n");
  fprintf(fp, "<href>grid_point.png</href>\n");
  fprintf(fp, "</Icon>\n");
  fprintf(fp, "</IconStyle>\n");
  fprintf(fp, "</Style>\n");
  fprintf(fp, "<Point>\n");
  fprintf(fp, "<coordinates>%f,%f,0</coordinates>\n", grid.lon, grid.lat);
  fprintf(fp, "</Point>\n");
  fprintf(fp, "</Placemark>\n");
}

// Convert RGPS cell to shape
void rgps2shape(cell_t cell, double *lat, double *lon, int vertices,
        DBFHandle dbase, SHPHandle shape, int n)
{
  // Write information into database file
  DBFWriteIntegerAttribute(dbase, n, 0, cell.cell_id);
  DBFWriteIntegerAttribute(dbase, n, 1, cell.nVertices);
  DBFWriteStringAttribute(dbase, n, 2, cell.date);
  DBFWriteStringAttribute(dbase, n, 3, cell.sourceImage);
  DBFWriteStringAttribute(dbase, n, 4, cell.targetImage);
  DBFWriteStringAttribute(dbase, n, 5, cell.stream);
  DBFWriteDoubleAttribute(dbase, n, 6, cell.area);
  DBFWriteDoubleAttribute(dbase, n, 7, cell.multi_year_ice);
  DBFWriteDoubleAttribute(dbase, n, 8, cell.open_water);
  DBFWriteDoubleAttribute(dbase, n, 9, cell.incidence_angle);
  DBFWriteDoubleAttribute(dbase, n, 10, cell.cell_x);
  DBFWriteDoubleAttribute(dbase, n, 11, cell.cell_y);
  DBFWriteDoubleAttribute(dbase, n, 12, cell.dudx);
  DBFWriteDoubleAttribute(dbase, n, 13, cell.dudy);
  DBFWriteDoubleAttribute(dbase, n, 14, cell.dvdx);
  DBFWriteDoubleAttribute(dbase, n, 15, cell.dvdy);
  DBFWriteDoubleAttribute(dbase, n, 16, cell.dtp);
  DBFWriteDoubleAttribute(dbase, n, 17, cell.temperature);
  DBFWriteDoubleAttribute(dbase, n, 18, cell.u_wind);
  DBFWriteDoubleAttribute(dbase, n, 19, cell.v_wind);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, vertices+1,
                      lon, lat, NULL);
  if (shapeObject == NULL)
    asfPrintError("Could not create shape object (%d)\n", n);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  return;
}

// Convert RGPS grid points to shape
void rgps_grid2shape(grid_attr_t grid, DBFHandle dbase, SHPHandle shape, int n)
{
  // Write information into database file
  DBFWriteIntegerAttribute(dbase, n, 0, grid.grid_id);
  DBFWriteStringAttribute(dbase, n, 1, grid.date);
  DBFWriteDoubleAttribute(dbase, n, 2, grid.day);
  DBFWriteDoubleAttribute(dbase, n, 3, grid.grid_x);
  DBFWriteDoubleAttribute(dbase, n, 4, grid.grid_y);
  DBFWriteStringAttribute(dbase, n, 5, grid.sourceImage);
  DBFWriteStringAttribute(dbase, n, 6, grid.targetImage);
  DBFWriteStringAttribute(dbase, n, 7, grid.stream);
  DBFWriteIntegerAttribute(dbase, n, 8, grid.quality);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POINT, 1,
                      &grid.lon, &grid.lat, NULL);
  if (shapeObject == NULL)
    asfPrintError("Could not create shape object (%d)\n", n);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  return;
}

// Convert RGPS weather data to shape
void rgps_weather2shape(char *line, DBFHandle dbase, SHPHandle shape, int n)
{
  double lat, lon, direction, speed, temperature, pressure;
  char date[15], *p;

  // Read weather information;
  p = strchr(line, ',');
  if (p) {
    sscanf(p+1, "%lf,%lf,%lf,%lf,%lf,%lf",
       &lat, &lon, &direction, &speed, &temperature, &pressure);
    *p = 0;
    sprintf(date, "%s", line);
  }

  // Write information into database file
  DBFWriteStringAttribute(dbase, n, 0, date);
  DBFWriteDoubleAttribute(dbase, n, 1, lat);
  DBFWriteDoubleAttribute(dbase, n, 2, lon);
  DBFWriteDoubleAttribute(dbase, n, 3, direction);
  DBFWriteDoubleAttribute(dbase, n, 4, speed);
  DBFWriteDoubleAttribute(dbase, n, 5, temperature);
  DBFWriteDoubleAttribute(dbase, n, 6, pressure);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POINT, 1, &lon, &lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  return;
}
