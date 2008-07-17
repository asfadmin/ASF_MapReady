#include "asf_view.h"
#include "shapefil.h"
#include "asf_vector.h"
#include <assert.h>

Shape **g_shapes = NULL;
int num_shapes = 0;

void free_shapes()
{
  if (g_shapes) {
    int i,j;
    for (i=0; i<num_shapes; ++i) {
      if (g_shapes[i]) {
        if (g_shapes[i]->num_meta_cols>0) {
          if (i==0)
            FREE(g_shapes[i]->meta_cols);
          for (j=0;j<g_shapes[i]->num_meta_cols;++j)
            FREE(g_shapes[i]->meta_info[j]);
          FREE(g_shapes[i]->meta_info);
        } 
        FREE(g_shapes[i]->lines);
        FREE(g_shapes[i]->samps);
      }
      FREE(g_shapes[i]);
    }
    FREE(g_shapes);
  }
}

static void add_generic_csv(meta_parameters *meta, const char *csv_file,
                            int auto_close_polys)
{
  int num_meta_cols, num_data_cols;
  csv_meta_column_t *meta_column_info;
  csv_data_column_t *data_column_info;

  if (!meta_supports_meta_get_latLon(meta)) {
    asfPrintStatus("No geolocation info - can't add CSV: %s\n", csv_file);
    return;
  }

  asfPrintStatus("Adding: %s\n", csv_file);

  if (!fileExists(csv_file)) {
    asfPrintWarning("File not found: %s\n", csv_file);
    return;
  }

  FILE *ifp = csv_open(csv_file,
                      &num_meta_cols, &meta_column_info,
                      &num_data_cols, &data_column_info);

  // csv_open() returns NULL if the file can't be processed
  if (!ifp)
    return;

  // this is just for debugging
  //csv_info(num_meta_cols, meta_column_info, num_data_cols, data_column_info);

  // start line counter at 1 (header line is not part of this loop)
  int i,line_num=1;
  int num = 0;

  char line[1024];
  while (fgets(line, 1023, ifp)) {
    ++line_num;

    char **column_data;
    double *lats, *lons;
    int ok = csv_line_parse(line, line_num,
                            num_meta_cols, meta_column_info,
                            num_data_cols, data_column_info,
                            &column_data, &lats, &lons);

    // csv_line_parse() will return FALSE when the line is invalid
    if (!ok)
      continue;

    ++num;

    csv_free(num_meta_cols, column_data, lats, lons);
  }
  FCLOSE(ifp);

  FREE(meta_column_info);
  meta_column_info = NULL;
  FREE(data_column_info);
  data_column_info = NULL;

  // Use line_num-1 so we do not count the header line
  asfPrintStatus("File had %d line%s, %d valid line%s.\n",
                 line_num-1, line_num-1==1?"":"s", num, num==1?"":"s");

  // free pre-existing loaded shapes
  if (g_shapes)
    free_shapes();

  // set up global array of shapes
  num_shapes = num;
  g_shapes = MALLOC(sizeof(Shape*)*num_shapes);
  for (i=0; i<num_shapes; ++i)
    g_shapes[i] = NULL;

  // now open file again, this time we will actually process!
  ifp = csv_open(csv_file, &num_meta_cols, &meta_column_info,
                           &num_data_cols, &data_column_info);
  assert(ifp);
  line_num = 1;
  num = 0;

  while (fgets(line, 1023, ifp)) {
    ++line_num;

    char **column_data;
    double *lats, *lons;
    int ok = csv_line_parse(line, line_num,
                            num_meta_cols, meta_column_info,
                            num_data_cols, data_column_info,
                            &column_data, &lats, &lons);

    // csv_line_parse() will return FALSE when the line is invalid
    if (!ok)
      continue;

    // dealing with metadata
    Shape *s = MALLOC(sizeof(Shape));
    s->num_meta_cols = num_meta_cols;
    s->meta_cols = meta_column_info;
    s->meta_info = MALLOC(sizeof(char*)*num_meta_cols);

    for (i=0; i<num_meta_cols; ++i)
      s->meta_info[i] = STRDUP(column_data[i]);

    // dealing with data
    s->num_points = num_data_cols;
    if (auto_close_polys)
      s->num_points++;

    s->lines = MALLOC(sizeof(double)*s->num_points);
    s->samps = MALLOC(sizeof(double)*s->num_points);

    for (i=0; i<num_data_cols; ++i) {
      double line, samp;
      meta_get_lineSamp(meta, lats[i], lons[i], 0, &line, &samp);
      s->lines[i] = line;
      s->samps[i] = samp;
    }

    if (auto_close_polys) {
      s->lines[num_data_cols] = s->lines[0];
      s->samps[num_data_cols] = s->samps[0];
    }

    // display info
    s->color_code = num%27 + 10;
    s->marker_code = 1; // square

    g_shapes[num] = s;
    ++num;

    csv_free(num_meta_cols, column_data, lats, lons);
  }
  FCLOSE(ifp);
  FREE(data_column_info);
  // do not free meta_column_info -- pointed to by g_shape now
}

void add_delta_shapes(meta_parameters *meta)
{
  const char *file = "corner_reflectors.csv";
  char *crf = find_in_share(file);
  add_generic_csv(meta,crf,TRUE);
  free(crf);
}

static void add_shapefile(meta_parameters *meta, char *inFile)
{
  if (!meta_supports_meta_get_latLon(meta)) {
    asfPrintStatus("No geolocation info - can't add shapefile: %s\n", inFile);
    return;
  }

  asfPrintStatus("Adding: %s\n", inFile);

  if (!fileExists(inFile)) {
    asfPrintWarning("File not found: %s\n", inFile);
    return;
  }

  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject;
  int ii, kk, nEntities, nVertices, pointType;

  // Open shapefile
  open_shape(inFile, &dbase, &shape);

  // Extract the vital information out of the shapefile
  SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);
  switch (pointType) {
  case SHPT_POLYGON:
  case SHPT_POINT:
    break;
  case SHPT_ARC:
    //asfPrintWarning("Shape file data type 'Arc' not supported\n");
    //return;
    break;
  case SHPT_MULTIPOINT:
    asfPrintWarning("Shape file data type 'Multipoint' not supported\n");
    return;
  default:
    asfPrintWarning("Unexpected or unrecognized shape file data format\n");
    return;
  }

  asfPrintStatus("  Shapefile contains %d shapes.\n", nEntities);

  // free pre-existing loaded shapes
  if (g_shapes)
    free_shapes();

  // first figure out how many shapes we will actually be showing
  double lat_min, lat_max, lon_min, lon_max;
  meta_get_bounding_box(meta, &lat_min, &lat_max, &lon_min, &lon_max);

  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;

  int num_shapes_visible = 0;
  for (ii=0; ii<nEntities; ii++) {

    // Read object for the number of vertices
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices-1;

    for (kk=0; kk<nVertices; ++kk) {
      double lat = shapeObject->padfY[kk];
      double lon = shapeObject->padfX[kk];

      if (lon<-180) lon+=360;
      else if (lon>180) lon-=360;

      if (lat<lat_min || lat>lat_max || lon<lon_min || lon>lon_max)
        continue;

      double line, samp;
      meta_get_lineSamp(meta, lat, lon, 0, &line, &samp);
      if (line<0 || line>nl || samp<0 || samp>ns)
        continue;

      ++num_shapes_visible;
      break;
    }

    SHPDestroyObject(shapeObject);
  }

  if (num_shapes_visible == 0) {
    asfPrintWarning("Shapefile contained no shapes within the scene.\n");
    num_shapes = 0;
    g_shapes = NULL;
    return;
  }

  asfPrintStatus("  ... visible shapes: %d\n", num_shapes_visible);

  // set up global array of shapes
  num_shapes = num_shapes_visible;
  g_shapes = MALLOC(sizeof(Shape*)*num_shapes);
  for (ii=0; ii<num_shapes; ++ii)
    g_shapes[ii] = NULL;

  int num=0,nverts=0;
  for (ii=0; ii<nEntities; ii++) {

    // Read object for the number of vertices
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices;

    int num_visible_points = 0;
    for (kk=0; kk<nVertices; ++kk) {
      double lat = shapeObject->padfY[kk];
      double lon = shapeObject->padfX[kk];

      if (lon<-180) lon+=360;
      else if (lon>180) lon-=360;

      if (lat<lat_min || lat>lat_max || lon<lon_min || lon>lon_max)
        continue;

      double line, samp;
      meta_get_lineSamp(meta, lat, lon, 0, &line, &samp);
      if (line<0 || line>nl || samp<0 || samp>ns)
        continue;

      ++num_visible_points;
    }

    if (num_visible_points > 0) {
      // there is no metadata
      Shape *s = MALLOC(sizeof(Shape));
      s->num_meta_cols = 0;
      s->meta_cols = NULL;
      s->meta_info = NULL;

      // dealing with data
      s->num_points = num_visible_points;

      s->lines = MALLOC(sizeof(double)*s->num_points);
      s->samps = MALLOC(sizeof(double)*s->num_points);

      int nn=0;
      for (kk=0; kk<nVertices; ++kk) {
        double lat = shapeObject->padfY[kk];
        double lon = shapeObject->padfX[kk];

        if (lon<-180) lon+=360;
        else if (lon>180) lon-=360;

        if (lat<lat_min || lat>lat_max || lon<lon_min || lon>lon_max)
          continue;

        double line, samp;
        meta_get_lineSamp(meta, lat, lon, 0, &line, &samp);
        if (line<0 || line>nl || samp<0 || samp>ns)
          continue;

        assert(nn<s->num_points);
        s->lines[nn] = line;
        s->samps[nn] = samp;
        ++nn;
        ++nverts;
      }
      assert(nn==num_visible_points);

      // display info
      s->color_code = 35;
      s->marker_code = 0; // no markers, just draw the lines

      assert(num<num_shapes_visible);
      assert(num<num_shapes);
      g_shapes[num] = s;
      ++num;
    }

    SHPDestroyObject(shapeObject);
  }
  assert(num==num_shapes);
  
  asfPrintStatus("  ... visible vertices: %d\n", nverts);

  // Close shapefile
  close_shape(dbase, shape);
}

void add_global_coast(meta_parameters *meta)
{
  const char *file = "gshhs.shp";
  char *gc = find_in_share(file);
  add_shapefile(meta,gc);
  free(gc);
}
