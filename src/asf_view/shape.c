#include "asf_view.h"
#include "asf_vector.h"
#include <assert.h>

Shape **g_shapes = NULL;
int num_shapes = 0;

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
    int ok = csv_line_parse(line,
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
    int ok = csv_line_parse(line,
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
    s->color_code = num + 10;
    s->marker_code = 1; // square

    g_shapes[num] = s;
    ++num;

    csv_free(num_meta_cols, column_data, lats, lons);
  }
  FCLOSE(ifp);
  FREE(data_column_info);
}

void add_delta_shapes(meta_parameters *meta)
{
  const char *file = "corner_reflectors.csv";
  char *crf = find_in_share(file);
  add_generic_csv(meta,crf,TRUE);
  free(crf);
}
