#include "winshl.h"
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
          if (i==0) // meta_cols info is shared among the shapes
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

// This is a REALLY BASIC kml parser...
/*
static void add_kml(meta_parameters *meta, const char *kml_file)
{
  if (!meta_supports_meta_get_latLon(meta)) {
    asfPrintStatus("No geolocation info - can't add kml file: %s\n", kml_file);
    return;
  }

  asfPrintStatus("Adding: %s\n", kml_file);

  if (!fileExists(kml_file)) {
    asfPrintWarning("File not found: %s\n", kml_file);
    return;
  }

  // free pre-existing loaded shapes
  if (g_shapes)
    free_shapes();

  asfPrintError("Not implemented yet.\n");
}
*/

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

static void add_overlay_file(char *overlay_file)
{
  char *ext = findExt(overlay_file);

  if (ext) {
    if (strcmp_case(ext, ".SHP") == 0) {
      add_shapefile(curr->meta, overlay_file);
      fill_big(curr);
      return;
    }
    else if (strcmp_case(ext, ".CSV") == 0) {
      add_generic_csv(curr->meta, overlay_file, FALSE);
      fill_big(curr);
      return;
    }
  }

  // if we got here, must have failed to find a handler
  message_box("Do not know how to handle the overlay file: %s", overlay_file);
}

#ifdef HAVE_DELTA_CR
void add_delta_shapes(meta_parameters *meta)
{
  const char *file = "corner_reflectors.csv";
  char *crf = find_in_share(file);
  add_generic_csv(meta,crf,TRUE);
  free(crf);
}
#endif

//----------------------------------------------------------------------------
// The rest of this file is the "open file" dialog crud
//----------------------------------------------------------------------------

#ifndef win32

static GtkWidget *add_overlay_widget = NULL;

// called when "cancel" clicked on the GtkFileChooser
SIGNAL_CALLBACK void add_overlay_cancel_clicked()
{
    gtk_widget_hide(add_overlay_widget);
}

// called when "ok" clicked on the GtkFileChooser
SIGNAL_CALLBACK void add_overlay_ok_clicked()
{
    GSList *files = gtk_file_chooser_get_filenames(
        GTK_FILE_CHOOSER(add_overlay_widget));

    gtk_widget_hide(add_overlay_widget);
    if (files)
    {
        GSList *iter = files;

        do {
          gchar *s = (gchar *) iter->data;
          add_overlay_file(s);
          g_free(s);
          iter =  iter->next;
        }
        while(iter);

        g_slist_free(files);
    }
}

SIGNAL_CALLBACK void add_overlay_widget_destroy()
{
    gtk_widget_destroy(add_overlay_widget);
    add_overlay_widget = NULL;
}

// sets up the file chooser dialog
static void create_file_chooser_dialog()
{
    GtkWidget *parent = get_widget_checked("ssv_main_window");

    add_overlay_widget = gtk_file_chooser_dialog_new(
        "Open Overlay File", GTK_WINDOW(parent),
        GTK_FILE_CHOOSER_ACTION_OPEN,
        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
        GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
        NULL);

    // we need to extract the buttons, so we can connect them to our
    // button handlers, above
    GtkHButtonBox *box =
        (GtkHButtonBox*)(((GtkDialog*)add_overlay_widget)->action_area);
    GList *buttons = box->button_box.box.children;

    GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
    GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

    g_signal_connect((gpointer)cancel_btn, "clicked",
        G_CALLBACK(add_overlay_cancel_clicked), NULL);
    g_signal_connect((gpointer)ok_btn, "clicked",
        G_CALLBACK(add_overlay_ok_clicked), NULL);
    g_signal_connect(add_overlay_widget, "destroy",
        G_CALLBACK(add_overlay_widget_destroy), NULL);
    g_signal_connect(add_overlay_widget, "destroy_event",
        G_CALLBACK(add_overlay_widget_destroy), NULL);
    g_signal_connect(add_overlay_widget, "delete_event",
        G_CALLBACK(add_overlay_widget_destroy), NULL);

    // add the filters
    GtkFileFilter *shp_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(shp_filt, "Shape Files (*.shp)");
    gtk_file_filter_add_pattern(shp_filt, "*.shp");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(add_overlay_widget),shp_filt);

    GtkFileFilter *csv_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(csv_filt, "Generic CSV Files (*.csv)");
    gtk_file_filter_add_pattern(csv_filt, "*.csv");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(add_overlay_widget),csv_filt);

    GtkFileFilter *all_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(all_filt, "All Files (*.*)");
    gtk_file_filter_add_pattern(all_filt, "*");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(add_overlay_widget),all_filt);

    // don't allow multi-select
    gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(add_overlay_widget),
                                         FALSE);

    // we need to make these modal -- if the user opens multiple "open"
    // dialogs, we'll get confused on the callbacks
    gtk_window_set_modal(GTK_WINDOW(add_overlay_widget), TRUE);
    gtk_window_set_destroy_with_parent(GTK_WINDOW(add_overlay_widget), TRUE);
    gtk_dialog_set_default_response(GTK_DIALOG(add_overlay_widget),
                                    GTK_RESPONSE_OK);
}
#endif

SIGNAL_CALLBACK void on_add_overlay_button_clicked(GtkWidget *w)
{
    if (!meta_supports_meta_get_latLon(curr->meta)) {
      message_box("Cannot add overlays to an image without "
                  "geolocation information.");
      return;
    }

#ifdef win32
    OPENFILENAME of;
    int retval;
    char fname[1024];

    fname[0] = '\0';

    memset(&of, 0, sizeof(of));

#ifdef OPENFILENAME_SIZE_VERSION_400
    of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
    of.lStructSize = sizeof(of);
#endif

    of.hwndOwner = NULL;
    of.lpstrFilter =
        "Shape Files (*.shp)\0*.shp\0"
        "Generic CSV Files (*.csv)\0*.csv\0"
        "All Files\0*\0";
    of.lpstrCustomFilter = NULL;
    of.nFilterIndex = 1;
    of.lpstrFile = fname;
    of.nMaxFile = sizeof(fname);
    of.lpstrFileTitle = NULL;
    of.lpstrInitialDir = ".";
    of.lpstrTitle = "Select File";
    of.lpstrDefExt = NULL;
    of.Flags = OFN_HIDEREADONLY | OFN_EXPLORER;

    retval = GetOpenFileName(&of);

    if (!retval) {
        if (CommDlgExtendedError())
            message_box("File dialog box error");
        return;
    }

    add_overlay_file(fname);

#else // #ifdef win32

    if (!add_overlay_widget)
        create_file_chooser_dialog();

    gtk_widget_show(add_overlay_widget);
#endif // #ifdef win32
}
