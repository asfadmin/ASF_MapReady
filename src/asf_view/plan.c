#include "asf_view.h"
#include "plan.h"
#include <ctype.h>

// flag indicating whether or not asf_view is running with the acquisition
// planning stuff turned on.  access from elsewhere with "planner_is_active()"

static int in_planning_mode = FALSE;
int planner_is_active()
{
  return in_planning_mode;
}

// Need to remember the modes, so we can refer to them later
char **modes=NULL;
int num_beam_modes=-1;

enum
{
  COL_COLOR = 0,
  COL_SELECTED,
  COL_DATE,
  COL_DATE_HIDDEN,
  COL_ORBIT_FRAME,
  COL_COVERAGE,
  COL_ORBITDIR,
  COL_INDEX
};

enum
{
  SORTID_DATE = 0,
  SORTID_ORBIT_FRAME,
  SORTID_COVERAGE,
  SORTID_ORBITDIR
};

static GtkTreeModel *liststore = NULL;

int sort_compare_func(GtkTreeModel *model,
                      GtkTreeIter *a, GtkTreeIter *b,
                      gpointer userdata)
{
  int sortcol = GPOINTER_TO_INT(userdata);
  int ret;

  switch (sortcol)
  {
    case SORTID_DATE:
    {
      char *date1_str, *date2_str;

      gtk_tree_model_get(model, a, COL_DATE_HIDDEN, &date1_str, -1);
      gtk_tree_model_get(model, b, COL_DATE_HIDDEN, &date2_str, -1);

      double date1 = atof(date1_str);
      double date2 = atof(date2_str);

      if (date1 != date2)
        ret = date1 > date2 ? 1 : -1;
      else
        ret = 0; // equal
    }
    break;

    case SORTID_COVERAGE:
    {
      char *coverage1_str, *coverage2_str;

      gtk_tree_model_get(model, a, COL_COVERAGE, &coverage1_str, -1);
      gtk_tree_model_get(model, b, COL_COVERAGE, &coverage2_str, -1);

      double cov1 = atof(coverage1_str);
      double cov2 = atof(coverage2_str);

      if (cov1 != cov2)
        ret = cov1 > cov2 ? 1 : -1;
      else
        ret = 0; // equal coverage
    }
    break;

    case SORTID_ORBIT_FRAME:
    {
      char *str1, *str2;

      gtk_tree_model_get(model, a, COL_ORBIT_FRAME, &str1, -1);
      gtk_tree_model_get(model, b, COL_ORBIT_FRAME, &str2, -1);

      int orbit1, orbit2, frame1, frame2;
      sscanf(str1, "%d/%d", &orbit1, &frame1);
      sscanf(str2, "%d/%d", &orbit2, &frame2);

      if (orbit1 != orbit2) {
        ret = orbit1 > orbit2 ? 1 : -1;
      } else {
        if (frame1 != frame2) {
          ret = frame1 > frame2 ? 1 : -1;
        } else {
          ret = 0;
        }
      }
    }
    break;

    case SORTID_ORBITDIR:
    {
      char *str1, *str2;

      gtk_tree_model_get(model, a, COL_ORBITDIR, &str1, -1);
      gtk_tree_model_get(model, b, COL_ORBITDIR, &str2, -1);

      if (strcmp(str1, str2) == 0) {
        // both ascending, or both descending -- sort by date
        char *date1_str, *date2_str;
        
        gtk_tree_model_get(model, a, COL_DATE_HIDDEN, &date1_str, -1);
        gtk_tree_model_get(model, b, COL_DATE_HIDDEN, &date2_str, -1);
        
        double date1 = atof(date1_str);
        double date2 = atof(date2_str);
        
        if (date1 != date2)
          ret = date1 > date2 ? 1 : -1;
        else
          ret = 0; // equal
      }
      else {
        ret = strcmp(str1, str2) > 0 ? 1 : -1;
      }
    }
    break;

    default:
      assert(0);
      ret = 0;
      break; // not reached
  }

  return ret;
}

static void clear_found()
{
    gtk_list_store_clear(GTK_LIST_STORE(liststore));

    int i;

    // leave the first one -- area of interest
    for (i=1; i<MAX_POLYS; ++i)
      g_polys[i].n = 0;

    which_poly = 0;
    g_poly = &g_polys[which_poly];
}

SIGNAL_CALLBACK void cb_callback(GtkCellRendererToggle *cell,
                                 char *path_str, gpointer data)
{
  GtkTreeIter iter;
  GtkTreePath *path = gtk_tree_path_new_from_string(path_str);
  gboolean enabled;

  gtk_tree_model_get_iter(liststore, &iter, path);
  gtk_tree_model_get(liststore, &iter, COL_SELECTED, &enabled, -1);
  enabled ^= 1;
  //printf("Enabled> %s %s\n", path_str, enabled ? "yes" : "no");
  gtk_list_store_set(GTK_LIST_STORE(liststore), &iter,
                     COL_SELECTED, enabled, -1);

  gtk_tree_path_free(path);

  fill_big(curr);
}

void setup_planner()
{
    in_planning_mode = TRUE;
    show_widget("planner_notebook", TRUE);
    show_widget("viewer_notebook", FALSE);

    // populate the "Satellite/Beam" dropdown from the
    // "beam_modes.txt" file
    modes = get_all_beam_modes(&num_beam_modes);

    int i;
    clear_combobox("satellite_combobox");
    for (i=0; i<num_beam_modes; ++i)
        add_to_combobox("satellite_combobox", modes[i]);

    //for (i=0; i<num_beam_modes; ++i)
    //    FREE(modes[i]);
    //FREE(modes);

    // by default select ALOS -- FIXME select alos!
    set_combo_box_item_checked("satellite_combobox", 0);

    // by default search for both ascending and descending
    set_combo_box_item_checked("orbit_direction_combobox", 0);

    // Now, setting up the "Found Acquisitions" table
    GtkTreeViewColumn *col;
    GtkCellRenderer *rend;

    GtkListStore *ls = gtk_list_store_new(8,
                                          GDK_TYPE_PIXBUF,
                                          G_TYPE_BOOLEAN,
                                          G_TYPE_STRING,
                                          G_TYPE_STRING,
                                          G_TYPE_STRING,
                                          G_TYPE_STRING,
                                          G_TYPE_STRING,
                                          G_TYPE_STRING);
    liststore = GTK_TREE_MODEL(ls);

    GtkWidget *tv = get_widget_checked("treeview_planner");

    // Column: The color pixbuf
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_pixbuf_new ();
    gtk_tree_view_column_pack_start (col, rend, TRUE);
    gtk_tree_view_column_add_attribute (col, rend, "pixbuf", COL_COLOR);

    // A checkbox column
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_toggle_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    g_signal_connect(G_OBJECT(rend), "toggled", G_CALLBACK(cb_callback), NULL);
    gtk_tree_view_column_add_attribute(col, rend, "active", COL_SELECTED);

    // Column: Date
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Date/Time");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_column_set_sort_column_id(col, SORTID_DATE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_DATE);

    // Column: Date (Hidden) -- this one stores the #secs since ref time
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "-");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_DATE_HIDDEN);

    // Column: Orbit/Frame
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Orbit/Frame");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_column_set_sort_column_id(col, SORTID_ORBIT_FRAME);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_ORBIT_FRAME);

    // Column: Coverage Pct
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "%");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_column_set_sort_column_id(col, SORTID_COVERAGE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_COVERAGE);

    // Column: Ascending/Descending
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_column_set_sort_column_id(col, SORTID_ORBITDIR);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_ORBITDIR);

    // Column: Additional Info (hidden)
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "-");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_INDEX);

    // No more columns, finish setup
    gtk_tree_view_set_model(GTK_TREE_VIEW(tv), GTK_TREE_MODEL(liststore));
    gtk_tree_selection_set_mode(gtk_tree_view_get_selection(GTK_TREE_VIEW(tv)),
                                GTK_SELECTION_MULTIPLE);

    GtkTreeSortable *sortable = GTK_TREE_SORTABLE(liststore);
    gtk_tree_sortable_set_sort_func(sortable, SORTID_DATE, sort_compare_func,
                                    GINT_TO_POINTER(SORTID_DATE), NULL);
    gtk_tree_sortable_set_sort_func(sortable, SORTID_ORBIT_FRAME,
                                    sort_compare_func,
                                    GINT_TO_POINTER(SORTID_ORBIT_FRAME), NULL);
    gtk_tree_sortable_set_sort_func(sortable, SORTID_COVERAGE,
                                    sort_compare_func,
                                    GINT_TO_POINTER(SORTID_COVERAGE), NULL);
    gtk_tree_sortable_set_sort_func(sortable, SORTID_ORBITDIR,
                                    sort_compare_func,
                                    GINT_TO_POINTER(SORTID_ORBITDIR), NULL);

    // intial sort: highest coverage on top
    gtk_tree_sortable_set_sort_column_id(sortable, SORTID_COVERAGE,
                                         GTK_SORT_DESCENDING);
    g_object_unref(liststore);

    // Kludge during testing...
    //g_polys[0].n = 1;
    //g_polys[0].line[0] = 1850;
    //g_polys[0].samp[0] = 3650;
    //g_polys[0].c = 0;
    crosshair_line = 1650;
    crosshair_samp = 3450;
    zoom = 1;
    //center_line = (crosshair_line + g_polys[0].line[0])/2;
    //center_samp = (crosshair_samp + g_polys[0].samp[0])/2;
    center_line = crosshair_line;
    center_samp = crosshair_samp;
    set_combo_box_item_checked("satellite_combobox", 1);
    put_string_to_entry("lat_min_entry", "62");
    put_string_to_entry("lat_max_entry", "64");
    put_string_to_entry("lon_min_entry", "-125");
    put_string_to_entry("lon_max_entry", "-120");
    // ... all this should be deleted

}

int row_is_checked(int row)
{
  // returns true if that row # in the found acquisitions is checked
  
  // return false if we aren't running the planner, or the row # is
  // not checked

  int ret = FALSE;
  if (in_planning_mode) {

    if (row <= gtk_tree_model_iter_n_children(liststore, NULL)) {

      GtkTreeIter iter;
      gboolean valid = gtk_tree_model_get_iter_first(liststore, &iter);
      while (valid)
      {
        gboolean selected;
        char *index_str;

        gtk_tree_model_get(liststore, &iter, COL_INDEX, &index_str,
                           COL_SELECTED, &selected, -1);
      
        int index = atoi(index_str);
        if (index == row) {
          ret = selected;
          break;
        }

        valid = gtk_tree_model_iter_next(liststore, &iter);
      }
    }
  }

  return ret;
}

static char *trim_whitespace(const char *s)
{
  // make a copy we can alter
  char *tmp = STRDUP(s);

  // first trim trailing whitespace
  while (isspace(tmp[strlen(tmp)-1]))
    tmp[strlen(tmp)-1] = '\0';

  // to trim leading whitespace: get a pointer to first non-whitespace char...
  char *p = tmp;
  while (isspace(*p))
    ++p;

  // ... then strdup from that pointer
  char *ret = STRDUP(p);
  free(tmp);
  return ret;
}

static void split2(const char *str_in, char sep, char **s1_out, char **s2_out)
{
  // splits a string into two pieces, stuff before the separater character
  // and the stuff after it.  The separater character is not included in
  // either string
  char *str = STRDUP(str_in);
  char *s1 = MALLOC(sizeof(char)*(strlen(str)+1));
  char *s2 = MALLOC(sizeof(char)*(strlen(str)+1));

  char *p = strchr(str, sep);

  if (p) {
    *p = '\0';
    strcpy(s1, str);
    *p = sep;
    strcpy(s2, p+1);
  } else {
    // no sep -- s2 is empty, s1 is a copy of str
    strcpy(s1, str);
    strcpy(s2, "");
  }

  // trim whitespace
  *s1_out = trim_whitespace(s1);
  *s2_out = trim_whitespace(s2);

  FREE(s1);
  FREE(s2);
  FREE(str);
}

static void time_to_orbit_frame(double t, int *orbit, int *frame)
{
    // FIXME fake calculation for now
    double t1 = t/(24.*60.*60.);
    t1 -= 365*13;

    *orbit = (int) t1;
    *frame = (int) ((t1 - (double)(*orbit)) * 671);
}

SIGNAL_CALLBACK void on_plan_button_clicked(GtkWidget *w)
{
    int i,pass_type,zone;
    char errstr[1024];
    char *satellite, *beam_mode;
    long startdate, enddate;
    double max_lat, min_lat, clat, clon;
    Polygon *aoi;

    // gather up error mesages in "errstr"
    // at the end, if errstr is non-empty, we can't do the planning
    strcpy(errstr, "");

    // satellite & beam mode
    GtkWidget *cb = get_widget_checked("satellite_combobox");
    i = gtk_combo_box_get_active(GTK_COMBO_BOX(cb));
    split2(modes[i], '/', &satellite, &beam_mode);

    // get the start/end dates
    startdate = (long)get_int_from_entry("start_date_entry");
    if (!is_valid_date(startdate))
      strcat(errstr, "Invalid start date.\n");

    enddate = (long)get_int_from_entry("end_date_entry");
    if (!is_valid_date(enddate))
      strcat(errstr, "Invalid end date.\n");

    // collect together the polygon, calculate min_lat, etc
    meta_parameters *meta = curr->meta;
    if (!(meta->projection || (meta->sar&&meta->state_vectors) ||
          meta->transform || meta->airsar))
    {
      strcat(errstr, "Image has no geolocation information.\n");
      zone = max_lat = min_lat = 0;
      aoi = NULL;
    }
    else {
      double x[MAX_POLY_LEN], y[MAX_POLY_LEN];
      
      if (g_poly->n == 0 &&
          entry_has_text("lat_min_entry") &&
          entry_has_text("lat_max_entry") &&
          entry_has_text("lon_min_entry") &&
          entry_has_text("lon_max_entry"))
      {
          // try to get area of interest from the lat/lon entries
          double lat_min = get_double_from_entry("lat_min_entry");
          double lat_max = get_double_from_entry("lat_max_entry");
          double lon_min = get_double_from_entry("lon_min_entry");
          double lon_max = get_double_from_entry("lon_max_entry");

          meta_get_lineSamp(meta, lat_min, lon_min, 0,
                            &crosshair_line, &crosshair_samp);
          meta_get_lineSamp(meta, lat_min, lon_max, 0,
                            &g_polys[0].line[0], &g_polys[0].samp[0]);
          meta_get_lineSamp(meta, lat_max, lon_max, 0,
                            &g_polys[0].line[1], &g_polys[0].samp[1]);
          meta_get_lineSamp(meta, lat_max, lon_min, 0,
                            &g_polys[0].line[2], &g_polys[0].samp[2]);
          meta_get_lineSamp(meta, lat_min, lon_min, 0,
                            &g_polys[0].line[3], &g_polys[0].samp[3]);

          center_line = 0.25 * (crosshair_line + g_polys[0].line[0] +
                                g_polys[0].line[1] + g_polys[0].line[2]);
          center_samp = 0.25 * (crosshair_samp + g_polys[0].samp[0] +
                                g_polys[0].samp[1] + g_polys[0].samp[2]);

          g_polys[0].n = 4;
          g_polys[0].c = 3;

          fill_small(curr);
          fill_big(curr);
      }

      if (g_poly->n == 0) {
        strcat(errstr, "No area of interest selected.\n");
        zone = max_lat = min_lat = 0;
        aoi = NULL;
      }
      else if (g_poly->n == 1) {
        // special handling if we have only two points (create a box)
        double lat1, lat2, lon1, lon2;

        // first the corner points -- then use the center point to find
        // zone all calculations are done with, this will mean greater
        // distortion towards the edges of large areas of interest
        meta_get_latLon(meta,crosshair_line,crosshair_samp,0,&lat1,&lon1);
        meta_get_latLon(meta,g_poly->line[0],g_poly->samp[0],0,&lat2,&lon2);
        zone = utm_zone((lon1+lon2)/2.);

        // now get all corner points into UTM
        latLon2UTM_zone(lat1, lon1, 0, zone, &x[0], &y[0]);
        latLon2UTM_zone(lat2, lon2, 0, zone, &x[2], &y[2]);

        double lat, lon;
        meta_get_latLon(meta, g_poly->line[0], crosshair_samp, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone, &x[1], &y[1]);

        meta_get_latLon(meta, crosshair_line, g_poly->samp[0], 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone, &x[3], &y[3]);

        clat = .5*(lat1+lat2);
        clon = .5*(lon1+lon2);

        min_lat = lat1 < lat2 ? lat1 : lat2;
        max_lat = lat1 > lat2 ? lat1 : lat2;

        aoi = polygon_new_closed(4, x, y);

        // replace line with the box we created
        double l = g_poly->line[0];
        double s = g_poly->samp[0];

        g_polys[0].n = 4;
        g_polys[0].c = 3;
        g_polys[0].show_extent = FALSE;

        g_polys[0].line[0] = l;
        g_polys[0].samp[0] = crosshair_samp;
        g_polys[0].line[1] = l;
        g_polys[0].samp[1] = s;
        g_polys[0].line[2] = crosshair_line;
        g_polys[0].samp[2] = s;
        g_polys[0].line[3] = crosshair_line;
        g_polys[0].samp[3] = crosshair_samp;        
      }
      else {

        // we make a first pass through, just to get the center point
        // (in lat/lon).  all polygon points are passed to the planner in
        // UTM, need to be in the same zone, so we try to pick the best one
        double lat, lon;
        meta_get_latLon(meta, crosshair_line, crosshair_samp, 0, &lat, &lon);
        max_lat = min_lat = clat = lat;
        clon = lon;

        for (i=0; i<g_poly->n; ++i) {
          meta_get_latLon(meta, g_poly->line[i], g_poly->samp[i], 0,
                          &lat, &lon);

          clat += lat;
          clon += lon;

          if (lat > max_lat) max_lat = lat;
          if (lat < min_lat) min_lat = lat;
        }

        clat /= (double)(g_poly->n+1);
        clon /= (double)(g_poly->n+1);

        // center point determines which zone to use
        zone = utm_zone(clon);

        printf("Center lat/lon: %f, %f (Zone %d)\n", clat, clon, zone);

        // now the second pass -- determining the actual coordinates
        // of the polygon in the right UTM zone
        meta_get_latLon(meta, crosshair_line, crosshair_samp, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone, &x[0], &y[0]);

        for (i=0; i<g_poly->n; ++i) {
          meta_get_latLon(meta, g_poly->line[i], g_poly->samp[i], 0,
                          &lat, &lon);
          latLon2UTM_zone(lat, lon, 0, zone, &x[i+1], &y[i+1]);
        }

        // now we can finally create the polygon
        aoi = polygon_new_closed(g_poly->n+1, x, y);
      }
    }

    // pass type
    cb = get_widget_checked("orbit_direction_combobox");
    i = gtk_combo_box_get_active(GTK_COMBO_BOX(cb));
    switch (i) {
      default:
      case 0: pass_type = ASCENDING_OR_DESCENDING; break;
      case 1: pass_type = ASCENDING_ONLY;          break;
      case 2: pass_type = DESCENDING_ONLY;         break;
    }
    
    // tle file
    char *tle_filename = find_in_share("tle");
    if (!tle_filename)
      strcat(errstr, "Unable to find two-line element file!\n");
    if (!fileExists(tle_filename))
      sprintf(errstr, "%sNo two-line element file found:\n %s\n",
              errstr, tle_filename);

    if (strlen(errstr) > 0) {
      put_string_to_label("plan_error_label", errstr);
    }
    else {
      put_string_to_label("plan_error_label", "Searching...");
      enable_widget("plan_button", FALSE);
      while (gtk_events_pending())
        gtk_main_iteration();

      char *err;
      PassCollection *pc;

      int n = plan(satellite, beam_mode, startdate, enddate, max_lat, min_lat,
                   clat, clon, pass_type, zone, aoi, tle_filename, &pc, &err);

      if (n < 0) {
        put_string_to_label("plan_error_label", err);
        free(err);
      }
      else {
        // switch to planning results tab
        GtkWidget *nb = get_widget_checked("planner_notebook");
        gtk_notebook_set_current_page(GTK_NOTEBOOK(nb), 1);

        char msg[256];
        sprintf(msg, "Found %d matches.\n", n);
        asfPrintStatus(msg);
        put_string_to_label("plan_error_label", msg);

        for (i=0; i<pc->num; ++i) {
          asfPrintStatus("#%02d: %s (%.1f%%)\n", i+1,
                         pc->passes[i]->start_time_as_string,
                         100. * pc->passes[i]->total_pct);
        }

        // this is for debugging, can be removed
        pass_collection_to_kml(pc, "test_kml.kml");
        clear_found();

        // polygon #0 is left alone (it is the area of interest)
        // The passes start at polygon #1 (clobber any existing polygons)


        // find the maximum coverage
        double max_coverage = 0;
        for (i=0; i<pc->num; ++i) {
          PassInfo *pi = pc->passes[i];
          if (pi->total_pct > max_coverage)
            max_coverage = pi->total_pct;
        }
        double coverage_cutoff = .6 * max_coverage;

        // now create polygons from each pass
        for (i=0; i<pc->num; ++i) {
          PassInfo *pi = pc->passes[i];

          int k,m=0;
          for (k=0; k<pi->num; ++k) {
            OverlapInfo *oi = pi->overlaps[k];
            Polygon *poly = oi->viewable_region;

            int j;
            for (j=0; j<poly->n; ++j) {
              double samp, line, lat, lon;

              // account for the false northing in the southern hemisphere
              if (clat<0)
                poly->y[j] -= 10000000;
              UTM2latLon(poly->x[j], poly->y[j], 0, oi->utm_zone, &lat, &lon);
              meta_get_lineSamp(meta, lat, lon, 0, &line, &samp);

              //printf("%d,%d -- %f,%f\n",i,m,line,samp);
              if (m >= MAX_POLY_LEN) {
                printf("Reached maximum polygon length!\n");
                printf("--> Pass number %d\n", i);
                printf("    Number of polygons: %d\n", pi->num);
                break;
              } else {
                g_polys[i+1].line[m]=line;
                g_polys[i+1].samp[m]=samp;
                ++m;
              }
            }
          }

          g_polys[i+1].n=m;
          g_polys[i+1].c=m-1;

          g_polys[i+1].show_extent=FALSE;

          // get ready to add this one to the list, create each column's value
          char date_info[256];
          sprintf(date_info, "%s", pi->start_time_as_string);
          
          char date_hidden_info[256];
          sprintf(date_hidden_info, "%f", pi->start_time);

          int orbit, frame;
          char orbit_frame_info[256];
          time_to_orbit_frame(pi->start_time, &orbit, &frame);
          sprintf(orbit_frame_info, "%d/%d", orbit, frame);

          char pct_info[256];
          sprintf(pct_info, "%.1f", 100.*pi->total_pct);

          // the index in the "g_polys" array which corresponds to this row
          char index_info[256];
          sprintf(index_info, "%d", i+1);

          // ascending or descending
          char orbit_dir[5];
          sprintf(orbit_dir, "%c", pi->dir);

          // select ones with >60% of the max coverage
          int selected = pi->total_pct > coverage_cutoff;

          // create a little block of coloring which will indicate which
          // region on the map corresponds to this line in the list
          GdkPixbuf *pb = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, 24, 16);
          unsigned char *pixels = gdk_pixbuf_get_pixels(pb);
          int rowstride = gdk_pixbuf_get_rowstride(pb);
          int n_channels = gdk_pixbuf_get_n_channels(pb);
          for (k=0; k<16; ++k) {
            for (m=0; m<24; ++m) {
              unsigned char *p = pixels + k*rowstride + m*n_channels;

              // edges are black on these 
              if (k==0 || k==15 || m==0 || m==23)
                p[0] = p[1] = p[2] = 0;
              else
                get_color(i+11, &p[0], &p[1], &p[2]);
            }
          }

          // now, can add the list entry (add to the end, later the sort
          // will rearrange them all anyway)
          GtkTreeIter iter;
          gtk_list_store_append(GTK_LIST_STORE(liststore), &iter);
          gtk_list_store_set(GTK_LIST_STORE(liststore), &iter,
                             COL_COLOR, pb,
                             COL_SELECTED, selected,
                             COL_DATE, date_info,
                             COL_DATE_HIDDEN, date_hidden_info,
                             COL_ORBIT_FRAME, orbit_frame_info,
                             COL_COVERAGE, pct_info,
                             COL_ORBITDIR, orbit_dir,
                             COL_INDEX, index_info,
                             -1);
              
          if (i>=MAX_POLYS-2) {
            printf("Too many polygons: %d\n", pc->num);
            break;
          }
        }
      }

      which_poly=0;
      enable_widget("plan_button", TRUE);
      fill_big(curr);
    }
}

SIGNAL_CALLBACK void on_save_acquisitions_button_clicked(GtkWidget *w)
{
    GtkTreeIter iter;
    gboolean valid = gtk_tree_model_get_iter_first(liststore, &iter);
    while (valid)
    {
        char *date_str, *dbl_date_str, *coverage_str, *orbit_frame_str;
        gboolean enabled;
        gtk_tree_model_get(liststore, &iter,
                           COL_SELECTED, &enabled,
                           COL_DATE, &date_str,
                           COL_DATE_HIDDEN, &dbl_date_str,
                           COL_COVERAGE, &coverage_str,
                           COL_ORBIT_FRAME, &orbit_frame_str,
                           -1);

        //double date = atof(dbl_date_str);
        double cov = atof(coverage_str);
        int orbit, frame;
        sscanf(orbit_frame_str, "%d/%d", &orbit, &frame);

        // now do something with all this great info
        if (enabled) {
          // here we will save it, or something
          printf("Saving: %s %.1f%% %d/%d\n", date_str, cov, orbit, frame);
        }

        valid = gtk_tree_model_iter_next(liststore, &iter);
    }
}

SIGNAL_CALLBACK void on_clear_button_clicked(GtkWidget *w)
{
    // clear found list
    clear_found();

    // repaint
    fill_big(curr);
}

SIGNAL_CALLBACK void on_show_box_button_clicked(GtkWidget *w)
{
    double lat_min = get_double_from_entry("lat_min_entry");
    double lat_max = get_double_from_entry("lat_max_entry");
    double lon_min = get_double_from_entry("lon_min_entry");
    double lon_max = get_double_from_entry("lon_max_entry");

    meta_parameters *meta = curr->meta;
    meta_get_lineSamp(meta, lat_min, lon_min, 0,
                      &crosshair_line, &crosshair_samp);
    meta_get_lineSamp(meta, lat_min, lon_max, 0,
                      &g_polys[0].line[0], &g_polys[0].samp[0]);
    meta_get_lineSamp(meta, lat_max, lon_max, 0,
                      &g_polys[0].line[1], &g_polys[0].samp[1]);
    meta_get_lineSamp(meta, lat_max, lon_min, 0,
                      &g_polys[0].line[2], &g_polys[0].samp[2]);
    meta_get_lineSamp(meta, lat_min, lon_min, 0,
                      &g_polys[0].line[3], &g_polys[0].samp[3]);

    g_polys[0].n = 4;
    g_polys[0].c = 3;

    center_line = 0.25 * (crosshair_line + g_polys[0].line[0] +
                          g_polys[0].line[1] + g_polys[0].line[2]);
    center_samp = 0.25 * (crosshair_samp + g_polys[0].samp[0] +
                          g_polys[0].samp[1] + g_polys[0].samp[2]);

    fill_small(curr);
    fill_big(curr);
}
