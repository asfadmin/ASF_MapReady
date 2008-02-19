#include "asf_view.h"
#include "plan.h"
#include <ctype.h>
#include <errno.h>

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
  COL_ORBIT_PATH,
  COL_COVERAGE,
  COL_ORBITDIR,
  COL_START_LAT,
  COL_DURATION,
  COL_INDEX
};

enum
{
  SORTID_DATE = 0,
  SORTID_ORBIT_PATH,
  SORTID_COVERAGE,
  SORTID_ORBITDIR,
  SORTID_START_LAT,
  SORTID_DURATION
};

static GtkTreeModel *liststore = NULL;

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

    case SORTID_ORBIT_PATH:
    {
      char *str1, *str2;

      gtk_tree_model_get(model, a, COL_ORBIT_PATH, &str1, -1);
      gtk_tree_model_get(model, b, COL_ORBIT_PATH, &str2, -1);

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

    case SORTID_START_LAT:
    {
      char *start_lat1_str, *start_lat2_str;

      gtk_tree_model_get(model, a, COL_START_LAT, &start_lat1_str, -1);
      gtk_tree_model_get(model, b, COL_START_LAT, &start_lat2_str, -1);

      double lat1 = atof(start_lat1_str);
      double lat2 = atof(start_lat2_str);

      if (lat1 != lat2)
        ret = lat1 > lat2 ? 1 : -1;
      else
        ret = 0; // equal
    }
    break;

    case SORTID_DURATION:
    {
      char *duration1_str, *duration2_str;

      gtk_tree_model_get(model, a, COL_DURATION, &duration1_str, -1);
      gtk_tree_model_get(model, b, COL_DURATION, &duration2_str, -1);

      double dur1 = atof(duration1_str);
      double dur2 = atof(duration2_str);

      if (dur1 != dur2)
        ret = dur1 > dur2 ? 1 : -1;
      else
        ret = 0; // equal
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

extern int ran_cb_callback;
SIGNAL_CALLBACK void cb_callback(GtkCellRendererToggle *cell,
                                 char *path_str, gpointer data)
{
  GtkTreeIter iter;
  GtkTreePath *path = gtk_tree_path_new_from_string(path_str);
  gboolean enabled;

  gtk_tree_model_get_iter(liststore, &iter, path);
  gtk_tree_model_get(liststore, &iter, COL_SELECTED, &enabled, -1);
  enabled ^= 1;
  gtk_list_store_set(GTK_LIST_STORE(liststore), &iter,
                     COL_SELECTED, enabled, -1);

  gtk_tree_path_free(path);

  ran_cb_callback = TRUE;
  fill_big(curr);
}

static void populate_tle_info()
{
    char *satellite, *beam_mode;
    GtkWidget *cb = get_widget_checked("satellite_combobox");
    int i = gtk_combo_box_get_active(GTK_COMBO_BOX(cb));
    split2(modes[i], '/', &satellite, &beam_mode);

    char *tle_filename = find_in_share("tle");
    const char *tle_info = get_tle_info(tle_filename, satellite);
    put_string_to_label("tle_info_label", tle_info);
}

static void populate_config_info()
{
    char *config_filename = find_in_share("planner_config.txt");
    FILE *fp = NULL;

    if (config_filename)
        fp = fopen(config_filename, "r");

    char *output_dir, *output_file;

    if (!fp) {
        // no config file -- default to the share dir
        output_dir = STRDUP(get_asf_share_dir());
        output_file = STRDUP("output.csv");
    }
    else {
        char s[256], *junk;
        if (!fgets(s, 255, fp))
            strcpy(s,"output directory = ");
        split2(s, '=', &junk, &output_dir);
        free(junk);

        if (!fgets(s, 255, fp))
            strcpy(s,"output file = ");
        split2(s, '=', &junk, &output_file);
        free(junk);

        fclose(fp);
    }

    put_string_to_entry("output_dir_entry", output_dir);
    free(output_dir);

    put_string_to_entry("output_file_entry", output_file);
    free(output_file);
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

    // by default select ALOS -- FIXME select alos!
    set_combo_box_item_checked("satellite_combobox", 0);

    // by default search for both ascending and descending
    set_combo_box_item_checked("orbit_direction_combobox", 0);

    // Now, setting up the "Found Acquisitions" table
    GtkTreeViewColumn *col;
    GtkCellRenderer *rend;

    GtkListStore *ls = gtk_list_store_new(10,
                                          GDK_TYPE_PIXBUF,
                                          G_TYPE_BOOLEAN,
                                          G_TYPE_STRING,
                                          G_TYPE_STRING,
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
    gtk_tree_view_column_set_title(col, "Orbit/Path");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_column_set_sort_column_id(col, SORTID_ORBIT_PATH);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_ORBIT_PATH);

    // Column: Coverage Pct
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "%");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_column_set_sort_column_id(col, SORTID_COVERAGE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_COVERAGE);

    // Column: Start Latitude
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Lat");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_column_set_sort_column_id(col, SORTID_START_LAT);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_START_LAT);

    // Column: Duration
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Duration");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_column_set_sort_column_id(col, SORTID_DURATION);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_DURATION);

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
    gtk_tree_sortable_set_sort_func(sortable, SORTID_ORBIT_PATH,
                                    sort_compare_func,
                                    GINT_TO_POINTER(SORTID_ORBIT_PATH), NULL);
    gtk_tree_sortable_set_sort_func(sortable, SORTID_COVERAGE,
                                    sort_compare_func,
                                    GINT_TO_POINTER(SORTID_COVERAGE), NULL);
    gtk_tree_sortable_set_sort_func(sortable, SORTID_START_LAT,
                                    sort_compare_func,
                                    GINT_TO_POINTER(SORTID_START_LAT), NULL);
    gtk_tree_sortable_set_sort_func(sortable, SORTID_DURATION,
                                    sort_compare_func,
                                    GINT_TO_POINTER(SORTID_DURATION), NULL);
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
    set_combo_box_item_checked("satellite_combobox", 2);
    put_string_to_entry("lat_min_entry", "44.6");
    put_string_to_entry("lat_max_entry", "45.2");
    put_string_to_entry("lon_min_entry", "-110.3");
    put_string_to_entry("lon_max_entry", "-109.4");
    put_string_to_entry("start_date_entry", "20070914");
    put_string_to_entry("end_date_entry", "20070916");
    // ... all this should be deleted

    // populate the "setup" tab's values
    populate_tle_info();
    populate_config_info();

    // redo the title to reflect that this is now a planner app
    GtkWidget *widget = get_widget_checked("ssv_main_window");
    gtk_window_set_title(GTK_WINDOW(widget),"Alaska Satellite Facility Acquisition Planning Application Program Software Tool Utility (ASF-APAPSTU)");
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

static int revolution2path(int revolution)
{
    return ((46*revolution+85)%671);
}

// calculate a satellite's nadir latitude for a particular frame
static double sat_lat(int ALOS_frame)
{
    double inclination = 90 - 98.16;
    double angle_thing = cos(D2R*inclination);
    double orbit = ALOS_frame/7200*360.;
    double ret = R2D*asin(sin(D2R*orbit)*angle_thing);
    return ret;
}

static double fake_lat(double real_lat, double inclination)
{
    return R2D*asin(sin(D2R*real_lat)/cos(D2R*inclination));
}

static double esa_node(double esa_lat, int direction)
{
    if (direction==0) {
        if (esa_lat < 0) esa_lat += 360;
    } else {
        esa_lat = 180-esa_lat;
    }
    return esa_lat*20;
}

// how long the satellite needs to stay on
static double alos_time(double start_lat, int start_direction,
                        double stop_lat, int stop_direction)
{
    double inclination=8.2352631198113;
    double secs_node=.822652757;
    double fake_1 = fake_lat(start_lat, inclination);
    double fake_2 = fake_lat(stop_lat, inclination);
    double node_1 = esa_node(fake_1, start_direction);
    double node_2 = esa_node(fake_2, stop_direction);
    if (node_2 <= node_1)
        node_2 = 7200 + node_2;
    double ret = secs_node*(node_2-node_1);
    return ret; 
}

static void calc_frame(double lat, int *frame)
{
  double sat_lat = lat*D2R;
  double inclination = D2R * (90-98.16);
  double angle_thing = cos(inclination);
  double rev = asin(sin(sat_lat)/angle_thing);
  *frame = (int)(rev*7200 + .5);
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

        // Polygon #0 is left alone (it is the area of interest), so
        // the passes start at polygon #1 (clobber any existing polygons)

        // find the maximum coverage
        double max_coverage = 0.;
        for (i=0; i<pc->num; ++i) {
          if (pc->passes[i]->total_pct > max_coverage)
            max_coverage = pc->passes[i]->total_pct;
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

          char orbit_info[256];
          //calc_frame(pc->clat, &frame);
          int orbit = pi->orbit;
          //int frame = pi->frame;
          int path = revolution2path(orbit);
          sprintf(orbit_info, "%d/%d", orbit, path);

          char pct_info[256];
          sprintf(pct_info, "%.1f", 100.*pi->total_pct);

          // the index in the "g_polys" array which corresponds to this row
          char index_info[256];
          sprintf(index_info, "%d", i+1);

          // start latitude
          char lat_info[256];
          sprintf(lat_info, "%.2f", pi->start_lat);

          // duration
          char duration_info[256];
          sprintf(duration_info, "%.1f", pi->duration);

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
              if (k==0 || k==15 || m==0 || m==23) {
                p[0] = p[1] = p[2] = 0;
              } else {
                get_color(i+11, &p[0], &p[1], &p[2]);
              }
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
                             COL_ORBIT_PATH, orbit_info,
                             COL_COVERAGE, pct_info,
                             COL_START_LAT, lat_info,
                             COL_DURATION, duration_info,
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
    char *output_dir = STRDUP(get_string_from_entry("output_dir_entry"));
    char *output_file = STRDUP(get_string_from_entry("output_file_entry"));
    char *out_name = MALLOC(sizeof(char) *
                            (strlen(output_dir) + strlen(output_file) + 5));
    sprintf(out_name, "%s/%s", output_dir, output_file);
    FILE *ofp = fopen(out_name, "w");
    if (!ofp) {
      message_box("Could not open output file!\n  %s\n  %s\n",
                  out_name, strerror(errno));
      return;
    }

    int num = 0;
    GtkTreeIter iter;

    gboolean valid = gtk_tree_model_get_iter_first(liststore, &iter);
    while (valid)
    {
        char *date_str, *dbl_date_str, *coverage_str, *orbit_path_str,
          *start_lat_str, *duration_str;
        gboolean enabled;
        gtk_tree_model_get(liststore, &iter,
                           COL_SELECTED, &enabled,
                           COL_DATE, &date_str,
                           COL_DATE_HIDDEN, &dbl_date_str,
                           COL_COVERAGE, &coverage_str,
                           COL_START_LAT, &start_lat_str,
                           COL_DURATION, &duration_str,
                           COL_ORBIT_PATH, &orbit_path_str,
                           -1);

        //double date = atof(dbl_date_str);
        double lat = atof(start_lat_str);
        double dur = atof(duration_str);
        double cov = atof(coverage_str);
        int orbit, path;
        sscanf(orbit_path_str, "%d/%d", &orbit, &path);

        // now do something with all this great info
        if (enabled) {
          //printf("Saving: %s %.1f%% %.2f %.1f %d/%d\n",
          //       date_str, cov, lat, dur, orbit, path);
          fprintf(ofp, "%s %.1f%% %.2f %.1f %d %d\n",
                  date_str, cov, lat, dur, orbit, path);
          ++num;
        }

        valid = gtk_tree_model_iter_next(liststore, &iter);
    }

    fclose(ofp);

    if (num==0)
        printf("Empty output file.\n");
    else
        printf("Saved %d acquisitions.\n", num);

    free(out_name);
    free(output_dir);
    free(output_file);
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
    if (lat_max==0 && lon_max==0) {
      meta_get_lineSamp(meta, lat_min, lon_min, 0,
                        &crosshair_line, &crosshair_samp);
    }
    else {
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
    }
      
    fill_small(curr);
    fill_big(curr);
}

SIGNAL_CALLBACK void on_save_setup_button_clicked(GtkWidget *w)
{
    FILE *ofp = fopen_share_file("planner_config.txt", "w");
    fprintf(ofp, "output directory = %s\n",
            get_string_from_entry("output_dir_entry"));
    fprintf(ofp, "output file = %s\n",
            get_string_from_entry("output_file_entry"));
    fclose(ofp);
}
