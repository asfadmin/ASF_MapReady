#include "asf_view.h"
#include "winshl.h"
#include "plan.h"
#include "dateUtil.h"
#include <ctype.h>
#include <errno.h>
#include <assert.h>

// For now this is a global constant -- the name of the satellite we are
// planning for.  The beam mode table code supports multiple satellites,
// so the GUI could be modified to allow selection of a satellite, and
// whatever the user selects there would be used in place of where this
// string is passed in.  Since for now the GUI only supports ALOS, there
// isn't really any reason to do this right now, but it should be an
// easy change.  Also, if we want to plan other for a different
// satellite, but still just one satellite, we can just change this
// string.
const char *planner_satellite = "ALOS";

// *** HOWEVER ***
// This is a fair bit of ALOS-specific code in here, in particular the
// orbit number calculations.  To extend this planner to use other
// satellites, this calculation is going to have be updated.  We have
// this function here to help isolate the ALOS-specific code, but I
// can't say for sure that things will work if this ever returns FALSE.
// (Though it SHOULD)
static int is_alos()
{
  return strcmp(planner_satellite,"ALOS")==0;
}

// flag indicating whether or not asf_view is running with the acquisition
// planning stuff turned on.  access from elsewhere with "planner_is_active()"

static int in_planning_mode = FALSE;
int planner_is_active()
{
  return in_planning_mode;
}

// Need to remember the modes, so we can refer to them later
typedef struct mode_info_struct
{
    char *mode;
    double look_min;
    double look_max;
    double look_incr;
    char allowed_look_angles[64];
} ModeInfo;

static ModeInfo *modes=NULL;
static int num_modes=-1;

// these two enums are for the treeview list of found acquisitions
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
  COL_STOP_LAT,
  COL_DURATION,
  COL_BEAMMODE,
  COL_ANGLE,
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

// Here is the treeview's storage structure
static GtkTreeModel *liststore = NULL;

// callback for the found acquisition grid, when a user clicks on a column
// heading to change the sort.  This is called by the GTK treeview code,
// with pointers to two rows -- we have to return which should go first
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

      g_free(date1_str);
      g_free(date2_str);
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

      g_free(coverage1_str);
      g_free(coverage2_str);
    }
    break;

    case SORTID_ORBIT_PATH:
    {
      char *str1, *str2;

      gtk_tree_model_get(model, a, COL_ORBIT_PATH, &str1, -1);
      gtk_tree_model_get(model, b, COL_ORBIT_PATH, &str2, -1);

      if (is_alos()) {
        // alos case
        // sort based on orbit/frame (actually, the path number)
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
      else {
        // non-alos case
        // sort just on orbit number (no path present)
        int orbit1, orbit2;
        sscanf(str1, "%d", &orbit1);
        sscanf(str2, "%d", &orbit2);

        if (orbit1 != orbit2) {
          ret = orbit1 > orbit2 ? 1 : -1;
        } else {
          ret = 0;
        }
      }

      g_free(str1);
      g_free(str2);
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

      g_free(str1);
      g_free(str2);
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

      g_free(start_lat1_str);
      g_free(start_lat2_str);
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

      g_free(duration1_str);
      g_free(duration2_str);
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
    // i.e., don't start at 0, start at 1
    for (i=1; i<MAX_POLYS; ++i)
      g_polys[i].n = 0;

    which_poly = 0;
    g_poly = &g_polys[which_poly];
}

static int s_ref_hour = 6;
static int s_ref_min = 6;
static int s_ref_sec = 17;

static int get_alos_orbit_number_at_time(double time)
{
  // KLUDGE: This should all be moved to a config file!
  double recurrent_period = 46;              // days
  double orbits_per_recurrent_period = 671;

  // reference: 26-Dec-2006, 6:15:34
  // reference orbit: 4904
  
  // We use a time slightly ahead of the reference time, to account
  // for the fact that this scene is not at the equator crossing.
  // Figured out the correct reference time by trial & error...

  ymd_date ref_ymd;
  hms_time ref_hms;

  ref_ymd.year = 2006;
  ref_ymd.month = 12;
  ref_ymd.day = 26;

  ref_hms.hour = s_ref_hour;
  ref_hms.min = s_ref_min;
  ref_hms.sec = s_ref_sec;

  int ref_orbit = 4904;

  // here on out are the actual calculations, no need to change
  // any of this if you just wanted to update the reference
  julian_date ref_jd;
  date_ymd2jd(&ref_ymd, &ref_jd);
  double ref = date2sec(&ref_jd, &ref_hms);

  double revolutions_per_day = orbits_per_recurrent_period/recurrent_period;
  double orbital_period = 24.*60.*60. / revolutions_per_day; // sec

  double revs_since_ref = (time - ref)/orbital_period;
  return (int)floor(ref_orbit + revs_since_ref);
}

static void update_look()
{
    int i = get_combo_box_item("mode_combobox");

    if (strlen(modes[i].allowed_look_angles) > 0) {
      assert(modes[i].look_max < 0 &&
             modes[i].look_min < 0 &&
             modes[i].look_incr < 0);

      show_widget("look_angle_entry_hbox", FALSE);
      show_widget("look_angle_combobox_hbox", TRUE);

      set_combo_box_item("look_angle_combobox", 0);
      clear_combobox("look_angle_combobox");

      char *p = modes[i].allowed_look_angles;
      while (p) {
        char *q = strchr(p+1, ',');
        if (q) *q = '\0';
        add_to_combobox("look_angle_combobox", p);
        if (q) *q = ',';
        p = q;
        if (p) ++p;
      }

      set_combo_box_item("look_angle_combobox", 0);
    }
    else {
      assert(strlen(modes[i].allowed_look_angles) == 0);
      
      show_widget("look_angle_entry_hbox", TRUE);
      show_widget("look_angle_combobox_hbox", FALSE);

      char buf[64];
      sprintf(buf, "(%.1f - %.1f)", modes[i].look_min, modes[i].look_max);
      put_string_to_label("look_angle_info_label", buf);
    }
}

// checkbutton callback -- updates the boolean entry in the row
// structure, which will then update the treeview (thus checking or
// unchecking the box)
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

  // refresh the main window, to show/hide the newly selected pass
  fill_big(curr);
}

// TLE info label is in the "setup" tab
static void populate_tle_info()
{
    char *tle_filename = find_in_share("tle");
    const char *tle_info = get_tle_info(tle_filename, planner_satellite);
    put_string_to_label("tle_info_label", tle_info);
}

static void populate_config_info()
{
    char *config_filename = find_in_share("planner.cfg");
    FILE *fp = NULL;

    if (config_filename)
        fp = fopen(config_filename, "r");

    char *output_dir, *output_file;
    int max_days;

    if (!fp) {
        // no config file -- default to the share dir
        output_dir = STRDUP(get_asf_share_dir());
        output_file = STRDUP("output.csv");
        max_days = 30;
    }
    else {
        char s[256], *junk;
        if (!fgets(s, 255, fp))
            strcpy(s,"output directory = ");
        split2(s, '=', &junk, &output_dir);
        if (strcmp_case(junk, "output directory")!=0) {
          printf("Invalid config file line 1, "
                 "does not specify output directory.\n");
          output_dir = STRDUP("");
        }
        free(junk);

        if (!fgets(s, 255, fp))
            strcpy(s,"output file = output.csv");
        split2(s, '=', &junk, &output_file);
        if (strcmp_case(junk, "output file")!=0) {
          printf("Invalid config file line 2, "
                 "does not specify output filename.\n");
          output_file = STRDUP("output.csv");
        }
        free(junk);

        char *tmp;
        if (!fgets(s, 255, fp))
            strcpy(s,"max days = 30");
        split2(s, '=', &junk, &tmp);
        if (strcmp_case(junk, "max days")!=0) {
          printf("Invalid config file line 3, "
                 "does not specify maximum days.\n");
          max_days = 30;
        }
        else {
          max_days = atoi(tmp);
        }
        free(junk);
        free(tmp);

        if (!fgets(s, 255, fp))
          strcpy(s,"reference time = 6:06:17");
        split2(s, '=', &junk, &tmp);
        if (strcmp_case(junk, "reference time")!=0) {
          printf("Invalid config file file 4, "
                 "does not specify reference time.\n");
          s_ref_hour = 6;
          s_ref_min = 6;
          s_ref_sec = 17;
        }
        else {
          sscanf(tmp, "%d:%d:%d", &s_ref_hour, &s_ref_min, &s_ref_sec);
        }
        free(junk);
        free(tmp);

        printf("Reference time: %02d:%02d:%02d\n",
               s_ref_hour, s_ref_min, s_ref_sec);

        fclose(fp);
    }

    put_string_to_entry("output_dir_entry", output_dir);
    free(output_dir);

    put_string_to_entry("output_file_entry", output_file);
    free(output_file);
    
    put_int_to_entry("max_days_entry", max_days);
}

static void select_planner_map()
{
    if (strstr(curr->filename, "land_shallow_topo") != NULL)
      set_combo_box_item("planner_map_combobox", 1);
    else if (strstr(curr->filename, "Arctic4_1800") != NULL)
      set_combo_box_item("planner_map_combobox", 2);
    else if (strstr(curr->filename, "moa125_hp1") != NULL)
      set_combo_box_item("planner_map_combobox", 3);
    else
      set_combo_box_item("planner_map_combobox", 0);
}

void setup_planner()
{
    in_planning_mode = TRUE;
    show_widget("planner_notebook", TRUE);
    show_widget("viewer_notebook", FALSE);

    // populate the "Mode" dropdown from the "beam_modes.txt" file
    char **names, **allowed_look_angles;
    double *min_looks, *max_looks, *look_incrs;
    get_all_beam_modes(planner_satellite, &num_modes,
                       &names, &min_looks, &max_looks, &look_incrs,
                       &allowed_look_angles);

    assert(num_modes>0);

    modes = MALLOC(sizeof(ModeInfo)*num_modes);
    int i;
    clear_combobox("mode_combobox");
    for (i=0; i<num_modes; ++i) {
        modes[i].mode = names[i];
        modes[i].look_min = min_looks[i];
        modes[i].look_max = max_looks[i];
        modes[i].look_incr = look_incrs[i];
        if (allowed_look_angles[i])
          strcpy(modes[i].allowed_look_angles, allowed_look_angles[i]);
        else
          strcpy(modes[i].allowed_look_angles, "");
        add_to_combobox("mode_combobox", names[i]);
    }

    // we don't free the pointed-to strings in the "names" array, they are now
    // owned by the static "modes" array.  This isn't true of the allowed
    // look angles array -- that one is a char buffer in the mode table
    FREE(names); 
    FREE(min_looks);
    FREE(max_looks);
    FREE(look_incrs);
    for (i=0; i<num_modes; ++i)
      FREE(allowed_look_angles[i]);
    FREE(allowed_look_angles);

    // by default select ... something
    set_combo_box_item("mode_combobox", 0);

    // by default search for both ascending and descending
    set_combo_box_item("orbit_direction_combobox", 0);

    // Now, setting up the "Found Acquisitions" table
    GtkTreeViewColumn *col;
    GtkCellRenderer *rend;

    GtkListStore *ls = gtk_list_store_new(
                           13,              // (# of columns)
                           GDK_TYPE_PIXBUF, // color pixbuf
                           G_TYPE_BOOLEAN,  // checkbox col
                           G_TYPE_STRING,   // date (string)
                           G_TYPE_STRING,   // date (# secs) -- hidden
                           G_TYPE_STRING,   // orbit/path
                           G_TYPE_STRING,   // coverage
                           G_TYPE_STRING,   // start lat
                           G_TYPE_STRING,   // stop lat -- hidden
                           G_TYPE_STRING,   // duration
                           G_TYPE_STRING,   // orbit direction
                           G_TYPE_STRING,   // beam mode -- hidden
                           G_TYPE_STRING,   // pointing angle -- hidden
                           G_TYPE_STRING);  // index (into g_polys) -- hidden

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

    // Column: Orbit/Path (ALOS)
    // Column: Orbit (other)
    // NOTE that if this is changed (for example, for other satellites
    // including a frame number), you'll need to update how the column
    // is populated and sorted elsewhere in this file (look for is_alos())
    col = gtk_tree_view_column_new();
    if (is_alos())
      gtk_tree_view_column_set_title(col, "Orbit/Path");
    else
      gtk_tree_view_column_set_title(col, "Orbit");
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

    // Column: Stop Latitude (currently not shown)
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Lat2");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_STOP_LAT);

    // Column: Duration
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Dur");
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

    // Column: Beam Mode (hidden)
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_BEAMMODE);

    // Column: Pointing Angle (hidden)
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(tv), col);
    rend = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, rend, TRUE);
    gtk_tree_view_column_add_attribute(col, rend, "text", COL_ANGLE);

    // Column: Index (hidden)
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

    // Hook up sort function to all the sortable rows, passing as "user_data"
    // the SORTID_ enum, indicating which column is the sort col
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

    // Select the appropriate map item!
    select_planner_map();

    // Kludge during testing...
    //g_polys[0].n = 1;
    //g_polys[0].line[0] = 1850;
    //g_polys[0].samp[0] = 3650;
    //g_polys[0].c = 0;
    //crosshair_line = 5465;
    //crosshair_samp = 12774;
    zoom = 1;
    //center_line = (crosshair_line + g_polys[0].line[0])/2;
    //center_samp = (crosshair_samp + g_polys[0].samp[0])/2;
    //center_line = crosshair_line;
    //center_samp = crosshair_samp;
    //set_combo_box_item("mode_combobox", 1);
    //put_string_to_entry("lat_min_entry", "-3.16");
    //put_string_to_entry("lat_max_entry", "0.8");
    //put_string_to_entry("lon_min_entry", "31.11");
    //put_string_to_entry("lon_max_entry", "34.91");
    //put_string_to_entry("start_date_entry", "20070814");
    //put_string_to_entry("end_date_entry", "20070820");
    char curr_date_str[32];
    long default_start = current_date();
    sprintf(curr_date_str, "%ld", default_start);
    put_string_to_entry("start_date_entry", curr_date_str);
    long default_end = add_days(default_start, 5);
    sprintf(curr_date_str, "%ld", default_end);
    put_string_to_entry("end_date_entry", curr_date_str);
    //put_string_to_entry("look_angle_entry", "28");
    // ... all this should be deleted

    // populate the "setup" tab's values
    populate_tle_info();
    populate_config_info();

    // redo the title to reflect that this is now a planner app
    GtkWidget *widget = get_widget_checked("ssv_main_window");

    char title[512];
    sprintf(title, "ASF ALOS Acqusition Planner - Version %s",
            AP_VERSION_STRING);
    gtk_window_set_title(GTK_WINDOW(widget), title);

    // take the "Help" text in the planner from the regular planner
    GtkWidget *help_label = get_widget_checked("help_label");
    put_string_to_label("planner_help_label",
                        gtk_label_get_text(GTK_LABEL(help_label)));

    // update look angle info label
    update_look();
}

void highlight_row(int row, int highlight)
{
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
        g_free(index_str);

        if (index == row) {
          // found the row we are looking for
          GtkTreeSelection *selection;
          GtkWidget *tv = get_widget_checked("treeview_planner");
          selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tv));
          if (highlight)
            gtk_tree_selection_select_iter(selection, &iter);
          else
            gtk_tree_selection_unselect_iter(selection, &iter);
          break;
        }

        valid = gtk_tree_model_iter_next(liststore, &iter);
      }
    }
  }
}

// this one is customized for the way the planner stores the matches,
// which has 5 points per frame:
//  0-4:   frame #1 (#4 is a duplicate of #0)
//  5-9:   frame #2 (#9 is a duplicate of #1)
//  10-14: frame #3 (#10 is a duplicate of #14)
//  15-19: etc
// they are drawn as one polygon, however this is a little bit of a
// cheat -- they are really a bunch of polygons in a row.  For the
// overlap test, we use the real frames to avoid any weirdness resulting
// from the conjoined polygon being re-entrant, etc.  Also, this
// should be a teeny bit faster...
static int poly_contains(UserPolygon *p, double line, double samp)
{
  int k, i, j, c = 0;
  for (k=0; k<p->n;  k+= 5) {
    for (i = k, j = k+3; i < k+4; j = i++) {
      if ((((p->line[i]<=line) && (line<p->line[j])) ||
           ((p->line[j]<=line) && (line<p->line[i]))) &&
          (samp < (p->samp[j] - p->samp[i]) * (line - p->line[i]) /
           (p->line[j] - p->line[i]) + p->samp[i]))
        c = !c;
    }
    if (c) return c;
  }
  return c;
}

void planner_click(int line, int samp)
{
  // this is called when the user has clicked somewhere in the image
  int i;

  for (i=0; i<MAX_POLYS; ++i) {
    if (g_polys[i].n > 1 && row_is_checked(i)) {
      highlight_row(i, poly_contains(&g_polys[i], line, samp));
    }
  }
}

// returns true if that row # in the found acquisitions is checked
// return false if we aren't running the planner, or the row # is
// not checked
int row_is_checked(int row)
{
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
        g_free(index_str);

        if (index == row) {
          // found the row we are looking for
          ret = selected;
          break;
        }

        valid = gtk_tree_model_iter_next(liststore, &iter);
      }
    }
  }

  return ret;
}

// Warning: ALOS specific!
// Not sure if other satellites even use a "path number" though
static int revolution2path(int revolution)
{
    return ((46*revolution+85)%671);
}

static int calibrate_pass_impl(long startdate, long enddate,
                               double lo_lat, double hi_lat,
                               double lo_lon, double hi_lon)
{
  double clat = .5*(hi_lat + lo_lat);
  double clon = .5*(hi_lon + lo_lon);
  char *tle_filename = find_in_share("tle");
  const char *sat = "ALOS";
  const char *beam = "PLR";
  double look = 21.5;
  int zone = utm_zone(clon);

  double x[4], y[4];
  ll2pr(lo_lat, lo_lon, zone, &x[0], &y[0]);
  ll2pr(lo_lat, hi_lon, zone, &x[1], &y[1]);
  ll2pr(hi_lat, hi_lon, zone, &x[2], &y[2]);
  ll2pr(hi_lat, lo_lon, zone, &x[3], &y[3]);
  Poly *aoi = polygon_new_closed(4, x, y);

  char *err=NULL;
  PassCollection *pc;
  int path;

  int n = plan(sat, beam, look*D2R, startdate, enddate, hi_lat, lo_lat,
               clat, clon, ASCENDING_ONLY, zone, aoi, tle_filename, &pc, &err);

  if (!err && n==1) {
    PassInfo *pi = pc->passes[0];
    int orbit = get_alos_orbit_number_at_time(pi->start_time);
    path = revolution2path(orbit);
  }
  else {
    printf("Found more than 1 result - no test. %d\n", n);
    path=-1;
  }

  return path;
}

const char *ref_str(int ref)
{
  int h = ref/3600;
  int m = (ref - h*3600)/60;
  int s = ref - 60*(m+h*60);

  static char buf[64];
  sprintf(buf, "%02d:%02d:%02d", h, m, s);
  return buf;
}

typedef struct {
    long startdate, enddate;
    double lo_lat1, hi_lat1;
    double lo_lon1, hi_lon1;
    int expected_path1, expected_path2;
    double lo_lat2, hi_lat2;
    double lo_lon2, hi_lon2;
} PassEvalInfo;

static int eval_pass_at(int m_and_s,
                        PassEvalInfo *p)
{
  int m = m_and_s/60;
  int s = m_and_s - m*60;

  s_ref_min = m;
  s_ref_sec = (double)s;

  int path1 = calibrate_pass_impl(p->startdate, p->enddate,
                                  p->lo_lat1, p->hi_lat1,
                                  p->lo_lon1, p->hi_lon1);
  int path2 = calibrate_pass_impl(p->startdate, p->enddate,
                                  p->lo_lat2, p->hi_lat2,
                                  p->lo_lon2, p->hi_lon2);

  //printf("%2d:%02d -- %3d %3d -- %3d,%3d\n",
  //       m, s, path1, p->expected_path1, path2, p->expected_path2);

  if (path1!=p->expected_path1 && path2!=p->expected_path2) {
    // we don't expect this to happen...
    asfPrintWarning("Unexpected path values: %d != %d && %d != %d\n",
                    path1, p->expected_path1, path2, p->expected_path2);
    return -99;
  }

  // normal cases
  else if (path1==p->expected_path1 && path2!=p->expected_path2)
    return -1;
  else if (path1!=p->expected_path1 && path2==p->expected_path2)
    return 1;
  else
    return 0;
}

static const char *ms2str(int m_and_s)
{
  static char buf[64];
  
  int m = m_and_s/60;
  int s = m_and_s - m*60;

  sprintf(buf, "%2d:%02d", m, s);

  return buf;
}

static void calibrate_pass(long startdate, long enddate,
                           double lo_lat1, double hi_lat1,
                           double lo_lon1, double hi_lon1,
                           int expected_path1,
                           double lo_lat2, double hi_lat2,
                           double lo_lon2, double hi_lon2,
                           int expected_path2,
                           int *ref_s)
{
  PassEvalInfo p;
  p.startdate = startdate;
  p.enddate = enddate;
  p.lo_lat1 = lo_lat1;
  p.hi_lat1 = hi_lat1;
  p.lo_lon1 = lo_lon1;
  p.hi_lon1 = hi_lon1;
  p.lo_lat2 = lo_lat2;
  p.hi_lat2 = hi_lat2;
  p.lo_lon2 = lo_lon2;
  p.hi_lon2 = hi_lon2;
  p.expected_path1 = expected_path1;
  p.expected_path2 = expected_path2;

  int m, s;

  int prev_quietflag=quietflag;
  quietflag=TRUE;

  int fs, ls;

  // search the interval 06:03:00 to 06:10:00 ... we may have to widen
  // this at some point...

  int interval_lo = 0*60;
  int val_lo = eval_pass_at(interval_lo, &p);

  int interval_hi = 20*60;
  int val_hi = eval_pass_at(interval_hi, &p);

  if (val_lo == val_hi) {
    asfPrintWarning("Unable to calibrate using the fast method!\n"
                    "Falling back to the old (slow!) search...\n");

    int first_m = -1;
    int first_s = -1;
    int last_m = -1;
    int last_s = -1;

    // use linear search
    for (m=3; m<=9; ++m) {
      for (s=0; s<=59; ++s) {
        
        s_ref_min = m;
        s_ref_sec = (double)s;
        
        int path1 = calibrate_pass_impl(startdate, enddate, lo_lat1, hi_lat1,
                                        lo_lon1, hi_lon1);
        int path2 = calibrate_pass_impl(startdate, enddate, lo_lat2, hi_lat2,
                                        lo_lon2, hi_lon2);
        
        printf("%d:%02d --> %3d %3d, %3d %3d\n", m, s,
               path1, expected_path1, path2, expected_path2);

        if (path1==expected_path1 && path2==expected_path2) {
          last_m=m; last_s=s;
          if (first_m==-1) {
            first_m=m; first_s=s;
          }
        }
      }
    }

    fs = first_s + 60*first_m;
    ls = last_s + 60*last_m;
  }
  else {
    // use binary search
    int niter=0;
    while (1) {
      ++niter;
      if (niter>100) {
        asfPrintWarning("Failed to converge!\nGiving up.\n");
        *ref_s = -1;
        return;
      }

      printf("%d: Searching interval: %s", niter, ms2str(interval_lo));
      printf(" -- %s\n", ms2str(interval_hi));

      int interval_mid = (interval_lo + interval_hi)/2;
      int val_mid = eval_pass_at(interval_mid, &p);

      // we're taking advantage of the fact that eval_pass_at return -1,0,1
      if (interval_lo+1 == interval_hi) {
        // this can happen if the range of agreement is really small
        fs = interval_lo;
        ls = interval_hi;
        break;
      }
      else if (val_mid == val_lo) {
        // redo with reduced interval
        interval_lo = interval_mid;
      }
      else if (val_mid == val_hi) {
        // reto with reduced interval
        interval_hi = interval_mid;
      }
      else {
        // found a place were it agrees... find the bounds
        printf("Found a match... looking for the bounds.\n");

        printf("Looking for lower bound...\n");
        fs = interval_mid;
        int val = val_mid;
        while (val==0) {          
          //printf("Checking %s...\n", ms2str(fs-1));
          val = eval_pass_at(fs-1, &p);
          --fs;
        }
        printf("Found %s.\nLooking for upper bound...\n", ms2str(fs));
        ls = interval_mid;
        val = val_mid;
        while (val==0) {
          //printf("Checking %s...\n", ms2str(ls+1));
          val = eval_pass_at(ls+1, &p);
          ++ls;
        }
        printf("Found %s.\n", ms2str(ls));

        break;
      }
    }
  } 

  int bs = (fs+ls)/2;

  int best_h = 6;
  int best_m = bs/60;
  int best_s = bs - best_m*60;

  *ref_s = best_s + 60*(best_m + 60*best_h);
  
  quietflag=prev_quietflag;
}

void calibrate_planner_reference()
{
  printf("Calibrating reference time ... \n");
  int ref1, ref2, ref3, ref4;

  printf("Area 1... (longitude -150)\n");
  calibrate_pass(20090315, 20090315,
                 1.24, 1.54, -150.55, -150.13, 239,
                 1.77, 2.09, -150.62, -150.22, 285, &ref1);
  printf("Reference for 1: %s\n\n", ref_str(ref1));

  printf("Area 2... (longitude 30)\n");
  calibrate_pass(20090306, 20090306,
                 0.94, 1.19, 29.81, 30.18, 574,
                 1.49, 2.86, 29.64, 30.01, 620, &ref2);
  printf("Reference for 2: %s\n\n", ref_str(ref2));

  printf("Area 3... (longitude 109)\n");
  calibrate_pass(20090320, 20090320,
                 1.23, 1.55, 109.17, 109.55, 426,
                 1.86, 2.14, 109.06, 109.43, 472, &ref3);
  printf("Reference for 3: %s\n\n", ref_str(ref3));

  printf("Area 4... (longitude -54)\n");
  calibrate_pass(20090325, 20090325,
                 0.72, 1.00, -54.85, -54.45, 61,
                 1.23, 1.55, -54.98, -54.62, 107, &ref4);
  printf("Reference for 4: %s\n\n", ref_str(ref4));

  int ref=0,ok=TRUE;
  if (ref1>0 && ref2>0 && ref3>0 && ref4>0) {
    ref = (int)(.5+((double)(ref1+ref2+ref3+ref4))/4.0);
  }
  else {
    // some results were invalid!
    int num_valid = 0;
    if (ref1>0) {
      num_valid++;
      ref += ref1;
    }
    if (ref2>0) {
      num_valid++;
      ref += ref2;
    }
    if (ref3>0) {
      num_valid++;
      ref += ref3;
    }
    if (ref4>0) {
      num_valid++;
      ref += ref4;
    }
    if (num_valid>0) {
      ref = (int)(.5 + (double)ref/(double)num_valid);
    }
    else {
      asfPrintWarning("Failed to calibrate the reference time.\n");
      ok=FALSE;
    }
  }

  if (ok) {
    printf("New reference time: %s\n", ref_str(ref));

    s_ref_hour = ref/3600;
    s_ref_min = (ref - s_ref_hour*3600)/60;
    s_ref_sec = ref - 60*(s_ref_min+s_ref_hour*60);

    printf("Writing configuration file...\n");
    printf("Share dir: %s\n", get_asf_share_dir());

    const char *cfgfile = "planner.cfg";
    char *full_config_filename = find_in_share(cfgfile);
    printf("Full: %s\n", full_config_filename);
    if (fileExists(full_config_filename)) {
      // file exists already, update entries with new reference
      FILE *fp = fopen_share_file(cfgfile, "r");
      if (!fp) {
        printf("Could not open configuration file!\n"
               "  %s\n"
               "  %s\n", full_config_filename, strerror(errno));
      }
      else {
        char lines[10][256];
        int l = 0;
        
        while (fgets(lines[l], 255, fp) != NULL) {
          if (strncmp(lines[l], "reference time", 14)==0)
            sprintf(lines[l], "reference time = %02d:%02d:%02d\n",
                    s_ref_hour, s_ref_min, s_ref_sec);
          if (++l>=10)
            asfPrintError("Config file is too long!\n");
        }
        fclose(fp);
        
        fp = fopen_share_file(cfgfile, "w");
        if (!fp) {
          printf("Could not open configuration file!\n"
                 "  %s\n"
                 "  %s\n", full_config_filename, strerror(errno));
        }
        else {
          int c = 0;
          while (c<l) {
            fprintf(fp, "%s", lines[c++]);
          }
          fclose(fp);
        }
      }

      printf("Updated reference time in the configuration file.\n");
    }
    else {
      // file does not exist yet, populate with defaults
      FILE *ofp = fopen_share_file(cfgfile, "w");
      if (!ofp) {
        printf("Could not open configuration file!\n"
               "  %s\n"
               "  %s\n", full_config_filename, strerror(errno));
      }
      else {
        fprintf(ofp, "output directory = \n");
        fprintf(ofp, "output file = output.csv\n");
        fprintf(ofp, "max days = 30\n");
        fprintf(ofp, "reference time = %02d:%02d:%02d\n",
                s_ref_hour, s_ref_min, s_ref_sec);
        fclose(ofp);
      }
      
      printf("Wrote new configuration file with reference time.\n");
    }
    
    printf("Calibration complete.\n");
  }
}

SIGNAL_CALLBACK void on_plan_button_clicked(GtkWidget *w)
{
    int i,j,pass_type,zone;
    char errstr[1024];
    double max_lat, min_lat, clat, clon;
    Poly *aoi;

    // use the projection appropriate for the map that is selected
    i = get_combo_box_item("planner_map_combobox");
    switch (i) {
      default:
      case 0: // "-"
      case 1: // "Standard"
        zone = 0; // will be determined by the center longitude of aoi
        break;
      case 2: // "north pole"
        zone = 999;
        break;
      case 3: // "south pole"
        zone = -999;
        break;
    }

    // gather up error mesages in "errstr"
    // at the end, if errstr is non-empty, we can't do the planning
    strcpy(errstr, "");

    // beam mode
    i = get_combo_box_item("mode_combobox");
    char *beam_mode = modes[i].mode;
    printf("\nPlanning Mode: %s\n", beam_mode);

    // look angle
    // have to figure out which widget to check -- entry, or combo?
    // to do that, check if the allowed look angles string is empty
    double look_angle=-999;
    if (strlen(modes[i].allowed_look_angles) > 0) {
      int look_index = get_combo_box_item("look_angle_combobox");
      int current_index = 0;
      // scan through the allowed look angle string to find the one with
      // the index that matches the drop-down
      char *p = modes[i].allowed_look_angles;
      while (p) {
        char *q = strchr(p+1, ',');
        if (q) *q = '\0';
        if (current_index == look_index) {
          look_angle = atof(p);
          if (q) *q = ',';
          break;
        }
        if (q) *q = ',';
        p = q;
        if (p) ++p;
        ++current_index;
      }
      // Make sure all that string stuff worked
      if (look_angle == -999)
        strcat(errstr, "Internal error: Couldn't find selected look angle.\n");
      else
        look_angle *= D2R;
    }
    else {
      look_angle = get_double_from_entry("look_angle_entry");
      if (look_angle < modes[i].look_min)
        strcat(errstr, "Look angle smaller than minimum.\n");
      else if (look_angle > modes[i].look_max)
        strcat(errstr, "Look angle larger than maximum.\n");
      else
      {
        // clamp to nearest allowed look angle (per increment)
        j=0;
        double a = modes[i].look_min, min_diff=999, closest=-999;
        while (a < modes[i].look_max + .0001) {
          a = modes[i].look_min + (double)j*modes[i].look_incr;
          double diff = fabs(look_angle - a);
          if (diff<min_diff) {
            min_diff = diff;
            closest = a;
          }
          ++j;
        }
        assert(closest != -999);
        //printf("Look angle: %.8f -> %.8f\n", look_angle, closest);
        put_double_to_entry_fmt("look_angle_entry", closest, "%.1f");
        look_angle = closest*D2R;
      }
    }
    printf("Look angle: %.2f\n", look_angle*R2D);

    // get the start/end dates
    long startdate = (long)get_int_from_entry("start_date_entry");
    if (!is_valid_date(startdate))
      strcat(errstr, "Invalid start date.\n");

    long enddate = (long)get_int_from_entry("end_date_entry");
    if (!is_valid_date(enddate))
      strcat(errstr, "Invalid end date.\n");

    int max_days = get_int_from_entry("max_days_entry");
    if (date_diff(startdate, enddate) > max_days) {
      char tmp[64];
      sprintf(tmp, "Date range too large (%d days max)\n", max_days);
      strcat(errstr, tmp);
    }

    // collect together the polygon, calculate min_lat, etc
    meta_parameters *meta = curr->meta;
    if (!(meta->projection || (meta->sar&&meta->state_vectors) ||
          meta->transform || meta->airsar))
    {
      strcat(errstr, "Image has no geolocation information.\n");
      max_lat = min_lat = 0;
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
        max_lat = min_lat = 0;
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

        if (zone == 0)
          zone = utm_zone((lon1+lon2)/2.);

        // now get all corner points into UTM
        ll2pr(lat1, lon1, zone, &x[0], &y[0]);
        ll2pr(lat2, lon2, zone, &x[2], &y[2]);

        double lat, lon;
        meta_get_latLon(meta, g_poly->line[0], crosshair_samp, 0,
                        &lat, &lon);
        ll2pr(lat, lon, zone, &x[1], &y[1]);

        meta_get_latLon(meta, crosshair_line, g_poly->samp[0], 0,
                        &lat, &lon);
        ll2pr(lat, lon, zone, &x[3], &y[3]);

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

        // center point determines which UTM zone to use (if we
        // are using UTM -- if we're in polar stereo, doesn't matter)
        if (zone == 0) {
          zone = utm_zone(clon);
          printf("Center lat/lon: %f, %f (Zone %d)\n", clat, clon, zone);
        }
        else {
          printf("Center lat/lon: %f, %f (Polar Stereo %s)\n", clat, clon,
                 zone > 0 ? "North" : "South");
        }

        // now the second pass -- determining the actual coordinates
        // of the polygon in the right UTM zone, or the right n/s projection
        meta_get_latLon(meta, crosshair_line, crosshair_samp, 0, &lat, &lon);
        ll2pr(lat, lon, zone, &x[0], &y[0]);

        for (i=0; i<g_poly->n; ++i) {
          meta_get_latLon(meta, g_poly->line[i], g_poly->samp[i], 0,
                          &lat, &lon);
          ll2pr(lat, lon, zone, &x[i+1], &y[i+1]);
        }

        // now we can finally create the polygon
        aoi = polygon_new_closed(g_poly->n+1, x, y);
      }
    }

    // make sure polygon is not "too small" to plan in
    if (aoi && polygon_area(aoi) < 100)
      sprintf(errstr, "%sArea of interest is too small (%.0f km)\n",
              errstr, polygon_area(aoi));

    // pass type
    i = get_combo_box_item("orbit_direction_combobox");
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
      // everything seems to be ok, can start the search...

      // update the GUI to let the user know we're looking
      put_string_to_label("plan_error_label", "Searching...");
      enable_widget("plan_button", FALSE);
      while (gtk_events_pending())
        gtk_main_iteration();

      char *err;
      PassCollection *pc;

      // here's the call that performs the search
      int n = plan(planner_satellite, beam_mode, look_angle,
                   startdate, enddate, max_lat, min_lat, clat, clon,
                   pass_type, zone, aoi, tle_filename, &pc, &err);

      if (n < 0) {
        put_string_to_label("plan_error_label", err);
        free(err);
      }
      else {
        // search was successful, convert the returned "pass collection"
        // into polygons for display, and populate the match list
        strcpy(errstr, "");

        char msg[256];
        sprintf(msg, "Found %d match%s.\n", n, n==1?"":"es");
        asfPrintStatus(msg);
        put_string_to_label("plan_error_label", msg);
        strcpy(errstr, msg);

        for (i=0; i<pc->num; ++i) {
          asfPrintStatus("#%02d: %s (%.1f%%)\n", i+1,
                         pc->passes[i]->start_time_as_string,
                         100. * pc->passes[i]->total_pct);
        }

        // this is for debugging, can be removed
        //pass_collection_to_kml(pc, "test_kml.kml");
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
            Poly *poly = oi->viewable_region;

            int j;
            for (j=0; j<poly->n; ++j) {
              if (m >= MAX_POLY_LEN) {
                printf("Reached maximum polygon length!\n");
                printf("--> Pass number %d\n", i);
                printf("    Number of polygons: %d\n", pi->num);
                break;
              } else {
                double samp, line;
                proj2lineSamp(meta, zone, poly->x[j], poly->y[j], 0,
                              &line, &samp);
                //printf("%d,%d -- %f,%f\n",i,m,line,samp);
                g_polys[i+1].line[m]=line;
                g_polys[i+1].samp[m]=samp;
                ++m;
              }
            }
          }

          g_polys[i+1].n=m;
          g_polys[i+1].c=m-1;

          g_polys[i+1].show_extent=FALSE;

          // debug: compare our duration with what alos_time returns
          // this won't work for passes over the pole, but since it is
          // just for debugging we don't really care...
          //int odir = pi->dir=='D' ? 1 : 0;
          //printf("%f %f\n", pi->duration, alos_time(pi->start_lat, odir,
          //                                          pi->stop_lat, odir));

          // get ready to add this one to the list, create each column's value
          char date_info[256];
          sprintf(date_info, "%s", pi->start_time_as_string);

          char date_hidden_info[256];
          sprintf(date_hidden_info, "%f", pi->start_time);

          int orbit;
          if (is_alos()) {
            // ALOS specific calculation of the orbit number, where we just
            // take a known reference orbit number, and do a simple period-
            // based division to get the current orbit number.
            orbit = get_alos_orbit_number_at_time(pi->start_time);
          }
          else {
            // use the orbit number from the TLE calc
            // not sure how good this number is -- seems to work 70% of
            // the time for ALOS (that's why we had to change to the above
            // calculation), for other satellites it could be better, or
            // worse!
            orbit = pi->orbit;
          }

          char orbit_info[256];

          if (is_alos()) {          
            // ALOS specific thing here-- orbit/path is the column contents
            int path = revolution2path(orbit);

            //sprintf(orbit_info, "%.2f/%d", orbit + pi->orbit_part, path);
            sprintf(orbit_info, "%d/%d", orbit, path);
          }
          else {
            // non-ALOS, don't give a path, orbit number only appears in
            // the table
            sprintf(orbit_info, "%d", orbit);
          }

          char pct_info[256];
          sprintf(pct_info, "%.1f", 100.*pi->total_pct);

          // the index in the "g_polys" array which corresponds to this row
          char index_info[256];
          sprintf(index_info, "%d", i+1);

          // start latitude
          char start_lat_info[256];
          sprintf(start_lat_info, "%.2f", pi->start_lat);

          // stop latitude
          char stop_lat_info[256];
          sprintf(stop_lat_info, "%.2f", pi->stop_lat);

          // duration
          char duration_info[256];
          sprintf(duration_info, "%.1f", pi->duration);

          // ascending or descending
          char orbit_dir[5];
          sprintf(orbit_dir, "%c", pi->dir);

          // pointing angle
          char pointing_angle_info[64];
          sprintf(pointing_angle_info, "%.1f", look_angle*R2D);

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
                             COL_START_LAT, start_lat_info,
                             COL_STOP_LAT, stop_lat_info,
                             COL_DURATION, duration_info,
                             COL_ORBITDIR, orbit_dir,
                             COL_BEAMMODE, beam_mode,
                             COL_ANGLE, pointing_angle_info,
                             COL_INDEX, index_info,
                             -1);

          if (i>=MAX_POLYS-2) {
            sprintf(errstr, "Too many matches: %d (max is %d)\n",
                    pc->num, MAX_POLYS-2);
            printf("%s", errstr);
            break;
          }
        }

        pass_collection_free(pc);

        // switch to planning results tab
        GtkWidget *nb = get_widget_checked("planner_notebook");
        gtk_notebook_set_current_page(GTK_NOTEBOOK(nb), 1);
        clear_nb_callback();

        if (strlen(errstr)>0)
          put_string_to_label("plan_result_error_label", errstr);
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

    if (strlen(output_file)==0)
      output_file = STRDUP("output.csv");

    //static char out_name[1024];
    //assert(strlen(output_dir)+strlen(output_file) < 1000);

    // The 25 characters of padding is in the case where both are left
    // blank, we still need to have room for the default output filename
    char *out_name = MALLOC(sizeof(char) *
                            (strlen(output_dir) + strlen(output_file) + 25));

    if (strlen(output_dir) > 0)
      sprintf(out_name, "%s/%s", output_dir, output_file);
    else
      strcpy(out_name, output_file);

    FILE *ofp = fopen(out_name, "w");
    if (!ofp) {
      message_box("Could not open output CSV file!\n"
                  "  %s\n"
                  "  %s\n", out_name, strerror(errno));
      return;
    }

    int num = 0;
    GtkTreeIter iter;

    fprintf(ofp, 
            "Year,Month,Day,Hour,Minute,Second,Duration,Start Latitude,"
            "Stop Latitude,Orbit,Path,Direction,Pointing Angle,Beam Mode,"
            "Coverage\n");

    gboolean valid = gtk_tree_model_get_iter_first(liststore, &iter);
    while (valid)
    {
        char *dbl_date_str, *coverage_str, *orbit_path_str,
             *start_lat_str, *stop_lat_str, *duration_str,
             *orbit_dir_str, *beam_mode_str, *pointing_angle_str;
        gboolean enabled;
        gtk_tree_model_get(liststore, &iter,
                           COL_SELECTED, &enabled,
                           COL_DATE_HIDDEN, &dbl_date_str,
                           COL_COVERAGE, &coverage_str,
                           COL_START_LAT, &start_lat_str,
                           COL_STOP_LAT, &stop_lat_str,
                           COL_DURATION, &duration_str,
                           COL_ORBITDIR, &orbit_dir_str,
                           COL_ORBIT_PATH, &orbit_path_str,
                           COL_BEAMMODE, &beam_mode_str,
                           COL_ANGLE, &pointing_angle_str,
                           -1);

        ymd_date ymd;
        julian_date jd;
        hms_time hms;
        double date = atof(dbl_date_str);
        sec2date(date, &jd, &hms);
        date_jd2ymd(&jd, &ymd);

        double start_lat = atof(start_lat_str);
        double stop_lat = atof(stop_lat_str);
        double dur = atof(duration_str);
        double cov = atof(coverage_str);
        double pointing_angle = atof(pointing_angle_str);

        int orbit, path;
        sscanf(orbit_path_str, "%d/%d", &orbit, &path);

        if (enabled)
        {
          //printf("Saving: %s %.1f%% %.2f %.1f %d/%d\n",
          //       date_str_long(date), cov, lat, dur, orbit, path);

          fprintf(ofp,
                  "%d"     // year
                  ",%d"    // month
                  ",%d"    // day
                  ",%d"    // hour
                  ",%d"    // minute
                  ",%.1f"  // second
                  ",%.1f"  // duration
                  ",%.2f"  // start latitude
                  ",%.2f"  // stop latitude
                  ",%d"    // orbit
                  ",%d"    // path
                  ",%s"    // orbit direction
                  ",%.1f"  // pointing angle
                  ",%s"    // beam mode
                  ",%.1f"  // coverage pct
                  "\n",
                  ymd.year,ymd.month,ymd.day,hms.hour,hms.min,
                  hms.sec,dur,start_lat,stop_lat,orbit,path,
                  orbit_dir_str,pointing_angle,beam_mode_str,cov);

          ++num;
        }

        g_free(dbl_date_str);
        g_free(coverage_str);
        g_free(start_lat_str);
        g_free(stop_lat_str);
        g_free(duration_str);
        g_free(orbit_path_str);
        g_free(orbit_dir_str);
        g_free(beam_mode_str);
        g_free(pointing_angle_str);

        valid = gtk_tree_model_iter_next(liststore, &iter);
    }

    fclose(ofp);

    if (num==0)
        asfPrintStatus("Empty output file.\n");
    else
        asfPrintStatus("Saved %d acquisition%s.\n", num, num==1?"":"s");

    open_csv(out_name);

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
    const char *cfg_filename = "planner.cfg";
    FILE *ofp = fopen_share_file(cfg_filename, "w");
    if (!ofp) {
      message_box("Could not open configuration file!\n"
                  "  %s\n"
                  "  %s\n", cfg_filename, strerror(errno));
    }
    else {
        fprintf(ofp, "output directory = %s\n",
                get_string_from_entry("output_dir_entry"));
        fprintf(ofp, "output file = %s\n",
                get_string_from_entry("output_file_entry"));
        fprintf(ofp, "max days = %d\n",
                get_int_from_entry("max_days_entry"));
        fprintf(ofp, "reference time = %02d:%02d:%02d\n",
                s_ref_hour, s_ref_min, s_ref_sec);
        fclose(ofp);
    }
}

SIGNAL_CALLBACK void on_beam_mode_combobox_changed(GtkWidget *w)
{
    update_look();
}

SIGNAL_CALLBACK void on_switch_map_button_clicked(GtkWidget *w)
{
    int i = get_combo_box_item("planner_map_combobox");
    int width=-1, height=-1;

    switch (i) {
      default:
      case 0:
        // "-" -> just revert back to what's already loaded
        select_planner_map();
        break;

      case 1:
        // "Standard"
        printf("Loading standard map...\n");
        load_file("land_shallow_topo_21600.jpg");
        width=320; height=256;
        break;

      case 2:
        // "North Pole"
        printf("Loading north pole map...\n");
        load_file("Arctic4_1800.tif");
        width=320; height=320;
        break;

      case 3:
        // "South Pole"
        printf("Loading south pole map...\n");
        load_file("moa125_hp1.tif");
        width=320; height=320;
        break;
    }

    if (width>0 && height>0) {
      GtkWidget *small_img = get_widget_checked("small_image");
      gtk_widget_set_size_request(small_img, width, height);
    } 
}

#ifndef win32
static GtkFileChooser *export_jpeg_widget = NULL;

// We call these export_jpeg, but we're really exporting PNGs on Linux
void export_jpeg_cancel_clicked()
{
  gtk_widget_hide(GTK_WIDGET(export_jpeg_widget));
}

void export_jpeg_ok_clicked()
{
  GSList *files = NULL;

  if (export_jpeg_widget) {
      files = gtk_file_chooser_get_filenames(export_jpeg_widget);
  }

  gtk_widget_hide(GTK_WIDGET(export_jpeg_widget));

  if (files) {
    static const int len = 2048; // max message length
    char msg[len];

    char *file = MALLOC(sizeof(char)*(strlen((char*)files->data)+25));
    strcpy(file, (char*)files->data);

    if (strlen(file) > 0) {
      append_ext_if_needed(file, ".png", ".png");

      GdkPixbuf *pb = make_big_image(curr, FALSE);
      GError *err = NULL;

      gdk_pixbuf_save(pb, file, "png", &err, NULL);

      if (err) {
        snprintf(msg, len, "Error exporting PNG: %s\n", err->message);
        printf("%s", msg);
      }
      else {
        snprintf(msg, len, "Exported PNG: %s\n", file);
      }

      g_object_unref(pb);
    }
    else {
      sprintf(msg, "Error exporting PNG: Zero length filename found.");
    }

    free(file);
    message_box(msg);

    g_slist_free(files);
  }
}

// sets up the save file chooser dialog
static void create_export_jpeg_file_chooser_dialog()
{
  GtkWidget *parent = get_widget_checked("ssv_main_window");

  export_jpeg_widget = GTK_FILE_CHOOSER(gtk_file_chooser_dialog_new(
      "Export PNG (*.png) File", GTK_WINDOW(parent),
      GTK_FILE_CHOOSER_ACTION_SAVE,
      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,  //Cancel button
      GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,    //Save button
      NULL));

  // we need to extract the buttons, so we can connect them to our
  // button handlers, above
  GtkHButtonBox *box =
      (GtkHButtonBox*)(((GtkDialog*)export_jpeg_widget)->action_area);
  GList *buttons = box->button_box.box.children;

  GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
  GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

  g_signal_connect((gpointer)cancel_btn, "clicked",
                    G_CALLBACK(export_jpeg_cancel_clicked), NULL);
  g_signal_connect((gpointer)ok_btn, "clicked",
                    G_CALLBACK(export_jpeg_ok_clicked), NULL);

  // add the filters
  GtkFileFilter *png_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(png_filt, "PNG Files (*.png)");
  gtk_file_filter_add_pattern(png_filt, "*.png");
  gtk_file_chooser_add_filter(export_jpeg_widget, png_filt);

  GtkFileFilter *all_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(all_filt, "All Files (*.*)");
  gtk_file_filter_add_pattern(all_filt, "*");
  gtk_file_chooser_add_filter(export_jpeg_widget, all_filt);

  // we need to make these modal -- if the user opens multiple "open"
  // dialogs, we'll get confused on the callbacks
  gtk_window_set_modal(GTK_WINDOW(export_jpeg_widget), TRUE);
  gtk_window_set_destroy_with_parent(GTK_WINDOW(export_jpeg_widget), TRUE);
  gtk_dialog_set_default_response(GTK_DIALOG(export_jpeg_widget),
                                  GTK_RESPONSE_OK);
}
#endif

SIGNAL_CALLBACK void on_export_jpeg_button_clicked(GtkWidget *w)
{
  char *output_dir = STRDUP(get_string_from_entry("output_dir_entry"));
  char *output_file = STRDUP("test");
  char *out_name = MALLOC(sizeof(char) *
                          (strlen(output_dir) + strlen(output_file) + 25));

  if (strlen(output_dir) > 0)
    sprintf(out_name, "%s/%s", output_dir, output_file);
  else
    strcpy(out_name, output_file);

#ifdef win32
  int retval;

  static const int len = 2048;
  char msg[len];
  char fname[len/2];// fname needs to be shorter, will be sprintf'ed into msg
  fname[0]='\0';

  OPENFILENAME of;
  memset(&of, 0, sizeof(of));

#ifdef OPENFILENAME_SIZE_VERSION_400
  of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
  of.lStructSize = sizeof(of);
#endif

  of.hwndOwner = NULL;
  of.lpstrFilter =
    "JPEG Files (*.jpg)\0*.jpg\0"
    "All Files\0*\0";
  of.lpstrCustomFilter = NULL;
  of.nFilterIndex = 1;
  of.lpstrFile = fname;
  of.nMaxFile = sizeof(fname);
  of.lpstrFileTitle = NULL;
  of.lpstrInitialDir = strlen(output_dir)>0 ? output_dir : "";
  of.lpstrTitle = "Select Output JPEG File";
  of.lpstrDefExt = NULL;
  of.Flags = OFN_HIDEREADONLY | OFN_EXPLORER;

  retval = GetSaveFileName(&of);

  if (!retval) {
    if (CommDlgExtendedError()) {
      message_box("File dialog box error");
    }
  }
  else {
    if (strlen(fname) > 0) {
      append_ext_if_needed(fname, ".jpg", ".jpeg");

      GdkPixbuf *pb = make_big_image(curr, FALSE);
      GError *err = NULL;

      gdk_pixbuf_save(pb, fname, "jpeg", &err, "quality", "100", NULL);

      if (err) {
        snprintf(msg, len, "Error exporting JPEG: %s\n", err->message);
        printf("%s", msg);
      }
      else {
        snprintf(msg, len, "Exported JPEG: %s\n", fname);
      }

      g_object_unref(pb);
    }
    else {
      sprintf(msg, "Error exporting JPEG: Zero length filename found.");
    }

    message_box(msg);
  }

#else // #ifdef win32

  if (!export_jpeg_widget)
    create_export_jpeg_file_chooser_dialog();

  gtk_file_chooser_set_filename(export_jpeg_widget, out_name);
  gtk_widget_show(GTK_WIDGET(export_jpeg_widget));

#endif // #ifdef win32

  free(out_name);
  free(output_dir);
  free(output_file);
}

static void set_all_checkboxes(int setting)
{
  if (in_planning_mode) {
    GtkTreeIter iter;
    gboolean valid = gtk_tree_model_get_iter_first(liststore, &iter);
    while (valid)
    {
      gtk_list_store_set(GTK_LIST_STORE(liststore), &iter,
                         COL_SELECTED, setting, -1);
      valid = gtk_tree_model_iter_next(liststore, &iter);
    }
  }
}

SIGNAL_CALLBACK void on_select_all_checkbutton_toggled(GtkWidget *w)
{
  set_all_checkboxes(get_checked("select_all_checkbutton"));
  // It actually looks weird to have the text of the checkbox change...
  //gtk_button_set_label(
  //    GTK_BUTTON(get_widget_checked("select_all_checkbutton")),
  //    get_checked("select_all_checkbutton") ? "De-select All" : "Select All")
  fill_big(curr);
}

static const char *TLE_URL = "http://celestrak.com/NORAD/elements/resource.txt";

static int download_tle(char **err)
{
  asfPrintStatus("Downloading TLE for %s...\n", planner_satellite);

  int len;
  unsigned char *u_tle_txt = download_url(TLE_URL, TRUE, &len);
  if (!u_tle_txt) {
    *err = MALLOC(sizeof(char)*512);
    strcpy(*err, "Failed to download TLE file from:\n");
    strcat(*err, TLE_URL);
    return FALSE;
  }

  char *tle_txt = (char *)u_tle_txt;

  // make sure it contains the satellite we want!
  if (strstr(tle_txt, planner_satellite)==NULL) {
    *err = MALLOC(sizeof(char)*512);
    strcpy(*err, "Successfully downloaded a TLE file, but it did not contain "
           "a two-line element for: ");
    strcat(*err, planner_satellite);
    free(u_tle_txt);
    return FALSE;
  }

  // back up the old TLE
  char old_name[1024];
  sprintf(old_name, "%s/tle", get_asf_share_dir());
  char new_name[1024];
  sprintf(new_name, "%s/tle.old", get_asf_share_dir());
  asfPrintStatus("Creating backup of current TLE:\n  %s\n", new_name);
  fileCopy(old_name, new_name);

  // write to TLE file in share dir
  FILE *fp = fopen_share_file("tle", "w");
  if (fp) {
    FWRITE(u_tle_txt, sizeof(char), len, fp);
    fclose(fp);
  }
  else {
    *err = STRDUP("Could not write TLE file in the ASF share directory.");
    free(u_tle_txt);
    return FALSE;
  }

  free(u_tle_txt);
  return TRUE;
}

SIGNAL_CALLBACK void on_update_tle_button_clicked(GtkWidget *w)
{
  char *err=NULL;
  int ok = download_tle(&err);

  if (!ok) {
    message_box(err);
    return;
  }

  calibrate_planner_reference();
  populate_tle_info();

  asfPrintStatus("\nDone.\n");
}
