#include "asf_view.h"
#include "plan.h"
#include <ctype.h>

// Need to remember the modes, so we can refer to them later
char **modes=NULL;
int num_beam_modes=-1;

void setup_planner()
{
    show_widget("planner_notebook", TRUE);
    show_widget("viewer_notebook", FALSE);

    // populate the "Satellite/Beam" dropdown from the
    // "beam_modes.txt" file
    modes = get_all_beam_modes(&num_beam_modes);

    int i;
    clear_combobox("satellite_combobox");
    for (i=0; i<num_beam_modes; ++i)
        add_to_combobox("satellite_combobox", modes[i]);

    set_combo_box_item_checked("satellite_combobox", 0);
    set_combo_box_item_checked("orbit_direction_combobox", 0);

    //for (i=0; i<num_beam_modes; ++i)
    //    FREE(modes[i]);
    //FREE(modes);
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

SIGNAL_CALLBACK void on_plan_button_clicked(GtkWidget *w)
{
    int i,pass_type;
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
    }
    else {
      double x[MAX_POLY_LEN], y[MAX_POLY_LEN];
      
      if (g_poly->n == 0) {
        strcat(errstr, "No area of interest selected.\n");
      }
      else if (g_poly->n == 1) {
        // special handling if we have only two points (create a box)
        double lat1, lat2, lon1, lon2;
        meta_get_latLon(meta, crosshair_line, crosshair_samp, 0, &lat1, &lon1);
        meta_get_latLon(meta, g_poly->line[0], g_poly->samp[0], 0,
                        &lat2, &lon2);
        
        int zone = utm_zone(lon1);
        latLon2UTM_zone(lat1, lon1, 0, zone, &x[0], &y[0]);
        latLon2UTM_zone(lat2, lon2, 0, zone, &x[2], &y[2]);

        x[1] = x[0];
        y[1] = y[2];

        x[3] = x[2];
        y[3] = y[0];

        clat = .5*(lat1+lat2);
        clon = .5*(lon1+lon2);

        min_lat = lat1 < lat2 ? lat1 : lat2;
        max_lat = lat1 > lat2 ? lat1 : lat2;

        aoi = polygon_new_closed(4, x, y);
      }
      else {
        double lat, lon;
        meta_get_latLon(meta, crosshair_line, crosshair_samp, 0, &lat, &lon);
        max_lat = min_lat = clat = lat;

        clon = lon;
        int zone = utm_zone(clon);
        latLon2UTM_zone(lat, lon, 0, zone, &x[0], &y[0]);

        for (i=0; i<g_poly->n; ++i) {
          meta_get_latLon(meta, g_poly->line[i], g_poly->samp[i], 0,
                          &lat, &lon);
          latLon2UTM_zone(lat, lon, 0, zone, &x[i+1], &y[i+1]);

          clat += lat;
          clon += lon;

          if (lat > max_lat) max_lat = lat;
          if (lat < min_lat) min_lat = lat;
        }

        clat /= (double)(g_poly->n+1);
        clon /= (double)(g_poly->n+1);

        printf("Center lat/lon: %f, %f\n", clat, clon);
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
                   clat, clon, pass_type, aoi, tle_filename, &pc, &err);

      if (n < 0) {
        put_string_to_label("plan_error_label", err);
        free(err);
      }
      else {
        // switch to planning results tab
        GtkWidget *nb = get_widget_checked("planner_notebook");
        gtk_notebook_set_current_page(GTK_NOTEBOOK(nb), 1);

        asfPrintStatus("Found %d matches.\n", n);
        for (i=0; i<pc->num; ++i) {
          asfPrintStatus("#%d: %s (%.1f%%)\n", i+1, 
                         pc->passes[i]->start_time_as_string,
                         100. * pc->passes[i]->total_pct);
        }

        // this is for debugging, can be removed
        pass_collection_to_kml(pc, "test_kml.kml");

        // polygon #0 is left alone (it is the area of interest)
        // The passes start at polygon #1 (clobber any existing polygons)

        // now create polygons from each pass
        // metadata for each polygon is also stored... somewhere!
        for (i=0; i<pc->num; ++i) {
          PassInfo *pi = pc->passes[i];

          int k,m=0;
          for (k=0; k<pi->num; ++k) {
            OverlapInfo *oi = pi->overlaps[k];
            Polygon *poly = oi->viewable_region;

            int j;
            for (j=0; j<poly->n; ++j) {
              double samp, line, lat, lon;

              UTM2latLon(poly->x[j], poly->y[j], 0, oi->utm_zone, &lat, &lon);
              meta_get_lineSamp(meta, lat, lon, 0, &line, &samp);

              //printf("%d,%d -- %f,%f\n",i,m,line,samp);
              g_polys[i+1].line[m]=line;
              g_polys[i+1].samp[m]=samp;
              ++m;
            }
          }

          g_polys[i+1].n=m;
          g_polys[i+1].c=m-1;

          g_polys[i+1].show_extent=FALSE;

          if (i>=MAX_POLYS-1) {
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
