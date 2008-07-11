#include "asf_view.h"
#include "asf_geocode.h"
#include <assert.h>

static int line_samp_ok(double line, double samp)
{
  if (!meta_is_valid_double(line) || !meta_is_valid_double(samp))
  {
    asfPrintWarning("Invalid line/sample values: line %f, sample %f.\n",
                    line, samp);
    return FALSE;
  }
  else {
    int nl = curr->meta->general->line_count;
    int ns = curr->meta->general->sample_count;

    if (line < 0 || line > nl || samp < 0 || samp > ns)
    {
      // we could actually plot the point if it isn't "too crazy"
      // we define "too crazy" as more than an image width/height out
      if (line < -nl || line > 2*nl || samp <-ns || samp > 2*ns) { 
        asfPrintWarning("Point is outside the image: line %f, sample %f.\n"
                        "Ignoring this point -- too far outside the image.\n",
                        line, samp);
        return FALSE;
      }
      else {
        asfPrintWarning("Point is outside the image: line %f, sample %f.\n",
                        line, samp);
        return TRUE;
        }
    }
    else {
      // normal case: inside the image
      return TRUE;
    }
  }

  // not reached
  assert(0);
  return FALSE;
}

static void to_line_samp(double line, double samp)
{
  if (line_samp_ok(line, samp)) {
    center_line = crosshair_line = line;
    center_samp = crosshair_samp = samp;
    
    update_pixel_info(curr);
    fill_small(curr);
    fill_big(curr);
  }
}

static void add_line_samp(double line, double samp)
{
  if (line_samp_ok(line, samp)) {
    center_line = line;
    center_samp = samp;

    if (g_poly->n < MAX_POLY_LEN) {
      g_poly->line[g_poly->n] = line;
      g_poly->samp[g_poly->n] = samp;
      ++g_poly->n;
      g_poly->c = g_poly->n-1;
    }
    else {
      asfPrintWarning("Exceeded maximum polygon length.\n"
                      "No more points can be added.\n");
    }
    
    // now arrow keys will move the just-added point
    last_was_crosshair = FALSE;

    update_pixel_info(curr);
    fill_small(curr);
    fill_big(curr);
  }
}

static int latlon_getls(double *line, double *samp)
{
  int ok = FALSE;

  meta_parameters *meta = curr->meta;
  if (meta->projection || (meta->sar&&meta->state_vectors) ||
      meta->transform || meta->airsar)
  {
    double lat = get_double_from_entry("go_to_lat_entry");
    double lon = get_double_from_entry("go_to_lon_entry");

    if (lat < -90 || lat > 90) { 
      asfPrintWarning("Illegal latitude value: %f\n", lat);
    }
    else if (lon < -360 || lon > 360) {
      asfPrintWarning("Illegal longitude value: %f\n", lon);
    }
    else {
      int bad = meta_get_lineSamp(curr->meta, lat, lon, 0, line, samp);
      ok = !bad;
    }
  }
  else {
    asfPrintWarning("No geolocation information available -- GoTo will only\n"
                    "work for GoTo by Line/Sample.\n");
  }

  return ok;
}

static int proj_getls(double *line, double *samp)
{
  meta_parameters *meta = curr->meta;
  if (meta->projection || (meta->sar&&meta->state_vectors) ||
      meta->transform || meta->airsar)
  {
    double x = get_double_from_entry("go_to_projx_entry");
    double y = get_double_from_entry("go_to_projy_entry");

    double lat, lon;

    if (curr->meta->projection) {
      double h;
      proj_to_latlon(curr->meta->projection, x, y, 0, &lat, &lon, &h);
      if (lat==y && lon==x) {
        // this is usually indicative of failure... an error will have
        // been printed out already by libproj
        return FALSE;
      }
      lat *= R2D;
      lon *= R2D;
    }
    else {
      int zone = utm_zone(curr->meta->general->center_longitude);
      asfPrintStatus("Unprojected data -- assuming UTM (zone: %d)\n", zone);
      UTM2latLon(x, y, 0, zone, &lat, &lon);
      if (lat==y*R2D && lon==x*R2D) {
        // this is usually indicative of failure... an error will have
        // been printed out already by libproj
        return FALSE;
      }
    }

    int bad = meta_get_lineSamp(curr->meta, lat, lon, 0, line, samp);
    return !bad;
  }
  else {
    asfPrintWarning("No geolocation information available -- GoTo will only\n"
                    "work for GoTo by Line/Sample.\n");
    return FALSE;

  }
}

//-- EVENT HANDLERS ---------------------------------------------------
SIGNAL_CALLBACK void on_go_to_lat_lon_button_clicked(GtkWidget *w)
{
  double line, samp;
  if (latlon_getls(&line, &samp))
    to_line_samp(line, samp);
}

SIGNAL_CALLBACK void on_add_to_lat_lon_button_clicked(GtkWidget *w)
{
  double line, samp;
  if (latlon_getls(&line, &samp))
    add_line_samp(line, samp);
}

SIGNAL_CALLBACK void on_go_to_line_samp_button_clicked(GtkWidget *w)
{
  to_line_samp(get_double_from_entry("go_to_line_entry"),
               get_double_from_entry("go_to_samp_entry"));
}

SIGNAL_CALLBACK void on_add_to_line_samp_button_clicked(GtkWidget *w)
{
  add_line_samp(get_double_from_entry("go_to_line_entry"), 
                get_double_from_entry("go_to_samp_entry"));
}

SIGNAL_CALLBACK void on_go_to_projxy_button_clicked(GtkWidget *w)
{
  double line, samp;
  if (proj_getls(&line, &samp))
    to_line_samp(line, samp);
}

SIGNAL_CALLBACK void on_add_to_projxy_button_clicked(GtkWidget *w)
{
  double line, samp;
  if (proj_getls(&line, &samp))
    add_line_samp(line, samp);
}
