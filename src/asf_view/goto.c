#include "asf_view.h"
#include "asf_geocode.h"

static void to_line_samp(double line, double samp)
{
  if (!meta_is_valid_double(line) || !meta_is_valid_double(samp))
  {
    asfPrintWarning("Invalid line/sample values: line %f, sample %f.\n",
                    line, samp);
  }
  else {
    int nl = curr->meta->general->line_count;
    int ns = curr->meta->general->sample_count;

    if(line < 0 || line > nl || samp < 0 || samp > ns)
    {
      asfPrintWarning("Point is outside the image: line %f, sample %f.\n",
                      line, samp);
    }
    else {
      center_line = crosshair_line = line;
      center_samp = crosshair_samp = samp;

      update_pixel_info(curr);
      fill_small(curr);
      fill_big(curr);
    }
  }
}

static void add_line_samp(double line, double samp)
{
  if (!meta_is_valid_double(line) || !meta_is_valid_double(samp))
  {
    asfPrintWarning("Invalid line/sample values: line %f, sample %f.\n",
                    line, samp);
  }
  else {
    int nl = curr->meta->general->line_count;
    int ns = curr->meta->general->sample_count;

    if(line < 0 || line > nl || samp < 0 || samp > ns)
    {
      asfPrintWarning("Point is outside the image: line %f, sample %f.\n",
                      line, samp);
    }
    else {
      center_line = line;
      center_samp = samp;

      if (g_poly->n < MAX_POLY_LEN) {
        g_poly->line[g_poly->n] = line;
        g_poly->samp[g_poly->n] = samp;
        ++g_poly->n;
        g_poly->c = g_poly->n-1;
      } else {
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
}

static void latlon_getls(double *line, double *samp)
{
  double lat = get_double_from_entry("go_to_lat_entry");
  double lon = get_double_from_entry("go_to_lon_entry");
  meta_get_lineSamp(curr->meta, lat, lon, 0, line, samp);
}

static void proj_getls(double *line, double *samp)
{
  double x = get_double_from_entry("go_to_projx_entry");
  double y = get_double_from_entry("go_to_projy_entry");

  double lat, lon;

  if (curr->meta->projection) {
    double h;
    proj_to_latlon(curr->meta->projection, x, y, 0, &lat, &lon, &h);
    lat *= R2D;
    lon *= R2D;
  }
  else {
    int zone = utm_zone(curr->meta->general->center_longitude);
    asfPrintStatus("Unprojected data -- assuming UTM (zone: %d)\n", zone);
    UTM2latLon(x, y, 0, zone, &lat, &lon);
  }

  meta_get_lineSamp(curr->meta, lat, lon, 0, line, samp);
}

SIGNAL_CALLBACK void on_go_to_lat_lon_button_clicked(GtkWidget *w)
{
  double line, samp;
  latlon_getls(&line, &samp);
  to_line_samp(line, samp);
}

SIGNAL_CALLBACK void on_add_to_lat_lon_button_clicked(GtkWidget *w)
{
  double line, samp;
  latlon_getls(&line, &samp);
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
  proj_getls(&line, &samp);
  to_line_samp(line, samp);
}

SIGNAL_CALLBACK void on_add_to_projxy_button_clicked(GtkWidget *w)
{
  double line, samp;
  proj_getls(&line, &samp);
  add_line_samp(line, samp);
}
