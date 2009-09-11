#include "proj2proj.h"
#include "asf_meta.h"
#include "libasf_proj.h"
#include "asf_geocode.h"

#include <ctype.h>
#include "common.c"

static int get_int_from_entry2(const char *str, const char *widget_name)
{
    char tmp[256];
    snprintf(tmp, 256, "%s_%s", str, widget_name);
    return get_int_from_entry(tmp);
}

static double get_double_from_entry2(const char *str, const char *widget_name)
{
    char tmp[256];
    snprintf(tmp, 256, "%s_%s", str, widget_name);
    return get_double_from_entry(tmp);
}

static GtkWidget *get_widget2(const char *str, const char *widget_name)
{
    char tmp[256];
    snprintf(tmp, 256, "%s_%s", str, widget_name);
    return get_widget_checked(tmp);
}

static void get_projection_info(project_parameters_t *pp, 
                                projection_type_t *projection_type,
                                datum_type_t *datum,
                                const char *str)
{
    GtkWidget *projection_option_menu =
        get_widget2(str, "projection_option_menu");

    int projection = gtk_option_menu_get_history(
        GTK_OPTION_MENU(projection_option_menu));

    switch (projection) {
      case PROJ_UTM:
          *projection_type = UNIVERSAL_TRANSVERSE_MERCATOR;
          pp->utm.zone = get_int_from_entry2(str, "utm_zone_entry");
          break;
      case PROJ_PS:
          *projection_type = POLAR_STEREOGRAPHIC;
          pp->ps.slat =
              get_double_from_entry2(str, "first_standard_parallel_entry");
          pp->ps.slon =
              get_double_from_entry2(str, "central_meridian_entry");
          pp->ps.is_north_pole = pp->ps.slat > 0;
          break;
      case PROJ_ALBERS:
          *projection_type = ALBERS_EQUAL_AREA;
          pp->albers.std_parallel1 =
              get_double_from_entry2(str, "first_standard_parallel_entry");
          pp->albers.std_parallel2 =
              get_double_from_entry2(str, "second_standard_parallel_entry");
          pp->albers.center_meridian =
              get_double_from_entry2(str, "central_meridian_entry");
          pp->albers.orig_latitude =
              get_double_from_entry2(str, "latitude_of_origin_entry");
          break;
      case PROJ_LAMAZ:
          *projection_type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
          pp->lamaz.center_lon =
              get_double_from_entry2(str, "central_meridian_entry");
          pp->lamaz.center_lat =
              get_double_from_entry2(str, "latitude_of_origin_entry");
          break;
      case PROJ_LAMCC:
          *projection_type = LAMBERT_CONFORMAL_CONIC;
          pp->lamcc.plat1 =
              get_double_from_entry2(str, "first_standard_parallel_entry");
          pp->lamcc.plat2 =
              get_double_from_entry2(str, "second_standard_parallel_entry");
          pp->lamcc.lon0 =
              get_double_from_entry2(str, "central_meridian_entry");
          pp->lamcc.lat0 =
              get_double_from_entry2(str, "latitude_of_origin_entry");
          break;
      case PROJ_LATLON:
          *projection_type = LAT_LONG_PSEUDO_PROJECTION;
          break;
      default:
          asfPrintError("Impossible.\n");
          break;
    }

    GtkWidget *datum_option_menu = get_widget2(str, "datum_option_menu");
    int datum_sel = gtk_option_menu_get_history(
        GTK_OPTION_MENU(datum_option_menu));

    switch (datum_sel) {
        default:
        case 0: *datum = WGS84_DATUM;  break;
        case 1: *datum = NAD27_DATUM;  break;
        case 2: *datum = NAD83_DATUM;  break;
        case 3: *datum = HUGHES_DATUM; break;
    }
}

static char *find_next_endl(char *q)
{
    if (!q) return NULL;
    char *p = strchr(q, '\n');
    if (!p) {
        // no endline, point to the end of the string
        p = q + strlen(q);
    }
    return p;
}

static void clear_textview(GtkWidget *tv)
{
    GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));

    if (gtk_text_buffer_get_char_count(tb) > 0)
    {
        GtkTextIter b, e;
        gtk_text_buffer_get_start_iter(tb, &b);
        gtk_text_buffer_get_end_iter(tb, &e);
        gtk_text_buffer_delete(tb, &b, &e);
    }
}

static void append_text(GtkWidget *tv, char *s)
{
    GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));

    GtkTextIter end;
    gtk_text_buffer_get_end_iter(tb, &end);

    gtk_text_buffer_insert(tb, &end, s, -1);
}

static void execute(const char *from, const char *to)
{
    char buf[256];

    // from/to: either "source" or "target"
    // they just indicate which textview/projparms widgets to grab from
    project_parameters_t source_pp, target_pp;
    projection_type_t source_proj, target_proj;
    datum_type_t source_datum, target_datum;

    get_projection_info(&source_pp, &source_proj, &source_datum, from);
    get_projection_info(&target_pp, &target_proj, &target_datum, to);

    //char *s = proj_info_as_string(source_proj, &source_pp);
    //printf("Source==>\n%s\n\n", s);
    //s = proj_info_as_string(target_proj, &target_pp);
    //printf("Target==>\n%s\n\n", s);

    project_t *source_proj_fn, *target_proj_fn;
    unproject_t *source_unproj_fn, *target_unproj_fn;

    determine_projection_fns(source_proj, &source_proj_fn, &source_unproj_fn);
    determine_projection_fns(target_proj, &target_proj_fn, &target_unproj_fn);

    GtkWidget *source_tv = get_widget2(from, "textview");
    GtkWidget *target_tv = get_widget2(to, "textview");
    clear_textview(target_tv);

    GtkTextIter start, end;
    GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(source_tv));
    gtk_text_buffer_get_start_iter(tb, &start);
    gtk_text_buffer_get_end_iter(tb, &end);
    char *txt = STRDUP(gtk_text_buffer_get_text(tb, &start, &end, FALSE));

    // strip off trailing whitespace
    while (isspace(txt[strlen(txt)-1]))
        txt[strlen(txt)-1]='\0';

    // go through the text line by line
    int line_num = 1;
    int valid_line_num = 1;
    char *iter = txt;
    char *endl = find_next_endl(iter);
    while (endl) {
        int len = endl - iter;
        if (len >= 255) {
            snprintf(buf, 255, "Lengthy line %d ignored.\n", line_num);
            append_text(target_tv, buf);
        }
        else if (len == 0) {
            append_text(target_tv, "\n");
        }
        else {
            char line[256];
            strncpy_safe(line, iter, len+1);

            // grab 2 or 3 floating point numbers from the line
            // if a third is provided, it is a height value
            double x, y, z=0;
            char *p2,*p3,*p4;
            x = strtod(line, &p2);
            if (errno==EINVAL) {
                snprintf(buf, 255, "Invalid line %d ignored.\n", line_num);
                append_text(target_tv, buf);
            } 
            else {
                if (*p2==',' || *p2==';') ++p2;
                y = strtod(p2, &p3);
                if (errno==EINVAL || p2==p3) {
                    snprintf(buf, 255, "Invalid line %d ignored.\n", line_num);
                    append_text(target_tv, buf);
                }
                else {
                    if (*p3==',' || *p3==';') ++p3;
                    if (*p3!='\0')
                        z = strtod(p3, &p4);

                    //printf("Parsed values: x=%f, y=%f, z=%f\n", x, y, z);

                    // now... project!
                    // first to lat/lon, then to target projection
                    double lat, lon, ht;
                    source_unproj_fn(&source_pp, x, y, z, &lat, &lon, &ht,
                                     source_datum);

                    //printf("Lat/Lon: %f %f\n", lat, lon);

                    if (valid_line_num==1 &&
                        target_proj==UNIVERSAL_TRANSVERSE_MERCATOR && 
                        target_pp.utm.zone==0)
                    {
                        fill_in_utm(lat*R2D, lon*R2D, &target_pp);
                        char name[64];
                        sprintf(name, "%s_utm_zone_entry", to);
                        put_int_to_entry(name, target_pp.utm.zone);
                    }

                    target_proj_fn(&target_pp, lat, lon, ht, &x, &y, &z,
                                   target_datum);
                    
                    //printf("Projected: x=%f, y=%f, z=%f\n", x, y, z);
                    // use different accuracies for lat/lon vs. projected
                    if (target_proj==LAT_LONG_PSEUDO_PROJECTION)
                      snprintf(buf, 256, "%.4f %.4f %.2f\n", x, y, z);
                    else
                      snprintf(buf, 256, "%.2f %.2f %.2f\n", x, y, z);
                    append_text(target_tv, buf);

                    ++valid_line_num;
                }
            }
        }

        if (*endl=='\0') break;

        iter = endl+1;
        endl = find_next_endl(iter);

        ++line_num;
    }

    FREE(txt);
}

void forward()
{
    execute("source", "target");
}

void backward()
{
    execute("target", "source");
}

SIGNAL_CALLBACK void
on_forward_button_clicked(GtkWidget * widget)
{
    forward();
}

SIGNAL_CALLBACK void
on_backward_button_clicked(GtkWidget * widget)
{
    backward();
}
