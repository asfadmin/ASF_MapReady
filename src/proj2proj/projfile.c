#include "proj2proj.h"
#include "asf_geocode.h"
#include <ctype.h>
#include "asf_nan.h"
#include "cla.h"
#include "asf.h"
#include "asf_meta.h"
#include "libasf_proj.h"

static GtkWidget * source_utm_menu = NULL;
static GtkWidget * source_ps_menu = NULL;
static GtkWidget * source_albers_menu = NULL;
static GtkWidget * source_lamcc_menu = NULL;
static GtkWidget * source_lamaz_menu = NULL;
static GtkWidget * source_latlon_menu = NULL;

static GtkWidget * target_utm_menu = NULL;
static GtkWidget * target_ps_menu = NULL;
static GtkWidget * target_albers_menu = NULL;
static GtkWidget * target_lamcc_menu = NULL;
static GtkWidget * target_lamaz_menu = NULL;
static GtkWidget * target_latlon_menu = NULL;


static char * projection_directory(int projection)
{
    char * location = NULL, * ret;

    switch (projection)
    {
    case PROJ_UTM:
        location = "utm";
        break;

    case PROJ_PS:
        location = "polar_stereographic";
        break;

    case PROJ_LAMCC:
        location = "lambert_conformal_conic";
        break;

    case PROJ_LAMAZ:
        location = "lambert_azimuthal_equal_area";
        break;

    case PROJ_ALBERS:
        location = "albers_equal_area_conic";
        break;
    }

    ret = (char *) malloc(sizeof(char) *
        (strlen(location) + strlen(get_asf_share_dir()) + 25));

    sprintf(ret, "%s%cprojections%c%s", get_asf_share_dir(),  DIR_SEPARATOR,
        DIR_SEPARATOR, location);

    return ret;
}

static int my_strcmp(const void *s1, const void *s2)
{
    char * string1 = * (char**)s1;
    char * string2 = * (char**)s2;

    if (strncmp(string1, "utm", 3) == 0 && strncmp(string2, "utm", 3) == 0)
    {
        int utm_zone1, utm_zone2;
        sscanf(string1, "utm_%d", &utm_zone1);
        sscanf(string2, "utm_%d", &utm_zone2);

        if (utm_zone1 == utm_zone2)
        {
            return *(string1 + strlen(string1) - 1) == 'N';
        }
        else
        {
            return utm_zone1 > utm_zone2;
        }
    }
    else
    {
        return g_ascii_strcasecmp(string1, string2);
    }
}

static const char * projection_file_prefix(int projection)
{
    switch (projection)
    {
    default:
    case PROJ_UTM:
        return "utm_";

    case PROJ_PS:
        return "polar_stereographic_";

    case PROJ_LAMCC:
        return "lambert_conformal_conic_";

    case PROJ_LAMAZ:
        return "lambert_azimuthal_equal_area_";

    case PROJ_ALBERS:
        return "albers_equal_area_conic_";
    }
}

static char * fudge_the_name(int projection, const char * name)
{
    static char buf[256];

    if (projection == PROJ_UTM)
    {
        strcpy(buf, name);
    }
    else
    {
        const char * prefix;
        prefix = projection_file_prefix(projection);

        if (strncmp(name, prefix, strlen(prefix)) == 0)
        {
            const char * p = name + strlen(prefix);
            strcpy(buf, p);
        }
        else
        {
            strcpy(buf, name);
        }
    }

    return buf;
}

static GtkWidget * populate_predefined_projections(int projection)
{
    gchar * proj_dir=NULL;
    GDir * dir;
    GtkWidget * m;
    GtkWidget * item;

    m = gtk_menu_new();

    item = gtk_menu_item_new_with_label("User Defined");
    gtk_menu_append(GTK_MENU(m), item);
    gtk_widget_show(item);

    item = gtk_separator_menu_item_new();
    gtk_menu_append(GTK_MENU(m), item);
    gtk_widget_show(item);

    // do not populate the predefined projections for UTM -- too many,
    // the large dropdownlist causes crashes on windows. Also, no
    // predefined projections for the lat/lon psuedoprojection
    if (projection != PROJ_UTM && projection != PROJ_LATLON) {
      proj_dir = projection_directory(projection);
      if (proj_dir)
      {
        dir = g_dir_open(proj_dir, 0, NULL);
        
        if (dir)
        {
          int i, n;
          char *names[512];
          
          n = 0;
          while (TRUE)
          {
            const char * name;
            
            name = (char*) g_dir_read_name(dir);
            
            if (name)
            {
              char * name_dup;
              char * p;
              
              name_dup = STRDUP(name);
              p = strrchr(name_dup, '.');
              
              if (p && strcmp(p, ".proj") == 0)
              {
                *p = '\0';
                
                names[n] = name_dup;
                ++n;
                
                if (n >= sizeof(names)/sizeof(names[0]))
                  break;
              }
            }
            else
            {
              break;
            }
          }
          
          g_dir_close(dir);
          
          qsort(names, n, sizeof(char *), my_strcmp);
          for (i = 0; i < n; ++i)
          {
            item = gtk_menu_item_new_with_label(
              fudge_the_name(projection, names[i]));
            
            g_object_set_data(G_OBJECT(item),
                              "file", (gpointer)names[i]);
            
            gtk_menu_append(GTK_MENU(m), item);
            gtk_widget_show(item);
          }
        }
      }
    }

    if (proj_dir)
      g_free(proj_dir);

    return m;
}

static int previous_projection = -1;

void release_predefined_projections()
{
    if (previous_projection != -1)
    {
        g_object_unref(source_utm_menu);
        g_object_unref(source_latlon_menu);
        g_object_unref(source_ps_menu);
        g_object_unref(source_lamcc_menu);
        g_object_unref(source_lamaz_menu);
        g_object_unref(source_albers_menu);

        g_object_unref(target_utm_menu);
        g_object_unref(target_latlon_menu);
        g_object_unref(target_ps_menu);
        g_object_unref(target_lamcc_menu);
        g_object_unref(target_lamaz_menu);
        g_object_unref(target_albers_menu);
    }
}

void set_predefined_projections(int is_source, int projection)
{
    GtkWidget * predefined_projection_option_menu;
    GtkWidget * menu = NULL;

    /* looking through all the files can be slow, skip it if we can */
    if (projection == previous_projection)
      return;

    if (previous_projection == -1)
    {
        /* populate all the predefined projection menus */
        source_utm_menu =
            populate_predefined_projections(PROJ_UTM);
        target_utm_menu =
            populate_predefined_projections(PROJ_UTM);

        source_latlon_menu =
            populate_predefined_projections(PROJ_LATLON);
        target_latlon_menu =
            populate_predefined_projections(PROJ_LATLON);

        source_ps_menu =
            populate_predefined_projections(PROJ_PS);
        target_ps_menu =
            populate_predefined_projections(PROJ_PS);

        source_lamcc_menu =
            populate_predefined_projections(PROJ_LAMCC);
        target_lamcc_menu =
            populate_predefined_projections(PROJ_LAMCC);

        source_lamaz_menu =
            populate_predefined_projections(PROJ_LAMAZ);
        target_lamaz_menu =
            populate_predefined_projections(PROJ_LAMAZ);

        source_albers_menu =
            populate_predefined_projections(PROJ_ALBERS);
        target_albers_menu =
            populate_predefined_projections(PROJ_ALBERS);

        g_object_ref(source_latlon_menu);
        g_object_ref(source_utm_menu);
        g_object_ref(source_ps_menu);
        g_object_ref(source_lamcc_menu);
        g_object_ref(source_lamaz_menu);
        g_object_ref(source_albers_menu);

        g_object_ref(target_latlon_menu);
        g_object_ref(target_utm_menu);
        g_object_ref(target_ps_menu);
        g_object_ref(target_lamcc_menu);
        g_object_ref(target_lamaz_menu);
        g_object_ref(target_albers_menu);
    }

    predefined_projection_option_menu = is_source ?
      get_widget_checked("source_predefined_projection_option_menu") :
      get_widget_checked("target_predefined_projection_option_menu");

    switch (projection)
    {
    case PROJ_UTM:
        menu = is_source ? source_utm_menu : target_utm_menu;
        break;

    case PROJ_LATLON:
        menu = is_source ? source_latlon_menu : target_latlon_menu;
        break;

    case PROJ_PS:
        menu = is_source ? source_ps_menu : target_ps_menu;
        break;

    case PROJ_LAMCC:
        menu = is_source ? source_lamcc_menu : target_lamcc_menu;
        break;

    case PROJ_LAMAZ:
        menu = is_source ? source_lamaz_menu : target_lamaz_menu;
        break;

    case PROJ_ALBERS:
        menu = is_source ? source_albers_menu : target_albers_menu;
        break;
    }

    g_assert(menu);

    previous_projection = projection;
    gtk_option_menu_set_menu(
        GTK_OPTION_MENU(predefined_projection_option_menu), menu);

    gtk_option_menu_set_history(
        GTK_OPTION_MENU(predefined_projection_option_menu), 0);

    gtk_widget_show(menu);
    gtk_widget_show(predefined_projection_option_menu);
}

static int out_of_sync(const char * filename, int projection)
{
    /* kludge: sometimes attempt to load a predefined projection for the
    projection type that is no longer selected */
    const char * prefix = projection_file_prefix(projection);
    return strncmp(prefix, filename, strlen(prefix)) != 0;
}

project_parameters_t *
load_selected_predefined_projection_parameters(int is_source, int projection,
                                               datum_type_t *datum,
					       spheroid_type_t *spheroid)
{
    GtkWidget * predefined_projection_option_menu;
    GtkWidget * menu;
    GtkWidget * selected_item;
    gchar filename[256];
    gchar * path;
    gchar * path_and_filename;
    project_parameters_t * ret;
    projection_type_t type;
    char *err=NULL;

    predefined_projection_option_menu = is_source ?
      get_widget_checked("source_predefined_projection_option_menu") :
      get_widget_checked("target_predefined_projection_option_menu");

    menu =
        gtk_option_menu_get_menu(
        GTK_OPTION_MENU(predefined_projection_option_menu));

    selected_item =
        gtk_menu_get_active(GTK_MENU(menu));

    sprintf(filename, "%s.proj", (char *)g_object_get_data(
        G_OBJECT(selected_item), "file"));

    if (out_of_sync(filename, projection))
        return NULL;

    path = projection_directory(projection);

    path_and_filename = (gchar *)
        g_malloc(sizeof(gchar *) * (strlen(path) + strlen(filename) + 5));

    sprintf(path_and_filename, "%s/%s", path, filename);

    ret = (project_parameters_t *) g_malloc(sizeof(project_parameters_t));
    if (!parse_proj_args_file(path_and_filename, ret, &type, datum, spheroid,
			      &err))
    {
        message_box(err);
        free(err);
        return NULL;
    }

    g_free(path);
    g_free(path_and_filename);

    return ret;
}
