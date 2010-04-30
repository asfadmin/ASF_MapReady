#include "asf_convert_gui.h"
#include "asf_geocode.h"
#include <ctype.h>
#include "asf_nan.h"
#include "cla.h"

static GtkWidget * utm_menu = NULL;
static GtkWidget * ps_menu = NULL;
static GtkWidget * albers_menu = NULL;
static GtkWidget * lamcc_menu = NULL;
static GtkWidget * lamaz_menu = NULL;
static GtkWidget * mer_menu = NULL;
static GtkWidget * eqr_menu = NULL;


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

    case PROJ_MER:
      location = "mercator";
      break;

    case PROJ_EQR:
      location = "equi_rectangular";
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

    case PROJ_MER:
      return "mercator_";

    case PROJ_EQR:
      return "equi_rectangular_";
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
    gchar * proj_dir;
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

    proj_dir = projection_directory(projection);

    /* do not populate the predefined projections for UTM -- too many,
    the large dropdownlist causes crashes on windows */
    if (proj_dir && projection != PROJ_UTM)
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

    if (proj_dir)
        g_free(proj_dir);

    return m;
}

static int previous_projection = -1;

void release_predefined_projections()
{
    if (previous_projection != -1)
    {
        g_object_unref(utm_menu);
        g_object_unref(ps_menu);
        g_object_unref(lamcc_menu);
        g_object_unref(lamaz_menu);
        g_object_unref(albers_menu);
	g_object_unref(mer_menu);
	g_object_unref(eqr_menu);
    }
}

void set_predefined_projections(int projection)
{
    GtkWidget * predefined_projection_option_menu;
    GtkWidget * menu = NULL;

    /* looking through all the files can be slow, skip it if we can */
    if (projection == previous_projection)
        return;

    if (previous_projection == -1)
    {
        /* populate all the predefined projection menus */
        utm_menu =
            populate_predefined_projections(PROJ_UTM);

        ps_menu =
            populate_predefined_projections(PROJ_PS);

        lamcc_menu =
            populate_predefined_projections(PROJ_LAMCC);

        lamaz_menu =
            populate_predefined_projections(PROJ_LAMAZ);

        albers_menu =
            populate_predefined_projections(PROJ_ALBERS);

	mer_menu =
	  populate_predefined_projections(PROJ_MER);

	eqr_menu =
	  populate_predefined_projections(PROJ_EQR);

        g_object_ref(utm_menu);
        g_object_ref(ps_menu);
        g_object_ref(lamcc_menu);
        g_object_ref(lamaz_menu);
        g_object_ref(albers_menu);
	g_object_ref(mer_menu);
	g_object_ref(eqr_menu);
    }

    predefined_projection_option_menu =
        get_widget_checked("predefined_projection_option_menu");
    g_assert(predefined_projection_option_menu);

    switch (projection)
    {
    case PROJ_UTM:
        menu = utm_menu;
        break;

    case PROJ_PS:
        menu = ps_menu;
        break;

    case PROJ_LAMCC:
        menu = lamcc_menu;
        break;

    case PROJ_LAMAZ:
        menu = lamaz_menu;
        break;

    case PROJ_ALBERS:
        menu = albers_menu;
        break;

    case PROJ_MER:
      menu = mer_menu;
      break;

    case PROJ_EQR:
      menu = eqr_menu;
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
load_selected_predefined_projection_parameters(int projection, 
					       datum_type_t *datum, spheroid_type_t *spheroid)
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

    predefined_projection_option_menu =
        get_widget_checked("predefined_projection_option_menu");

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
    if (!parse_proj_args_file(path_and_filename, ret, &type, datum, spheroid, &err))
    {
        message_box(err);
        free(err);
        return NULL;
    }

    g_free(path);
    g_free(path_and_filename);

    return ret;
}
