#include "ait.h"
#include <ctype.h>
#include "asf_nan.h"

static GtkWidget * utm_menu = NULL;
static GtkWidget * ps_menu = NULL;
static GtkWidget * albers_menu = NULL;
static GtkWidget * lamcc_menu = NULL;
static GtkWidget * lamaz_menu = NULL;

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
    char * proj_dir;
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

                    name_dup = strdup(name);
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
        free(proj_dir);

    return m;
}

static const char * bracketed_projection_name(projection_type_t proj_type)
{
    switch (proj_type)
    {
    case PROJ_UTM:
        return "[Universal Transverse Mercator]";

    case PROJ_PS:
        return "[Polar Stereographic]";

    case PROJ_ALBERS:
        return "[Albers Conical Equal Area]";

    case PROJ_LAMAZ:
        return "[Lambert Azimuthal Equal Area]";

    case PROJ_LAMCC:
        return "[Lambert Conformal Conic]";

    default:
        return MAGIC_UNSET_STRING;
    }
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

        g_object_ref(utm_menu);
        g_object_ref(ps_menu);
        g_object_ref(lamcc_menu);
        g_object_ref(lamaz_menu);
        g_object_ref(albers_menu);
    }

    predefined_projection_option_menu =
        glade_xml_get_widget(glade_xml, "predefined_projection_option_menu");
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

static void readline(FILE * f, char * buffer, size_t n)
{
    char * p;
    char * newline;

    p = fgets(buffer, n, f);

    if (!p)
    {
        strcpy(buffer, "");
    }
    else
    {
        newline = strrchr(buffer, '\n');
        if (newline)
            *newline = '\0';
    }
}

static int parse_double(const char * str, double * val)
{
    char *p;
    *val = strtod(str, &p);

    if (*str == '\0' || *p != '\0')
    {
        return FALSE;
    }

    return TRUE;
}

static int parse_val(char * inbuf, char * key, double * val)
{
    char * p, * eq, * buf;
    int match = FALSE;

    buf = strdup(inbuf);

    p = eq = strchr(buf, '=');
    if (!eq)
        return FALSE;

    *eq = '\0';
    --p;

    while (isspace((int)(*p)))
        *p-- = '\0';

    if (g_ascii_strcasecmp(buf, key) == 0)
    {
        p = eq + 1;
        while (isspace((int)(*p)))
            ++p;

        if (*p)
        {
            double d;
            if (parse_double(p, &d))
            {
                *val = d;
                match = TRUE;
            }
            else
            {
                *val = MAGIC_UNSET_DOUBLE;
            }
        }
    }

    free(buf);
    return match;
}

static void get_fields(FILE * fp, ...)
{
    va_list ap;
    char * keys[32];
    double * vals[32];
    char buf[256];
    unsigned int nkeys = 0;

    va_start(ap, fp);
    while (nkeys < sizeof(keys))
    {
        keys[nkeys] = va_arg(ap, char *);
        if (!keys[nkeys])
            break;

        vals[nkeys] = va_arg(ap, double *);
        ++nkeys;
    }
    va_end(ap);

    while (!feof(fp))
    {
        unsigned int i;
        int found = FALSE;

        readline(fp, buf, sizeof(buf));

        if (strlen(buf) > 0)
        {
            for (i = 0; i < nkeys; ++i)
            {
                if (parse_val(buf, keys[i], vals[i]))
                {
                    found = TRUE;
                    break;
                }
            }
        }
    }
}

static int read_proj_args_file(char * file, project_parameters_t * pps,
                               projection_type_t * proj_type)
{
    FILE * fp;
    char buf[256];
    int ret;

    fp = fopen(file, "rt");
    if (!fp)
    {
        return FALSE;
    }

    readline(fp, buf, sizeof(buf));
    ret = TRUE;

    if (strcmp(buf, bracketed_projection_name(PROJ_ALBERS)) == 0 ||
        strcmp(buf, "[Albers Equal Area Conic]") == 0)
    {
        *proj_type = PROJ_ALBERS;
        get_fields(fp,
            "First standard parallel", &pps->albers.std_parallel1,
            "Second standard parallel", &pps->albers.std_parallel2,
            "Central Meridian", &pps->albers.center_meridian,
            "Latitude of Origin", &pps->albers.orig_latitude,
            "False Easting", &pps->albers.false_easting,
            "False Northing", &pps->albers.false_northing,
            NULL);
    }
    else if (strcmp(buf, bracketed_projection_name(PROJ_LAMAZ)) == 0)
    {
        *proj_type = PROJ_LAMAZ;
        get_fields(fp,
            "Central Meridian", &pps->lamaz.center_lon,
            "Latitude of Origin", &pps->lamaz.center_lat,
            "False Easting", &pps->lamaz.false_easting,
            "False Northing", &pps->lamaz.false_northing,
            NULL);
    }
    else if (strcmp(buf, bracketed_projection_name(PROJ_LAMCC)) == 0)
    {
        *proj_type = PROJ_LAMCC;
        get_fields(fp,
            "First standard parallel", &pps->lamcc.plat1,
            "Second standard parallel", &pps->lamcc.plat2,
            "Central Meridian", &pps->lamcc.lon0,
            "Latitude of Origin", &pps->lamcc.lat0,
            "False Easting", &pps->lamcc.false_easting,
            "False Northing", &pps->lamcc.false_northing,
            /* "Scale Factor", &pps->lamcc.scale_factor, */
            NULL);
    }
    else if (strcmp(buf, bracketed_projection_name(PROJ_PS)) == 0)
    {
        double is_north_pole;
        *proj_type = PROJ_PS;
        get_fields(fp,
            "First standard parallel", &pps->ps.slat,
            "Standard Parallel", &pps->ps.slat,
            "Central Meridian", &pps->ps.slon,
            "False Easting", &pps->ps.false_easting,
            "False Northing", &pps->ps.false_northing,
            "Northern Projection", &is_north_pole,
            NULL);
        pps->ps.is_north_pole = (int) is_north_pole;
    }
    else if (strcmp(buf, bracketed_projection_name(PROJ_UTM)) == 0)
    {
        double zone;
        *proj_type = PROJ_UTM;
        get_fields(fp,
            "Scale Factor", &pps->utm.scale_factor,
            "Central Meridian", &pps->utm.lon0,
            "Latitude of Origin", &pps->utm.lat0,
            "False Easting", &pps->utm.false_easting,
            "False Northing", &pps->utm.false_northing,
            "Zone", &zone,
            NULL);
        pps->utm.zone = (int) zone;
    }
    else
    {	
        ret = FALSE;
    }

    fclose(fp);
    return ret;
}

static int out_of_sync(const char * filename, int projection)
{
    /* kludge: sometimes attempt to load a predefined projection for the
    projection type that is no longer selected */
    const char * prefix = projection_file_prefix(projection);
    return strncmp(prefix, filename, strlen(prefix)) != 0;
}

project_parameters_t *
load_selected_predefined_projection_parameters(int projection)
{
    GtkWidget * predefined_projection_option_menu;
    GtkWidget * menu;
    GtkWidget * selected_item;
    char filename[256];
    char * path;
    char * path_and_filename;
    project_parameters_t * ret;
    projection_type_t type;

    predefined_projection_option_menu =
        glade_xml_get_widget(glade_xml, "predefined_projection_option_menu");

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

    path_and_filename = (char *)
        MALLOC(sizeof(char *) * (strlen(path) + strlen(filename) + 5));

    sprintf(path_and_filename, "%s/%s", path, filename);

    ret = (project_parameters_t *) MALLOC(sizeof(project_parameters_t));
    if (!read_proj_args_file(path_and_filename, ret, &type))
    {
        char * tmp = (char *)
            MALLOC(sizeof(char *) * (strlen(path_and_filename) + 100));

        sprintf(tmp, "Error opening .proj file: %s\n", path_and_filename); 
        message_box(tmp);
        free(tmp);

        return NULL;
    }

    free(path);
    free(path_and_filename);

    return ret;
}
