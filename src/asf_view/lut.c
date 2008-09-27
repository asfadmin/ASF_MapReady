#include "asf_view.h"
#include "asf_raster.h"
#include <geotiff_support.h>

static int g_have_lut = FALSE;
static unsigned char *g_lut_buffer = NULL;

// kludge, here --
// all items we will want to pre-select should have their indexes
// memorized here.  This will work around a deficiency in the GTK
// menu widget, we can't iterate through the items
// ...yes, there has to be a better way (iterate through drop-down
// menu items)
static int g_cloude16_index = 0;
static int g_cloude8_index = 0;
static int g_dem_index = 0;
static int g_interferogram_index = 0;
static int g_unwrapping_mask_index = 0;
static int g_layover_mask_index = 0;
static int g_polarimetry_index = 0;
static int g_water_mask_index = 0;
static int g_tiff_lut_index = 0;
static int g_asf_lut_index = 0;
static int g_current_index = 0;

SIGNAL_CALLBACK void on_viewer_notebook_switch_page(GtkWidget *w)
{
    GtkWidget *option_menu = get_widget_checked("lut_optionmenu");
    gtk_option_menu_set_history(GTK_OPTION_MENU(option_menu), get_current_index());
}

static int my_strcmp(const void *v1, const void *v2)
{
  char **s1 = (char **)v1;
  char **s2 = (char **)v2;
  return strcmp_case(*s1, *s2);
}

static char *get_lut_loc()
{
    char *lut_loc = MALLOC(sizeof(char)*(strlen(get_asf_share_dir())+64));
    sprintf(lut_loc, "%s/look_up_tables", get_asf_share_dir());
    return lut_loc;
}

void populate_lut_combo()
{
    GtkWidget *menu = NULL;
    GtkWidget *option_menu = get_widget_checked("lut_optionmenu");
    if (option_menu) {
        menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(option_menu));
        if (menu) {
            gtk_option_menu_remove_menu(GTK_OPTION_MENU(option_menu));
        }
    }
    menu = gtk_menu_new();

    GtkWidget *item = gtk_menu_item_new_with_label("None");
    gtk_menu_append(GTK_MENU(menu), item);
    gtk_widget_show(item);

    item = gtk_separator_menu_item_new();
    gtk_menu_append(GTK_MENU(menu), item);
    gtk_widget_show(item);

    char *lut_loc = get_lut_loc();

    // Open up the share dir's look up tables list, populate dropdown
    // from the files in that directory.
    GDir *lut_dir = g_dir_open(lut_loc, 0, NULL);
    if (lut_dir) {
        int i, n=0;
        char **names = MALLOC(sizeof(char*)*20); // keep the list 20 items max

        while (1) {
            const char *name = (char*)g_dir_read_name(lut_dir);
            if (name) {
                char *name_dup = STRDUP(name);
                char *p = findExt(name_dup);
                if (p && strcmp(p, ".lut") == 0) {
                    *p = '\0'; // don't show ".lut" extension in menu
                    names[n++] = name_dup;
                    // quit when we get too many
                    if (n > 20)
                        break;
                }
            } else
                break;
        }
        g_dir_close(lut_dir);

        // alphabetize
        qsort(names, n, sizeof(char*), my_strcmp);

        // now populate the menu
        for (i=0; i<n; ++i) {
            item = gtk_menu_item_new_with_label(names[i]);
            g_object_set_data(G_OBJECT(item), "file", (gpointer)names[i]);
            gtk_menu_append(GTK_MENU(menu), item);
            gtk_widget_show(item);

            // memorize the indexes we will need later
            if (strncmp_case(names[i], "cloude16", 8) == 0) {
                g_cloude16_index = i+2;
            }
            if (strncmp_case(names[i], "cloude8", 7) == 0) {
                g_cloude8_index = i+2;
            }
            if (strncmp_case(names[i], "dem", 3) == 0) {
                g_dem_index = i+2;
            }
            if (strncmp_case(names[i], "interferogram", 13) == 0) {
                g_interferogram_index = i+2;
            }
            if (strncmp_case(names[i], "unwrapping_mask", 15) == 0) {
                g_unwrapping_mask_index = i+2;
            }
            if (strncmp_case(names[i], "layover_mask", 12) == 0) {
                g_layover_mask_index = i+2;
            }
            if (strncmp_case(names[i], "polarimetry", 11) == 0) {
                g_polarimetry_index = i+2;
            }
            if (strncmp_case(names[i], "water_mask", 11) == 0) {
                g_water_mask_index = i+2;
            }
            if (strncmp(names[i], EMBEDDED_TIFF_COLORMAP_LUT,
                        strlen(EMBEDDED_TIFF_COLORMAP_LUT)) == 0) {
                g_tiff_lut_index = i+2;
            }
            if (strncmp(names[i], EMBEDDED_ASF_COLORMAP_LUT,
                        strlen(EMBEDDED_ASF_COLORMAP_LUT)) == 0) {
                g_asf_lut_index = i+2;
            }
        }
    }

    option_menu = get_widget_checked("lut_optionmenu");

    gtk_option_menu_set_menu(GTK_OPTION_MENU(option_menu), menu);
    gtk_option_menu_set_history(GTK_OPTION_MENU(option_menu), 0);
    set_current_index(0);

    gtk_widget_show(menu);
    gtk_widget_show(option_menu);

    free(lut_loc);
}

void set_lut(const char *lut_basename)
{
    if (g_lut_buffer) {
        free(g_lut_buffer);
        g_lut_buffer = NULL;
    }

    char filename[512];
    sprintf(filename, "%s.lut", lut_basename);

    char *lut_loc = get_lut_loc();
    char *path_and_file =
            MALLOC(sizeof(char)*(strlen(lut_loc)+strlen(filename)+20));
    sprintf(path_and_file, "%s/%s", lut_loc, filename);
    free(lut_loc);

    g_lut_buffer = MALLOC(sizeof(unsigned char) * MAX_LUT_DN*3);

    read_lut(path_and_file, g_lut_buffer);
    g_have_lut = TRUE;

    free(path_and_file);
}

void select_lut(const char *lut_basename)
{
    int which = 0;

    // there must be a better way!
    if (strcmp_case(lut_basename, "cloude16") == 0)
      which = get_cloude16_lut_index();
    else if (strcmp_case(lut_basename, "cloude8") == 0)
      which = get_cloude8_lut_index();
    else if (strcmp_case(lut_basename, "dem") == 0)
      which = get_dem_lut_index();
    else if (strcmp_case(lut_basename, "interferogram") == 0)
      which = get_interferogram_lut_index();
    else if (strcmp_case(lut_basename, "unwrapping_mask") == 0)
      which = get_unwrapping_mask_lut_index();
    else if (strcmp_case(lut_basename, "layover_mask") == 0)
      which = get_layover_mask_lut_index();
    else if (strcmp_case(lut_basename, "polarimetry") == 0)
      which = get_polarimetry_lut_index();
    else if (strcmp_case(lut_basename, "water_mask") == 0)
        which = get_water_mask_lut_index();
    else if (strcmp_case(lut_basename, EMBEDDED_TIFF_COLORMAP_LUT) == 0)
        which = get_tiff_lut_index();
    else if (strcmp_case(lut_basename, EMBEDDED_ASF_COLORMAP_LUT) == 0)
        which = get_asf_lut_index();
    else if (!lut_basename || strlen(lut_basename) < 1 ||
             strcmp_case(lut_basename, "none") == 0)
        which = 0; // Default to none

    GtkWidget *option_menu = get_widget_checked("lut_optionmenu");
    gtk_option_menu_set_history(GTK_OPTION_MENU(option_menu), which);
    set_current_index(which);
}

void check_lut()
{
    if (g_lut_buffer) {
        free(g_lut_buffer);
        g_lut_buffer = NULL;
    }

    GtkWidget *option_menu = get_widget_checked("lut_optionmenu");
    GtkWidget *menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(option_menu));
    GtkWidget *selected_item = gtk_menu_get_active(GTK_MENU(menu));

    const char *lut_basename =
        g_object_get_data(G_OBJECT(selected_item), "file");
    if (!lut_basename) {
        // no lut selected
        g_have_lut = FALSE;
    }
    else {
        // something was selected!
        char filename[512];
        sprintf(filename, "%s.lut", lut_basename);

        char *lut_loc = get_lut_loc();
        char *path_and_file =
            MALLOC(sizeof(char)*(strlen(lut_loc)+strlen(filename)+20));
        sprintf(path_and_file, "%s/%s", lut_loc, filename);
        free(lut_loc);

        g_lut_buffer = MALLOC(sizeof(unsigned char) * MAX_LUT_DN*3);

        read_lut(path_and_file, g_lut_buffer);
        g_have_lut = TRUE;
        select_lut(lut_basename);

        free(path_and_file);
    }
}

int have_lut()
{
    return g_have_lut;
}

void apply_lut(int val, unsigned char *r,
               unsigned char *g, unsigned char *b)
{
    assert(g_have_lut);
    assert(g_lut_buffer);

    if (val>MAX_LUT_DN-1)
        val = MAX_LUT_DN-1;
    else if (val<0)
        val = 0;

    *r = g_lut_buffer[val*3];
    *g = g_lut_buffer[val*3+1];
    *b = g_lut_buffer[val*3+2];
}

static int get_default_lut(image_data_type_t image_data_type)
{
    // for now, just return 0 -- always default to no LUT
    return 0;
    //int which=0;
    //if (image_data_type == DEM) {
    //    // DEMs use the awesome "dem.lut"
    //    printf("Preselecting color look-up-table: DEM\n");
    //    which = g_dem_index;
    //}
    //return which;
}

int set_lut_based_on_image_type(image_data_type_t image_data_type)
{
    int which = get_default_lut(image_data_type);
    GtkWidget *option_menu = get_widget_checked("lut_optionmenu");
    GtkWidget *menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(option_menu));
    gtk_menu_set_active(GTK_MENU(menu), which);
    return which > 0;
}

void apply_lut_to_data(ThumbnailData *td)
{
    assert(g_have_lut);
    unsigned char *data = td->data;

    int ii, jj;
    for ( ii = 0 ; ii < td->size_y ; ii++ ) {
        for ( jj = 0 ; jj < td->size_x ; jj++ ) {
            int index = jj+ii*td->size_x;
            int n = 3*index;
            unsigned char uval = data[n];
            double val = (((double)uval - .5) * (curr->stats.map_max-curr->stats.map_min)) / 255. + curr->stats.map_min;
            apply_lut((int)(val+.5), &data[n], &data[n+1], &data[n+2]);
        }
    }
}

int check_for_embedded_tiff_lut (char *curr_file, int *lut_specified, char *lut)
{
    TIFF *tiff = NULL;
    short sample_format;
    short bits_per_sample;
    short planar_config;
    short num_bands = 0;
    int is_scanline_format;
    int is_palette_color_tiff;
    data_type_t data_type;
    char *ext;
    int ret = 0;

    ext = findExt(curr_file);
    if (ext && strlen(ext) && strncmp(uc(ext), ".TIF", 4) == 0) {
      tiff = XTIFFOpen(curr_file, "r");
      if (tiff) {
          get_tiff_data_config(tiff, &sample_format, &bits_per_sample, &planar_config,
                              &data_type, &num_bands, &is_scanline_format,
                              &is_palette_color_tiff, REPORT_LEVEL_NONE);
          *lut_specified = is_palette_color_tiff;
          if (is_palette_color_tiff) {
              strcpy(lut, EMBEDDED_TIFF_COLORMAP_LUT);
              ret = 1;
          }
      }
    }

    return ret;
}

int get_cloude16_lut_index(void) {
    return g_cloude16_index;
}

int get_cloude8_lut_index(void) {
    return g_cloude8_index;
}

int get_dem_lut_index(void) {
    return g_dem_index;
}

int get_interferogram_lut_index(void) {
    return g_interferogram_index;
}

int get_unwrapping_mask_lut_index(void) {
    return g_unwrapping_mask_index;
}

int get_layover_mask_lut_index(void) {
    return g_layover_mask_index;
}

int get_polarimetry_lut_index(void) {
    return g_polarimetry_index;
}

int get_water_mask_lut_index(void) {
    return g_water_mask_index;
}

int get_tiff_lut_index(void)
{
  return g_tiff_lut_index;
}

int get_asf_lut_index(void)
{
  return g_asf_lut_index;
}

int get_current_index(void) {
    return g_current_index;
}

void set_current_index(int index) {
    g_current_index = index;
}

int is_colormap_ASF_file(char *file)
{
  int ret=0;
  char *img_file;
  char *meta_file;

  img_file = appendExt(file, ".img");
  meta_file = appendExt(file, ".meta");
  if (fileExists(img_file) && fileExists(meta_file)) {
    meta_parameters *meta = meta_read(meta_file);
    ret = meta->colormap ? 1 : 0;
  }
  FREE(img_file);
  FREE(meta_file);

  return ret;
}
