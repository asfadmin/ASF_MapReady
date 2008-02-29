#include "asf_view.h"

// global variable for the band info
// BandConfig g_band_cfg;

static int is_multiband = FALSE;

void set_bands_rgb(int r, int g, int b)
{
    curr->band_cfg.is_rgb = TRUE;
    curr->band_cfg.band_r = r;
    curr->band_cfg.band_g = g;
    curr->band_cfg.band_b = b;
    curr->band_cfg.band_gs = -1;
}

void set_bands_greyscale(int b)
{
    curr->band_cfg.is_rgb = FALSE;
    curr->band_cfg.band_r = -1;
    curr->band_cfg.band_g = -1;
    curr->band_cfg.band_b = -1;
    curr->band_cfg.band_gs = b;
}

static void single_band(const char *str)
{
    show_widget("hbox_single", TRUE);
    show_widget("hbox_multi", FALSE);

    if (strlen(str)>0 && strcmp(str, MAGIC_UNSET_STRING)!=0) {
        char tmp[128];
        snprintf(tmp, 128, "Band: %s", str);
        put_string_to_label("single_band_label", tmp);
    } else
        put_string_to_label("single_band_label", "Bands: -");

    is_multiband = FALSE;
}

static void multi_band(const char *str)
{
    show_widget("hbox_single", FALSE);
    show_widget("hbox_multi", TRUE);

    char tmp[128];
    snprintf(tmp, 128, "Bands: %s", br(str));
    put_string_to_label("multi_band_label", tmp);

    is_multiband = TRUE;
}

static void disable_correct_hbox()
{
    int is_rgb = get_checked("combine_bands_into_rgb_radiobutton");
    enable_widget("single_band_as_greyscale_hbox", !is_rgb);
    enable_widget("combine_bands_into_rgb_hbox", is_rgb);
    enable_widget("lut_optionmenu", !is_rgb);
}

static void populate_combo_csv(const char *widget_name, char *csv, int i)
{
    clear_combobox(widget_name);

    char *p = csv; // current point (past the last seen comma)
    char *q = strchr(p, ','); // next point - 1 (the next comma)
    while (q) {
        *q = '\0'; // temporary
        add_to_combobox(widget_name, p);
        *q = ',';

        // go to next item
        p = q+1; 
        q = strchr(p, ',');
    }

    // add the last piece of the string
    add_to_combobox(widget_name, p);

    // select the correct item
    if (i<0) i=0;
    set_combo_box_item(widget_name, i);
}

void setup_bands_tab(meta_parameters *meta)
{
    int multilook, multiband;

    if (!meta ||
        meta->general->band_count == 1 || 
        meta->general->band_count == MAGIC_UNSET_INT ||
        !meta->general->bands ||
        strlen(meta->general->bands) == 0 ||
        strcmp(meta->general->bands, MAGIC_UNSET_STRING) == 0)
    {
        single_band(meta->general->bands);
        multiband = FALSE;
    }
    else {
        // populate band selectors
        //enable_widget("single_band_as_greyscale_hbox", TRUE);
        //enable_widget("combine_bands_into_rgb_hbox", TRUE);
        populate_combo_csv("red_combobox",
            meta->general->bands, curr->band_cfg.band_r);
        populate_combo_csv("green_combobox",
            meta->general->bands, curr->band_cfg.band_g);
        populate_combo_csv("blue_combobox",
            meta->general->bands, curr->band_cfg.band_b);
        populate_combo_csv("single_band_as_greyscale_combobox",
            meta->general->bands, curr->band_cfg.band_gs);

        multi_band(meta->general->bands);

        set_checked("combine_bands_into_rgb_radiobutton",
                    curr->band_cfg.is_rgb);
        set_checked("single_band_as_greyscale_radiobutton",
                    !curr->band_cfg.is_rgb);

        disable_correct_hbox();
        multiband = TRUE;
    }

    //set_checked("multilook_checkbutton", FALSE);
    if (!meta->sar || meta->sar->multilook) {
        multilook = FALSE;
        enable_widget("multilook_checkbutton", FALSE);
    } else {
        multilook = TRUE;
        enable_widget("multilook_checkbutton", TRUE);
    }

    //show_widget("hbox_bands_buttons", multilook || multiband);
}

SIGNAL_CALLBACK void
on_single_band_as_greyscale_radiobutton_toggled(GtkWidget *w)
{
    disable_correct_hbox();
}

SIGNAL_CALLBACK void
on_combine_bands_into_rgb_radiobutton_toggled(GtkWidget *w)
{
    disable_correct_hbox();
}

SIGNAL_CALLBACK void
on_bands_apply_button_clicked(GtkWidget *w)
{
    int ml = get_checked("multilook_checkbutton");
    if (!is_multiband) {
        // multilook checkbox only, single-band data
        char *f = STRDUP(curr->filename);
        reload_file_banded(f, NULL, ml);
        free(f);
    }
    else if (get_checked("combine_bands_into_rgb_radiobutton")) {
        // multiband rgb
        char *r = get_band_combo_text(curr->meta, "red_combobox");
        char *g = get_band_combo_text(curr->meta, "green_combobox");
        char *b = get_band_combo_text(curr->meta, "blue_combobox");
        char *s = MALLOC(sizeof(char)*(strlen(r)+strlen(g)+strlen(b)+10));
        sprintf(s,"%s,%s,%s",r?r:"-",g?g:"-",b?b:"-");
        //printf("Load banded: %s, %s\n", g_filename, s);
        char *f = STRDUP(curr->filename);
        reload_file_banded(f, s, ml);
        free(s);
        FREE(r); FREE(g); FREE(b);
        free(f);
    } else {
        // greyscale
        char *gs = get_band_combo_text(curr->meta,
                                     "single_band_as_greyscale_combobox");
        if (gs) {
            char *f = STRDUP(curr->filename);
            reload_file_banded(f, gs, ml);
            FREE(gs);
            FREE(f);
        }
    }
}

SIGNAL_CALLBACK void
on_bands_cancel_button_clicked(GtkWidget *w)
{
    setup_bands_tab(curr->meta);
}
