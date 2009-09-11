// We aren't using the asf.h ones because of conflicts in Windows.h
#if defined(win32)
const char PATH_SEPARATOR = ';';
const char DIR_SEPARATOR = '\\';
#else
const char PATH_SEPARATOR = ':';
const char DIR_SEPARATOR;
#endif

#include "proj2proj.h"

void clear_combobox(const char *widget_name)
{
    GtkWidget *w = get_widget_checked(widget_name);
    int x=0;
    while (gtk_combo_box_get_active(GTK_COMBO_BOX(w)) != -1) {
        gtk_combo_box_remove_text(GTK_COMBO_BOX(w), 0);
        if (++x>MAX_BANDS) break;
    }
}

void add_to_combobox(const char *widget_name, const char *txt)
{
    GtkWidget *w = get_widget_checked(widget_name);
    gtk_combo_box_append_text(GTK_COMBO_BOX(w), txt);
}

void set_combobox_entry_maxlen(const char *widget_name, int maxlen)
{
    GtkWidget *w = get_widget_checked(widget_name);
    GtkEntry *e = GTK_ENTRY (GTK_BIN (w)->child);
    gtk_entry_set_max_length(e, maxlen);
}

void
set_combo_box_item_checked(const char *widget_name, gint index)
{
    GtkWidget *ddl = get_widget_checked(widget_name);
    gtk_combo_box_set_active(GTK_COMBO_BOX(ddl), index);
}

void
set_combo_box_item(GtkWidget *ddl, gint index)
{
    gtk_option_menu_set_history(GTK_OPTION_MENU(ddl), index);
}

void
rb_select(const char *widget_name, gboolean is_on)
{
    GtkWidget *rb = get_widget_checked(widget_name);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rb), is_on);
}

double get_double_from_entry(const char *widget_name)
{
    GtkWidget *e = get_widget_checked(widget_name);
    return atof(gtk_entry_get_text(GTK_ENTRY(e)));
}

void put_double_to_entry(const char *widget_name, double val)
{
    GtkWidget *e = get_widget_checked(widget_name);
    
    char tmp[64];
    sprintf(tmp, "%f", val);

    gtk_entry_set_text(GTK_ENTRY(e), tmp);
}

char* get_string_from_entry(const char *widget_name)
{
    GtkWidget *e = get_widget_checked(widget_name);
    return (char*)gtk_entry_get_text(GTK_ENTRY(e));
}

void put_string_to_entry(const char *widget_name, char *txt)
{
    GtkWidget *e = get_widget_checked(widget_name);
    gtk_entry_set_text(GTK_ENTRY(e), txt ? txt : "");
}

char *get_string_from_comboboxentry(const char *widget_name)
{
    GtkWidget *cbe = get_widget_checked(widget_name);
    GtkEntry *e = GTK_ENTRY(GTK_BIN(cbe)->child);
    return (char*)gtk_entry_get_text(e);
}

void put_string_to_comboboxentry(const char *widget_name, char *txt)
{
    GtkWidget *cbe = get_widget_checked(widget_name);
    GtkEntry *e = GTK_ENTRY(GTK_BIN(cbe)->child);
    gtk_entry_set_text(e, txt ? txt : "");
}

int get_int_from_entry(const char *widget_name)
{
    GtkWidget *e = get_widget_checked(widget_name);
    return atoi(gtk_entry_get_text(GTK_ENTRY(e)));
}

void put_int_to_entry(const char *widget_name, int val)
{
    GtkWidget *e = get_widget_checked(widget_name);
    
    char tmp[64];
    sprintf(tmp, "%d", val);

    gtk_entry_set_text(GTK_ENTRY(e), tmp);
}


int get_checked(const char *widget_name)
{
    GtkWidget *cb = get_widget_checked(widget_name);
    return gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(cb));
}

void set_checked(const char *widget_name, int checked)
{
    GtkWidget *cb = get_widget_checked(widget_name);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cb), checked);
}

void message_box(const char *format, ...)
{
    char buf[1024];
    int len;

    va_list ap;
    va_start(ap, format);
    len = vsnprintf(buf, sizeof(buf), format, ap);
    va_end(ap);

    if (len > 1022)
        printf("Lengthy message may have been truncated.\n");

    // strip trailing newline -- looks much nicer, and allows us to
    // call asfPrintWarning/Error & message_box interchangably and
    // have both look good.
    if (buf[strlen(buf)-1] == '\n')
        buf[strlen(buf)-1] = '\0';

    GtkWidget *dialog, *label;

    dialog = gtk_dialog_new_with_buttons( "Message",
        NULL,
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        GTK_STOCK_OK,
        GTK_RESPONSE_NONE,
        NULL);

    label = gtk_label_new(buf);

    g_signal_connect_swapped(dialog, 
        "response", 
        G_CALLBACK(gtk_widget_destroy),
        dialog);

    gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);

    gtk_widget_show_all(dialog);

    // Seems that sometimes the message box ends up hidden behind other
    // windows... this might bring it to the front
    gtk_window_present(GTK_WINDOW(dialog));
}

GtkWidget *get_widget_checked(const char *widget_name)
{
    GtkWidget *w = glade_xml_get_widget(glade_xml, widget_name);
    if (!w)
    {
        asfPrintError("get_widget_checked() failed: "
            "The widget %s was not found.\n", widget_name);
    }
    return w;
}

GtkWidget *get_widget_checked2(int source, const char *widget_name)
{
    char tmp_name[512];
    snprintf(tmp_name, 512, "%s_%s", source?"source":"target", widget_name);

    GtkWidget *w = glade_xml_get_widget(glade_xml, tmp_name);
    if (!w)
    {
        asfPrintError("get_widget_checked2() failed: "
            "The widget %s was not found.\n", tmp_name);
    }
    return w;
}

static void
put_text_in_textview_impl(const gchar * txt, GtkWidget * textview_output)
{
    GtkTextBuffer *text_buffer;
    text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview_output));
    if (gtk_text_buffer_get_char_count(text_buffer) > 0)
    {
        GtkTextIter b, e;

        gtk_text_buffer_get_start_iter(text_buffer, &b);
        gtk_text_buffer_get_end_iter(text_buffer, &e);
        gtk_text_buffer_delete(text_buffer, &b, &e);
    }

    GtkTextIter end;
    gtk_text_buffer_get_end_iter(text_buffer, &end);

    gtk_text_buffer_insert(text_buffer, &end, txt, -1);
}

void put_file_in_textview(const char *file, const char *widget_name)
{
    FILE *f = fopen(file, "r");
    if (f) {
        // only put the first MAX chars, actually
        int MAX=32768;
        char buf[MAX+1];
        GtkWidget *tv = get_widget_checked(widget_name);

        int len = fread(buf, sizeof(char), MAX, f);
        buf[len] = '\0';

        // remove EOF character (not needed on Windows)
#ifndef win32
        buf[strlen(buf)-1]='\0';
#endif

        // hack: ensure valid ascii
        int i;
        for (i=0; i<strlen(buf); ++i)
          buf[i] &= 0x7F;

        put_text_in_textview_impl(buf, tv);
        fclose(f);
    }
}

void put_text_in_textview(const char *txt, const char *widget_name)
{
    GtkWidget *tv = get_widget_checked(widget_name);
    put_text_in_textview_impl(txt, tv);
}

void put_string_to_label(const char *widget_name, const char *txt)
{
    GtkWidget *w = get_widget_checked(widget_name);
    gtk_label_set_text(GTK_LABEL(w), txt);
}

void show_widget(const char *widget_name, int show)
{
    GtkWidget *w = get_widget_checked(widget_name);
    if (show)
        gtk_widget_show(w);
    else
        gtk_widget_hide(w);
}

void enable_widget(const char *widget_name, int enable)
{
    GtkWidget *w = get_widget_checked(widget_name);
    gtk_widget_set_sensitive(w, enable);
}
