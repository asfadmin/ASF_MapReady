// We aren't using the asf.h ones because of conflicts in Windows.h
#if defined(win32)
const char PATH_SEPARATOR = ';';
const char DIR_SEPARATOR = '\\';
#else
const char PATH_SEPARATOR = ':';
const char DIR_SEPARATOR;
#endif

#include "asf_view.h"
#include <ctype.h>

void clear_combobox(const char *widget_name)
{
    GtkWidget *w = get_widget_checked(widget_name);
    int x=0;
    while (1 /* gtk_combo_box_get_active(GTK_COMBO_BOX(w)) != -1*/) {
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

void set_combo_box_item(const char *widget_name, gint index)
{
    GtkWidget *ddl = get_widget_checked(widget_name);
    gtk_combo_box_set_active(GTK_COMBO_BOX(ddl), index);
}

int get_combo_box_item(const char *widget_name)
{
    GtkWidget *ddl = get_widget_checked(widget_name);
    return (int)gtk_combo_box_get_active(GTK_COMBO_BOX(ddl));
}

char *get_band_combo_text(meta_parameters *meta, const char *widget_name)
{
    // specific to combobox populated with meta->general->bands
    GtkWidget *w = get_widget_checked(widget_name);
    int i = gtk_combo_box_get_active(GTK_COMBO_BOX(w));
    char *b = STRDUP(meta->general->bands);
    char *p = b;
    while (i-- > 0) {
        char *p1 = strchr(p,',');
        if (!p1) { printf("Can't happen!\n"); break; }
        p=p1+1;
    }
    char *q = strchr(p+1,',');
    if (q) *q = '\0';

    char *ret = STRDUP(p);
    free(b);
    return ret;
}

void rb_select(const char *widget_name, gboolean is_on)
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

    // strip off trailing zeros, and a trailing decimal point
    if (strchr(tmp, '.')) {
      while (tmp[strlen(tmp)-1]=='0') tmp[strlen(tmp)-1]='\0';
      if (tmp[strlen(tmp)-1]=='.') tmp[strlen(tmp)-1]='\0';
    }

    gtk_entry_set_text(GTK_ENTRY(e), tmp);
}

void put_double_to_entry_fmt(const char *widget_name, double val,
                             const char *format)
{
    GtkWidget *e = get_widget_checked(widget_name);
    
    char tmp[64];
    sprintf(tmp, format, val);

    gtk_entry_set_text(GTK_ENTRY(e), tmp);
}

char* get_string_from_entry(const char *widget_name)
{
    GtkWidget *e = get_widget_checked(widget_name);
    return (char*)gtk_entry_get_text(GTK_ENTRY(e));
}

int entry_has_text(const char *widget_name)
{
    return strlen(get_string_from_entry(widget_name)) > 0;
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

static void
put_text_in_textview_impl(const gchar * txt, GtkWidget * textview_output)
{
    GtkTextBuffer * text_buffer;
    GtkTextIter end;
    GtkTextTag *tt;
    GtkTextTagTable *tt_table;

    text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview_output));
    tt_table = gtk_text_buffer_get_tag_table(text_buffer);    
    tt = gtk_text_tag_table_lookup(tt_table, "mono");

    if (!tt)
    {
#ifdef win32
	tt = gtk_text_buffer_create_tag(text_buffer, "mono", 
					"font", "Courier", NULL);
#else
	tt = gtk_text_buffer_create_tag(text_buffer, "mono", "font", "Mono",
                                        "size-points", (double)8.0, NULL);
#endif
    }

    if (gtk_text_buffer_get_char_count(text_buffer) > 0)
    {
        GtkTextIter b, e;

        gtk_text_buffer_get_start_iter(text_buffer, &b);
        gtk_text_buffer_get_end_iter(text_buffer, &e);
        gtk_text_buffer_delete(text_buffer, &b, &e);
    }

    gtk_text_buffer_get_end_iter(text_buffer, &end);
    gtk_text_buffer_insert_with_tags(text_buffer, &end, txt, -1, tt, NULL);
}

void put_file_in_textview(const char *file, const char *widget_name)
{
    FILE *f = fopen(file, "r");
    if (f) {
        // only put the first MAX chars, actually
        int MAX=32768;
        char buf[MAX];
        GtkWidget *tv = get_widget_checked(widget_name);
        fread(buf, sizeof(char), MAX, f);
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

char *trim_whitespace(const char *s)
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
