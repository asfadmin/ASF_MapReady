#include "req.h"

static void
get_combo_box_entry_item(const char *widget_name, char *dest)
{
    GtkWidget *w = get_widget_checked(widget_name);
    GtkEntry *e = GTK_ENTRY (GTK_BIN (w)->child);
    strcpy(dest, gtk_entry_get_text(e));
}

void process()
{
    char csv_file[1024];
    get_combo_box_entry_item("csv_dir_combobox", csv_file);    
    if (strlen(csv_file) > 0)
    {
        if (fileExists(csv_file)) {
            printf("Processing: %s\n", csv_file);
        } else {
            message_box("File not found: %s\n", csv_file);
        }
    }
}
