#include "req.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>

void clear_csv_combo()
{
    GtkWidget *w = get_widget_checked("csv_dir_combobox");
    do {
        gtk_combo_box_remove_text(GTK_COMBO_BOX(w), 0);
    } while (gtk_combo_box_get_active(GTK_COMBO_BOX(w)) != -1);
}

void add_to_csv_combo(const char *txt)
{
    add_to_combobox("csv_dir_combobox", txt);    
}

void csv_combo_select(int i)
{
    set_combo_box_item_checked("csv_dir_combobox", i);
}

void populate_csvs()
{
    char *csv_dir = settings_get_csv_dir();
    if (csv_dir)
    {
        char name[1024];
        int to_be_selected=-1;
        int num=0;
        time_t most_recent_time = (time_t)0;
        struct dirent *dp;
        DIR *dfd;

        clear_csv_combo();
        if ((dfd = opendir(csv_dir)) == NULL) {
            message_box("Error opening csv dir: %s\n", csv_dir);
            return;
        }
        while ((dp = readdir(dfd)) != NULL) {
            if (strcmp(dp->d_name, ".")==0 || strcmp(dp->d_name, "..")==0) {
                continue;
            }
            if (strlen(csv_dir)+strlen(dp->d_name)+2 > sizeof(name)) {
                printf("name %s/%s exceeds buffersize.\n", csv_dir, dp->d_name);
                return;
            }
            else {
#ifdef win32
                sprintf(name, "%s\\%s", csv_dir, dp->d_name);
#else
                sprintf(name, "%s/%s", csv_dir, dp->d_name);
#endif
                //printf("Checking: %s\n", name);
                char *e = findExt(name);
                if (e && strcmp_case(e, ".csv") == 0) {
                    struct stat st_buf;
                    int status = stat(name, &st_buf);
                    //printf("Status: %d\b", status);
                    if (status != -1) {
                        //printf("Checking: %s %ld\n", name, (long)st_buf.st_mtime);
                        if (st_buf.st_mtime > most_recent_time) {
                            to_be_selected = num;
                            most_recent_time = st_buf.st_mtime;
                            //printf(" --> Most recent found so far!\n");
                        }
                        //printf("Adding: %s\n", name);
                        add_to_csv_combo(name);
                        ++num;
                    } else {
                        printf("Error getting stats for: %s\n", name);
                    }
                }
            }
        }
        closedir(dfd);

        if (to_be_selected >= 0) {
            csv_combo_select(to_be_selected);
            gui_process(FALSE);
        }
    }
}

SIGNAL_CALLBACK void
on_csv_dir_combobox_changed(GtkWidget *w, gpointer callback_data)
{
    settings_set_start_date(-1);
    settings_set_end_date(-1);
    gui_process(FALSE);
}

void hook_up_csv_dir_entry_changed()
{
    GtkWidget *w = get_widget_checked("csv_dir_combobox");
    GtkEntry *e = GTK_ENTRY (GTK_BIN (w)->child);
    g_signal_connect(GTK_OBJECT(e), "csv_changed",
        G_CALLBACK(on_csv_dir_combobox_changed), NULL);

}

void alert(const char *s)
{
    put_string_to_label("generate_label", s);
}
