#include "req.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>

void clear_csv_combo()
{
    GtkWidget *w = get_widget_checked("csv_dir_combobox");
    gtk_combo_box_remove_text(GTK_COMBO_BOX(w), 0);
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
                sprintf(name, "%s/%s", csv_dir, dp->d_name);
                char *e = findExt(name);
                if (strcmp_case(e, ".csv") == 0) {
                    struct stat st_buf;
                    int status = stat(name, &st_buf);
                    if (status != -1) {
                        printf("Checking: %s %ld\n", name, (long)st_buf.st_mtime);
                        if (st_buf.st_mtime > most_recent_time) {
                            to_be_selected = num;
                            printf(" --> Most recent found so far!\n");
                        }
                        printf("Adding: %s\n", name);
                        add_to_csv_combo(name);
                        ++num;
                    } else {
                        printf("Error getting stats for: %s\n", name);
                    }
                }
            }
        }
        closedir(dfd);

        if (to_be_selected > 0) {
            csv_combo_select(to_be_selected);
            process();
        }
    }
}
