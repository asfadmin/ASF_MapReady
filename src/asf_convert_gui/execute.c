
#include <unistd.h>
#include "asf_convert_gui.h"
#include <asf.h>
#include <asf_convert.h>

/*
#include <fcntl.h>
#include <sys/select.h>
*/

#include <errno.h>
#include <sys/wait.h>
#include <time.h>
#include <sys/stat.h>

#ifdef win32
#include <process.h>
#endif

#include "asf.h"

static gboolean keep_going = TRUE;

static gboolean confirm_overwrite()
{
    GtkTreeIter iter;
    gboolean valid;
    gboolean exist = FALSE;
    gboolean settings_different = TRUE;

    Settings * user_settings;

    user_settings = settings_get_from_gui();
    if (settings_on_execute)
    {
        settings_different = !settings_equal(user_settings,
            settings_on_execute);
    }

    valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);

    while (valid)
    {
        gchar *output_file;
        gchar *status;
        gboolean done;

        gtk_tree_model_get (GTK_TREE_MODEL(list_store), &iter,
            COL_OUTPUT_FILE, &output_file,
            COL_STATUS, &status, -1);

        done = strcmp("Done", status) == 0;

        if ((settings_different || !done) &&
            g_file_test(output_file, G_FILE_TEST_EXISTS))
            exist = TRUE;

        g_free(output_file);
        g_free(status);

        if (exist)
            break;

        valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
    }

    settings_delete(user_settings);

    if (exist)
    {
        GtkWidget * dialog_confirm_overwrite;
        gint result;

        dialog_confirm_overwrite =
            get_widget_checked("dialog_confirm_overwrite");

        result = gtk_dialog_run( GTK_DIALOG(dialog_confirm_overwrite) );
        gtk_widget_hide( dialog_confirm_overwrite );

        switch (result)
        {
        default:
            return FALSE;

        case GTK_RESPONSE_OK:
            return TRUE;
        }
    }
    else
    {
        /* no need to confirm -- no overwrites */
        return TRUE;
    }
}

gchar *
do_cmd(gchar *cmd, gchar *log_file_name)
{
    gchar *the_output;
    FILE *output;

    int pid = fork();

    if (pid == 0)
    {
        int ret;

        ret = asfSystem(cmd);
        if (ret == -1 || ret > 0)
        {
            int saved_errno;
            /* Problem running the command... if we got a log file assume the
            error is logged in there, and user will see it through that.

            Otherwise, see if errno can give us anything useful.
            Put this into the log file that parent expects to see.
            */

            saved_errno = errno;

            if (!g_file_test(log_file_name, G_FILE_TEST_EXISTS))
            {
                output = fopen(log_file_name, "wt");
                if (output)
                {
                    if (saved_errno > 0)
                    {
                        fprintf(output,
                            "*** Error! Could not run command. (%d) ***\n"
                            "Command: %s\n"
                            "Error %d: %s\n",
                            ret, cmd, saved_errno, strerror(saved_errno));
                    }
                    else
                    {
                        gchar *p;
                        gboolean have_import = FALSE, have_export = FALSE;

                        /* one possibility is that they haven't got asf_import
                        or asf_export.  Check for that. */
                        p = find_in_path("asf_import");
                        if (!p)
                        {
                            p = find_in_path("asf_import.exe");
                            if (p)
                            {
                                have_import = TRUE;
                                g_free(p);
                            }
                        }
                        else
                        {
                            have_import = TRUE;
                            g_free(p);
                        }

                        p = find_in_path("asf_export");
                        if (!p)
                        {
                            p = find_in_path("asf_export.exe");
                            if (p)
                            {
                                have_export = TRUE;
                                g_free(p);
                            }
                        }
                        else
                        {
                            have_export = TRUE;
                            g_free(p);
                        }

                        if (have_import && have_export)
                        {
                            /* found the executable(s)... not sure what went wrong. */
                            fprintf(output, "Unknown error trying to run command:\n%s\n",
                                cmd);
                        }
                        else
                        {
                            fprintf(output,
                                "*** ERROR! ***\n"
                                "Couldn't find one, or both, of asf_import or "
                                "asf_export!\n"
                                "Please ensure that these programs are installed, "
                                "and are in your PATH.\n"
                                "Was trying to run the command:\n%s\n",
                                cmd);
                        }
                    }
                    fclose(output);
                }
                else
                {
                    /* couldn't open ... */
                }
            }
        }

        exit(EXIT_SUCCESS);
    }
    else
    {
        while (waitpid(-1, NULL, WNOHANG) == 0)
        {
            while (gtk_events_pending())
                gtk_main_iteration();

            g_usleep(50);
        }
    }


    the_output = NULL;

    output = fopen(log_file_name, "rt");

    if (!output)
    {
        the_output = (gchar *)g_malloc(512);
        sprintf(the_output, "Error Opening Log File: %s\n", strerror(errno));
    }
    else
    {
        while (!feof(output))
        {
            gchar buffer[4096];
            gchar *p = fgets(buffer, sizeof(buffer), output);
            if (p && !g_str_has_prefix(p, "Processing "))
            {
                if (the_output)
                {
                    the_output = (gchar *)g_realloc(the_output, sizeof(gchar) *
                        (strlen(the_output) + strlen(buffer) + 1));

                    strcat(the_output, buffer);
                }
                else
                {
                    the_output = (gchar *)g_malloc(sizeof(gchar) * (strlen(buffer) + 1));
                    strcpy(the_output, buffer);
                }
            }
        }
        fclose(output);
        remove(log_file_name);
    }

    if (!the_output)
    {
        /* Log file existed, but had immediate EOF */
        /* This is most likely caused by a "Disk Full" situation... */
        /*  ... or a segmentation fault! */
        the_output = g_strdup("Error Opening Log File: Disk Full?\n");
    }

    return the_output;
}

gboolean check_for_error(gchar * txt)
{
    /* kludge */
    gchar *p, *q;

    p = txt;

    while (p)
    {
        q = strchr(p + 1, '\n');
        if (q)
        {
            *q = '\0';

            /* ignore use of the word "error" in the comments */
            if (strstr(p, "Calibration Comments") == NULL &&
                (strstr(p, "Error") != NULL ||
                strstr(p, "ERROR") != NULL))
            {
                *q = '\n';
                return TRUE;
            }

            *q = '\n';
        }

        p = q;
    }

    return FALSE;
}

static void set_thumbnail(GtkTreeIter *iter, const gchar * tmp_dir,
                          const gchar *out_full)
{
    if (use_thumbnails)
    {
        int scaling_required = FALSE;

        char *basename = get_basename(out_full);
        char *thumbnail_name =
            MALLOC(sizeof(char)*(strlen(tmp_dir)+strlen(basename)+32));
        sprintf(thumbnail_name, "%s/%s_thumb.jpg", tmp_dir, basename);

        if (!fileExists(thumbnail_name)) {
            if (strcmp(findExt(out_full), ".jpg") == 0) {
                scaling_required = TRUE;
                strcpy(thumbnail_name, out_full);
            }
        }

        printf("Thumb: %s\nscale= %d\n", thumbnail_name, scaling_required);

        GError * err = NULL;
        GdkPixbuf * pb;

	if (g_file_test(thumbnail_name, G_FILE_TEST_EXISTS))
	{
            if (scaling_required)
                pb = gdk_pixbuf_new_from_file_at_size(thumbnail_name,
                                                      THUMB_SIZE, THUMB_SIZE,
                                                      &err);
            else
                pb = gdk_pixbuf_new_from_file(thumbnail_name, &err);

            if (!err)
            {
                gtk_list_store_set(completed_list_store, iter,
                                   COMP_COL_OUTPUT_THUMBNAIL, pb, -1);
            }
            else
            {
                g_warning("Error loading image '%s': %s\n",
                          thumbnail_name, err->message);
                g_error_free(err);
            }
	}
        
        free(thumbnail_name);
    }
}

static gboolean
have_access_to_dir(const gchar * dir, gchar ** err_string)
{
    struct stat buf;
    int result;

    if (strlen(dir) == 0)
        dir = ".";

    result = stat (dir, &buf);

    if (result == -1)
    {
        *err_string = (gchar *) g_malloc (sizeof(gchar) * 512);
        snprintf(*err_string, 512, "%s: %s", dir, strerror(errno));
        return FALSE;
    }
    else
    {
        /* Try opening a file in that dir */
        FILE * tmp;
        gchar fname [4096];
        sprintf(fname, "%s/tmp00%li", dir, time(NULL));
        tmp = fopen(fname, "wt");
        if (!tmp)
        {
            *err_string = (gchar *) g_malloc (sizeof(gchar) * 512);
            if (strcmp(dir, ".") == 0 && errno == EACCES)
                sprintf(*err_string, "Cannot write to output directory!");
            else
                snprintf(*err_string, 512, "%s: %s", dir, strerror(errno));

            return FALSE;
        }
        fclose(tmp);
        unlink(fname);
    }

    return TRUE;
}

//static void
//build_executable(char *buf, const char *exec_name)
//{
//#ifdef win32
//    sprintf(buf, "\"%s/%s\"", get_asf_bin_dir(), exec_name);
//#else
//    sprintf(buf, "%s/%s", get_asf_bin_dir(), exec_name);
//#endif
//}

static char *
do_convert(int pid, GtkTreeIter *iter, char *cfg_file, int save_dem,
           int keep_files)
{
    extern int logflag;
    extern FILE *fLog;

    FILE *output;
    char *logFile = appendExt(cfg_file, ".log");

    pid = fork();
    if (pid == 0)
    {
        /* child */
        logflag = TRUE;
        fLog = fopen(logFile, "a");

        asfPrintStatus("Running convert with configuration file: %s\n",
		       cfg_file);

	asf_convert_ext(FALSE, cfg_file, save_dem);

	FCLOSE(fLog);
	exit(EXIT_SUCCESS);
    }
    else
    {
        /* parent */
        int counter = 1;
	char *statFile = appendExt(cfg_file, ".status");
	char *projFile = appendExt(cfg_file, ".proj");

        while (waitpid(-1, NULL, WNOHANG) == 0)
	{
	    while (gtk_events_pending())
	      gtk_main_iteration();

            g_usleep(50);

	    if (++counter % 10 == 0)
	    {
	        /* check status file */
	        char buf[256];
		FILE *fStat = fopen(statFile, "rt");
		if (fStat)
		{
		    fgets(buf, sizeof(buf), fStat);
		    fclose(fStat);

		    gtk_list_store_set(list_store, iter, COL_STATUS, buf, -1);
		}
	    }
        }

	unlink(statFile);

	free(projFile);
	free(statFile);
    }

    gchar *the_output = NULL;

    output = fopen(logFile, "rt");

    if (!output)
    {
        the_output = (gchar *)g_malloc(512);
        sprintf(the_output, "Error Opening Log File: %s\n", strerror(errno));
    }
    else
    {
        while (!feof(output))
        {
            gchar buffer[4096];
            gchar *p = fgets(buffer, sizeof(buffer), output);
            if (p && !g_str_has_prefix(p, "Processing "))
            {
                if (the_output)
                {
                    the_output = (gchar *)g_realloc(the_output, sizeof(gchar) *
                        (strlen(the_output) + strlen(buffer) + 1));

                    strcat(the_output, buffer);
                }
                else
                {
                    the_output = (gchar *)
		      g_malloc(sizeof(gchar) * (strlen(buffer) + 1));

                    strcpy(the_output, buffer);
                }
            }
        }
        fclose(output);

        if (!keep_files)
            unlink(logFile);
    }

    if (!the_output)
    {
        /* Log file existed, but had immediate EOF */
        /* This is most likely caused by a "Disk Full" situation... */
        /*  ... or a segmentation fault! */
        the_output = g_strdup("Error Opening Log File: Disk Full?\n");
    }

    free(logFile);
    return the_output;
}

static void
process_item(GtkTreeIter *iter, Settings *user_settings, gboolean skip_done,
             int is_first)
{
    gchar *in_data, *out_full, *status;
    int pid;

    pid = getpid();

    gtk_tree_model_get(GTK_TREE_MODEL(list_store), iter,
        COL_DATA_FILE, &in_data,
        COL_OUTPUT_FILE, &out_full,
        COL_STATUS, &status,
        -1);

    if (strcmp(status, "Done") != 0 || !skip_done)
    {
        //char *in_basename = stripExt(in_data);
	char *out_basename = stripExt(out_full);
        char *out_nameonly = get_basename(out_full);
	char *output_dir = getPath(out_full);
	char *config_file, *cmd_output, *tmp_dir;
	gchar *err_string;
	int err;

        /* Ensure we have access to the output directory */
        if (!have_access_to_dir(output_dir, &err_string))
        {
            /* We don't -- issue a message in the "Status" column. */
            gtk_list_store_set(list_store, iter, COL_STATUS, err_string, -1);

            g_free(err_string);
            //free(in_basename);
            free(out_basename);
            free(output_dir);

            return;
        }

        tmp_dir = MALLOC(sizeof(char)*
                         (strlen(output_dir)+strlen(out_nameonly)+32));
        sprintf(tmp_dir, "%s/%s-%s", output_dir, out_nameonly,
                time_stamp_dir());

        create_clean_dir(tmp_dir);
	set_asf_tmp_dir(tmp_dir);

        settings_update_dem(user_settings, output_dir, is_first);
        settings_update_mask(user_settings, output_dir, is_first);

	config_file =
	  settings_to_config_file(user_settings, in_data, out_full,
				  output_dir, tmp_dir);
        if (!config_file) {
            message_box("Error creating configuration file.\n");
            return;
        }

	cmd_output = do_convert(pid, iter, config_file, TRUE,
                                user_settings->keep_files);
	err = check_for_error(cmd_output);
        if (err) {
            // unsuccessful
            gtk_list_store_set(list_store, iter, COL_STATUS, "Error", 
                               COL_LOG, cmd_output, -1);
        }
        else {
            // successful -- move to "completed" list
            GtkTreeIter completed_iter;
            move_to_completed_files_list(iter, &completed_iter, cmd_output);
            set_thumbnail(&completed_iter, tmp_dir, out_full);
        }

	free(config_file);
	free(out_basename);
	free(output_dir);
        free(out_nameonly);
	g_free(cmd_output);

        if (!user_settings->keep_files)
            remove_dir(tmp_dir);
    }

    g_free(status);
    g_free(out_full);
    g_free(in_data);
}

void
process_items_from_list(GList * list_of_row_refs, gboolean skip_done)
{
    GList * i;
    Settings * user_settings;
    GtkTreeIter iter;
    int is_first = TRUE;

    processing = TRUE;
    keep_going = TRUE;
    show_execute_button(FALSE);
    user_settings = settings_get_from_gui();

    i = list_of_row_refs;

    while (i && keep_going)
    {
        GtkTreeRowReference * ref;
        GtkTreePath * path;

        ref = (GtkTreeRowReference *) i->data;
        path = gtk_tree_row_reference_get_path(ref);

        gtk_tree_model_get_iter(GTK_TREE_MODEL(list_store), &iter, path);

        process_item(&iter, user_settings, skip_done, is_first);

        while (gtk_events_pending())
            gtk_main_iteration();

        i = g_list_next(i);
        is_first = FALSE;
    }

    processing = FALSE;
    settings_delete(user_settings);
    show_execute_button(TRUE);

    g_list_foreach(list_of_row_refs, (GFunc)gtk_tree_row_reference_free, NULL);
    g_list_free(list_of_row_refs);
}

SIGNAL_CALLBACK void
on_execute_button_clicked (GtkWidget *button)
{
    GtkTreeIter iter;
    gboolean valid;
    Settings * user_settings;
    GList * rows;

    /* gui should prevent this from happening */
    if (processing)
        return;

    if (confirm_overwrite())
    {
        user_settings = settings_get_from_gui();
        settings_on_execute = settings_copy(user_settings);

        rows = NULL;
        valid = gtk_tree_model_get_iter_first(
            GTK_TREE_MODEL(list_store), &iter);

        while (valid && keep_going)
        {
            GtkTreeRowReference * ref;
            GtkTreePath * path;

            path = gtk_tree_model_get_path(GTK_TREE_MODEL(list_store), &iter);
            ref = gtk_tree_row_reference_new(GTK_TREE_MODEL(list_store), path);

            rows = g_list_append(rows, ref);

            valid = gtk_tree_model_iter_next(
                GTK_TREE_MODEL(list_store), &iter);
        }

        process_items_from_list(rows, TRUE);
	settings_delete(user_settings);
    }
}

SIGNAL_CALLBACK void
on_stop_button_clicked(GtkWidget * widget)
{
    keep_going = FALSE;

    const char *tmp_dir = get_asf_tmp_dir();
    char *stop_file = MALLOC(sizeof(char) * (strlen(tmp_dir) + 24));
    sprintf(stop_file, "%s/stop.txt", tmp_dir);
    FILE * fp = fopen(stop_file, "w");
    if (fp) {
        fprintf(fp,
                "Temporary file.\n\n"
                "Flags any asf tools currently running in this directory "
                "to halt processing immediately.\n\n"
                "This file should be deleted once processing has stopped.\n");
        fclose(fp);
    }
    free(stop_file);
}
