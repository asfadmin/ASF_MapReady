#include <ctype.h>
#include <unistd.h>

#ifdef win32
#define BYTE __byte
#include "asf.h"
#include "asf_meta.h"
#undef BYTE
#include <windows.h>
#undef DIR_SEPARATOR
#endif

#include "asf_convert_gui.h"
#include <asf.h>
#include <asf_convert.h>

/*
#include <fcntl.h>
#include <sys/select.h>
*/

#include <errno.h>
#ifndef win32
#include <sys/wait.h>
#endif
#include <time.h>
#include <sys/stat.h>

#ifdef win32
#include <process.h>
#endif

#ifdef linux
#include <sys/types.h>
#include <signal.h>
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

static char *check_for_error(gchar * txt)
{
    /* kludge */
    gchar *p, *q;
    int success = FALSE;

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
                // some of the more common cases here
                if (strstr(p, "Out of Memory") != NULL)
                   return STRDUP("Error: Out of Memory");
                else if (strstr(p, "Cannot Open File") != NULL)
                   return STRDUP("Error: Cannot Open File");
                else if (strstr(p, "Read past end of file") != NULL)
                   return STRDUP("Error: Read past end of file");
                else if (strstr(p, "Error reading file") != NULL)
                   return STRDUP("Error reading file");
                else if (strstr(p, "writing file") != NULL)
                   return STRDUP("Error writing file");

                *q = '\n';

                // grab the next non-empty line
                int n = 0;
                do {
                    p = q + 1;
                    q = strchr(p, '\n');
                    if (!q)
                      q = p + strlen(p) - 1;
                    else
                      --q;
                    while (isspace(*q)) --q;
                    if (q - p > 2) {
                        char *err_string = MALLOC(sizeof(char)*64);

                        // first 50 characters of the error string, unless
                        // line ends first don't cut off in the middle of
                        // a word, though
                        strcpy(err_string, "Error: ");
                        strncat(err_string, p, 50);
                        while (isalnum(err_string[strlen(err_string)-1]))
                            err_string[strlen(err_string)-1] = '\0';
                        strcat(err_string, " ...");
                        for (n=0; n<strlen(err_string); ++n)
                            if (err_string[n] == '\n') err_string[n] = '\0';

                        return err_string;
                    }
                }
                while (*p != '*' && ++n<5); // * flags the end of the msg

                // couldn't pull out more error infomation...
                return STRDUP("Error");
            }

            if (strstr(p,"Successful completion!") != NULL)
                success = TRUE;

            *q = '\n';
        }

        p = q;
    }

    // if we found the "Successful completion!" message, return NULL (no
    // error string), otherwise we couldn't figure out what the actual
    // error was, so just return a generic "Error"
    if (success)
        return NULL;
    else
        return STRDUP("Error");
}

static void set_thumbnail(GtkTreeIter *iter, const gchar * tmp_dir,
                          const gchar *out_full, int is_PolSARpro)
{
    if (use_thumbnails)
    {
        // changed this to always do scaling, no reason not to really
        // since it is pretty quick
        int scaling_required = TRUE;

        char *basename = get_basename(out_full);
        char *thumbnail_name =
            MALLOC(sizeof(char)*(strlen(tmp_dir)+strlen(basename)+32));

        sprintf(thumbnail_name, "%s%c%s_thumb.png",
                tmp_dir, DIR_SEPARATOR, basename);

        // if output was a png image, we will be using that instead
        // of the "_thumb" file
        if (!fileExists(thumbnail_name) &&
            strcmp_case(findExt(out_full), ".png") == 0)
        {
            strcpy(thumbnail_name, out_full);
        }

        GError *err = NULL, *err_big = NULL;
        GdkPixbuf *pb, *pb_big;

        if (g_file_test(thumbnail_name, G_FILE_TEST_EXISTS))
        {
            if (scaling_required) {
                pb = gdk_pixbuf_new_from_file_at_size(thumbnail_name,
                                              THUMB_SIZE, THUMB_SIZE,
                                              &err);
                pb_big = gdk_pixbuf_new_from_file_at_size(thumbnail_name,
                                              THUMB_SIZE_BIG, THUMB_SIZE_BIG,
                                              &err_big);
            }
            else {
                pb_big = gdk_pixbuf_new_from_file(thumbnail_name, &err);
                pb = gdk_pixbuf_new_from_file_at_size(thumbnail_name,
                                              THUMB_SIZE, THUMB_SIZE, &err);
            }

            if (!err)
            {
                gtk_list_store_set(completed_list_store, iter,
                                   COMP_COL_OUTPUT_THUMBNAIL, pb, -1);

                if (!err_big)
                {
                    gtk_list_store_set(completed_list_store, iter,
                                       COMP_COL_OUTPUT_THUMBNAIL_BIG,
                                       pb_big, -1);
                }
                else
                {
                    g_warning("Error loading image '%s': %s\n",
                              thumbnail_name, err_big->message);
                    g_error_free(err_big);
                }
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
        tmp = fopen(fname, "w");
        if (!tmp)
        {
            *err_string = (gchar *) g_malloc (sizeof(gchar) * 512);
            if (strcmp(dir, ".") == 0 && errno == EACCES)
                strcpy(*err_string, "Cannot write to output directory!");
            else
                snprintf(*err_string, 512, "%s: %s", dir, strerror(errno));

            return FALSE;
        }
        fclose(tmp);
        remove_file(fname);
    }

    return TRUE;
}

static void log_summary_text(FILE *fp)
{
    const char *s = get_summary_text();
    printf("MapReady version %s\nRunning with settings:\n\n%s\n\n",
           MAPREADY_VERSION_STRING, s);
    fprintf(fp, "MapReady version %s\nRunning with settings:\n\n%s\n\n",
            MAPREADY_VERSION_STRING, s);
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
           int keep_files, char **intermediates_file)
{
    FILE *output;
    char *logFile = appendExt(cfg_file, ".log");

    gtk_list_store_set(list_store, iter, COL_STATUS, "Processing...", -1);

#ifdef win32
    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    memset(&si, 0, sizeof(si));
    memset(&pi, 0, sizeof(pi));
    si.cb = sizeof(si);

    char *cmd = MALLOC(sizeof(char)*
                       (strlen(cfg_file) + strlen(logFile) +
                        strlen(get_asf_bin_dir_win()) + 256));
    sprintf(cmd, "\"%s/asf_mapready.exe\" %s-log \"%s\" \"%s\"",
        get_asf_bin_dir_win(),
        save_dem ? "--save-dem " : "",
        logFile, cfg_file);

    fLog = fopen(logFile, "a");
    log_summary_text(fLog);
    FCLOSE(fLog);

    //printf("Running command> %s\n", cmd);
    if (!CreateProcess(NULL, cmd, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi))
    {
        DWORD dw = GetLastError();
        //printf( "CreateProcess failed (%ld)\n", dw );

        LPVOID lpMsgBuf;
        FormatMessage(
          FORMAT_MESSAGE_ALLOCATE_BUFFER |
          FORMAT_MESSAGE_FROM_SYSTEM |
          FORMAT_MESSAGE_IGNORE_INSERTS,
          NULL,
          dw,
          MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT),
              (LPTSTR)&lpMsgBuf,
          0,
          NULL);

        printf("CreateProcess() failed with error %ld: %s\n",
            dw, (char*)lpMsgBuf);
        printf("Failed command: %s\n", cmd);
    }

    char *statFile = appendExt(cfg_file, ".status");
    DWORD dwWaitResult;
    int counter = 1;

    // now wait for process to finish
    do {
        while (gtk_events_pending())
            gtk_main_iteration();

        if (++counter % 200 == 0) {
            // check status file
            char buf[256];
            FILE *fStat = fopen(statFile, "r");
            if (fStat)
            {
                fgets(buf, sizeof(buf), fStat);
                fclose(fStat);

                gtk_list_store_set(list_store, iter, COL_STATUS, buf, -1);
            }
        }

        dwWaitResult = WaitForSingleObject(pi.hProcess, 50);
    }
    while (dwWaitResult == WAIT_TIMEOUT);

    remove_file(statFile);
    free(statFile);
    // Don't do this, CreateProcess() takes it
    //free(cmd);
#else
    extern int logflag;
    extern FILE *fLog;

    pid = fork();
    if (pid == 0)
    {
        /* child */
        logflag = TRUE;
        fLog = fopen(logFile, "a");

        asfPrintStatus("Running MapReady with configuration file: %s\n",
                       cfg_file);

        log_summary_text(fLog);

        asf_convert_ext(FALSE, cfg_file, save_dem);

        FCLOSE(fLog);
        exit(EXIT_SUCCESS);
    }
    else
    {
        /* parent */
        int counter = 1;
        char *statFile = appendExt(cfg_file, ".status");
        while (waitpid(pid, NULL, WNOHANG) == 0)
        {
            while (gtk_events_pending())
                gtk_main_iteration();

            g_usleep(50);

            if (++counter % 200 == 0) {
                /* check status file */
                char buf[256];
                FILE *fStat = fopen(statFile, "r");
                if (fStat)
                {
                    if (fgets(buf, sizeof(buf), fStat))
                        fclose(fStat);
                    else
                        strcpy(buf,"");

                    gtk_list_store_set(list_store, iter, COL_STATUS, buf, -1);

                    if (strcmp(buf, "Done")==0 || strcmp(buf, "Error")==0) {
                         // kludge:
                         // Status file says "Done" but we're still here.
                         // This could happen because it *just* finished
                         // during the most recent g_usleep(), but a much more
                         // likely reason is that the waitpid() detection
                         // failed.  (I say "much more likely" because waitpid
                         // is checked 200x more often than the status file)

                         // UPDATE!
                         // We figured out why this is occurring on Linux,
                         // it is a bug in the GtkFileChooser.  Since the
                         // chooser is much nicer than the older FileSelector,
                         // we'll keep this kludge... seems to be no other
                         // side effects... hopefully...

                         // Expanded the kludge to update the status file
                         // with "Error", so that even if the process exits
                         // with an error we'll still be ok.  This only
                         // leaves the core-dump case as a loose end.

#ifdef linux
                         // On Linux (which is actually the only platform
                         // I have seen this problem), we can try to kill
                         // the zombie process.
                         kill(pid, 9);
#endif

                         // We'll break out of the loop now, and
                         // possibly leave a zombie process around, if this
                         // were to occur somewhere other than Linux
                         break;
                     }
                }
            }
        }

        remove_file(statFile);
        free(statFile);
    }
#endif

    gchar *the_output = NULL;
    output = fopen(logFile, "r");

    // see if we got a file containing a list of useful intermediate files
    *intermediates_file = appendExt(cfg_file, ".files");
    if (!fileExists(*intermediates_file)) {
      free(*intermediates_file);
      *intermediates_file = NULL;
    }

    if (!output)
    {
        the_output = (gchar *)g_malloc(512);
        sprintf(the_output, "Error Opening Log File: %s\n", strerror(errno));
    }
    else
    {
        gchar buffer[4096];
        gchar *p = fgets(buffer, sizeof(buffer), output);
        while (!feof(output))
        {
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
            p = fgets(buffer, sizeof(buffer), output);
        }
        fclose(output);

        if (!keep_files)
            remove_file(logFile);
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
    gchar *in_file, *out_full, *ancillary_file, *meta_file,
      *status, *polsarpro_aux_info, *interferogram_file,
      *coherence_file, *slave_metadata_file, *baseline_file;
    int pid, isPolSARPro = FALSE;

    pid = getpid();

    gtk_tree_model_get(GTK_TREE_MODEL(list_store), iter,
		       COL_INPUT_FILE, &in_file,
		       COL_ANCILLARY_FILE, &ancillary_file,
		       COL_METADATA_FILE, &meta_file,
		       COL_OUTPUT_FILE, &out_full,
		       COL_STATUS, &status,
		       COL_POLSARPRO_INFO, &polsarpro_aux_info,
		       COL_INTERFEROGRAM, &interferogram_file,
		       COL_COHERENCE, &coherence_file,
		       COL_SLAVE_METADATA, &slave_metadata_file,
		       COL_BASELINE, &baseline_file,
		       -1);

    int image_data_type = extract_image_data_type(polsarpro_aux_info);
    if (image_data_type >= 0 && image_data_type < 3)
      isPolSARPro = TRUE;

    if (strcmp(status, "Done") != 0 || !skip_done)
    {
        //char *in_basename = stripExt(in_file);
        char *out_basename = stripExt(out_full);
        char *out_nameonly = get_basename(out_full);
        char *output_dir = getPath(out_full);
        char *config_file, *cmd_output, *tmp_dir, *intermediates_file;
        gchar *err_string;

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
        if (strlen(output_dir) > 0) {
          sprintf(tmp_dir, "%s%c%s-%s", output_dir, DIR_SEPARATOR,
                  out_nameonly, time_stamp_dir());
        }
        else {
          sprintf(tmp_dir, "%s-%s", out_nameonly, time_stamp_dir());
        }

        create_clean_dir(tmp_dir);
        set_asf_tmp_dir(tmp_dir);

        config_file =
            settings_to_config_file(user_settings,
                                    in_file, ancillary_file, meta_file,
                                    out_full, output_dir,
                                    tmp_dir, polsarpro_aux_info,
				    interferogram_file, coherence_file,
				    slave_metadata_file, baseline_file);
        if (!config_file) {
            err_string = "Error creating configuration file.";
            gtk_list_store_set(list_store, iter, COL_STATUS, err_string, -1);

            free(out_basename);
            free(output_dir);
            free(tmp_dir);
            return;
        }

        cmd_output = do_convert(pid, iter, config_file, TRUE,
            user_settings->keep_files, &intermediates_file);
        err_string = check_for_error(cmd_output);
        if (err_string) {
            // unsuccessful
            gtk_list_store_set(list_store, iter, COL_STATUS, err_string,
                COL_LOG, cmd_output, -1);
            FREE(err_string);
        }
        else {
            // successful -- move to "completed" list
            GtkTreeIter completed_iter;
            move_to_completed_files_list(iter, &completed_iter, cmd_output,
                                         intermediates_file);
            set_thumbnail(&completed_iter, tmp_dir, out_full, isPolSARPro);
            input_data_formats_changed();
            refresh_file_names();
        }

        // for subsequent runs, save the imported dem & mask
        settings_update_dem(user_settings, output_dir);
        settings_update_mask(user_settings, output_dir);

        free(config_file);
        free(out_basename);
        free(output_dir);
        free(out_nameonly);
        free(intermediates_file);
        g_free(cmd_output);

        if (!user_settings->keep_files)
            remove_dir(tmp_dir);

        free(tmp_dir);
    }

    g_free(status);
    g_free(out_full);
    g_free(ancillary_file);
    g_free(meta_file);
    g_free(in_file);
    g_free(polsarpro_aux_info);
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
    settings_delete_dem_and_mask(user_settings);
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
        keep_going = TRUE;
        user_settings = settings_get_from_gui();
        settings_on_execute = settings_copy(user_settings);

        rows = NULL;
        valid = gtk_tree_model_get_iter_first(
            GTK_TREE_MODEL(list_store), &iter);

        if (valid) {
            hide_sections_for_execute();
        }
        else {
            message_box(" No files to process (click Browse...) ");
        }

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

void set_stop()
{
    if (!processing)
        return;

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

SIGNAL_CALLBACK void
on_stop_button_clicked(GtkWidget * widget)
{
    set_stop();
}
