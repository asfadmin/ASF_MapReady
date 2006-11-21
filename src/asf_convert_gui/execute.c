
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
            glade_xml_get_widget(glade_xml, "dialog_confirm_overwrite");

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
                            fprintf(output, "Unknown Error trying to run command:\n%s\n",
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

void
append_output(const gchar * txt)
{
    GtkWidget * textview_output;
    GtkTextBuffer * text_buffer;
    GtkTextIter end;

    textview_output = glade_xml_get_widget(glade_xml, "textview_output");
    text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview_output));
    gtk_text_buffer_get_end_iter(text_buffer, &end);
    gtk_text_buffer_insert(text_buffer, &end, txt, -1);

    /* taking this out... annoying to have the window scrolling all the time
    gtk_text_buffer_get_end_iter(text_buffer, &end);
    gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(textview_output),
    &end, 0, TRUE, 0.0, 1.0);
    */
}

static void
append_begin_processing_tag(const gchar * input_filename)
{
    static GtkTextTag * tt = NULL;

    GtkWidget * textview_output;
    GtkTextBuffer * text_buffer;
    GtkTextMark * mark;
    GtkTextIter end, line_begin;
    gchar * txt;
    const gchar * tag = "Processing Input File: ";

    textview_output = glade_xml_get_widget(glade_xml, "textview_output");
    text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview_output));
    gtk_text_buffer_get_end_iter(text_buffer, &end);

    /* mark the location of this file -- remove any existing mark */
    mark = gtk_text_buffer_get_mark(text_buffer, input_filename);
    if (mark)
        gtk_text_buffer_delete_mark(text_buffer, mark);

    mark = gtk_text_buffer_create_mark(text_buffer, input_filename,
        &end, TRUE);

    txt = (gchar *) g_malloc( sizeof(gchar) *
        (strlen(input_filename) + strlen(tag) + 2) );

    sprintf(txt, "%s%s\n", tag, input_filename);
    gtk_text_buffer_insert(text_buffer, &end, txt, -1);

    line_begin = end;
    gtk_text_iter_backward_line(&line_begin);

    if (!tt)
    {
        /*
        tt = gtk_text_buffer_create_tag(text_buffer, "blue_foreground",
        "foreground", "blue", NULL);
        */

        tt = gtk_text_buffer_create_tag(text_buffer, "bold",
            "weight", PANGO_WEIGHT_BOLD, 
            "foreground", "blue",
            NULL);
    }

    gtk_text_buffer_apply_tag(text_buffer, tt, &line_begin, &end);

    g_free(txt);
}

void
invalidate_progress()
{
    gboolean valid;
    GtkTreeIter iter;

    assert (list_store);

    valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
    while (valid)
    {
        gtk_list_store_set(list_store, &iter, COL_STATUS, "-", -1);
        valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
    }
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
                gtk_list_store_set(list_store, iter, COL_OUTPUT_THUMBNAIL,
                                   pb, -1);
            }
            else
            {
                g_warning("Error loading image '%s': %s\n",
                          thumbnail_name, err->message);
                g_error_free(err);
	   }
	}
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
do_convert(int pid, GtkTreeIter *iter, char *cfg_file, int save_dem)
{
    extern int logflag;
    extern FILE *fLog;

    FILE *output;
    char logFile[256];

    snprintf(logFile, sizeof(logFile), "tmp%d.log", pid);

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
        unlink(logFile);
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

static char *
generate_input_basename(const char *in_data)
{
    int prepen_len = has_prepension(in_data);

    // Two conventions we must be able to handle.  The first is
    // extension-based, the second prepension-based.

    // For an extension-based basename scheme, we just need to strip
    // off the extension -- output files will just use a new extension.

    // For prepension-based schemes, we need to strip off the path, then
    // strip off the prepension, then add back on the path info again.
    // We don't do anything with any extensions (as there aren't really
    // any extensions to worry about).
   
    if (prepen_len > 0) {
        gchar *path = g_path_get_dirname(in_data);
        char *basename = get_basename(in_data);
        char *ret = MALLOC(sizeof(char)*(strlen(path)+strlen(basename)+1));
        sprintf(ret, "%s%c%s", path, DIR_SEPARATOR, basename+prepen_len);
        g_free(path);
        free(basename);
        return ret;
    } else {
        return stripExt(in_data);
    }
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
        char *in_basename = generate_input_basename(in_data);
	char *out_basename = stripExt(out_full);
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
            free(in_basename);
            free(out_basename);
            free(output_dir);

            return;
        }

        tmp_dir = MALLOC(sizeof(char)*(strlen(output_dir)+32));
        sprintf(tmp_dir, "%s/convert-%s", output_dir, time_stamp_dir());

        create_clean_dir(tmp_dir);
	set_asf_tmp_dir(tmp_dir);

        settings_update_dem(user_settings, output_dir, is_first);
        settings_update_mask(user_settings, output_dir, is_first);

	config_file =
	  settings_to_config_file(user_settings, in_basename, out_full,
				  output_dir, tmp_dir);
        if (!config_file) {
            message_box("Error creating configuration file.\n");
            return;
        }

	append_begin_processing_tag(in_data);
	cmd_output = do_convert(pid, iter, config_file, TRUE);
	err = check_for_error(cmd_output);
	append_output(cmd_output);

	free(config_file);
	free(out_basename);
	free(output_dir);
	free(in_basename);
	g_free(cmd_output);
	
	if (use_thumbnails)
	{
            set_thumbnail(iter, tmp_dir, out_full);
	}	
	    
	char *done = err ? "Error" : "Done"; 
	gtk_list_store_set(list_store, iter, COL_STATUS, done, -1);

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

        if (!keep_going)
            append_output("Processing stopped by user.\n");

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

        if (settings_on_execute &&
            !settings_equal(user_settings, settings_on_execute))
        {
            /* settings have changed since last time clicked execute,
            or loaded settings from a file - must clear progress so far */
            invalidate_progress();
        }

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
    append_output("Stopping...\n");
    keep_going = FALSE;
}
