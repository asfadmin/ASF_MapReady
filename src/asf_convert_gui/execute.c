
#include <unistd.h>
#include "asf_convert_gui.h"

/*
#include <fcntl.h>
#include <sys/select.h>
*/

#include <errno.h>
#include <sys/wait.h>
#include <time.h>
#include <sys/stat.h>

#include "asf.h"

static gboolean keep_going = TRUE;

static gboolean confirm_overwrite()
{
  LSL;
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
	      LSU;
	      return FALSE;

            case GTK_RESPONSE_OK:
	      LSU;
	      return TRUE;
        }
    }
    else
    {
        /* no need to confirm -- no overwrites */
      LSU;
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

    ret = system(cmd);
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
		    "*** Error! Could not run command. ***\n"
		    "Command: %s\n"
		    "Error: %s\n",
		    cmd, strerror(saved_errno));
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
    the_output = g_strdup("Error Opening Log File: Disk Full?\n");
  }

  return the_output;
}

/*
void
do_cmd_does_not_work(char *cmd)
{
  FILE *f;
  char buffer[4096];
  int fd;

  f = popen(cmd, "r");
  fd = fileno(f);

  fd_set rfds;
  struct timeval tv;
  int retval;
  
  FD_ZERO(&rfds);
  FD_SET(fd, &rfds);
  
  while (waitpid(-1, NULL, WNOHANG) == 0)
  {
    tv.tv_sec = 1;
    tv.tv_usec = 0;
    
    retval = select(fd + 1, &rfds, NULL, NULL, &tv);

    if (retval == -1)
      perror(NULL);
    else if (retval)
    {
      int count = read(fd, buffer, 4096);

      if (count == -1 && errno != EAGAIN)
      {
    perror(NULL);
    break;
      }

      buffer[count] = '\0';

      printf("%s\n", buffer);

      if (count == 0)
    break;
    }
    else
    {
      / * no data * /
      
    }
    
    while (gtk_events_pending())
      gtk_main_iteration();

  }

  pclose(f); 
}
*/

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
  LSL;
  gboolean valid;
  GtkTreeIter iter;

  assert (list_store);
    
  valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
  while (valid)
  {
    gtk_list_store_set(list_store, &iter, COL_STATUS, "-", -1);
    valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
  }
  LSU;
}

static void set_thumbnail(GtkTreeIter *iter, const gchar * file)
{
    if (use_thumbnails)
    {
	LSL;
	
	GError * err = NULL;
	GdkPixbuf * pb;

#ifdef THUMBNAILS	
	pb = gdk_pixbuf_new_from_file_at_size(file, THUMB_SIZE, THUMB_SIZE, 
					      &err);
#endif

	if (!err)
	{
	    gtk_list_store_set(list_store, iter, COL_OUTPUT_THUMBNAIL, pb, -1);
	}
	else
	{
	    g_warning("Couldn't load image '%s': %s\n", file, err->message);
	    g_error_free(err);
	}
	
	LSU;
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

static void
process_item(GtkTreeIter *iter, Settings *user_settings, gboolean skip_done)
{
  LSL;
  gchar *in_data, *out_full, *status;
  int pid;
  /* time_t s; */

  pid = getpid();
  /* s = time(NULL); */

  gtk_tree_model_get(GTK_TREE_MODEL(list_store), iter, 
		     COL_DATA_FILE, &in_data,
		     COL_OUTPUT_FILE, &out_full,
		     COL_STATUS, &status,
		     -1);
  
  if (strcmp(status, "Done") != 0 || !skip_done)
  {
    gchar *basename, *before_geocoding_basename, *output_dir,
	*out_basename, *p, *done, *err_string, *cd_dir;
    gchar convert_cmd[4096];
    gchar executable[256];
    gchar log_file[1024];
    gboolean err;

    basename = g_strdup(in_data);
    p = strrchr(basename, '.');
    if (p)
      *p = '\0';

    out_basename = g_strdup(out_full);
    p = strrchr(out_basename, '.');
    if (p)
      *p = '\0';

    output_dir = g_strdup(out_basename);
    p = strrchr(output_dir, DIR_SEPARATOR);
    if (p)
	*(p+1) = '\0';
    else
	output_dir[0] = '\0';

    if (strlen(output_dir) == 0)
    {
	cd_dir = g_strdup(".");
    }
    else
    {
	cd_dir = g_strdup(output_dir);
	p = strrchr(cd_dir, DIR_SEPARATOR);
	if (p && p != cd_dir)
	    *p = '\0';
    }

    /* Ensure we have access to the output directory */
    if (!have_access_to_dir(output_dir, &err_string))
    {
	/* We don't -- issue a message in the "Status" column. */
	gtk_list_store_set(list_store, iter, COL_STATUS, err_string, -1);

	g_free(err_string);
	g_free(basename);
	g_free(out_basename);
	g_free(output_dir);

	return;
    }

    if (settings_get_run_geocode(user_settings))
    {
	before_geocoding_basename =
	    (gchar *)g_malloc(sizeof(gchar) * (strlen(out_basename) + 10));

	sprintf(before_geocoding_basename, "%s_tmp", out_basename);
    }
    else
    {
	before_geocoding_basename = g_strdup(out_basename);
    }

    gtk_list_store_set(list_store, iter, COL_STATUS, "Processing...", -1);
    append_begin_processing_tag(in_data);

    while (gtk_events_pending())
      gtk_main_iteration();
  
    if (settings_get_run_import(user_settings))
    {
      gchar * cmd_output;
      
      gtk_list_store_set(list_store, iter, COL_STATUS, "Importing...", -1);
      g_snprintf(log_file, sizeof(log_file), "%stmpi%d.log", output_dir, pid);

      if (user_settings->input_data_format == INPUT_FORMAT_CEOS_LEVEL0 &&
	  strcasecmp(in_data, out_full) == 0)
      {
	 /* should be enough room -- we chopped the extension */
	 strcat(out_basename, "_out");
      }

      sprintf(executable, "%s/asf_import", get_asf_bin_dir());

      g_snprintf(convert_cmd, sizeof(convert_cmd), 
    "cd \"%s\"; %s %s -format %s %s -log \"%s\" \"%s\" \"%s\" 2>&1",
	 cd_dir,
	 executable,
         settings_get_data_type_arg_string(user_settings),
         settings_get_input_data_format_string(user_settings),
         settings_get_latitude_argument(user_settings),
         log_file,
         basename,
         before_geocoding_basename);

      cmd_output = do_cmd(convert_cmd, log_file);
      err = check_for_error(cmd_output);

      append_output(cmd_output);

      if (!settings_get_run_export(user_settings) &&
	  !settings_get_run_geocode(user_settings))
      {
	done = err ? "Error" : "Done";
	gtk_list_store_set(list_store, iter, COL_STATUS, done, -1);
      }

      g_free(cmd_output);
    }

    while (gtk_events_pending())
      gtk_main_iteration();

    if (!err && settings_get_run_geocode(user_settings))
    {
      gchar * cmd_output;

      gtk_list_store_set(list_store, iter, COL_STATUS, "Geocoding...", -1);

      g_snprintf(log_file, sizeof(log_file), "%stmpg%d.log", output_dir, pid);

      g_snprintf(executable, sizeof(executable), 
		 "%s/asf_geocode", get_asf_bin_dir());
    
      snprintf(convert_cmd, sizeof(convert_cmd),
           "cd \"%s\"; %s %s -log \"%s\" \"%s\" \"%s\" 2>&1",
	   cd_dir,
	   executable,
           settings_get_geocode_options(user_settings),
           log_file,
           before_geocoding_basename,
           out_basename);

      cmd_output = do_cmd(convert_cmd, log_file);
      err = check_for_error(cmd_output);

      append_output(cmd_output);
     
      g_free(cmd_output);

      /* delete temporary .img/.meta pair generated before geocoding,
         they are just eating up space ... */

      gchar * fname;
      fname = (gchar *) 
          g_malloc(sizeof(gchar) * 
              (strlen(before_geocoding_basename) + 10));

      sprintf(fname, "%s.img", before_geocoding_basename);
      remove(fname);

      sprintf(fname, "%s.meta", before_geocoding_basename);
      remove(fname);

      g_free(fname);
    }

    if (!err && settings_get_run_export(user_settings))
    {
      gchar * cmd_output;
    
      g_snprintf(log_file, sizeof(log_file), "%stmpe%d.log", output_dir, pid);
    
      gtk_list_store_set(list_store, iter, COL_STATUS, "Exporting...", -1);

      g_snprintf(executable, sizeof(executable), 
		 "%s/asf_export", get_asf_bin_dir());

      snprintf(convert_cmd, sizeof(convert_cmd),
           "cd \"%s\"; %s -format %s %s %s -log \"%s\" \"%s\" \"%s\" 2>&1",
	   cd_dir,
	   executable,
           settings_get_output_format_string(user_settings),
           settings_get_size_argument(user_settings),
           settings_get_output_bytes_argument(user_settings),
           log_file,
           out_basename,
           out_full);

      cmd_output = do_cmd(convert_cmd, log_file);
      err = check_for_error(cmd_output);

      append_output(cmd_output);

      if (use_thumbnails &&
	  settings_get_output_format_can_be_thumbnailed(user_settings))
      {
	  set_thumbnail(iter, out_full);
      }

      g_free(cmd_output);

    }

    done = err ? "Error" : "Done"; 
    gtk_list_store_set(list_store, iter, COL_STATUS, done, -1);

    g_free(basename);
    g_free(before_geocoding_basename);
    g_free(out_basename);
    g_free(output_dir);
    g_free(cd_dir);
  }

  g_free(status);
  g_free(out_full);
  g_free(in_data);

  LSU;
}

void
process_items_from_list(GList * list_of_row_refs, gboolean skip_done)
{
  LSL;

    GList * i;
    Settings * user_settings;
    GtkTreeIter iter;

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

	process_item(&iter, user_settings, skip_done);

	while (gtk_events_pending())
	    gtk_main_iteration();

	if (!keep_going)
	    append_output("Processing stopped by user.\n");

	i = g_list_next(i);
    }

    processing = FALSE;
    settings_delete(user_settings);
    show_execute_button(TRUE);

    g_list_foreach(list_of_row_refs, (GFunc)gtk_tree_row_reference_free, NULL);
    g_list_free(list_of_row_refs);

    LSU;
}

SIGNAL_CALLBACK void
on_execute_button_clicked (GtkWidget *button)
{
  LSL;

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
    }

    LSU;
}

SIGNAL_CALLBACK void
on_stop_button_clicked(GtkWidget * widget)
{
  append_output("Stopping...\n");
  keep_going = FALSE;
}
