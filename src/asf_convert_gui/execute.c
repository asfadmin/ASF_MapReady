
#include <unistd.h>
#include "asf_convert_gui.h"

/*
#include <fcntl.h>
#include <sys/select.h>
*/

#include <errno.h>
#include <sys/wait.h>
#include <time.h>

char *
do_cmd(char *cmd, char *log_file_name)
{
  char *the_output;
  FILE *output;

  int pid = fork();

  if (pid == 0)
  {
    system(cmd);
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
    the_output = (char *)malloc(256);
    sprintf(the_output, "Error Opening Log File: %s", strerror(errno));
  }
  else
  {
    while (!feof(output))
    {
      char buffer[4096];
      char *p = fgets(buffer, 4096, output);
      if (p)
      {
	if (the_output)
        {
	  the_output = (char *)realloc(the_output, 
				    strlen(the_output) + strlen(buffer) + 1);

	  strcat(the_output, buffer);
	}
	else
	{
	  the_output = (char *)malloc(strlen(buffer) + 1);
	  strcpy(the_output, buffer);
	}
      }
    }
    fclose(output);
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

void
append_output(char * txt)
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

void
process_item(GtkTreeIter *iter,
	     Settings *user_settings)
{
  gchar *in_data, *in_meta, *out_full, *basename, *p;
  gchar convert_cmd[4096];
  char log_file[128];
  int pid;
  time_t s;

  pid = getpid();
  s = time(NULL);

  gtk_tree_model_get(GTK_TREE_MODEL(list_store), iter, 
		     0, &in_data, 1, &out_full, -1);
  
  in_meta = meta_file_name(in_data);

  basename = strdup(in_data);
  p = strrchr(basename, '.');
  if (p)
    *p = '\0';
  
  gtk_list_store_set(list_store, iter, 2, "Processing...", -1);
  
  while (gtk_events_pending())
    gtk_main_iteration();
  
  if (settings_get_run_import(user_settings))
  {
    char * cmd_output;

    sprintf(log_file, "tmp_%d_%ld_import.log", pid, s);

    snprintf(convert_cmd, 4096, 
	     "asf_import -%s -format %s %s -log \"%s\" \"%s\" \"%s\" \"%s\"",
	     settings_get_data_type_string(user_settings),
	     settings_get_input_data_format_string(user_settings),
	     settings_get_latitude_argument(user_settings),
	     log_file,
	     in_data,
	     in_meta,
	     basename);
    
    cmd_output = do_cmd(convert_cmd, log_file);

    append_output(cmd_output);

    char * out_name_full = (char *)malloc(strlen(basename) + 10);
    sprintf(out_name_full, "%s.img", basename);
    
    if (!settings_get_run_export(user_settings))
    {
      gtk_list_store_set(list_store, iter, 1, out_name_full, -1);
      gtk_list_store_set(list_store, iter, 2, "Done", -1);
    }

    free(out_name_full);
    free(cmd_output);
  }

  while (gtk_events_pending())
    gtk_main_iteration();

  if (settings_get_run_export(user_settings))
  {
    char * cmd_output;
    
    sprintf(log_file, "tmp_%d_%ld_export.log", pid, s);
    
    snprintf(convert_cmd, 4096,
             "asf_export -format %s %s -log \"%s\" \"%s\" \"%s\"",
	     settings_get_output_format_string(user_settings),
	     settings_get_size_argument(user_settings),
	     log_file,
	     basename,
	     out_full);
    
    cmd_output = do_cmd(convert_cmd, log_file);
    
    gtk_list_store_set(list_store, iter, 2, "Done", -1);
    
    free(cmd_output);
  }

  free(basename);
  free(in_meta);
}

SIGNAL_CALLBACK void
on_execute_button_clicked (GtkWidget *button)
{
  GtkTreeIter iter;
  gboolean valid;
  Settings * user_settings;

  /* gui should prevent this from happening */
  if (processing)
    return;

  user_settings = settings_get_from_gui();

  show_execute_button(FALSE);
  valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
  keep_going = TRUE;  
  processing = TRUE;

  while (valid && keep_going)
  {
    process_item(&iter, user_settings);

    while (gtk_events_pending())
      gtk_main_iteration();

    if (!keep_going)
    {
      append_output("Processing stopped by user.");
      /*
      gtk_list_store_set(list_store, &iter,
			 2, "Processing stopped by user.", -1);
      */
    }

    valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
  }

  processing = FALSE;
  free(user_settings);
  show_execute_button(TRUE);
}

SIGNAL_CALLBACK void
on_stop_button_clicked(GtkWidget * widget)
{
  append_output("Stopping...\n");
  keep_going = FALSE;
}
