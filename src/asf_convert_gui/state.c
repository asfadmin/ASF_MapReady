#include "asf_convert_gui.h"
#include <errno.h>

/* for now use a hard-coded file */
const char *save_name = "asf_convert_gui.sav";
const int save_major_ver = 1;
const int save_minor_ver = 0;

SIGNAL_CALLBACK void
on_save_button_clicked(GtkWidget *w, gpointer data)
{
  FILE *f;
  Settings * s;

  s = settings_get_from_gui();
  f = fopen(save_name, "wt");

  assert(s);
  if (!f)
  {
    char msg[1024];
    sprintf(msg, "Couldn't open save file: %s", strerror(errno));
    message_box(msg);
    return;
  }

  /* quick & dirty -- this should be redone */
  /* first the user settings */
  fprintf(f, "%d.%d\n\n", save_major_ver, save_minor_ver);  
  fprintf(f, "[Import]\nFormat=%d\n\n", s->input_data_format);
  fprintf(f, "[Transformations]\nType=%d\nLatLo=%f\nLatHi=%f\n\n",
	  s->data_type, s->latitude_low, s->latitude_hi);
  fprintf(f, "[Export]\nFormat=%d\nScale=%d\nLongest=%d\n\n",
	  s->output_format, s->apply_scaling, s->longest_dimension);

  /* next is the files & their statuses */
  fprintf(f, "[Files]\n");
  if (list_store)
  {
    gboolean valid;
    GtkTreeIter iter;

    valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
    while (valid)
    {
      gchar *data_file, *output_file, *status;

      gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
			 0, &data_file, 1, &output_file, 2, &status, -1);

      if (strcmp(status, "Processing...") == 0)
      {
	/* user "Saved" while processing was in progress */
	fprintf(f, "Data=%s\nOutput=%s\nStatus=-\n",
		data_file, output_file);
      }
      else
      {
	fprintf(f, "Data=%s\nOutput=%s\nStatus=%s\n",
		data_file, output_file, status);
      }

      g_free(data_file);
      g_free(output_file);
      g_free(status);

      valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
    }
  }

  fprintf(f, "[End]");
  fclose(f);

  message_box("Settings Saved.");
}

SIGNAL_CALLBACK void
on_load_button_clicked(GtkWidget *w, gpointer data)
{
  FILE *f;
  Settings s;
  int major_ver, minor_ver;
  GtkTreeIter iter;

  f = fopen(save_name, "rt");
  if (!f)
  {
    message_box("Nothing to load.");
    return;
  }

  fscanf(f, "%d.%d\n\n", &major_ver, &minor_ver);

  if (major_ver != save_major_ver || minor_ver != save_minor_ver)
  {
    char msg[64];
    sprintf(msg, "Don't know how to load version %d.%d!",
	    major_ver, minor_ver); 
    message_box(msg);
    return;
  }

  fscanf(f, "[Import]\nFormat=%d\n\n", &s.input_data_format);
  fscanf(f, "[Transformations]\nType=%d\nLatLo=%f\nLatHi=%f\n\n",
	 &s.data_type, &s.latitude_low, &s.latitude_hi);
  fscanf(f, "[Export]\nFormat=%d\nScale=%d\nLongest=%d\n\n",
	 &s.output_format, &s.apply_scaling, &s.longest_dimension);

  settings_apply_to_gui(&s);
  settings_on_execute = settings_copy(&s);

  /* files */
  gtk_list_store_clear(list_store);

  fscanf(f, "[Files]\n");
  while (!feof(f))
  {
    char line[1024];
    char *p;

    p = fgets(line, 1024, f);
    if (!p)
    {
      break;
    }
    else
    {
      if (strcmp(line, "[End]") == 0)
      {
	break;
      }
      else
      {
	char *data_file, *output_file, *status;
	char *data_file_p, *output_file_p, *status_p;
	char *newline;

	data_file = strdup(line);

	data_file_p = strchr(data_file, '=');
	if (!data_file_p)
	  continue;
	++data_file_p;

	newline = strchr(data_file_p, '\n');
	if (newline)
	  *newline = '\0';

	p = fgets(line, 1024, f);
	if (!p)
	  continue;

	output_file = strdup(line);

	output_file_p = strchr(output_file, '=');
	if (!output_file_p)
	  continue;
	++output_file_p;

	newline = strchr(output_file_p, '\n');
	if (newline)
	  *newline = '\0';
	
	p = fgets(line, 1024, f);
	if (!p)
	  continue;

	status = strdup(line);

	status_p = strchr(status, '=');
	if (!status_p)
	  continue;
	++status_p;

	newline = strchr(status_p, '\n');
	if (newline)
	  *newline = '\0';
	
	gtk_list_store_append(list_store, &iter);
	gtk_list_store_set(list_store, &iter,
			   0, data_file_p, 1, output_file_p, 2, status_p, -1);

	free(status);
	free(output_file);
	free(data_file);
      }
    }
  }

  fclose(f);
}
