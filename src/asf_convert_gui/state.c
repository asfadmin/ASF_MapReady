#include "asf_convert_gui.h"
#include <errno.h>

/* for now use a hard-coded file */
const gchar *save_name = "asf_convert_gui.sav";
const int save_major_ver = 1;
const int save_minor_ver = 2;

static void readline(FILE * f, gchar * buffer, size_t n)
{
  gchar * p;
  gchar * newline;

  p = fgets(buffer, n, f);

  if (!p)
  {
    strcpy(buffer, "");
  }
  else
  {
    newline = strrchr(buffer, '\n');
    if (newline)
      *newline = '\0';
  }
}
                        
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
    gchar msg[1024];
    g_snprintf(msg, sizeof(msg),
           "Couldn't open save file: %s", strerror(errno));
    message_box(msg);
    return;
  }

  /* quick & dirty -- this should be redone */
  /* first the user settings */
  fprintf(f, "%d.%d\n\n", save_major_ver, save_minor_ver);  
  fprintf(f, "[Import]\nFormat=%d\n\n", s->input_data_format);
  fprintf(f, "[Transformations]\nType=%d\nUseLat=%d\nLatLo=%f\nLatHi=%f\n\n",
      s->data_type, s->latitude_checked, s->latitude_low, s->latitude_hi);
  fprintf(f, "[Export]\nFormat=%d\nScale=%d\nLongest=%d\n"
          "OutputBytes=%d\nScalingMethod=%d\n\n",
          s->output_format, s->apply_scaling, s->longest_dimension,
          s->output_bytes, s->scaling_method);

  fprintf(f, "[NamingScheme]\nPrefix=%s\nSuffix=%s\nScheme=%s\n\n",
          current_naming_scheme->prefix,
          current_naming_scheme->suffix,
          current_naming_scheme->scheme);

  fprintf(f, "[OutputDirectory]\nDir=%s\n\n",
          output_directory);

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
  settings_delete(s);
  
  message_box("Settings Saved.");
}

static void read_ver_1_0(FILE *f)
{
  Settings s;
  GtkTreeIter iter;

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
    gchar line[1024];
    gchar *p;

    p = fgets(line, sizeof(line), f);
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
    gchar *data_file, *output_file, *status;
    gchar *data_file_p, *output_file_p, *status_p;
    gchar *newline;

    data_file = g_strdup(line);

    data_file_p = strchr(data_file, '=');
    if (!data_file_p)
      continue;
    ++data_file_p;

    newline = strchr(data_file_p, '\n');
    if (newline)
      *newline = '\0';

    p = fgets(line, sizeof(line), f);
    if (!p)
      continue;

    output_file = g_strdup(line);

    output_file_p = strchr(output_file, '=');
    if (!output_file_p)
      continue;
    ++output_file_p;

    newline = strchr(output_file_p, '\n');
    if (newline)
      *newline = '\0';
    
    p = fgets(line, sizeof(line), f);
    if (!p)
      continue;

    status = g_strdup(line);

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

    g_free(status);
    g_free(output_file);
    g_free(data_file);
      }
    }
  }

  if (output_directory)
      g_free(output_directory);
  output_directory = NULL;

  naming_scheme_delete(current_naming_scheme);
  current_naming_scheme = naming_scheme_default();
}

static void read_ver_1_1(FILE *f)
{
  Settings s;
  GtkTreeIter iter;
  gchar line[1024];
  gchar *prefix, *suffix, *scheme;

  fscanf(f, "[Import]\nFormat=%d\n\n", &s.input_data_format);
  fscanf(f, "[Transformations]\nType=%d\nLatLo=%f\nLatHi=%f\n\n",
     &s.data_type, &s.latitude_low, &s.latitude_hi);
  fscanf(f, "[Export]\nFormat=%d\nScale=%d\nLongest=%d\n"
          "OutputBytes=%d\nScalingMethod=%d\n\n",
        &s.output_format, &s.apply_scaling, &s.longest_dimension,
        &s.output_bytes, &s.scaling_method);

  settings_apply_to_gui(&s);
  settings_on_execute = settings_copy(&s);

  /* read naming scheme */
  fscanf(f, "[NamingScheme]\nPrefix=");
  readline(f, line, sizeof(line));
  prefix = g_strdup(line);
    
  fscanf(f, "Suffix=");
  readline(f, line, sizeof(line));
  suffix = g_strdup(line);

  fscanf(f, "Scheme=");
  readline(f, line, sizeof(line));
  scheme = g_strdup(line);

  if (current_naming_scheme)
      naming_scheme_delete(current_naming_scheme);

  current_naming_scheme = naming_scheme_new(prefix, suffix, scheme);

  g_free(prefix);
  g_free(suffix);
  g_free(scheme);

  /* output directory */
  if (output_directory)
      g_free(output_directory);

  fscanf(f, "\n[OutputDirectory]\nDir=");
  readline(f, line, sizeof(line));
  output_directory = g_strdup(line);

  fscanf(f, "\n");
            
  /* files */
  gtk_list_store_clear(list_store);

  fscanf(f, "[Files]\n");
  while (!feof(f))
  {
    readline(f, line, sizeof(line));
    if (strlen(line) > 0)
    {
      if (strcmp(line, "[End]") == 0)
      {
        break;
      }
      else
      {
        gchar *data_file, *output_file, *status;
        gchar *data_file_p, *output_file_p, *status_p;

        data_file = g_strdup(line);

        data_file_p = strchr(data_file, '=');
        if (!data_file_p)
            continue;
        ++data_file_p;

        readline(f, line, sizeof(line));
        output_file = g_strdup(line);

        output_file_p = strchr(output_file, '=');
        if (!output_file_p)
            continue;
        ++output_file_p;

        readline(f, line, sizeof(line));
        status = g_strdup(line);

        status_p = strchr(status, '=');
        if (!status_p)
            continue;
        ++status_p;

        gtk_list_store_append(list_store, &iter);
        gtk_list_store_set(list_store, &iter,
               0, data_file_p, 1, output_file_p, 2, status_p, -1);

        g_free(status);
        g_free(output_file);
        g_free(data_file);
      }
    }
  }
}

static void read_ver_1_2(FILE *f)
{
  Settings s;
  GtkTreeIter iter;
  gchar line[1024];
  gchar *prefix, *suffix, *scheme;

  fscanf(f, "[Import]\nFormat=%d\n\n", &s.input_data_format);
  fscanf(f, "[Transformations]\nType=%d\nUseLat=%d\nLatLo=%f\nLatHi=%f\n\n",
     &s.data_type, &s.latitude_checked, &s.latitude_low, &s.latitude_hi);
  fscanf(f, "[Export]\nFormat=%d\nScale=%d\nLongest=%d\n"
          "OutputBytes=%d\nScalingMethod=%d\n\n",
        &s.output_format, &s.apply_scaling, &s.longest_dimension,
        &s.output_bytes, &s.scaling_method);

  settings_apply_to_gui(&s);
  settings_on_execute = settings_copy(&s);

  /* read naming scheme */
  fscanf(f, "[NamingScheme]\nPrefix=");
  readline(f, line, sizeof(line));
  prefix = g_strdup(line);
    
  fscanf(f, "Suffix=");
  readline(f, line, sizeof(line));
  suffix = g_strdup(line);

  fscanf(f, "Scheme=");
  readline(f, line, sizeof(line));
  scheme = g_strdup(line);

  if (current_naming_scheme)
      naming_scheme_delete(current_naming_scheme);

  current_naming_scheme = naming_scheme_new(prefix, suffix, scheme);

  g_free(prefix);
  g_free(suffix);
  g_free(scheme);

  /* output directory */
  if (output_directory)
      g_free(output_directory);

  fscanf(f, "\n[OutputDirectory]\nDir=");
  readline(f, line, sizeof(line));
  output_directory = g_strdup(line);

  fscanf(f, "\n");
            
  /* files */
  gtk_list_store_clear(list_store);

  fscanf(f, "[Files]\n");
  while (!feof(f))
  {
    readline(f, line, sizeof(line));
    if (strlen(line) > 0)
    {
      if (strcmp(line, "[End]") == 0)
      {
        break;
      }
      else
      {
        gchar *data_file, *output_file, *status;
        gchar *data_file_p, *output_file_p, *status_p;

        data_file = g_strdup(line);

        data_file_p = strchr(data_file, '=');
        if (!data_file_p)
            continue;
        ++data_file_p;

        readline(f, line, sizeof(line));
        output_file = g_strdup(line);

        output_file_p = strchr(output_file, '=');
        if (!output_file_p)
            continue;
        ++output_file_p;

        readline(f, line, sizeof(line));
        status = g_strdup(line);

        status_p = strchr(status, '=');
        if (!status_p)
            continue;
        ++status_p;

        gtk_list_store_append(list_store, &iter);
        gtk_list_store_set(list_store, &iter,
               0, data_file_p, 1, output_file_p, 2, status_p, -1);

        g_free(status);
        g_free(output_file);
        g_free(data_file);
      }
    }
  }
}

SIGNAL_CALLBACK void
on_load_button_clicked(GtkWidget *w, gpointer data)
{
    FILE *f;
    int major_ver, minor_ver;

    f = fopen(save_name, "rt");
    if (!f)
    {
        message_box("Nothing to load.");
        return;
    }

    fscanf(f, "%d.%d\n\n", &major_ver, &minor_ver);

    if (major_ver == 1 && minor_ver == 0)
    {
        read_ver_1_0(f);
    }
    else if (major_ver == 1 && minor_ver == 1)
    {
        read_ver_1_1(f);
    }
    else if (major_ver == 1 && minor_ver == 2)
    {
        read_ver_1_2(f);
    }
    else
    {
        gchar msg[64];
        g_snprintf(msg, sizeof(msg),
                   "Don't know how to load version %d.%d!",
                   major_ver, minor_ver);

        message_box(msg);
        return;
    }

    fclose(f);
}
