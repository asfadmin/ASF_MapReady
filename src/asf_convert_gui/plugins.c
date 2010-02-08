#include "asf_convert_gui.h"
#include <ctype.h>

typedef struct {
    int type;
    int optional;
    char format[32];
    char description[32];
} Arg;

#define MAX_ARGS 4
#define MAX_COMMENT_LEN 400
#define MAX_NAME_LEN 32

typedef struct {
    char name[MAX_NAME_LEN];
    char command[1024];
    char comment[MAX_COMMENT_LEN];
    int num_args;
    Arg args[MAX_ARGS];
} ExternalCommand;

int num_external;
ExternalCommand commands[10];

int is_valid_external(int n, int line_num)
{
  int ok = TRUE;
  if (n>=0) { 
    if (strlen(commands[n].name)==0) {
      asfPrintWarning("Name field not found for plugin ending on line %d.\n",
                      line_num-1);
      ok=FALSE;
    }
    else if (strlen(commands[n].command)==0) {
      asfPrintWarning("Command field not found for plugin ending on line %d.\n",
                      line_num-1);
      ok=FALSE;
    }
    else if (strstr(commands[n].command, "{Input}")==NULL) {
      asfPrintWarning("Command field does not specify {Input} placeholder "
                      "for the input file,\nfor plugin ending on line %d.\n",
                      line_num-1);
      ok=FALSE;
    }
    else if (strstr(commands[n].command, "{Output}")==NULL) {
      asfPrintWarning("Command field does not specify {Output} placeholder "
                      "for the output file,\nfor plugin ending on line %d.\n",
                      line_num-1);
      ok=FALSE;
    }
    else {
      int i;
      for (i=0; i<MAX_ARGS; ++i) {
        char key[4];
        sprintf(key, "$P%d", i+1);
        if (commands[n].args[i].type==0 &&
            strstr(commands[n].command, key)!=NULL)
        {
          asfPrintWarning("%s not specified for command ending on "
                          "line %d.\n", key, line_num-1);
          ok=FALSE;
        }
      }
    }
  }
  if (!ok) {
    if (strlen(commands[n].name)>0)
      asfPrintWarning("*** Plugin '%s' was not added!\n", commands[n].name);
    else
      asfPrintWarning("*** Plugin ending on line %d not added!\n", line_num);
  }
  return ok;
}

void load_external_commands()
{
  // read the "plugins.cfg" file
  int i,j;
  FILE *pf = fopen_share_file("plugins.cfg", "r");
  if (pf) {
    char buf[1025];
    int n=-1; // this is the external tool number we're parsing
    int line_num=1;

    asfPrintStatus("Parsing plugins.cfg...\n");
    while (fgets(buf, 1024, pf) != NULL) {

      if (buf[strlen(buf)-1] != '\n') {
        // did not read the entire line... we will ignore the rest of the line
        char eat[64];
        while (fgets(eat, 63, pf) != NULL)
          if (eat[strlen(eat)-1]=='\n') break;
      }

      char *line = trim_spaces(buf);

      // skip comment lines, blank lines
      if (strlen(line)==0 || line[0]=='#') {
        ++line_num;
        continue;
      }

      // divide the line into before & after the '='
      char *key, *val;
      split2(line, '=', &key, &val);

      // "Name" signals the end of the external definition -- in that case,
      // move to the next element of the array before starting parsing of
      // this new tool definition.  A special case is the first "Name", where
      // we just want the code that clears out the next command entry spot
      if (strncmp_case(key, "Name", 4) == 0) {
        if (is_valid_external(n, line_num)) {
          // move to next external command
          ++n;
        }

        // clear out this command entry.  if the definition was ok, we are
        // clearing out the next entry (which is already empty), if we had
        // an error we are clearing out the erroneous data from the current
        // entry, to be overwritten by the next entry in the file
        commands[n].num_args=0;
        for (i=0; i<MAX_ARGS; ++i)
          commands[n].args[i].type=0;
        strcpy(commands[n].comment, "");
        strcpy(commands[n].command, "");
        strcpy(commands[n].name, "");
      }

      // n should have been incremented from -1 by now
      if (n<0) {
        asfPrintWarning("\"Name\" is not first specified field!\n");
        n=0;
      }

      // Parsing parameters for the current external tool
      if (strcmp_case(key, "Name")==0) {
        if (strlen(val)>=MAX_NAME_LEN) {
          asfPrintWarning("Plugin name:  %s\nis too long, truncated.\n"
                          "Maximum %d characters.  (Line %d)\n",
                          val, MAX_NAME_LEN, line_num);
          val[MAX_NAME_LEN-1]='\0';
        }
        strcpy(commands[n].name, val);
      }
      else if (strcmp_case(key, "Command")==0) {
        if (strlen(commands[n].command)>0)
          asfPrintWarning("Multiple command entries found! (line %d).\n",
                          line_num);
        if (strlen(val)>=1024) {
          asfPrintWarning("Plugin command:\n  %s\ntoo long, truncated.\n"
                          "Maximum 1024 characters. (Line %d)\n",
                          val, line_num);
          val[1023]='\0';
        }
        strcpy(commands[n].command, val);
      }
      else if (strcmp_case(key, "Comment")==0) {
        if (strlen(commands[n].comment)>0)
          asfPrintWarning("Multiple comment entries found! (line %d).\n",
                          line_num);
        if (strlen(val)>=MAX_COMMENT_LEN) {
          asfPrintWarning("Plugin comment:\n  %s\ntoo long, truncated.\n"
                          "Maximum %d characters.  (Line %d)\n",
                          val, MAX_COMMENT_LEN, line_num);
          val[MAX_COMMENT_LEN-1]='\0';
        }
        strcpy(commands[n].comment, val);
      }
      else if (key[0]=='P' && strlen(key)==2 && isdigit(key[1])) {
        char **cols;
        int num, type, optional;
        char format[32], description[32];
        split_into_array(val, ',', &num, &cols);
        if (num != 4) {
          asfPrintWarning("Did not find 4 columns on line %d.\n", line_num);
        }
        else {
          // parsing the four columns:
          // Column #1: parameter type
          type=0;
          if (strcmp_case(cols[0], "double")==0)
            type=1;
          else if (strcmp_case(cols[0], "int")==0)
            type=2;
          else if (strcmp_case(cols[0], "string")==0)
            type=3;
          else
            asfPrintWarning("Invalid type entry on line %d: %s\n",
                            line_num, cols[0]);

          // Column #2: parameter optional/required flag
          optional=1;
          if (strcmp_case(cols[1], "optional")==0)
            optional=1;
          else if (strcmp_case(cols[1], "required")==0)
            optional=0;
          else
            asfPrintWarning("Invalid required/optional flag on line %d: %s\n",
                            line_num, cols[1]);

          // Column #3: format
          if (strlen(cols[2])>32)
            asfPrintWarning("Format specification on line %d too long, "
                            "truncated to 32 characters.\n", line_num);
          strncpy_safe(format, cols[2], 30);

          // Column #4: description
          if (strlen(cols[3])>32)
            asfPrintWarning("Description on line %d too long, "
                            "truncated to 32 characters.\n", line_num);
          strncpy_safe(description, cols[3], 30);

          // Checking that this code is in the command (if the command
          // has been given yet)
          int which = key[1] - '1';
          char code[4];
          sprintf(code, "$P%d", which+1);

          if (strstr(commands[n].command, code)==NULL) {
            asfPrintWarning("%s specified on line %d is not in the "
                            "command string (no %s).\n",
                            key, line_num, code);
          }
          else if (which >= MAX_ARGS) {
            asfPrintWarning("Currently, the maximum number of args is %d, "
                            "arg %d on line %d is ignored.\n",
                            MAX_ARGS, which+1, line_num);
          }
          else {
            if (commands[n].args[which].type != 0)
              asfPrintWarning("Multiple %s entries found! (line %d).\n",
                              key, line_num);
            commands[n].args[which].type = type;
            commands[n].args[which].optional = optional;
            strcpy(commands[n].args[which].format, format);
            strcpy(commands[n].args[which].description, description);
            if (which>=commands[n].num_args)
              commands[n].num_args = which+1;
          }
        }
        free_char_array(&cols, num);
      }
      else {
        asfPrintWarning("Invalid key found on line %d: %s\n", line_num, key);
      }

      ++line_num;
    }
    fclose(pf);

    // no "Name" to signal the end of the last entry, so move ahead manually
    if (is_valid_external(n, line_num))
      ++n;
    num_external = n;
    asfPrintStatus("Found %d plugin%s.\n",
                   num_external, num_external==1?"":"s");
  }
  else {
    num_external = 0;
    asfPrintStatus("No plugins.\n");
  }

  // debug check
  //{
  //  for (i=0; i<num_external; ++i) {
  //    printf("\n----External Command #%d----\n", i);
  //    printf("  Name        : %s\n", commands[i].name);
  //    printf("  Command-line: %s\n", commands[i].command);
  //    printf("  Comment     : %s\n", commands[i].comment);
  //    printf("  Number Args : %d\n", commands[i].num_args);
  //    for (j=0; j<MAX_ARGS; ++j) {
  //      if (commands[i].args[j].type>0) {
  //        printf("  P%d Type     : %s\n", j+1,
  //               commands[i].args[j].type==1 ? "double" :
  //               commands[i].args[j].type==2 ? "int" :
  //               commands[i].args[j].type==3 ? "string" :
  //               "error");
  //        printf("  P%d Optional : %s\n", j+1,
  //               commands[i].args[j].optional ? "Yes" : "No");
  //        printf("  P%d Format   : %s\n", j+1,
  //               commands[i].args[j].format);
  //        printf("  P%d Descrip  : %s\n", j+1,
  //               commands[i].args[j].description);
  //      }
  //    }
  //    printf("----END----\n");
  //  }
  //}

  // now populate the menu
  GtkWidget *menu = gtk_menu_new();
  GtkWidget *item;

  item = gtk_menu_item_new_with_label("None");
  gtk_menu_append(GTK_MENU(menu), item);
  gtk_widget_show(item);

  if (num_external > 0) {
    item = gtk_separator_menu_item_new();
    gtk_menu_append(GTK_MENU(menu), item);
    gtk_widget_show(item);

    int i;
    for (i=0; i<num_external; ++i) {
      item = gtk_menu_item_new_with_label(commands[i].name);
      gtk_menu_append(GTK_MENU(menu), item);
      gtk_widget_show(item);
    }
  }

  GtkWidget *external_optionmenu = get_widget_checked("external_optionmenu");
  gtk_option_menu_set_menu(GTK_OPTION_MENU(external_optionmenu), menu);
  gtk_option_menu_set_history(GTK_OPTION_MENU(external_optionmenu), 0);
  gtk_widget_show(menu);
  gtk_widget_show(external_optionmenu);

  // now create widgets for all the parameters -- all hidden at first
  GtkWidget *vbox = get_widget_checked("vbox_external_params");

  // these loops go from the top down, because of the way the pack_end()
  // gtk function works -- things get added in reverse order
  for (i=num_external-1; i>=0; --i) {
    for (j=MAX_ARGS-1; j>=0; --j) {
      if (commands[i].args[j].type>0) {

        // creating the label
        int len = strlen(commands[i].args[j].description);
        char *txt = MALLOC(sizeof(char)*(5+len));
        sprintf(txt, "%s: ", commands[i].args[j].description);
        GtkWidget *label = gtk_label_new("");
        const char *format = "%s";
        if (!commands[i].args[j].optional) // required params will be bold
          format = "<span weight=\"bold\">%s</span>";
        char *markup = g_markup_printf_escaped(format,txt);
        gtk_label_set_markup(GTK_LABEL(label), markup);
        g_free(markup);
        //gtk_widget_set_size_request(label, 140, -1);
        gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
        gtk_widget_show(label);

        // creating the textbox
        GtkWidget *entry = gtk_entry_new();
        char code[32];
        snprintf(code, sizeof(code), "entry_%d_%d", i, j);
        gtk_widget_set_name(entry, code);
        int width = -1;
        switch (commands[i].args[j].type) {
          case 1: width = 100; break; // double
          case 2: width = 80;  break; // int
          case 3: width = 200; break; // string
        }
        gtk_widget_set_size_request(entry, width, -1);
        gtk_widget_show(entry);

        // now plop into an hbox
        GtkWidget *hbox = gtk_hbox_new(FALSE, 1);
        gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
        gtk_box_pack_end(GTK_BOX(hbox), entry, FALSE, FALSE, 2);

	int *ip = MALLOC(sizeof(int));
	*ip = i;
	int *jp = MALLOC(sizeof(int));
	*jp = j;

        g_object_set_data(G_OBJECT(hbox), "tool", (gpointer)ip);
        g_object_set_data(G_OBJECT(hbox), "arg", (gpointer)jp);
        gtk_widget_hide(hbox);
        //gtk_widget_show(hbox);

        // and pack into the vbox of all the params
        gtk_box_pack_end(GTK_BOX(vbox), hbox, FALSE, FALSE, 2);
      }
    }
  }
}

void hide_show_fn(GtkWidget *hbox, gpointer data)
{
  int which = *((int*)data);

  // which is the item selected -- subtract 2 to eliminate "none" and "----"
  // the index is the tool ID
  which -= 2;

  // if the tool ID matches that of the parameter specified in this hbox
  // for "None", which will be -2, which won't match anything
  if (which == *(int*)g_object_get_data(G_OBJECT(hbox), "tool"))
    gtk_widget_show(hbox);
  else
    gtk_widget_hide(hbox);
}

void external_settings_changed()
{
  // first, disable the tab if the "external" option is not set
  // on the general tab.
  int external_is_checked = get_checked("external_checkbutton");

  GtkWidget *external_tab_label = get_widget_checked("external_tab_label");
  GtkWidget *vbox_external = get_widget_checked("vbox_external");
  
  gtk_widget_set_sensitive(external_tab_label, external_is_checked);
  gtk_widget_set_sensitive(vbox_external, external_is_checked);

  if (external_is_checked) {
    // now, hide/show all of the parameters that are appropriate for the
    // selected tool.  See "hide_show_fn" above
    GtkWidget *external_optionmenu =
      get_widget_checked("external_optionmenu");
    int which = gtk_option_menu_get_history(
      GTK_OPTION_MENU(external_optionmenu));
    
    GtkWidget *vbox = get_widget_checked("vbox_external_params");

    // the children of this vbox are all hboxes, containing the parameters
    int *ip = MALLOC(sizeof(int));
    *ip = which;
    gtk_container_foreach(GTK_CONTAINER(vbox), hide_show_fn, (gpointer)ip);
    FREE(ip);

    // show the "Comment" for the selected plugin.
    GtkWidget *lbl = get_widget_checked("external_comment_label");
    if (which >= 2) {
      gtk_label_set_text(GTK_LABEL(lbl), commands[which-2].comment);
    }
    else {
      gtk_label_set_text(GTK_LABEL(lbl), "");
    }
  }
}

SIGNAL_CALLBACK void on_external_optionmenu_changed(GtkWidget *widget)
{
  external_settings_changed();  
}

static char cmd_buf[512];

GtkWidget *find_entry(GtkWidget *hbox, const char *name)
{
  GList *list = gtk_container_get_children(GTK_CONTAINER(hbox));
  GList *curr = list;

  GtkWidget *entry = NULL;

  do {
    if (strcmp(name, gtk_widget_get_name(GTK_WIDGET(curr->data)))==0) {
      entry = GTK_WIDGET(curr->data);
      break;
    }
    curr = curr->next;
  }
  while (curr != NULL);

  return entry;
}

// this is the callback function that replaces each parameters placeholder
// with the user-entered parameter value, using the format specifier from
// the config file.  this is called once per parameter, each time replacing
// just one of the placeholders
void collect_args_fn(GtkWidget *hbox, gpointer data)
{
  // this is the menu index -- we must -2 because of "None" and separator
  // in order to get the external tool ID (index into the commands array)
  int which = *(int*)data - 2;

  if (which == *(int*)g_object_get_data(G_OBJECT(hbox), "tool")) {
    int key_num = *(int*)g_object_get_data(G_OBJECT(hbox), "arg");

    char key[5], code[32];
    snprintf(key, sizeof(key), "$P%d", key_num+1);
    snprintf(code, sizeof(code), "entry_%d_%d", which, key_num);

    // find the textbox within this hbox -- that's where the parameter value is
    GtkWidget *entry = find_entry(hbox, code);
    if (!entry)
      return;

    const char *val = gtk_entry_get_text(GTK_ENTRY(entry));

    // replace "$P<id>" in the command string with what the user entered,
    // using the format string provided in the config file.  If the user left
    // the entry blank, we'll replace with an empty string unless the
    // parameters was marked as required -- in this case, double & integer
    // parameters will be passed with zeros, and string params ... here we
    // act as if the parameter was optional -- likely the user will get an
    // error when the external program runs, which is the best scenario.
    char arg[64];
    if (strlen(val)>0 || !commands[which].args[key_num].optional) {
      double d;
      int i;

      switch (commands[which].args[key_num].type) {
        default:
        case 0:
          asfPrintError("Invalid type found at position %d %d\n",
                        which, key_num);
          break;
        case 1:
          d = atof(val);
          snprintf(arg, sizeof(arg), commands[which].args[key_num].format, d);
          break;
        case 2:
          i = atoi(val);
          snprintf(arg, sizeof(arg), commands[which].args[key_num].format, i);
          break;
        case 3:
          if (strlen(val)==0) {
            // required string entry left blank -- leave out entirely
            strcpy(arg, "");
          }
          else {
            snprintf(arg, sizeof(arg),
                     commands[which].args[key_num].format, val);
          }
          break;
      }
    }
    else {
      strcpy(arg, "");
    }

    // here's the actual replacement of "$P<id>" in the command string
    char *cpy = STRDUP(cmd_buf);
    char *rep = asf_strReplace(cmd_buf, key, arg);
    strncpy_safe(cmd_buf, rep, 511);
    FREE(cpy);
    FREE(rep);
  }
}

// This code actually builds a command-line for asf_mapready to use,
// substituting for each parameter the values entered by the user.  It
// is called in "settings_from_gui()", to populate the "cmd" string.
const char *get_external_command_line()
{
  GtkWidget *external_optionmenu = get_widget_checked("external_optionmenu");
  int which = gtk_option_menu_get_history(GTK_OPTION_MENU(external_optionmenu));
  strcpy(cmd_buf, commands[which-2].command);

  // the children of this vbox are all hboxes, containing the parameters
  GtkWidget *vbox = get_widget_checked("vbox_external_params");

  int *ip = MALLOC(sizeof(int));
  *ip = which;

  // here's the loop that replaces each parameter placeholder (i.e., "$P1"
  // $P2, etc) with the parameter's value, using the format string from
  // the config file.  For example, "smooth $P1 {Input} {Output}" would
  // turn into "smooth -kernel-size 7 {Input} {Output}" assuming the user
  // has entered 7.  The "-kernel-size" comes from the format string in
  // the configuration file.
  gtk_container_foreach(GTK_CONTAINER(vbox), collect_args_fn, (gpointer)ip);

  FREE(ip);
  return cmd_buf;
}

// this is the callback used to generate a CSV list of all parameters
// in all the textboxes -- used with "Save Settings"
void list_args_fn(GtkWidget *hbox, gpointer data)
{
  int tool = *(int*)g_object_get_data(G_OBJECT(hbox), "tool");
  int arg = *(int*)g_object_get_data(G_OBJECT(hbox), "arg");

  char name[32];
  snprintf(name, sizeof(name), "entry_%d_%d", tool, arg);

  // find the textbox within this hbox -- that's where the parameter value is
  GtkWidget *entry = find_entry(hbox, name);
  if (!entry)
    return;

  const char *val = gtk_entry_get_text(GTK_ENTRY(entry));

  strcat(cmd_buf, "\"");
  strcat(cmd_buf, val);
  strcat(cmd_buf, "\"");
  strcat(cmd_buf, ",");
}

const char *get_external_parameters_as_csv()
{
  GtkWidget *vbox = get_widget_checked("vbox_external_params");
  strcpy(cmd_buf, "");

  // the children of this vbox are all hboxes, containing the parameters
  gtk_container_foreach(GTK_CONTAINER(vbox), list_args_fn, NULL);

  // strip trailing comma, if present
  if (strlen(cmd_buf)>0 && cmd_buf[strlen(cmd_buf)-1]==',')
    cmd_buf[strlen(cmd_buf)-1]='\0';

  return cmd_buf;
}

// this populates the parameter textboxes from a CSV, used by "Load Settings"
void populate_external_params_from_csv(char *csv_str)
{
  int num;
  char **vals;

  split_into_array(csv_str, ',', &num, &vals);

  GtkWidget *vbox = get_widget_checked("vbox_external_params");
  GList *list = gtk_container_get_children(GTK_CONTAINER(vbox));
  GList *curr = list;

  int i=0;
  do {
    GtkWidget *hbox = (GtkWidget*)curr->data;

    int tool = *(int*)g_object_get_data(G_OBJECT(hbox), "tool");
    int arg = *(int*)g_object_get_data(G_OBJECT(hbox), "arg");

    char name[32];
    snprintf(name, sizeof(name), "entry_%d_%d", tool, arg);

    GtkWidget *entry = find_entry(hbox, name);
    if (entry) {
      gtk_entry_set_text(GTK_ENTRY(entry), vals[i]);
    }

    curr = curr->next;
    ++i;

    if (i>=num) break;
  }
  while (curr != NULL);

  g_list_free(list);
}
