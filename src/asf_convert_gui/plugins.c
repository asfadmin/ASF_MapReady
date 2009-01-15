#include "asf_convert_gui.h"
#include <ctype.h>

typedef struct {
    int type;
    int optional;
    char format[32];
    char description[32];
} Arg;

#define MAX_ARGS 4
typedef struct {
    char name[32];
    char command[1024];
    int num_args;
    Arg args[MAX_ARGS];
} ExternalCommand;

int num_external;
ExternalCommand commands[10];

static void strip_end_whitesp_inplace(char *s)
{
    char *p = s + strlen(s) - 1;
    while (isspace(*p) && p>s)
        *p-- = '\0';
}

static void consolidate_quotes(char *s)
{
    int i,j=0,l=strlen(s)+1;
    char *buf = MALLOC(sizeof(char)*l);
    for (i=0; i<l; ++i) {
        if (s[i]=='\"' && s[i+1]=='\"') {
            buf[j] = '\"';
            ++i;
        } else {
            buf[j] = s[i];
        }
        ++j;
    }
    strcpy(s,buf);
    FREE(buf);
}

static char *quoted_string_parse(char *p, char *s, int max_len, int line_num)
{
  if (!p) {
    // reached end of the line prematurely
    if (line_num>0)
      printf("Line %d: Not enough data columns.\n", line_num);
    strcpy(s,"");
    return NULL;
  }

  // starting at p, eat characters, putting into s (allocated by caller),
  // until we reach the ending comma, or max_len.  For quoted strings,
  // we eat until we find the closing quote.

  // first, we eat whitespace
  while (isspace(*p))
    ++p;

  // now see if we have a quoted string or not
  if (*p == '\"') {
    // a quoted string... scan ahead to next quote instead of comma
    char *q = strchr(p+1, '\"');
    if (q) {
      // after the quote, the next non-whitespace character should be a comma
      // if it is another quote, then we need to keep going
      char *r = q+1;
      while (*r == '\"') {
        char *q1 = strchr(r+1, '\"');
        if (!q1)
          break;
        else {
          q = q1;
          r = q+1;
        }
      }

      // eat whitespace, until we get to what should be a comma
      while (isspace(*r))
        ++r;
      // now, eat the comma
      if (*r != ',') {
        if (line_num>0)
          printf("Line %d: Column entry has extra characters\n", line_num);
        // scan ahead to the next comma...
        r = strchr(r,',');        
      }
      
      // do not include the quotes in the returned value
      *q = '\0'; // temporary
      strncpy_safe(s, p+1, max_len);
      *q = '\"';

      // convert all instances of "" to just plain "
      consolidate_quotes(s);

      // now return pointer to just after the comma (if we found one)
      if (r)
        return r+1;
      else
        return NULL;
    }
    else {
      // no trailing quote found!
      // just put everything that's left into the output string
      strncpy_safe(s, p+1, max_len);
      // return NULL (to indicate this is the last column)
      return NULL;
    }
  }
  else {
    // non-quoted string
    char *q = strchr(p, ',');
    if (q) {
      *q = '\0'; // temporary
      strncpy_safe(s, p, max_len);
      strip_end_whitesp_inplace(s);
      *q = ',';

      // return pointer to just after the comma
      return q+1;
    }
    else {
      // no more commas, just return what is left in the string
      // and return NULL to indicate that we have everything now
      strncpy_safe(s, p, max_len);
      strip_end_whitesp_inplace(s);
      return NULL;
    } 
  }
}

static void split_into_array(char *str, char sep, int *nelem, char ***parr)
{
  char *p = str;
  int i,n=0;
  do {
    char val[512];
    p = quoted_string_parse(p, val, 512, -1);
    ++n;
  } while (p);

  char **arr = MALLOC(sizeof(char*)*n);
  p = str;
  for (i=0; i<n; ++i) {
    char val[512];
    p = quoted_string_parse(p, val, 512, -1);
    arr[i] = MALLOC(sizeof(char)*(strlen(val)+5));
    strcpy(arr[i], val);
  }

  *parr = arr;
  *nelem = n;
}

// splits a string into two pieces, stuff before the separater character
// and the stuff after it.  The separator character is not included in
// either string
static void split2(const char *str_in, char sep, char **s1_out, char **s2_out)
{
  char *str = STRDUP(str_in);
  char *s1 = MALLOC(sizeof(char)*(strlen(str)+1));
  char *s2 = MALLOC(sizeof(char)*(strlen(str)+1));

  char *p = strchr(str, sep);

  if (p) {
    *p = '\0';
    strcpy(s1, str);
    *p = sep;
    strcpy(s2, p+1);
  } else {
    // no sep -- s2 is empty, s1 is a copy of str
    strcpy(s1, str);
    strcpy(s2, "");
  }

  // trim whitespace
  *s1_out = trim_spaces(s1);
  *s2_out = trim_spaces(s2);

  FREE(s1);
  FREE(s2);
  FREE(str);
}

void load_external_commands()
{
  // read the "plugins.cfg" file
  int i,j;
  FILE *pf = fopen_share_file("plugins.cfg", "r");
  if (pf) {
    char buf[1025];
    int n=0, line_num=1;
    commands[0].num_args=0;
    for (i=0; i<MAX_ARGS; ++i)
      commands[n].args[i].type=0;
    asfPrintStatus("Parsing plugins.cfg...\n");
    while (fgets(buf, 1024, pf) != NULL) {
      char *line = trim_spaces(buf);
      if (strlen(line) == 0) {
        int ok = TRUE;
        if (strlen(commands[n].name)==0 || strlen(commands[n].command)==0) {
          asfPrintWarning("Invalid blank line found on line %d.\n", line_num);
          ok=FALSE;
        }
        else {
          for (i=0; i<MAX_ARGS; ++i) {
            char key[4];
            sprintf(key, "$P%d", i+1);
            if (commands[n].args[i].type==0 &&
                strstr(commands[n].command, key)!=NULL)
            {
              asfPrintWarning("%s not specified for command ending on "
                              "line %d.\n", key, line_num);
              ok=FALSE;
            }
          }
        }
        if (ok) {
          // move to next external command
          ++n;
          commands[n].num_args=0;
          for (i=0; i<MAX_ARGS; ++i) // setting all args to "unset"
            commands[n].args[i].type=0;
        }
      }
      else {
        char *key, *val;
        split2(line, '=', &key, &val);
        if (strcmp_case(key, "Name")==0) {
          strcpy(commands[n].name, val);
        }
        else if (strcmp_case(key, "Command")==0) {
          strcpy(commands[n].command, val);
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

            optional=1;
            if (strcmp_case(cols[1], "optional")==0)
              optional=1;
            else if (strcmp_case(cols[1], "required")==0)
              optional=0;
            else
              asfPrintWarning("Invalid required/optional flag on line %d: %s\n",
                              line_num, cols[1]);

            strncpy_safe(format, cols[2], 30);
            strncpy_safe(description, cols[3], 30);

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
              commands[n].args[which].type = type;
              commands[n].args[which].optional = optional;
              strcpy(commands[n].args[which].format, format);
              strcpy(commands[n].args[which].description, description);
              if (which>=commands[n].num_args)
                commands[n].num_args = which+1;
            }
          }
        }
        else {
          asfPrintWarning("Invalid key found on line %d: %s\n", key, line_num);
        }
      }
      ++line_num;
    }
    if (strlen(commands[n].name)>0 && strlen(commands[n].command)>0) {
      ++n;
    }
    num_external = n;
    fclose(pf);
  }
  else {
    num_external = 0;
  }

  // debug check
  {
    for (i=0; i<num_external; ++i) {
      printf("\n----External Command #%d----\n", i);
      printf("  Name        : %s\n", commands[i].name);
      printf("  Command-line: %s\n", commands[i].command);
      printf("  Number Args : %d\n", commands[i].num_args);
      for (j=0; j<MAX_ARGS; ++j) {
        if (commands[i].args[j].type>0) {
          printf("  P%d Type     : %s\n", j+1,
                 commands[i].args[j].type==1 ? "double" :
                 commands[i].args[j].type==2 ? "int" :
                 commands[i].args[j].type==3 ? "string" :
                 "error");
          printf("  P%d Optional : %s\n", j+1,
                 commands[i].args[j].optional ? "Yes" : "No");
          printf("  P%d Format   : %s\n", j+1,
                 commands[i].args[j].format);
          printf("  P%d Descrip  : %s\n", j+1,
                 commands[i].args[j].description);
        }
      }
      printf("----END----\n");
    }
  }

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
        GtkWidget *label = gtk_label_new(txt);
        gtk_widget_set_size_request(label, 200, -1);
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
        gtk_widget_set_size_request(label, width, -1);
        gtk_widget_show(entry);

        // now plop into an hbox
        GtkWidget *hbox = gtk_hbox_new(FALSE, 1);
        gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
        gtk_box_pack_end(GTK_BOX(hbox), entry, FALSE, FALSE, 2);
        g_object_set_data(G_OBJECT(hbox), "tool", (gpointer)i);
        g_object_set_data(G_OBJECT(hbox), "arg", (gpointer)j);
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
  int which = (int)data;

  // which is the item selected -- subtract 2 to eliminate "none" and "----"
  // the index is the tool ID
  which -= 2;

  // if the tool ID matches that of the parameter specified in this hbox
  // for "None", which will be -2, which won't match anything
  if (which == (int)g_object_get_data(G_OBJECT(hbox), "tool"))
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

  // now, hide/show all of the parameters that are appropriate for the
  // selected tool.  See "hide_show_fn" above
  if (external_is_checked) {
    GtkWidget *external_optionmenu =
      get_widget_checked("external_optionmenu");
    int which = gtk_option_menu_get_history(
      GTK_OPTION_MENU(external_optionmenu));
    
    GtkWidget *vbox = get_widget_checked("vbox_external_params");

    // the children of this vbox are all hboxes, containing the parameters
    gtk_container_foreach(GTK_CONTAINER(vbox), hide_show_fn, (gpointer)which);
  }
}

SIGNAL_CALLBACK void on_external_optionmenu_changed(GtkWidget *widget)
{
  external_settings_changed();  
}

static char cmd_buf[512];

void collect_args_fn(GtkWidget *hbox, gpointer data)
{
  int which = (int)data - 2;

  if (which == (int)g_object_get_data(G_OBJECT(hbox), "tool")) {
    int key_num = (int)g_object_get_data(G_OBJECT(hbox), "arg");

    char key[5], code[32];
    snprintf(key, sizeof(key), "$P%d", key_num+1);
    snprintf(code, sizeof(code), "entry_%d_%d", which, key_num);

    // find the textbox within this hbox -- that's where the parameter value is
    GList *list = gtk_container_get_children(GTK_CONTAINER(hbox));
    GList *curr=list;
    GtkWidget *entry = NULL;
    do {
      if (strcmp(code, gtk_widget_get_name(GTK_WIDGET(curr->data)))==0) {
        entry = GTK_WIDGET(curr->data);
        break;
      }

      curr = curr->next;
    }
    while (curr != NULL);

    if (!entry)
      return;

    const char *val = gtk_entry_get_text(GTK_ENTRY(entry));
    g_list_free(list);

    double d;
    int i;
    char arg[64];
    switch (commands[which].args[key_num].type) {
      default:
      case 0:
        asfPrintError("Invalid type found at position %d %d\n", which, key_num);
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
        snprintf(arg, sizeof(arg), commands[which].args[key_num].format, val);
        break;
    }

    char *cpy = STRDUP(cmd_buf);
    char *rep = strReplace(cmd_buf, key, arg);
    strncpy_safe(cmd_buf, rep, 511);
    FREE(cpy);
    FREE(rep);
  }
}

const char *get_external_command_line()
{
  GtkWidget *external_optionmenu = get_widget_checked("external_optionmenu");
  int which = gtk_option_menu_get_history(GTK_OPTION_MENU(external_optionmenu));
  GtkWidget *vbox = get_widget_checked("vbox_external_params");
  strcpy(cmd_buf, commands[which-2].command);

  // the children of this vbox are all hboxes, containing the parameters
  gtk_container_foreach(GTK_CONTAINER(vbox), collect_args_fn, (gpointer)which);

  return cmd_buf;
}
