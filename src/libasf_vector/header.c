#include <ctype.h>
#include "asf_vector.h"

int isVisible(dbf_header_t *dbf, int nCols, char *header)
{
  int ii;
  for (ii=0; ii<nCols; ii++) {
    if (strcmp(dbf[ii].meta, header) == 0)
      return TRUE;
  }
  return FALSE;
}

static void msg(const char *format, ...)
{
    char buf[1024];
    va_list ap;
    va_start(ap, format);
    vsprintf(buf, format, ap);
    va_end(ap);

    // in the future, we'll be putting this in a textview or something!!
    //GtkWidget *tv = get_widget_checked("messages_textview");
    //GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));

    //GtkTextIter end;
    //gtk_text_buffer_get_end_iter(tb, &end);
    //gtk_text_buffer_insert(tb, &end, buf, -1);

    printf("%s",buf);
}

static char *my_parse_string(char *p, char *s, int max_len)
{
    if (!p || *p == '\0') {
        strcpy(s, "");
        msg("  --> Unexpected end of string\n");
        return NULL;
    }

    // scan ahead to the comma, or end of string
    char *q = strchr(p, ',');
    if (q) {
      *q = '\0'; // temporarily...
      strncpy_safe(s, p, max_len);
      *q = ',';

      // point to beginning of next item
      return q+1;
    }
    else {
      strncpy_safe(s, p, max_len);

      // no more strings
      return NULL;
    }
}

char *get_column(char *line, int column)
{
  char *p = line;
  char *val = (char *) MALLOC(sizeof(char)*255);
  int col=0;

  while (p) {
    p = my_parse_string(p, val, 256);
    if (col == column)
      break;
    ++col;
  }
  return val;
}

int get_number_columns(char *line)
{
  char *p = line;
  char val[256];
  int col=0;

  // Check for anomalous separator at the end of the line
  int len = strlen(line);
  if (line[len-1] == ',')
    line[len-1] = '\0';

  while (p) {
    p = my_parse_string(p, val, 256);
    ++col;
  }
  return col;
}

int read_header_config(const char *format, dbf_header_t **dbf, 
	int *nAttr, char *shape_type)
{
  if (!format)
    return FALSE;
  strcpy(shape_type, "UNKNOWN");

  char header_file[1024];
  sprintf(header_file, "%s%c%s", 
	  get_asf_share_dir(), DIR_SEPARATOR, "header.lst");
  
  FILE *fp;
  char line[1024], params[255], format_str[255], dictionary[255], *str;
  int found_format=FALSE;
  sprintf(format_str, "[%s]", uc(format));

  // Check how many parameters we have in the section
  fp = FOPEN(header_file, "r");
  while (fgets(line, 255, fp)) {
    if (strncmp_case(line, format_str, strlen(format_str)-1) == 0)
      strcpy(params, format);
    if (strcmp_case(params, format) == 0) {
      found_format = TRUE;
      str = strstr(line, "=");
      if (strncmp_case(line, "type =", 6) == 0 && str) 
	      sprintf(shape_type, "%s", trim_spaces(str+1));
      str = strstr(line, "=");
      if (strncmp_case(line, "dictionary =", 12) == 0 && str)
        sprintf(dictionary, "%s", trim_spaces(str+1));
      if (line[0] == '[' && 
        strncmp(line, format_str, strlen(format_str)-1) != 0) {
	      break;
      }
    }
  }
  FCLOSE(fp);

  // Return if we can't find the format we are looking for
  if (!found_format)
    return FALSE;

  // Fill the header information
  char dictionary_file[1024], type[25];
  sprintf(dictionary_file, "%s%c%s", 
    get_asf_share_dir(), DIR_SEPARATOR, dictionary);
  fp = FOPEN(dictionary_file, "r");
  fgets(line, 1024, fp);
  int n = 0;
  while (fgets(line, 1024, fp))
    n++;
  FCLOSE(fp);
  dbf_header_t *header = (dbf_header_t *) MALLOC(sizeof(dbf_header_t)*n);

  fp = FOPEN(dictionary_file, "r");
  fgets(line, 1024, fp);
  n = 0;
  int nCols;
  char **column;
  while (fgets(line, 1024, fp)) {
    chomp(line);
    split_into_array(line, ',', &nCols, &column);
    header[n].meta = STRDUP(column[0]);
    header[n].shape = STRDUP(column[1]);
    sprintf(type, "%s", column[2]);
    if (strncmp_case(type, "DOUBLE", 6) == 0)
      header[n].format = DBF_DOUBLE;
    else if(strncmp_case(type, "INTEGER", 7) == 0)
      header[n].format = DBF_INTEGER;
    else
      header[n].format = DBF_STRING;
    header[n].length = atoi(column[3]);
    header[n].decimals = atoi(column[4]);
    header[n].definition = STRDUP(column[5]); 
  	n++;
  }
  FCLOSE(fp);

  *dbf = header;
  *nAttr = n;

  return TRUE;
}
