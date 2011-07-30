#include <ctype.h>
#include "asf_vector.h"

static char * new_blank_str(void)
{
  char *ret = CALLOC(512, sizeof(char));
  return ret;
}

static int strindex(char s[], char t[])
{
  int i, j, k;

  for (i=0; s[i]!='\0'; i++) {
    for (j=i, k=0; t[k]!='\0' && s[j]==t[k]; j++, k++)
      ;
    if (k>0 && t[k]=='\0')
      return i;
  }
  return -1;
}

static char *read_param(char *line)
{
  int i, k;
  char *value=new_blank_str();

  i=strindex(line, "]");
  k=strindex(line, "=");
  if (i>0) strncpy(value, line, i+1);
  if (k>0) strncpy(value, line, k-1);
  return value;
}

static void read_str(char *dest, char *line, char *param)
{
  char *start = strchr(line, '=');
  if (start)
    sscanf(start+1, "%s", dest);
  else
    strcpy(dest, "");
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

    printf(buf);
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

int isVisible(dbf_header_t *dbf, int nCols, char *header)
{
  int ii;
  for (ii=0; ii<nCols; ii++) {
    if (strcmp(dbf[ii].header, header) == 0 && dbf[ii].visible)
      return TRUE;
  }
  return FALSE;
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

int read_header_config(const char *format, dbf_header_t **dbf, int *nColumns)
{
  char header_file[1024];
  sprintf(header_file, "%s%c%s", 
	  get_asf_share_dir(), DIR_SEPARATOR, "header.lst");
  return read_header_config_ext(format, dbf, nColumns, header_file);
}

int read_header_config_ext(const char *format, dbf_header_t **dbf, 
			   int *nColumns, char *header_file)
{
  if (!format)
    return FALSE;

  FILE *fp;
  char line[255], params[255], format_str[255], type[25];
  int m=0, n=0, ii, found_format=FALSE;;
  char *test = new_blank_str();
  sprintf(format_str, "[%s]", uc(format));

  // Check how many parameters we have in the section
  fp = FOPEN(header_file, "r");
  while (fgets(line, 255, fp)) {
    if (strncmp_case(line, format_str, strlen(format_str)-1) == 0)
      strcpy(params, format);
    if (strcmp_case(params, format) == 0) {
      found_format = TRUE;
      char *str = strchr(line, '=');
      if (str) 
	m++;
      test = read_param(line);
      if (test[0] == '[' && strncmp(test, format_str, 
				    strlen(format_str)-1) != 0)
	break;
    }
  }
  FCLOSE(fp);
  *nColumns = m;

  // Return if we can't find the format we are looking for
  if (!found_format)
    return FALSE;

  // Fill the header information
  dbf_header_t *header = (dbf_header_t *) MALLOC(sizeof(dbf_header_t)*m);
  strcpy(params, "");
  strcpy(test, "");
  fp = FOPEN(header_file, "r");
  while (fgets(line, 255, fp)) {
    if (strncmp_case(line, format_str, strlen(format_str)-1) == 0)
      strcpy(params, format);
    if (strcmp_case(params, format) == 0) {
      found_format = TRUE;
      test = read_param(line);
      char *str = strchr(line, '=');
      if (str) {
	read_str(type, line, test);

	// Assign format
	if (strncmp_case(type, "DOUBLE", 6) == 0)
	  header[n].format = DBF_DOUBLE;
	else if(strncmp_case(type, "INTEGER", 7) == 0)
	  header[n].format = DBF_INTEGER;
	else
	  header[n].format = DBF_STRING;

	// Is parameter commented out?
	if (test[0] == '#') {
	  header[n].visible = FALSE;
	  test++;
	  while (isspace(*test))
	    test++;
	}
	else
	  header[n].visible = TRUE;

	// Assign header
	sprintf(header[n].header, "%s", test);
	n++;
      }
      if (test[0] == '[' && strncmp(test, format_str, 
				    strlen(format_str)-1) != 0)
        break;
    }  
  }
  FCLOSE(fp);

  // Check for duplicates
  int kk;
  for (ii=0; ii<n; ii++) {
    for (kk=0; kk<n; kk++) {
      if (strcmp(header[ii].header, header[kk].header) == 0 && ii != kk) {
	// Allowed known exceptions
	if (strcmp_case(header[ii].header, "SCN_RULAT") == 0)
	  strcpy(header[kk].header, "SCN_RDLAT");
	else {
	  asfPrintError("Duplicate header name - don't know how to "
                        "handle that.\n"
			"  Column %d header: %s\n"
			"  Column %d header: %s\n", 
			ii, header[ii].header, 
			kk, header[kk].header);
	}
      }
    }
  }

  *dbf = header;

  return TRUE;
}

