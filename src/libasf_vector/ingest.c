#include <ctype.h>
#include "shapefil.h"
#include "asf_vector.h"

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

int find_column(char *line, const char *column_header)
{
  char *p = line;
  char val[256];
  int col=0;

  while (p) {
    p=my_parse_string(p,val,256);
    if (strcmp_case(val,column_header)==0)
      return col;
    ++col;
  }

  // column heading was not found
  return -1;
}

int find_column_start(char *line, const char *column_header, int n)
{
  char *p = line;
  char val[256];
  int col=0;

  while (p) {
    p=my_parse_string(p,val,256);
    if (strncmp_case(val,column_header,n)==0)
      return col;
    ++col;
  }

  // column heading was not found
  return -1;
}

char *get_column(char *line, int column)
{
  char *p = line;
  char *val = (char *) MALLOC(sizeof(char)*255);;
  int col=0;

  while (p) {
    p=my_parse_string(p,val,256);
    ++col;
    if (col == column)
      break;
  }
  return val;
}

int get_number_columns(char *line)
{
  char *p = line;
  char val[256];
  int col=0;

  while (p) {
    p=my_parse_string(p,val,256);
    ++col;
  }
  return col;
}

int is_lat_name(const char *header)
{
  int ret = FALSE;
  if (strcmp_case(header, "SCN_LULAT")==0) ret = TRUE;
  if (strcmp_case(header, "SCN_RULAT")==0) ret = TRUE;
  if (strcmp_case(header, "SCN_LDLAT")==0) ret = TRUE;
  if (strcmp_case(header, "SCN_RDLAT")==0) ret = TRUE;
  if (strncmp_case(header, "LAT", 3)==0) ret = TRUE;
  return ret;
}

int is_lon_name(const char *header)
{
  int ret = FALSE;
  if (strcmp_case(header, "SCN_LULON")==0) ret = TRUE;
  if (strcmp_case(header, "SCN_RULON")==0) ret = TRUE;
  if (strcmp_case(header, "SCN_LDLON")==0) ret = TRUE;
  if (strcmp_case(header, "SCN_RDLON")==0) ret = TRUE;
  if (strncmp_case(header, "LON", 3)==0) ret = TRUE;
  return ret;
}

void read_dbf_header_info(char *inFile, dbf_header_t **dbf, int *nCols, 
			  int *nLatLons, loc_style_t *locStyle)
{
  dbf_header_t *dbf_header;
  loc_style_t location = LOC_UNKNOWN;
  FILE *fp;
  char header[4096]="", format[4096]="", *formatStr, *p;
  int ii, format_found=FALSE, vertices=0, columns, lat_col, lon_col;
 
  // Expect one or two line header information in the following format:
  // # <header name 1>, <header name 2>, ...
  // # <format header 1>, <format header 2>, ...
  // Second line with format information is optional: set to string if not
  // available

  fp = FOPEN(inFile, "r");
  while (fgets(header, 4096, fp)) {
    if (header[0] == '#')
      break;
  }
  while (fgets(format, 4096, fp)) {
    if (format[0] == '#') {
      format_found = TRUE;
      break;
    }
  }
  FCLOSE(fp);
  
  // Allocate memory for header structure
  columns = get_number_columns(header);
  dbf_header = (dbf_header_t *) MALLOC(sizeof(dbf_header_t)*columns);
  for (ii=0; ii<columns; ii++) {
    dbf_header[ii].header = (char *) MALLOC(sizeof(char)*50);
    strcpy(dbf_header[ii].header, "");
    // Default format for attributes is STRING, just in case we cannot
    // figure out what the values actually are.
    dbf_header[ii].format = DBF_STRING;
  }

  if (location == LOC_UNKNOWN) {
    // Find vertices ALOS CSV style
    lat_col = find_column(header, "SCN_LULAT");
    lon_col = find_column(header, "SCN_LULON");
    if (lat_col > 0 && lon_col > 0) {
      location = LOC_ALOS_CSV;
      vertices++;
    }
    lat_col = find_column(header, "SCN_RULAT");
    lon_col = find_column(header, "SCN_RULON");
    if (lat_col > 0 && lon_col > 0) {
      location = LOC_ALOS_CSV;
      vertices++;
    }
    lat_col = find_column(header, "SCN_LDLAT");
    lon_col = find_column(header, "SCN_LDLON");
    if (lat_col > 0 && lon_col > 0) {
      location = LOC_ALOS_CSV;
      vertices++;
    }
    lat_col = find_column(header, "SCN_RDLAT");
    lon_col = find_column(header, "SCN_RDLON");
    if (lat_col > 0 && lon_col > 0) {
      location = LOC_ALOS_CSV;
      vertices++;
    }
  }
  if (location == LOC_UNKNOWN) {
    // Look for 'LATx/LONx' scheme
    // FIXME: Condition does not work this way. Need to think it through
    for (ii=0; ii<columns; ii++) {
      lat_col = find_column_start(dbf_header[ii].header, "LAT", 3);
      lon_col = find_column_start(dbf_header[ii].header, "LON", 3);
      if (lat_col > 0 && lon_col > 0) {
	location = LOC_LAT_LON;
	vertices++;
      }
    }
  }

  // Read the header content into the structure
  formatStr = (char *) MALLOC(sizeof(char)*10);
  for (ii=0; ii<columns; ii++) {
    dbf_header[ii].header = get_column(header, ii+1);
    formatStr = get_column(format, ii+1);
    if (format_found) {
      if (strcmp(uc(formatStr), "STRING")==0)
	dbf_header[ii].format = DBF_STRING;
      else if (strcmp(uc(formatStr), "DOUBLE")==0)
	dbf_header[ii].format = DBF_DOUBLE;
      else if (strcmp(uc(formatStr), "INTEGER")==0)
	dbf_header[ii].format = DBF_INTEGER;
    }
  }

  // Clean up first and last column header
  p = dbf_header[0].header;
  ii = 0;
  while (dbf_header[0].header[ii] == '#' || dbf_header[0].header[ii] == ' ') {
    p++;
    ii++;
  }
  dbf_header[0].header = p;
  p = trim_spaces(dbf_header[columns-1].header);
  dbf_header[columns-1].header = p;;

  // Clean up
  FREE(formatStr);

  // Return values
  *dbf = dbf_header;
  *nCols = columns;
  *nLatLons = vertices;
  *locStyle = location;  
}
