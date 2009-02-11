/* parse_funcs.c
 *
 * String parsing functions made only for the baseline catalog tool
 */
#include <stdio.h>
#include <string.h>

char *my_parse_string(char *p, char *s, int max_len)
{
    if (!p || *p == '\0') {
        strcpy(s, "");
        asfPrintError("  --> Unexpected end of string\n");
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

char *my_get_str(char *line, int column_num)
{
    int i;
    char *p = line;
    static char ret[256];

    for (i=0; i<=column_num; ++i)
      p = my_parse_string(p,ret,256);

    return ret;
}

int my_get_int(char *line, int column_num)
{
    if (column_num >= 0) {
        char *s = my_get_str(line, column_num);
        if (s)
          return atoi(s);
        else
          return 0;
    }
    else {
        return 0;
    }
}

double my_get_double(char *line, int column_num)
{
    if (column_num >= 0) {
        char *s = my_get_str(line, column_num);
        if (s)
          return atof(s);
        else
          return 0.0;
    } else
        return 0.0;
}
