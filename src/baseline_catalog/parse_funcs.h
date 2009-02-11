/* parse_funcs.h
 *
 * String parsing functions made only for the baseline catalog tool
 */

#ifndef __PARSE_FUNCS_H

#define __PARSE_FUNCS_H

char   *my_parse_string(char *p, char *s, int max_len);
char   *my_get_str(char *line, int column_num);
int     my_get_int(char *line, int column_num);
double  my_get_double(char *line, int column_num);

#endif
