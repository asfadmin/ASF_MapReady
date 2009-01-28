#include <libxml/parser.h>
#include <libxml/tree.h>

const char *xml_get_string_value(xmlDoc *doc, char *str);
double xml_get_double_value(xmlDoc *doc, char *str);
double xml_get_int_value(xmlDoc *doc, char *str);
