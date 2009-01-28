#include <libxml/parser.h>
#include <libxml/tree.h>

int xml_get_element_exists(xmlDoc *doc, char *str);
const char *xml_get_string_value(xmlDoc *doc, char *str);
double xml_get_double_value(xmlDoc *doc, char *str);
double xml_get_int_value(xmlDoc *doc, char *str);
