#include <libxml/parser.h>
#include <libxml/tree.h>

int xml_get_element_exists(xmlDoc *doc, char *str);

const char *xml_get_string_value(xmlDoc *doc, char *format, ...);
double xml_get_double_value(xmlDoc *doc, char *format, ...);
int xml_get_int_value(xmlDoc *doc, char *format, ...);
long xml_get_long_value(xmlDoc *doc, char *format, ...);

const char *xml_get_string_attribute(xmlDoc *doc, char *format, ...);
double xml_get_double_attribute(xmlDoc *doc, char *format, ...);
int xml_get_int_attribute(xmlDoc *doc, char *format, ...);
long xml_get_long_attribute(xmlDoc *doc, char *format, ...);
