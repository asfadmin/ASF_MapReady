#include <libxml/parser.h>
#include <libxml/tree.h>

xmlNode *findNode(xmlDoc *doc, xmlNode *node, char *name);
const char *xml_get_string(xmlDoc *doc, char *str);
double xml_get_double(xmlDoc *doc, char *str);
double xml_get_int(xmlDoc *doc, char *str);
