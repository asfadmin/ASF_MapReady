#include "xml_util.h"
#include "asf.h"

static char buf[256];

xmlNode *findNode(xmlDoc *doc, xmlNode *node, char *name)
{
  xmlNode *cur = node->xmlChildrenNode;
  //printf("Find: %s\n", name);
  while (cur != NULL) {
    //printf("... %s?\n", (char*)cur->name);
    if (!xmlStrcmp(cur->name, (const xmlChar *)name)) {
      //printf("    yes!\n\n", (char*)cur->name);
      return cur;
    }
    cur = cur->next;
  }
  return NULL;
}

const char *xml_get_string(xmlDoc *doc, char *str)
{
  int i,n;
  char **arr;

  split_into_array(str, ':', &n, &arr);

  xmlNode *cur = xmlDocGetRootElement(doc);

  if (strcmp(arr[0], (char*)cur->name)!=0) {
    strcpy(buf, "");
    return buf;
  }

  for (i=1; i<n; ++i) {
    xmlNode *next = findNode(doc, cur, arr[i]);
    if (!next) {
      strcpy(buf, "");
      return buf;
    }

    cur = next;
  }
    
  xmlChar *ret = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
  strncpy_safe(buf, (char*)ret, 255);

  return buf;
}

double xml_get_double(xmlDoc *doc, char *str)
{
  const char *val = xml_get_string(doc, str);
  return atof(val);
}

double xml_get_int(xmlDoc *doc, char *str)
{
  const char *val = xml_get_string(doc, str);
  return atoi(val);
}
