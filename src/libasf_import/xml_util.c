#include "xml_util.h"
#include "asf.h"
#include <assert.h>

#define MAX_LEN
static char buf[MAX_LEN];

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
  int found = TRUE;

  split_into_array(str, ':', &n, &arr);

  xmlNode *cur = xmlDocGetRootElement(doc);

  // first item much match the name of the root
  if (strcmp(arr[0], (char*)cur->name)!=0) {
    // root node doesn't match -- return empty string
    strcpy(buf, "");
  }
  else {
    // subsequent items specify the search path through the xml tree
    for (i=1; i<n; ++i) {
      xmlNode *next = findNode(doc, cur, arr[i]);
      if (!next) {
        // not found -- return empty string
        found = FALSE;
        strcpy(buf, "");
        break;
      }
      
      cur = next;
    }
  }

  if (found) {
    assert(cur != NULL);
    xmlChar *ret = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    strncpy_safe(buf, (char*)ret, MAX_LEN-1);
    xmlFree(ret);
  }

  free_char_array(&arr, n);
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
