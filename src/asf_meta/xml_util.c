#include "xml_util.h"
#include "asf.h"
#include "asf_meta.h"
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>

#define MAX_LEN 100000
static char buf[MAX_LEN];

static xmlNode *findNode(xmlDoc *doc, xmlNode *node, char *name, int desired)
{
  // the "count" refers to how many Nodes with that name we've found,
  // in case we are supposed to find one that isn't the first in a list
  int count = 0;
  xmlNode *cur = node->xmlChildrenNode;

  //printf("XML>> Looking for: %s (occurence: %d)\n", name, desired);
  while (cur != NULL) {
    //printf("XML>>... %s?\n", (char*)cur->name);
    if (!xmlStrcmp(cur->name, (const xmlChar *)name)) {
      ++count;
      //printf("XML>>    yes!  that's %d!\n", count);
      if (count == desired) {
        //printf("XML>>      ... which is the one we want!\n");
        return cur;
      }
    }
    cur = cur->next;
  }
  return NULL;
}

static void extract_array_specifier(const char *in, char **out_p, int *n)
{
  char *out=NULL;
  int len = strlen(in);
  int has_array_specifier = FALSE;

  if (in[len-1] == ']') {
    int m = len-2;
    if (isdigit(in[m])) {
      while (isdigit(in[m])) --m;
      if (in[m] == '[') {
        // looks like it has the form "blahblah[%d]" -- strip off the end
        out = CALLOC(strlen(in)+1, sizeof(char));
        strncpy(out, in, m);
        *n = 1+atoi(&in[m+1]);
        has_array_specifier = TRUE;
      }
    }
  }

  if (!has_array_specifier) {
    out = STRDUP(in);
    *n=1;
  }

  *out_p = out;
}

int xml_get_element_exists(xmlDoc *doc, char *str)
{
  const char *val = xml_get_string_value(doc, str);
  return val != NULL;
}

const char *xml_get_string_value(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[100000];

  va_start(ap, format);
  vsnprintf(str, 99999, format, ap);
  va_end(ap);

  int i,n;
  char **arr;
  int found = TRUE;

  split_into_array(str, '.', &n, &arr);

  xmlNode *cur = xmlDocGetRootElement(doc);

  // first item much match the name of the root
  if (strcmp(arr[0], (char*)cur->name)!=0) {
    // root node doesn't match -- return empty string
    strcpy(buf, "");
  }
  else {
    // subsequent items specify the search path through the xml tree
    for (i=1; i<n; ++i) {
      char *elem;
      int k;
      extract_array_specifier(arr[i], &elem, &k);

      xmlNode *next = findNode(doc, cur, elem, k);
      if (!next) {
        // not found -- return NULL
        found = FALSE;
        strcpy(buf, "");
        FREE(elem);
        break;
      }
      
      FREE(elem);
      cur = next;
    }
  }

  if (found) {
    assert(cur != NULL);
    xmlChar *ret = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
    strncpy_safe(buf, (char*)ret, MAX_LEN-1);
    xmlFree(ret);
  }
  else {
    strcpy(buf, MAGIC_UNSET_STRING);
  }

  free_char_array(&arr, n);

  return buf;
}

const char *xml_get_string_attribute(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[4096];

  va_start(ap, format);
  vsnprintf(str, 4095, format, ap);
  va_end(ap);

  int i,n;
  char **arr;
  int found = TRUE;

  split_into_array(str, '.', &n, &arr);

  xmlNode *cur = xmlDocGetRootElement(doc);

  // first item much match the name of the root
  if (strcmp(arr[0], (char*)cur->name)!=0) {
    // root node doesn't match -- return empty string
    strcpy(buf, "");
  }
  else {
    // subsequent items specify the search path through the xml tree
    // all except the last one -- that is the attribute name
    for (i=1; i<n-1; ++i) {
      char *elem;
      int k;
      extract_array_specifier(arr[i], &elem, &k);

      xmlNode *next = findNode(doc, cur, elem, k);
      if (!next) {
        // not found -- return NULL
        found = FALSE;
        strcpy(buf, "");
        FREE(elem);
        break;
      }
      
      FREE(elem);
      cur = next;
    }
  }

  if (found) {
    assert(cur != NULL);
    xmlChar *val = xmlGetProp(cur, (xmlChar*)(arr[n-1]));
    if (val) {
      strncpy_safe(buf, (char*)val, MAX_LEN-1);
      xmlFree(val);
    }
    else {
      // found the node, but it did not have the requested attribute
      found = FALSE;
    }
  }

  if (!found) {
    strcpy(buf, MAGIC_UNSET_STRING);
  }

  free_char_array(&arr, n);
  return buf;
}

double xml_get_double_value(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[4096];

  va_start(ap, format);
  vsnprintf(str, 4095, format, ap);
  va_end(ap);

  const char *val = xml_get_string_value(doc, str);
  if (val && strcmp(val, MAGIC_UNSET_STRING) != 0)
    return atof(val);
  else
    return MAGIC_UNSET_DOUBLE;
}

int xml_get_int_value(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[4096];

  va_start(ap, format);
  vsnprintf(str, 4095, format, ap);
  va_end(ap);

  const char *val = xml_get_string_value(doc, str);
  if (val && strcmp(val, MAGIC_UNSET_STRING) != 0)
    return atoi(val);
  else
    return MAGIC_UNSET_INT;
}

long xml_get_long_value(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[4096];

  va_start(ap, format);
  vsnprintf(str, 4095, format, ap);
  va_end(ap);

  const char *val = xml_get_string_value(doc, str);
  if (val && strcmp(val, MAGIC_UNSET_STRING) != 0)
    return atol(val);
  else
    return MAGIC_UNSET_INT;
}

double xml_get_double_attribute(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[4096];

  va_start(ap, format);
  vsnprintf(str, 4095, format, ap);
  va_end(ap);

  const char *val = xml_get_string_attribute(doc, str);
  if (val && strcmp(val, MAGIC_UNSET_STRING) != 0)
    return atof(val);
  else
    return MAGIC_UNSET_DOUBLE;
}

int xml_get_int_attribute(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[4096];

  va_start(ap, format);
  vsnprintf(str, 4095, format, ap);
  va_end(ap);

  const char *val = xml_get_string_attribute(doc, str);
  if (val && strcmp(val, MAGIC_UNSET_STRING) != 0)
    return atoi(val);
  else
    return MAGIC_UNSET_INT;
}

long xml_get_long_attribute(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[4096];

  va_start(ap, format);
  vsnprintf(str, 4095, format, ap);
  va_end(ap);

  const char *val = xml_get_string_attribute(doc, str);
  if (val && strcmp(val, MAGIC_UNSET_STRING) != 0)
    return atol(val);
  else
    return MAGIC_UNSET_INT;
}

/* xmlChildElementCount not available in libxml2 2.6 */
/* Returns the number of children of node */
static long
my_xmlChildElementCount(xmlNodePtr node)
{
    long ret = 0;
    xmlNodePtr cur = NULL;

    if (!node || node->type != XML_ELEMENT_NODE)
        return 0;
    cur = node->children;
    while (cur) {
        if (cur->type == XML_ELEMENT_NODE)
            ret++;
        cur = cur->next;
    }
    return ret;
}

int xml_get_children_count(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[100000];
  int count=0;
  
  va_start(ap, format);
  vsnprintf(str, 99999, format, ap);
  va_end(ap);

  int i,n;
  char **arr;
  int found = TRUE;

  split_into_array(str, '.', &n, &arr);

  xmlNode *cur = xmlDocGetRootElement(doc);

  // first item much match the name of the root
  if (strcmp(arr[0], (char*)cur->name)!=0) {
    // root node doesn't match -- return empty string
    strcpy(buf, "");
  }
  else {
    // subsequent items specify the search path through the xml tree
    for (i=1; i<n; ++i) {
      char *elem;
      int k;
      extract_array_specifier(arr[i], &elem, &k);

      xmlNode *next = findNode(doc, cur, elem, k);
      if (!next) {
        // not found -- return NULL
        found = FALSE;
        strcpy(buf, "");
        FREE(elem);
        break;
      }
      
      FREE(elem);
      cur = next;
    }
  }

  if (found) {
    assert(cur != NULL);
    count = my_xmlChildElementCount(cur);
  }

  free_char_array(&arr, n);

  return count;
}

int xml_xpath_element_exists(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[100000];

  va_start(ap, format);
  vsnprintf(str, 99999, format, ap);
  va_end(ap);

  xmlXPathContext *xpathContext;
  xmlXPathObject *xpathObject;
  xmlNodeSet *nodeset;
  
  xpathContext = xmlXPathNewContext(doc);
  if (xpathContext == NULL) {
    asfPrintWarning("Unable to create new XPath context.\n");
    return MAGIC_UNSET_STRING;
  }
  xpathObject = xmlXPathEvalExpression((xmlChar *)str, xpathContext);
  xmlXPathFreeContext(xpathContext);
  if (xpathObject == NULL) {
    asfPrintWarning("Unable to evaluate XPath expression.\n");
    return MAGIC_UNSET_STRING;
  }
  if (xmlXPathNodeSetIsEmpty(xpathObject->nodesetval)) {
    xmlXPathFreeObject(xpathObject);
    return FALSE;
  }
  else {
    xmlXPathFreeObject(xpathObject);
    return TRUE;
  }
}

const char *xml_xpath_get_string_value(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[100000];

  va_start(ap, format);
  vsnprintf(str, 99999, format, ap);
  va_end(ap);

  xmlXPathContext *xpathContext;
  xmlXPathObject *xpathObject;
  xmlNodeSet *nodeset;
  
  xpathContext = xmlXPathNewContext(doc);
  if (xpathContext == NULL) {
    asfPrintWarning("Unable to create new XPath context.\n");
    return MAGIC_UNSET_STRING;
  }
  xpathObject = xmlXPathEvalExpression((xmlChar *)str, xpathContext);
  xmlXPathFreeContext(xpathContext);
  if (xpathObject == NULL) {
    asfPrintWarning("Unable to evaluate XPath expression.\n");
    return MAGIC_UNSET_STRING;
  }
  if (xmlXPathNodeSetIsEmpty(xpathObject->nodesetval)) {
    xmlXPathFreeObject(xpathObject);
    asfPrintWarning("Could not find '%s'.\n", str);
    return MAGIC_UNSET_STRING;
  }
  nodeset = xpathObject->nodesetval;
  return 
    (char *)xmlNodeListGetString(doc, nodeset->nodeTab[0]->xmlChildrenNode,1);
}

int xml_xpath_get_int_value(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[4096];

  va_start(ap, format);
  vsnprintf(str, 4095, format, ap);
  va_end(ap);

  const char *val = xml_xpath_get_string_value(doc, str);
  if (val && strcmp(val, MAGIC_UNSET_STRING) != 0)
    return atoi(val);
  else
    return MAGIC_UNSET_INT;
}

double xml_xpath_get_double_value(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[4096];

  va_start(ap, format);
  vsnprintf(str, 4095, format, ap);
  va_end(ap);

  const char *val = xml_xpath_get_string_value(doc, str);
  if (val && strcmp(val, MAGIC_UNSET_STRING) != 0)
    return atof(val);
  else
    return MAGIC_UNSET_DOUBLE;
}

int xml_xpath_get_count(xmlDoc *doc, char *format, ...)
{
  va_list ap;
  char str[100000];

  va_start(ap, format);
  vsnprintf(str, 99999, format, ap);
  va_end(ap);

  xmlXPathContext *xpathContext;
  xmlXPathObject *xpathObject;
  xmlNodeSet *nodeset;
  
  xpathContext = xmlXPathNewContext(doc);
  if (xpathContext == NULL) {
    asfPrintWarning("Unable to create new XPath context.\n");
    return MAGIC_UNSET_STRING;
  }
  xpathObject = xmlXPathEvalExpression((xmlChar *)str, xpathContext);
  xmlXPathFreeContext(xpathContext);
  if (xpathObject == NULL) {
    asfPrintWarning("Unable to evaluate XPath expression.\n");
    return MAGIC_UNSET_STRING;
  }
  if (xmlXPathNodeSetIsEmpty(xpathObject->nodesetval)) {
    xmlXPathFreeObject(xpathObject);
    asfPrintWarning("Could not find '%s'.\n", str);
    return MAGIC_UNSET_STRING;
  }
  nodeset = xpathObject->nodesetval;
  return nodeset->nodeNr;
}


// Whole bunch of test code... 

static int n_ok=0;
static int n_bad=0;

static void test_string(xmlDoc *doc, char *key, const char *expected)
{
  const char *val = xml_get_string_value(doc, key);
  int passed;

  if (!expected) {
    passed = val==NULL;
  }
  else {
    passed = strcmp(val, expected)==0;
  }

  if (passed) {
    ++n_ok;
  }
  else {
    printf("WRONG!  for key: %s\n"
           "  Expected: %s\n"
           "       Got: %s\n",
           key, expected, val);
    ++n_bad;
  }
}

static void test_string2(xmlDoc *doc, char *expected, char *str, int arg)
{
  const char *val = xml_get_string_value(doc, str, arg);
  int passed;

  if (!expected) {
    passed = val==NULL;
  }
  else {
    passed = strcmp(val, expected)==0;
  }

  if (passed) {
    ++n_ok;
  }
  else {
    printf("WRONG!  for key: %s\n"
           "  Expected: %s\n"
           "       Got: %s\n",
           str, expected, val);
    ++n_bad;
  }
}

static void test_string3(xmlDoc *doc, char *expected, char *str, int a1, int a2)
{
  const char *val = xml_get_string_value(doc, str, a1, a2);
  int passed;

  if (!expected) {
    passed = val==NULL;
  }
  else {
    passed = strcmp(val, expected)==0;
  }

  if (passed) {
    ++n_ok;
  }
  else {
    printf("WRONG!  for key: %s\n"
           "  Expected: %s\n"
           "       Got: %s\n",
           str, expected, val);
    ++n_bad;
  }
}

static void test_string_attr(xmlDoc *doc, char *key, const char *expected)
{
  const char *val = xml_get_string_attribute(doc, key);
  int passed;

  if (!expected) {
    passed = val==NULL;
  }
  else {
    passed = strcmp(val, expected)==0;
  }

  if (passed) {
    ++n_ok;
  }
  else {
    printf("WRONG!  for key: %s\n"
           "  Expected: %s\n"
           "       Got: %s\n",
           key, expected, val);
    ++n_bad;
  }
}

static void test_double(xmlDoc *doc, char *key, double expected)
{
  double val = xml_get_double_value(doc, key);
  if (meta_is_valid_double(expected)) {
    if (fabs(val-expected)<.00001 ) {
      ++n_ok;
    }
    else {
      printf("WRONG!  for key: %s\n"
             "  Expected: %f\n"
             "       Got: %f\n",
             key, expected, val);
      ++n_bad;
    }
  }
  else {
    if (meta_is_valid_double(val)) {
      printf("WRONG!  for key: %s\n"
             "  Expected: %f\n"
             "       Got: %f\n",
             key, expected, val);
      ++n_bad;
    }
    else {
      ++n_ok;
    }
  }
}

static void test_int(xmlDoc *doc, char *key, int expected)
{
  int val = xml_get_int_value(doc, key);
  if (expected == val) {
    ++n_ok;
  }
  else {
    printf("WRONG!  for key: %s\n"
           "  Expected: %d\n"
           "       Got: %d\n",
           key, expected, val);
    ++n_bad;
  }
}

static void test_double_attr(xmlDoc *doc, char *key, double expected)
{
  double val = xml_get_double_attribute(doc, key);
  if (meta_is_valid_double(expected)) {
    if (fabs(val-expected)<.00001 ) {
      ++n_ok;
    }
    else {
      printf("WRONG!  for key: %s\n"
             "  Expected: %f\n"
             "       Got: %f\n",
             key, expected, val);
      ++n_bad;
    }
  }
  else {
    if (meta_is_valid_double(val)) {
      printf("WRONG!  for key: %s\n"
             "  Expected: %f\n"
             "       Got: %f\n",
             key, expected, val);
      ++n_bad;
    }
    else {
      ++n_ok;
    }
  }
}

static void test_int_attr(xmlDoc *doc, char *key, int expected)
{
  int val = xml_get_int_attribute(doc, key);
  if (expected == val) {
    ++n_ok;
  }
  else {
    printf("WRONG!  for key: %s\n"
           "  Expected: %d\n"
           "       Got: %d\n",
           key, expected, val);
    ++n_bad;
  }
}

#define TOP "CornerReflectorCoordinateListDelta"
#define DESC "CornerReflectorDescriptor"

void xml_test(int *num_ok, int *num_bad)
{
  FILE *f = fopen("test.xml", "w");
  if (!f)
    asfPrintError("Failed to open test.xml.\n");
  fprintf(f, "%s",
"<CornerReflectorCoordinateListDelta>"
"<CornerReflectorDescriptor>\n"
"        <ReflectorNumber type=\"string\">DJ1</ReflectorNumber>\n"
"        <RespOrbDir>DESCENDING</RespOrbDir>\n"
"        <StartDate>19920313</StartDate>\n"
"        <EndDate>19971103</EndDate>\n"
"        <SiteName>Gerstle_River</SiteName>\n"
"        <Notes>NULL</Notes>\n"
"        <UTM_North>7077183.015</UTM_North>\n"
"        <UTM_East>598098.981</UTM_East>\n"
"        <UTM_Zone>6</UTM_Zone>\n"
"        <GeographicLat>63.80829694</GeographicLat>\n"
"        <GeographicLon>-145.007776389</GeographicLon>\n"
"        <Height>452.1</Height>\n"
"        <RefEllipsoid>WGS84</RefEllipsoid>\n"
"        <TestingAttributes attr=\"testattrvalue\">Val</TestingAttributes>"
"</CornerReflectorDescriptor>"
"<CornerReflectorDescriptor>"
"        <ReflectorNumber>DJ2</ReflectorNumber>"
"        <RespOrbDir>ASCENDING</RespOrbDir>"
"        <StartDate>19920709</StartDate>"
"        <EndDate>19970714</EndDate>"
"        <SiteName>Gerstle_River</SiteName>"
"        <Notes>NULL</Notes>"
"        <UTM_North>7077022.411</UTM_North>"
"        <UTM_East>598411.301</UTM_East>"
"        <UTM_Zone>6</UTM_Zone>"
"        <GeographicLat>63.806769</GeographicLat>"
"        <GeographicLon>-145.00154028</GeographicLon>"
"        <Height>451.9</Height>"
"        <RefEllipsoid>WGS84</RefEllipsoid>"
"        <NestedListTest>"
"            <ListItem id=\"1\">"
"                <A>A1</A>"
"                <AA>AA1</AA>"
"                <Bb>1_Bb</Bb>"
"                <BB bb=\"bb1\">1_BB</BB>"
"            </ListItem>"
"            <ListItem id=\"2\" attr2=\"oops\">"
"                <A>A2</A>"
"                <AA>AA2</AA>"
"                <Bb dbl=\"17.818\">2_Bb</Bb>"
"                <BB bb=\"bb2\">2_BB</BB>"
"            </ListItem>"
"        </NestedListTest>"
"</CornerReflectorDescriptor>"
"<CornerReflectorDescriptor>"
"        <ReflectorNumber>DJ3</ReflectorNumber>"
"        <RespOrbDir>DESCENDING</RespOrbDir>"
"        <StartDate>19920116</StartDate>"
"        <EndDate>19960125</EndDate>"
"        <SiteName>UAF_Ag_Farm</SiteName>"
"        <Notes>NULL</Notes>"
"        <UTM_North>7090249.346</UTM_North>"
"        <UTM_East>581677.867</UTM_East>"
"        <UTM_Zone>6</UTM_Zone>"
"        <GeographicLat>63.92972</GeographicLat>"
"        <GeographicLon>-145.33415</GeographicLon>"
"        <Height>375.1</Height>"
"        <RefEllipsoid>WGS84</RefEllipsoid>"
"</CornerReflectorDescriptor>"
"<CornerReflectorDescriptor>"
"        <ReflectorNumber>DJ4</ReflectorNumber>"
"        <RespOrbDir>ASCENDING</RespOrbDir>"
"        <StartDate>19951010</StartDate>"
"        <EndDate>19971006</EndDate>"
"        <SiteName>UAF_Ag_Farm</SiteName>"
"        <Notes>NULL</Notes>"
"        <UTM_North>7089984.631</UTM_North>"
"        <UTM_East>582207.175</UTM_East>"
"        <UTM_Zone>6</UTM_Zone>"
"        <GeographicLat>63.9272208</GeographicLat>"
"        <GeographicLon>-145.3235</GeographicLon>"
"        <Height>376.3</Height>"
"        <RefEllipsoid>WGS84</RefEllipsoid>"
"</CornerReflectorDescriptor>"
"</CornerReflectorCoordinateListDelta>"
);
  fclose(f);

  xmlDoc *doc = xmlReadFile("test.xml", NULL, 0);
  if (!doc)
    asfPrintError("Could not read 'test.xml'\n");

  test_string(doc, TOP "." DESC "[0].ReflectorNumber", "DJ1");
  test_string2(doc, "DJ1", TOP "." DESC "[%d].ReflectorNumber", 0);
  test_string(doc, TOP "." DESC "[1].ReflectorNumber", "DJ2"); 
  test_string2(doc, "DJ2", TOP "." DESC "[%d].ReflectorNumber", 1);
  test_string(doc, TOP "." DESC "[2].ReflectorNumber", "DJ3"); 
  test_string(doc, TOP "." DESC "[3].ReflectorNumber", "DJ4"); 
  test_string(doc, TOP "." DESC "[4].ReflectorNumber", MAGIC_UNSET_STRING);
  test_string(doc, TOP "." DESC "[0].RespOrbDir", "DESCENDING"); 
  test_string2(doc, "DESCENDING", TOP "." DESC "[%d].RespOrbDir", 0);
  test_string(doc, TOP "." DESC "[1].RespOrbDir", "ASCENDING");

  test_string(doc, TOP "." DESC "[0].SiteName", "Gerstle_River"); 
  test_int(doc, TOP "." DESC "[0].StartDate", 19920313);
  test_int(doc, TOP "." DESC "[0].EndDate", 19971103);
  test_double(doc, TOP "." DESC "[0].GeographicLat", 63.80829694);
  test_double(doc, TOP "." DESC "[0].GeographicLon", -145.007776389);
  test_double(doc, TOP "." DESC "[0].Height", 452.1);
  test_string(doc, TOP "." DESC "[0].RefEllipsoid", "WGS84");
  test_string(doc, TOP "." DESC "[0].TestingAttributes", "Val");
  test_int(doc, TOP "." DESC "[0].UTM_Zone", 6);

  test_string(doc, TOP "." DESC "[1].SiteName", "Gerstle_River"); 
  test_int(doc, TOP "." DESC "[1].StartDate", 19920709);
  test_int(doc, TOP "." DESC "[1].EndDate", 19970714);
  test_double(doc, TOP "." DESC "[1].GeographicLat", 63.806769);
  test_double(doc, TOP "." DESC "[1].GeographicLon", -145.00154028);
  test_double(doc, TOP "." DESC "[1].Height", 451.9);
  test_string(doc, TOP "." DESC "[1].RefEllipsoid", "WGS84");

  test_string(doc, TOP "." DESC "[2].SiteName", "UAF_Ag_Farm"); 
  test_int(doc, TOP "." DESC "[2].StartDate", 19920116);
  test_int(doc, TOP "." DESC "[2].EndDate", 19960125);
  test_double(doc, TOP "." DESC "[2].GeographicLat", 63.92972);
  test_double(doc, TOP "." DESC "[2].GeographicLon", -145.33415);
  test_double(doc, TOP "." DESC "[2].Height", 375.1);
  test_string(doc, TOP "." DESC "[2].RefEllipsoid", "WGS84");

  test_string(doc, TOP "." DESC "[3].SiteName", "UAF_Ag_Farm"); 
  test_int(doc, TOP "." DESC "[3].StartDate", 19951010);
  test_int(doc, TOP "." DESC "[3].EndDate", 19971006);
  test_double(doc, TOP "." DESC "[3].GeographicLat", 63.9272208);
  test_double(doc, TOP "." DESC "[3].GeographicLon", -145.3235);
  test_double(doc, TOP "." DESC "[3].Height", 376.3);
  test_string(doc, TOP "." DESC "[3].RefEllipsoid", "WGS84");

  test_string(doc, TOP "." DESC "[4].SiteName", MAGIC_UNSET_STRING); 

  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].A", "A1");
  test_string3(doc, "A1",
               TOP "." DESC "[%d].NestedListTest.ListItem[%d].A", 1, 0);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].A", "A2");
  test_string3(doc, "A2",
               TOP "." DESC "[%d].NestedListTest.ListItem[%d].A", 1, 1);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].AA", "AA1");
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].AA", "AA2");
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].Bb", "1_Bb");
  test_string3(doc, "1_Bb",
               TOP "." DESC "[%d].NestedListTest.ListItem[%d].Bb", 1, 0);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].Bb", "2_Bb");
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].BB", "1_BB");
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].BB", "2_BB");
  test_string3(doc, "2_BB",
               TOP "." DESC "[%d].NestedListTest.ListItem[%d].BB", 1, 1);

  // some failure testing
  char *us = MAGIC_UNSET_STRING;
  test_string(doc, TOP "." DESC "[2].NestedListTest.ListItem[1].BB", us);
  test_string(doc, TOP "." DESC "[25].NestedListTest.ListItem[1].BB", us);
  test_string3(doc, us,
               TOP "." DESC "[%d].NestedListTest.ListItem[%d].BB", 25, 1);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[4].BB", us);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[2].A", us);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[200].AA", us);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].Arrg", us);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem.A", "A1");
  test_string(doc, TOP "." DESC "[1].SiteNameXXX", us); 
  test_int(doc, TOP "." DESC "[1].StortDate", MAGIC_UNSET_INT);
  test_string(doc, TOP "." DESC "[1].StortDate", us);
  test_double(doc, TOP "." DESC "[1].Heigth", MAGIC_UNSET_DOUBLE);
  test_string(doc, TOP "." DESC "[1].Heigth", us);

  // testing attributes
  test_string_attr(doc, TOP "." DESC "[0].ReflectorNumber.type", "string"); 
  test_string_attr(doc, TOP "." DESC "[0].TestingAttributes.attr",
                   "testattrvalue");
  test_string_attr(doc, TOP "." DESC "[1].ReflectorNumber.type", us);
  test_int_attr(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].id", 1);
  test_string_attr(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].attr2",
                   "oops");
  test_string_attr(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].BB.bb",
                   "bb1");
  test_string_attr(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].BB.bb",
                   "bb2");
  test_string_attr(doc, TOP "." DESC "[1].NestedListTest.ListItem[2].BB.bb",
                   us);
  test_string_attr(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].Bb.bb",
                   us);
  test_string_attr(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].AA",
                   us);
  test_double_attr(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].Bb.dbl",
                   17.818);
  test_string_attr(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].BB.dbl",
                   us);
  test_double_attr(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].BB.dbl",
                   MAGIC_UNSET_DOUBLE);

  //printf("Number ok: %d\n", n_ok);
  if (n_bad>0)
    printf("**** Not all tests passed!\n"
           "**** Number of failures: %d\n", n_bad);

  xmlFreeDoc(doc);
  xmlCleanupParser();
  unlink("test.xml");

  *num_ok = n_ok;
  *num_bad = n_bad;
}

