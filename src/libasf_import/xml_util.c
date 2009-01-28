#include "xml_util.h"
#include "asf.h"
#include <assert.h>
#include <ctype.h>

#define MAX_LEN 256
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

const char *xml_get_string_value(xmlDoc *doc, char *str)
{
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

  free_char_array(&arr, n);

  if (found)
    return buf;
  else
    return NULL;
}

double xml_get_double_value(xmlDoc *doc, char *str)
{
  const char *val = xml_get_string_value(doc, str);
  if (val)
    return atof(val);
  else
    return MAGIC_UNSET_DOUBLE;
}

double xml_get_int_value(xmlDoc *doc, char *str)
{
  const char *val = xml_get_string_value(doc, str);
  if (val)
    return atoi(val);
  else
    return MAGIC_UNSET_INT;
}

/*
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
    printf("OK for key: %s\n", key);
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
  if (fabs(val-expected)>.00001 ) {
    printf("WRONG!  for key: %s\n"
           "  Expected: %f\n"
           "       Got: %f\n",
           key, expected, val);
    ++n_bad;
  }
  else {
    printf("OK for key: %s\n", key);
    ++n_ok;
  }
}

static void test_int(xmlDoc *doc, char *key, int expected)
{
  int val = xml_get_int_value(doc, key);
  if (expected != val) {
    printf("WRONG!  for key: %s\n"
           "  Expected: %d\n"
           "       Got: %d\n",
           key, expected, val);
    ++n_bad;
  }
  else {
    printf("OK for key: %s\n", key);
    ++n_ok;
  }
}

#define TOP "CornerReflectorCoordinateListDelta"
#define DESC "CornerReflectorDescriptor"

void test_xml()
{
  FILE *f = fopen("test.xml", "w");
  if (!f)
    asfPrintError("Failed to open test.xml.\n");
  fprintf(f, "%s",
"<CornerReflectorCoordinateListDelta>"
"<CornerReflectorDescriptor>\n"
"        <ReflectorNumber>DJ1</ReflectorNumber>\n"
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
"            <ListItem id=\"2\">"
"                <A>A2</A>"
"                <AA>AA2</AA>"
"                <Bb>2_Bb</Bb>"
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
  test_string(doc, TOP "." DESC "[1].ReflectorNumber", "DJ2"); 
  test_string(doc, TOP "." DESC "[2].ReflectorNumber", "DJ3"); 
  test_string(doc, TOP "." DESC "[3].ReflectorNumber", "DJ4"); 
  test_string(doc, TOP "." DESC "[4].ReflectorNumber", NULL);
  test_string(doc, TOP "." DESC "[0].RespOrbDir", "DESCENDING"); 
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

  test_string(doc, TOP "." DESC "[4].SiteName", NULL); 

  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].A", "A1");
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].A", "A2");
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].AA", "AA1");
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].AA", "AA2");
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].Bb", "1_Bb");
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].Bb", "2_Bb");
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].BB", "1_BB");
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[1].BB", "2_BB");

  // some failure testing
  test_string(doc, TOP "." DESC "[2].NestedListTest.ListItem[1].BB", NULL);
  test_string(doc, TOP "." DESC "[25].NestedListTest.ListItem[1].BB", NULL);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[4].BB", NULL);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[2].A", NULL);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[200].AA", NULL);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem[0].Arrg", NULL);
  test_string(doc, TOP "." DESC "[1].NestedListTest.ListItem.A", "A1");
  test_string(doc, TOP "." DESC "[1].SiteNameXXX", NULL); 
  test_int(doc, TOP "." DESC "[1].StortDate", MAGIC_UNSET_INT);
  test_double(doc, TOP "." DESC "[1].Heigth", MAGIC_UNSET_DOUBLE);

  printf("Number ok: %d\n", n_ok);
  if (n_bad>0)
    printf("**** Not all tests passed!\n"
           "**** Number of failures: %d\n", n_bad);

  xmlFreeDoc(doc);
  xmlCleanupParser();
  unlink("test.xml");
  exit(1);
}

*/
