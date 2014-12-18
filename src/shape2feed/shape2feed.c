#include "asf_vector.h"
#include "asf.h"
#include "asf_meta.h"
#include <ctype.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include "dateUtil.h"

#define VERSION 1.0
#define KEY "AIzaSyCZQ5mlWKzQhNbvtiUA3-dM6TTEd1yFhag"

void usage(char *name)
{
  printf("\n"
   "USAGE:\n"
   "   %s <configFile> <outFile>\n", name);
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   configFile   Name of the feed configuration file\n"
   "   outFile      Name of the output data feed\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program converts a list of shapefiles into an ATOM feed.\n");
  printf("\n"
   "Version %.2f, ASF SAR Tools\n"
   "\n",VERSION);
  exit(EXIT_FAILURE);
}

int strmatches(const char *key, ...)
{
    va_list ap;
    char *arg = NULL;
    int found = FALSE;

    va_start(ap, key);
    do {
        arg = va_arg(ap, char *);
        if (arg) {
            if (strcmp(key, arg) == 0) {
                found = TRUE;
                break;
            }
        }
    } while (arg);

    return found;
}

static char *get_format(DBFHandle dbase)
{
  struct stat statbuf;
  struct dirent *dp;
  dbf_header_t *header;
  char file[1024], path[1024], fieldName[25], shape_type[25], *format=NULL;
  int ii, n, field_count, width, decimals, found=TRUE;

  sprintf(path, "%s%cdata_dictionaries", get_asf_share_dir(), DIR_SEPARATOR);
  DIR *dir = opendir(path);
  while ((dp = readdir(dir)) != NULL) {
    if (dp->d_name && strlen(dp->d_name)) {
      sprintf(file, "%s%c%s", path, DIR_SEPARATOR, dp->d_name);
      if (stat(file, &statbuf) < 0)
        continue;
      else if (S_ISREG(statbuf.st_mode) &&
        endsWith(file, "data_dictionary.csv")) {
        found = TRUE;
        format = (char *) MALLOC(sizeof(char)*25);
        strncpy_safe(format, dp->d_name, strlen(dp->d_name)-19);
        read_header_config(format, &header, &n, shape_type);
        field_count = DBFGetFieldCount(dbase);
        if (n != field_count)
          found = FALSE;
        else {
          for (ii=0; ii<field_count; ii++) {
            DBFGetFieldInfo(dbase, ii, fieldName, &width, &decimals);
            if (strcmp_case(fieldName, header[ii].shape) != 0)
              found = FALSE;
          }
        }
        if (found)
          return format;
        else {
          FREE(format);
          format = NULL;
        }
      }
    }
    else
      continue;
  }
  closedir(dir);

  return format;
}

static int strindex(char s[], char t[])
{
  int i, j, k;

  for (i=0; s[i]!='\0'; i++) {
    for (j=i, k=0; t[k]!='\0' && s[j]==t[k]; j++, k++)
      ;
    if (k>0 && t[k]=='\0')
      return i;
  }
  return -1;
}

static char *read_param(char *line)
{
  int i, k;
  char *value=(char *)CALLOC(256, sizeof(char));

  strcpy(value, "");
  i=strindex(line, "]");
  k=strindex(line, "=");
  if (i>0) strncpy(value, line, i+1);
  if (k>0) strncpy(value, line, k);
  return value;
}

static char *read_str(char *line, char *param)
{
  static char value[255];
  char *p = strchr(line, '=');

  // skip past the '=' sign, and eat up any whitespace
  ++p;
  while (isspace(*p))
      ++p;

  strcpy(value, p);

  // eat up trailing whitespace, too
  p = value + strlen(value) - 1;
  while (isspace(*p))
      *p-- = '\0';

  return value;
}

int main(int argc, char **argv)
{
  char configFile[512], outFile[512], title[255], self[512], alternate[512];
  char directory[255], shapeFile[512], timestamp[35], site[512];
  extern int currArg; // Pre-initialized to 1

  // Parse command line
  if ((argc-currArg) < 2) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }
  strcpy(configFile, argv[currArg]);
  strcpy(outFile, argv[currArg+1]);

  asfSplashScreen(argc, argv);
  
  // Get feed information from configuration file
  char line[512], params[50], *test;
  strcpy(params, "");
  FILE *fpConfig = FOPEN(configFile, "r");
  if (!fpConfig)
    asfPrintError("Could not read configuration file!\n");
  while (fgets(line, 512, fpConfig) != NULL) {
    if (strncmp_case(line, "[Feed]", 6) == 0) 
      strcpy(params, "feed");
    if (strcmp(params, "feed") == 0) {
      test = read_param(line);
      if (strncmp(test, "title", 5) == 0)
      	strcpy(title, read_str(line, "title"));
      if (strncmp(test, "xml", 3) == 0)
      	strcpy(self, read_str(line, "xml"));
      if (strncmp(test, "html", 4) == 0)
      	strcpy(alternate, read_str(line, "html"));
      FREE(test);
    }
  }
  FCLOSE(fpConfig);
  asfPrintStatus("Feed title: %s\n", title);
  asfPrintStatus("XML URL: %s\n", self);
  asfPrintStatus("HTML URL: %s\n", alternate);
  strncpy_safe(site, alternate, strlen(alternate)-9);
  asfPrintStatus("\nConverting shapefiles ...\n");

  // Create directory for HTML files and setup index file
  create_clean_dir(directory);
  char *index = (char *) MALLOC(sizeof(char)*(strlen(directory)+12));
  FILE *fpHtml;
  char *html = (char *) MALLOC(sizeof(char)*(strlen(directory)+100));
  sprintf(index, "%s%cindex.html", directory, DIR_SEPARATOR);
  FILE *fpIndex = FOPEN(index, "w");
  fprintf(fpIndex, "<!DOCTYPE html\n");
  fprintf(fpIndex, "  PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"\n");
  fprintf(fpIndex, "  \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
    "\n");
  fprintf(fpIndex, "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
    "xml:lang=\"en-us\">\n");
  fprintf(fpIndex, "<head>\n");
  fprintf(fpIndex, "<meta name=\"viewport\" content=\"width=480, "
    "initial-scale=1.0, maximum-scale=2.0, user-scalable=yes\"/>\n");
  fprintf(fpIndex, "<link media=\"screen\" href=\"../xhtml.css\" "
    "type=\"text/css\" rel=\"stylesheet\"/>\n"); 
  fprintf(fpIndex, "<link media=\"handheld, only screen and (max-width: "
    "480px), only screen and (max-device-width: 480px)\" "
    "href=\"../mobile.css\" type=\"text/css\" rel=\"stylesheet\"/>\n");
  fprintf(fpIndex, "<meta http-equiv=\"Content-Type\" "
    "content=\"application/xhtml+xml; charset=iso-8859-1\"/>\n");
  fprintf(fpIndex, "<title>%s</title>\n", title);
  fprintf(fpIndex, "</head>\n");
  fprintf(fpIndex, "<body>\n");
  fprintf(fpIndex, "<h1>%s</h1>\n", title);

  // Set up feed
  sprintf(timestamp, "%s", iso_date());
  FILE *fp = FOPEN(outFile, "w");
  fprintf(fp, "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
  fprintf(fp, "<?xml-stylesheet type=\"text/xsl\" "
    "href=\"feed2html.xsl\"?>\n");
  fprintf(fp, "<feed xmlns:atom=\"http://www.w3.org/2005/Atom\" "
    "xmlns:georss=\"http://www.georss.org/georss\" "
    "xmlns:gml=\"http://www.opengis.net/gml\">\n");
  fprintf(fp, "  <title>%s</title>\n", title);
  fprintf(fp, "  <link rel=\"self\" href=\"%s\"/>\n", self);
  fprintf(fp, "  <link rel=\"alternate\" type=\"text/html\" "
    "href=\"%s\"/>\n", alternate);
  fprintf(fp, "  <id>tag:asf.alaska.edu,%s</id>\n", title);
  fprintf(fp, "  <updated>%s</updated>\n", timestamp);

  // Read shapefile information
  dbf_header_t *header;
  DBFHandle dbase;
  SHPHandle shape;
  DBFFieldType fieldType;
  SHPObject *shapeObject = NULL;
  int ii, kk, n, width, decimals, nEntities, pointType, nVertices, nValue;
  int field_count, index;
  double fValue;
  char *sValue, fieldName[25], shape_type[25], granule[512];
  open_shape(shapeFile, &dbase, &shape);
  char *format = get_format(dbase);
  read_header_config(format, &header, &n, shape_type);
  SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);
  for (ii=0; ii<nEntities; ii++) {
    index = DBFGetFieldIndex(dbase, "GRANULE");
    sprintf(granule, "%s", DBFReadStringAttribute(dbase, ii, index));
    
    // Set up feed entry
    fprintf(fp, "  <entry>\n");
    fprintf(fp, "    <title>New in ASF datapool: %s</title>\n", granule);
    fprintf(fp, "    <link href=\"%s%s.html\"/>\n", site, granule);
    fprintf(fp, "    <id>tag:asf.alaska.edu,%s</id>\n", granule);
    fprintf(fp, "    <updated>%s</updated>\n", timestamp);
    fprintf(fp, "    <summary>A new granule (%s) has been added to the "
      "ASF datapool.</summary>\n", granule);
    fprintf(fp, "    <content type=\"html\">\n");
    fprintf(fp, "      <img src=\"http://maps.googleapis.com/maps/api/"
      "staticmap?size=300x300&amp;key=%s&amp;maptype=satellite&amp;path="
      "color:0xFFFF00FF|weight:3", KEY);

    // Add granule to HTML index file
    fprintf(fpIndex, "<a href=\"%s.html\">%s</a><br/>\n",
      granule, granule);

    // Set up HTML file for entry
    sprintf(html, "%s%c%s.html", directory, DIR_SEPARATOR, granule);
    fpHtml = FOPEN(html, "w");
    fprintf(fpHtml, "<!DOCTYPE html\n");
    fprintf(fpHtml, "  PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"\n");
    fprintf(fpHtml, "  \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
      "\n");
    fprintf(fpHtml, "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
      "xml:lang=\"en-us\">\n");
    fprintf(fpHtml, "<head>\n");
    fprintf(fpHtml, "<meta name=\"viewport\" content=\"width=480, "
      "initial-scale=1.0, maximum-scale=2.0, user-scalable=yes\"/>\n");
    fprintf(fpHtml, "<link media=\"screen\" href=\"../xhtml.css\" "
      "type=\"text/css\" rel=\"stylesheet\"/>\n"); 
    fprintf(fpHtml, "<link media=\"handheld, only screen and (max-width: "
      "480px), only screen and (max-device-width: 480px)\" "
      "href=\"../mobile.css\" type=\"text/css\" rel=\"stylesheet\"/>\n");
    fprintf(fpHtml, "<meta http-equiv=\"Content-Type\" "
      "content=\"application/xhtml+xml; charset=iso-8859-1\"/>\n");
    fprintf(fpHtml, "<title>%s - %s</title>\n", title, granule);
    fprintf(fpHtml, "</head>\n");
    fprintf(fpHtml, "<body>\n");
    fprintf(fpHtml, "<h1>%s</h1>\n", title);
    fprintf(fpHtml, "<h2>%s</h2>\n", granule);
    fprintf(fpHtml, "<div id=\"container\">\n");
    fprintf(fpHtml, "<div id=\"location\">\n");
    fprintf(fpHtml, "<div id=\"zoom\">\n");
    fprintf(fpHtml, "<img src=\"http://maps.googleapis.com/maps/api/"
      "staticmap?size=300x300&amp;key=%s&amp;maptype=satellite&amp;"
      "path=color:0xFFFF00FF|weight:3", KEY);
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices;
    for (kk=0; kk<nVertices; kk++) {
      fprintf(fp, "|%.5f,%.5f", 
        shapeObject->padfY[kk], shapeObject->padfX[kk]);
      fprintf(fpHtml, "|%.5f,%.5f", 
        shapeObject->padfY[kk], shapeObject->padfX[kk]);
    }
    fprintf(fpHtml, "\" width=\"300\" height=\"300\" alt="
      "\"location map\"/>\n");
    fprintf(fpHtml, "</div>\n<div id=\"overview\">");
    fprintf(fpHtml, "<img src=\"http://maps.googleapis.com/maps/api/"
      "staticmap?size=300x300&amp;key=%s&amp;maptype=satellite&amp;"
      "zoom=1&amp;path=color:0xFFFF00FF|weight:3", KEY);
    for (kk=0; kk<nVertices; kk++) {
      fprintf(fp, "|%.5f,%.5f", 
        shapeObject->padfY[kk], shapeObject->padfX[kk]);
      fprintf(fpHtml, "|%.5f,%.5f", 
        shapeObject->padfY[kk], shapeObject->padfX[kk]);
    }
    SHPDestroyObject(shapeObject);
    fprintf(fp, "\" width=\"300\" height=\"300\"/>\n");
    fprintf(fpHtml, "\" width=\"300\" height=\"300\" alt="
      "\"location map\"/>\n");
    fprintf(fpHtml, "</div>\n</div>\n");
    fprintf(fpHtml, "<div id=\"data\">\n");

    // Writing attributes into output file
    field_count = DBFGetFieldCount(dbase);
    for (kk=0; kk<field_count; kk++) {
      fieldType = 
        DBFGetFieldInfo(dbase, kk, fieldName, &width, &decimals);
      if (fieldType == FTString) {
        sValue = (char *) MALLOC(sizeof(char)*(width+1));
        strcpy(sValue, DBFReadStringAttribute(dbase, ii, kk));
        if (strcmp_case(fieldName, "URL") == 0) {
          fprintf(fp, "        <strong>%s: </strong><a href=\"%s\">%s</a>"
            "<br/>\n", header[kk].definition, sValue, sValue);
          fprintf(fpHtml, "<strong>%s: </strong><a href=\"%s\">%s</a>"
            "<br/>\n", header[kk].definition, sValue, sValue);
        }
        else {
          fprintf(fp, "        <strong>%s: </strong>%s<br/>\n",
            header[kk].definition, sValue);
          if (strcmp_case(fieldName, "GRANULE") != 0)
            fprintf(fpHtml, "<strong>%s: </strong>%s<br/>\n",
              header[kk].definition, sValue);
        }
        FREE(sValue);
      }
      else if (fieldType == FTInteger) {
        nValue = DBFReadIntegerAttribute(dbase, ii, kk);
        fprintf(fp, "        <strong>%s: </strong>%d<br/>\n", 
          header[kk].definition, nValue);
        fprintf(fpHtml, "<strong>%s: </strong>%d<br/>\n", 
          header[kk].definition, nValue);
      }
      else if (fieldType == FTDouble) {
        fValue = DBFReadDoubleAttribute(dbase, ii, kk);
        fprintf(fp, "        <strong>%s: </strong>%lf<br/>\n",
          header[kk].definition, fValue);
        fprintf(fpHtml, "<strong>%s: </strong>%lf<br/>\n",
          header[kk].definition, fValue);
      }
    }

    fprintf(fp, "    </content>\n");
    fprintf(fp, "    <author>\n");
    fprintf(fp, "      <name>Alaska Satellite Facility</name>\n");
    fprintf(fp, "      <email>uso@asf.alaska.edu</email>\n");
    fprintf(fp, "    </author>\n");
    fprintf(fp, "  </entry>\n");

    fprintf(fpHtml, "</div>\n</div>\n");
    fprintf(fpHtml, "</body>\n");
    fprintf(fpHtml, "</html>\n");      
    FCLOSE(fpHtml);
  }
  close_shape(dbase, shape);
  fprintf(fpIndex, "</body>\n");
  fprintf(fpIndex, "</html>\n");
  FCLOSE(fpIndex);
  FREE(index);
  FREE(html);
  
  // Close feed
  fprintf(fp, "</feed>\n");      
  FCLOSE(fp);

  return(0);
}
