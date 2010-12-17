#include "asf.h"
#include "fgdc_meta.h"
#include "asf_sar.h"
#include <ctype.h>

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

static char *read_para(FILE *fConfig, char *line, char *param)
{
  static char value[5000];
  char *p = strchr(line, '=');
  ++p;
  while (isspace(*p))
    ++p;

  // A paragraph requires quotation marks around it. Complain otherwise
  if (*p != '"')
    asfPrintError("This parameter (%s) is of type 'paragraph', which requires "
		  "quotation marks around it.\n", param);
  else
    ++p;

  // Read until the end of the buffer or a quotation mark
  int i=0;
  char c;
  while (*p != '\0' && *p != '"') {
    value[i] = *p;
    i++;
    ++p;
  }
  if (*p == '"') {
    value[i] = '\0';
    return value;
  }
  else
    do {
      c = fgetc(fConfig);
      if (c == '"') {
	value[i] = '\0';
	return value;
      }
      else {
	value[i] = c;
	i++;
      }
    } while (c != EOF);

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

static int read_int(char *line, char *param)
{
  char *tmp;
  int value;

  tmp = read_str(line, param);
  sscanf(tmp, "%i", &value);

  return value;
}

static double read_double(char *line, char *param)
{
  char *tmp;
  double value;

  tmp = read_str(line, param);
  sscanf(tmp, "%lf", &value);

  return value;
}

int init_fgdc_config(char *configFile, char *type)
{
  FILE *fCfg;
  int comment = FALSE;

  if (fileExists(configFile)) {
    asfPrintError("Cannot create file, %s, because it already exists.\n",
                  configFile);
  }
  fCfg = FOPEN(configFile, "w");
  if (strcmp_case(type, "long") == 0)
    comment = TRUE;

  fprintf(fCfg, "asf_fgdc_meta configuration file\n\n");

  // Identification Information
  fprintf(fCfg, "[Identification Information]\n");
  if (comment)
    fprintf(fCfg, "\n# Abstract -- a brief narrative summary of the data set."
	    "\n\n");
  fprintf(fCfg, "abstract = some text here\n");
  if (comment)
    fprintf(fCfg, "\n# Purpose -- a summary of the intentions with which the "
	    "data set was developed.\n\n");
  fprintf(fCfg, "purpose = whatever that is\n");
  if (comment)
    fprintf(fCfg, "\n# Supplemental Information -- other descriptive "
	    "information about the data set.\n\n");
  fprintf(fCfg, "supplement = \"\"\n");
  if (comment)
    fprintf(fCfg, "\n# Acquisition -- date and time of image acquisition. This"
	    " parameter will\n# overwrite all information that is extracted "
	    "from the data.\n"
	    "# Format: YYYY-MM-DD hh:mm:ss\n\n");
  fprintf(fCfg, "acquisition =\n");
  if (comment)
    fprintf(fCfg, "\n# Currentness Reference -- the basis on which the time\n"
	    "# period of content information is determined.\n# Values: "
	    "\"ground condition\" \"publication date\" free text\n\n");
  fprintf(fCfg, "currentness = ground condition\n");
  if (comment)
    fprintf(fCfg, "\n# Progress -- the state of the data set.\n"
	    "# Values: \"complete\" \"in work\" \"planned\"\n\n");
  fprintf(fCfg, "progress = complete\n");
  if (comment)
    fprintf(fCfg, "\n# Maintenance and Update Frequency -- the frequency with"
	  " which changes and\n# additions are made to the data set after the"
	  " initial data set is completed.\n"
	  "# Values: \"continually\" \"daily\" \"weekly\" \"monthly\""
	  " \"annually\" \"unknown\"\n# \"as needed\" \"irregular\""
	  " \"none planned\" free text\n\n");
  fprintf(fCfg, "update = none planned\n");
  if (comment)
    fprintf(fCfg, "\n# The complete name of the platform.\n\n");
  fprintf(fCfg, "platform = Radarsat-1\n");
  if (comment)
    fprintf(fCfg, "\n# The complete name of the instrument.\n\n");
  fprintf(fCfg, "instrument = SAR\n");
  if (comment)
    fprintf(fCfg, "\n# Access Constraints -- restrictions and legal "
	    "prerequisites for\n# accessing the data set. These include any "
	    "access constraints applied to\n# assure the protection of "
	    "privacy or intellectual property, and any special\n# restrictions"
	    " or limitations on obtaining the data set.\n"
	    "# Values: \"none\" free text\n\n");
  fprintf(fCfg, "access_constraints = \"\"\n");
  if (comment)
    fprintf(fCfg, "\n# Use Constraints -- restrictions and legal prerequisites"
	    "for using\n# the data set after access is granted. These include "
	    "any use constraints\n# applied to assure the protection of "
	    "privacy or intellectual property,\n# and any special restrictions"
	    " or limitations on using the data set.\n"
	    "# Values: \"none\" free text\n\n");
  fprintf(fCfg, "use_constraints = \"\"\n");
  if (comment)
    fprintf(fCfg, "\n# Copyright -- name of the copyright holder.\n"
	    "# This is a user defined field (not part of FGDC metadata "
	    "standard).\n\n");
  fprintf(fCfg, "copyright = Canadian Space Agency\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Organization -- the name of the organization to "
	    "which\n# the contact type applies.\n\n");
  fprintf(fCfg, "organization = Alaska Satellite Facility\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Position -- the title of individual (optional)."
	    "\n\n");
  fprintf(fCfg, "position = User Services Office\n");
  if (comment)
    fprintf(fCfg, "\n# Address Type -- the information provided by the "
	    "address.\n"
	    "# Values: \"mailing\" \"physical\" \"mailing and physical\", "
	    "free texts.\n\n");
  fprintf(fCfg, "address_type = mailing and physical\n");
  if (comment)
    fprintf(fCfg, "\n# Address -- an address line for the address.\n\n");
  fprintf(fCfg, "address_line = 903 Koyukuk Dr.\n");
  if (comment)
    fprintf(fCfg, "\n# City -- the city of the address.\n\n");
  fprintf(fCfg, "city = Fairbanks\n");
  if (comment)
    fprintf(fCfg, "\n# State or Province -- the state or province of the\n"
	    "# address.\n\n");
  fprintf(fCfg, "state = Alaska\n");
  if (comment)
    fprintf(fCfg, "\n# Postal Code -- the ZIP or other postal code of the\n"
	    "# address.\n\n");
  fprintf(fCfg, "postal = 99775-7320\n");
  if (comment)
    fprintf(fCfg, "\n# Country -- the country of the address (optional).\n\n");
  fprintf(fCfg, "country = USA\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Voice Telephone -- the telephone number by "
	    "which\n# individuals can speak to the organization or "
	    "individual (optional).\n\n");
  fprintf(fCfg, "phone = 907-474-6166\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Facsimile Telephone -- the telephone number of "
	    "a facsimile\n# machine of the organization or individual "
	    "(optional).\n\n");
  fprintf(fCfg, "fax = 907-474-2665\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Electronic Mail Address -- the address of the "
	    "electronic\n# mailbox of the organization or individual "
	    "(optional).\n\n");
  fprintf(fCfg, "email = uso@asf.alaska.edu\n");
  if (comment)
    fprintf(fCfg, "\n# Browse Graphic File Name -- name of a related graphic "
	    "file that provides\n# an illustration of the data set.\n\n");
  fprintf(fCfg, "browse_name = somewhere\n");
  if (comment)
    fprintf(fCfg, "\n# Browse Graphic File Description -- a text description "
	    "of the illustration.\n\n");
  fprintf(fCfg, "browse_description = some text here\n");
  if (comment)
    fprintf(fCfg, "\n# Browse Graphic File Type -- graphic file type of a "
	    "related graphic file.\n\n");
  fprintf(fCfg, "browse_type = JPEG\n");
  if (comment)
    fprintf(fCfg, "\n# Data Set Credit -- recognition of those who contributed"
	    " to the data set.\n\n");
  fprintf(fCfg, "data_credit = \"\"");
  if (comment)
    fprintf(fCfg, "\n# Security Classification System -- name of the "
	    "classification system.\n\n");
  fprintf(fCfg, "security_system = top secure\n");
  if (comment)
    fprintf(fCfg, "\n# Security Classification -- name of the handling "
	    "restrictions on the data set.\n"
	    "# Values: \"top secret\" \"secret\" \"confidential\" "
	    "\"restricted\" \"unclassified\"\n# \"sensitive\" free text\n\n");
  fprintf(fCfg, "security_classification = restricted\n");
  if (comment)
    fprintf(fCfg, "\n# Security Handling Description -- additional information"
	    " about the restrictions\n# on handling the data set.\n\n");
  fprintf(fCfg, "security_handling = yet another story\n\n\n");

  // Citation
  fprintf(fCfg, "[Citation]\n");
  if (comment)
    fprintf(fCfg, "\n# Originator -- the name of an organization or individual"
	    " that developed\n# the data set. If the name of editors or "
	    "compilers are provided, the name must\n# be followed by \"(ed.)\""
	    " or \"(comp.)\" respectively.\n\n");
  fprintf(fCfg, "originator = KSAT\n");
  if (comment)
    fprintf(fCfg, "\n# Publication Date -- the date when the data set is "
	    "published\n# or otherwise made available for release.\n"
	    "# Values: \"Unknown\" \"Unpublished material\" free date\n\n");
  fprintf(fCfg, "publication_date = unpublished material\n");
  if (comment)
    fprintf(fCfg, "\n# Title -- the name by which the data set is known.\n\n");
  fprintf(fCfg, "title = \"\"\n");
  if (comment)
    fprintf(fCfg, "\n# Online Linkage -- the name of an online computer "
	    "resource that\n# contains the data set. Entries should follow the"
	    " Uniform Resource Locator\n# convention of the Internet.\n\n");
  fprintf(fCfg, "online_link = some place on the web\n\n\n");

  // Processing Level
  fprintf(fCfg, "[Processing Level]\n");
  if (comment)
    fprintf(fCfg, "\n# Data distributor's code that identifies the level of "
	    "data processing\n# applied to the measurements, as defined in "
	    "Processing_Level_Authority.\n\n");
  fprintf(fCfg, "level = CEOS Level 0\n");
  if (comment)
    fprintf(fCfg, "\n# Originator -- the name of an organization or individual"
	    " that developed\n# the data set. If the name of editors or "
	    "compilers are provided, the name must\n# be followed by \"(ed.)\""
	    " or \"(comp.)\" respectively.\n\n");
  fprintf(fCfg, "originator = Some guys in Norway\n");
  if (comment)
    fprintf(fCfg, "\n# Publication Date -- the date when the data set is "
	    "published\n# or otherwise made available for release.\n"
	    "# Values: \"Unknown\" \"Unpublished material\" free date\n\n");
  fprintf(fCfg, "publication_date = unpublished material\n");
  if (comment)
    fprintf(fCfg, "\n# Title -- the name by which the data set is known.\n\n");
  fprintf(fCfg, "title = IPY Greenland data\n");
  if (comment)
    fprintf(fCfg, "\n# Geospatial Data Presentation Form -- the mode in which "
	    "the geospatial\n# data are represented.\n"
	    "# Values: \"atlas\" \"audio\" \"diagram\" \"document\" \"globe\""
	    " \"map\" \"model\"\n# \"multimedia presentation\" \"profile\" "
	    "\"raster digital data\"\n# \"remote-sensing image\" \"section\""
	    "\"spreadsheet\" \"tabular digital data\"\n# \"vector digital "
	    "data\" \"video\" \"view\" free text\n\n");
  fprintf(fCfg, "data_form = remote-sensing image\n\n\n");

  // Keywords
  fprintf(fCfg, "[Keywords]\n");
  if (comment)
    fprintf(fCfg, "\n# Number of themes\n\n");
  fprintf(fCfg, "theme_count = 1\n");
  if (comment)
    fprintf(fCfg, "\n# Theme Keyword Thesaurus -- reference to a formally "
	    "registered thesaurus\n# or a similar authoritative source of "
	    "theme keywords.\n"
	    "# Values: \"none\" free text\n\n");
  fprintf(fCfg, "theme_thesaurus = none\n");
  if (comment)
    fprintf(fCfg, "\n# Theme Keyword -- common-use word or phrase used to "
	    "describe the subject\n# of the data set.\n\n");
  fprintf(fCfg, "theme_key = theme 1\n");
  if (comment)
    fprintf(fCfg, "\n# Number of places\n\n");
  fprintf(fCfg, "place_count = 1\n");
  if (comment)
    fprintf(fCfg, "\n# Place Keyword Thesaurus -- reference to a formally "
	    "registered thesaurus\n# or a similar authoritative source of "
	    "place keywords.\n"
	    "# Values: \"none\" \"geographic names information system\" "
	    "free text\n\n");
  fprintf(fCfg, "place_thesaurus = none\n");
  if (comment)
    fprintf(fCfg, "\n# Place Keyword -- the geographic name of a location "
	    "covered by a data set.on.\n\n");
  fprintf(fCfg, "place_key = place 1\n\n\n");

  // Data Quality Information
  fprintf(fCfg, "[Data Quality Information]\n");
  if (comment)
    fprintf(fCfg, "\n# Attribute Accuracy Report -- an explanation of the "
	    "accuracy of the\n# identification of the entities and assignments"
	    " of values in the data set and\n# a description of the tests "
	    "used. (optional)\n\n");
  fprintf(fCfg, "attribute_accuracy = \"\"\n");
  if (comment)
    fprintf(fCfg, "\n# Logical Consistency Report -- an explanation of the "
	    "fidelity of\n# relationships in the data set and tests used."
	    "\n\n");
  fprintf(fCfg, "logical_consistency = \"\"\n");
  if (comment)
    fprintf(fCfg, "\n# Completeness Report -- information about omissions, "
	    "selection criteria,\n# generalization, definitions used, and "
	    "other rules used to derive the data set.\n\n");
  fprintf(fCfg, "completeness = completely\n");
  if (comment)
    fprintf(fCfg, "\n# Horizontal Positional Accuracy Report -- an explanation"
	    " of the accuracy\n# of the horizontal coordinate measurements and"
	    " a description of the tests used.\n\n");
  fprintf(fCfg, "horizontal_accuracy = \"\"\n");
  if (comment)
    fprintf(fCfg, "\n# Vertical Positional Accuracy Report -- an explanation "
	    "of the accuracy\n# of the vertical coordinate measurements and "
	    "a description of the tests used.\n\n");
  fprintf(fCfg, "vertical_accuracy = \"\"\n\n\n");

  // Lineage
  fprintf(fCfg, "[Lineage]\n");
  if (comment)
    fprintf(fCfg, "\n# Number of sources.\n\n");
  fprintf(fCfg, "source_count = 0\n");
  if (comment)
    fprintf(fCfg, "\n# Originator -- the name of an organization or individual"
	    " that developed\n# the data set. If the name of editors or "
	    "compilers are provided, the name must\n# be followed by \"(ed.)\""
	    " or \"(comp.)\" respectively.\n\n");
  fprintf(fCfg, "originator = \n");
  if (comment)
    fprintf(fCfg, "\n# Publication Date -- the date when the data set is "
	    "published\n# or otherwise made available for release.\n"
	    "# Values: \"Unknown\" \"Unpublished material\" free date\n\n");
  fprintf(fCfg, "publication_date = \n");
  if (comment)
    fprintf(fCfg, "\n# Title -- the name by which the data set is known.\n\n");
  fprintf(fCfg, "title = \n");
  if (comment)
    fprintf(fCfg, "\n# Geospatial Data Presentation Form -- the mode in which "
	    "the geospatial data\n# are represented.\n"
	    "# Values: \"atlas\" \"audio\" \"diagram\" \"document\" \"globe\" "
	    "\"map\" \"model\"\n# \"multimedia presentation\" \"profile\" "
	    "\"raster digital data\"\n# \"remote-sensing image\" \"section\" "
	    "\"spreadsheet\" \"tabular digital data\"\n# \"vector digital "
	    "data\" \"video\" \"view\" free text\n\n");
  fprintf(fCfg, "data_form = \n");
  if (comment)
    fprintf(fCfg, "\n# Type of Source Media -- the medium of the source data "
	    "set.\n"
	    "# Values: \"paper\" \"stable-base material\" \"microfiche\" "
	    "\"microfilm\"\n# \"audiocassette\" \"chart\" \"filmstrip\" "
	    "\"transparency\" \"videocassette\"\n# \"videodisc\" \"videotape\" "
	    "\"physical model\" \"computer program\" \"disc\"\n# "
	    "\"cartridge tape\"\"magnetic tape\" \"online\" \"CD-ROM\"\n# "
	    "\"electronic bulletin board\" \"electronic mail system\" "
	    "free text\n\n");
  fprintf(fCfg, "media_type = \n");
  if (comment)
    fprintf(fCfg, "\n# Source Time Period of Content -- time period(s) for "
	    "which the source data set\ncorresponds to the ground.\n\n");
  fprintf(fCfg, "time_period = \n");
  if (comment)
    fprintf(fCfg, "\n# Source Currentness Reference -- the basis on which the "
	    "source time period of\n# content information of the source data "
	    "set is determined.\n"
	    "Values: \"ground condition\" \"publication date\" free text\n\n");
  fprintf(fCfg, "currentness = \n");
  if (comment)
    fprintf(fCfg, "\n# Source Citation Abbreviation -- short-form alias for "
	    "the source citation.\n\n");
  fprintf(fCfg, "cite_abbreviation = \n");
  if (comment)
    fprintf(fCfg, "\n# Source Contribution -- brief statement identifying the "
	    "information\n# contributed by the source to the data set.\n\n");
  fprintf(fCfg, "contribution = \n");
  if (comment)
    fprintf(fCfg, "\n# Number of processing steps.\n\n");
  fprintf(fCfg, "processing_step_count = 0\n");
  if (comment)
    fprintf(fCfg, "\n# Process Description -- an explanation of the event and "
	    "related parameters or\n# tolerances.\n\n");
  fprintf(fCfg, "description = \n");
  if (comment)
    fprintf(fCfg, "\n# Contact Person -- the name of the individual to which "
	    "the contact type\napplies.\n\n");
  fprintf(fCfg, "contact = \n");
  if (comment)
    fprintf(fCfg, "\n# Contact Organization -- the name of the organization to "
	    "which\n# the contact type applies.\n\n");
  fprintf(fCfg, "organization = \n");
  if (comment)
    fprintf(fCfg, "\n# Contact Position -- the title of individual (optional)."
	    "\n\n");
  fprintf(fCfg, "position = \n");
  if (comment)
    fprintf(fCfg, "\n# Address Type -- the information provided by the "
	    "address.\n"
	    "# Values: \"mailing\" \"physical\" \"mailing and physical\", "
	    "free texts.\n\n");
  fprintf(fCfg, "address_type = \n");
  if (comment)
    fprintf(fCfg, "\n# Address -- an address line for the address.\n\n");
  fprintf(fCfg, "address_line = 903 Koyukuk Dr.\n");
  if (comment)
    fprintf(fCfg, "\n# City -- the city of the address.\n\n");
  fprintf(fCfg, "city = \n");
  if (comment)
    fprintf(fCfg, "\n# State or Province -- the state or province of the\n"
	    "# address.\n\n");
  fprintf(fCfg, "state = \n");
  if (comment)
    fprintf(fCfg, "\n# Postal Code -- the ZIP or other postal code of the\n"
	    "# address.\n\n");
  fprintf(fCfg, "postal = \n");
  if (comment)
    fprintf(fCfg, "\n# Country -- the country of the address (optional).\n\n");
  fprintf(fCfg, "country = \n");
  if (comment)
    fprintf(fCfg, "\n# Contact Voice Telephone -- the telephone number by "
	    "which\n# individuals can speak to the organization or "
	    "individual (optional).\n\n");
  fprintf(fCfg, "phone = 907-474-6166\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Facsimile Telephone -- the telephone number of "
	    "a facsimile\n# machine of the organization or individual "
	    "(optional).\n\n");
  fprintf(fCfg, "fax = 907-474-2665\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Electronic Mail Address -- the address of the "
	    "electronic\n# mailbox of the organization or individual "
	    "(optional).\n\n");
  fprintf(fCfg, "email = \n\n\n");


  // Spatial Data Organization Information
  fprintf(fCfg, "[Spatial Data Organization Information]\n");
  if (comment)
    fprintf(fCfg, "\n# Direct Spatial Reference Method -- the system of "
	    "objects used\n# to represent space in the data set.\n"
	    "# Values: \"Point\" \"Vector\" \"Raster\"\n\n");
  fprintf(fCfg, "spatial_reference = raster\n");
  if (comment)
    fprintf(fCfg, "\n# Raster Object Type -- raster spatial objects used to "
	    "locate zero-,\n# two-, or three-dimensional locations in the data"
	    " set.\n# Values: \"Point\" \"Pixel\" \"Grid Cell\" \"Voxel\"\n\n");
  fprintf(fCfg, "raster_type = pixel\n\n\n");

  // Spatial Reference Information
  fprintf(fCfg, "[Spatial Reference Information]\n");
  if (comment)
    fprintf(fCfg, "\n# Planar Coordinate Encoding Method -- the means used to "
	    "represent\n# horizontal positions.\n"
	    "# \"coordinate pair\" \"distance and bearing\" \"row and column\""
	    "\n\n");
  fprintf(fCfg, "encoding = row and column\n");
  if (comment)
    fprintf(fCfg, "\n# The point in the pixel corresponding to the earth "
	    "location\n# of the pixel.\n"
	    "# Values: \"center\" \"lower left corner\" \"lower right corner\""
	    "\n# \"upper left corner\" \"upper right corner\" free text\n\n");
  fprintf(fCfg, "point_position = center\n");
  if (comment)
    fprintf(fCfg, "\n# Description of which index varies most rapidly in the "
	    "sequential storage\n# of raster elements - row index (row major) "
	    "or column index (column major).\n"
	    "# Values: \"row major\" \"column major\" free text\n\n");
  fprintf(fCfg, "storage_order = row major\n\n\n");

  // Distribution Information
  fprintf(fCfg, "[Distribution Information]\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Organization -- the name of the organization to "
	    "which\n# the contact type applies.\n\n");
  fprintf(fCfg, "organization = Alaska Satellite Facility\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Position -- the title of individual (optional)."
	    "\n\n");
  fprintf(fCfg, "position = User Services Office\n");
  if (comment)
    fprintf(fCfg, "\n# Address Type -- the information provided by the "
	    "address.\n"
	    "# Values: \"mailing\" \"physical\" \"mailing and physical\", "
	    "free texts.\n\n");
  fprintf(fCfg, "address_type = mailing and physical\n");
  if (comment)
    fprintf(fCfg, "\n# Address -- an address line for the address.\n\n");
  fprintf(fCfg, "address_line = 903 Koyukuk Dr.\n");
  if (comment)
    fprintf(fCfg, "\n# City -- the city of the address.\n\n");
  fprintf(fCfg, "city = Fairbanks\n");
  if (comment)
    fprintf(fCfg, "\n# State or Province -- the state or province of the\n"
	    "# address.\n\n");
  fprintf(fCfg, "state = Alaska\n");
  if (comment)
    fprintf(fCfg, "\n# Postal Code -- the ZIP or other postal code of the\n"
	    "# address.\n\n");
  fprintf(fCfg, "postal = 99775-7320\n");
  if (comment)
    fprintf(fCfg, "\n# Country -- the country of the address (optional).\n\n");
  fprintf(fCfg, "country = USA\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Voice Telephone -- the telephone number by "
	    "which\n# individuals can speak to the organization or "
	    "individual (optional).\n\n");
  fprintf(fCfg, "phone = 907-474-6166\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Facsimile Telephone -- the telephone number of "
	    "a facsimile\n# machine of the organization or individual "
	    "(optional).\n\n");
  fprintf(fCfg, "fax = 907-474-2665\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Electronic Mail Address -- the address of the "
	    "electronic\n# mailbox of the organization or individual "
	    "(optional).\n\n");
  fprintf(fCfg, "email = uso@asf.alaska.edu\n");
  if (comment)
    fprintf(fCfg, "\n# Distribution Liability -- statement of the liability "
	    "assumed\n# by the distributor.\n\n");
  fprintf(fCfg, "liability = \n");
  if (comment)
    fprintf(fCfg, "\n# Network Resource Name -- the name of the file or service "
	    "from which\n# the data set can be obtained.\n\n");
  fprintf(fCfg, "network_path = somewhere on the net\n");
  if (comment)
    fprintf(fCfg, "\n# Fees -- the fees and terms for retrieving the data set."
	    "\n\n");
  fprintf(fCfg, "fees = big bugs\n\n\n");

  // Metadata Reference Information
  fprintf(fCfg, "[Metadata Reference Information]\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Organization -- the name of the organization to "
	    "which\n# the contact type applies.\n\n");
  fprintf(fCfg, "organization = Alaska Satellite Facility\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Position -- the title of individual (optional)."
	    "\n\n");
  fprintf(fCfg, "position =\n");
  if (comment)
    fprintf(fCfg, "\n# Address Type -- the information provided by the "
	    "address.\n"
	    "# Values: \"mailing\" \"physical\" \"mailing and physical\", "
	    "free texts.\n\n");
  fprintf(fCfg, "address_type = mailing and physical\n");
  if (comment)
    fprintf(fCfg, "\n# Address -- an address line for the address.\n\n");
  fprintf(fCfg, "address_line = 903 Koyukuk Dr.\n");
  if (comment)
    fprintf(fCfg, "\n# City -- the city of the address.\n\n");
  fprintf(fCfg, "city = Fairbanks\n");
  if (comment)
    fprintf(fCfg, "\n# State or Province -- the state or province of the\n"
	    "# address.\n\n");
  fprintf(fCfg, "state = Alaska\n");
  if (comment)
    fprintf(fCfg, "\n# Postal Code -- the ZIP or other postal code of the\n"
	    "# address.\n\n");
  fprintf(fCfg, "postal = 99775-7320\n");
  if (comment)
    fprintf(fCfg, "\n# Country -- the country of the address (optional).\n\n");
  fprintf(fCfg, "country = USA\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Voice Telephone -- the telephone number by "
	    "which\n# individuals can speak to the organization or "
	    "individual (optional).\n\n");
  fprintf(fCfg, "phone = 907-474-6166\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Facsimile Telephone -- the telephone number of "
	    "a facsimile\n# machine of the organization or individual "
	    "(optional).\n\n");
  fprintf(fCfg, "fax = 907-474-2665\n");
  if (comment)
    fprintf(fCfg, "\n# Contact Electronic Mail Address -- the address of the "
	    "electronic\n# mailbox of the organization or individual "
	    "(optional).\n\n");
  fprintf(fCfg, "email = uso@asf.alaska.edu\n");
  if (comment)
    fprintf(fCfg, "\n# Metadata Standard Name -- the name of the metadata "
	    "standard\n# used to document the data set.\n\n");
  fprintf(fCfg, "standard_name = FGDC Content Standard for Digital "
	  "Geospatial Metadata\n");
  if (comment)
    fprintf(fCfg, "\n# Metadata Standard Version -- identification of the "
	    " version\n# of the metadata standard used to document the data "
	    "set.\n\n");
  fprintf(fCfg, "standard_version = FGDC-STD-001-1998\n");
  if (comment)
    fprintf(fCfg, "\n# Profile Name -- the name given to a document that "
	    "describes\n# the application of the Standard to a specific user "
	    "community.\n\n");
  fprintf(fCfg, "profile_name = Extensions for Remote Sensing Metadata "
	  "(FGDC-STD-012-2002)\n\n");
  
  FCLOSE(fCfg);

  asfPrintStatus("   Initialized configuration file\n\n");

  return(0);
}

void update_fgdc_meta(fgdc_meta *fgdc, char *configFile)
{
  FILE *fConfig;
  char line[512], params[50];
  char *test;
  int ii, kk, theme_count, theme_key_count, place_count, place_key_count;

  strcpy(params, "");
  fConfig = fopen(configFile, "r");
  if (!fConfig) return;
  while (fgets(line, 512, fConfig) != NULL) {

    if (strncmp(line, "[Identification Information]", 28)==0) 
      strcpy(params, "Identification");
    if (strncmp(params, "Identification", 14)==0) {
      test = read_param(line);
      if (strncmp(test, "abstract", 8)==0) 
	strcpy(fgdc->abstract, read_para(fConfig, line, "abstract"));
      if (strncmp(test, "purpose", 7)==0) 
        strcpy(fgdc->purpose, read_para(fConfig, line, "purpose"));
      if (strncmp(test, "supplement", 10)==0) {
	fgdc->supplinf = (char *) MALLOC(sizeof(char)*5000);
	strcpy(fgdc->supplinf, read_para(fConfig, line, "supplement"));
      }
      if (strncmp(test, "acquisition", 11)==0 &&
	  strlen(read_str(line, "acquisition")) > 1) {
	fgdc->center_time = (char *) MALLOC(sizeof(char)*30);
        strcpy(fgdc->center_time, read_str(line, "acquisition"));
      }
      if (strncmp(test, "begin", 5) == 0 &&
	  strlen(read_str(line, "begin")) > 1) {
	strcpy(fgdc->start_time, read_str(line, "begin"));
      }
      if (strncmp(test, "end", 3) == 0 &&
	  strlen(read_str(line, "end")) > 1) {
	strcpy(fgdc->end_time, read_str(line, "end"));
      }
      if (strncmp(test, "currentness", 11)==0)
        strcpy(fgdc->current, read_str(line, "currentness"));
      if (strncmp(test, "progress", 8)==0)
        strcpy(fgdc->progress, read_str(line, "progress"));
      if (strncmp(test, "update", 6)==0)
        strcpy(fgdc->update, read_str(line, "update"));
      if (strncmp(test, "platform", 8)==0)
        strcpy(fgdc->platflnm, read_str(line, "platform"));
      if (strncmp(test, "instrument", 10)==0)
        strcpy(fgdc->instflnm, read_str(line, "instrument"));
      if (strncmp(test, "access_constraints", 18)==0)
        strcpy(fgdc->accconst, read_para(fConfig, line, "access_constraints"));
      if (strncmp(test, "use_constraints", 15)==0)
        strcpy(fgdc->useconst, read_para(fConfig, line, "use_constraints"));
      if (strncmp(test, "copyright", 9)==0 &&
	  strlen(read_str(line, "copyright")) > 1) {
	fgdc->copyright = (char *) MALLOC(sizeof(char)*100);
        strcpy(fgdc->copyright, read_str(line, "copyright"));
      }
      if (strncmp(test, "contact", 7)==0) {
        strcpy(fgdc->ptcontac.cntper, read_str(line, "contact"));
      }
      if (strncmp(test, "organization", 12)==0)
	strcpy(fgdc->ptcontac.cntorg, read_str(line, "organization"));
      if (strncmp(test, "position", 8)==0 &&
	  strlen(read_str(line, "position")) > 0) {
	fgdc->ptcontac.cntpos = (char *) MALLOC(sizeof(char)*255);
        strcpy(fgdc->ptcontac.cntpos, read_str(line, "position"));
      }
      if (strncmp(test, "address_type", 12)==0)
        strcpy(fgdc->ptcontac.addrtype, read_str(line, "address_type"));
      if (strncmp(test, "address_line", 12)==0)
        strcpy(fgdc->ptcontac.address, read_str(line, "address_line"));
      if (strncmp(test, "city", 4)==0)
        strcpy(fgdc->ptcontac.city, read_str(line, "city"));
      if (strncmp(test, "state", 5)==0)
        strcpy(fgdc->ptcontac.state, read_str(line, "state"));
      if (strncmp(test, "postal", 6)==0)
        strcpy(fgdc->ptcontac.postal, read_str(line, "postal"));
      if (strncmp(test, "country", 7)==0 &&
	  strlen(read_str(line, "country")) > 0) {
	fgdc->ptcontac.country = (char *) MALLOC(sizeof(char)*255);
        strcpy(fgdc->ptcontac.country, read_str(line, "country"));
      }
      if (strncmp(test, "phone", 5)==0 &&
	  strlen(read_str(line, "phone")) > 0) {
	fgdc->ptcontac.cntvoice = (char *) MALLOC(sizeof(char)*100);
        strcpy(fgdc->ptcontac.cntvoice, read_str(line, "phone"));
      }
      if (strncmp(test, "fax", 3)==0 &&
	  strlen(read_str(line, "fax")) > 0) {
	fgdc->ptcontac.cntfax = (char *) MALLOC(sizeof(char)*100);
        strcpy(fgdc->ptcontac.cntfax, read_str(line, "fax"));
      }
      if (strncmp(test, "email", 5)==0 &&
	  strlen(read_str(line, "email")) > 0) {
	fgdc->ptcontac.cntemail = (char *) MALLOC(sizeof(char)*100);
        strcpy(fgdc->ptcontac.cntemail, read_str(line, "email"));
      }
      if (strncmp(test, "browse_name", 11)==0 &&
	  strlen(read_str(line, "browse_name")) > 1) {
	fgdc->browse = (browseinfo *) MALLOC(sizeof(browseinfo));
        strcpy(fgdc->browse->browsen, read_str(line, "browse_name"));
      }
      if (strncmp(test, "browse_description", 18)==0 &&
	  strlen(read_str(line, "browse_description")) > 1)
        strcpy(fgdc->browse->browsed, read_str(line, "browse_description"));
      if (strncmp(test, "browse_type", 11)==0 &&
	  strlen(read_str(line, "browse_type")) > 1)
        strcpy(fgdc->browse->browset, read_str(line, "browse_type"));
      if (strncmp(test, "data_credit", 11)==0 &&
	  strlen(read_str(line, "data_credit")) > 3) {
	fgdc->datacred = (char *) MALLOC(sizeof(char)*5000);
        strcpy(fgdc->datacred, read_para(fConfig, line, "data_credit"));
      }
      if (strncmp(test, "security_system", 15)==0 &&
	  strlen(read_str(line, "security_system")) > 1) {
	fgdc->security = (securityinfo *) MALLOC(sizeof(securityinfo));
        strcpy(fgdc->security->secsys, read_str(line, "security_system"));
      }
      if (strncmp(test, "security_classification", 23)==0)
        strcpy(fgdc->security->secclass, 
	       read_str(line, "security_classification"));
      if (strncmp(test, "security_handling", 17)==0 &&
	  strlen(read_str(line, "security_handling")) > 1)
        strcpy(fgdc->security->sechandl, read_str(line, "security_handling"));
      //FREE(test);
    }

    if (strncmp(line, "[Citation]", 10)==0) 
      strcpy(params, "Citation");
    if (strncmp(params, "Citation", 7)==0) {
      test = read_param(line);
      if (strncmp(test, "originator", 10)==0)
        strcpy(fgdc->citation.origin, read_str(line, "originator"));
      if (strncmp(test, "publication_date", 16)==0)
        strcpy(fgdc->citation.pubdate, read_str(line, "publication_date"));
      if (strncmp(test, "title", 5)==0)
        strcpy(fgdc->citation.title, read_para(fConfig, line, "title"));
      if (strncmp(test, "online_link", 11)==0)
        strcpy(fgdc->citation.onlink, read_str(line, "online_link"));
      //FREE(test);
    }

    if (strncmp(line, "[Processing Level]", 18)==0) 
      strcpy(params, "Processing");
    if (strncmp(params, "Processing", 7)==0) {
      test = read_param(line);
      if (strncmp(test, "level", 5)==0)
        strcpy(fgdc->prolevid, read_str(line, "level"));
      if (strncmp(test, "originator", 10)==0)
        strcpy(fgdc->prolevau.origin, read_str(line, "originator"));
      if (strncmp(test, "publication_date", 16)==0)
        strcpy(fgdc->prolevau.pubdate, read_str(line, "publication_date"));
      if (strncmp(test, "title", 5)==0)
        strcpy(fgdc->prolevau.title, read_str(line, "title"));
      if (strncmp(test, "data_form", 9)==0)
        strcpy(fgdc->prolevau.geoform, read_str(line, "data_form"));
      //FREE(test);
    }

    if (strncmp(line, "[Keywords]", 10)==0) 
      strcpy(params, "Keywords");
    if (strncmp(params, "Keywords", 8)==0) {
      test = read_param(line);
      if (strncmp(test, "theme_count", 11) == 0) {
	theme_count = read_int(line, "theme_count");
	fgdc->keywords.theme_count  = theme_count;
	fgdc->keywords.theme = 
	  (keyinfo *) MALLOC(sizeof(keyinfo)*theme_count);
	for (ii=0; ii<theme_count; ii++) {
	  fgets(line, 255, fConfig); 
	  strcpy(fgdc->keywords.theme[ii].thesaurus, 
		 read_str(line, "theme_thesaurus"));
	  fgets(line, 255, fConfig); 
	  theme_key_count = read_int(line, "theme_key_count");
	  fgdc->keywords.theme[ii].key_count = theme_key_count;
	  fgdc->keywords.theme[ii].key = 
	    (char **) MALLOC(sizeof(char *)*theme_key_count);
	  for (kk=0; kk<theme_key_count; kk++) {
	    fgdc->keywords.theme[ii].key[kk] = 
	      (char *) MALLOC(sizeof(char)*100);
	    fgets(line, 255, fConfig);
	    strcpy(fgdc->keywords.theme[ii].key[kk], 
		   read_str(line, "theme_key"));
	  }
	}
      }
      if (strncmp(test, "place_count", 11) == 0) {
	place_count = read_int(line, "place_count");
	fgdc->keywords.place_count  = place_count;
	for (ii=0; ii<place_count; ii++) {
	  fgdc->keywords.place = 
	    (keyinfo *) MALLOC(sizeof(keyinfo)*place_count);
	  fgets(line, 255, fConfig); 
	  strcpy(fgdc->keywords.place[ii].thesaurus, 
		 read_str(line, "place_thesaurus"));
	  fgets(line, 255, fConfig); 
	  place_key_count = read_int(line, "place_key_count");
	  fgdc->keywords.place[ii].key_count = place_key_count;
	  fgdc->keywords.place[ii].key = 
	    (char **) MALLOC(sizeof(char *)*place_key_count);
	  for (kk=0; kk<place_key_count; kk++) {
	    fgdc->keywords.place[ii].key[kk] = 
	      (char *) MALLOC(sizeof(char)*100);
	    fgets(line, 255, fConfig);
	    strcpy(fgdc->keywords.place[ii].key[kk], 
		   read_str(line, "place_key"));
	  }
	}
      }
      //FREE(test);
    }

    if (strncmp(line, "[Spatial Reference Information]", 31)==0) 
      strcpy(params, "Spatial");
    if (strncmp(params, "Spatial", 7)==0) {
      test = read_param(line);
      if (strncmp(test, "encoding", 8)==0)
        strcpy(fgdc->plance, read_str(line, "encoding"));
      if (strncmp(test, "point_position", 14)==0)
        strcpy(fgdc->ptpos, read_str(line, "point_position"));
      if (strncmp(test, "storage_order", 13)==0)
        strcpy(fgdc->storord, read_str(line, "storage_order"));
      //FREE(test);
    }
    
    if (strncmp(line, "[Data Quality Information]", 26)==0) 
      strcpy(params, "Quality");
    if (strncmp(params, "Quality", 7)==0) {
      test = read_param(line);
      if (strncmp(test, "attribute_accuracy", 18)==0 &&
	  strlen(read_str(line, "attribute_accuracy")) > 3) {
	fgdc->attraccr = (char *) MALLOC(sizeof(char)*1000);
        strcpy(fgdc->attraccr, read_para(fConfig, line, "attribute_accuracy"));
      }
      if (strncmp(test, "logical_consistency", 19)==0)
        strcpy(fgdc->logic, read_para(fConfig, line, "logical_consistency"));
      if (strncmp(test, "completeness", 12)==0)
        strcpy(fgdc->complete, read_str(line, "completeness"));
      if (strncmp(test, "horizontal_accuracy", 19)==0 &&
	  strlen(read_str(line, "horizontal_accuracy")) > 3) {
	fgdc->horizpar = (char *) MALLOC(sizeof(char)*1000);
        strcpy(fgdc->horizpar, read_para(fConfig, line, "horizontal_accuracy"));
      }
      if (strncmp(test, "vertical_accuracy", 17)==0 &&
	  strlen(read_str(line, "vertical_accuracy")) > 3) {
	fgdc->vertaccr = (char *) MALLOC(sizeof(char)*1000);
        strcpy(fgdc->vertaccr, read_para(fConfig, line, "vertical_accuracy"));
      }
      //FREE(test);
    }
    
    if (strncmp(line, "[Lineage]", 9)==0) 
      strcpy(params, "Lineage");
    if (strncmp(params, "Lineage", 7)==0) {
      test = read_param(line);
      if (strncmp(test, "source_count", 12) == 0) {
	fgdc->source_count = read_int(line, "source_count");
	for (ii=0; ii<fgdc->source_count; ii++) {
	  fgdc->srcinfo = 
	    (sourceinfo *) MALLOC(sizeof(sourceinfo)*fgdc->source_count);
	  fgets(line, 255, fConfig); 
	  strcpy(fgdc->srcinfo[ii].srccite.origin, 
		 read_str(line, "originator"));
	  fgets(line, 255, fConfig); 
	  strcpy(fgdc->srcinfo[ii].srccite.pubdate, 
		 read_str(line, "publication_date"));
	  fgets(line, 255, fConfig); 
	  strcpy(fgdc->srcinfo[ii].srccite.title, 
		 read_str(line, "title"));
	  fgets(line, 255, fConfig); 
	  strcpy(fgdc->srcinfo[ii].srccite.geoform, 
		 read_str(line, "data_form"));
	  fgets(line, 255, fConfig); 
	  strcpy(fgdc->srcinfo[ii].typesrc, read_str(line, "media_type"));
	  fgets(line, 255, fConfig); 
	  strcpy(fgdc->srcinfo[ii].srctime, read_str(line, "time_period"));
	  fgets(line, 255, fConfig); 
	  strcpy(fgdc->srcinfo[ii].srccurr, read_str(line, "currentness"));
	  fgets(line, 255, fConfig); 
	  strcpy(fgdc->srcinfo[ii].srccitea, 
		 read_str(line, "cite_abbreviation"));
	  fgets(line, 255, fConfig); 
	  strcpy(fgdc->srcinfo[ii].srccontr, 
		 read_para(fConfig, line, "contribution"));
	}
      }
      if (strncmp(test, "processing_step_count", 21) == 0) {
	fgdc->process_count = read_int(line, "processing_step_count");
	for (ii=0; ii<fgdc->process_count; ii++) {
	  fgdc->procstep = 
	    (processinfo *) MALLOC(sizeof(processinfo)*fgdc->process_count);
	  fgets(line, 255, fConfig);
	  strcpy(fgdc->procstep[ii].procdesc, 
		 read_para(fConfig, line, "description"));
	  fgets(line, 255, fConfig);
	  fgets(line, 255, fConfig);
	  strcpy(fgdc->procstep[ii].procdate, 
		 read_str(line, "date"));
	  fgets(line, 255, fConfig);
	  strcpy(fgdc->procstep[ii].proccont.cntper, 
		 read_str(line, "contact"));
	  fgets(line, 255, fConfig);
	  strcpy(fgdc->procstep[ii].proccont.cntorg, 
		 read_str(line, "organization"));
	  fgets(line, 255, fConfig);
	  fgdc->procstep[ii].proccont.cntpos = 
	      (char *) MALLOC(sizeof(char)*100);
	  strcpy(fgdc->procstep[ii].proccont.cntpos, 
		 read_str(line, "position"));
	  fgets(line, 255, fConfig);
	  strcpy(fgdc->procstep[ii].proccont.addrtype,
		 read_str(line, "address_type"));
	  fgets(line, 255, fConfig);
	  strcpy(fgdc->procstep[ii].proccont.address,
		 read_str(line, "address_line"));
	  fgets(line, 255, fConfig);
	  strcpy(fgdc->procstep[ii].proccont.city, read_str(line, "city"));
	  fgets(line, 255, fConfig);
	  strcpy(fgdc->procstep[ii].proccont.state, 
		 read_str(line, "state"));
	  fgets(line, 255, fConfig);
	  strcpy(fgdc->procstep[ii].proccont.postal, 
		 read_str(line, "postal"));
	  fgets(line, 255, fConfig);
	  fgdc->procstep[ii].proccont.country = 
	    (char *) MALLOC(sizeof(char)*100);
	  strcpy(fgdc->procstep[ii].proccont.country, 
		 read_str(line, "country"));
	  fgets(line, 255, fConfig);
	  fgdc->procstep[ii].proccont.cntvoice = 
	    (char *) MALLOC(sizeof(char)*100);
	  strcpy(fgdc->procstep[ii].proccont.cntvoice, 
		   read_str(line, "phone"));
	  fgets(line, 255, fConfig);
	  fgdc->procstep[ii].proccont.cntfax = 
	    (char *) MALLOC(sizeof(char)*100);
	  strcpy(fgdc->procstep[ii].proccont.cntfax, 
		 read_str(line, "fax"));
	  fgets(line, 255, fConfig);
	  fgdc->procstep[ii].proccont.cntemail = 
	    (char *) MALLOC(sizeof(char)*100);
	  strcpy(fgdc->procstep[ii].proccont.cntemail, 
		 read_str(line, "email"));
	}
      }
      //FREE(test);
    }
    
    if (strncmp(line, "[Spatial Data Organization Information]", 39)==0) 
      strcpy(params, "Organization");
    if (strncmp(params, "Organization", 12)==0) {
      test = read_param(line);
      if (strncmp(test, "spatial_reference", 17)==0)
        strcpy(fgdc->direct, read_str(line, "spatial_reference"));
      if (strncmp(test, "raster_type", 11)==0)
        strcpy(fgdc->rasttype, read_str(line, "raster_type"));
      //FREE(test);
    }
    
    if (strncmp(line, "[Spatial Reference Information]", 31)==0) 
      strcpy(params, "Spatial");
    if (strncmp(params, "Spatial", 7)==0) {
      test = read_param(line);
      if (strncmp(test, "encoding", 8)==0)
        strcpy(fgdc->plance, read_str(line, "encoding"));
      if (strncmp(test, "point_position", 14)==0)
        strcpy(fgdc->ptpos, read_str(line, "point_position"));
      if (strncmp(test, "storage_order", 13)==0)
        strcpy(fgdc->storord, read_str(line, "storage_order"));
      //FREE(test);
    }
    
    if (strncmp(line, "[Distribution Information]", 26)==0) 
      strcpy(params, "Distribution");
    if (strncmp(params, "Distribution", 12)==0) {
      test = read_param(line);
      if (strncmp(test, "organization", 12)==0)
        strcpy(fgdc->distrib.cntorg, read_str(line, "Organization"));
      if (strncmp(test, "position", 8)==0 &&
	  strlen(read_str(line, "position")) > 0) {
	fgdc->distrib.cntpos = (char *) MALLOC(sizeof(char)*255);
        strcpy(fgdc->distrib.cntpos, read_str(line, "position"));
      }
      if (strncmp(test, "address_type", 12)==0)
        strcpy(fgdc->distrib.addrtype, read_str(line, "address_type"));
      if (strncmp(test, "address_line", 12)==0)
        strcpy(fgdc->distrib.address, read_str(line, "address_line"));
      if (strncmp(test, "city", 4)==0)
        strcpy(fgdc->distrib.city, read_str(line, "city"));
      if (strncmp(test, "state", 5)==0)
        strcpy(fgdc->distrib.state, read_str(line, "state"));
      if (strncmp(test, "postal", 6)==0)
        strcpy(fgdc->distrib.postal, read_str(line, "postal"));
      if (strncmp(test, "country", 7)==0 &&
	  strlen(read_str(line, "country")) > 0) {
	fgdc->distrib.country = (char *) MALLOC(sizeof(char)*255);
        strcpy(fgdc->distrib.country, read_str(line, "country"));
      }
      if (strncmp(test, "phone", 5)==0 &&
	  strlen(read_str(line, "phone")) > 0) {
	fgdc->distrib.cntvoice = (char *) MALLOC(sizeof(char)*100);
        strcpy(fgdc->distrib.cntvoice, read_str(line, "phone"));
      }
      if (strncmp(test, "fax", 3)==0 &&
	  strlen(read_str(line, "fax")) > 0) {
	fgdc->distrib.cntfax = (char *) MALLOC(sizeof(char)*100);
        strcpy(fgdc->distrib.cntfax, read_str(line, "fax"));
      }
      if (strncmp(test, "email", 5)==0 &&
	  strlen(read_str(line, "email")) > 0) {
	fgdc->distrib.cntemail = (char *) MALLOC(sizeof(char)*100);
        strcpy(fgdc->distrib.cntemail, read_str(line, "email"));
      }
      if (strncmp(test, "liability", 9)==0)
        strcpy(fgdc->distliab, read_para(fConfig, line, "liability"));
      if (strncmp(test, "network_path", 12)==0)
        strcpy(fgdc->networkr, read_str(line, "network_path"));
      if (strncmp(test, "fees", 4)==0)
        strcpy(fgdc->fees, read_str(line, "fees"));
      //FREE(test);
    }
    
    if (strncmp(line, "[Metadata Reference Information]", 32)==0) 
      strcpy(params, "Metadata");
    if (strncmp(params, "Metadata", 8)==0) {
      test = read_param(line);
      if (strncmp(test, "organization", 12)==0)
        strcpy(fgdc->metc.cntorg, read_str(line, "Organization"));
      if (strncmp(test, "position", 8)==0 &&
	  strlen(read_str(line, "position")) > 0) {
	fgdc->metc.cntpos = (char *) MALLOC(sizeof(char)*255);
        strcpy(fgdc->metc.cntpos, read_str(line, "position"));
      }
      if (strncmp(test, "address_type", 12)==0)
        strcpy(fgdc->metc.addrtype, read_str(line, "address_type"));
      if (strncmp(test, "address_line", 12)==0)
        strcpy(fgdc->metc.address, read_str(line, "address_line"));
      if (strncmp(test, "city", 4)==0)
        strcpy(fgdc->metc.city, read_str(line, "city"));
      if (strncmp(test, "state", 5)==0)
        strcpy(fgdc->metc.state, read_str(line, "state"));
      if (strncmp(test, "postal", 6)==0)
        strcpy(fgdc->metc.postal, read_str(line, "postal"));
      if (strncmp(test, "country", 7)==0 &&
	  strlen(read_str(line, "country")) > 0) {
	fgdc->metc.country = (char *) MALLOC(sizeof(char)*255);
        strcpy(fgdc->metc.country, read_str(line, "country"));
      }
      if (strncmp(test, "phone", 5)==0 &&
	  strlen(read_str(line, "phone")) > 0) {
	fgdc->metc.cntvoice = (char *) MALLOC(sizeof(char)*100);
        strcpy(fgdc->metc.cntvoice, read_str(line, "phone"));
      }
      if (strncmp(test, "fax", 3)==0 &&
	  strlen(read_str(line, "fax")) > 0) {
	fgdc->metc.cntfax = (char *) MALLOC(sizeof(char)*100);
        strcpy(fgdc->metc.cntfax, read_str(line, "fax"));
      }
      if (strncmp(test, "email", 5)==0 &&
	  strlen(read_str(line, "email")) > 0) {
	fgdc->metc.cntemail = (char *) MALLOC(sizeof(char)*100);
        strcpy(fgdc->metc.cntemail, read_str(line, "email"));
      }
      if (strncmp(test, "standard_name", 13)==0)
        strcpy(fgdc->metstdn, read_str(line, "standard_name"));
      if (strncmp(test, "standard_version", 16)==0)
        strcpy(fgdc->metstdv, read_str(line, "standard_version"));
      if (strncmp(test, "profile_name", 12)==0)
        strcpy(fgdc->metprof, read_str(line, "profile_name"));
      //FREE(test);
    }
  }
  
  FCLOSE(fConfig);
}
