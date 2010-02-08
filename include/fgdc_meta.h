#ifndef __FGDC_H__
#define __FGDC_H__

#include "asf_meta.h"

typedef struct
{
  char origin[255];    // Originator
  char pubdate[30];    // Publication Date
  char title[50];      // Title
  char geoform[50];    // Geospatial Data Presentation Form
  char onlink[255];    // Online Linkage
} citeinfo;

typedef struct
{
  char cntorg[100];    // Contact Organization
  char *cntpos;        // Contact Position
  char addrtype[25];   // Address Type
  char address[255];   // Address
  char city[255];      // City
  char state[50];      // State or Province
  char postal[20];     // Postal Code
  char *country;       // Country
  char *cntvoice;      // Contact Voice Telephone
  char *cntfax;        // Contact Facsimile Telephone
  char *cntemail;      // Contact Electronic Mail Address
} contactinfo;

typedef struct
{
  char thesaurus[50];  // Keyword Thesaurus
  int key_count;       // Number of keywords
  char **keyword;      // Keyword
} keyinfo;

typedef struct
{
  int theme_count;     // Number of themes
  keyinfo *theme;      // Theme
  int place_count;     // Number of places
  keyinfo *place;      // Place
} keywordinfo;

typedef struct
{
  char browsen[255];   // Browse Graphic File Name
  char browsed[255];   // Browse Graphic File Description
  char browset[25];    // Browse Graphic File Type
} browseinfo;

typedef struct
{
  citeinfo srccite;    // Source Citation
  int srcscale;        // Source Scale Denominator
  char typesrc[50];    // Type of Source Media
  char srctime[30];    // Source Time Period of Content
  char srccurr[50];    // Source Currentness Reference
  char srccitea[30];   // Source Citation Abbreviation
  char srccontr[500];  // Source Contribution
} sourceinfo;

typedef struct
{
  char procdesc[5000]; // Process Description
  char procdate[30];   // Process Date
} processinfo;

// FGDC meta structure
typedef struct 
{
  // Identification Information
  char datsetid[255];  // Dataset Identifier
  citeinfo citation;   // Citation flag
  char abstract[5000]; // Abstract
  char purpose[5000];  // Purpose
  char timeperd[30];   // Time Period of Content
  char current[50];    // Currentness Reference
  char progress[50];   // Progress
  char update[50];     // Update
  double westbc;       // West Bounding Coordinate
  double eastbc;       // East Bounding Coordinate
  double northbc;      // North Bounding Coordinate
  double southbc;      // South Bounding Coordinate
  char prolevid[25];   // Processing Level
  citeinfo prolevau;   // Processing Level Authority
  keywordinfo keywords;// Keywords
  char platflnm[50];   // Platform Full Name
  char instflnm[50];   // Instrument Full Name
  int numbands;        // Number of Bands
  char accconst[50];   // Access Constraints
  char useconst[50];   // Use Constraints
  browseinfo *browse;  // Browse image (optional)
  char secsys[50];     // Security Classification System
  char secclass[30];   // Security Classification
  char sechandl[100];  // Security Handling Description

  // Data Quality Information
  char *attraccr;      // Attribute Accuracy Report
  char logic[500];     // Logical Consistency Report
  char complete[500];  // Completeness Report
  char *horizpar;      // Horizontal Positional Accuracy Report
  char *vertaccr;      // Vertical Positioanl Accuracy Report
  int source_count;
  sourceinfo *srcinfo; // Source Information
  int process_count;
  processinfo *procstep; // Processing Step

  // Spatial Data Organization Information
  char direct[20];     // Direct Spatial Reference Method
  char cvaltype[50];   // Cell Value Type
  char rasttype[15];   // Raster Object Type
  int rowcount;        // Row Count
  int colcount;        // Column Count

  // Spatial Reference Information
  int projected;       // Flag for map projected data
  meta_projection *projection; // Map projection parameters
  char plance[25];     // Planar Coordinate Encoding Method
  char ptpos[25];      // Point Position in Pixel
  char storord[25];    // Storage Order

  // Distribution Information
  contactinfo distrib; // Distributor
  char distliab[500];  // Distribution Liability
  char formname[20];   // Digital Form
  char networkr[200];  // Network Resource Name
  char fees[50];       // Fees

  // Metadata Reference Information
  contactinfo metc;    // Metadata Contact
  char metstdn[100];   // Metadata Standard Name
  char metstdv[50];    // Metadata Standard Version
  char metprof[100];   // Profile Name

} fgdc_meta;

// Function prototypes
fgdc_meta *read_fgdc_meta(const char *dataFile);

#endif
