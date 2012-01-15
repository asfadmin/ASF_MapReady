#define ASF_NAME_STRING "dumpLineHeader"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-line <number>] <in_name> <template>\n"

#define ASF_DESCRIPTION_STRING \
"     This program dumps the line header of a CEOS data file.\n\n"

#define ASF_INPUT_STRING \
"     The input file is required, and should be a CEOS data file.\n"

#define ASF_OPTIONS_STRING \
"     -line <number>\n"\
"          Define a particular line number to dump.\n"\
"          Special predefined values: FIRST, CENTER, LAST\n"\
"     -log <log file>\n"\
"          Output will be written to a specified log file.\n"\
"\n"\
"     -quiet\n"\
"          Supresses all non-essential output.\n"\
"\n"\
"     -license\n"\
"          Print copyright and license for this software then exit.\n"\
"\n"\
"     -version\n"\
"          Print version and copyright then exit.\n"\
"\n"\
"     -help\n"\
"          Print a help page and exit.\n"

#include <stdio.h>
#include <ctype.h>
#include <dirent.h>
#include "asf.h"
#include "asf_endian.h"
#include "asf_meta.h"
#include "asf_license.h"
#include "asf_contact.h"

// Print minimalistic usage info & exit
static void usage(const char *name)
{
  char path[1024];
  struct dirent *dp;
  DIR *dir;

  asfPrintStatus("\n"
      "Usage:\n"
      ASF_USAGE_STRING
      "\n");
  asfPrintStatus("Currently available templates are:\n");
  sprintf(path, "%s/metadata", get_asf_share_dir());
  dir = opendir(path);
  while ((dp = readdir(dir)) != NULL) {
    if (strcmp(dp->d_name, ".") != 0 && strcmp(dp->d_name, "..") != 0)
      asfPrintStatus("   %s\n", dp->d_name);
  }
  asfPrintStatus("\n");
  closedir(dir);
  exit(EXIT_FAILURE);
}

// Print the help info & exit
static void print_help(void)
{
  asfPrintStatus(
      "\n"
      "Tool name:\n   " ASF_NAME_STRING "\n\n"
      "Usage:\n" ASF_USAGE_STRING "\n"
      "Description:\n" ASF_DESCRIPTION_STRING "\n"
      "Input:\n" ASF_INPUT_STRING "\n"
      "Options:\n" ASF_OPTIONS_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " SVN_REV " (part of " TOOL_SUITE_NAME " " MAPREADY_VERSION_STRING ")\n\n");
  exit(EXIT_SUCCESS);
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

void dumpLineHeader(const char *inName, const char *template, int line_number)
{
  struct HEADER hdr;
  long offset;
  int itype, subtype[3], rec_seq, length, format_length, bytes;
  char label[255], *line, *p, format[5], byteValue, templateFile[1024];
  unsigned char value[4];

  printf("\nFile: %s\n", inName);
  printf("Format template: %s\n", template);

  // Determine offset and jump to it
  offset = firstRecordLen(inName);
  FILE *fpData = FOPEN(inName, "rb");

  int current_line = 1;
  while (current_line < line_number) {
    
    // Read the header information
    FSEEK64(fpData, offset, SEEK_SET);
    FREAD(&hdr, 1, 12, fpData);
    subtype[0] = hdr.rectyp[0];
    itype = hdr.rectyp[1];
    subtype[1] = hdr.rectyp[2];
    subtype[2] = hdr.rectyp[3];
    rec_seq = bigInt32(hdr.recnum);
    length = bigInt32(hdr.recsiz);
  
    offset += length;
    current_line++;
  }

  asfPrintStatus("\nHeader length: %d\n", offset);
  asfPrintStatus("Record type: %d\n", itype);
  asfPrintStatus("Sub-record[1]: %d\nSub-record[2]: %d\nSub_record[3]: %d\n",
		 subtype[0], subtype[1], subtype[2]);
  asfPrintStatus("Sequence: %d\nLength: %d\n\n", rec_seq, length);

  // Check whether template file exists in current directory.
  // Otherwise try in the share directory. After that give up.
  FILE *fp = fopen(template, "r");
  if (!fp) {
    sprintf(templateFile, "%s/metadata/%s", get_asf_share_dir(), template);
    fp = fopen(templateFile, "r");
    if (!fp)
      asfPrintError("Couldn't open template file: %s\n", template);
  }
  else 
    strcpy(templateFile, template);
  
  // Read through the template and dump the values
  printf("\nLabel\t\tBytes\tFormat\tValue\n");
  printf("-----\t\t-----\t------\t-----\n");
  line = (char *) MALLOC(sizeof(char)*1024);
  fp = FOPEN(templateFile, "r");
  while (fgets(line, 1024, fp)) {
    
    // Extract relevant informaton out of template file
    line[strlen(line)-1] = '\0';
    p = line;
    while (isspace((int)(*p))) p++;
    if (*p != '#' && strlen(p) > 0) {
      p = strchr(line, ',');
      if (p && *p == ',') {
	sscanf(p+1, "%d, %s", &bytes, format);
	chomp(format);
	sscanf(format, "B%d", &format_length);
	*p = '\0';
	sprintf(label, "%s", line);
	
	// Read value
	FSEEK64(fpData, offset+bytes-1, SEEK_SET);
	switch (format_length)
	  {
	  case (1):
	    FREAD(&byteValue, 1, 1, fpData);
	    break;
	  case (2):
	    FREAD(&value, 1, 2, fpData);
	    break;
	  case (4):
	    FREAD(&value, 1, 4, fpData);
	    break;
	  default:
	    asfPrintError("Unknown template format (%s)\n", format);
	  }
	
	// Report your findings
	printf("%s\t", label);
	if (strlen(label) < 8)
	  printf("\t");
	printf("%d-%d\t%s\t", bytes, bytes+format_length-1, format);
	switch (format_length)
	  {
	  case (1):
	    printf("%d\n", byteValue);
	    break;
	  case (2):
	    printf("%d\n", bigInt16(value));
	    break;
	  case (4):
	    printf("%d\n", bigInt32(value));
 	    break;
	  }
      }
    }
  }
  
  FREE(line);
  FCLOSE(fp);
  FCLOSE(fpData);
}

// Main program body.
int
main (int argc, char *argv[])
{
  struct dataset_sum_rec *dssr=NULL;
  struct IOF_VFDR *iof=NULL;
  char *inFile, *template, lineStr[15]="1";
  int currArg = 1;
  int NUM_ARGS = 2;
  int line;

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc<=1)
      usage(ASF_NAME_STRING);
  else if (strmatches(argv[1],"-help","--help",NULL))
      print_help();

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-help","--help",NULL)) {
        print_help(); // doesn't return
    }
    else if (strmatches(key,"-log","--log",NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
      quietflag = TRUE;
    }
    else if (strmatches(key,"-line","--line","-l",NULL)) {
      CHECK_ARG(1);
      strcpy(lineStr, GET_ARG(1));
    }
    else {
        --currArg;
        break;
    }
  }

  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    usage(argv[0]);
  }

  inFile = argv[currArg];
  template = argv[currArg+1];

  // Determine the number of lines (if necessary)
  if (strncmp_case(lineStr, "FIRST", 5) == 0)
    line = 1;
  else if (strncmp_case(lineStr, "CENTER", 6) == 0 ||
	   strncmp_case(lineStr, "LAST", 4) == 0) {
    iof = (struct IOF_VFDR*) MALLOC(sizeof(struct IOF_VFDR));
    get_ifiledr(inFile, iof);
    dssr = (struct dataset_sum_rec *) MALLOC(sizeof(struct dataset_sum_rec));
    get_dssr(inFile, dssr);
    if (iof->numofrec == 0)
      line = dssr->sc_lin * 2;
    else
      line = iof->numofrec;
    if (strncmp_case(lineStr, "CENTER", 6) == 0)
      line /= 2;
    FREE(iof);
    FREE(dssr);
  }
  else {
    line = atoi(lineStr);
    if (line == 0)
      asfPrintError("No valid line number: %s\n", lineStr);
  }

  dumpLineHeader(inFile, template, line);
  asfPrintStatus("\nDone.\n");

  return EXIT_SUCCESS;
}
