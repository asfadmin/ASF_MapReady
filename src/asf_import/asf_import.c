/*==================BEGIN ASF AUTO-GENERATED DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to change the
following defines. For the short ones (like ASF_NAME_STRING) this is no big
deal. However, for some of the longer ones, such as ASF_COPYRIGHT_STRING, it
can be a daunting task to get all the newlines in correctly, etc. In order to
help you with this task, there is a tool, edit_man_header.pl. The tool *only*
works with this portion of the code, so fear not. It will scan in defines of
the format #define ASF_<something>_STRING between the two auto-generated
documentation markers, format them for a text editor, run that editor, allow
you to edit the text in a clean manner, and then automatically generate these
defines, formatted appropriately. The only warning is that any text between
those two markers and not part of one of those defines will not be preserved,
and that all of this auto-generated code will be at the top of the source
file. Save yourself the time and trouble, and use edit_man_header.pl. :)
*/

#define ASF_NAME_STRING \
"   asf_import"

#define ASF_USAGE_STRING \
"[-amplitude | -sigma | -gamma | -beta | -power]\n"\
"              [-prc] [-old] [-format <inputFormat>] [-lat <lower> <upper>]\n"\
"              [-metadata <inMetaFile>] [-log <logFile>] [-quiet] [-help]\n"\
"              <inBaseName> <outBaseName>"

#define ASF_DESCRIPTION_STRING \
"   Ingests all varieties of CEOS and STF data formats as well as the\n"\
"   external ESRI and ENVI data formats and outputs ASF internal format\n"\
"   metadata and data files."

#define ASF_INPUT_STRING \
"   The format of the input file must be specified as CEOS, STF, ESRI, or\n"\
"   ENVI."

#define ASF_OUTPUT_STRING \
"   Outputs data and metadata files with the user-provided base name and\n"\
"   appropriate extensions."

#define ASF_OPTIONS_STRING \
"   -amplitude     Create an amplitude image. This is the default behavior.\n"\
"   -sigma         Create a calibrated image (sigma dB values).\n"\
"   -gamma         Create a calibrated image (gamma dB values).\n"\
"   -beta          Create a calibrated image (beta dB values).\n"\
"   -power         Create a power image.\n"\
"   -format        Force input data to be read as the given format type.\n"\
"                    Valid options are ceos, stf, esri, and envi.\n"\
"                    'ceos' is the default behavior.\n"\
"   -log           Output will be written to a specified log file.\n"\
"   -quiet         Supresses all non-essential output.\n"\
"   -lat           Specify lower and upper latitude contraints.\n"\
"   -old           Output in old style ASF internal format.\n"\
"   -metadata      Use a different name for the metadata file.\n"\
"                    Requires only the base name.\n"\
"   -prc           Replace the restituted state vectors from the original\n"\
"                    raw data acquired by the ERS satellites with preceision\n"\
"                    state vectors from DLR."\
"   -lut           Applies a user defined look up table to the\n"\
"                    data. Look up contains incidence angle dependent\n"\
"                    scaling factors."

#define ASF_EXAMPLES_STRING \
"   To import CEOS format to the ASF tools internal format run:\n"\
"       example> asf_import fileCEOS fileASF\n"\
"\n"\
"   To import a STF fileset (fileSTF.000 & file.000.par) you will need to\n"\
"   specify the -format option since STF is not the default.\n"\
"       example> asf_import -format stf fileSTF.000 fileASF\n"\
"\n"

#define ASF_LIMITATIONS_STRING \
"  CEOS base name issue:\n"\
"   If you have two or more CEOS filesets ([*.D & *.L], [*.RAW & *.LDR], or\n"\
"   [dat.* & lea.*]) with the same base name, then this program will\n"\
"   automatically fetch the first set in the aforementioned list."

#define ASF_SEE_ALSO_STRING \
"   asf_convert, asf_export"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks\n"\
"All rights reserved.\n"\
"\n"\
"Redistribution and use in source and binary forms, with or without\n"\
"modification, are permitted provided that the following conditions are met:\n"\
"\n"\
"    * Redistributions of source code must retain the above copyright notice,\n"\
"      this list of conditions and the following disclaimer.\n"\
"    * Redistributions in binary form must reproduce the above copyright\n"\
"      notice, this list of conditions and the following disclaimer in the\n"\
"      documentation and/or other materials provided with the distribution.\n"\
"    * Neither the name of the Geophysical Institute nor the names of its\n"\
"      contributors may be used to endorse or promote products derived from\n"\
"      this software without specific prior written permission.\n"\
"\n"\
"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\n"\
"AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"\
"IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n"\
"ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE\n"\
"LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"\
"CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF\n"\
"SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS\n"\
"INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN\n"\
"CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)\n"\
"ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n"\
"POSSIBILITY OF SUCH DAMAGE.\n"\
"\n"\
"       For more information contact us at:\n"\
"\n"\
"       Alaska Satellite Facility\n"\
"       Geophysical Institute\n"\
"       University of Alaska Fairbanks\n"\
"       P.O. Box 757320\n"\
"       Fairbanks, AK 99775-7320\n"\
"\n"\
"       http://www.asf.alaska.edu\n"\
"       uso@asf.alaska.edu"

#define ASF_PROGRAM_HISTORY_STRING \
"    VERS:   DATE:  AUTHOR:     PURPOSE:\n"\
"    ---------------------------------------------------------------\n"\
"    0.1    12/03   R. Gens     Combined sarin, calibrate, trim_slc and\n"\
"                                ceos2raw into one program\n"\
"    0.2     4/04   P. Denny    Allowed for RSI naming scheme as well as\n"\
"                                our typical CEOS naming scheme\n"\
"    0.3     5/04   R. Gens     Added stf2raw and external ESRI and ENVI\n"\
"                                formats; renamed the tool to import2asf\n"\
"    0.4     5/04   J. Nicoll   Fixed sign issue when converting 8 bit\n"\
"                                data to calibrated amplitude data\n"\
"    0.41    5/04   P. Denny    Made format of input data a required\n"\
"                                argument (CEOS,STF,ESRI,ENVI)\n"\
"    0.5     5/04   P. Denny    Added hidden option to write out sprocket\n"\
"                                style metadata.\n"\
"    0.6     7/04   G. Short    New command line parsing.\n"\
"    0.7     7/04   G. Short    New usage/help style.\n"\
"    1.0     8/04   P. Denny    Create functions to do each of the different\n"\
"                                filetype imports... ready for release."


#define ASF_VERSION_MAJOR_STRING "1.0"
/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/

#define VERSION 1.0

#include "asf_import.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "ceos.h"
#include "decoder.h"
#include "get_ceos_names.h"
#include "get_stf_names.h"
#include <ctype.h>

#define REQUIRED_ARGS 2

/* usage - enter here on command-line usage error*/
void usage(void)
{
	printf("\n"
		"USAGE:\n"
		ASF_NAME_STRING
		" "
		ASF_USAGE_STRING
	"\n\n");
	exit (EXIT_FAILURE);
}

/* help_page - go here when the -help option is specified */
void help_page()
{
  char happy_string[4066];
  char command[4096];

  /* What to print out for help */
  sprintf(happy_string,
          "\n\n\n"
          "Tool name:\n" ASF_NAME_STRING "\n\n\n"
          "Usage:\n" ASF_NAME_STRING " " ASF_USAGE_STRING "\n\n\n"
          "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
          "Input:\n" ASF_INPUT_STRING "\n\n\n"
          "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
          "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
          "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
          "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
          "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
          "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
          "Version: " ASF_VERSION_MAJOR_STRING "\n\n\n");

  /* If we can, use less */
  sprintf(command,"echo '%s' | less",happy_string);
  if(system(command) != -1)
    exit(EXIT_SUCCESS);

  /* Hmmm, less didn't work cause we got here, try using more */
  sprintf(command,"echo '%s' | more",happy_string);
  if(system(command) != -1)
    exit(EXIT_SUCCESS);

  /* Okay, neither less or more work (obviously if we made it here),
   * just print the info straight to stdout and exit */
  printf(happy_string);
  exit(EXIT_SUCCESS);
}


/******************************************************************************
 * Lets rock 'n roll!
 *****************************************************************************/
int main(int argc, char *argv[])
{
  char inBaseName[256]="";
  char inDataName[256]="", inMetaName[256]="";
  char outBaseName[256]="";
  char inMetaNameOption[256], prcPath[256]="";
  char *lutName=NULL;
  char format_type[256]="";
  int ii;
  double lowerLat=NAN, upperLat=NAN;
  flag_indices_t flags[NUM_FLAGS];

  /* Set all flags to 'not set' */
  for (ii=0; ii<NUM_FLAGS; ii++) {
    flags[ii] = FLAG_NOT_SET;
  }

/**********************BEGIN COMMAND LINE PARSING STUFF**********************/
  /* Most importantly, check to see if the help option was specified */
  if (   (checkForOption("--help", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-h", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-help", argc, argv) != FLAG_NOT_SET) )
    help_page();
  /*Check to see if any options were provided*/
  flags[f_AMP] = checkForOption("-amplitude", argc, argv);
  flags[f_SIGMA] = checkForOption("-sigma", argc, argv);
  flags[f_BETA] = checkForOption("-beta", argc, argv);
  flags[f_GAMMA] = checkForOption("-gamma", argc, argv);
  flags[f_POWER] = checkForOption("-power", argc, argv);
  flags[f_SPROCKET] = checkForOption("-sprocket", argc, argv);
  flags[f_LUT] = checkForOption("-lut", argc, argv);
  flags[f_LAT_CONSTRAINT] = checkForOption("-lat", argc, argv);
  flags[f_PRC] = checkForOption("prc", argc, argv);
  flags[f_OLD_META] = checkForOption("-old", argc, argv);
  flags[f_METADATA_FILE] = checkForOption("-metadata", argc, argv);
  flags[f_LOG] = checkForOption("-log", argc, argv);
  flags[f_QUIET] = checkForOption("-quiet", argc, argv);
  flags[f_FORMAT] = checkForOption("-format", argc, argv);

  { /*Check for mutually exclusive options: we can only have one of these*/
    int temp = 0;
    if(flags[f_AMP] != FLAG_NOT_SET)      temp++;
    if(flags[f_SIGMA] != FLAG_NOT_SET)    temp++;
    if(flags[f_BETA] != FLAG_NOT_SET)     temp++;
    if(flags[f_GAMMA] != FLAG_NOT_SET)    temp++;
    if(flags[f_POWER] != FLAG_NOT_SET)    temp++;
    if(flags[f_SPROCKET] != FLAG_NOT_SET) temp++;
    if(flags[f_LUT] != FLAG_NOT_SET)      temp++;
    if(temp > 1)/*If more than one option was selected*/
      usage();/*This exits with a failure*/
  }
  { /*We need to make sure the user specified the proper number of arguments*/
    int needed_args = REQUIRED_ARGS+1;/*command & in_base & out_base*/
    if(flags[f_AMP] != FLAG_NOT_SET)      needed_args += 1;/*option*/
    if(flags[f_SIGMA] != FLAG_NOT_SET)    needed_args += 1;/*option*/
    if(flags[f_BETA] != FLAG_NOT_SET)     needed_args += 1;/*option*/
    if(flags[f_GAMMA] != FLAG_NOT_SET)    needed_args += 1;/*option*/
    if(flags[f_POWER] != FLAG_NOT_SET)    needed_args += 1;/*option*/
    if(flags[f_SPROCKET] != FLAG_NOT_SET) needed_args += 1;/*option*/
    if(flags[f_LUT] != FLAG_NOT_SET)      needed_args += 2;/*option & parameter*/
    if(flags[f_LAT_CONSTRAINT] != FLAG_NOT_SET)
      needed_args += 3;/*option & parameter & parameter*/
    if(flags[f_PRC] != FLAG_NOT_SET)      needed_args += 2;/*option & parameter*/
    if(flags[f_OLD_META] != FLAG_NOT_SET) needed_args += 1;/*option*/
    if(flags[f_METADATA_FILE] != FLAG_NOT_SET)  needed_args += 2;/*option & parameter*/
    if(flags[f_LOG] != FLAG_NOT_SET)      needed_args += 2;/*option & parameter*/
    if(flags[f_QUIET] != FLAG_NOT_SET)    needed_args += 1;/*option*/
    if(flags[f_FORMAT] != FLAG_NOT_SET)   needed_args += 2;/*option & parameter*/

    /*Make sure we have enough arguments*/
    if(argc != needed_args)
      usage();/*This exits with a failure*/
  }

  /*We also need to make sure any options that have parameters are specified
    correctly.  This includes: -lat, -prc, -log, -lut*/
  if(flags[f_LAT_CONSTRAINT] != FLAG_NOT_SET)
    /*Make sure there's no "bleeding" into the required arguments
      No check for '-' in the two following fields because negative numbers
      are legit (eg -lat -67.5 -70.25)*/
    if(flags[f_LAT_CONSTRAINT] >= argc - (REQUIRED_ARGS+1))
       usage();
  if(flags[f_PRC] != FLAG_NOT_SET)
    /*Make sure the field following -prc isn't another option
      Also check for bleeding into required arguments*/
    if(   argv[flags[f_PRC]+1][0] == '-'
       || flags[f_PRC] >= argc-REQUIRED_ARGS)
      usage();
  if(flags[f_METADATA_FILE] != FLAG_NOT_SET)
    /*Make sure the field following -metadata isn't another option*/
    if(   argv[flags[f_METADATA_FILE] + 1][0] == '-'
       || flags[f_METADATA_FILE] >= argc - REQUIRED_ARGS)
      usage();/*This exits with a failure*/
  if(flags[f_LOG] != FLAG_NOT_SET)
    /*Make sure the field following -log isn't another option*/
    if(   argv[flags[f_LOG]+1][0] == '-'
       || flags[f_LOG] >= argc-REQUIRED_ARGS)
      usage();
  if(flags[f_LUT] != FLAG_NOT_SET)
    /*Make sure the field following -lut isn't another option*/
    if(   argv[flags[f_LUT]+1][0] == '-'
       || flags[f_LUT] >= argc-REQUIRED_ARGS)
      usage();
  if(flags[f_FORMAT] != FLAG_NOT_SET)
    /*Make sure the field following -format isn't another option*/
    if(   argv[flags[f_FORMAT]+1][0] == '-'
       || flags[f_FORMAT] >= argc-REQUIRED_ARGS)
      usage();

  /* Be sure to open log ASAP */
  if(flags[f_LOG] != FLAG_NOT_SET)
    strcpy(logFile, argv[flags[f_LOG] + 1]);
  else /*default behavior: log to tmp<pid>.log*/
    sprintf(logFile, "tmp%i.log", (int)getpid());
  logflag = TRUE; /* Since we always log, set the old school logflag to true */
  fLog = FOPEN(logFile, "a");
  /* Set old school quiet flag (for use in our libraries) */
  quietflag = (flags[f_QUIET]!=FLAG_NOT_SET) ? TRUE : FALSE;

  /*We must be close to good enough at this point... (logfile is open)
    start filling in fields as needed*/
  if(flags[f_QUIET] == FLAG_NOT_SET)
    print_splash_screen(argc, argv);/*display splash screen if not quiet*/

  if(flags[f_PRC] != FLAG_NOT_SET)
    strcpy(prcPath, argv[flags[f_PRC] + 1]);

  if(flags[f_LAT_CONSTRAINT] != FLAG_NOT_SET) {
    lowerLat = strtod(argv[flags[f_LAT_CONSTRAINT] + 2],NULL);
    upperLat = strtod(argv[flags[f_LAT_CONSTRAINT] + 1],NULL);
    if(lowerLat > upperLat) {
      float swap = upperLat;
      upperLat = lowerLat;
      lowerLat = swap;
    }
    if(lowerLat < -90.0 || lowerLat > 90.0 || upperLat < -90.0 || upperLat > 90.0)
    {
      print_error("Invalid latitude constraint (must be -90 to 90)");
    }
  }

  if(flags[f_LUT] != FLAG_NOT_SET) {
    lutName = (char *) MALLOC(sizeof(char)*256);
    strcpy(lutName, argv[flags[f_LUT] + 1]);
  }

  { /* BEGIN: Check for conflict between pixel type flags */
    char flags_used[256] = "";
    int flag_count=0;

    if (flags[f_AMP] != FLAG_NOT_SET) {
      pixel_type_flag_looker(&flag_count, flags_used, "amplitude");
    }
    if (flags[f_SIGMA] != FLAG_NOT_SET) {
      pixel_type_flag_looker(&flag_count, flags_used, "sigma");
    }
    if (flags[f_GAMMA] != FLAG_NOT_SET) {
      pixel_type_flag_looker(&flag_count, flags_used, "gamma");
    }
    if (flags[f_BETA] != FLAG_NOT_SET) {
      pixel_type_flag_looker(&flag_count, flags_used, "beta");
    }
    if (flags[f_POWER] != FLAG_NOT_SET) {
      pixel_type_flag_looker(&flag_count, flags_used, "power");
    }
    if (flags[f_LUT] != FLAG_NOT_SET) {
      pixel_type_flag_looker(&flag_count, flags_used, "lut");
    }
    if (flag_count > 1) {
      sprintf(logbuf, "Cannot mix the %s flags.", flags_used);
      print_error(logbuf);
    }
  } /* END: Check for conflict between pixel type flags */

  /* Get the input metadata name if the flag was specified (probably for a meta
   * name with a different base name than the data name) */
  if(flags[f_METADATA_FILE] != FLAG_NOT_SET) {
    strcpy(inMetaNameOption, argv[flags[f_METADATA_FILE] + 1]);
  }

  /* Deal with input format type */
  if(flags[f_FORMAT] != FLAG_NOT_SET) {
    strcpy(format_type, argv[flags[f_FORMAT] + 1]);
    for (ii=0; ii<strlen(format_type); ii++) {
      format_type[ii] = (char)toupper(format_type[ii]);
    }
  }
  else
    strcpy(format_type, "CEOS");

  /* Make sure STF specific options are not used with other data types */
  if (strcmp(format_type, "STF")!=0) {
    if (flags[f_PRC] != FLAG_NOT_SET) {
      printAndLog("WARNING: Precision state vectors only work with STF data\n"
                  "         and will not be used with this data set!\n");
      flags[f_PRC]=FLAG_NOT_SET;
    }
    if (flags[f_LAT_CONSTRAINT] != FLAG_NOT_SET) {
      printAndLog("WARNING: No latitude constraints only work with STF data\n"
                  "         and will not be used with this data set!\n");
      flags[f_LAT_CONSTRAINT]=FLAG_NOT_SET;
    }
  }

  /* Fetch required arguments */
  strcpy(inBaseName, argv[argc - 2]);
  strcpy(outBaseName,argv[argc - 1]);

/***********************END COMMAND LINE PARSING STUFF***********************/

  /* If the user wants sprocket layers, first check to see if the asf data is
     already there and go straight to creating the layers */
  if (flags[f_SPROCKET] != FLAG_NOT_SET) {
    char asfimage[256], asfmeta[256];
    strcat(strcpy(asfimage,outBaseName),TOOLS_IMAGE_EXT);
    strcat(strcpy(asfmeta,outBaseName),TOOLS_META_EXT);
    if (fileExists(asfimage) && fileExists(asfmeta)) {
      create_sprocket_layers(outBaseName, inMetaName);
      /* Nix the log file if the user didn't ask for it */
    	if (flags[f_LOG] == FLAG_NOT_SET) {
    		FCLOSE(fLog);
    		remove(logFile);
    	}
    	exit(EXIT_SUCCESS);
    }
  }

  /* Ingest all sorts of flavors of CEOS data */
  if (strncmp(format_type, "CEOS", 4) == 0) {
    printAndLog("   Data format: %s\n", format_type);
    if (flags[f_METADATA_FILE] == FLAG_NOT_SET)
      require_ceos_pair(inBaseName, inDataName, inMetaName);
    else {
      /* Allow the base name to be different for data & meta */
      require_ceos_data(inBaseName, inDataName);
      require_ceos_metadata(inMetaNameOption, inMetaName);
    }
    import_ceos(inDataName, inMetaName, lutName, outBaseName, flags);
  }
  /* Ingest ENVI format data */
  else if (strncmp(format_type, "ENVI", 4) == 0) {
    printAndLog("   Data format: %s\n", format_type);
    import_envi(inDataName, inMetaName, outBaseName, flags);
  }
  /* Ingest ESRI format data */
  else if (strncmp(format_type, "ESRI", 4) == 0) {
    printAndLog("   Data format: %s\n", format_type);
    import_esri(inDataName, inMetaName, outBaseName, flags);
  }
  /* Ingest Vexcel Sky Telemetry Format (STF) data */
  else if (strncmp(format_type, "STF", 3) == 0) {
    printAndLog("   Data format: %s\n", format_type);
    if (flags[f_METADATA_FILE] == FLAG_NOT_SET)
      require_stf_pair(inBaseName, inDataName, inMetaName);
    else {
      /* Allow the base name to be different for data & meta */
      require_stf_data(inBaseName, inDataName);
      require_stf_metadata(inMetaNameOption, inMetaName);
    }
    import_stf(inDataName, inMetaName, outBaseName, flags,
               lowerLat, upperLat, prcPath);
  }
  /* Don't recognize this data format; report & quit */
  else {
    sprintf(logbuf,"Unrecognized data format: '%s'",format_type);
    print_error(logbuf);
    printLog(logbuf);
  }

  /* If the user asked for sprocket layers, create sprocket data layers
     now that we've got asf tools format data */
  if (flags[f_SPROCKET] != FLAG_NOT_SET) {
    create_sprocket_layers(outBaseName, inMetaName);
  }

  /* If the user didn't ask for a log file then we can nuke the one that
     we've been keeping since we've finished everything */
	if (flags[f_LOG] == FLAG_NOT_SET) {
		FCLOSE(fLog);
		remove(logFile);
	}

  exit(EXIT_SUCCESS);
}
