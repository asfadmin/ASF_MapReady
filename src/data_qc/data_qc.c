/****************************************************************************
*                                                                             *
*   Data_qc verifies the validity of a CEOS data set                          *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#define ASF_NAME_STRING "data_qc"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" -verbose <in_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program verifies the validity of a CEOS data set.\n\n"

#define ASF_INPUT_STRING \
"     The input file is required, and should be the basename of a CEOS\n"\
"     data set.\n"

#define ASF_OPTIONS_STRING \
"     -verbose\n"\
"          Prints out the results of all the tests carried out.\n"\
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
#include <asf.h>
#include <asf_meta.h>
#include <asf_endian.h>
#include <ceos.h>
#include <asf_license.h>
#include <asf_contact.h>
#include <get_ceos_names.h>

// Print minimalistic usage info & exit
static void usage(const char *name)
{
  asfPrintStatus("\n"
      "Usage:\n"
      ASF_USAGE_STRING
      "\n");
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

typedef enum {
  CEOS_ATDR=40,         /* Attitude data record.*/
  CEOS_DHR=70,          /* Data histograms record.*/
  CEOS_DQSR=60,         /* Data quality summary record.*/
  CEOS_DSSR=10,         /* Data set summary record.*/
  CEOS_ASFFACDR=210,    /* ASF Facility-related data record.*/
  CEOS_ESAFACDR=220,    /* ESA Facility-related data record.*/
  CEOS_JAXAFACDR=230,   // JAXA Facility-related data record
  CEOS_FACDR=200,       /* Pre-1996 Facdr*/
  CEOS_MPDR=20,         /* Map-projection data record.*/
  CEOS_PPDR=30,         /* Platform position data record.*/
  CEOS_RADDR=50,        /* Radiometric data record.*/
  CEOS_RSR=80,          /* Range Spectra Record.*/
  CEOS_IFILEDR=192,     /* Imagery options file.*/
  CEOS_FDR=300,         /* File Descriptor Record.*/
  CEOS_PPR=120,         /* Processing Parameter Record - CDPF defined.*/
  CEOS_SHR=18,          // Scene Header Record - ALOS
  CEOS_AMPR=36,         // Map Projection Record - ALOS
  CEOS_ARDR=50,         // Radiometric Data Record - ALOS
  CEOS_RCDR=51,         // Radiometric Compensation Data Record - RSI
  CEOS_TFDR=193         // Trailer File Descriptor Record - ALOS
} CEOS_RECORD_TYPE;

int firstRecordLen(const char *ceosName)
{
  FILE *f;
  struct HEADER h;

  f=FOPEN(ceosName,"rb");    /*Open file.*/
  FREAD(&h,1,12,f);          /*Read first CEOS header.*/
  FCLOSE(f);                 /*Close file*/
  return bigInt32(h.recsiz); /*put recsiz in proper endian format & return*/
}

char *strip_alos_prep(const char *name)
{
  char *prep = (char *) MALLOC(sizeof(char)*25);
  char *basename = (char *) MALLOC(sizeof(char)*1024);
  const char file[][10] = { "IMG-","LED-","TRL-","VOL-" };
  int ii, kk;
  
  // Go through data types
  for (ii=0; ii<4; ii++) {
    // Optical data sets first
    for (kk=1; kk<10; kk++) {
      sprintf(prep, "%s0%d-", file[ii], kk);
      if (strstr(name, prep) != NULL) {
	sprintf(basename, "%s", &name[7]);
	return basename;
      }
    }
    // Different polarizations next
    sprintf(prep, "%sHH-", file[ii]);
    if (strstr(name, prep) != NULL) {
      sprintf(basename, "%s", &name[7]);
      return basename;
    }
    sprintf(prep, "%sHV-", file[ii]);
    if (strstr(name, prep) != NULL) {
      sprintf(basename, "%s", &name[7]);
      return basename;
    }
    sprintf(prep, "%sVH-", file[ii]);
    if (strstr(name, prep) != NULL) {
      sprintf(basename, "%s", &name[7]);
      return basename;
    }
    sprintf(prep, "%sVV-", file[ii]);
    if (strstr(name, prep) != NULL) {
      sprintf(basename, "%s", &name[7]);
      return basename;
    }
    // The rest of it
    if (strstr(name, file[ii]) != NULL) {
      sprintf(basename, "%s", &name[4]);
      return basename;
    }
  }
  return basename;
}

void data_qc(char *ceosName, int verbose)
{
  FILE *fp;
  struct HEADER bufhdr;
  struct dataset_sum_rec *dssr=NULL;
  struct IOF_VFDR *iof=NULL;
  struct trl_file_des_rec *tfdr=NULL;
  unsigned char *buff;
  char **dataName=NULL, **metaName=NULL, *trailerName=NULL, *volumeName=NULL;
  char *workreport;
  int ii, nBands, trailer, total, estimated_size, actual_size;

  // Verify that the essential files exist. Require_ceos_pair takes care 
  // of the essentials: data file, leader/trailer file
  require_ceos_pair(ceosName, &dataName, &metaName, &nBands, &trailer);

  // Check for ALOS files
  dssr = (struct dataset_sum_rec *) MALLOC(sizeof(struct dataset_sum_rec));
  if (get_dssr((const char *)metaName[0], dssr) == -1)
    asfPrintError("Leader file corrupted\n"
		  "Data QC status: FAILED\n");
  if (strncmp_case(dssr->mission_id, "ALOS", 4) == 0) {
    char *tmp = (char *) MALLOC(sizeof(char)*512);
    tmp = get_basename(ceosName);
    char *test_name = (char *) MALLOC(sizeof(char)*512);
    char *basename = strip_alos_prep(tmp);

    // Check for trailer file
    sprintf(test_name, "TRL-%s", basename);
    fp = fopen(test_name, "r"); 
    if (fp) {
      fclose(fp);
      trailerName = (char *) MALLOC(sizeof(char)*1024);
      sprintf(trailerName, "%s", test_name);
    }
    else
      asfPrintError("Missing trailer file!\nData QC status: FAILED\n");

    // Check for volume file
    sprintf(test_name, "VOL-%s", basename);
    fp = fopen(test_name, "r");
    if (fp) {
      fclose(fp);
      volumeName = (char *) MALLOC(sizeof(char)*1024);
      sprintf(volumeName, "%s", test_name);
    }
    else
      asfPrintError("Missing volume file!\nData QC status: FAILED\n");
    
    // Check for workreport file
    fp = fopen("workreport", "r");
    if (fp){
      fclose(fp);
      workreport = (char *) MALLOC(sizeof(char)*15);
      strcpy(workreport, "workreport");
    }
    else
      asfPrintError("Missing workreport file!\nData QC status: FAILED\n");
    
    // Clean up
    FREE(tmp);
    FREE(basename);
    FREE(test_name);
  }
  FREE(dssr);

  // Check the image data file size
  for (ii=0; ii<nBands; ii++) {
    iof = (struct IOF_VFDR*) MALLOC(sizeof(struct IOF_VFDR));
    if (get_ifiledr((const char *)dataName[ii], iof) == -1)
      asfPrintError("Image data file corrupted\n"
		    "Data QC status: FAILED\n");
    estimated_size = 
      iof->numofrec * iof->reclen + firstRecordLen(dataName[ii]);
    actual_size = fileSize(dataName[ii]);
    if (verbose) {
      asfPrintStatus("File: %s\n", dataName[ii]);
      asfPrintStatus("estimated file size: %d\n", estimated_size);
      asfPrintStatus("actual file size: %d\n", actual_size);
    }
    FREE(iof);
    if (estimated_size != actual_size)
      asfPrintError("Data QC status: FAILED\n");
  }

  // Check the leader/trailer file for CEOS records size
  for (ii=0; ii<trailer+1; ii++) {
    actual_size = fileSize(metaName[ii]);
    fp = FOPEN(metaName[ii], "r");
    total = 0;

    while (1==fread(&bufhdr, 12, 1, fp)) {
      int itype, subtype[3], rec_seq, length, mallocBytes;
      subtype[0] = bufhdr.rectyp[0];
      itype = bufhdr.rectyp[1];
      subtype[1] = bufhdr.rectyp[2];
      subtype[2] = bufhdr.rectyp[3];
      rec_seq = bigInt32(bufhdr.recnum);
      length = bigInt32(bufhdr.recsiz);
      total += length;
      if ((itype==CEOS_FACDR && rec_seq==17 && length<=5000) ||
          (itype==CEOS_FACDR && rec_seq==18)) {
	tfdr = (struct trl_file_des_rec *) 
	  MALLOC(sizeof(struct trl_file_des_rec));
	if (trailer) {
	  if (get_tfdr(metaName[1], tfdr) == - 1)
	    asfPrintError("Trailer file corrupted\n"
			  "Data QC status: FAILED\n");
	}
	else {
	  if (get_tfdr(metaName[0], tfdr) == -1)
	    asfPrintError("Leader file corrupted\n"
			  "Data QC status: FAILED\n");
	}
	mallocBytes = length = tfdr->facdr_len[10];
      }
      else
	mallocBytes = (length>16920) ? length : 16920;
      buff=(unsigned char *)MALLOC(mallocBytes);
      *(struct HEADER *)buff=bufhdr;
      fread((buff)+12, length-12, 1, fp);
    }
    FCLOSE(fp);
    if (verbose) {
      asfPrintStatus("\nFile: %s\n", metaName[ii]);
      asfPrintStatus("reported size of CEOS records in file: %d\n", total);
      asfPrintStatus("actual size: %d\n", actual_size);
    }
    // Some ALOS 1.5 test data had some unaccounted bytes.
    if (actual_size < total)
      asfPrintError("Data QC status: FAILED\n");      
  }

  // Check ALOS trailer file
  if (trailerName) {
    actual_size = fileSize(trailerName);
    fp = FOPEN(trailerName, "r");
    total = 0;
    fread(&bufhdr, 12, 1, fp);
    int length = bigInt32(bufhdr.recsiz);
    total = length;
    buff = (unsigned char *) MALLOC(length);
    tfdr = (struct trl_file_des_rec *)
      MALLOC(sizeof(struct trl_file_des_rec));
    fread((buff)+12, length-12, 1, fp);
    Code_TFDR(buff, tfdr, fromASCII);
    // Add the length for the low resolution image
    total += tfdr->low_res_pixels * tfdr->low_res_lines * tfdr->low_res_bytes;
    FCLOSE(fp);
    FREE(buff);
    if (verbose) {
      asfPrintStatus("\nFile_: %s\n", trailerName);
      asfPrintStatus("reported size of CEOS records in file: %d\n", total);
      asfPrintStatus("actual size: %d\n", actual_size);
    }
    if (actual_size != total)
      asfPrintError("Data QC status: FAILED\n");
  }

  // Check ALOS volume file
  if (volumeName) {
    actual_size = fileSize(volumeName);
    fp = FOPEN(volumeName, "r");
    total = 0;
    while (1==fread(&bufhdr, 12, 1, fp)) {
      int itype, subtype[3], rec_seq, length, mallocBytes;
      subtype[0] = bufhdr.rectyp[0];
      itype = bufhdr.rectyp[1];
      subtype[1] = bufhdr.rectyp[2];
      subtype[2] = bufhdr.rectyp[3];
      rec_seq = bigInt32(bufhdr.recnum);
      length = bigInt32(bufhdr.recsiz);
      total += length;
      if ((itype==CEOS_FACDR && rec_seq==17 && length<=5000) ||
          (itype==CEOS_FACDR && rec_seq==18)) {
        tfdr = (struct trl_file_des_rec *)
          MALLOC(sizeof(struct trl_file_des_rec));
        if (trailer) {
          if (get_tfdr(metaName[1], tfdr) == - 1)
            asfPrintError("Trailer file corrupted\n"
                          "Data QC status: FAILED\n");
        }
        else {
          if (get_tfdr(metaName[0], tfdr) == -1)
            asfPrintError("Leader file corrupted\n"
                          "Data QC status: FAILED\n");
        }
        mallocBytes = length = tfdr->facdr_len[10];
      }
      else
        mallocBytes = (length>16920) ? length : 16920;
      buff=(unsigned char *)MALLOC(mallocBytes);
      *(struct HEADER *)buff=bufhdr;
      fread((buff)+12, length-12, 1, fp);
    }
    FCLOSE(fp);
    if (verbose) {
      asfPrintStatus("\nFile: %s\n", volumeName);
      asfPrintStatus("reported size of CEOS records in file: %d\n", total);
      asfPrintStatus("actual size: %d\n", actual_size);
    }
    if (actual_size != total)
      asfPrintError("Data QC status: FAILED\n");
  }

  // Check workreport file
  if (workreport) {
    actual_size = fileSize(workreport);
    if (verbose)
      asfPrintStatus("\nFile: %s\nactual size: %d\n", workreport, actual_size);
    if (actual_size == 0)
      asfPrintError("Data QC status: FAILED\n");
  }

  // If you made it here, then all the test passed
  asfPrintStatus("\nData QC status: SUCCESS\n");

  // Clean up
  if (tfdr)
    FREE(tfdr);
  free_ceos_names(dataName, metaName);  
}

// Main program body.
int
main (int argc, char *argv[])
{
  char *inFile;
  int currArg = 1;
  int NUM_ARGS = 1;
  int verbose = FALSE;

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
    else if (strmatches(key,"-verbose","--verbose","-v",NULL)) {
      verbose = TRUE;
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

  data_qc(inFile, verbose);
  asfPrintStatus("\nDone.\n");

  return EXIT_SUCCESS;
}
