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
"   "ASF_NAME_STRING" [-ignore_spec] [-essential] <in_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program verifies the validity of a CEOS data set.\n\n"

#define ASF_INPUT_STRING \
"     The input file is required, and should be the basename of a CEOS\n"\
"     data set.\n"

#define ASF_OPTIONS_STRING \
"     -ignore_spec\n"\
"          Does not test whether the actual file size meets the "\
"specification.\n"\
"     -essential\n"\
"          Only checks image data file and leader file.\n"\
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

void data_qc(char *ceosName, int ignore_spec, int essential)
{
  FILE *fp;
  struct HEADER bufhdr;
  struct dataset_sum_rec *dssr=NULL;
  struct IOF_VFDR *iof=NULL;
  struct trl_file_des_rec *tfdr=NULL;
  unsigned char *buff;
  char **dataName=NULL, **metaName=NULL, *trailerName=NULL, *volumeName=NULL;
  char *workreport=NULL, reason[1024]="", tmp[255];
  int ii, nBands=0, trailer, total, estimated_size, actual_size, alos=FALSE;
  int level, beam, spec=TRUE, status=TRUE, no_leader=FALSE;
  int foundWorkreport=FALSE;

  // Verify that the essential files exist. Require_ceos_pair takes care 
  // of the essentials: data file, leader/trailer file
  // But first we need to check whether the leader file exists.
  ceos_metadata_ext_t ret =
    get_ceos_metadata_name(ceosName, &metaName, &trailer);
  if (!ret) {
    strcat(reason, "Missing leader file!\nAborted all other tests.\n");
    status = FALSE;
    no_leader = TRUE;
    trailer = -1;
  }
  else
    require_ceos_pair(ceosName, &dataName, &metaName, &nBands, &trailer);

  if (!essential) {
    if (!no_leader) {
      dssr = (struct dataset_sum_rec *) MALLOC(sizeof(struct dataset_sum_rec));
      if (get_dssr((const char *)metaName[0], dssr) == -1) {
	strcat(reason, "Leader file corrupted\n");
	status = FALSE;
      }

      // Generate a metadata structure. We need to know what data we are
      // dealing with since the beam number index is apparently not reliable
      // for all data types (does not cover multi-band data properly)
      meta_parameters *meta = meta_read(metaName[0]);
	
      if (strcmp(meta->general->sensor, "ALOS") == 0 &&
	  strcmp(meta->general->sensor_name, "SAR") == 0) {
	char *tmp = (char *) MALLOC(sizeof(char)*512);
	tmp = get_basename(ceosName);
	char *test_name = (char *) MALLOC(sizeof(char)*512);
	char *basename = strip_alos_prep(tmp);
	alos = TRUE;
	
	// Check what level we are dealing with
	if (strncmp(dssr->lev_code, "1.0", 3) == 0)
	  level = 10;
	else if (strncmp(dssr->lev_code, "1.1", 3) == 0)
	  level = 11;
	else if (strncmp(dssr->lev_code, "1.5", 3) == 0)
	  level = 15;
	
	// Determine the specified image file range
	beam = dssr->ant_beam_num;
	if (dssr->nchn == 2) // dual-pol data
	  beam = 39;
	if (dssr->nchn == 4) // quad-pol data
	  beam = 127;
	if (meta->projection && meta->projection->type == SCANSAR_PROJECTION)
	  beam = 80;
	if (dssr->product_id[5] == 'S')
	  beam = 80;
	
	// Check for image data files based on beam information
	if ((beam >= 0 && beam <= 17) ||
	    (beam >= 84 && beam <= 101)) { // HH polarization
	  sprintf(test_name, "IMG-HH-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp) {
	    sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	    strcat(reason, tmp);
	    status = FALSE;
	  }
	  else
	    fclose(fp);
	}
	else if ((beam >= 18 && beam <= 35) ||
		 (beam >= 102 && beam <= 119)) { // VV polarization
	  sprintf(test_name, "IMG-VV-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp) {
	    sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	    strcat(reason, tmp);
	    status = FALSE;
	  }
	  else
	    fclose(fp);
	}
	else if (beam >= 36 && beam <= 71) { // (HH+HV) or (VV+VH) polarization
	  int hh=TRUE, hv=TRUE, vh=TRUE, vv=TRUE, dual_hh=FALSE, dual_vv=FALSE;
	  sprintf(test_name, "IMG-HH-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp)
	    hh = FALSE;
	  else
	    fclose(fp);
	  sprintf(test_name, "IMG-HV-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp)
	    hv = FALSE;
	  else 
	    fclose(fp);
	  sprintf(test_name, "IMG-VV-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp)
	    vh = FALSE;
	  else
	    fclose(fp);
	  sprintf(test_name, "IMG-VH-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp) 
	    vv = FALSE;
	  else 
	    fclose(fp);

	  if (hh || hv)
	    dual_hh = TRUE;
	  if (vv || vh)
	    dual_vv = TRUE;
	  if (dual_hh) {
	    if (!hh) {
	      sprintf(test_name, "IMG-HH-%s", basename);
	      sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	      strcat(reason, tmp);
	      status = FALSE;
	    }
	    if (!hv) {
	      sprintf(test_name, "IMG-HV-%s", basename);
	      sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	      strcat(reason, tmp);
	      status = FALSE;
	    }
	  }
	  if (dual_vv) {
	    if (!vv) {
	      sprintf(test_name, "IMG-VV-%s", basename);
	      sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	      strcat(reason, tmp);
	      status = FALSE;
	    }
	    if (!vh) {
	      sprintf(test_name, "IMG-VH-%s", basename);
	      sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	      strcat(reason, tmp);
	      status = FALSE;
	    }
	  }
	  if (!dual_hh && !dual_vv) {
	    strcat(reason, "Both image data files are missing!\n");
	    status = FALSE;
	  }

	}
	else if (beam == 80) { // ScanSAR case
	  int hh=TRUE, vv=TRUE, scansar_hh=FALSE, scansar_vv=FALSE;
	  sprintf(test_name, "IMG-HH-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp)
	    hh = FALSE;
	  else
	    fclose(fp);
	  sprintf(test_name, "IMG-VV-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp)
	    vv = FALSE;
	  else
	    fclose(fp);

	  if (hh)
	    scansar_hh = TRUE;
	  if (vv)
	    scansar_vv = TRUE;
	  
	  if (!hh && !scansar_vv) {
	    sprintf(test_name, "IMG-HH-%s", basename);
	    sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	    strcat(reason, tmp);
	    status = FALSE;
	  } 
	  if (!vv && !scansar_hh) {
	    sprintf(test_name, "IMG-VV-%s", basename);
	    sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	    strcat(reason, tmp);
	    status = FALSE;
	  }
	  if (!scansar_hh && !scansar_vv) {
	    strcat(reason, "Missing image data file!\n");
	    status = FALSE;
	  }
	}
	else if (beam >= 120 && beam <= 131) { // HH+HV+HV+HH polarization
	  sprintf(test_name, "IMG-HH-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp) {
	    sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	    strcat(reason, tmp);
	    status = FALSE;
	  }
	  else
	    fclose(fp);
	  sprintf(test_name, "IMG-HV-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp) {
	    sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	    strcat(reason, tmp);
	    status = FALSE;
	  }
	  else 
	    fclose(fp);
	  sprintf(test_name, "IMG-VH-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp) {
	    sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	    strcat(reason, tmp);
	    status = FALSE;
	  }
	  else
	    fclose(fp);
	  sprintf(test_name, "IMG-VV-%s", basename);
	  fp = fopen(test_name, "r"); 
	  if (!fp) {
	    sprintf(tmp, "Missing image data file (%s)!\n", test_name);
	    strcat(reason, tmp);
	    status = FALSE;
	  }
	  else 
	    fclose(fp);
	}

	// Check for trailer file
	sprintf(test_name, "TRL-%s", basename);
	fp = fopen(test_name, "r"); 
	if (fp) {
	  fclose(fp);
	  trailerName = (char *) MALLOC(sizeof(char)*1024);
	  sprintf(trailerName, "%s", test_name);
	}
	else {
	  strcat(reason, "Missing trailer file!\n");
	  status = FALSE;
	}
	
	// Check for volume file
	sprintf(test_name, "VOL-%s", basename);
	fp = fopen(test_name, "r");
	if (fp) {
	  fclose(fp);
	  volumeName = (char *) MALLOC(sizeof(char)*1024);
	  sprintf(volumeName, "%s", test_name);
	}
	else {
	  strcat(reason, "Missing volume file!\n");
	  status = FALSE;
	}
	
	// Check for workreport file
	sprintf(test_name, "%s.txt", basename);
	fp = fopen(test_name, "r");
	if (fp){
	  fclose(fp);
	  workreport = (char *) MALLOC(sizeof(char)*1024);
	  sprintf(workreport, "%s", test_name);
	  foundWorkreport = TRUE;
	}
	strcpy(test_name, "workreport");
	fp = fopen(test_name, "r");
	if (fp){
	  fclose(fp);
	  workreport = (char *) MALLOC(sizeof(char)*1024);
	  strcpy(workreport, "workreport");
	  foundWorkreport = TRUE;
	}	
	if (!foundWorkreport) {
	  strcat(reason, "Missing workreport file!\n");
	  status = FALSE;
	}
	
	// Clean up
	FREE(basename);
	FREE(test_name);
      }
      FREE(dssr);
      meta_free(meta);
    }
  }

  // Check the image data file size
  for (ii=0; ii<nBands; ii++) {
    iof = (struct IOF_VFDR*) MALLOC(sizeof(struct IOF_VFDR));
    if (get_ifiledr((const char *)dataName[ii], iof) == -1) {
      strcat(reason, "Image data file corrupted\n");
      status = FALSE;
    }
    estimated_size =
      iof->numofrec * iof->reclen + firstRecordLen(dataName[ii]);
    actual_size = fileSize(dataName[ii]);
    if (!quietflag) {
      asfPrintStatus("File: %s\n", dataName[ii]);
      asfPrintStatus("estimated file size: %d bytes (%.1f MB)\n", 
		     estimated_size, (float)estimated_size/1024/1024);
      asfPrintStatus("actual file size: %d bytes (%.1f MB)\n", 
		     actual_size, (float)actual_size/1024/1024);
    }
    FREE(iof);
    if (alos && level == 10) {
      float spec_min, spec_max;
      if (beam == 3 || beam == 21) { // FSB at 21.5 deg
	spec_min = 414292378; // 395.1 MB
	spec_max = 450373878; // 429.51 MB
      }
      else if (beam == 7 || beam == 25) { // FSB at 34.3 deg
	spec_min = 704779387; // 672.13 MB
	spec_max = 750633615; // 715.86 MB
      }
      else if (beam == 10 || beam == 28) { // FSB at 41.5 deg
	spec_min = 704779387; // 678.26 MB
	spec_max = 916140852; // 873.70 MB
      }
      else if (beam == 39 || beam == 57) { // FSD at 21.5 deg
	spec_min = 428584469 / 2; // 408.73 MB
	spec_max = 465903288 / 2; // 444.32 MB
      }
      else if (beam == 43 || beam == 61) { // FSD at 34.3 deg
	spec_min = 721483203 / 2; // 688.06 MB
	spec_max = 768417464 / 2; // 732.82 MB
      }
      else if (beam == 46 || beam == 64) { // FSD at 41.5 deg
	spec_min = 367735603 / 2; // 350.70 MB
	spec_max = 392586854 / 2; // 374.40 MB
      }
      else if (beam == 80 || beam == 82) { // ScanSAR (5 scan)
	spec_min = 1204572652; // 1148.77 MB
	spec_max = 1296637624; // 1236.57 MB
      }
      else if (beam == 87 || beam == 105) { // Direct downlink at 21.5 deg
	spec_min = 214286991; // 204.36 MB
	spec_max = 232951644; // 222.16 MB
      }
      else if (beam == 91 || beam == 109) { // Direct downlink at 34.3 deg
	spec_min = 360741601; // 344.03 MB
	spec_max = 384208732; // 366.41 MB
      }
      else if (beam == 94 || beam == 112) { // Direct downlink at 41.5 deg
	spec_min = 367735603; // 350.70 MB
	spec_max = 392586854; // 374.40 MB
      }
      else if (beam == 127) { // Polarimetric data at 21.5 deg
	spec_min = 430943764 / 4; // 410.98 MB
	spec_max = 459465032 / 4; // 438.18 MB
      }
      else {
	sprintf(tmp, "No specs for beam index %d available\n", beam);
	strcat(reason, tmp);
	spec = FALSE;
      }
      if (spec && actual_size < spec_min) {
	sprintf(tmp, "Image file smaller than JAXA specified range "
		"(%.1f MB).\n", spec_min / 1024 / 1024);
	strcat(reason, tmp);
	spec = FALSE;
      }
      else if (spec && actual_size > spec_max) {
	sprintf(tmp, "Image file larger than JAXA specified range "
		"(%.1f MB).\n", spec_max / 1024 / 1024);
	strcat(reason, tmp);
	spec = FALSE;
      }
    }
    if (estimated_size != actual_size || (!spec && !ignore_spec)) {
      sprintf(tmp, "Band %d: estimated file size differs from the actual file size "
	      "(%.0f bytes).\n", ii, fabs(estimated_size - actual_size));
      strcat(reason, tmp);
      status = FALSE;
    }
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
	  if (get_tfdr(metaName[1], tfdr) == - 1) {
	    strcat(reason, "Trailer file corrupted\n");
	    status = FALSE;
	  }
	}
	else {
	  if (get_tfdr(metaName[0], tfdr) == -1) {
	    strcat(reason, "Leader file corrupted\n");
	    status = FALSE;
	  }
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
    if (!quietflag) {
      asfPrintStatus("\nFile: %s\n", metaName[ii]);
      asfPrintStatus("reported size of CEOS records in file: %d bytes\n", 
		     total);
      asfPrintStatus("actual size: %d bytes\n", actual_size);
    }
    // Some ALOS 1.5 test data had some unaccounted bytes.
    if (actual_size < total) {
      sprintf(tmp, "Actual size of the leader file smaller than estimated "
	      "(%d).\n", (int) fabs(actual_size - total));
      strcat(reason, tmp);
      status = FALSE;
    }
  }

  if (!essential) {
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
      total += 
	tfdr->low_res_pixels * tfdr->low_res_lines * tfdr->low_res_bytes;
      FCLOSE(fp);
      FREE(buff);
      if (!quietflag) {
	asfPrintStatus("\nFile_: %s\n", trailerName);
	asfPrintStatus("reported size of CEOS records in file: %d bytes\n", 
		       total);
	asfPrintStatus("actual size: %d bytes\n", actual_size);
      }
      if (actual_size != total) {
	sprintf(tmp, "Actual and estimated size of trailer file differ "
		"(%d).\n", (int) fabs(actual_size - total));
	strcat(reason, tmp);
	status = FALSE;
      }
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
	    if (get_tfdr(metaName[1], tfdr) == - 1) {
	      asfForcePrintStatus("Trailer file corrupted\n");
	      status = FALSE;
	    }
	  }
	  else {
	    if (get_tfdr(metaName[0], tfdr) == -1) {
	      asfForcePrintStatus("Leader file corrupted\n");
	      status = FALSE;
	    }
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
      if (!quietflag) {
	asfPrintStatus("\nFile: %s\n", volumeName);
	asfPrintStatus("reported size of CEOS records in file: %d bytes\n", 
		       total);
	asfPrintStatus("actual size: %d bytes\n", actual_size);
      }
      if (actual_size != total) {
	sprintf(tmp, "Actual and estimated size of the volume file differ "
		"(%d).\n", (int) fabs(actual_size - total));
	strcat(reason, tmp);
	status = FALSE;
      }
    }
    
    // Check workreport file
    if (workreport) {
      actual_size = fileSize(workreport);
      if (!quietflag)
	asfPrintStatus("\nFile: %s\nactual size: %d bytes\n", 
		       workreport, actual_size);
      if (actual_size == 0) {
	strcat(reason, "Zero file length workreport file.\n");
	status = FALSE;
      }
    }
  }

  // Report status
  if (status)
    asfForcePrintStatus("\nData QC status: SUCCESS\n");
  else
    asfPrintError("Data QC status: FAILED\n%s", reason);

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
  int essential = FALSE;
  int ignore_spec = FALSE;

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
    else if (strmatches(key,"-essential","--essential","-e",NULL)) {
      essential = TRUE;
    }
    else if (strmatches(key,"-ignore_spec","--ignore_spec","-i",NULL)) {
      ignore_spec = TRUE;
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

  data_qc(inFile, ignore_spec, essential);
  asfForcePrintStatus("\nDone.\n");

  return EXIT_SUCCESS;
}
