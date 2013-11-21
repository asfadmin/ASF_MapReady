#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "ceos.h"
#include "asf.h"
#include "asf_meta.h"
#include "get_ceos_names.h"
#include "metadisplay.h"
#include "cla.h"

#ifdef win32
#include <unistd.h>
#include <signal.h>
#include <process.h>
#endif

void add(char **s, const char *format, ...)
{
    const int line_len = 512;
    static char line[512];

    va_list ap;
    va_start(ap,format);
    int n = vsnprintf(line, line_len, format, ap);
    va_end(ap);

    if (n<0 || n>=line_len)
        asfPrintWarning("Truncated line.\n");

    int curr = strlen(*s);
    char *new = MALLOC(sizeof(char)*(curr+strlen(line)+1));
    strcpy(new, *s);
    strcat(new, line);
    FREE(*s);
    *s = new;
}

void output_record(char *fileName, char *extension, int rec, int save)
{
  FILE *fp;
  char *outName;

    if (save) {
      outName = (char *) MALLOC(sizeof(char)*255);
      outName = appendExt(fileName, extension);
      fp = FOPEN(outName, "w");
      print_record(fp, fileName, rec);
      FCLOSE(fp);
      FREE(outName);
    }
    else {
      fp = stdout;
      print_record(fp, fileName, rec);
    }
}

int output_all_metadata(char *infile, char *outfile) 
{
  FILE *fp = FOPEN(outfile, "w");
  char *rec = get_record_as_string(infile, 10); // .dssr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 18); // .shr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 20); // .mpdr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 30); // .ppdr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 40); // .atdr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 44); // .ampr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 50); // .radr/.ardr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 50); // .radr/.ardr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 51); // .atdr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 60); // .dqsr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 70); // .pdhr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 71); // .shdr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 80); // .rasr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 120); // .ppr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 192); // .ifdr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 200); // .facdr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 300); // .lfdr
  fprintf(fp, "%s", rec);
  FREE(rec);
  rec = get_record_as_string(infile, 193); // .tfdr
  fprintf(fp, "%s", rec);
  FREE(rec);
  FCLOSE(fp);
  
  return TRUE;
}

void print_record(FILE *fp, char *fileName, int reqrec)
{
    char *rec = get_record_as_string(fileName, reqrec);
    fprintf(fp, "%s", rec);
    FREE(rec);
}

char *get_record_as_string(char *fileName, int reqrec)
{
  struct VFDRECV *facdr=NULL;            // Facility Related Data record
  struct VRADDR *raddr=NULL;             // Radiometric Data record
  struct IOF_VFDR *vfdr=NULL;            // Image File Descriptor record
  struct VMPDREC *mpdrec=NULL;           // Map Projection Data record
  struct FDR *fdr=NULL;                  // File Descriptor record
  struct dataset_sum_rec *dssr=NULL;     // Data Set Summary record
  struct pos_data_rec *ppdr=NULL;        // Platform Position Data record
  struct att_data_rec *atdr=NULL;        // Attitude Data record
  struct data_hist_rec *dhr=NULL;        // Data Histogram record
  struct rng_spec_rec *rsr=NULL;         // Range Spectra record
  struct qual_sum_rec *dqsr=NULL;        // Data Quality Summary record
  struct radio_comp_data_rec *rcdr=NULL; // Radiometric Compensation Data record
  struct scene_header_rec *shr=NULL;     // Scene Header record
  struct alos_map_proj_rec *ampr=NULL;   // Map Projection Data record - ALOS
  struct ESA_FACDR *esa_facdr=NULL;      // Facility Related Data (ESA) record
  struct JAXA_FACDR *jaxa_facdr=NULL;    // Facility Related Data (JAXA) record
  struct proc_parm_rec *ppr=NULL;        // Processing Parameter record
  struct alos_rad_data_rec *ardr=NULL;   // Radiometric Data record (ALOS)
  struct RSI_VRADDR *rsi_raddr=NULL;     // Radiometric Data record (RSI/CDPF)
  struct trl_file_des_rec *tfdr=NULL;    // Trailer File Descriptor record - ALOS

  ceos_data_ext_t data_ext;
  ceos_metadata_ext_t metadata_ext;
  char **dataNames=NULL, **metaName=NULL;
  char baseName[1024];
  int nBands, trailer, dataNameExists=1, leaderNameExists=1;
  char rectype_str[64];
  char ret_str[128];
  char *ret=NULL;
  char facility[25];
  char sensor[16];

  strcpy(rectype_str, "Unknown");
  strcpy(ret_str, "");

  metadata_ext = get_ceos_metadata_name(fileName, &metaName, &trailer);
  data_ext = get_ceos_data_name(fileName, baseName, &dataNames, &nBands);

  // if fileName had a path -- add that back on to baseName
  char *path = get_dirname(fileName);
  if (path && strlen(path) > 0) {
    char *tmp = STRDUP(baseName);
    sprintf(baseName, "%s/%s", path, tmp);
    free(tmp);
  }
  free(path);

  if (data_ext == NO_CEOS_DATA) {
    if (192 == reqrec) {
      if (nBands > 0)
        asfPrintWarning("Data file (%s) missing.\n"
                        "Unable to extract Image File Descriptor Record.\n",
                        dataNames[0]);
      else
        asfPrintWarning("Data file missing.\n"
                        "Unable to extract Image File Descriptor Record.\n");
    }
    dataNameExists = 0;
  }
  if (metadata_ext == NO_CEOS_METADATA) {
    if (192 != reqrec) {
      asfPrintWarning("Leader (metadata) file missing.\n"
	              "ONLY able to extract Image File Descriptor Record.\n");
    }
    leaderNameExists = 0;
  }
  if (!dataNameExists && !leaderNameExists)
      return STRDUP("Not able to open either data or leader file.\n");

  // Determine a couple things from the dssr before we get going since
  // the way some records are read depend on which facility the data was
  // processed at, which satellite captured the data, etc, etc
  dssr = (struct dataset_sum_rec *) CALLOC(1, sizeof(struct dataset_sum_rec));
  if (leaderNameExists) {
    if (get_dssr(metaName[0],dssr) >= 0) {
        strcpy(sensor, trim_spaces(dssr->mission_id));
        strcpy(facility, trim_spaces(dssr->fac_id));
    }
    else if (trailer && get_dssr(metaName[1],dssr) >= 0) {
        strcpy(sensor, trim_spaces(dssr->mission_id));
        strcpy(facility, trim_spaces(dssr->fac_id));
    }
    else {
        strcpy(sensor, MAGIC_UNSET_STRING);
        strcpy(facility, MAGIC_UNSET_STRING);
        FREE(dssr);
        dssr = NULL;
    }
  }
  else {
     strcpy(sensor, MAGIC_UNSET_STRING);
     strcpy(facility, MAGIC_UNSET_STRING);
  }

  // Set specific facility data record to retrieve
  if ( reqrec == 200 ) {
    if (0 == strncmp(facility, "ASF", 3))
      reqrec = 210;
    if (0 == strncmp(facility, "ES", 2) ||
	0 == strncmp(facility, "CSTARS", 6) ||
	0 == strncmp(facility, "D-PAF", 5) ||
	0 == strncmp(facility, "I-PAF", 5) ||
	0 == strncmp(facility, "Beijing", 7))
      reqrec = 220; // ESA style
    if (0 == strncmp(facility, "EOC", 3))
      reqrec = 230; // JAXA style
  }
  // Make sure the map projection record for ALOS is found, regardless of
  // which switch is called
  ceos_satellite_t ceos_satellite;
  ceos_sensor_t ceos_sensor;
  get_satellite_sensor (fileName, &ceos_satellite, &ceos_sensor, REPORT_LEVEL_NONE);
  if ((ceos_sensor == AVNIR || ceos_sensor == PRISM) && reqrec == 20)
    reqrec = 44;

  switch (reqrec)
    {
    case (10):
      strcpy(rectype_str, "Data set summary");
      if (dssr)
        ret = sprn_dssr(dssr,1);
      break;
    case (18):
      strcpy(rectype_str, "Scene header");
      shr = (struct scene_header_rec *) MALLOC(sizeof(struct scene_header_rec));
      if (leaderNameExists) {
	if (get_shr(metaName[0],shr) >= 0 )
	  ret = sprn_shr(shr);
	else if (trailer) {
	  if (get_shr(metaName[1],shr) >= 0 )
	    ret = sprn_shr(shr);
	}
      }
      FREE(shr);
      break;
    case (20):
      strcpy(rectype_str, "Map projection data");
      mpdrec = (struct VMPDREC *) MALLOC(sizeof(struct VMPDREC));
      if (leaderNameExists) {
	if (get_mpdr(metaName[0],mpdrec) >= 0 )
	  ret = sprn_mpdr(mpdrec);
	else if (trailer) {
	  if (get_mpdr(metaName[1],mpdrec) >= 0 )
	    ret = sprn_mpdr(mpdrec);
	}
      }
      FREE(mpdrec);
      break;
    case (30):
      strcpy(rectype_str, "Platform position data");
      ppdr = (struct pos_data_rec *) MALLOC(sizeof(struct pos_data_rec));
      if (leaderNameExists) {
	if (get_ppdr(metaName[0],ppdr) >= 0)
	  ret = sprn_ppdr(ppdr);
	else if (trailer) {
	  if (get_ppdr(metaName[1],ppdr) >= 0)
	    ret = sprn_ppdr(ppdr);
	}
      }
      FREE(ppdr);
      break;
    case (40):
      strcpy(rectype_str, "Attitude data");
      atdr = (struct att_data_rec *) MALLOC(sizeof(struct att_data_rec));
      if (leaderNameExists) {
	if (get_atdr(metaName[0],atdr) >= 0)
	  ret = sprn_atdr(atdr);
	else if (trailer) {
	  if (get_atdr(metaName[1],atdr) >= 0)
	    ret = sprn_atdr(atdr);
	}
      }
      FREE(atdr);
      break;
    case (44):
      strcpy(rectype_str, "ALOS map projection data");
      ampr = (struct alos_map_proj_rec *) MALLOC(sizeof(struct alos_map_proj_rec));
      if (leaderNameExists) {
	if (get_ampr(metaName[0],ampr) >= 0)
	  ret = sprn_ampr(ampr);
	else if (trailer) {
	  if (get_ampr(metaName[1],ampr) >= 0)
	    ret = sprn_ampr(ampr);
	}
      }
      FREE(ampr);
      break;
    case (50):
        if (strncmp(sensor, "ALOS", 4) == 0) {
            strcpy(rectype_str, "ALOS radiometric data");
            ardr = (struct alos_rad_data_rec *)
                   MALLOC (sizeof(struct alos_rad_data_rec));
            if (leaderNameExists) {
                if (get_ardr(metaName[0],ardr) >= 0)
                    ret = sprn_ardr(ardr);
                else if (trailer) {
                    if (get_ardr(metaName[1],ardr) >= 0)
                        ret = sprn_ardr(ardr);
                }
            }
            FREE(ardr);
        }
	else if (strncmp(facility, "CDPF", 4) == 0 ||
		 strncmp(facility, "RSI", 3) == 0 ||
		 strncmp(facility, "CSTARS", 6) == 0) {
          strcpy(rectype_str, "RSI radiometric data");
	  rsi_raddr = (struct RSI_VRADDR  *) MALLOC(sizeof(struct RSI_VRADDR));
	  if (leaderNameExists) {
	    if (get_rsi_raddr(metaName[0],rsi_raddr) >= 0)
	      ret = sprn_rsi_raddr(rsi_raddr);
	    else if (trailer) {
	      if (get_rsi_raddr(metaName[1],rsi_raddr) >= 0)
		ret = sprn_rsi_raddr(rsi_raddr);
	    }
	  }
	  FREE(rsi_raddr);
	}
	else {
            strcpy(rectype_str, "Radiometric data");
            raddr = (struct VRADDR  *) MALLOC(sizeof(struct VRADDR));
            if (leaderNameExists) {
                if (get_raddr(metaName[0],raddr) >= 0)
                    ret = sprn_raddr(raddr);
                else if (trailer) {
                    if (get_raddr(metaName[1],raddr) >= 0)
                        ret = sprn_raddr(raddr);
                }
            }
            FREE(raddr);
        }
      break;
    case (51):
      strcpy(rectype_str, "Radiometric compensation data");
      rcdr = (struct radio_comp_data_rec *)
	MALLOC(sizeof(struct radio_comp_data_rec));
      if (leaderNameExists) {
	if (get_rcdr(metaName[0],rcdr) >= 0)
	  ret = sprn_rcdr(rcdr);
	else if (trailer) {
	  if (get_rcdr(metaName[1],rcdr) >= 0)
	    ret = sprn_rcdr(rcdr);
	}
      }
      FREE(rcdr);
      break;
    case (60):
      strcpy(rectype_str, "Data quality summary");
      dqsr = (struct qual_sum_rec *) MALLOC(sizeof(struct qual_sum_rec));
      if (leaderNameExists) {
	if (get_dqsr(metaName[0],dqsr) >= 0)
	  ret = sprn_dqsr(dqsr,1);
	else if (trailer) {
	  if (get_dqsr(metaName[1],dqsr) >= 0)
	    ret = sprn_dqsr(dqsr,1);
	}
      }
      FREE(dqsr);
      break;
    case (70):
      strcpy(rectype_str, "Processed data histograms");
      dhr = (struct data_hist_rec *) MALLOC(sizeof(struct data_hist_rec));
      if (leaderNameExists) {
	if (get_dhr(metaName[0],dhr) >= 0)
	  ret = sprn_dhr(dhr);
	else if (trailer) {
	  if (get_dhr(metaName[1],dhr) >= 0)
	    ret = sprn_dhr(dhr);
	}
      }
      FREE(dhr);
      break;
    case (71):
      strcpy(rectype_str, "Signal data histograms");
      dhr = (struct data_hist_rec *) MALLOC(sizeof(struct data_hist_rec));
      if (leaderNameExists) {
	if (get_sdhr(metaName[0],dhr) >= 0)
	  ret = sprn_dhr(dhr);
	else if (trailer) {
	  if (get_sdhr(metaName[1],dhr) >= 0)
	    ret = sprn_dhr(dhr);
	}
      }
      FREE(dhr);
      break;
    case (80):
      strcpy(rectype_str, "Range spectra");
      if (0 != strncmp(facility, "EOC", 3)) {
	rsr = (struct rng_spec_rec *) MALLOC(sizeof(struct rng_spec_rec));
	if (leaderNameExists) {
	  if (get_rsr(metaName[0],rsr) >= 0)
	    ret = sprn_rsr(rsr);
	  else if (trailer) {
	    if (get_rsr(metaName[1],rsr) >= 0)
	      ret = sprn_rsr(rsr);
	  }
	}
	FREE(rsr);
      }
      break;
    case (120):
      strcpy(rectype_str, "Processing parameter");
      if (0 != strncmp(facility, "EOC", 3)) {
	ppr = (struct proc_parm_rec *) MALLOC(sizeof(struct proc_parm_rec));
	if (leaderNameExists) {
	  if (get_ppr(metaName[0],ppr) >= 0)
	    ret = sprn_ppr(ppr);
	  else if (trailer) {
	    if (get_ppr(metaName[1],ppr) >= 0)
	      ret = sprn_ppr(ppr);
	  }
	}
	FREE(ppr);
      }
      break;
    case (192):
      strcpy(rectype_str, "Image file descriptor");
      vfdr = (struct IOF_VFDR *) MALLOC(sizeof(struct IOF_VFDR));
      if (dataNameExists) {
	if (get_ifiledr(baseName,vfdr) >= 0)
	  ret = sprn_ifiledr(vfdr);
      }
      FREE(vfdr);
      break;
    case (193):
      strcpy(rectype_str, "Trailer file descriptor");
      if (0 == strncmp(facility, "EOC", 3)) {
	tfdr = (struct trl_file_des_rec *)
	  MALLOC(sizeof(struct trl_file_des_rec));
	if (dataNameExists) {
	  if (get_tfdr(baseName,tfdr) >= 0)
	    ret = sprn_tfdr(tfdr);
	}
	FREE(tfdr);
      }
      break;
    case (200):
      asfPrintWarning("Unrecogized facility data record...\n"
                      "Assuming ASF format.\n");
    case (210):
      strcpy(rectype_str, "ASF facility related data");
      facdr = (struct VFDRECV *) MALLOC(sizeof(struct VFDRECV));
      if (leaderNameExists) {
	if (get_asf_facdr(metaName[0],facdr) >= 0)
	  ret = sprn_facdr(facdr,1);
	else if (trailer) {
	  if (get_asf_facdr(metaName[1],facdr) >= 0)
	    ret = sprn_facdr(facdr,1);
	}
      }
      FREE(facdr);
      break;
    case (220):
      strcpy(rectype_str, "ESA facility related data");
      esa_facdr = (struct ESA_FACDR *) MALLOC(sizeof(struct ESA_FACDR));
      if (leaderNameExists) {
	if (get_esa_facdr(metaName[0],esa_facdr) >= 0)
	  ret = sprn_esa_facdr(esa_facdr);
	else if (trailer) {
	  if (get_esa_facdr(metaName[1],esa_facdr) >= 0)
	    ret = sprn_esa_facdr(esa_facdr);
	}
      }
      FREE(esa_facdr);
      break;
    case (230):
      strcpy(rectype_str, "JAXA facility related data");
      jaxa_facdr = (struct JAXA_FACDR *) MALLOC(sizeof(struct JAXA_FACDR));
      struct trl_file_des_rec *tfdr =
	(struct trl_file_des_rec *) MALLOC(sizeof(struct trl_file_des_rec));
      if (leaderNameExists) {
	if (get_jaxa_facdr(metaName[0],jaxa_facdr) >= 0) {
	  get_tfdr(metaName[0], tfdr);
	  ret = sprn_jaxa_facdr(jaxa_facdr, tfdr->facdr_len[10]);
	}
	else if (trailer) {
	  if (get_jaxa_facdr(metaName[1],jaxa_facdr) >= 0) {
	    get_tfdr(metaName[1], tfdr);
	    ret = sprn_jaxa_facdr(jaxa_facdr, tfdr->facdr_len[10]);
	  }
	}
      }
      FREE(jaxa_facdr);
      break;
    case (300):
      strcpy(rectype_str, "Leader file descriptor");
      fdr = (struct FDR *) MALLOC(sizeof(struct FDR));
      if (leaderNameExists) {
	if (get_fdr(metaName[0],fdr) >= 0)
	  ret = sprn_fdr(fdr);
	else if (trailer) {
	  if (get_fdr(metaName[1],fdr) >= 0)
	    ret = sprn_fdr(fdr);
	}
      }
      FREE(fdr);
      break;
    default:
      asfPrintStatus("Not a valid record type\n");
      break;
    }
  FREE(dssr);
  free_ceos_names(dataNames, metaName);
  sprintf(ret_str,"%s record not found.\n\n", rectype_str);
  if (!ret)
      return STRDUP(ret_str);
  return ret;
}
