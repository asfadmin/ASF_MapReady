#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "ceos.h"
#include "asf.h"
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
      FREE(fp);
    }
    else {
      fp = stdout;
      print_record(fp, fileName, rec);
    }
}

void print_record(FILE *fp, char *fileName, int reqrec)
{
    char *rec = get_record_as_string(fileName, reqrec);
    fprintf(fp, "%s", rec);
    FREE(rec);
}

static char *get_base_name(const char *fileName)
{
    char *dir = MALLOC(sizeof(char)*(strlen(fileName)+1));
    char *file = MALLOC(sizeof(char)*(strlen(fileName)+1));

    split_dir_and_file(fileName, dir, file);

    char *s;
    if (strncmp(file, "LED-", 4) == 0 || strncmp(file, "IMG-", 4) == 0)
        s = file + 4;
    else
        s = file;

    char *ret = MALLOC(sizeof(char)*(strlen(fileName)+1));
    sprintf(ret, "%s%s", dir, s);

    FREE(file);
    FREE(dir);

    return ret;
}

char *get_record_as_string(char *fileName, int reqrec)
{
  struct VFDRECV *facdr;                // Facility Related Data record
  struct VRADDR *raddr;                 // Radiometric Data record
  struct IOF_VFDR *vfdr;                // Image File Descriptor record
  struct VMPDREC *mpdrec;               // Map Projection Data record
  struct FDR *fdr;                      // File Descriptor record
  struct dataset_sum_rec *dssr;         // Data Set Summary record
  struct pos_data_rec *ppdr;            // Platform Position Data record
  struct att_data_rec *atdr;            // Attitude Data record
  struct data_hist_rec *dhr;            // Data Histogram record
  struct rng_spec_rec *rsr;             // Range Spectra record
  struct qual_sum_rec *dqsr;            // Data Quality Summary record
  struct radio_comp_data_rec *rcdr;     // Radiometric Compensation Data record
  struct scene_header_rec *shr;         // Scene Header record
  struct alos_map_proj_rec *ampr;       // Map Projection Data record - ALOS
  struct ESA_FACDR *esa_facdr;          // Facility Related Data (ESA) record
  struct PPREC *ppr;                    // Processing Parameter record
  struct alos_rad_data_rec *ardr;       // Radiometric Data record

  char **dataNames, **metaName;
  int ii, nBands, trailer, dataNameExists, leaderNameExists;
  char *ret=NULL;

  dataNames = MALLOC(sizeof(char*)*MAX_BANDS);
  for (ii=0; ii<MAX_BANDS; ++ii)
      dataNames[ii] = MALLOC(sizeof(char*)*512);
  metaName = (char **) MALLOC(2*sizeof(char *));
  for (ii=0; ii<2; ii++)
    metaName[ii] = (char *) MALLOC(sizeof(char)*512);

  char *baseName = get_base_name(fileName);
  get_ceos_names(baseName, dataNames, metaName, &nBands, &trailer);

  FILE *fp_tmp = fopen(dataNames[0], "r");
  if (fp_tmp != NULL) {
      dataNameExists = 1;
      fclose(fp_tmp);
  }
  else {
      asfPrintWarning("Data file (%s) missing.\n"
                      "Unable to extract Image File Descriptor Record.\n",
                      dataNames[0]);
      dataNameExists = 0;
  }

  // Check leader file
  fp_tmp = fopen(metaName[0], "r");
  if (fp_tmp != NULL) {
      leaderNameExists = 1;
      fclose(fp_tmp);
  }
  else {
      if (dataNameExists) {
          asfPrintWarning("Leader (meta data) file missing.\n"
                       "ONLY able to extract Image File Descriptor Record.\n");
      }
      else {
          asfPrintWarning("Leader (meta data) file missing.\n");
      }
      leaderNameExists = 0;
  }

  // Check trailer file (if exists)
  if (trailer) {
    fp_tmp = fopen(metaName[0], "r");
    if (fp_tmp != NULL) {
      leaderNameExists = 1;
      fclose(fp_tmp);
    }
    else {
      if (dataNameExists) {
	asfPrintWarning("Trailer (meta data) file missing.\n"
			"ONLY able to extract Image File Descriptor Record.\n");
      }
      else {
          asfPrintWarning("Trailer (meta data) file missing.\n");
      }
      leaderNameExists = 0;
    }
  }

  if (!dataNameExists && !leaderNameExists)
      return STRDUP("Unable to open data (.D) or leader (.L) file.\n");

  switch (reqrec) 
    {
    case (10): 
      dssr = (struct dataset_sum_rec *) MALLOC(sizeof(struct dataset_sum_rec));
      if (leaderNameExists) {
	if (get_dssr(metaName[0],dssr) >= 0 )
	  ret = sprn_dssr(dssr,1);
	else if (get_dssr(metaName[1],dssr) >= 0 )
	  ret = sprn_dssr(dssr,1);
      }
      FREE(dssr);
      break;
    case (18):
      shr = (struct scene_header_rec *) MALLOC(sizeof(struct scene_header_rec));
      if (leaderNameExists) {
	if (get_shr(metaName[0],shr) >= 0 )
	  ret = sprn_shr(shr);
	else if (get_shr(metaName[1],shr) >= 0 )
	  ret = sprn_shr(shr);
      }
      FREE(shr);
      break;
    case (20):	
      mpdrec = (struct VMPDREC *) MALLOC(sizeof(struct VMPDREC));
      if (leaderNameExists) {
	if (get_mpdr(metaName[0],mpdrec) >= 0 ) 
	  ret = sprn_mpdr(mpdrec); 
	else if (get_mpdr(metaName[1],mpdrec) >= 0 ) 
	  ret = sprn_mpdr(mpdrec);
      }
      FREE(mpdrec);
      break;
    case (30):	
      ppdr = (struct pos_data_rec *) MALLOC(sizeof(struct pos_data_rec));
      if (leaderNameExists) {
	if (get_ppdr(metaName[0],ppdr) >= 0)
	  ret = sprn_ppdr(ppdr);
	else if (get_ppdr(metaName[1],ppdr) >= 0)
	  ret = sprn_ppdr(ppdr);
      }
      FREE(ppdr);
      break;
    case (40):	
      atdr = (struct att_data_rec *) MALLOC(sizeof(struct att_data_rec));
      if (leaderNameExists) {
	if (get_atdr(metaName[0],atdr) >= 0)
	  ret = sprn_atdr(atdr);
	else if (get_atdr(metaName[1],atdr) >= 0)
	  ret = sprn_atdr(atdr);
      }
      FREE(atdr);
      break;
    case (44):
      ampr = (struct alos_map_proj_rec *) MALLOC(sizeof(struct alos_map_proj_rec));
      if (leaderNameExists) {
	if (get_ampr(metaName[0],ampr) >= 0)
	  ret = sprn_ampr(ampr);
	else if (get_ampr(metaName[1],ampr) >= 0)
	  ret = sprn_ampr(ampr);
      }
      FREE(ampr);
      break;
    case (50):	
      raddr = (struct VRADDR  *) MALLOC(sizeof(struct VRADDR));
      if (leaderNameExists) {
	if (get_raddr(metaName[0],raddr) >= 0)
	  ret = sprn_raddr(raddr);
	else if (get_raddr(metaName[1],raddr) >= 0)
	  ret = sprn_raddr(raddr);
      }
      FREE(raddr);
      ardr = (struct alos_rad_data_rec *) 
	MALLOC(sizeof(struct alos_rad_data_rec));
      if (leaderNameExists) {
	if (get_ardr(metaName[0],ardr) >= 0)
	  ret = sprn_ardr(ardr);
	else if (get_ardr(metaName[1],ardr) >= 0)
	  ret = sprn_ardr(ardr);
      }
      FREE(ardr);
      break;
    case (51):
      rcdr = (struct radio_comp_data_rec *) 
	MALLOC(sizeof(struct radio_comp_data_rec));
      if (leaderNameExists) {
	if (get_rcdr(metaName[0],rcdr) >= 0)
	  ret = sprn_rcdr(rcdr);
	else if (get_rcdr(metaName[1],rcdr) >= 0)
	  ret = sprn_rcdr(rcdr);
      }
      FREE(rcdr);
      break;
    case (60):	
      dqsr = (struct qual_sum_rec *) MALLOC(sizeof(struct qual_sum_rec));
      if (leaderNameExists) {
	if (get_dqsr(metaName[0],dqsr) >= 0) 
	  ret = sprn_dqsr(dqsr,1);
	else if (get_dqsr(metaName[1],dqsr) >= 0) 
	  ret = sprn_dqsr(dqsr,1);
      }
      FREE(dqsr);
      break;
    case (70):	
      dhr = (struct data_hist_rec *) MALLOC(sizeof(struct data_hist_rec));
      if (leaderNameExists) {
	if (get_dhr(metaName[0],dhr) >= 0) 
	  ret = sprn_dhr(dhr);
	else if (get_dhr(metaName[1],dhr) >= 0) 
	  ret = sprn_dhr(dhr);
      }
      FREE(dhr);
      break;
    case (71): 
      dhr = (struct data_hist_rec *) MALLOC(sizeof(struct data_hist_rec));
      if (leaderNameExists) {
	if (get_sdhr(metaName[0],dhr) >= 0) 
	  ret = sprn_dhr(dhr);
	else if (get_sdhr(metaName[1],dhr) >= 0) 
	  ret = sprn_dhr(dhr);
      }
      FREE(dhr);
      break;
    case (80):	
      rsr = (struct rng_spec_rec *) MALLOC(sizeof(struct rng_spec_rec));
      if (leaderNameExists) {
	if (get_rsr(metaName[0],rsr) >= 0) 
	  ret = sprn_rsr(rsr);
	else if (get_rsr(metaName[1],rsr) >= 0) 
	  ret = sprn_rsr(rsr);
      }
      FREE(rsr);
      break;
    case (120):
      ppr = (struct PPREC *) MALLOC(sizeof(struct PPREC));
      if (leaderNameExists) {
	if (get_ppr(metaName[0],ppr) >= 0)
	  ret = sprn_ppr(ppr);
	else if (get_ppr(metaName[1],ppr) >= 0)
	  ret = sprn_ppr(ppr);
      }
      FREE(ppr);
      break;
    case (192): 
      vfdr = (struct IOF_VFDR *) MALLOC(sizeof(struct IOF_VFDR));
      char *s = get_base_name(dataNames[0]);
      if (dataNameExists && get_ifiledr(s,vfdr) >= 0)
	ret = sprn_ifiledr(vfdr);
      FREE(s);
      FREE(vfdr);
      break;
    case (200):
    case (210): 
      facdr = (struct VFDRECV *) MALLOC(sizeof(struct VFDRECV));
      if (leaderNameExists) {
	if (get_asf_facdr(metaName[0],facdr) >= 0) 
	  ret = sprn_facdr(facdr,1);
	else if (get_asf_facdr(metaName[1],facdr) >= 0) 
	  ret = sprn_facdr(facdr,1);
      }
      FREE(facdr);
      break;
    case (220):
      esa_facdr = (struct ESA_FACDR *) MALLOC(sizeof(struct ESA_FACDR));
      if (leaderNameExists) {
	if (get_esa_facdr(metaName[0],esa_facdr) >= 0)
	  ret = sprn_esa_facdr(esa_facdr);
	else if (get_esa_facdr(metaName[1],esa_facdr) >= 0)
	  ret = sprn_esa_facdr(esa_facdr);
      }
      FREE(esa_facdr);
      break;
    case (300): 
      fdr = (struct FDR *) MALLOC(sizeof(struct FDR));
      if (leaderNameExists) {
	if (get_fdr(metaName[0],fdr) >= 0) 
	  ret = sprn_fdr(fdr);
	else if (get_fdr(metaName[1],fdr) >= 0) 
	  ret = sprn_fdr(fdr);
      }
      FREE(fdr);
      break;
    default:    
      printf("Not Valid Record Type\n");
      break;
    }
  FREE_BANDS(dataNames);
  FREE(baseName);
  if (!ret)
      return STRDUP("Record not found.\n");
  return ret;
}

int check_record(char *fileName, int reqrec) 
{
  struct VFDRECV *facdr;                // Facility Related Data record
  struct VRADDR *raddr;                 // Radiometric Data record
  struct IOF_VFDR *vfdr;                // Image File Descriptor record
  struct VMPDREC *mpdrec;               // Map Projection Data record
  struct FDR *fdr;                      // File Descriptor record
  struct dataset_sum_rec *dssr;         // Data Set Summary record
  struct pos_data_rec *ppdr;            // Platform Position Data record
  struct att_data_rec *atdr;            // Attitude Data record
  struct data_hist_rec *dhr;            // Data Histogram record
  struct rng_spec_rec *rsr;             // Range Spectra record
  struct qual_sum_rec *dqsr;            // Data Quality Summary record
  struct radio_comp_data_rec *rcdr;     // Radiometric Compensation Data record
  struct scene_header_rec *shr;         // Scene Header record
  struct alos_map_proj_rec *ampr;       // Map Projection Data record - ALOS
  struct ESA_FACDR *esa_facdr;          // Facility Related Data (ESA) record
  struct PPREC *ppr;                    // Processing Parameter record

  char **dataName, **metaName;
  int ii, nBands, trailer;

  // Allocate memory
  dataName = (char **) MALLOC(MAX_BANDS*sizeof(char *));
  for (ii=0; ii<MAX_BANDS; ii++)
    dataName[ii] = (char *) MALLOC(512*sizeof(char));
  metaName = (char **) MALLOC(2*sizeof(char *));
  for (ii=0; ii<2; ii++)
    metaName[ii] = (char *) MALLOC(512*sizeof(char *));
  
  require_ceos_pair(fileName, dataName, metaName, &nBands, &trailer);

  switch (reqrec) 
    {
    case (10): 
      dssr = (struct dataset_sum_rec *) MALLOC(sizeof(struct dataset_sum_rec));
      if (get_dssr(metaName[0],dssr) >= 0 )
	return 1;
      else if (trailer) {
	if (get_dssr(metaName[1],dssr) >= 0 )
	  return 1;
      }
      else
	printf("\nNo Data Set Summary Record Found\n\n");
      FREE(dssr);
      break;
    case (18):
      shr = (struct scene_header_rec *) MALLOC(sizeof(struct scene_header_rec));
      if (get_shr(metaName[0],shr) >= 0 )
	return 1;
      else if (trailer) {
	if (get_shr(metaName[1],shr) >= 0 )
	  return 1;
      }
      else
	printf("\nNo Scene Header Record Found\n\n");
      FREE(shr);
      break;
    case (20):	
      mpdrec = (struct VMPDREC *) MALLOC(sizeof(struct VMPDREC));
      if (get_mpdr(metaName[0],mpdrec) >= 0 ) 
        return 1;
      else if (trailer) {
	if (get_mpdr(metaName[1],mpdrec) >= 0 ) 
        return 1;
      }
      else 
	printf("\nNo Map Projection Data Record Found\n\n");
      FREE(mpdrec);
      break;
    case (30):	
      ppdr = (struct pos_data_rec *) MALLOC(sizeof(struct pos_data_rec));
      if (get_ppdr(metaName[0],ppdr) >= 0)
        return 1;
      else if (trailer) {
	if (get_ppdr(metaName[1],ppdr) >= 0)
	  return 1;
      }
      else
	printf("\nNo Platform Position Data Record Found\n\n");
      FREE(ppdr);
      break;
    case (40):	
      atdr = (struct att_data_rec *) MALLOC(sizeof(struct att_data_rec));
      if (get_atdr(metaName[0],atdr) >= 0)
	return 1;
      else if (trailer) {
	if (get_atdr(metaName[1],atdr) >= 0)
	  return 1;
      }
      else
	printf("\nNo Attitude Data Record Found\n\n");
      FREE(atdr);
      break;
    case (44):
      ampr = (struct alos_map_proj_rec *) MALLOC(sizeof(struct alos_map_proj_rec));
      if (get_ampr(metaName[0],ampr) >= 0)
        return 1;
      else if (trailer) {
	if (get_ampr(metaName[1],ampr) >= 0)
	  return 1;
      }
      else
	printf("\nNo Map Projection Record (ALOS) Found\n\n");
      FREE(ampr);
      break;
    case (50):	
      raddr = (struct VRADDR  *) MALLOC(sizeof(struct VRADDR));
      if (get_raddr(metaName[0],raddr) >= 0)
	return 1;
      else if (trailer) {
	if (get_raddr(metaName[1],raddr) >= 0)
	  return 1;
      }
      else
	printf("\nNo Radiometric Data Record\n\n");
      FREE(raddr);
      break;
    case (51):
      rcdr = (struct radio_comp_data_rec *) 
	MALLOC(sizeof(struct radio_comp_data_rec));
      if (get_rcdr(metaName[0],rcdr) >= 0)
	return 1;
      else if (trailer) {
	if (get_rcdr(metaName[1],rcdr) >= 0)
	  return 1;
      }
      else
	printf("\nNo Radiometric Compensation Data Record\n\n");
      FREE(rcdr);
      break;
    case (60):	
      dqsr = (struct qual_sum_rec *) MALLOC(sizeof(struct qual_sum_rec));
      if (get_dqsr(metaName[0],dqsr) >= 0) 
	return 1;
      else if (trailer) {
	if (get_dqsr(metaName[1],dqsr) >= 0) 
	  return 1;
      }
      else 
	printf("\nNo Data Quality Summary Record Found\n\n");
      FREE(dqsr);
      break;
    case (70):	
      dhr = (struct data_hist_rec *) MALLOC(sizeof(struct data_hist_rec));
      if (get_dhr(metaName[0],dhr) >= 0) 
	return 1;
      else if (trailer) {
	if (get_dhr(metaName[1],dhr) >= 0) 
	  return 1;
      }
      else 
	printf("\nNo Processed Data Histograms Record Found\n\n");
      FREE(dhr);
      break;
    case (71): 
      dhr = (struct data_hist_rec *) MALLOC(sizeof(struct data_hist_rec));
      if (get_sdhr(metaName[0],dhr) >= 0) 
	return 1;
      else if (trailer) {
	if (get_sdhr(metaName[1],dhr) >= 0) 
	  return 1;
      }
      else 
	printf("\nNo Signal Data Histograms Record Found\n\n");
      FREE(dhr);
      break;
    case (80):	
      rsr = (struct rng_spec_rec *) MALLOC(sizeof(struct rng_spec_rec));
      if (get_rsr(metaName[0],rsr) >= 0) 
	return 1;
      else if (trailer) {
	if (get_rsr(metaName[1],rsr) >= 0) 
	  return 1;
      }
      else 
	printf("\nNo Range Spectra Record Found\n\n");
      FREE(rsr);
      break;
    case (120):
      ppr = (struct PPREC *) MALLOC(sizeof(struct PPREC));
      if (get_ppr(metaName[0],ppr) >= 0)
	return 1;
      else if (trailer) {
	if (get_ppr(metaName[1],ppr) >= 0)
	  return 1;
      }
      else
	printf("\nNo Processing Parameter Record Found\n\n");
      FREE(ppr);
      break;
    case (192): 
      vfdr = (struct IOF_VFDR *) MALLOC(sizeof(struct IOF_VFDR));
      if (get_ifiledr(dataName[0],vfdr) >= 0)
	return 1;
      else
	printf("\nNo Image File Descriptor Record Found\n\n");
      FREE(vfdr);
      break;
    case (200):
    case (210): 
      facdr = (struct VFDRECV *) MALLOC(sizeof(struct VFDRECV));
      if (get_asf_facdr(metaName[0],facdr) >= 0) 
	return 1;
      else if (trailer) {
	if (get_asf_facdr(metaName[1],facdr) >= 0) 
	  return 1;
      }
      else 
	printf("\nNo Facility Related Data Record Found\n\n");
      break;
      FREE(facdr);
    case (220):
      esa_facdr = (struct ESA_FACDR *) MALLOC(sizeof(struct ESA_FACDR));
      if (get_esa_facdr(metaName[0],esa_facdr) >= 0)
	return 1;
      else if (trailer) {
	if (get_esa_facdr(metaName[1],esa_facdr) >= 0)
	  return 1;
      }
      else
	printf("\nNo Facility Related Data Record (ESA) Found\n\n");
      FREE(esa_facdr);
      break;
    case (300): 
      fdr = (struct FDR *) MALLOC(sizeof(struct FDR));
      if (get_fdr(metaName[0],fdr) >= 0) 
	return 1;
      else if (trailer) {
	if (get_fdr(metaName[1],fdr) >= 0) 
	  return 1;
      }
      else
	printf("\nNo File Descriptor Record Found\n\n");
      break;
      FREE(fdr);
    default:    
      printf("Not Valid Record Type\n");
      break;
    }
  return 0;
}
