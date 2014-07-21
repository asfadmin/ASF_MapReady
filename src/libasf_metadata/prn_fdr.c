/********************************************************************
NAME:     print_string_fdr.c --  print file descriptor record

SYNOPSIS: prn_fdr(struct FDR *fdr)

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           6/99   M. Ayers (ASF)
  
*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_fdr(struct FDR *fdr)
{
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  add(&ret, "\n************ Beginning of File Descriptor Record ***********\n\n");
  add(&ret, "Number of Dataset Summary Records = 			%i\n", 
	  fdr->n_dssr);
  add(&ret, "Dataset Summary Record Length (bytes) =			%i\n", 
	  fdr->l_dssr);
  add(&ret, "Number of Map Projection Records =			%i\n", 
	  fdr->n_mpdr);
  add(&ret, "Map Projection Record Length (bytes) =			%i\n", 
	  fdr->l_mpdr);
  add(&ret, "Number of Platform Position Records =			%i\n", 
	  fdr->n_ppdr);
  add(&ret, "Platform Position Record Length (bytes) =		%i\n", 
	  fdr->l_ppdr);
  add(&ret, "Number of Attitude Data Records = 			%i\n", 
	  fdr->n_atdr);
  add(&ret, "Attitude Data Record Length (bytes) = 			%i\n", 
	  fdr->l_atdr);
  add(&ret, "Number of Radiometric Data Records =			%i\n", 
	  fdr->n_raddr);
  add(&ret, "Radiometric Data Record Length (bytes) =		%i\n", 
	  fdr->l_raddr);
  add(&ret, "Number of Radiometric Compensation Records = 		%i\n", 
	  fdr->n_rcr);
  add(&ret, "Radiometric Compensation Record Length (bytes) =	%i\n", 
	  fdr->l_rcr);
  add(&ret, "Number of Data Quality Summary Records = 		%i\n", 
	  fdr->n_qsr);
  add(&ret, "Data Quality Summary Record Length (bytes) =		%i\n", 
	  fdr->l_qsr);
  add(&ret, "Number of Data Histogram Records =			%i\n", 
	  fdr->n_dhr);
  add(&ret, "Data Histogram Record Length (bytes) =			%i\n", 
	  fdr->l_dhr);
  add(&ret, "Number of Range Spectra Records =			%i\n", 
	  fdr->n_rsr);
  add(&ret, "Range Spectra Record Length (bytes) =			%i\n", 
	  fdr->l_rsr);
  add(&ret, "Number of DEM Descriptor Records =			%i\n", 
	  fdr->n_demdr);
  add(&ret, "DEM Descriptor Record Length (bytes) =			%i\n", 
	  fdr->l_demdr);
  add(&ret, "Number of RADAR Parameter Records =			%i\n", 
	  fdr->n_rpr);
  add(&ret, "RADAR Parameter Record Length (bytes) =			%i\n", 
	  fdr->l_rpr);
  add(&ret, "Number of Annotation Data Records = 			%i\n", 
	  fdr->n_adr);
  add(&ret, "Annotation Data Record Length (bytes) =			%i\n", 
	  fdr->l_adr);
  add(&ret, "Number of Detailed Processing Parameter Records =	%i\n", 
	  fdr->n_dpr);
  add(&ret, "Detailed Processing Parameter Record Length (bytes) =	%i\n", 
	  fdr->l_dpr);
  add(&ret, "Number of Calibration Records = 			%i\n", 
	  fdr->n_calr);
  add(&ret, "Calibration Record Length (bytes) =			%i\n", 
	  fdr->l_calr);
  add(&ret, "Number of GCP Records =					%i\n", 
	  fdr->n_gcp);
  add(&ret, "GCP Record Length (bytes) =				%i\n", 
	  fdr->l_gcp);
  add(&ret, "Number of Facility Data Records =			%i\n", 
	  fdr->n_facdr);
  add(&ret, "Facility Data Record Length (bytes) =			%i\n", 
	  fdr->l_facdr);
  add(&ret, "\n******************End of Descriptor Record ***************\n\n"); 
  return ret;
}

void prn_fdr(FILE *fp, struct FDR *fdr)
{
    char *rec = sprn_fdr(fdr);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
