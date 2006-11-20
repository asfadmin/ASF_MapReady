/********************************************************************
NAME:     print_string_fdr.c --  print file descriptor record

SYNOPSIS: prn_fdr(struct FDR *fdr)

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           6/99   M. Ayers (ASF)
  
*********************************************************************/
#include <stdio.h>
#include "ceos.h"
#include "metadisplay.h"

void prn_fdr(FILE *fp, struct FDR *fdr)
{
  fprintf(fp, "\n************ Beginning of File Descriptor Record ***********\n\n");
  fprintf(fp, "Number of Dataset Summary Records = 			%i\n", 
	  fdr->n_dssr);
  fprintf(fp, "Dataset Summary Record Length (bytes) =			%i\n", 
	  fdr->l_dssr);
  fprintf(fp, "Number of Map Projection Records =			%i\n", 
	  fdr->n_mpdr);
  fprintf(fp, "Map Projection Record Length (bytes) =			%i\n", 
	  fdr->l_mpdr);
  fprintf(fp, "Number of Platform Position Records =			%i\n", 
	  fdr->n_ppdr);
  fprintf(fp, "Platform Position Record Length (bytes) =		%i\n", 
	  fdr->l_ppdr);
  fprintf(fp, "Number of Attitude Data Records = 			%i\n", 
	  fdr->n_atdr);
  fprintf(fp, "Attitude Data Record Length (bytes) = 			%i\n", 
	  fdr->l_atdr);
  fprintf(fp, "Number of Radiometric Data Records =			%i\n", 
	  fdr->n_raddr);
  fprintf(fp, "Radiometric Data Record Length (bytes) =		%i\n", 
	  fdr->l_raddr);
  fprintf(fp, "Number of Radiometric Compensation Records = 		%i\n", 
	  fdr->n_rcr);
  fprintf(fp, "Radiometric Compensation Record Length (bytes) =	%i\n", 
	  fdr->l_rcr);
  fprintf(fp, "Number of Data Quality Summary Records = 		%i\n", 
	  fdr->n_qsr);
  fprintf(fp, "Data Quality Summary Record Length (bytes) =		%i\n", 
	  fdr->l_qsr);
  fprintf(fp, "Number of Data Histogram Records =			%i\n", 
	  fdr->n_dhr);
  fprintf(fp, "Data Histogram Record Length (bytes) =			%i\n", 
	  fdr->l_dhr);
  fprintf(fp, "Number of Range Spectra Records =			%i\n", 
	  fdr->n_rsr);
  fprintf(fp, "Range Spectra Record Length (bytes) =			%i\n", 
	  fdr->l_rsr);
  fprintf(fp, "Number of DEM Descriptor Records =			%i\n", 
	  fdr->n_demdr);
  fprintf(fp, "DEM Descriptor Record Length (bytes) =			%i\n", 
	  fdr->l_demdr);
  fprintf(fp, "Number of RADAR Parameter Records =			%i\n", 
	  fdr->n_rpr);
  fprintf(fp, "RADAR Parameter Record Length (bytes) =			%i\n", 
	  fdr->l_rpr);
  fprintf(fp, "Number of Annotation Data Records = 			%i\n", 
	  fdr->n_adr);
  fprintf(fp, "Annotation Data Record Length (bytes) =			%i\n", 
	  fdr->l_adr);
  fprintf(fp, "Number of Detailed Processing Parameter Records =	%i\n", 
	  fdr->n_dpr);
  fprintf(fp, "Detailed Processing Parameter Record Length (bytes) =	%i\n", 
	  fdr->l_dpr);
  fprintf(fp, "Number of Calibration Records = 			%i\n", 
	  fdr->n_calr);
  fprintf(fp, "Calibration Record Length (bytes) =			%i\n", 
	  fdr->l_calr);
  fprintf(fp, "Number of GCP Records =					%i\n", 
	  fdr->n_gcp);
  fprintf(fp, "GCP Record Length (bytes) =				%i\n", 
	  fdr->l_gcp);
  fprintf(fp, "Number of Facility Data Records =			%i\n", 
	  fdr->n_facdr);
  fprintf(fp, "Facility Data Record Length (bytes) =			%i\n", 
	  fdr->l_facdr);
  fprintf(fp, "\n******************End of Descriptor Record ***************\n\n"); 
  return;
 }
