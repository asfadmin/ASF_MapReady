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

void prn_fdr(struct FDR *fdr)
{
 printf("\n************ Beginning of File Descriptor Record ****************\n\n");
 printf("Number of Dataset Summary Records = 			%i\n", fdr->n_dssr);
 printf("Dataset Summary Record Length (bytes) =			%i\n", fdr->l_dssr);
 printf("Number of Map Projection Records =			%i\n", fdr->n_mpdr);
 printf("Map Projection Record Length (bytes) =			%i\n", fdr->l_mpdr);
 printf("Number of Platform Position Records =			%i\n", fdr->n_ppdr);
 printf("Platform Position Record Length (bytes) =		%i\n", fdr->l_ppdr);
 printf("Number of Attitude Data Records = 			%i\n", fdr->n_atdr);
 printf("Attitude Data Record Length (bytes) = 			%i\n", fdr->l_atdr);
 printf("Number of Radiometric Data Records =			%i\n", fdr->n_raddr);
 printf("Radiometric Data Record Length (bytes) =		%i\n", fdr->l_raddr);
 printf("Number of Radiometric Compensation Records = 		%i\n", fdr->n_rcr);
 printf("Radiometric Compensation Record Length (bytes) =	%i\n", fdr->l_rcr);
 printf("Number of Data Quality Summary Records = 		%i\n", fdr->n_qsr);
 printf("Data Quality Summary Record Length (bytes) =		%i\n", fdr->l_qsr);
 printf("Number of Data Histogram Records =			%i\n", fdr->n_dhr);
 printf("Data Histogram Record Length (bytes) =			%i\n", fdr->l_dhr);
 printf("Number of Range Spectra Records =			%i\n", fdr->n_rsr);
 printf("Range Spectra Record Length (bytes) =			%i\n", fdr->l_rsr);
 printf("Number of DEM Descriptor Records =			%i\n", fdr->n_demdr);
 printf("DEM Descriptor Record Length (bytes) =			%i\n", fdr->l_demdr);
 printf("Number of RADAR Parameter Records =			%i\n", fdr->n_rpr);
 printf("RADAR Parameter Record Length (bytes) =			%i\n", fdr->l_rpr);
 printf("Number of Annotation Data Records = 			%i\n", fdr->n_adr);
 printf("Annotation Data Record Length (bytes) =			%i\n", fdr->l_adr);
 printf("Number of Detailed Processing Parameter Records =	%i\n", fdr->n_dpr);
 printf("Detailed Processing Parameter Record Length (bytes) =	%i\n", fdr->l_dpr);
 printf("Number of Calibration Records = 			%i\n", fdr->n_calr);
 printf("Calibration Record Length (bytes) =			%i\n", fdr->l_calr);
 printf("Number of GCP Records =					%i\n", fdr->n_gcp);
 printf("GCP Record Length (bytes) =				%i\n", fdr->l_gcp);
 printf("Number of Facility Data Records =			%i\n", fdr->n_facdr);
 printf("Facility Data Record Length (bytes) =			%i\n", fdr->l_facdr);
 printf("\n***********************End of Descriptor Record *****************\n\n"); 
 return;
 }
