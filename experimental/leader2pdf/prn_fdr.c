#include "asf.h"
#include "ceos.h"

void prn_fdr(char *file, struct FDR *fdr)
{
  FILE *fp;

  fp = FOPEN(file, "w");
  fprintf(fp, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n");
  fprintf(fp, "\"http://www.w3.org/TR/html4/loose.dtd\">\n<html>\n<head>\n");
  fprintf(fp, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n");
  fprintf(fp, "<title>File Descriptor record</title>\n</head>\n<body>\n");
  fprintf(fp, "<strong><h2>File Descriptor record</h2>\n");
  fprintf(fp, "<strong>Number of Dataset Summary Records: </strong>%i<br>\n", fdr->n_dssr);
  fprintf(fp, "<strong>Dataset Summary Record Length (bytes): </strong>%i<br>\n", fdr->l_dssr);
  fprintf(fp, "<strong>Number of Map Projection Records: </strong>%i<br>\n", fdr->n_mpdr);
  fprintf(fp, "<strong>Map Projection Record Length (bytes): </strong>%i<br>\n", fdr->l_mpdr);
  fprintf(fp, "<strong>Number of Platform Position Records: </strong>%i<br>\n", fdr->n_ppdr);
  fprintf(fp, "<strong>Platform Position Record Length (bytes): </strong>%i<br>\n", fdr->l_ppdr);
  fprintf(fp, "<strong>Number of Attitude Data Records: </strong>%i<br>\n", fdr->n_atdr);
  fprintf(fp, "<strong>Attitude Data Record Length (bytes): </strong>%i<br>\n", fdr->l_atdr);
  fprintf(fp, "<strong>Number of Radiometric Data Records: </strong>%i<br>\n", fdr->n_raddr);
  fprintf(fp, "<strong>Radiometric Data Record Length (bytes): </strong>%i<br>\n", fdr->l_raddr);
  fprintf(fp, "<strong>Number of Radiometric Compensation Records: </strong>%i<br>\n", fdr->n_rcr);
  fprintf(fp, "<strong>Radiometric Compensation Record Length (bytes): </strong>%i<br>\n", fdr->l_rcr);
  fprintf(fp, "<strong>Number of Data Quality Summary Records: </strong>%i<br>\n", fdr->n_qsr);
  fprintf(fp, "<strong>Data Quality Summary Record Length (bytes): </strong>%i<br>\n", fdr->l_qsr);
  fprintf(fp, "<strong>Number of Data Histogram Records: </strong>%i<br>\n", fdr->n_dhr);
  fprintf(fp, "<strong>Data Histogram Record Length (bytes): </strong>%i<br>\n", fdr->l_dhr);
  fprintf(fp, "<strong>Number of Range Spectra Records: </strong>%i<br>\n", fdr->n_rsr);
  fprintf(fp, "<strong>Range Spectra Record Length (bytes): </strong>%i<br>\n", fdr->l_rsr);
  fprintf(fp, "<strong>Number of DEM Descriptor Records: </strong>%i<br>\n", fdr->n_demdr);
  fprintf(fp, "<strong>DEM Descriptor Record Length (bytes): </strong>%i<br>\n", fdr->l_demdr);
  fprintf(fp, "<strong>Number of RADAR Parameter Records: </strong>%i<br>\n", fdr->n_rpr);
  fprintf(fp, "<strong>RADAR Parameter Record Length (bytes): </strong>%i<br>\n", fdr->l_rpr);
  fprintf(fp, "<strong>Number of Annotation Data Records: </strong>%i<br>\n", fdr->n_adr);
  fprintf(fp, "<strong>Annotation Data Record Length (bytes): </strong>%i<br>\n", fdr->l_adr);
  fprintf(fp, "<strong>Number of Detailed Processing Parameter Records: </strong>%i<br>\n", fdr->n_dpr);
  fprintf(fp, "<strong>Detailed Processing Parameter Record Length (bytes): </strong>%i<br>\n", fdr->l_dpr);
  fprintf(fp, "<strong>Number of Calibration Records: </strong>%i<br>\n", fdr->n_calr);
  fprintf(fp, "<strong>Calibration Record Length (bytes): </strong>%i<br>\n", fdr->l_calr);
  fprintf(fp, "<strong>Number of GCP Records: </strong>	%i<br>\n", fdr->n_gcp);
  fprintf(fp, "<strong>GCP Record Length (bytes): </strong>%i<br>\n", fdr->l_gcp);
  fprintf(fp, "<strong>Number of Facility Data Records: </strong>%i<br>\n", fdr->n_facdr);
  fprintf(fp, "<strong>Facility Data Record Length (bytes): </strong>%i<br>\n", fdr->l_facdr);
  fprintf(fp, "</body>\n</html>\n");
  FCLOSE(fp);

  return;
}
