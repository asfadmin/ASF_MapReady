#include "asf.h"
#include "asf_meta.h"
#include "ceos.h"
#include "get_ceos_names.h"
#include <unistd.h>

#define VERSION 1.0

/* Heavily sliced and diced from 'metadata' tool */

/* Prototypes */
void prn_atdr (char *file, struct att_data_rec *a); 
void prn_dhr (char *file, struct data_hist_rec* h);
void prn_dqsr (char *file, struct qual_sum_rec *q, int era); 
void prn_dssr (char *file, struct dataset_sum_rec *ds, int era); 
void prn_facdr (char *file, struct VFDRECV *ofdr, int era); 
void prn_ifiledr (char *file, struct IOF_VFDR *ifdr); 
void prn_mpdr (char *file, struct VMPDREC *vmpdr); 
void prn_ppdr (char *file, struct pos_data_rec *p); 
void prn_raddr (char *file, struct VRADDR *dr); 
void prn_rsr (char *file, struct rng_spec_rec *r);
void prn_fdr (char *file, struct FDR *fdr);

void usage(char *name)
{
  printf("\n"
         "USAGE:\n"
         "   %s  <leader file>\n",name);
  printf("\n"
         "REQUIRED ARGUMENTS:\n"
         "   leader file   Basename of file to be converted "
	 "(requires .D and .L files).");
  printf("\n\n"
         "DESCRIPTION:\n"
         "   %s converts a leader file into PDF format.\n", name);
  printf("\n"
         "Version %.2f, ASF SAR Tools\n"
         "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  struct VFDRECV         *facdr;
  struct VRADDR          *raddr;
  struct IOF_VFDR        *vfdr;
  struct VMPDREC         *mpdrec;
  struct FDR		 *fdr;
  struct dataset_sum_rec *dssr;
  struct pos_data_rec    *ppdr;
  struct att_data_rec    *atdr;
  struct data_hist_rec   *dhr;
  struct rng_spec_rec    *rsr;
  struct qual_sum_rec    *dqsr;
  char leaderFile[255], dataFile[255], htmlFile[255], texFile[255], *file, cmd[255];
  int pid, era=1;
  
  /* Parse command line args */
  if ((argc-currArg) < 1) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  /* Get the file names sorted out */
  pid = (int) getpid();
  file = argv[1];
  sprintf(leaderFile, "%s.L", file);
  sprintf(dataFile, "%s.D", file);
  sprintf(texFile, "tmp%i.tex", pid);

  /* Data Set Summary Record */
  dssr = (struct dataset_sum_rec *) malloc(sizeof(struct dataset_sum_rec));
  get_dssr(leaderFile,dssr);
  sprintf(htmlFile, "%s_dssr.html", file);
  printf("\nWriting data set summary record\n\n");
  prn_dssr(htmlFile, dssr, era); 
  sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
  system(cmd);
  free(dssr);
  
  /* Map Projection Data Record */
  mpdrec = (struct VMPDREC  *) malloc (sizeof(struct VMPDREC));
  if (get_mpdr(leaderFile,mpdrec) >= 0) {
    sprintf(htmlFile, "%s_mpdr.html", file);
    printf("\nWriting map projection data record\n\n"); 
    prn_mpdr(htmlFile, mpdrec);
    sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
    system(cmd);
  }
  free(mpdrec);

  /* Platform Position Data Record */
  ppdr=(struct pos_data_rec*)malloc(sizeof(struct pos_data_rec));
  get_ppdr(leaderFile,ppdr);
  sprintf(htmlFile, "%s_ppdr.html", file);
  printf("\nWriting platform position record\n\n"); 
  prn_ppdr(htmlFile, ppdr); 
  sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
  system(cmd);
  free(ppdr);

  /* Attitude Data Record */
  atdr=(struct att_data_rec*)malloc(sizeof(struct att_data_rec));
  get_atdr(leaderFile,atdr);
  sprintf(htmlFile, "%s_atdr.html", file);
  printf("\nWriting attitude data record\n\n"); 
  prn_atdr(htmlFile, atdr); 
  sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
  system(cmd);
  free(atdr);

  /* Radiometric Data Record */
  raddr = (struct VRADDR  *) malloc (sizeof(struct VRADDR));
  get_raddr(leaderFile,raddr);
  sprintf(htmlFile, "%s_raddr.html", file);
  printf("\nWriting radiometric data record\n\n"); 
  prn_raddr(htmlFile, raddr); 
  sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
  system(cmd);
  free(raddr);

  /* Data Quality Summary Record */
  dqsr=(struct qual_sum_rec*)malloc(sizeof(struct qual_sum_rec));
  if (get_dqsr(leaderFile,dqsr) >= 0) {
    sprintf(htmlFile, "%s_dqsr.html", file);
    printf("\nWriting data quality summary record\n\n"); 
    prn_dqsr(htmlFile, dqsr,era); 
    sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
    system(cmd);
  }
  free(dqsr);

  /* Processed Data Histograms Record */
  dhr=(struct data_hist_rec*)malloc(sizeof(struct data_hist_rec));
  if (get_dhr(leaderFile,dhr) >= 0) { 
    sprintf(htmlFile, "%s_pdhr.html", file);
    printf("\nWriting processed data histogram record\n\n");
    prn_dhr(htmlFile, dhr); 
    sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
    system(cmd);
  }
  free(dhr);

  /* Signal Data Histograms Record */
  dhr=(struct data_hist_rec*)malloc(sizeof(struct data_hist_rec));
  if (get_sdhr(leaderFile,dhr) >= 0) {
    sprintf(htmlFile, "%s_sdhr.html", file);
    printf("\nWriting signal data histogram record\n\n"); 
    prn_dhr(htmlFile, dhr); 
    sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
    system(cmd);
  }
  free(dhr);

  /* Range Spectra Record */
  rsr=(struct rng_spec_rec*) malloc (sizeof(struct rng_spec_rec));
  if (get_rsr(leaderFile,rsr) >= 0) {
    sprintf(htmlFile, "%s_rsr.html", file);
    printf("\nWriting range spectra record\n\n"); 
    prn_rsr(htmlFile, rsr); 
    sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
    system(cmd);
  }
  free(rsr);

  /* Image File Descriptor Record */
  vfdr=(struct IOF_VFDR *) malloc (sizeof(struct IOF_VFDR));
  get_ifiledr(dataFile,vfdr);
  sprintf(htmlFile, "%s_ifdr.html", file);
  printf("\nWriting image file descriptor record\n\n");
  prn_ifiledr(htmlFile, vfdr);
  sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
  system(cmd);
  free(vfdr);

  /* Facility Related Data Record */
  facdr = (struct VFDRECV  *) malloc (sizeof(struct VFDRECV));
  if (get_asf_facdr(leaderFile,facdr) >= 0) { 
    sprintf(htmlFile, "%s_facdr.html", file);
    printf("\nWriting facility related data record\n\n");
    prn_facdr(htmlFile, facdr,era); 
    sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
    system(cmd);
  }
  free(facdr);

  /* File Descriptor Record */
  fdr = (struct FDR *) malloc (sizeof(struct FDR));
  get_fdr(leaderFile,fdr);
  sprintf(htmlFile, "%s_fdr.html", file);
  printf("\nWriting file descriptor record\n\n"); 
  prn_fdr(htmlFile, fdr); 
  sprintf(cmd, "html2latex --package=\"times, nopageno\" --head \"pdftex\" --pdf %s", htmlFile);
  system(cmd);
  free(fdr);

  /* Clean up the intermediate files *
  sprintf(cmd, "rm %s", texFile);
  system(cmd);*/

  exit(0);
}
