/******************************************************************************
NAME:	writeAsfCeosSignal - converts an ASF .raw file into a .D CCSD file

SYNOPSIS:  

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.1	    7/97   T. Logan	Create ASF Complient outputs
    i.0	    3/99   T. Logan	Ditto

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include "asf.h"
#include "sarout.h"
#include "asf_meta.h"

int init_hdr(int type, int size, struct HEADER *h);
int inc_hdr(struct HEADER *bf);
int init_rhdr(int type,ceosLeader *leader,int ns,meta_parameters *meta,struct RHEADER *r);
int inc_rhdr(int type, meta_parameters *meta, int ns, struct RHEADER *bf);
int get_hist_stats(struct hist_dset *d, int nl, int ns);
void getSignalFormat(char *,int *,int *,float *,float *,char *);
void prepare_sdhr(ceosLeader *leader, int nl, int ns);
void fill_iof_vfdr(int mode,struct IOF_VFDR *v,int nl,int ns,int nbytes, char *filename);


void writeAsfCeosSignal(ceosLeader *leader,meta_parameters *meta,
	 	   char *inFile,char *outFile,int leaderOnly)
{
  struct IOF_VFDR 	vfdr;
  struct hist_dset      *i;
  struct hist_dset      *q;

  char   infile[256], outfile[256];
  FILE   *fpi, *fpo;
  int     nbytes, nl, ns, j, line;
  unsigned char *buf;
  unsigned char *inBuf;
  long long   sumIval=0, sumQval=0;
  double sumsqIval = 0.0, sumsqQval = 0.0;
  double dsum,dtot;
  double vari;

  int    inbytes, nhead;
  float  xmi, xmq;
  char   iqflip; 

  /* Set processing options
   -----------------------*/  
  strcpy(infile,inFile);
  strcat(infile,".raw");
  strcat(strcpy(outfile,outFile),".D");
  getSignalFormat(inFile, &inbytes, &nhead, &xmi, &xmq, &iqflip);

  /* Determine number of lines and samples in file
   ----------------------------------------------*/
  ns = (inbytes-nhead) / 2;
  fpi = FOPEN(infile,"rb");
  fseek(fpi,0,SEEK_END);
  nl = ftell(fpi)/inbytes;
  fseek(fpi,0,SEEK_SET);

  /* Get the data histogram structure ready
   ---------------------------------------*/
  prepare_sdhr(leader, nl, ns);
  i = leader->sdhr.data;
  q = i->next; 

  /* Determine size of output line, allocate buffers 
   ------------------------------------------------*/
  nbytes = ns*2 + H_SZ + R_SZ;
  buf = (unsigned char *) MALLOC (nbytes);
  inBuf = (unsigned char *) MALLOC (inbytes);

  if (!leaderOnly)
   {
    /* Pack 12 byte header, fill IOF_VFDR, convert to ascii, & write it out
     ---------------------------------------------------------------------*/
    fpo = FOPEN(outfile,"wb");
    init_hdr(IOFDR,nbytes,(struct HEADER *) buf);
    fill_iof_vfdr(CEOS_CCSD,&vfdr,nl,ns,nbytes,leader->dssr.product_id);
    Code_IOF(buf,&vfdr,toASCII);
    FWRITE(buf,nbytes,1,fpo);

    /* Initialize header structures 
     -----------------------------*/
    init_hdr(CEOS_CCSD,nbytes,(struct HEADER *) buf);
    init_rhdr(CEOS_CCSD,leader,ns,meta,(struct RHEADER *) &buf[12]);
   }

  /* Loop through file, converting and writing image lines 
   ------------------------------------------------------*/
  for (line=0; line<nl; line++)
   {
      unsigned char *obuf; 

      FREAD(inBuf,inbytes,1,fpi);
      obuf = &buf[192];
      for (j=0; j<ns*2; j+=2)
       {
 	unsigned char itmp, qtmp;
	int tmp;

	obuf[j]   = itmp = inBuf[j+nhead];
	obuf[j+1] = qtmp = inBuf[j+1+nhead];

	if (line % 10 == 0)
 	 {
          sumIval+=itmp;
  	  sumsqIval += (double)(itmp*itmp);

	  sumQval+=qtmp;
	  sumsqQval += (double)(qtmp*qtmp);

   	  tmp = (int) (itmp/i->smp_inc);
	  i->data_values_hist[tmp]++;

   	  tmp = (int) (qtmp/q->smp_inc);
          q->data_values_hist[tmp]++;
         }
       }

      if (!leaderOnly)
       {
        FWRITE(buf,nbytes,1,fpo);
        inc_hdr((struct HEADER *)buf);
        inc_rhdr(CEOS_CCSD, meta, ns, (struct RHEADER *)&buf[12]);
       }
      if (line%500==0) printf("...Converting Line %i\n",line);
   }
  fclose(fpi);
  if (!leaderOnly) fclose(fpo);

  get_hist_stats(i,nl/10,ns);
  dtot = (double) ns*nl/10;
  dsum = (double) sumIval;
  i->mean_smp = dsum / dtot;
  vari = (sumsqIval-(dsum*dsum)/dtot)/(dtot-1.0);
  i->std_smp = sqrt(vari);

  get_hist_stats(q,nl/10,ns); 
  dsum = (double) sumQval;
  q->mean_smp = dsum / dtot;
  vari = (sumsqQval-(dsum*dsum)/dtot)/(dtot-1.0);
  q->std_smp = sqrt(vari);

  leader->dssr.i_bias = i->mean_smp;
  leader->dssr.q_bias = q->mean_smp;
  leader->dssr.iq_ratio = i->mean_smp / q->mean_smp; 
}

void prepare_sdhr(ceosLeader *leader, int nl, int ns)
 {
    int cnt;
    struct hist_dset *i;
    struct hist_dset *q;

    leader->sdhr.seq_num = 1;
    leader->sdhr.sar_chan = 1;
    leader->sdhr.ntab = 2;
    leader->sdhr.ltab = 2296;
    leader->sdhr.data = (struct hist_dset *) MALLOC (sizeof(struct hist_dset));

    /* Prepare I component Histogram
     ------------------------------*/
    i = leader->sdhr.data;
    strcpy(i->hist_desc,"I from SEPARATE I Q");
    i->nrec = 1;
    i->tab_seq = 1;
    i->nbin = 256;
    i->ns_lin = i->ngrp_lin = i->nsamp_lin = ns;
    i->ns_pix = i->ngrp_pix = i->nsamp_pix = nl/10;
    i->nhist = i->nbin;
    i->min_smp = 0.0;
    i->max_smp = 255.0;
    i->smp_inc = 1;
    i->data_values_hist = (int *) MALLOC (i->nhist * sizeof(int));
    for (cnt=0; cnt< (i->nhist); cnt++) i->data_values_hist[cnt] = 0;
    i->next = (struct hist_dset *) MALLOC (sizeof(struct hist_dset));

    /* Prepare Q component Histogram
     ------------------------------*/
    q = i->next;
    strcpy(q->hist_desc,"Q from SEPARATE I Q");
    q->nrec = 1;
    q->tab_seq = 2;
    q->nbin = 256;
    q->ns_lin = q->ngrp_lin = q->nsamp_lin = ns;
    q->ns_pix = q->ngrp_pix = q->nsamp_pix = nl/10; 
    q->nhist = q->nbin;
    q->min_smp = 0.0;
    q->max_smp = 255.0;
    q->smp_inc = 1;
    q->data_values_hist = (int *) MALLOC (q->nhist * sizeof(int));
    for (cnt=0; cnt<(q->nhist); cnt++) q->data_values_hist[cnt] = 0;
    q->next = NULL;

    printf("Histogram Structure Initialized\n");
 }
