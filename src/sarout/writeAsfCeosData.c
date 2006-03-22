/******************************************************************************
NAME:	writeAsfCeosData - converts a LAS .img/.cpx file into a .D file

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

double getSRfromGR(stateVector,double,double,double);
void prepare_pdhr(int,ceosLeader *,int,int);
void fill_iof_vfdr(int mode,struct IOF_VFDR *v,int nl,int ns,int nbytes, char *filename);
int get_hist_stats(struct hist_dset *d, int nl, int ns);
int init_hdr(int type, int size, struct HEADER *h);
int inc_hdr(struct HEADER *bf);
int init_rhdr(int type,ceosLeader *leader,int ns,meta_parameters *meta,struct RHEADER *r);
int inc_rhdr(int type, meta_parameters *meta, int ns, struct RHEADER *bf);


void writeAsfCeosData(int mode, ceosLeader *leader,struct DDR *ddr,
 		meta_parameters *meta, char *inFile,char *outFile)
{
  struct IOF_VFDR 	vfdr;
  struct hist_dset      *i;
  struct hist_dset      *q;

  char   infile[256], outfile[256];
  FILE   *fpi, *fpo;
  int    nbytes, nl, ns, ii, line;
  float  *floatBuf;
  short  *ofbuf;
  unsigned char *buf;
  long long   sumIval =0  , sumQval = 0;
  double sumsqIval = 0.0, sumsqQval = 0.0;
  double dsum,dtot;
  double vari;
  float belowZeroCnt = 0.0;
  float above255Cnt = 0.0;
  float scale = 1.0;

  /* Set processing options
   -----------------------*/  
  strcpy(infile,inFile);
  if (mode == CEOS_SLC)
    {
      strcat(infile,".cpx");
      floatBuf = (float *) MALLOC (ddr->ns*2*sizeof(float));
    }
  else if (mode == CEOS_LOW)
    {
      strcat(infile,".img");
      floatBuf = (float *) MALLOC (ddr->ns*sizeof(float));

      /* Scale value for output pixel values */
      /* JBN (04-01-02) removed this whole structure. Now that ARDOP is calibrateable,
	 these values can be scaled by an antenna pattern application.
      if (strncmp(meta->info->sensor,"ERS1",4)==0) scale = 3.75;
      else if (strncmp(meta->info->sensor,"ERS2",4)==0) scale = 6.0;
      else if (strncmp(meta->info->sensor,"JERS1",4)==0) scale = 1.07;
      else if (strncmp(meta->info->sensor,"RSAT-1",4)==0)
	{
	   scale = 8.80; 
 	} */

    }
  nl = ddr->nl;
  ns = ddr->ns;
  strcat(strcpy(outfile,outFile),".D");

  /* Get the data histogram structure ready
   ---------------------------------------*/
  prepare_pdhr(mode, leader, nl, ns);
  i = leader->pdhr.data;
  if (mode == CEOS_SLC) { q = i->next; }

  /* Determine size of output line, allocate buffers 
   ------------------------------------------------*/
  switch (mode)
   {
     case CEOS_SLC: nbytes = ns * 4; break;
     case CEOS_LOW: nbytes = ns; break;
   }
  nbytes += H_SZ;
  nbytes += R_SZ;
  buf = (unsigned char *) MALLOC (nbytes);

  /* Pack 12 byte header, fill IOF_VFDR, convert to ascii, & write it out
   ---------------------------------------------------------------------*/
  fpo = FOPEN(outfile,"wb");
  init_hdr(IOFDR,nbytes,(struct HEADER *) buf);
  fill_iof_vfdr(mode,&vfdr,nl,ns,nbytes,leader->dssr.product_id);
  Code_IOF(buf,&vfdr,toASCII);
  for (ii=448; ii<nbytes; ii++) {    /*fill IOF_VFDR to the end with spaces*/
    buf[ii] = ' ';
  }
  FWRITE(buf,nbytes,1,fpo);

  /* Initialize header structures 
   ------------------------------*/
  init_hdr(mode,nbytes,(struct HEADER *) buf);
  init_rhdr(mode,leader,ns,meta,(struct RHEADER *) &buf[12]);
  fpi = FOPEN(infile,"rb");

  /* Loop through file, converting and writing image lines 
   ------------------------------------------------------*/
  for (line=0; line<nl; line++)
   {
    if (mode == CEOS_SLC)
     {
      FREAD(floatBuf,sizeof(float),ns*2,fpi);
      ofbuf = (short *)&buf[192];
      for (ii=0; ii<ns*2; ii+=2)
       {
 	double itmp, qtmp;
	short  iVal, qVal;
	long   bigNum;

	itmp = (double) (floatBuf[ii] * (SLC_AVG / 10.0));
	qtmp = (double) (floatBuf[ii+1] * (SLC_AVG / 10.0));

	ofbuf[ii]   = iVal = (short) (itmp);
	ofbuf[ii+1] = qVal = (short) (qtmp);

        sumIval+=iVal;
	sumsqIval += (double)(iVal*iVal);

	sumQval+=qVal;
	sumsqQval += (double)(qVal*qVal);

	bigNum = (iVal+SLC_AVG) / i->smp_inc; iVal = (short) (bigNum);
	bigNum = (qVal+SLC_AVG) / q->smp_inc; qVal = (short) (bigNum);

	i->data_values_hist[iVal]++;
        q->data_values_hist[qVal]++;
       }
     }
    else
     {
      FREAD(floatBuf,sizeof(float),ns,fpi);

      for (ii=192; ii<nbytes; ii++)
       {
        int tmp;

	if (floatBuf[ii-192] < 0.0)
	  {
	    buf[ii] = 0;
	    belowZeroCnt+=1.0;
	  }
	else 
	  {
	    floatBuf[ii-192]*=scale;

	    if (floatBuf[ii-192] > 255.0) 
	     {
	       buf[ii] = 255;
	       above255Cnt+=1.0;
	     }
	    else buf[ii] = (unsigned char) floatBuf[ii-192]+0.5;
	  }

	tmp = (int) buf[ii];
	sumIval += tmp;
        sumsqIval += (double) (tmp*tmp);
        if (i->data_values_hist == NULL) printf("MALLOC PROBLEM\n");
        i->data_values_hist[tmp]+= 1;
       }
     }

    FWRITE(buf,nbytes,1,fpo);
    inc_hdr((struct HEADER *)buf);
    inc_rhdr(mode, meta, ns, (struct RHEADER *)&buf[12]);
    if (line%500==0) printf("...Converting Line %i\n",line);
   }
  fclose(fpi);
  fclose(fpo);

  if (mode == CEOS_LOW)
    {
      printf("Total Points %i\n",ns*nl);
      printf("Percentage below zero %f\n",(float)belowZeroCnt/(float)(ns*nl));
      printf("Percentage above 255  %f\n",(float)above255Cnt/(float)(ns*nl));
    }


  get_hist_stats(i,nl,ns);
  dtot = (double) ns*nl;
  dsum = (double) sumIval;
  i->mean_smp = dsum / dtot;
  vari = (sumsqIval-(dsum*dsum)/dtot)/(dtot-1.0);
  i->std_smp = sqrt(vari);

  if (mode == CEOS_SLC)
   {
    get_hist_stats(q,ns,nl); 
    dsum = (double) sumQval;
    q->mean_smp = dsum / dtot;
    vari = (sumsqQval-(dsum*dsum)/dtot)/(dtot-1.0);
    q->std_smp = sqrt(vari);
   }

  if (mode == CEOS_LOW) leader->facdr.nsatpnts = i->data_values_hist[255];
}

void prepare_pdhr(int mode, ceosLeader *leader, int nl, int ns)
 {
    int cnt;
    struct hist_dset *i;
    struct hist_dset *q;

    leader->pdhr.seq_num = 2;
    leader->pdhr.sar_chan = 1;
    leader->pdhr.ltab = 2296;
    leader->pdhr.data = (struct hist_dset *) MALLOC (sizeof(struct hist_dset));
    i = leader->pdhr.data;
    i->nrec = 1; i->tab_seq = 1;
    i->nbin = 256;
    i->ns_lin = i->ngrp_lin = i->nsamp_lin = ns;
    i->ns_pix = i->ngrp_pix = i->nsamp_pix = nl; i->nhist = i->nbin;
    i->data_values_hist = (int *) MALLOC (i->nhist * sizeof(int));
    for (cnt=0; cnt< (i->nhist); cnt++) i->data_values_hist[cnt] = 0;

    if (mode == CEOS_LOW)
      {
	leader->pdhr.ntab = 1;
	strcpy(i->hist_desc,"DETECTED DATA");
        i->min_smp = 0.0;
        i->max_smp = 255.0;
        i->smp_inc = 1.0;
	i->next = NULL;
      }
    else if (mode == CEOS_SLC)
      {
	leader->pdhr.ntab = 2;
	strcpy(i->hist_desc,"REAL COMPONENT");
        i->min_smp = -32768.0;
        i->max_smp = 32767.0;
        i->smp_inc = 256.0;
        i->next = (struct hist_dset *) MALLOC (sizeof(struct hist_dset));

        /* Prepare Q component Histogram
        ------------------------------*/
        q = i->next;
        strcpy(q->hist_desc,"IMAGINARY COMPONENT");
        q->min_smp = -32768.0;
        q->max_smp = 32767.0;
        q->nrec = 1; q->tab_seq = 2; q->nbin = 256;
        q->ns_lin = q->ngrp_lin = q->nsamp_lin = ns;
        q->ns_pix = q->ngrp_pix = q->nsamp_pix = nl; q->nhist = q->nbin;
        q->data_values_hist = (int *) MALLOC (q->nhist * sizeof(int));
        for (cnt=0; cnt<(q->nhist); cnt++) q->data_values_hist[cnt] = 0;
        q->smp_inc = 256;
        q->next = NULL;
      }

    printf("Histogram Structure Initialized\n");
 }
