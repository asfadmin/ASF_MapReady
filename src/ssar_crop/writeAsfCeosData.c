/******************************************************************************
NAME:	writeAsfCeosData - creates a CEOS formatted .D file

SYNOPSIS: writeAsfCeosData(struct IOF_VFDR *vfdr, unsigned char *rhdr, 
		unsigned char *ibuf,int nl,int ns,char *ofile)

	struct IOF_VFDR *vfdr 	Data file descriptor record
				- will be modified to this image's size
	unsigned char *rhdr	Radarsat ERA 180 byte header
				- used as a template
	unsigned char *ibuf	Input image data to be written
	int nl,int ns		Number of lines and samples to be written
	char *ofile		Name of the output file to create

DESCRIPTION:

	Creates a CEOS formatted file:
	 - Pack 12 byte header and IOFDR; write to output file
	 - Initialize RSAT data header
	 - For each image line
		- pack the image data with the RSAT header
		- write the image line
		- update the RSAT header

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    7/97   T. Logan	Create ASF Complient outputs

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "caplib.h"
#include "ceos.h"
#include "ddr.h"
#include "asf_endian.h"

int init_hdr(int type, int size, struct HEADER *h);
int inc_hdr(struct HEADER *bf);
int inc_rhdr(struct RHEADER *bf);
void change_iof_vfdr(struct IOF_VFDR *v,int nl, int np, int nbytes);
void change_rhdr(struct RHEADER *r,int lc,int dc,int rc);
void scan_line(unsigned char *buf,int ns, int *lc, int *dc, int *rc);

int writeAsfCeosDatafile(struct IOF_VFDR *vfdr, unsigned char *rhdr, 
		unsigned char *ibuf,int nl,int ns,char *ofile)
{
  unsigned char   *buf;
  /*struct HEADER	  hdr;*/
  char   outfile[256];
  FILE   *fpo;
  int    nbytes, j/*, cnt*/, line;
  int    i;

  /* Determine size of output line, allocate buffer 
   -----------------------------------------------*/
  strcat(strcpy(outfile,ofile),".D");
  nbytes = ns;
  nbytes += H_SZ;
  nbytes += R_SZ;
  buf = (unsigned char *) MALLOC (nbytes);

  /* Pack 12 byte header, fill IOF_VFDR, convert to ascii, write header
   --------------------------------------------------------------------*/
  fpo = FOPEN(outfile,"wb");
  init_hdr(IOFDR,nbytes,(struct HEADER *) buf);
  change_iof_vfdr(vfdr,nl,ns,nbytes);
  Code_IOF(buf,vfdr,toASCII);
  FWRITE(buf,nbytes,1,fpo);

  /* Initialize header structures 
   ------------------------------*/
  for (i=12; i<192; i++) buf[i] = rhdr[i];

  /* Loop through file, converting and writing image lines 
   ------------------------------------------------------*/
  for (line=0; line<nl; line++)
   {
     int lc,dc,rc;  /*left,data,& right pixel counts */

     scan_line(&ibuf[line*ns],ns,&lc,&dc,&rc);
     change_rhdr((struct RHEADER *)&(buf[12]),lc,dc,rc);
     for (j=0; j<ns; j++) { buf[j+192] = ibuf[line*ns+j]; } 
     FWRITE(buf,nbytes,1,fpo);
     inc_hdr((struct HEADER *)buf);
     inc_rhdr((struct RHEADER *)&(buf[12]));
     if (line%500==0) printf("  ...Converting Line %i\n",line);
   }
  fclose(fpo);
  return(0);
}


/* Scans a line of data, counting the left fill, data, and right fill pixels
 --------------------------------------------------------------------------*/
void scan_line(unsigned char *buf,int ns, int *lc, int *dc, int *rc)
  {
    int i=0;
    int left=1,data=0,right=0;

    *lc = ns;
    *rc = 0;
    *dc = 0;

    for (i=0; i<ns; i++)
     {
        if (left && buf[i]!=0) { *lc = i; left = 0; data = 1; }
	else if (data && buf[i]==0) { *dc = i - *lc; data = 0; right = 1; }
	else if (right && buf[i]!=0) { right = 0; data = 1; }
     }
    *rc = ns - (*lc + *dc);
  }

/* Set image size fields in the image file descriptor record */
void change_iof_vfdr(struct IOF_VFDR *v,int nl, int np, int nbytes)
{
 v->reclen      = nbytes;
 v->sardata     = nbytes - (H_SZ+R_SZ);
 v->numofrec    = nl;
 v->linedata    = nl;
 v->datgroup    = np;
}

/*---------------------------------------------------------------
        ROUTINES TO PROCESS STANDARD 12 BYTE LEADER
 ---------------------------------------------------------------*/
/* Pack 12 byte header based on record type */
init_hdr(int type, int size, struct HEADER *h)
 {
  static int nxt_num = 1;
  int actual_size;

  actual_size = size;
  h->rectyp[0] = '\12';
  h->rectyp[2] = '\22';
  h->rectyp[3] = '\24';
  bigInt32_out(nxt_num,&(h->recnum[0]));

  switch (type) {
   case CEOS_SIC:
   case CEOS_FUL:
   case CEOS_LOW:
  		bigInt32_out(2,&(h->recnum[0]));
	        h->rectyp[0]= '2'; h->rectyp[1]='\13'; break;
   case IOFDR:  h->rectyp[0]='?'; h->rectyp[1]='\300';h->rectyp[3]='\22'; break;
   case DSSR:   h->rectyp[1]='\12'; actual_size=4096; break;
   case MPDR:   h->rectyp[1]='\24'; actual_size=1620; break;
   case PPDR:   h->rectyp[1]='\36'; actual_size=1024; break;
   case ATDR:   h->rectyp[1]='\50'; actual_size=1024; break;
   case RADR:   h->rectyp[1]='\62'; actual_size=4232; break;
   case DQSR:   h->rectyp[1]='\74'; actual_size=1620; break;
   case SDHR:   h->rectyp[1]='\106';actual_size=4628; break;
   case PDHR:   h->rectyp[1]='\106';actual_size=4628; break;
   case RNSR:   h->rectyp[1]='\120';actual_size=5120; break;
   case DEMR:   h->rectyp[1]='\132';actual_size=1024; break;
   case FACDR:  h->rectyp[1]='\310';h->rectyp[3]='\75';actual_size=1717; break;
   default:     printf("Can't find type %i\n",type); exit(1);
  }
  bigInt32_out(actual_size,&(h->recsiz[0]));
  if (type != CEOS_SIC && type != CEOS_FUL && type != CEOS_LOW) nxt_num++;

  return(0);
 }

/* Increment header record counter */ 
inc_hdr(struct HEADER *bf)
 { int tmp;
   tmp = bigInt32(bf->recnum);
   tmp++;
   bigInt32_out(tmp,&(bf->recnum[0]));
   return(0);
 }

/*---------------------------------------------------------------
        ROUTINES TO PROCESS RADARSAT ERA 180 BYTE LEADER
 ---------------------------------------------------------------*/
/* Set left,data, & right pixel counts for RHEADER */
void change_rhdr(struct RHEADER *r,int lc,int dc,int rc)
 {
  r->n_left_pixel = lc;
  r->n_data_pixel = dc;
  r->n_right_pixel = rc;
 }

/* Increment RHEADER record counter */
inc_rhdr(struct RHEADER *bf) 
  {
     bf->line_num++;
     return(0);
  }

