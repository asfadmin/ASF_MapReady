#include "asf.h"
#include "ifm.h"
#include "asf_meta.h"
#include "ddr.h"

int getphase(char *phasein, char *tiept, char *outfile)
{
  char ddrin[255];
  FILE *fphase, *fout, *ftie;
  float phase;
  int i=1;
  int wid, len;
  int seek_byte;
  float x, y, ht;
  struct DDR ddr;

  sprintf(ddrin, "%s", phasein);

  /* Get input scene size*/
  i = c_getddr(ddrin, &ddr);
  if (i != 0) { 
    sprintf(errbuf, "   ERROR: Unable to get ddr for file %s\n",ddrin);
    printErr(errbuf); 
  }
  wid = ddr.ns;
  len = ddr.nl;

  /* open input files*/
/*  printf("Opening input files...\n");*/
  ftie = FOPEN(tiept,"r");
  fout = FOPEN(outfile,"w");
  fphase = fopenImage(phasein,"rb");
/*  printf("\nCols = %5d\nRows = %5d",wid,len);*/

  /* get phase for each tie pt. location*/
  while (fscanf(ftie,"%f %f %f\n",&x,&y,&ht) != EOF) {
  	double orig_x=x,orig_y=y;
#define ifPrint if (i<20) printf
/*     ifPrint("Reading tie point at %.0f, %.0f...\n",orig_x,orig_y);*/
     y-=ddr.master_line-1;
     y/=ddr.line_inc;
     x-=ddr.master_sample-1;
     x/=ddr.sample_inc;
     if ((x<0)||(y<0)||(x>=wid)||(y>=len))
     {
	printf("   ERROR: Tie point not within image bounds. Discarding.\n");
       continue;
     }
     seek_byte = ((int)(y) * wid + (int)(x)) * sizeof(float);
     FSEEK(fphase,seek_byte,0);
     FREAD(&phase,sizeof(float),1,fphase);
     if (fabs(phase)<0.0001) {
       printf("   ERROR: Phase equals zero. Discarding-- Escher couldn't unwrap this tie point..\n");
       continue;
     }
     fprintf(fout,"%4d  %9.2f  %9.2f  %10.3f  %.8f\n",i,orig_x,orig_y,ht,phase);
     i++;
  }

  if (!quietflag) printf("   Read %d seed points\n\n", i);

  /* close input files*/
  FCLOSE(fout);
  FCLOSE(ftie);
  FCLOSE(fphase);
  return(0);
}


