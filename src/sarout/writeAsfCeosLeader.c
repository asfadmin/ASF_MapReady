/****************************************************************
FUNCTION NAME:

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"
#include "asf_endian.h"
#include "ceos_lfdr.h"
#include "sarout.h"

int init_hdr(int type, int size, struct HEADER *h);
int inc_hdr(struct HEADER *bf);
void writeAsfCeosLeader(int mode,ceosLeader *data,char *filename);

void writeAsfCeosLeader(int mode,ceosLeader *data,char *filename)
 {
   unsigned char buf[20000];
   struct FDR    lfdr;
   int 	  	 i, nbytes;
   FILE		 *fpo;
   char          lfile[256];

   /* Open output file 
    -----------------*/ 
   strcat(strcpy(lfile,filename),".L");
   fpo = FOPEN(lfile,"wb");

   /* Create the Leader File Descriptor Record
   ------------------------------------------*/
   nbytes = 720;
   for (i=0; i<nbytes; i++) buf[1] = ' ';
   init_hdr(IOFDR,nbytes,(struct HEADER *) buf);
   lfdr = default_lfdr;

   /* Set file name
    --------------*/
   strcpy(lfdr.product_id,data->dssr.product_id);

   if (data->mpdr.nlines > 0)
     {
	lfdr.n_mpdr = 1;
	lfdr.l_mpdr = 1620;
     }

   /* Write the leader file descriptor record
    ----------------------------------------*/
   Code_FDR(buf, &lfdr, toASCII);
   FWRITE(buf,nbytes,1,fpo); 
   printf("Finished with leader file descriptor record.\n");
 
   /* Write the data set summary record
   -----------------------------------*/
   nbytes = 4096;
   for (i=0; i<nbytes; i++) buf[i] = ' ';
   init_hdr(DSSR,nbytes,(struct HEADER *) buf);
   Code_DSSR(buf,&(data->dssr),1,toASCII);
   FWRITE(buf,nbytes,1,fpo);
   printf("Finished with data set summary record.\n");
   fflush(NULL);

   /* Write map projection data record
    ---------------------------------*/
   if (data->mpdr.nlines > 0)
    {
      nbytes = 1620;
      for (i=0; i<nbytes; i++) buf[i] = ' ';
      init_hdr(MPDR,nbytes,(struct HEADER *) buf);
      Code_MPDR(buf,&(data->mpdr),toASCII);
      FWRITE(buf,nbytes,1,fpo);
      printf("Finished with map projection data record.\n");
      fflush(NULL);
    }

   /* Write platform postion data record
    -----------------------------------*/
   nbytes = 1024;
   for (i=0; i<nbytes; i++) buf[i] = ' ';
   init_hdr(PPDR,nbytes,(struct HEADER *) buf);
   Code_PPDR(buf,&(data->ppdr),toASCII);
   FWRITE(buf,nbytes,1,fpo);
   printf("Finished with platform position data record.\n");
   fflush(NULL);

   /* Write the attitude data record
    -------------------------------*/
   nbytes = 1024;
   for (i=0; i<nbytes; i++) buf[i] = ' ';
   init_hdr(ATDR,nbytes,(struct HEADER *) buf);
   Code_ATDR(buf,&(data->atdr),toASCII);
   FWRITE(buf,nbytes,1,fpo);
   printf("Finished with the attitude data record.\n");
   fflush(NULL);

   /* Write the radiometric data record
    ----------------------------------*/
   if (mode != CEOS_CCSD)
    {
      nbytes = 4232;
      for (i=0; i<nbytes; i++) buf[i] = ' ';
      init_hdr(RADR,nbytes,(struct HEADER *) buf);
      Code_RADDR(buf,&(data->raddr),toASCII);
      FWRITE(buf,nbytes,1,fpo);
      printf("Finished with the radiometric data record.\n");
      fflush(NULL);
    }

   /* Write the data quality summary record
    --------------------------------------*/
   nbytes = 1620;
   for (i=0; i<nbytes; i++) buf[i] = ' ';
   init_hdr(DQSR,nbytes,(struct HEADER *) buf);
   Code_DQS(buf,&(data->dqsr),1,toASCII);
   FWRITE(buf,nbytes,1,fpo);
   printf("Finished with data quality summary record.\n");
   fflush(NULL);

   /* Write signal data histogram record
    -----------------------------------*/
   nbytes = 4628;
   for (i=0; i<nbytes; i++) buf[i] = ' ';
   init_hdr(SDHR,nbytes,(struct HEADER *) buf);
   Code_DHR(buf,&(data->sdhr),toASCII);
   FWRITE(buf,nbytes,1,fpo);
   printf("Finished with signal data histogram record.\n");
   fflush(NULL);

   /* Write processed data histogram record
    --------------------------------------*/
   if (mode != CEOS_CCSD)
    {
      nbytes = 4628;
      for (i=0; i<nbytes; i++) buf[i] = ' ';
      init_hdr(PDHR,nbytes,(struct HEADER *) buf);
      Code_DHR(buf,&(data->pdhr),toASCII); 
      FWRITE(buf,nbytes,1,fpo);
      printf("Finished with processed data histogram record.\n");
      fflush(NULL);
    }

   /* Write range spectra record
    ---------------------------*/
   nbytes = 5120;
   for (i=0; i<nbytes; i++) buf[i] = ' ';
   init_hdr(RNSR,nbytes,(struct HEADER *) buf);
   Code_RSR(buf,&(data->rsr),toASCII);
   FWRITE(buf,nbytes,1,fpo);
   printf("Finished with range spectra record.\n");
   fflush(NULL);

   /* Write facility related data record
    -----------------------------------*/
   nbytes = 1717;
   for (i=0; i<nbytes; i++) buf[i] = ' ';
   init_hdr(FACDR,nbytes,(struct HEADER *) buf);
   Code_FACDR(buf,&(data->facdr),1,toASCII);
   FWRITE(buf,nbytes,1,fpo);
   printf("Finished with facility related data record.\n");
   fflush(NULL);

   fclose(fpo);
 }
