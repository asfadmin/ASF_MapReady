/*Get_ceos:contains

getCeosRecord:
  Finds and returns a single record in the given CEOS file.

get_*:
  Finds and decodes CEOS record of the specified type from the CEOS file.
*/
#include "asf.h"
#include <ctype.h>
#include "ceos.h"
#include "asf_meta.h"
#include "asf_endian.h"

/*Enumeration of the numeric types of various CEOS fields.*/
typedef enum {
  CEOS_ATDR=40,         /* Attitude data record.*/
  CEOS_DHR=70,          /* Data histograms record.*/
  CEOS_DQSR=60,         /* Data quality summary record.*/
  CEOS_DSSR=10,         /* Data set summary record.*/
  CEOS_ASFFACDR=210,    /* ASF Facility-related data record.*/
  CEOS_ESAFACDR=220,    /* ESA Facility-related data record.*/
  CEOS_JAXAFACDR=230,   // JAXA Facility-related data record
  CEOS_FACDR=200,       /* Pre-1996 Facdr*/
  CEOS_MPDR=20,         /* Map-projection data record.*/
  CEOS_PPDR=30,         /* Platform position data record.*/
  CEOS_RADDR=50,        /* Radiometric data record.*/
  CEOS_RSR=80,          /* Range Spectra Record.*/
  CEOS_IFILEDR=192,     /* Imagery options file.*/
  CEOS_FDR=300,         /* File Descriptor Record.*/
  CEOS_PPR=120,         /* Processing Parameter Record - CDPF defined.*/
  CEOS_SHR=18,          // Scene Header Record - ALOS
  CEOS_AMPR=36,         // Map Projection Record - ALOS
  CEOS_ARDR=50,         // Radiometric Data Record - ALOS
  CEOS_RCDR=51,         // Radiometric Compensation Data Record - RSI
  CEOS_TFDR=193         // Trailer File Descriptor Record - ALOS
} CEOS_RECORD_TYPE;

void dumpCeosRecord(const char *inName)
{
  FILE *fp;
  struct HEADER bufhdr;
  char **metaName=NULL;
  struct trl_file_des_rec *tfdr=NULL;
  int ii, trailer, total;
  unsigned char *buff;

  require_ceos_metadata(inName, &metaName, &trailer);

  for (ii=0; ii<trailer+1; ii++) {
    fp = FOPEN(metaName[ii], "rb");
    total = 0;

    printf("\nFile: %s\n", metaName[ii]);
    while (1==fread(&bufhdr, 12, 1, fp)) {
      int itype, subtype[3], rec_seq, length, mallocBytes;
      subtype[0] = bufhdr.rectyp[0];
      itype = bufhdr.rectyp[1];
      subtype[1] = bufhdr.rectyp[2];
      subtype[2] = bufhdr.rectyp[3];
      rec_seq = bigInt32(bufhdr.recnum);
      length = bigInt32(bufhdr.recsiz);
      printf(">>> length=%d\n", length);
      total += length;
      if ((itype==CEOS_FACDR && rec_seq==17 && length<=5000) ||
          (itype==CEOS_FACDR && rec_seq==18)) {
	tfdr = (struct trl_file_des_rec *) 
	  MALLOC(sizeof(struct trl_file_des_rec));
	if (trailer)
	  get_tfdr(metaName[1], tfdr);
	else
	  get_tfdr(metaName[0], tfdr);
	mallocBytes = length = tfdr->facdr_len[10];
      }
      else
	mallocBytes = (length>16920) ? length : 16920;
      buff=(unsigned char *)MALLOC(mallocBytes);
      *(struct HEADER *)buff=bufhdr;
      
      printf("\nrecord type: %d ", itype);
      switch (itype)
	{
	case (40):
	  printf("(Attitude data record)\n");
	  break;
	case (70):
          printf("(Data histograms record)\n");
	  break;
	case (60):
	  printf("(Data quality summary record)\n");
	  break;
	case (10):
	  printf("(Data set summary record)\n");
	  break;
	case (200):
	case (210):
	  printf("(Facility-related data record)\n");
	  break;
	case (20):
	case (36):
	  printf("(Map-projection data record)\n");
	  break;
	case (30):
	  printf("(Platform position data record)\n");
	  break;
	case (50):
	  printf("(Radiometric data record)\n");
	  break;
	case (80):
	  printf("(Range Spectra Record)\n");
	  break;
	case (192):
	  printf("(Imagery options file)\n");
	  break;
	case (300):
	  printf("(File Descriptor Record)\n");
	  break;
	case (120):
	  printf("(Processing Parameter Record)\n");
	  break;
	case (18):
	  printf("(Scene Header Record)\n");
	  break;
	case (51):
	  printf("(Radiometric Compensation Data Record)\n");
	  break;
	case (193):
	  printf("(Trailer File Descriptor Record)\n");
	  break;
	}

      printf("sub-record[1]: %d, sub-record[2]: %d sub_record[3]: %d\n",
	     subtype[0], subtype[1], subtype[2]);
      printf("sequence: %d, length: %d\n", rec_seq, length);

      FREAD((buff)+12, length-12, 1, fp);
    }
    FCLOSE(fp);
    
    printf("\nTotal length of records: %i\n", total);
  }
  if (tfdr)
    FREE(tfdr);


  free_ceos_names(NULL, metaName);
}

int getCeosRecord(const char *inName, CEOS_RECORD_TYPE recordType, int recordNo,
                  unsigned char **buff)
{
  FILE *fp;
  char **dataName=NULL, **metaName=NULL;
  struct HEADER  bufhdr;
  struct trl_file_des_rec *tfdr=NULL;
  int nOccurences=0, era=1, ii, trailer, total;
  long long size;

  if (recordType==CEOS_IFILEDR) {
            int nBands;
            require_ceos_pair(inName, &dataName, &metaName, &nBands, &trailer);
            strcpy(metaName[0], dataName[0]);
  } else {
            require_ceos_metadata(inName, &metaName, &trailer);
            /* If looking for FDR record type, set it to IFILEDR type */
            if (recordType==CEOS_FDR)
                recordType=CEOS_IFILEDR;
  }

  for (ii=0; ii<trailer+1; ii++) {
    size = fileSize(metaName[ii]);
    fp=FOPEN(metaName[ii], "rb");
    total = 0;
    while (1==fread(&bufhdr, 12, 1, fp)) {
      int itype,subtype[3],rec_seq,length,mallocBytes;
      subtype[0] = bufhdr.rectyp[0];
      itype = bufhdr.rectyp[1];
      subtype[1] = bufhdr.rectyp[2];
      subtype[2] = bufhdr.rectyp[3];
      rec_seq = bigInt32(bufhdr.recnum);
      length = bigInt32(bufhdr.recsiz);
      if ((itype==CEOS_FACDR && rec_seq==17 && length<=5000) ||
          (itype==CEOS_FACDR && rec_seq==18)) {
	tfdr = (struct trl_file_des_rec *) 
	  MALLOC(sizeof(struct trl_file_des_rec));
	if (trailer)
	  get_tfdr(metaName[1], tfdr);
	else
	  get_tfdr(metaName[0], tfdr);
	mallocBytes = length = tfdr->facdr_len[10];
      }
      else
	mallocBytes = (length>16920) ? length : 16920;
      total += length;
      *buff=(unsigned char *)MALLOC(mallocBytes);
      *(struct HEADER *)*buff=bufhdr;
      /*
      printf("get_ceos - record type: %d\nsub-record[1]: %d, sub-record[2]: %d"
	     " sub_record[3]: %d\nsequence: %d, length: %d\n", itype, 
	     subtype[0], subtype[1], subtype[2], rec_seq, length);
      */
      if ((long long)total-length > size)
        break;
      else
	    FREAD((*buff)+12, length-12, 1, fp);

      // The JAXA FACDR requires the sequence number to be able to pick
      // the correct one. For level 1.1 it is sequence 17, for level 1.5
      // it is sequence 18.

      if ((itype==recordType)||
          (itype==CEOS_FACDR && recordType==CEOS_ASFFACDR) ||
          (itype==CEOS_FACDR && recordType==CEOS_ESAFACDR) ||
          (itype==CEOS_FACDR && rec_seq==17 && recordType==CEOS_JAXAFACDR &&
	   length<=5000) ||
          (itype==CEOS_FACDR && rec_seq==18 && recordType==CEOS_JAXAFACDR) ||
	  (itype==CEOS_IFILEDR && recordType==CEOS_TFDR))
      {/*We have the correct kind of record.*/
        nOccurences++;
        if (nOccurences==recordNo)
        { /*This is the correct occurence! Clean up and return.*/
          FCLOSE(fp);
          free_ceos_names(dataName, metaName);
          return era;
        }
        else /*Move on.*/
          FREE(*buff);
      }
      else /*Move on.*/
        FREE(*buff);
    }
    FCLOSE(fp);
  }

  free_ceos_names(dataName, metaName);
  if (tfdr)
    FREE(tfdr);

  if (recordType==CEOS_MPDR ||
      recordType==CEOS_DQSR ||
      recordType==CEOS_DHR  ||
      recordType==CEOS_PPR)
  {
    return -1;/*It's OK if the MPDR, DQSR, or DHR are missing.*/
  }
  return -1;
}

int get_atdr(const char *filename,struct att_data_rec *rec)
{
  unsigned char *buff;
  int era;
  if ((era = getCeosRecord(filename,CEOS_ATDR,1,&buff)) != -1) {
    Code_ATDR(buff,rec,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_dhr(const char *filename,struct data_hist_rec *rec)
{
  unsigned char *buff;
  int era;
  if ((era = getCeosRecord(filename,CEOS_DHR,2,&buff)) != -1) {
    Code_DHR(buff,rec,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_sdhr(const char *filename,struct data_hist_rec *rec)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_DHR,1,&buff)) != -1) {
    Code_DHR(buff,rec,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_dqsr(const char *filename,struct qual_sum_rec *rec)
{
  unsigned char *buff;
  int era;
  if ((era=getCeosRecord(filename,CEOS_DQSR,1,&buff)) != -1) {
    Code_DQS(buff,rec,era,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_dssr(const char *filename,struct dataset_sum_rec *rec)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_DSSR,1,&buff)) != -1) {
    Code_DSSR(buff,rec,era,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_asf_facdr(const char *filename,struct VFDRECV *rec)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_ASFFACDR,1,&buff)) != -1) {
    Code_ASF_FACDR(buff,rec,era,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_esa_facdr(const char *filename,struct ESA_FACDR *rec)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_ESAFACDR,1,&buff)) != -1) {
    Code_ESA_FACDR(buff,rec,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_jaxa_facdr(const char *filename,struct JAXA_FACDR *rec)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_JAXAFACDR,1,&buff)) != -1) {
    struct trl_file_des_rec *tfdr =
        (struct trl_file_des_rec *) MALLOC(sizeof(struct trl_file_des_rec));
    get_tfdr((char*)filename, tfdr);
    Code_JAXA_FACDR(buff,rec,fromASCII,tfdr->facdr_len[10]);
    FREE(buff);
  }

  return era;
}

int get_mpdr(const char *filename,struct VMPDREC *rec)
{
  unsigned char *buff;
  int era=getCeosRecord(filename,CEOS_MPDR,1,&buff);
  if (era!=-1)
  {
    Code_MPDR(buff,rec,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_ppdr(const char *filename,struct pos_data_rec *rec)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_PPDR,1,&buff)) != -1) {
    Code_PPDR(buff,rec,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_raddr(const char *filename,struct VRADDR *rec)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_RADDR,1,&buff)) != -1) {
    Code_RADDR(buff,rec,fromASCII);
    FREE(buff);
  }
  return era;
}
int get_rsi_raddr(const char *filename, struct RSI_VRADDR *rec)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_RADDR,1,&buff)) != -1) {
    Code_RSI_RADDR(buff,rec,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_rsr(const char *filename,struct rng_spec_rec *rec)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_RSR,1,&buff)) != -1) {
    Code_RSR(buff,rec,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_ifiledr(const char *filename,struct IOF_VFDR *vfdr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_IFILEDR,1,&buff)) != -1) {
    Code_IOF((unsigned char *)buff, vfdr,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_ALOS_optical_ifiledr(const char *filename,struct IOF_VFDR *vfdr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_IFILEDR,1,&buff)) != -1) {
    Code_ALOS_optical_IOF((unsigned char *)buff, vfdr,fromASCII);
    FREE(buff);
  }
  return era;
}

int get_fdr(const char *filename,struct FDR *fdr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_FDR,1,&buff)) != -1) {
    Code_FDR(buff,fdr,fromASCII);
    FREE(buff);
  }
  return(era);
}

int get_ppr(const char *filename,struct proc_parm_rec *ppr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_PPR,1,&buff)) != -1) {
    Code_PPR(buff,ppr,fromASCII);
    FREE(buff);
  }
  return(era);
}

int get_rcdr(const char *filename, struct radio_comp_data_rec *rcdr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_RCDR,1,&buff)) != -1) {
    Code_RCDR(buff,rcdr,fromASCII);
    FREE(buff);
  }
  return(era);
}

int get_shr(const char *filename, struct scene_header_rec *shr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_SHR,1,&buff)) != -1) {
    Code_SHR(buff,shr,fromASCII);
    FREE(buff);
  }
  return(era);
}

int get_ampr(const char *filename, struct alos_map_proj_rec *ampr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_AMPR,1,&buff)) != -1) {
    Code_AMPR(buff,ampr,fromASCII);
    FREE(buff);
  }
  return(era);
}

int get_ardr(const char *filename, struct alos_rad_data_rec *ardr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename, CEOS_ARDR, 1, &buff)) != -1) {
    Code_ARDR(buff,ardr,fromASCII);
    FREE(buff);
  }
  return(era);
}

int get_tfdr(char *filename,struct trl_file_des_rec *tfdr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename, CEOS_TFDR, 1, &buff)) != -1) {
    Code_TFDR(buff,tfdr,fromASCII);
    FREE(buff);
  }
  return(era);
}

/*
int get_vdr(char *filename,struct VDREC *vdr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_VDR,1,&buff)) != -1) {
    Code_VDR(buff,vdr,fromASCII);
    FREE(buff);
  }
  return(era);
}

int get_lfpr(char *filename,struct FPREC *fpr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_LFPR,1,&buff)) != -1) {
    Code_LFPR(buff,fpr,fromASCII);
    FREE(buff);
  }
  return(era);
}

int get_dfpr(const char *filename,struct FPREC *nvdr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_DFPR,1,&buff)) != -1) {
    Code_DFPR(buff,nvdr,fromASCII);
    FREE(buff);
  }
  return(era);
}

int get_tr(const char *filename,struct TREC *tr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_TR,1,&buff)) != -1) {
    Code_TR(buff,tr,fromASCII);
    FREE(buff);
  }
  return(era);
}

int get_nvdr(const char *filename,struct VDREC *nvdr)
{
  unsigned char *buff;
  int era;
  if ( (era = getCeosRecord(filename,CEOS_NVDR,1,&buff)) != -1) {
    Code_NVDR(buff,nvdr,fromASCII);
    FREE(buff);
  }
  return(era);
}
*/
