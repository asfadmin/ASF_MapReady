#include "asf.h"
#include "ceos.h"
#include "asf_endian.h"


/*Enumeration of the numeric types of various CEOS fields.*/
typedef enum {
        CEOS_ATDR=40,         /* Attitude data record.*/
        CEOS_DHR=70,          /* Data histograms record.*/
        CEOS_DQSR=60,         /* Data quality summary record.*/
        CEOS_DSSR=10,         /* Data set summary record.*/
        CEOS_ASFFACDR=210,    /* ASF Facility-related data record.*/
        CEOS_ESAFACDR=220,    /* ESA Facility-related data record.*/
        CEOS_FACDR=200,       /* Pre-1996 Facdr*/
        CEOS_MPDR=20,         /* Map-projection data record.*/
        CEOS_PPDR=30,         /* Platform position data record.*/
        CEOS_RADDR=50,        /* Radiometric data record.*/
        CEOS_RSR=80,          /* Range Spectra Record.*/
        CEOS_IFILEDR=192,     /* Imagery options file.*/
        CEOS_FDR=300,         /* File Descriptor Record.*/
        CEOS_PPR=120,         /* Processing Parameter Record - CDPF defined.*/
        CEOS_FAKE=51
} CEOS_RECORD_TYPE;

int getCeosRecord(char *inName, CEOS_RECORD_TYPE recordType, int recordNo,
                  unsigned char **buff);


void fill_iof_vfdr(int mode, struct IOF_VFDR *v, int nl, int ns, int nbytes);
int init_hdr(int type, int size, struct HEADER *h);
int inc_hdr(struct HEADER *bf);
int init_rhdr(int ns, meta_parameters *meta, struct RHEADER *r);
int inc_rhdr(meta_parameters *meta, int ns, struct RHEADER *bf);

void updateLutLeader(char *leaderIn, char *leaderOut,
		     char *cal_params_file, char *cal_comment)
{
  FILE *fpIn, *fpOut;
  struct HEADER bufhdr;
  struct dataset_sum_rec dssr;
  struct qual_sum_rec dqsr;
  unsigned char *buf;

  /* Open leader files */
  fpIn = FOPEN(leaderIn, "r");
  fpOut = FOPEN(leaderOut, "w");

  /* Keep reading input leader file headers */
  while (1==fread(&bufhdr, 12, 1, fpIn))
    {
      int itype, length, era;

      /* Check out which record it is */
      itype = bufhdr.rectyp[1];
      length = bigInt32(bufhdr.recsiz);
      buf = (unsigned char *) MALLOC(length);
      *(struct HEADER *)buf = bufhdr;

      /* Read the rest of the record */
      FREAD(buf+12, length-12, 1, fpIn);
      
      /* Change the values that were passed in */
      switch (itype) 
	{
	case CEOS_DSSR:
	  /* Here is the calibration parameter file we want to change */
	  era = getCeosRecord(leaderIn, 10, 1, &buf);
	  Code_DSSR(buf, &dssr, era, fromASCII);
	  strcpy(dssr.cal_params_file, cal_params_file);
	  Code_DSSR(buf, &dssr, era, toASCII);
	  break;
	case CEOS_DQSR:
	  /* Here is the calibration comment we want to change */
	  era = getCeosRecord(leaderIn, 60, 1, &buf);
	  Code_DQS(buf, &dqsr, era, fromASCII);
	  strcpy(dqsr.cal_comment, cal_comment);
	  Code_DQS(buf, &dqsr, era, toASCII);
	  break;
      }

      /* Write out the record as they come */
      FWRITE(buf, length, 1, fpOut);
      fflush(NULL);

      FREE(buf);
    }
  
  FCLOSE(fpIn);
  FCLOSE(fpOut);
}

void writeDataFile(char *image_data_file_name,
		   char *metadata_file_name,
		   char *image_data_output_file)
{
  FILE *fpIn, *fpOut;
  meta_parameters *meta;
  struct IOF_VFDR vfdr;
  int ii, kk;
  int lines, samples, nbytes;
  unsigned char *buf;
  float *floatBuf;

  /* Read metadata and figure out dimensions and data type */
  meta = meta_read(metadata_file_name);
  lines = meta->general->line_count;
  samples = meta->general->sample_count;

  /* Restrictions for image types: only floating point images considered */
  nbytes = samples * sizeof(unsigned char); 
  nbytes += H_SZ; /* General header size */
  nbytes += R_SZ; /* Extended header since RADARSAT launch */

  buf = (unsigned char *) MALLOC (nbytes); /* Current restriction for output data:
					      only 8-bit data supported */
  floatBuf = (float *) MALLOC (samples*sizeof(float));

  /* Open image files */
  fpIn = FOPEN(image_data_file_name, "rb");
  fpOut = FOPEN(image_data_output_file, "wb");

  /* Initialize, fill, convert and write IOF_VFDR header */
  init_hdr(IOFDR, nbytes, (struct HEADER *) buf);
  fill_iof_vfdr(CEOS_LOW, &vfdr, lines, samples, nbytes);
  Code_IOF(buf, &vfdr, toASCII);
  for (ii=448; ii<nbytes; ii++) /* fill end with spaces */
    buf[ii] = ' ';
  FWRITE(buf, nbytes, 1, fpOut);

  /* Initialize header structures */
  init_hdr(CEOS_LOW, nbytes, (struct HEADER *) buf);
  init_rhdr(samples, meta, (struct RHEADER *) &buf[12]);

  /* Loop through data file, converting and writing image lines */
  for (ii=0; ii<lines; ii++) {
    FREAD(floatBuf, sizeof(float), samples, fpIn);
    for (kk=192; kk<nbytes; kk++) {
      /* Map values into 8-bit range */
      if (floatBuf[kk-192] < 0.0) buf[kk] = 0;
      else if (floatBuf[kk-192] > 255.0) buf[kk] = 255;
      /* Round the rest of the values */
      else buf[kk] = (unsigned char) floatBuf[kk-192] + 0.5;
    }
    FWRITE(buf, nbytes, 1, fpOut);
    inc_hdr((struct HEADER *)buf); 
    inc_rhdr(meta, samples, (struct RHEADER *)&buf[12]);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
}

void export_as_ceos (char *metadata_file_name, 
		     char *image_data_file_name,
		     char *output_file_name, 
		     char *leader_file_name,
		     char *calibration_parameter_file, 
		     char *calibration_comment)
{

  char leader_output_file[255]="", image_data_output_file[255]="";

  /* Assign output file names */
  sprintf(leader_output_file, "%s.L", output_file_name);
  sprintf(image_data_output_file, "%s.D", output_file_name);

  /* Update the leader file with passed information */
  updateLutLeader(leader_file_name, leader_output_file,
		  calibration_parameter_file, calibration_comment);

  /* Write the image data file */
  writeDataFile(image_data_file_name, metadata_file_name,
		image_data_output_file);
}
