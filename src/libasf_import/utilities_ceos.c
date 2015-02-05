#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "ceos.h"
#include "decoder.h"
#include "dateUtil.h"
#include "get_ceos_names.h"

/*Internal Metadata creation routine prototype*/
void ceos_init(const char *in_fName, meta_parameters *sar, report_level_t level);


/******************************************************************************
 * Return the length (in lines) of the given CEOS image */
int ceosLen(char *inN)
{
  struct IOF_VFDR viof;/*Imagery Options File (values)*/
  get_ifiledr(inN,&viof);
  return viof.numofrec;
}

/******************************************************************************
 * createMeta_ceos:
 * Create outN.meta file using information from CEOS in the inN.L file.  */
void createMeta_ceos(bin_state *s, struct dataset_sum_rec *dssr, char *inN,
                     char *outN)
{
  meta_parameters *meta=raw_init();

  ceos_init(inN, meta, REPORT_LEVEL_NONE);
  s->lookDir = meta->sar->look_direction;

  /* Check for VEXCEL LZP Data-- has odd state vectors */
  if (0==strncmp(dssr->sys_id,"SKY",3))
  {
  /* Correct for wrong time of image start-- image start time (from DSSR) is
   * *actually* in the center of the image.*/
    double imgLen;/*Half of length of image, in seconds*/
    int i;
    imgLen=ceosLen(inN)/2.0/s->prf;
    asfPrintStatus("   VEXCEL Level-0 CEOS: Shifted by %f seconds...\n",imgLen);
    /*Correct the image start time*/
    meta->state_vectors->second -= imgLen;
    if (meta->state_vectors->second<0) {
      meta->state_vectors->julDay--;
      meta->state_vectors->second+=24*60*60;
    }
    /* Correct the time of the state vectors, which are *relative* to image
     * start.*/
    for (i=0;i<meta->state_vectors->vector_count;i++)
      meta->state_vectors->vecs[i].time += imgLen;
  /* State vectors are too far apart or too far from image as read -- propagate
   * them */
    asfPrintStatus("   Updating state vectors...  ");
    fflush(NULL);
    propagate_state(meta, 3, (s->nLines / s->prf / 2.0) );
    asfPrintStatus("Done.\n");
  }

  /* Update s-> fields with new state vector */
  addStateVector(s,&meta->state_vectors->vecs[0].vec);

  /* Update fields for which we have decoded header info */
  updateMeta(s,meta,NULL,0);

  /* Write out and free the metadata structure */
  meta_write(meta,outN);
}

/******************************************************************************
 * convertMetadata_ceos:
 * Creates ARDOP .in and .fmt files, as well as determining the number of lines
 * in the l0 file, by reading the granule (.gran) file.  */
bin_state *convertMetadata_ceos(char *inN, char *outN, int *nLines,
                                readPulseFunc *readNextPulse)
{
  bin_state *s=NULL;
  struct dataset_sum_rec dssr;
  char *satName;

/*Figure out what kind of SAR data we have, and initialize the appropriate
 *decoder.*/
  get_dssr(inN,&dssr);
  satName=dssr.mission_id;

  if (0==strncmp(satName,"E",1))
    s=ERS_ceos_decoder_init(inN,outN,readNextPulse);
  else if (0==strncmp(satName,"J",1)) {
    s=JRS_ceos_decoder_init(inN,outN,readNextPulse);
  }
  else if (0==strncmp(satName,"R",1))
    s=RSAT_ceos_decoder_init(inN,outN,readNextPulse);
  else if (0==strncmp(satName,"A",1))
    s=ALOS_ceos_decoder_init(inN,outN,readNextPulse);
  else
    asfPrintError("Unrecognized satellite '%s'!\n",satName);
  createMeta_ceos(s,&dssr,inN,outN);

/*Write out ARDOP input parameter files.*/
  if (fabs(dssr.crt_dopcen[0]) > 15000)
    writeARDOPparams(s,outN, 0, -99, -99);
  else if (fabs(dssr.crt_dopcen[1]) > 1)
    writeARDOPparams(s,outN, dssr.crt_dopcen[0], -99*s->prf, -99*s->prf);
  else
    writeARDOPparams(s,outN,dssr.crt_dopcen[0],dssr.crt_dopcen[1],
             dssr.crt_dopcen[2]);
  writeARDOPformat(s,outN);

  *nLines=s->nLines;

  return s;
}

/******************************************************************************
 * openCeos:
 * Open the given CEOS image, seek to the beginning of the file, and return a
 * file pointer  */
FILE *openCeos(char *fName, char *outN, bin_state *s)
{
  char **dataName;
  int ii, nBands=1;
  FILE *ret;
  dataName = (char **) MALLOC(512*MAX_BANDS*sizeof(char));
  for (ii=0; ii<MAX_BANDS; ii++)
    dataName[ii] = (char *) MALLOC(512*sizeof(char));
  require_ceos_data(fName,&dataName,&nBands);
  ret=FOPEN(dataName[0],"rb");
  FSEEK64(ret,0,0);/*Seek to beginning of file*/
  getNextCeosLine(ret, s, fName, outN);/*Skip over first line of file*/
  return ret;
}

/******************************************************************************
 * getNextCeosLine:
 * Reads the next entire CEOS record from the given file. Returns pointer into
 * static buffer. */
signalType *getNextCeosLine(FILE *f, bin_state *s, char *inN, char *outN)
{
  static int headerLen=12,totHeaderLen=192;
  static signalType buffer[100000];/*Input Buffer*/
  struct HEADER head;
  int length;
  struct dataset_sum_rec dssr;

  if (headerLen!=fread(&head,1,headerLen,f)) {
    /* create metadata file */
    get_dssr(inN,&dssr);
    createMeta_ceos(s,&dssr,inN,outN);

    /* write out ARDOP input parameter file */
    if (fabs(dssr.crt_dopcen[0])<15000)
      writeARDOPparams(s,outN,dssr.crt_dopcen[0],dssr.crt_dopcen[1],dssr.crt_dopcen[2]);
    else
      writeARDOPparams(s,outN,0,0,0);

    writeARDOPformat(s,outN);

    asfPrintStatus("\n   Wrote %i lines of raw signal data.\n\n",s->nLines);

    exit(EXIT_SUCCESS);
  }

  length=bigInt32(head.recsiz);
  ASF_FREAD(&buffer,1,length-12,f);
  return &buffer[totHeaderLen-headerLen];
}
