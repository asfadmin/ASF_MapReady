#include "asf.h"
#include "ceos.h"

void prn_ifiledr(char *file, struct IOF_VFDR *ifdr)
{
  FILE *fp;

  fp = FOPEN(file, "w");
  fprintf(fp, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n");
  fprintf(fp, "\"http://www.w3.org/TR/html4/loose.dtd\">\n<html>\n<head>\n");
  fprintf(fp, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n");
  fprintf(fp, "<title>Image File Descriptor record</title>\n</head>\n<body>\n");
  fprintf(fp, "<h2>Image File Descriptor record</h2>\n");
  fprintf(fp, "<strong>Number of data records: </strong>%i<br>\n", ifdr->numofrec );
  fprintf(fp, "<strong>Record Length: </strong>%i<br>\n", ifdr->reclen );
  fprintf(fp, "<strong>Bits per sample: </strong>%i<br>\n", ifdr->bitssamp );
  fprintf(fp, "<strong>Samples per data group: </strong>%i<br>\n", ifdr->sampdata );
  fprintf(fp, "<strong>Bytes per group: </strong>%i<br>\n", ifdr->bytgroup );
  fprintf(fp, "<strong>Justification & order of samples: </strong>%s<br>\n", ifdr->justific );
  fprintf(fp, "<strong>SAR channels: </strong>%i<br>\n", ifdr->sarchan );
  fprintf(fp, "<strong>Lines per data set: </strong>%i<br>\n", ifdr->linedata );
  fprintf(fp, "<strong>Left border pixels per line: </strong>%i<br>\n", ifdr->lbrdrpxl );
  fprintf(fp, "<strong>Total number of data groups: </strong>%i<br>\n", ifdr->datgroup );
  fprintf(fp, "<strong>Right border pixels per line: </strong>%i<br>\n", ifdr->rbrdrpxl );
  fprintf(fp, "<strong>Top border lines: </strong>%i<br>\n", ifdr->topbrdr );
  fprintf(fp, "<strong>Bottom border lines: </strong>%i<br>\n", ifdr->botbrdr );
  fprintf(fp, "<strong>Interleave indicator: </strong>%s<br>\n", ifdr->interlv );
  fprintf(fp, "<strong>Physical records per line: </strong>%i<br>\n", ifdr->recline );
  fprintf(fp, "<strong>Physical records per multi-line channel: </strong>%i<br>\n", ifdr->mrecline );
  fprintf(fp, "<strong>Bytes of prefix data per record: </strong>%i<br>\n", ifdr->predata );
  fprintf(fp, "<strong>Bytes of SAR data per record: </strong>%i<br>\n", ifdr->sardata );
  fprintf(fp, "<strong>Bytes of suffix data per record: </strong>%i<br>\n", ifdr->sufdata );
  fprintf(fp, "<strong>Suffix/Prefix repeat flag: </strong>%s<br>\n", ifdr->repflag );
  fprintf(fp, "<strong>SAR data format identifier: </strong>%s<br>\n", ifdr->formatid );
  fprintf(fp, "<strong>SAR data format code: </strong>%s<br>\n", ifdr->formcode );
  fprintf(fp, "<strong>Left fill bits per pixel: </strong>%i<br>\n", ifdr->leftfill );
  fprintf(fp, "<strong>Right fill bits per pixel: </strong>%i<br>\n", ifdr->rigtfill );
  fprintf(fp, "<strong>Maximum Data Value: </strong>%i<br>\n", ifdr->maxidata );
  fprintf(fp, "</body>\n</html>\n");
  FCLOSE(fp);

  return;
}

