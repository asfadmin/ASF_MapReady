#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_rcdr(struct radio_comp_data_rec *rc)
{
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  add(&ret, "\n********* begin of Radiometric compensation data record*******\n");
  add(&ret, "\n*********** end of Radiometric compensation data record ******\n");
  return ret;
}

void prn_rcdr(FILE *fp, struct radio_comp_data_rec *rc)
{
    char *rec = sprn_rcdr(rc);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
