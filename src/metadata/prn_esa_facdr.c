#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_esa_facdr(struct ESA_FACDR *f)
{
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  add(&ret, "\n********* begin of Facility related data (ESA) record ********\n");
  add(&ret, "\n********* end of Facility related data (ESA)  record *********\n");
  return ret;
}

void prn_esa_facdr(FILE *fp, struct ESA_FACDR *f)
{
    char *rec = sprn_esa_facdr(f);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
