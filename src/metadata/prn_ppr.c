#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_ppr(struct PPREC *p)
{
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  add(&ret, "\n*********** begin of record *******************\n");
  add(&ret, "\n*********** end of record ********************\n");
  return ret;
}

void prn_ppr(FILE *fp, struct PPREC *p)
{
    char *rec = sprn_ppr(p);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
