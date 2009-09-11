#include "asf_vector.h"
#include "shapefil.h"
#include "asf_nan.h"
#include <assert.h>
#include <errno.h>
#include <ctype.h>

typedef struct {
  char sensor[64];
  char scnid[64];
  char dlpathno[64];
  char usesegno[64];
  char recmode[64];
  char recpath[64];
  char gscd[64];
  char opemd[64];
  int tblno;
  char rev[64];
  int pathno;
  int cenflmno;
  char proofflg[64];
  char steering[64];
  char orbitstat[64];
  int grs_lineno;
  int obs_orbitno;
  char obtdir[64];
  int sunele;
  int sunazi;
  char scn_sdate[64];
  char scn_stime[64];
  char scn_cdate[64];
  char scn_ctime[64];
  double scn_clat;
  double scn_clon;
  double scn_lulat;
  double scn_lulon;
  double scn_rulat;
  double scn_rulon;
  double scn_ldlat;
  double scn_ldlon;
  double scn_rdlat;
  double scn_rdlon;
  char pointflg[64];
  char ucut[64];
  char fcut[64];
  char rcut[64];
  char ugain[64];
  char fgain[64];
  char rgain[64];
  char pntang[64];
  char gainsts[64];
  char epssts[64];
  char rec_cldscene[64];
  char rec_clddevscene[64];
  char rec_allqlty[64];
  char rec_linelos[64];
  char dlsegno[64];
  char vldsdate[64];
  char trnsdate[64];
  char obssdate[64];
  char obsedate[64];
  char satcd[64];
  char vldsdead_date[64];
  char vldedead_date[64];
  int datarate;
  char filename[64];
  char l0status[64];
  char archmode[64];
  char urgflg[64];
  char semireal[64];
  int grs_colno;
  double scn_posx;
  double scn_posy;
  double scn_posz;
  double scn_spdx;
  double scn_spdy;
  double scn_spdz;
  char rotcorevise[64];
  char rec_cldversion[64];
  char rec_pixcel[64];
  char rec_linenum[64];
  char prc_bit_pixcel[64];
  char prc_pixcelsetstat[64];
  char rec_gps_dms[64];
  char rec_orbitnum[64];
  char rec_pathdate[64];
  char rec_pathnum[64];
  char rec_vldsdate[64];
  char rec_vldedate[64];
  char rec_satcntperiod[64];
  char rec_basesattime[64];
  char rec_basegrddate[64];
  char rec_utc_gps[64];
  char brs_filename[64];
  char brs_filesize[64];
  char brs_filedate[64];
  char tnl_filename[64];
  char tnl_filesize[64];
  char tnl_filedate[64];
  char l0_ex[64];
  char chk_flg[64];
  char opestat[64];
  char rec_rband[64];
  char rec_gband[64];
  char rec_bband[64];
  double offnadir;
} auig_type_t;

void auig_init(auig_type_t *auig)
{
  strcpy(auig->sensor, MAGIC_UNSET_STRING);
  strcpy(auig->scnid, MAGIC_UNSET_STRING);
  strcpy(auig->dlpathno, MAGIC_UNSET_STRING);
  strcpy(auig->usesegno, MAGIC_UNSET_STRING);
  strcpy(auig->recmode, MAGIC_UNSET_STRING);
  strcpy(auig->recpath, MAGIC_UNSET_STRING);
  strcpy(auig->gscd, MAGIC_UNSET_STRING);
  strcpy(auig->opemd, MAGIC_UNSET_STRING);
  auig->tblno = MAGIC_UNSET_INT;
  strcpy(auig->rev, MAGIC_UNSET_STRING);
  auig->pathno = MAGIC_UNSET_INT;
  auig->cenflmno = MAGIC_UNSET_INT;
  strcpy(auig->proofflg, MAGIC_UNSET_STRING);
  strcpy(auig->steering, MAGIC_UNSET_STRING);
  strcpy(auig->orbitstat, MAGIC_UNSET_STRING);
  auig->grs_lineno = MAGIC_UNSET_INT;
  auig->obs_orbitno = MAGIC_UNSET_INT;
  strcpy(auig->obtdir, MAGIC_UNSET_STRING);
  auig->sunele = MAGIC_UNSET_INT;
  auig->sunazi = MAGIC_UNSET_INT;
  strcpy(auig->scn_sdate, MAGIC_UNSET_STRING);
  strcpy(auig->scn_stime, MAGIC_UNSET_STRING);
  strcpy(auig->scn_cdate, MAGIC_UNSET_STRING);
  strcpy(auig->scn_ctime, MAGIC_UNSET_STRING);
  auig->scn_clat = MAGIC_UNSET_DOUBLE;
  auig->scn_clon = MAGIC_UNSET_DOUBLE;
  auig->scn_lulat = MAGIC_UNSET_DOUBLE;
  auig->scn_lulon = MAGIC_UNSET_DOUBLE;
  auig->scn_rulat = MAGIC_UNSET_DOUBLE;
  auig->scn_rulon = MAGIC_UNSET_DOUBLE;
  auig->scn_ldlat = MAGIC_UNSET_DOUBLE;
  auig->scn_ldlon = MAGIC_UNSET_DOUBLE;
  auig->scn_rdlat = MAGIC_UNSET_DOUBLE;
  auig->scn_rdlon = MAGIC_UNSET_DOUBLE;
  strcpy(auig->pointflg, MAGIC_UNSET_STRING);
  strcpy(auig->ucut, MAGIC_UNSET_STRING);
  strcpy(auig->fcut, MAGIC_UNSET_STRING);
  strcpy(auig->rcut, MAGIC_UNSET_STRING);
  strcpy(auig->ugain, MAGIC_UNSET_STRING);
  strcpy(auig->fgain, MAGIC_UNSET_STRING);
  strcpy(auig->rgain, MAGIC_UNSET_STRING);
  strcpy(auig->pntang, MAGIC_UNSET_STRING);
  strcpy(auig->gainsts, MAGIC_UNSET_STRING);
  strcpy(auig->epssts, MAGIC_UNSET_STRING);
  strcpy(auig->rec_cldscene, MAGIC_UNSET_STRING);
  strcpy(auig->rec_clddevscene, MAGIC_UNSET_STRING);
  strcpy(auig->rec_allqlty, MAGIC_UNSET_STRING);
  strcpy(auig->rec_linelos, MAGIC_UNSET_STRING);
  strcpy(auig->dlsegno, MAGIC_UNSET_STRING);
  strcpy(auig->vldsdate, MAGIC_UNSET_STRING);
  strcpy(auig->trnsdate, MAGIC_UNSET_STRING);
  strcpy(auig->obssdate, MAGIC_UNSET_STRING);
  strcpy(auig->obsedate, MAGIC_UNSET_STRING);
  strcpy(auig->satcd, MAGIC_UNSET_STRING);
  strcpy(auig->vldsdead_date, MAGIC_UNSET_STRING);
  strcpy(auig->vldedead_date, MAGIC_UNSET_STRING);
  auig->datarate = MAGIC_UNSET_INT;
  strcpy(auig->filename, MAGIC_UNSET_STRING);
  strcpy(auig->l0status, MAGIC_UNSET_STRING);
  strcpy(auig->archmode, MAGIC_UNSET_STRING);
  strcpy(auig->urgflg, MAGIC_UNSET_STRING);
  strcpy(auig->semireal, MAGIC_UNSET_STRING);
  auig->grs_colno = MAGIC_UNSET_INT;
  auig->scn_posx = MAGIC_UNSET_DOUBLE;
  auig->scn_posy = MAGIC_UNSET_DOUBLE;
  auig->scn_posz = MAGIC_UNSET_DOUBLE;
  auig->scn_spdx = MAGIC_UNSET_DOUBLE;
  auig->scn_spdy = MAGIC_UNSET_DOUBLE;
  auig->scn_spdz = MAGIC_UNSET_DOUBLE;
  strcpy(auig->rotcorevise, MAGIC_UNSET_STRING);
  strcpy(auig->rec_cldversion, MAGIC_UNSET_STRING);
  strcpy(auig->rec_pixcel, MAGIC_UNSET_STRING);
  strcpy(auig->rec_linenum, MAGIC_UNSET_STRING);
  strcpy(auig->prc_bit_pixcel, MAGIC_UNSET_STRING);
  strcpy(auig->prc_pixcelsetstat, MAGIC_UNSET_STRING);
  strcpy(auig->rec_gps_dms, MAGIC_UNSET_STRING);
  strcpy(auig->rec_orbitnum, MAGIC_UNSET_STRING);
  strcpy(auig->rec_pathdate, MAGIC_UNSET_STRING);
  strcpy(auig->rec_pathnum, MAGIC_UNSET_STRING);
  strcpy(auig->rec_vldsdate, MAGIC_UNSET_STRING);
  strcpy(auig->rec_vldedate, MAGIC_UNSET_STRING);
  strcpy(auig->rec_satcntperiod, MAGIC_UNSET_STRING);
  strcpy(auig->rec_basesattime, MAGIC_UNSET_STRING);
  strcpy(auig->rec_basegrddate, MAGIC_UNSET_STRING);
  strcpy(auig->rec_utc_gps, MAGIC_UNSET_STRING);
  strcpy(auig->brs_filename, MAGIC_UNSET_STRING);
  strcpy(auig->brs_filesize, MAGIC_UNSET_STRING);
  strcpy(auig->brs_filedate, MAGIC_UNSET_STRING);
  strcpy(auig->tnl_filename, MAGIC_UNSET_STRING);
  strcpy(auig->tnl_filesize, MAGIC_UNSET_STRING);
  strcpy(auig->tnl_filedate, MAGIC_UNSET_STRING);
  strcpy(auig->l0_ex, MAGIC_UNSET_STRING);
  strcpy(auig->chk_flg, MAGIC_UNSET_STRING);
  strcpy(auig->opestat, MAGIC_UNSET_STRING);
  strcpy(auig->rec_rband, MAGIC_UNSET_STRING);
  strcpy(auig->rec_gband, MAGIC_UNSET_STRING);
  strcpy(auig->rec_bband, MAGIC_UNSET_STRING);
  auig->offnadir = MAGIC_UNSET_DOUBLE;
}

static void strip_end_whitesp(char *s)
{
    char *p = s + strlen(s) - 1;
    while (isspace(*p) && p>s)
        *p-- = '\0';
}

// output of double values is left aligned and leaves plenty white space
// as times - get rid of it
char *lf(double value)
{
    char *value_str = (char *) MALLOC(sizeof(char)*20);
    if (meta_is_valid_double(value)) {
        sprintf(value_str, "%-16.11g", value);
        strip_end_whitesp(value_str);
    }
    else {
        // For NaN, just leave the value blank 
        strcpy(value_str, "");
    }
    return value_str;
}

static void add_to_kml(FILE *fp, auig_type_t *auig, dbf_header_t *dbf,
		       int nCols)
{
  int ii;
  char begin[10], end[10];

  // Print out according to configuration
  fprintf(fp, "<Placemark>\n");
  fprintf(fp, "  <description><![CDATA[\n");
  fprintf(fp, "<table width=\"350\"><tr><td>\n");
  fprintf(fp, "<!-- Format: AUIG (generated by convert2vector "
          "(version %s)) -->\n", SVN_REV);
  for (ii=0; ii<nCols; ii++) {
    if (dbf[ii].visible == 0) {
      strcpy(begin, "<!--");
      strcpy(end, "-->\n");
    }
    else {
      strcpy(begin, "");
      strcpy(end, "\n");
    }
    if (strcmp(dbf[ii].header, "SENSOR") == 0)
      fprintf(fp, "%s<strong>Sensor</strong>: %s <br>%s", 
	      begin, auig->sensor, end);
    else if (strcmp(dbf[ii].header, "SCNID") == 0)
      fprintf(fp, "%s<strong>Scene ID</strong>: %s <br>%s", 
	      begin, auig->scnid, end);
    else if (strcmp(dbf[ii].header, "DLPATHNO") == 0)
      fprintf(fp, "%s<strong>DL path number</strong>: %s <br>%s",
	      begin, auig->dlpathno, end);
    else if (strcmp(dbf[ii].header, "USESEGNO") == 0)
      fprintf(fp, "%s<strong>Used segment number</strong>: %s <br>%s",
	      begin, auig->usesegno, end);
    else if (strcmp(dbf[ii].header, "RECMODE") == 0)
      fprintf(fp, "%s<strong>Recording mode</strong>: %s <br>%s",
	      begin, auig->recmode, end);
    else if (strcmp(dbf[ii].header, "RECPATH") == 0)
      fprintf(fp, "%s<strong>Recording path</strong>: %s <br>%s",
	      begin, auig->recpath, end);
    else if (strcmp(dbf[ii].header, "GSCD") == 0)
      fprintf(fp, "%s<strong>GSCD</strong>: %s <br>%s",
	      begin, auig->gscd, end);
    else if (strcmp(dbf[ii].header, "OPEMD") == 0)
      fprintf(fp, "%s<strong>OPEMD</strong>: %s <br>%s",
	      begin, auig->opemd, end);
    else if (strcmp(dbf[ii].header, "TBLNO") == 0)
      fprintf(fp, "%s<strong>Table number</strong>: %d <br>%s",
	      begin, auig->tblno, end);
    else if (strcmp(dbf[ii].header, "REV") == 0)
      fprintf(fp, "%s<strong>Revolution</strong>: %s <br>%s",
	      begin, auig->rev, end);
    else if (strcmp(dbf[ii].header, "PATHNO") == 0)
      fprintf(fp, "%s<strong>Path number</strong>: %d <br>%s",
	      begin, auig->pathno, end);
    else if (strcmp(dbf[ii].header, "CENFLMNO") == 0)
      fprintf(fp, "%s<strong>CENFLMNO</strong>: %d <br>%s",
	      begin, auig->cenflmno, end);
    else if (strcmp(dbf[ii].header, "PROOFFLG") == 0)
      fprintf(fp, "%s<strong>Proof flag</strong>: %s <br>%s",
	      begin, auig->proofflg, end);
    else if (strcmp(dbf[ii].header, "STEERING") == 0)
      fprintf(fp, "%s<strong>Steering</strong>: %s <br>%s",
	      begin, auig->steering, end);
    else if (strcmp(dbf[ii].header, "ORBITSTAT") == 0)
      fprintf(fp, "%s<strong>Orbit status</strong>: %s <br>%s",
	      begin, auig->orbitstat, end);
    else if (strcmp(dbf[ii].header, "GRS_LINENO") == 0)
      fprintf(fp, "%s<strong>GRS line number</strong>: %d <br>%s",
	      begin, auig->grs_lineno, end);
    else if (strcmp(dbf[ii].header, "OBS_ORBITNO") == 0)
      fprintf(fp, "%s<strong>OBS orbit number</strong>: %d <br>%s",
	      begin, auig->obs_orbitno, end);
    else if (strcmp(dbf[ii].header, "OBTDIR") == 0)
      fprintf(fp, "%s<strong>Orbit direction</strong>: %s <br>%s",
	      begin, auig->obtdir, end);
    else if (strcmp(dbf[ii].header, "SUN_SUNELE") == 0)
      fprintf(fp, "%s<strong>Sun elevation angle</strong>: %d <br>%s",
	      begin, auig->sunele, end);
    else if (strcmp(dbf[ii].header, "SUN_SUNAZI") == 0)
      fprintf(fp, "%s<strong>Sun azimuth angle</strong>: %d <br>%s",
	      begin, auig->sunazi, end);
    else if (strcmp(dbf[ii].header, "SCN_SDATE") == 0)
      fprintf(fp, "%s<strong>Scene start date</strong>: %s <br>%s",
	      begin, auig->scn_sdate, end);
    else if (strcmp(dbf[ii].header, "SCN_STIME") == 0)
      fprintf(fp, "%s<strong>Scene start time</strong>: %s <br>%s",
	      begin, auig->scn_stime, end);
    else if (strcmp(dbf[ii].header, "SCN_CDATE") == 0)
      fprintf(fp, "%s<strong>Scene center date</strong>: %s <br>%s",
	      begin, auig->scn_cdate, end);
    else if (strcmp(dbf[ii].header, "SCN_CTIME") == 0)
      fprintf(fp, "%s<strong>Scene center time</strong>: %s <br>%s",
	      begin, auig->scn_ctime, end);
    else if (strcmp(dbf[ii].header, "SCN_CLAT") == 0)
      fprintf(fp, "%s<strong>Scene center latitude</strong>: %s <br>%s",
	      begin, lf(auig->scn_clat), end);
    else if (strcmp(dbf[ii].header, "SCN_CLON") == 0)
      fprintf(fp, "%s<strong>Scene center longitude</strong>: %s <br>%s",
	      begin, lf(auig->scn_clon), end);
    else if (strcmp(dbf[ii].header, "SCN_LULAT") == 0)
      fprintf(fp, "%s<strong>Scene left upper latitude</strong>: %s <br>%s", 
	      begin, lf(auig->scn_lulat), end);
    else if (strcmp(dbf[ii].header, "SCN_LULON") == 0)
      fprintf(fp, "%s<strong>Scene left upper longitude</strong>: %s <br>%s", 
	      begin, lf(auig->scn_lulon), end);
    else if (strcmp(dbf[ii].header, "SCN_RULAT") == 0)
      fprintf(fp, "%s<strong>Scene right upper latitude</strong>: %s <br>%s", 
	      begin, lf(auig->scn_rulat), end);
    else if (strcmp(dbf[ii].header, "SCN_RULON") == 0)
      fprintf(fp, "%s<strong>Scene right upper longitude</strong>: %s <br>%s", 
	      begin, lf(auig->scn_rulon), end);
    else if (strcmp(dbf[ii].header, "SCN_LDLAT") == 0)
      fprintf(fp, "%s<strong>Scene left down latitude</strong>: %s <br>%s", 
	      begin, lf(auig->scn_ldlat), end);
    else if (strcmp(dbf[ii].header, "SCN_LDLON") == 0)
      fprintf(fp, "%s<strong>Scene left down longitude</strong>: %s <br>%s", 
	      begin, lf(auig->scn_ldlon), end);
    else if (strcmp(dbf[ii].header, "SCN_RDLAT") == 0)
      fprintf(fp, "%s<strong>Scene right down latitude</strong>: %s <br>%s", 
	      begin, lf(auig->scn_rdlat), end);
    else if (strcmp(dbf[ii].header, "SCN_RDLON") == 0)
      fprintf(fp, "%s<strong>Scene right down longitude</strong>: %s <br>%s", 
	      begin, lf(auig->scn_rdlon), end);
    else if (strcmp(dbf[ii].header, "POINTFLG") == 0)
      fprintf(fp, "%s<strong>Point flag</strong>: %s <br>%s",
	      begin, auig->pointflg, end);
    else if (strcmp(dbf[ii].header, "UCUT") == 0)
      fprintf(fp, "%s<strong>UCUT</strong>: %s <br>%s",
	      begin, auig->ucut, end);
    else if (strcmp(dbf[ii].header, "FCUT") == 0)
      fprintf(fp, "%s<strong>FCUT</strong>: %s <br>%s",
	      begin, auig->fcut, end);
    else if (strcmp(dbf[ii].header, "RCUT") == 0)
      fprintf(fp, "%s<strong>RCUT</strong>: %s <br>%s",
	      begin, auig->rcut, end);
    else if (strcmp(dbf[ii].header, "UGAIN") == 0)
      fprintf(fp, "%s<strong>UGAIN</strong>: %s <br>%s",
	      begin, auig->ugain, end);
    else if (strcmp(dbf[ii].header, "FGAIN") == 0)
      fprintf(fp, "%s<strong>FGAIN</strong>: %s <br>%s",
	      begin, auig->fgain, end);
    else if (strcmp(dbf[ii].header, "RGAIN") == 0)
      fprintf(fp, "%s<strong>RGAIN</strong>: %s <br>%s",
	      begin, auig->rgain, end);
    else if (strcmp(dbf[ii].header, "PNTANG") == 0)
      fprintf(fp, "%s<strong>Pointing angle</strong>: %s <br>%s",
	      begin, auig->pntang, end);
    else if (strcmp(dbf[ii].header, "GAINSTS") == 0)
      fprintf(fp, "%s<strong>GAINSTS</strong>: %s <br>%s",
	      begin, auig->gainsts, end);
    else if (strcmp(dbf[ii].header, "EPSSTS") == 0)
      fprintf(fp, "%s<strong>EPSSTS</strong>: %s <br>%s",
	      begin, auig->epssts, end);
    else if (strcmp(dbf[ii].header, "REC_CLDSCENE") == 0)
      fprintf(fp, "%s<strong>REC_CLDSCENE</strong>: %s <br>%s",
	      begin, auig->rec_cldscene, end);
    else if (strcmp(dbf[ii].header, "REC_CLDDEVSCENE") == 0)
      fprintf(fp, "%s<strong>REC_CLDDEVSCENE</strong>: %s <br>%s",
	      begin, auig->rec_clddevscene, end);
    else if (strcmp(dbf[ii].header, "REC_ALLQLTY") == 0)
      fprintf(fp, "%s<strong>REC_ALLQLTY</strong>: %s <br>%s",
	      begin, auig->rec_allqlty, end);
    else if (strcmp(dbf[ii].header, "REC_LINELOS") == 0)
      fprintf(fp, "%s<strong>REC_LINELOS</strong>: %s <br>%s",
	      begin, auig->rec_linelos, end);
    else if (strcmp(dbf[ii].header, "DLSEGNO") == 0)
      fprintf(fp, "%s<strong>DL segment number</strong>: %s <br>%s",
	      begin, auig->dlsegno, end);
    else if (strcmp(dbf[ii].header, "VLDSDATE") == 0)
      fprintf(fp, "%s<strong>VLDS date</strong>: %s <br>%s",
	      begin, auig->vldsdate, end);
    else if (strcmp(dbf[ii].header, "TRNSDATE") == 0)
      fprintf(fp, "%s<strong>TRNS date</strong>: %s <br>%s",
	      begin, auig->trnsdate, end);
    else if (strcmp(dbf[ii].header, "OBSSDATE") == 0)
      fprintf(fp, "%s<strong>OBSS date</strong>: %s <br>%s",
	      begin, auig->obssdate, end);
    else if (strcmp(dbf[ii].header, "OBSEDATE") == 0)
      fprintf(fp, "%s<strong>OBSE date</strong>: %s <br>%s",
	      begin, auig->obsedate, end);
    else if (strcmp(dbf[ii].header, "SATCD") == 0)
      fprintf(fp, "%s<strong>SATCD</strong>: %s <br>%s",
	      begin, auig->satcd, end);
    else if (strcmp(dbf[ii].header, "VLDSDEAD_DATE") == 0)
      fprintf(fp, "%s<strong>VLDSDEAD date</strong>: %s <br>%s",
	      begin, auig->vldsdead_date, end);
    else if (strcmp(dbf[ii].header, "VLDEDEAD_DATE") == 0)
      fprintf(fp, "%s<strong>VLDEDEAD date</strong>: %s <br>%s",
	      begin, auig->vldedead_date, end);
    else if (strcmp(dbf[ii].header, "DATARATE") == 0)
      fprintf(fp, "%s<strong>Data rate</strong>: %d <br>%s",
	      begin, auig->datarate, end);
    else if (strcmp(dbf[ii].header, "FILENAME") == 0)
      fprintf(fp, "%s<strong>File name</strong>: %s <br>%s",
	      begin, auig->filename, end);
    else if (strcmp(dbf[ii].header, "L0STATUS") == 0)
      fprintf(fp, "%s<strong>L0 status</strong>: %s <br>%s",
	      begin, auig->l0status, end);
    else if (strcmp(dbf[ii].header, "ARCHMODE") == 0)
      fprintf(fp, "%s<strong>Archive mode</strong>: %s <br>%s",
	      begin, auig->archmode, end);
    else if (strcmp(dbf[ii].header, "URGFLG") == 0)
      fprintf(fp, "%s<strong>Urgent flag</strong>: %s <br>%s",
	      begin, auig->urgflg, end);
    else if (strcmp(dbf[ii].header, "SEMIREAL") == 0)
      fprintf(fp, "%s<strong>Semi real</strong>: %s <br>%s",
	      begin, auig->semireal, end);
    else if (strcmp(dbf[ii].header, "GRS_COLNO") == 0)
      fprintf(fp, "%s<strong>GRS column number</strong>: %d <br>%s",
	      begin, auig->grs_colno, end);
    else if (strcmp(dbf[ii].header, "SCN_POSX") == 0)
      fprintf(fp, "%s<strong>Scene position x</strong>: %s <br>%s",
	      begin, lf(auig->scn_posx), end);
    else if (strcmp(dbf[ii].header, "SCN_POSY") == 0)
      fprintf(fp, "%s<strong>Scene position y</strong>: %s <br>%s",
	      begin, lf(auig->scn_posy), end);
    else if (strcmp(dbf[ii].header, "SCN_POSZ") == 0)
      fprintf(fp, "%s<strong>Scene position z</strong>: %s <br>%s",
	      begin, lf(auig->scn_posz), end);
    else if (strcmp(dbf[ii].header, "SCN_SPDX") == 0)
      fprintf(fp, "%s<strong>Scene SPDX</strong>: %s <br>%s",
	      begin, lf(auig->scn_spdx), end);
    else if (strcmp(dbf[ii].header, "SCN_SPDY") == 0)
      fprintf(fp, "%s<strong>Scene SPDY</strong>: %s <br>%s",
	      begin, lf(auig->scn_spdx), end);
    else if (strcmp(dbf[ii].header, "SCN_SPDZ") == 0)
      fprintf(fp, "%s<strong>Scene SPFZ</strong>: %s <br>%s",
	      begin, lf(auig->scn_spdz), end);
    else if (strcmp(dbf[ii].header, "ROTCOREVISE") == 0)
      fprintf(fp, "%s<strong>ROTCOREVISE</strong>: %s <br>%s",
	      begin, auig->rotcorevise, end);
    else if (strcmp(dbf[ii].header, "REC_CLDVERSION") == 0)
      fprintf(fp, "%s<strong>REC_CLD version</strong>: %s <br>%s",
	      begin, auig->rec_cldversion, end);
    else if (strcmp(dbf[ii].header, "REC_PIXCEL") == 0)
      fprintf(fp, "%s<strong>REC_PIXCEL</strong>: %s <br>%s",
	      begin, auig->rec_pixcel, end);
    else if (strcmp(dbf[ii].header, "REC_LINENUM") == 0)
      fprintf(fp, "%s<strong>REC line number</strong>: %s <br>%s",
	      begin, auig->rec_linenum, end);
    else if (strcmp(dbf[ii].header, "PRC_BIT_PIXCEL") == 0)
      fprintf(fp, "%s<strong>PRC_BIT_PIXCEL</strong>: %s <br>%s",
	      begin, auig->prc_bit_pixcel, end);
    else if (strcmp(dbf[ii].header, "PRC_PIXCELSETSTAT") == 0)
      fprintf(fp, "%s<strong>PRC_PIXCELSETSTAT</strong>: %s <br>%s",
	      begin, auig->prc_pixcelsetstat, end);
    else if (strcmp(dbf[ii].header, "REC_GPS_DMS") == 0)
      fprintf(fp, "%s<strong>REC GPS DMS</strong>: %s <br>%s",
	      begin, auig->rec_gps_dms, end);
    else if (strcmp(dbf[ii].header, "REC_ORBITNUM") == 0)
      fprintf(fp, "%s<strong>REC orbit number</strong>: %s <br>%s",
	      begin, auig->rec_orbitnum, end);
    else if (strcmp(dbf[ii].header, "REC_PATHDATE") == 0)
      fprintf(fp, "%s<strong>REC path date</strong>: %s <br>%s",
	      begin, auig->rec_pathdate, end);
    else if (strcmp(dbf[ii].header, "REC_PATHNUM") == 0)
      fprintf(fp, "%s<strong>REC path number</strong>: %s <br>%s",
	      begin, auig->rec_pathnum, end);
    else if (strcmp(dbf[ii].header, "REC_VLDSDATE") == 0)
      fprintf(fp, "%s<strong>REC VLDS date </strong>: %s <br>%s",
	      begin, auig->rec_vldsdate, end);
    else if (strcmp(dbf[ii].header, "REC_VLDEDATE") == 0)
      fprintf(fp, "%s<strong>REC VLDE date</strong>: %s <br>%s",
	      begin, auig->rec_vldedate, end);
    else if (strcmp(dbf[ii].header, "REC_SATCNTPERIOD") == 0)
      fprintf(fp, "%s<strong>REC SATCNT period</strong>: %s <br>%s",
	      begin, auig->rec_satcntperiod, end);
    else if (strcmp(dbf[ii].header, "REC_BASESATTIME") == 0)
      fprintf(fp, "%s<strong>REC BASESAT time</strong>: %s <br>%s",
	      begin, auig->rec_basesattime, end);
    else if (strcmp(dbf[ii].header, "REC_BASEGRDDATE") == 0)
      fprintf(fp, "%s<strong>REC BASEGRD date</strong>: %s <br>%s",
	      begin, auig->rec_basegrddate, end);
    else if (strcmp(dbf[ii].header, "REC_UTC_GPS") == 0)
      fprintf(fp, "%s<strong>REC UTC GPS</strong>: %s <br>%s",
	      begin, auig->rec_utc_gps, end);
    else if (strcmp(dbf[ii].header, "BRS_FILENAME") == 0)
      fprintf(fp, "%s<strong>Browse file name</strong>: %s <br>%s",
	      begin, auig->brs_filename, end);
    else if (strcmp(dbf[ii].header, "BRS_FILESIZE") == 0)
      fprintf(fp, "%s<strong>Browse file size</strong>: %s <br>%s",
	      begin, auig->brs_filesize, end);
    else if (strcmp(dbf[ii].header, "BRS_FILEDATE") == 0)
      fprintf(fp, "%s<strong>Browse file date</strong>: %s <br>%s",
	      begin, auig->brs_filedate, end);
    else if (strcmp(dbf[ii].header, "TNL_FILENAME") == 0)
      fprintf(fp, "%s<strong>TNL file name</strong>: %s <br>%s",
	      begin, auig->tnl_filename, end);
    else if (strcmp(dbf[ii].header, "TNL_FILESIZE") == 0)
      fprintf(fp, "%s<strong>TNL file size</strong>: %s <br>%s",
	      begin, auig->tnl_filesize, end);
    else if (strcmp(dbf[ii].header, "TNL_FILEDATE") == 0)
      fprintf(fp, "%s<strong>TNL file date</strong>: %s <br>%s",
	      begin, auig->tnl_filedate, end);
    else if (strcmp(dbf[ii].header, "L0_EX") == 0)
      fprintf(fp, "%s<strong>L0_EX</strong>: %s <br>%s",
	      begin, auig->l0_ex, end);
    else if (strcmp(dbf[ii].header, "CHK_FLG") == 0)
      fprintf(fp, "%s<strong>CHK flag</strong>: %s <br>%s",
	      begin, auig->chk_flg, end);
    else if (strcmp(dbf[ii].header, "OPESTAT") == 0)
      fprintf(fp, "%s<strong>OPE status</strong>: %s <br>%s",
	      begin, auig->opestat, end);
    else if (strcmp(dbf[ii].header, "REC RBAND") == 0)
      fprintf(fp, "%s<strong>REC red band</strong>: %s <br>%s",
	      begin, auig->rec_rband, end);
    else if (strcmp(dbf[ii].header, "REC_GBAND") == 0)
      fprintf(fp, "%s<strong>REC green band</strong>: %s <br>%s",
	      begin, auig->rec_gband, end);
    else if (strcmp(dbf[ii].header, "REC_BBAND") == 0)
      fprintf(fp, "%s<strong>REC blue band</strong>: %s <br>%s",
	      begin, auig->rec_bband, end);
    else if (strcmp(dbf[ii].header, "OFFNADIR") == 0)
      fprintf(fp, "%s<strong>Off nadir angle</strong>: %s <br>%s",
	      begin, lf(auig->offnadir), end);
  }
  fprintf(fp, "</td></tr></table>\n");
  fprintf(fp, "  ]]></description>\n");
  fprintf(fp, "  <name>%s</name>\n", auig->scnid);
  fprintf(fp, "  <LookAt>\n");
  fprintf(fp, "    <longitude>%.10f</longitude>\n", auig->scn_clon);
  fprintf(fp, "    <latitude>%.10f</latitude>\n", auig->scn_clat);
  fprintf(fp, "    <range>400000</range>\n");
  //fprintf(fp, "    <tilt>30</tilt>\n");
  fprintf(fp, "  </LookAt>\n");
  fprintf(fp, "  <visibility>1</visibility>\n");
  fprintf(fp, "  <open>1</open>\n");

  write_kml_style_keys(fp);

  fprintf(fp, "  <Polygon>\n");
  fprintf(fp, "    <extrude>1</extrude>\n");
  fprintf(fp, "    <altitudeMode>%s</altitudeMode>\n", altitude_mode());
  fprintf(fp, "    <outerBoundaryIs>\n");
  fprintf(fp, "     <LinearRing>\n");
  fprintf(fp, "      <coordinates>\n");
  fprintf(fp, "       %.12f,%.12f,7000\n", auig->scn_lulon, auig->scn_lulat);
  fprintf(fp, "       %.12f,%.12f,7000\n", auig->scn_rulon, auig->scn_rulat);
  fprintf(fp, "       %.12f,%.12f,7000\n", auig->scn_rdlon, auig->scn_rdlat);
  fprintf(fp, "       %.12f,%.12f,7000\n", auig->scn_ldlon, auig->scn_ldlat);
  fprintf(fp, "       %.12f,%.12f,7000\n", auig->scn_lulon, auig->scn_lulat);
  fprintf(fp, "      </coordinates>\n");
  fprintf(fp, "     </LinearRing>\n");
  fprintf(fp, "    </outerBoundaryIs>\n");
  fprintf(fp, "  </Polygon>\n");
  fprintf(fp, "</Placemark>\n");
}

static int read_auig_line(char *header, int n,char *line, auig_type_t *auig)
{
  int ii, ok;
  char *test = (char *) MALLOC(sizeof(char)*256);
  for (ii=0; ii<n; ii++) {
    strcpy(test, get_str(header, ii));
    if (strcmp(test, "SENSOR") == 0)
      strcpy(auig->sensor, get_str(line, ii));
    else if (strcmp(test, "SCNID") == 0)
      strcpy(auig->scnid, get_str(line, ii));
    else if (strcmp(test, "DLPATHNO") == 0)
      strcpy(auig->dlpathno, get_str(line, ii));
    else if (strcmp(test, "USESEGNO") == 0)
      strcpy(auig->usesegno, get_str(line, ii));
    else if (strcmp(test, "RECMODE") == 0)
      strcpy(auig->recmode, get_str(line, ii));
    else if (strcmp(test, "RECPATH") == 0)
      strcpy(auig->recpath, get_str(line, ii));
    else if (strcmp(test, "GSCD") == 0)
      strcpy(auig->gscd, get_str(line, ii));
    else if (strcmp(test, "OPEMD") == 0)
      strcpy(auig->opemd, get_str(line, ii));
    else if (strcmp(test, "TBLNO") == 0)
      auig->tblno = get_int(line, ii);
    else if (strcmp(test, "REV") == 0)
      strcpy(auig->rev, get_str(line, ii));
    else if (strcmp(test, "PATHNO") == 0)
      auig->pathno = get_int(line, ii);
    else if (strcmp(test, "CENFLMNO") == 0)
      auig->cenflmno = get_int(line, ii);
    else if (strcmp(test, "PROOFFLG") == 0)
      strcpy(auig->proofflg, get_str(line, ii));
    else if (strcmp(test, "STEERING") == 0)
      strcpy(auig->steering, get_str(line, ii));
    else if (strcmp(test, "ORBITSTAT") == 0)
      strcpy(auig->orbitstat, get_str(line, ii));
    else if (strcmp(test, "GRS_LINENO") == 0)
      auig->grs_lineno = get_int(line, ii);
    else if (strcmp(test, "OBS_ORBITNO") == 0)
      auig->obs_orbitno = get_int(line, ii);
    else if (strcmp(test, "OBTDIR") == 0)
      strcpy(auig->obtdir, get_str(line, ii));
    else if (strcmp(test, "SUN_SUNELE") == 0)
      auig->sunele = get_int(line, ii);
    else if (strcmp(test, "SUN_SUNAZI") == 0)
      auig->sunazi = get_int(line, ii);
    else if (strcmp(test, "SCN_SDATE") == 0)
      strcpy(auig->scn_sdate, get_str(line, ii));
    else if (strcmp(test, "SCN_STIME") == 0)
      strcpy(auig->scn_stime, get_str(line, ii));
    else if (strcmp(test, "SCN_CDATE") == 0)
      strcpy(auig->scn_cdate, get_str(line, ii));
    else if (strcmp(test, "SCN_CTIME") == 0)
      strcpy(auig->scn_ctime, get_str(line, ii));
    else if (strcmp(test, "SCN_CLAT") == 0)
      auig->scn_clat = get_double(line, ii);
    else if (strcmp(test, "SCN_CLON") == 0)
      auig->scn_clon = get_double(line, ii);
    else if (strcmp(test, "SCN_LULAT") == 0)
      auig->scn_lulat = get_req_double(line, ii, &ok);
    else if (strcmp(test, "SCN_LULON") == 0)
      auig->scn_lulon = get_req_double(line, ii, &ok);
    else if (strcmp(test, "SCN_RULAT") == 0) {
      int kk = find_2nd_str(header, "SCN_RULAT");
      if (kk != ii)
	auig->scn_rulat = get_req_double(line, ii, &ok);
      else
	auig->scn_rdlat = get_req_double(line, kk, &ok);
    }
    else if (strcmp(test, "SCN_RULON") == 0)
      auig->scn_rulon = get_req_double(line, ii, &ok);
    else if (strcmp(test, "SCN_LDLAT") == 0)
      auig->scn_ldlat = get_req_double(line, ii, &ok);
    else if (strcmp(test, "SCN_LDLON") == 0)
      auig->scn_ldlon = get_req_double(line, ii, &ok);
    else if (strcmp(test, "SCN_RDLAT") == 0)
      auig->scn_rdlat = get_req_double(line, ii, &ok);
    else if (strcmp(test, "SCN_RDLON") == 0)
      auig->scn_rdlon = get_req_double(line, ii, &ok);
    else if (strcmp(test, "POINTFLG") == 0)
      strcpy(auig->pointflg, get_str(line, ii));
    else if (strcmp(test, "UCUT") == 0)
      strcpy(auig->ucut, get_str(line, ii));
    else if (strcmp(test, "FCUT") == 0)
      strcpy(auig->fcut, get_str(line, ii));
    else if (strcmp(test, "RCUT") == 0)
      strcpy(auig->rcut, get_str(line, ii));
    else if (strcmp(test, "UGAIN") == 0)
      strcpy(auig->ugain, get_str(line, ii));
    else if (strcmp(test, "FGAIN") == 0)
      strcpy(auig->fgain, get_str(line, ii));
    else if (strcmp(test, "RGAIN") == 0)
      strcpy(auig->rgain, get_str(line, ii));
    else if (strcmp(test, "PNTANG") == 0)
      strcpy(auig->pntang, get_str(line, ii));
    else if (strcmp(test, "GAINSTS") == 0)
      strcpy(auig->gainsts, get_str(line, ii));
    else if (strcmp(test, "EPSSTS") == 0)
      strcpy(auig->epssts, get_str(line, ii));
    else if (strcmp(test, "REC_CLDSCENE") == 0)
      strcpy(auig->rec_cldscene, get_str(line, ii));
    else if (strcmp(test, "REC_CLDDEVSCENE") == 0)
      strcpy(auig->rec_clddevscene, get_str(line, ii));
    else if (strcmp(test, "REC_ALLQLTY") == 0)
      strcpy(auig->rec_allqlty, get_str(line, ii));
    else if (strcmp(test, "REC_LINELOS") == 0)
      strcpy(auig->rec_linelos, get_str(line, ii));
    else if (strcmp(test, "DLSEGNO") == 0)
      strcpy(auig->dlsegno, get_str(line, ii));
    else if (strcmp(test, "VLDSDATE") == 0)
      strcpy(auig->vldsdate, get_str(line, ii));
    else if (strcmp(test, "TRNSDATE") == 0)
      strcpy(auig->trnsdate, get_str(line, ii));
    else if (strcmp(test, "OBSSDATE") == 0)
      strcpy(auig->obssdate, get_str(line, ii));
    else if (strcmp(test, "OBSEDATE") == 0)
      strcpy(auig->obsedate, get_str(line, ii));
    else if (strcmp(test, "SATCD") == 0)
      strcpy(auig->satcd, get_str(line, ii));
    else if (strcmp(test, "VLDSDEAD_DATE") == 0)
      strcpy(auig->vldsdead_date, get_str(line, ii));
    else if (strcmp(test, "VLDEDEAD_DATE") == 0)
      strcpy(auig->vldedead_date, get_str(line, ii));
    else if (strcmp(test, "DATARATE") == 0)
      auig->datarate = get_int(line, ii);
    else if (strcmp(test, "FILENAME") == 0)
      strcpy(auig->filename, get_str(line, ii));
    else if (strcmp(test, "L0STATUS") == 0)
      strcpy(auig->l0status, get_str(line, ii));
    else if (strcmp(test, "ARCHMODE") == 0)
      strcpy(auig->archmode, get_str(line, ii));
    else if (strcmp(test, "URGFLG") == 0)
      strcpy(auig->urgflg, get_str(line, ii));
    else if (strcmp(test, "SEMIREAL") == 0)
      strcpy(auig->semireal, get_str(line, ii));
    else if (strcmp(test, "GRS_COLNO") == 0)
      auig->grs_colno = get_int(line, ii);
    else if (strcmp(test, "SCN_POSX") == 0)
      auig->scn_posx = get_double(line, ii);
    else if (strcmp(test, "SCN_POSY") == 0)
      auig->scn_posy = get_double(line, ii);
    else if (strcmp(test, "SCN_POSZ") == 0)
      auig->scn_posz = get_double(line, ii);
    else if (strcmp(test, "SCN_SPDX") == 0)
      auig->scn_spdx = get_double(line, ii);
    else if (strcmp(test, "SCN_SPDY") == 0)
      auig->scn_spdy = get_double(line, ii);
    else if (strcmp(test, "SCN_SPDZ") == 0)
      auig->scn_spdz = get_double(line, ii);
    else if (strcmp(test, "ROTCOREVISE") == 0)
      strcpy(auig->rotcorevise, get_str(line, ii));
    else if (strcmp(test, "REC_CLDVERSION") == 0)
      strcpy(auig->rec_cldversion, get_str(line, ii));
    else if (strcmp(test, "REC_PIXCEL") == 0)
      strcpy(auig->rec_pixcel, get_str(line, ii));
    else if (strcmp(test, "REC_LINENUM") == 0)
      strcpy(auig->rec_linenum, get_str(line, ii));
    else if (strcmp(test, "PRC_BIT_PIXCEL") == 0)
      strcpy(auig->prc_bit_pixcel, get_str(line, ii));
    else if (strcmp(test, "PRC_PIXCELSETSTAT") == 0)
      strcpy(auig->prc_pixcelsetstat, get_str(line, ii));
    else if (strcmp(test, "REC_GPS_DMS") == 0)
      strcpy(auig->rec_gps_dms, get_str(line, ii));
    else if (strcmp(test, "REC_ORBITNUM") == 0)
      strcpy(auig->rec_orbitnum, get_str(line, ii));
    else if (strcmp(test, "REC_PATHDATE") == 0)
      strcpy(auig->rec_pathdate, get_str(line, ii));
    else if (strcmp(test, "REC_PATHNUM") == 0)
      strcpy(auig->rec_pathnum, get_str(line, ii));
    else if (strcmp(test, "REC_VLDSDATE") == 0)
      strcpy(auig->rec_vldsdate, get_str(line, ii));
    else if (strcmp(test, "REC_VLDEDATE") == 0)
      strcpy(auig->rec_vldedate, get_str(line, ii));
    else if (strcmp(test, "REC_SATCNTPERIOD") == 0)
      strcpy(auig->rec_satcntperiod, get_str(line, ii));
    else if (strcmp(test, "REC_BASESATTIME") == 0)
      strcpy(auig->rec_basesattime, get_str(line, ii));
    else if (strcmp(test, "REC_BASEGRDDATE") == 0)
      strcpy(auig->rec_basegrddate, get_str(line, ii));
    else if (strcmp(test, "REC_UTC_GPS") == 0)
      strcpy(auig->rec_utc_gps, get_str(line, ii));
    else if (strcmp(test, "BRS_FILENAME") == 0)
      strcpy(auig->brs_filename, get_str(line, ii));
    else if (strcmp(test, "BRS_FILESIZE") == 0)
      strcpy(auig->brs_filesize, get_str(line, ii));
    else if (strcmp(test, "BRS_FILEDATE") == 0)
      strcpy(auig->brs_filedate, get_str(line, ii));
    else if (strcmp(test, "TNL_FILENAME") == 0)
      strcpy(auig->tnl_filename, get_str(line, ii));
    else if (strcmp(test, "TNL_FILESIZE") == 0)
      strcpy(auig->tnl_filesize, get_str(line, ii));
    else if (strcmp(test, "TNL_FILEDATE") == 0)
      strcpy(auig->tnl_filedate, get_str(line, ii));
    else if (strcmp(test, "L0_EX") == 0)
      strcpy(auig->l0_ex, get_str(line, ii));
    else if (strcmp(test, "CHK_FLG") == 0)
      strcpy(auig->chk_flg, get_str(line, ii));
    else if (strcmp(test, "OPESTAT") == 0)
      strcpy(auig->opestat, get_str(line, ii));
    else if (strcmp(test, "REC_RBAND") == 0)
      strcpy(auig->rec_rband, get_str(line, ii));
    else if (strcmp(test, "REC_GBAND") == 0)
      strcpy(auig->rec_gband, get_str(line, ii));
    else if (strcmp(test, "REC_BBAND") == 0)
      strcpy(auig->rec_bband, get_str(line, ii));
    else if (strcmp(test, "OFFNADIR") == 0)
      auig->offnadir = get_double(line, ii);
  }
  FREE(test);

  return ok;
}

// Check location information
static int check_auig_location(FILE *ifp, char **header_line, int *n)
{
  dbf_header_t *dbf;
  int ii, nCols;
  char *header = (char *) MALLOC(sizeof(char)*1024);
  fgets(header, 1024, ifp);
  strip_end_whitesp(header);
  int nColumns = get_number_columns(header);
  
  // Read configuration file
  read_header_config("AUIG", &dbf, &nCols);
  
  // ensure we have the columns we need
  int scnid_col = find_str(header, "SCNID");
  int lulat_col = find_str(header, "SCN_LULAT");
  int lulon_col = find_str(header, "SCN_LULON");
  int rulat_col = find_str(header, "SCN_RULAT");
  int rulon_col = find_str(header, "SCN_RULON");
  int ldlat_col = find_str(header, "SCN_LDLAT");
  int ldlon_col = find_str(header, "SCN_LDLON");
  int rdlat_col = find_str(header, "SCN_RDLAT");
  int rdlon_col = find_str(header, "SCN_RDLON");
  
  // kludge to handle goofy AUIG files (duplicate SCN_RULAT cols,
  // the second one should be SCN_RDLAT)
  if (rdlat_col < 0) {
    rdlat_col = find_2nd_str(header, "SCN_RULAT");
    if (rdlat_col == rulat_col)
      rdlat_col = -1;
    else
      printf("Using second SCN_RULAT column as SCN_RDLAT (missing)\n");
  }
  
  // Check whether all visible columns are actually available in the file
  for (ii=0; ii<nCols; ii++) {
    if (find_str(header, dbf[ii].header) < 0 &&
	strcmp(dbf[ii].header, "SCN_RDLAT") != 0) // another kludge
      dbf[ii].visible = FALSE;
  }
  
  int all_ok=TRUE;
  if (scnid_col < 0) {
    printf("Missing: SCNID\n");
    all_ok=FALSE;
  }
  if (lulat_col < 0) {
    printf("Missing: SCN_LULAT\n");
    all_ok=FALSE;
  }
  if (lulon_col < 0) {
    printf("Missing: SCN_LULON\n");
    all_ok=FALSE;
  }
  if (rulat_col < 0) {
    printf("Missing: SCN_RULAT\n");
    all_ok=FALSE;
  }
  if (rulon_col < 0) {
    printf("Missing: SCN_RULON\n");
    all_ok=FALSE;
  }
  if (ldlat_col < 0) {
    printf("Missing: SCN_LDLAT\n");
    all_ok=FALSE;
  }
  if (ldlon_col < 0) {
    printf("Missing: SCN_LDLON\n");
    all_ok=FALSE;
  }
  if (rdlat_col < 0) {
    printf("Missing: SCN_RDLAT\n");
    all_ok=FALSE;
  }
  if (rdlon_col < 0) {
    printf("Missing: SCN_RDLON\n");
    all_ok=FALSE;
  }
  if (!all_ok) {
    printf("Required data columns missing, cannot process this file.\n");
    return 0;
  }
  *header_line = header;
  *n = nColumns;

  return 1;
}

// Convert auig to kml file
int auig2kml(char *in_file, char *out_file, int listFlag)
{
  auig_type_t auig;
  dbf_header_t *dbf;
  char *header;
  int nCols, nColumns;
  char line[1024];
  
  // Read configuration file
  read_header_config("AUIG", &dbf, &nCols);

  FILE *ifp = FOPEN(in_file, "r");
  assert(ifp);
  check_auig_location(ifp, &header, &nColumns);

  FILE *ofp = FOPEN(out_file, "w");
  if (!ofp) {
    printf("Failed to open output file %s: %s\n", out_file, strerror(errno));
    return 0;
  }
  
  kml_header(ofp);
  
  while (fgets(line, 1022, ifp) != NULL) {
    strip_end_whitesp(line);
    
    // now get the individual column values
    auig_init(&auig);
    if (read_auig_line(header, nColumns, line, &auig))
      add_to_kml(ofp, &auig, dbf, nCols);
  }
  
  kml_footer(ofp);
  
  fclose(ifp);
  fclose(ofp);
  
  return 1;
}

void shape_auig_init(char *inFile, char *header)
{
  char *dbaseFile;
  DBFHandle dbase;
  SHPHandle shape;
  dbf_header_t *dbf;
  int ii, nCols, length=50;

  // Read configuration file
  read_header_config("AUIG", &dbf, &nCols);

  // Open database for initialization
  dbaseFile = (char *) MALLOC(sizeof(char)*(strlen(inFile)+5));
  sprintf(dbaseFile, "%s.dbf", inFile);
  dbase = DBFCreate(dbaseFile);
  if (!dbase)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);

  // Add fields to database
  for (ii=0; ii<nCols; ii++) {
    if (strcmp(dbf[ii].header, "SENSOR") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SENSOR", FTString, length, 0) == -1)
        asfPrintError("Could not add SCNID field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCNID") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCNID", FTString, length, 0) == -1)
        asfPrintError("Could not add SCNID field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "DLPATHNO") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "DLPATHNO", FTString, length, 0) == -1)
        asfPrintError("Could not add DLPATHNO field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "USESEGNO") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "USESEGNO", FTString, length, 0) == -1)
        asfPrintError("Could not add USESEGNO field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "RECMODE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "RECMODE", FTString, length, 0) == -1)
        asfPrintError("Could not add RECMODE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "RECPATH") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "RECPATH", FTString, length, 0) == -1)
        asfPrintError("Could not add RECPATH field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "GSCD") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "GSCD", FTString, length, 0) == -1)
        asfPrintError("Could not add GSCD field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "OPEMD") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "OPEMD", FTString, length, 0) == -1)
        asfPrintError("Could not add OPEMD field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "TBLNO") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "TBLNO", FTInteger, 5, 0) == -1)
        asfPrintError("Could not add TBLNO field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REV") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REV", FTString, length, 0) == -1)
        asfPrintError("Could not add REV field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "PATHNO") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "PATHNO", FTInteger, 5, 0) == -1)
        asfPrintError("Could not add PATHNO field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "CENFLMNO") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "CENFLMNO", FTInteger, 6, 0) == -1)
        asfPrintError("Could not add CENFLMNO field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "PROOFFLG") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "PROOFFLG", FTString, length, 0) == -1)
        asfPrintError("Could not add PROOFFLG field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "STEERING") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "STEERING", FTString, length, 0) == -1)
        asfPrintError("Could not add STEERING field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "ORBITSTAT") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "ORBITSTAT", FTString, length, 0) == -1)
        asfPrintError("Could not add ORBITSTAT field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "GRS_LINENO") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "GRS_LINENO", FTInteger, 8, 0) == -1)
        asfPrintError("Could not add GRS_LINENO field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "OBS_ORBITNO") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "OBS_ORBITNO", FTInteger, 8, 0) == -1)
        asfPrintError("Could not add OBS_ORBITNO field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "OBTDIR") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "OBTDIR", FTString, length, 0) == -1)
        asfPrintError("Could not add OBTDIR field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SUN_SUNELE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SUN_SUNELE", FTInteger, 5, 0) == -1)
        asfPrintError("Could not add SUN_SUNELE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SUN_SUNAZI") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SUN_SUNAZI", FTInteger, 5, 0) == -1)
        asfPrintError("Could not add SUN_SUNAZI field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_SDATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_SDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add SCN_SDATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_STIME") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_STIME", FTString, length, 0) == -1)
        asfPrintError("Could not add SCN_STIME field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_CDATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_CDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add SCN_CDATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_CTIME") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_CTIME", FTString, length, 0) == -1)
        asfPrintError("Could not add  field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_CTIME") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "", FTString, length, 0) == -1)
        asfPrintError("Could not add  field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_CLAT") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_CLAT", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_CLAT field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_CLON") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_CLON", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_CLON field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_LULAT") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_LULAT", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_LULAT field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_LULON") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_LULON", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_LULON field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_RULAT") == 0 && 
	     find_2nd_str(header, "SCN_RULAT") != ii && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_RULAT", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_RULAT field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_RULON") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_RULON", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_RULON field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_LDLAT") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_LDLAT", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_LDLAT field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_LDLON") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_LDLON", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_LDLON field to database file\n");
    }
    else if (((strcmp(dbf[ii].header, "SCN_RULAT") == 0 && 
	       find_2nd_str(header, "SCN_RULAT") == ii) || 
	      strcmp(dbf[ii].header, "SCN_RDLAT") == 0) && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_RDLAT", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_RDLAT field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_RDLON") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_RDLON", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_RDLON field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "POINTFLG") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "POINTFLG", FTString, length, 0) == -1)
        asfPrintError("Could not add POINTFLG field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "UCUT") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "UCUT", FTString, length, 0) == -1)
        asfPrintError("Could not add UCUT field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "FCUT") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "FCUT", FTString, length, 0) == -1)
        asfPrintError("Could not add FCUT field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "RCUT") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "RCUT", FTString, length, 0) == -1)
        asfPrintError("Could not add RCUT field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "UGAIN") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "UGAIN", FTString, length, 0) == -1)
        asfPrintError("Could not add UGAIN field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "FGAIN") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "FGAIN", FTString, length, 0) == -1)
        asfPrintError("Could not add FGAIN field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "RGAIN") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "RGAIN", FTString, length, 0) == -1)
        asfPrintError("Could not add RGAIN field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "PNTANG") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "PNTANG", FTString, length, 0) == -1)
        asfPrintError("Could not add PNTANG field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "GAINSTS") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "GAINSTS", FTString, length, 0) == -1)
        asfPrintError("Could not add GAINSTS field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "EPSSTS") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "EPSSTS", FTString, length, 0) == -1)
        asfPrintError("Could not add EPSSTS field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_CLDSCENE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_CLDSCENE", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_CLDSCENE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_CLDDEVSCENE") == 0 && 
	     dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_CLDDEVSCENE", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_CLDDEVSCENE to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_ALLQLTY") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_ALLQLTY", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_ALLQLTY field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_LINELOS") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_LINELOS", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_LINELOS field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "DLSEGNO") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "DLSEGNO", FTString, length, 0) == -1)
        asfPrintError("Could not add DLSEGNO field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "VLDSDATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "VLDSDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add VLDSDATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "TRNSDATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "TRNSDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add TRNSDATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "OBSSDATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "OBSSDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add OBSSDATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "OBSEDATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "OBSEDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add OBSEDATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SATCD") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SATCD", FTString, length, 0) == -1)
        asfPrintError("Could not add SATCD field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "VLDSDEAD_DATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "VLDSDEAD_DATE", FTString, length, 0) == -1)
        asfPrintError("Could not add VLDSDEAD_DATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "VLDEDEAD_DATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "VLDEDEAD_DATE", FTString, length, 0) == -1)
        asfPrintError("Could not add VLDEDEAD_DATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "DATARATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "DATARATE", FTInteger, 5, 0) == -1)
        asfPrintError("Could not add DATARATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "FILENAME") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "FILENAME", FTString, length, 0) == -1)
        asfPrintError("Could not add FILENAME field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "L0STATUS") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "L0STATUS", FTString, length, 0) == -1)
        asfPrintError("Could not add L0STATUS field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "ARCHMODE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "ARCHMODE", FTString, length, 0) == -1)
        asfPrintError("Could not add ARCHMODE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "URGFLG") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "URGFLG", FTString, length, 0) == -1)
        asfPrintError("Could not add URGFLG field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SEMIREAL") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SEMIREAL", FTString, length, 0) == -1)
        asfPrintError("Could not add SEMIREAL field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "GRS_COLNO") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "GRS_COLNO", FTInteger, 8, 0) == -1)
        asfPrintError("Could not add GRS_COLNO field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_POSX") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_POSX", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_POSX field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_POSY") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_POSY", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_POSY field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_POSZ") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_POSZ", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_POSZ field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_SPDX") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_SPDX", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_SPDX field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_SPDY") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_SPDY", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_SPDY field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "SCN_SPDZ") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "SCN_SPDZ", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add SCN_SPDZ field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "ROTCOREVISE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "ROTCOREVISE", FTString, length, 0) == -1)
        asfPrintError("Could not add ROTCOREVISE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_CLDVERSION") == 0 && 
	     dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_CLDVERSION", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_CLDVERSION field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_PIXCEL") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_PIXCEL", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_PIXCEL field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_LINENUM") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_LINENUM", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_LINENUM field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "PRC_BIT_PIXCEL") == 0 && 
	     dbf[ii].visible) {
      if (DBFAddField(dbase, "PRC_BIT_PIXCEL", FTString, length, 0) == -1)
        asfPrintError("Could not add PRC_BIT_PIXCEL field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "PRC_PIXCELSETSTAT") == 0 && 
	     dbf[ii].visible) {
      if (DBFAddField(dbase, "PRC_PIXCELSETSTAT", FTString, length, 0) == -1)
        asfPrintError("Could not add PRC_PIXCELSETSTAT to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_GPS_DMS") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_GPS_DMS", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_GPS_DMS field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_ORBITNUM") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_ORBITNUM", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_ORBITNUM field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_PATHDATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_PATHDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_PATHDATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_PATHNUM") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_PATHNUM", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_PATHNUM field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_VLDSDATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_VLDSDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_VLDSDATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_VLDEDATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_VLDEDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_VLDEDATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_SATCNTPERIOD") == 0 && 
	     dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_SATCNTPERIOD", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_SATCNTPERIOD to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_BASESATTIME") == 0 && 
	     dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_BASESATTIME", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_BASESATTIME to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_BASEGRDDATE") == 0 && 
	     dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_BASEGRDDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_BASEGRDDATE to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_UTC_GPS") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_UTC_GPS", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_UTC_GPS field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "BRS_FILENAME") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "BRS_FILENAME", FTString, length, 0) == -1)
        asfPrintError("Could not add BRS_FILENAME field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "BRS_FILESIZE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "BRS_FILESIZE", FTString, length, 0) == -1)
        asfPrintError("Could not add BRS_FILESIZE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "BRS_FILEDATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "BRS_FILEDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add BRS_FILEDATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "TNL_FILENAME") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "TNL_FILENAME", FTString, length, 0) == -1)
        asfPrintError("Could not add TNL_FILENAME field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "TNL_FILESIZE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "TNL_FILESIZE", FTString, length, 0) == -1)
        asfPrintError("Could not add TNL_FILESIZE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "TNL_FILEDATE") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "TNL_FILEDATE", FTString, length, 0) == -1)
        asfPrintError("Could not add TNL_FILEDATE field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "L0_EX") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "L0_EX", FTString, length, 0) == -1)
        asfPrintError("Could not add L0_EX field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "CHK_FLG") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "CHK_FLG", FTString, length, 0) == -1)
        asfPrintError("Could not add CHK_FLG field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "OPESTAT") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "OPESTAT", FTString, length, 0) == -1)
        asfPrintError("Could not add OPESTAT field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_RBAND") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_RBAND", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_RBAND field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_GBAND") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_GBAND", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_GBAND field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "REC_BBAND") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "REC_BBAND", FTString, length, 0) == -1)
        asfPrintError("Could not add REC_BBAND field to database file\n");
    }
    else if (strcmp(dbf[ii].header, "OFFNADIR") == 0 && dbf[ii].visible) {
      if (DBFAddField(dbase, "OFFNADIR", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add OFFNADIR field to database file\n");
    }
  }

  // Close the database for initialization
  DBFClose(dbase);
  
  // Open shapefile for initialization
  shape = SHPCreate(inFile, SHPT_POLYGON);
  if (!shape)
    asfPrintError("Could not create shapefile '%s'\n", inFile);
  
  // Close shapefile for initialization
  SHPClose(shape);
  
  FREE(dbaseFile);

  return;
}

static void add_to_shape(DBFHandle dbase, SHPHandle shape, auig_type_t *auig,
			 dbf_header_t *dbf, int nCols, int n)
{
  int ii, field = 0;

  // Write fields into the database
  for (ii=0; ii<nCols; ii++) {
    if (strcmp(dbf[ii].header, "SENSOR") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->sensor);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCNID") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->scnid);
      field++;
    }
    else if (strcmp(dbf[ii].header, "DLPATHNO") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->dlpathno);
      field++;
    }
    else if (strcmp(dbf[ii].header, "USESEGNO") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->usesegno);
      field++;
    }
    else if (strcmp(dbf[ii].header, "RECMODE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->recmode);
      field++;
    }
    else if (strcmp(dbf[ii].header, "RECPATH") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->recpath);
      field++;
    }
    else if (strcmp(dbf[ii].header, "GSCD") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->gscd);
      field++;
    }
    else if (strcmp(dbf[ii].header, "OPEMD") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->opemd);
      field++;
    }
    else if (strcmp(dbf[ii].header, "TBLNO") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, auig->tblno);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REV") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rev);
      field++;
    }
    else if (strcmp(dbf[ii].header, "PATHNO") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, auig->pathno);
      field++;
    }
    else if (strcmp(dbf[ii].header, "CENFLMNO") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, auig->cenflmno);
      field++;
    }
    else if (strcmp(dbf[ii].header, "PROOFFLG") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->proofflg);
      field++;
    }
    else if (strcmp(dbf[ii].header, "STEERING") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->steering);
      field++;
    }
    else if (strcmp(dbf[ii].header, "ORBITSTAT") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->orbitstat);
      field++;
    }
    else if (strcmp(dbf[ii].header, "GRS_LINENO") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, auig->grs_lineno);
      field++;
    }
    else if (strcmp(dbf[ii].header, "OBS_ORBITNO") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, auig->obs_orbitno);
      field++;
    }
    else if (strcmp(dbf[ii].header, "OBTDIR") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->obtdir);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SUN_SUNELE") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, auig->sunele);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SUN_SUNAZI") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, auig->sunazi);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_SDATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->scn_sdate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_STIME") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->scn_stime);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_CDATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->scn_cdate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_CTIME") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->scn_ctime);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_CLAT") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_clat);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_CLON") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_clon);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_LULAT") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_lulat);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_LULON") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_lulon);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_RULAT") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_rulat);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_RULON") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_rulon);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_LDLAT") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_ldlat);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_LDLON") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_ldlon);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_RDLAT") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_rdlat);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_RDLON") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_rdlon);
      field++;
    }
    else if (strcmp(dbf[ii].header, "POINTFLG") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->pointflg);
      field++;
    }
    else if (strcmp(dbf[ii].header, "UCUT") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->ucut);
      field++;
    }
    else if (strcmp(dbf[ii].header, "FCUT") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->fcut);
      field++;
    }
    else if (strcmp(dbf[ii].header, "RCUT") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rcut);
      field++;
    }
    else if (strcmp(dbf[ii].header, "UGAIN") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->ugain);
      field++;
    }
    else if (strcmp(dbf[ii].header, "FGAIN") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->fgain);
      field++;
    }
    else if (strcmp(dbf[ii].header, "RGAIN") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rgain);
      field++;
    }
    else if (strcmp(dbf[ii].header, "PNTANG") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->pntang);
      field++;
    }
    else if (strcmp(dbf[ii].header, "GAINSTS") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->gainsts);
      field++;
    }
    else if (strcmp(dbf[ii].header, "EPSSTS") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->epssts);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_CLDSCENE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_cldscene);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_CLDDEVSCENE") == 0 && 
	     dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_clddevscene);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_ALLQLTY") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_allqlty);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_LINELOS") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_linelos);
      field++;
    }
    else if (strcmp(dbf[ii].header, "DLSEGNO") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->dlsegno);
      field++;
    }
    else if (strcmp(dbf[ii].header, "VLDSDATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->vldsdate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "TRNSDATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->trnsdate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "OBSSDATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->obssdate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "OBSEDATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->obsedate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SATCD") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->satcd);
      field++;
    }
    else if (strcmp(dbf[ii].header, "VLDSDEAD_DATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->vldsdead_date);
      field++;
    }
    else if (strcmp(dbf[ii].header, "VLDEDEAD_DATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->vldedead_date);
      field++;
    }
    else if (strcmp(dbf[ii].header, "DATARATE") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, auig->datarate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "FILENAME") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->filename);
      field++;
    }
    else if (strcmp(dbf[ii].header, "L0STATUS") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->l0status);
      field++;
    }
    else if (strcmp(dbf[ii].header, "ARCHMODE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->archmode);
      field++;
    }
    else if (strcmp(dbf[ii].header, "URGFLG") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->urgflg);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SEMIREAL") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->semireal);
      field++;
    }
    else if (strcmp(dbf[ii].header, "GRS_COLNO") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, auig->grs_colno);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_POSX") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_posx);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_POSY") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_posy);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_POSZ") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_posz);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_SPDX") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_spdx);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_SPDY") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_spdy);
      field++;
    }
    else if (strcmp(dbf[ii].header, "SCN_SPDZ") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->scn_spdz);
      field++;
    }
    else if (strcmp(dbf[ii].header, "ROTCOREVISE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rotcorevise);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_CLDVERSION") == 0 && 
	     dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_cldversion);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_PIXCEL") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_pixcel);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_LINENUM") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_linenum);
      field++;
    }
    else if (strcmp(dbf[ii].header, "PRC_BIT_PIXCEL") == 0 && 
	     dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->prc_bit_pixcel);
      field++;
    }
    else if (strcmp(dbf[ii].header, "PRC_PIXCELSETSTAT") == 0 && 
	     dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->prc_pixcelsetstat);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_GPS_DMS") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_gps_dms);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_ORBITNUM") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_orbitnum);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_PATHDATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_pathdate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_PATHNUM") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_pathnum);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_VLDSDATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_vldsdate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_VLDEDATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_vldedate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_SATCNTPERIOD") == 0 && 
	     dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_satcntperiod);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_BASESATTIME") == 0 && 
	     dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_basesattime);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_BASEGRDDATE") == 0 && 
	     dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_basegrddate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_UTC_GPS") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_utc_gps);
      field++;
    }
    else if (strcmp(dbf[ii].header, "BRS_FILENAME") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->brs_filename);
      field++;
    }
    else if (strcmp(dbf[ii].header, "BRS_FILESIZE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->brs_filesize);
      field++;
    }
    else if (strcmp(dbf[ii].header, "BRS_FILEDATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->brs_filedate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "TNL_FILENAME") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->tnl_filename);
      field++;
    }
    else if (strcmp(dbf[ii].header, "TNL_FILESIZE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->tnl_filesize);
      field++;
    }
    else if (strcmp(dbf[ii].header, "TNL_FILEDATE") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->tnl_filedate);
      field++;
    }
    else if (strcmp(dbf[ii].header, "L0_EX") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->l0_ex);
      field++;
    }
    else if (strcmp(dbf[ii].header, "CHK_FLG") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->chk_flg);
      field++;
    }
    else if (strcmp(dbf[ii].header, "OPESTAT") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->opestat);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_RBAND") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_rband);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_GBAND") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_gband);
      field++;
    }
    else if (strcmp(dbf[ii].header, "REC_BBAND") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, auig->rec_bband);
      field++;
    }
    else if (strcmp(dbf[ii].header, "OFFNADIR") == 0 && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, auig->offnadir);
      field++;
    }
  }

  double lat[5], lon[5];
  lat[0] = lat[4] = auig->scn_lulat;
  lon[0] = lon[4] = auig->scn_lulon;
  lat[1] = auig->scn_rulat;
  lon[1] = auig->scn_rulon;
  lat[2] = auig->scn_rdlat;
  lon[2] = auig->scn_rdlon;
  lat[3] = auig->scn_ldlat;
  lon[3] = auig->scn_ldlon;  

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, 5, lon, lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);
}

int auig2shape(char *inFile, char *outFile, int listFlag)
{
  DBFHandle dbase;
  SHPHandle shape;
  auig_type_t auig;
  dbf_header_t *dbf;
  char *header, line[1024];
  int nCols, nColumns, ii=0;

  // Read configuration file
  read_header_config("AUIG", &dbf, &nCols);

  // Read auig file
  FILE *ifp = FOPEN(inFile, "r");
  assert(ifp);
  check_auig_location(ifp, &header, &nColumns);

  // Initalize the database file
  shape_auig_init(outFile, header);
  open_shape(outFile, &dbase, &shape);

  while (fgets(line, 1022, ifp) != NULL) {
    strip_end_whitesp(line);

    // now get the individual column values
    auig_init(&auig);
    if (read_auig_line(header, nColumns, line, &auig)) {
      add_to_shape(dbase, shape, &auig, dbf, nCols, ii);
      ii++;
    }
  }

  // Clean up
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  FCLOSE(ifp);

  return 1;
}
