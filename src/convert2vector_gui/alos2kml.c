#include "c2v.h"
#include "asf_vector.h"
#include <assert.h>
#include <errno.h>
#include <ctype.h>

typedef struct _AlosCsvInfo
{
    char sensor[64];
    char scnid[64];
    char opemd[64];
    int pathno;
    int cenflmn;
    int grs_line;
    char orbitdir;
    char scn_cdate[64];
    char scn_ctime[64];
    double scn_clat, scn_clon;
    double scn_lulat, scn_lulon;
    double scn_rulat, scn_rulon;
    double scn_ldlat, scn_ldlon;
    double scn_rdlat, scn_rdlon;
    int grs_colno;
    double offnadir;
} AlosCsvInfo;

static void strip_end_whitesp(char *s)
{
    char *p = s + strlen(s) - 1;
    while (isspace(*p) && p>s)
        *p-- = '\0';
}

static void msg(const char *format, ...)
{
    char buf[1024];
    va_list ap;
    va_start(ap, format);
    vsprintf(buf, format, ap);
    va_end(ap);

    // in the future, we'll be putting this in a textview or something!!
    //GtkWidget *tv = get_widget_checked("messages_textview");
    //GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));

    //GtkTextIter end;
    //gtk_text_buffer_get_end_iter(tb, &end);
    //gtk_text_buffer_insert(tb, &end, buf, -1);

    printf(buf);
}

static char *my_parse_string(char *p, char *s, int max_len)
{
    if (*p == '\0') {
        msg("  --> Unexpected end of string\n");
        return NULL;
    }

    // scan ahead to the comma (even last one should end with comma)
    char *q = strchr(p, ',');
    if (!q) {
        msg("  --> Couldn't find the end of the string: %s\n", p);
        return NULL;
    }

    *q = '\0'; // temporarily...
    strncpy_safe(s, p, max_len);
    *q = ',';

    // point to beginning of next item
    return q+1;
}

static char *my_parse_int(char *p, int *i)
{
    char tmp[64];
    p = my_parse_string(p, tmp, 64);
    if (p && strlen(tmp)>0)
        *i = atoi(tmp);
    else
        *i = MAGIC_UNSET_INT;
    return p;
}

static char *my_parse_char(char *p, char *c)
{
    char tmp[64];
    p = my_parse_string(p, tmp, 64);
    if (p && strlen(tmp)==1)
        *c = tmp[0];
    else
        *c = 0;
    return p;
}

static char *my_parse_double(char *p, double *d)
{
    char tmp[64];
    p = my_parse_string(p, tmp, 64);
    if (p && strlen(tmp)>0)
        *d = atof(tmp);
    else
        *d = -999999.99;
    return p;
}

static int parse_alos_csv_line(char *line, AlosCsvInfo *info)
{
    // cheat!  easier to validate if every piece will end with a comma
    line[strlen(line)+1] = '\0';
    line[strlen(line)] = ',';

    // iterator
    char *p = line;

    // sensor
    p = my_parse_string(p, info->sensor, 64);
    if (!p) return FALSE;

    // scnid
    p = my_parse_string(p, info->scnid, 64);
    if (!p) return FALSE;

    // opemd
    p = my_parse_string(p, info->opemd, 64);
    if (!p) return FALSE;

    // pathno
    p = my_parse_int(p, &info->pathno);
    if (!p) return FALSE;
    //if (info->pathno > 671 || info->pathno < 0) {
    //  msg("  Invalid path number: %d\n", info->pathno);
    //  return FALSE;
    //}

    // cenflmn
    p = my_parse_int(p, &info->cenflmn);
    if (!p) return FALSE;

    // grs_line
    p = my_parse_int(p, &info->grs_line);
    if (!p) return FALSE;

    // orbitdir
    p = my_parse_char(p, &info->orbitdir);
    if (!p) return FALSE;
    if (info->orbitdir!='A' && info->orbitdir!='D') {
      msg("  Invalid orbit direction: %c\n", info->orbitdir);
      //  Make this non-fatal for now ...
      //return FALSE;
      info->orbitdir='?';
    }

    // scn_cdate
    p = my_parse_string(p, info->scn_cdate, 64);
    if (!p) return FALSE;

    // scn_ctime
    p = my_parse_string(p, info->scn_ctime, 64);
    if (!p) return FALSE;

    // scn_clat
    p = my_parse_double(p, &info->scn_clat);
    if (!p) return FALSE;
    if (info->scn_clat < -90 || info->scn_clat > 90) {
      msg("  Invalid scn_clat: %f\n", info->scn_clat);
      return FALSE;
    }

    // scn_clon
    p = my_parse_double(p, &info->scn_clon);
    if (!p) return FALSE;
    if (info->scn_clon < -360 || info->scn_clon > 360) {
      msg("  Invalid scn_clon: %f\n", info->scn_clon);
      return FALSE;
    }

    // scn_lulat
    p = my_parse_double(p, &info->scn_lulat);
    if (!p) return FALSE;
    if (info->scn_lulat < -90 || info->scn_lulat > 90) {
      msg("  Invalid scn_lulat: %f\n", info->scn_lulat);
      return FALSE;
    }

    // scn_lulon
    p = my_parse_double(p, &info->scn_lulon);
    if (!p) return FALSE;
    if (info->scn_lulon < -360 || info->scn_lulon > 360) {
      msg("  Invalid scn_lulon: %f\n", info->scn_lulon);
      return FALSE;
    }

    // scn_rulat
    p = my_parse_double(p, &info->scn_rulat);
    if (!p) return FALSE;
    if (info->scn_rulat < -90 || info->scn_rulat > 90) {
      msg("  Invalid scn_rulat: %f\n", info->scn_rulat);
      return FALSE;
    }

    // scn_rulon
    p = my_parse_double(p, &info->scn_rulon);
    if (!p) return FALSE;
    if (info->scn_rulon < -360 || info->scn_rulon > 360) {
      msg("  Invalid scn_rulon: %f\n", info->scn_rulon);
      return FALSE;
    }

    // scn_ldlat
    p = my_parse_double(p, &info->scn_ldlat);
    if (!p) return FALSE;
    if (info->scn_ldlat < -90 || info->scn_ldlat > 90) {
      msg("  Invalid scn_ldlat: %f\n", info->scn_ldlat);
      return FALSE;
    }

    // scn_ldlon
    p = my_parse_double(p, &info->scn_ldlon);
    if (!p) return FALSE;
    if (info->scn_ldlon < -360 || info->scn_ldlon > 360) {
      msg("  Invalid scn_ldlon: %f\n", info->scn_ldlon);
      return FALSE;
    }

    // scn_rdlat
    p = my_parse_double(p, &info->scn_rdlat);
    if (!p) return FALSE;
    if (info->scn_rdlat < -90 || info->scn_rdlat > 90) {
      msg("  Invalid scn_rdlat: %f\n", info->scn_rdlat);
      return FALSE;
    }

    // scn_rdlon
    p = my_parse_double(p, &info->scn_rdlon);
    if (!p) return FALSE;
    if (info->scn_rdlon < -360 || info->scn_rdlon > 360) {
      msg("  Invalid scn_rdlon: %f\n", info->scn_rdlon);
      return FALSE;
    }

    // the next four fields are all ignored for now ...
    //   pntang, rec_cldscene, rec_allqlty, rec_linelos
    char ignore[64];

    // pntang
    p = my_parse_string(p, ignore, 64);

    // rec_cldscene
    p = my_parse_string(p, ignore, 64);

    // rec_allqlty
    p = my_parse_string(p, ignore, 64);

    // rec_linelos
    p = my_parse_string(p, ignore, 64);

    // ... now back to fields that are actually read in

    // grs_colno
    p = my_parse_int(p, &info->grs_colno);
    if (!p) return FALSE;

    // offnadir
    p = my_parse_double(p, &info->offnadir);
    if (!p) return FALSE;

    if (*p != '\0') // non-fatal error
      msg("  Line has extra characters\n");

    return TRUE;
}

static void add_to_kml(FILE *fp, AlosCsvInfo *info)
{
  fprintf(fp, "<Placemark>\n");
  fprintf(fp, "  <description><![CDATA[\n");
  fprintf(fp, "<strong>Sensor</strong>: %s<br>\n", info->sensor);
  fprintf(fp, "<strong>Scene ID</strong>: %s<br>\n", info->scnid);
  fprintf(fp, "<strong>Path</strong>: %d<br>\n", info->pathno);
  fprintf(fp, "<strong>Date/Time</strong>: %s %s<br>\n",
          info->scn_cdate, info->scn_ctime);
  if (info->orbitdir=='A')
    fprintf(fp, "<strong>Orbit Dir</strong>: Ascending<br>\n");
  else if (info->orbitdir=='D')
    fprintf(fp, "<strong>Orbit Dir</strong>: Descending<br>\n");
  else
    fprintf(fp, "<strong>Orbit Dir</strong>: Unknown<br>\n");
  fprintf(fp, "  ]]></description>\n");
  fprintf(fp, "  <name>%s</name>\n", info->scnid);
  fprintf(fp, "  <LookAt>\n");
  fprintf(fp, "    <longitude>%.10f</longitude>\n", info->scn_clon);
  fprintf(fp, "    <latitude>%.10f</latitude>\n", info->scn_clat);
  fprintf(fp, "    <range>400000</range>\n");
  fprintf(fp, "    <tilt>30</tilt>\n");
  fprintf(fp, "  </LookAt>\n");
  fprintf(fp, "  <visibility>1</visibility>\n");
  fprintf(fp, "  <open>1</open>\n");

  write_kml_style_keys(fp);

  fprintf(fp, "  <Polygon>\n");
  fprintf(fp, "    <extrude>1</extrude>\n");
  fprintf(fp, "    <altitudeMode>absolute</altitudeMode>\n");
  fprintf(fp, "    <outerBoundaryIs>\n");
  fprintf(fp, "     <LinearRing>\n");
  fprintf(fp, "      <coordinates>\n");
  fprintf(fp, "       %.12f,%.12f,7000\n", info->scn_lulon, info->scn_lulat);
  fprintf(fp, "       %.12f,%.12f,7000\n", info->scn_rulon, info->scn_rulat);
  fprintf(fp, "       %.12f,%.12f,7000\n", info->scn_rdlon, info->scn_rdlat);
  fprintf(fp, "       %.12f,%.12f,7000\n", info->scn_ldlon, info->scn_ldlat);
  fprintf(fp, "       %.12f,%.12f,7000\n", info->scn_lulon, info->scn_lulat);
  fprintf(fp, "      </coordinates>\n");
  fprintf(fp, "     </LinearRing>\n");
  fprintf(fp, "    </outerBoundaryIs>\n");
  fprintf(fp, "  </Polygon>\n");
  fprintf(fp, "</Placemark>\n");
}

int alos_csv_to_kml(const char *in_file, const char *out_file)
{
    FILE *ifp = FOPEN(in_file, "r");
    assert(ifp);

    char line[1024];
    fgets(line, 1024, ifp);
    strip_end_whitesp(line);

    const char const * expected_csv_header_line =
      "SENSOR,SCNID,OPEMD,PATHNO,CENFLMNO,GRS_LINENO,OBTDIR,SCN_CDATE,"
      "SCN_CTIME,SCN_CLAT,SCN_CLON,SCN_LULAT,SCN_LULON,SCN_RULAT,SCN_RULON,"
      "SCN_LDLAT,SCN_LDLON,SCN_RDLAR,SCN_RDLON,PNTANG,REC_CLDSCENE,"
      "REC_ALLQLTY,REC_LINELOS,GRS_COLNO,OFFNADIR";

    if (strcmp(line, expected_csv_header_line) != 0) {
      printf("Header line differed from expected!\n"
             "Is '%s' a valid ALOS CSV file?\n", in_file);
      return 0;
    }

    FILE *ofp = FOPEN(out_file, "w");
    if (!ofp) {
      printf("Failed to open output file %s: %s\n", out_file, strerror(errno));
      return 0;
    }

    kml_header(ofp);

    int n_valid_lines = 0;
    int n_invalid_lines = 0;
    int line_no = 2; // start at 2, we already read line 1
    while (fgets(line, 1024, ifp) != NULL) {
      strip_end_whitesp(line);

      AlosCsvInfo info;
      int valid = parse_alos_csv_line(line, &info);

      if (valid) {
        add_to_kml(ofp, &info);
        ++n_valid_lines;
      } else {
        msg("Invalid line %d in CSV file.\n", line_no);
        ++n_invalid_lines;
      }

      ++line_no;
    }

    kml_footer(ofp);

    fclose(ifp);
    fclose(ofp);

    return 1;
}
