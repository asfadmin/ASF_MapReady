#include "c2v.h"
#include <assert.h>
#include <errno.h>
#include <ctype.h>

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

static char *my_parse_double(char *p, double *d)
{
    char tmp[64];
    p = my_parse_string(p, tmp, 64);
    if (p && strlen(tmp)>0)
        *d = atof(tmp);
    else
        *d = MAGIC_UNSET_DOUBLE;
    return p;
}

static int parse_alos_csv_line(char *line,
                               char *sensor, char *scnid, char *opemd,
                               int *pathno, int *cenflmn, int *grs_line,
                               char *orbitdir, char *scn_cdate,char *scn_ctime,
                               double *scn_clat, double *scn_clon,
                               double *scn_lulat, double *scn_lulon,
                               double *scn_rulat, double *scn_rulon,
                               double *scn_ldlat, double *scn_ldlon,
                               double *scn_rdlat, double *scn_rdlon,
                               int *grs_colno, double *offnadir)
{
    // cheat!  easier to validate if every piece will end with a comma
    line[strlen(line)+1] = '\0';
    line[strlen(line)] = ',';

    // iterator
    char *p = line;

    // sensor
    p = my_parse_string(p, sensor, 64);
    if (!p) return FALSE;

    // scnid
    p = my_parse_string(p, scnid, 64);
    if (!p) return FALSE;

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

    while (fgets(line, 1024, ifp) != NULL) {
      strip_end_whitesp(line);

      char sensor[64], scnid[64], opemd[64], scn_cdate[64], scn_ctime[64];
      int pathno, cenflmn, grs_line, grs_colno;
      char orbitdir;
      double scn_clat, scn_clon, scn_lulat, scn_lulon, scn_rulat, scn_rulon;
      double scn_ldlat, scn_ldlon, scn_rdlat, scn_rdlon, offnadir;
      
      int ret =
        parse_alos_csv_line(line, sensor, scnid, opemd, &pathno, &cenflmn,
                            &grs_line, &orbitdir, scn_cdate, scn_ctime,
                            &scn_clat, &scn_clon, &scn_lulat, &scn_lulon,
                            &scn_rulat, &scn_rulon, &scn_ldlat, &scn_ldlon,
                            &scn_rdlat, &scn_rdlon, &grs_colno, &offnadir);

      
      
    }

    fclose(ifp);
    fclose(ofp);

    return 1;
}
