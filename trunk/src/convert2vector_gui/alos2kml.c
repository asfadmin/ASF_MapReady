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
    char full_line[1024];
} AlosCsvInfo;

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

static void add_to_kml(FILE *fp, AlosCsvInfo *info, char *header_line)
{
  fprintf(fp, "<Placemark>\n");
  fprintf(fp, "  <description><![CDATA[\n");
  if (strlen(header_line)>0)
    fprintf(fp, "<!-- CSVHeader:%s -->\n", header_line);
  fprintf(fp, "<!-- CSVLine:%s -->\n", info->full_line);
  if (strlen(info->sensor)>0 && strcmp(info->sensor,MAGIC_UNSET_STRING)!=0)
    fprintf(fp, "<strong>Sensor</strong>: %s<br>\n", info->sensor);
  fprintf(fp, "<strong>Scene ID</strong>: %s<br>\n", info->scnid);
  if (info->pathno>0)
    fprintf(fp, "<strong>Path</strong>: %d<br>\n", info->pathno);
  if (strlen(info->scn_cdate)>0 && strlen(info->scn_ctime)>0 &&
      strcmp(info->scn_cdate,MAGIC_UNSET_STRING)!=0 &&
      strcmp(info->scn_ctime,MAGIC_UNSET_STRING)!=0)
    fprintf(fp, "<strong>Date/Time</strong>: %s %s<br>\n",
            info->scn_cdate, info->scn_ctime);
  else if (strlen(info->scn_cdate)>0 &&
           strcmp(info->scn_cdate,MAGIC_UNSET_STRING)!=0)
    fprintf(fp, "<strong>Date</strong>: %s<br>\n", info->scn_cdate);
  else if (strlen(info->scn_ctime)>0 &&
           strcmp(info->scn_ctime,MAGIC_UNSET_STRING)!=0)
    fprintf(fp, "<strong>Time</strong>: %s<br>\n", info->scn_ctime);
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

int kml_to_alos_csv(const char *in_file, const char *out_file)
{
    FILE *ifp = FOPEN(in_file, "r");
    assert(ifp);

    FILE *ofp = FOPEN(out_file, "w");
    if (!ofp) {
      printf("Failed to open output file %s: %s\n", out_file, strerror(errno));
      return 0;
    }

    int hdr_count = 0;
    int line_count = 0;

    char line[1024];
    while (fgets(line, 1023, ifp) != NULL) {
        if (strstr(line, "CSVHeader") || strstr(line, "CSVLine"))
        {
            if (strstr(line, "CSVHeader"))
                ++hdr_count;
            if (strstr(line, "CSVLine"))
                ++line_count;

            char *p = strchr(line, ':');
            if (p) {
              ++p;
              char *q = strstr(p, "-->");
              if (q) {
                *q='\0';
                fprintf(ofp, "%s\n", p);
              }
            }
        }
    }

    fclose(ifp);
    fclose(ofp);

    if (hdr_count < 1)
      message_box("No header line found -- this may be a KML file that\n"
                  "was not converted from an AUIG csv file.  Currently,\n"
                  "Convert To Vector can only convert KML files that were\n"
                  "created by this program from the ALOS CSV format.\n");
    else if (hdr_count > 1)
      message_box("More than one header line found! "
                  "Output is probably invalid.\n");
    else if (line_count == 0)
      message_box("No data lines found.\n");

    return 1;
}

int alos_csv_to_kml(const char *in_file, const char *out_file)
{
    FILE *ifp = FOPEN(in_file, "r");
    assert(ifp);

    char line[1024],header[1024];
    fgets(header, 1024, ifp);
    strip_end_whitesp_inplace(header);

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

    // don't really care if any of these are missing
    int sensor_col = find_str(header, "SENSOR");
    int opemd_col = find_str(header, "OPEMD");
    int pathno_col = find_str(header, "PATHNO");
    int cenflmno_col = find_str(header, "CENFLMNO");
    int grs_line_col = find_str(header, "GRS_LINE");
    int orbitdir_col = find_str(header, "OBTDIR");
    int cdate_col = find_str(header, "SCN_CDATE");
    int ctime_col = find_str(header, "SCN_CTIME");
    int clat_col = find_str(header, "SCN_CLAT");
    int clon_col = find_str(header, "SCN_CLON");
    int grs_col = find_str(header, "GRS_COLNO");
    int offnadir_col = find_str(header, "OFFNADIR");

    //const char const * expected_csv_header_line =
    //  "SENSOR,SCNID,OPEMD,PATHNO,CENFLMNO,GRS_LINENO,OBTDIR,SCN_CDATE,"
    //  "SCN_CTIME,SCN_CLAT,SCN_CLON,SCN_LULAT,SCN_LULON,SCN_RULAT,SCN_RULON,"
    //  "SCN_LDLAT,SCN_LDLON,SCN_RDLAR,SCN_RDLON,PNTANG,REC_CLDSCENE,"
    //  "REC_ALLQLTY,REC_LINELOS,GRS_COLNO,OFFNADIR";

    FILE *ofp = FOPEN(out_file, "w");
    if (!ofp) {
      printf("Failed to open output file %s: %s\n", out_file, strerror(errno));
      return 0;
    }

    kml_header(ofp);

    int n_valid_lines = 0;
    int n_invalid_lines = 0;
    int line_no = 2; // start at 2, we already read line 1
    while (fgets(line, 1022, ifp) != NULL) {
      strip_end_whitesp_inplace(line);

      AlosCsvInfo info;

      // copy the full line
      strcpy(info.full_line, line);

      // now get the individual column values
      int ok;
      strcpy(info.sensor, get_str(line, sensor_col));
      strcpy(info.scnid, get_str(line, scnid_col));
      strcpy(info.opemd, get_str(line, opemd_col));
      info.pathno = get_int(line, pathno_col);
      info.cenflmn = get_int(line, cenflmno_col);
      info.grs_line = get_int(line, grs_line_col);
      info.orbitdir = get_char(line, orbitdir_col);
      strcpy(info.scn_cdate, get_str(line, cdate_col));
      strcpy(info.scn_ctime, get_str(line, ctime_col));
      info.scn_clat = get_double(line, clat_col);
      info.scn_clon = get_double(line, clon_col);
      info.scn_lulat = get_req_double(line, lulat_col, &ok);
      info.scn_lulon = get_req_double(line, lulon_col, &ok);
      info.scn_rulat = get_req_double(line, rulat_col, &ok);
      info.scn_rulon = get_req_double(line, rulon_col, &ok);
      info.scn_ldlat = get_req_double(line, ldlat_col, &ok);
      info.scn_ldlon = get_req_double(line, ldlon_col, &ok);
      info.scn_rdlat = get_req_double(line, rdlat_col, &ok);
      info.scn_rdlon = get_req_double(line, rdlon_col, &ok);
      info.grs_colno = get_int(line, grs_col);
      info.offnadir = get_double(line, offnadir_col);

      if (ok) {
        add_to_kml(ofp, &info, header);
        ++n_valid_lines;

        strcpy(header, "");
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
