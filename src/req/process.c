#include "req.h"
#include <time.h>
#include <assert.h>
#include "asf_nan.h"

static void
get_combo_box_entry_item(const char *widget_name, char *dest)
{
    GtkWidget *w = get_widget_checked(widget_name);
    GtkEntry *e = GTK_ENTRY (GTK_BIN (w)->child);
    strcpy(dest, gtk_entry_get_text(e));
}

static int is_all_asterisks(char *s)
{
    assert(s && strlen(s)>0);
    char *p=s;
    do {
        if (*p++ != '*')
            return FALSE;
    } while (*p);
    return TRUE;
}

static char *my_parse_string(char *p, char *s)
{
    if (*p == '\0') {
        printf("  --> Unexpected end of string\n");
        return NULL;
    }

    // scan ahead to the comma (even last one should end with comma)
    char *q = strchr(p, ',');
    if (!q) {
        printf("  --> Couldn't find the end of the string: %s\n", p);
        return NULL;
    }

    *q = '\0'; // temporarily...
    strcpy(s, p);
    *q = ',';

    // point to beginning of next item
    return q+1;
}

static char *my_parse_int(char *p, int *i)
{
    char tmp[64];
    p = my_parse_string(p, tmp);
    if (p && strlen(tmp)>0 && !is_all_asterisks(tmp))
        *i = atoi(tmp);
    else
        *i = MAGIC_UNSET_INT;
    return p;
}

static char *my_parse_long(char *p, long *l)
{
    char tmp[64];
    p = my_parse_string(p, tmp);
    if (p && strlen(tmp)>0 && !is_all_asterisks(tmp))
        *l = atol(tmp);
    else
        *l = MAGIC_UNSET_INT;
    return p;
}

static char *my_parse_double(char *p, double *d)
{
    char tmp[64];
    p = my_parse_string(p, tmp);
    if (p && strlen(tmp)>0 && !is_all_asterisks(tmp))
        *d = atof(tmp);
    else
        *d = MAGIC_UNSET_DOUBLE;
    return p;
}

static int valid_angle(double a, double min, double max)
{
    if (meta_is_valid_double(a))
        return a > min && a < max;
    else
        return TRUE;
}

static int valid_avnir_pointing_angle(double a)
{
    if (meta_is_valid_double(a)) {
        // in the form "SNN.NN" the last digit must be EVEN
        char tmp[64];
        sprintf(tmp, "%6.2f", a);
        assert(strlen(tmp)==6);
        char l = tmp[5];
        if (l!='0' && l!='2' && l!='4' && l!='6' && l!='8')
            return FALSE;
    }
    return TRUE;
}

static int parse_line(// input
                      char *line,
                      // outputs:
                      char *sensor, long *date, int *path, double *start_lat,
                      int *direction, int *duration, char *observation_mode,
                      int *observation_purpose, double *prism_nadir_angle,
                      double *prism_forward_angle, double *prism_backward_angle,
                      int *prism_nadir_gain, int *prism_forward_gain,
                      int *prism_backward_gain, double *avnir_pointing_angle,
                      int *avnir_gain, int *avnir_exposure,
                      int *palsar_table_number)
{
    // cheat!  easier to validate if every piece will end with a comma
    line[strlen(line)] = '\0';
    line[strlen(line)-1] = ',';

    // iterator
    char *p = line;

    // some flags to help validation
    int is_prism = 0;
    int is_avnir = 0;
    int is_palsar = 0;

    // sensor
    p = my_parse_string(p, sensor);
    if (!p) return FALSE;
    if (strcmp(sensor, "PSM") == 0)
        is_prism = 1;
    else if (strcmp(sensor, "AV2") == 0)
        is_avnir = 1;
    else if (strcmp(sensor, "PSR") == 0)
        is_palsar = 1;
    else {
        printf("  --> Invalid sensor string: %s\n", sensor);
        return FALSE;
    }
    assert(is_avnir + is_prism + is_palsar == 1);

    // date
    p = my_parse_long(p, date);
    if (!p) return FALSE;
    char tmp[32];
    sprintf(tmp, "%ld", *date);
    if (strlen(tmp) != 8 || tmp[0] != '2') { // need to update this in 2999
        printf("  --> Invalid date: %ld\n", *date);
        return FALSE;
    }

    // path
    p = my_parse_int(p, path);
    if (!p) return FALSE;
    if (*path > 671 || *path < 0) {
        printf("  --> Invalid path number: %d\n", *path);
        return FALSE;
    }

    // start_lat
    p = my_parse_double(p, start_lat);
    if (!p) return FALSE;

    // direction
    p = my_parse_int(p, direction);
    if (!p) return FALSE;
    if (*direction != 1 && *direction != 2) {
        printf("  --> Invalid direction: %d\n", *direction);
        return FALSE;
    }

    // duration
    p = my_parse_int(p, duration);
    if (!p) return FALSE;

    // observation mode
    p = my_parse_string(p, observation_mode);
    if (!p) return FALSE;
    if (strlen(observation_mode) != 3) {
        printf("  -> Invalid observation mode: %s\n", observation_mode);
        return FALSE;
    }
 
    // observation purpose
    p = my_parse_int(p, observation_purpose);
    if (!p) return FALSE;
    if (*observation_purpose < 1 || *observation_purpose > 6) {
        printf("  --> Invalid observation purpose: %d\n",
            *observation_purpose);
        return FALSE;
    }

    // prism nadir angle
    p = my_parse_double(p, prism_nadir_angle);
    if (!p) return FALSE;
    if (meta_is_valid_double(*prism_nadir_angle) && !is_prism) {
        printf("  --> prism nadir angle set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && !valid_angle(*prism_nadir_angle, -1.5, 1.5)) {
        printf("  --> prism nadir angle appears invalid: %g\n", *prism_nadir_angle);
        return FALSE;
    }

    // prism forward angle
    p = my_parse_double(p, prism_forward_angle);
    if (!p) return FALSE;
    if (meta_is_valid_double(*prism_forward_angle) && !is_prism) {
        printf("  --> prism forward angle set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && !valid_angle(*prism_forward_angle, -1.5, 1.5)) {
        printf("  --> prism forward angle appears invalid: %g\n", *prism_forward_angle);
        return FALSE;
    }

    // prism backward angle
    p = my_parse_double(p, prism_backward_angle);
    if (!p) return FALSE;
    if (meta_is_valid_double(*prism_backward_angle) && !is_prism) {
        printf("  --> prism backward angle set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && !valid_angle(*prism_backward_angle, -1.5, 1.5)) {
        printf("  --> prism backward angle appears invalid: %g\n", *prism_backward_angle);
        return FALSE;
    }

    // prism nadir gain
    p = my_parse_int(p, prism_nadir_gain);
    if (!p) return FALSE;
    if (*prism_nadir_gain != MAGIC_UNSET_INT && !is_prism) {
        printf("  --> prism nadir gain set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && *prism_nadir_gain != MAGIC_UNSET_INT &&
            (*prism_nadir_gain < 1 || *prism_nadir_gain > 4)) {
        printf("  --> prism nadir invalid: %d\n", *prism_nadir_gain);
        return FALSE;
    }

    // prism forward gain
    p = my_parse_int(p, prism_forward_gain);
    if (!p) return FALSE;
    if (*prism_forward_gain != MAGIC_UNSET_INT && !is_prism) {
        printf("  --> prism forward gain set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && *prism_forward_gain != MAGIC_UNSET_INT &&
            (*prism_forward_gain < 1 || *prism_forward_gain > 4)) {
        printf("  --> prism forward invalid: %d\n", *prism_forward_gain);
        return FALSE;
    }

    // prism backward gain
    p = my_parse_int(p, prism_backward_gain);
    if (!p) return FALSE;
    if (*prism_backward_gain != MAGIC_UNSET_INT && !is_prism) {
        printf("  --> prism backward gain set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && *prism_backward_gain != MAGIC_UNSET_INT &&
            (*prism_backward_gain < 1 || *prism_backward_gain > 4)) {
        printf("  --> prism backward invalid: %d\n", *prism_backward_gain);
        return FALSE;
    }

    // avnir pointing angle
    p = my_parse_double(p, avnir_pointing_angle);
    if (!p) return FALSE;
    if (meta_is_valid_double(*avnir_pointing_angle) && !is_avnir) {
        printf("  --> avnir pointing angle set for non-avnir data\n");
        return FALSE;
    }
    if (!valid_angle(*avnir_pointing_angle, -44, 44)) {
        printf("  --> avnir pointing angle appears invalid: %g\n", *avnir_pointing_angle);
        return FALSE;
    }
    if (!valid_avnir_pointing_angle(*avnir_pointing_angle)) {
        printf("  --> avnir pointing angle appears invalid: %g\n", *avnir_pointing_angle);
        printf("       (+/- 0.02 steps is absolutely specified)\n");
        return FALSE;
    }

    // avnir gain
    p = my_parse_int(p, avnir_gain);
    if (!p) return FALSE;
    if (*avnir_gain != MAGIC_UNSET_INT && !is_avnir) {
        printf("  --> avnir gain set for non-avnir data\n");
        return FALSE;
    }
    if (is_avnir && *avnir_gain != MAGIC_UNSET_INT &&
            (*avnir_gain < 1 || *avnir_gain > 4)) {
        printf("  --> avnir gain invalid: %d\n", *avnir_gain);
        return FALSE;
    }

    // anvir exposure
    p = my_parse_int(p, avnir_exposure);
    if (!p) return FALSE;
    if (*avnir_exposure != MAGIC_UNSET_INT && !is_avnir) {
        printf("  --> avnir exposure set for non-avnir data\n");
        return FALSE;
    }

    // palsar table number
    p = my_parse_int(p, palsar_table_number);
    if (!p) return FALSE;
    if (*palsar_table_number != MAGIC_UNSET_INT && !is_palsar) {
        printf("  --> palsar table number set for non-palsar data\n");
        return FALSE;
    }
    if (is_palsar && *palsar_table_number != MAGIC_UNSET_INT &&
            (*palsar_table_number > 999 || *palsar_table_number < 1)) {
        printf("  --> palsar table number is invalid: %d\n", *palsar_table_number);
        return FALSE;
    }

    // end if the line!
    if (*p != '\0') // non-fatal error
        printf("  --> Line has extra characters\n");

    return TRUE;
}

static int direction_converter(int direction)
{
    // 2 --> ascending --> 0
    // 1 --> descending --> 1
    if (direction == 2) return 0;
    if (direction == 1) return 1;
    assert(0); // impossible
    return 42;
}

static void write_common(FILE *fout, long date, int path, double start_lat,
                        int direction, int duration, char *observation_mode,
                        int observation_purpose, int req_id, char *sensor)
{
    char tmp_buf[32];
    //   0   16  Observation Request ID
    //  16    1  Blank
    snprintf(tmp_buf, 32, "AADN%011dD ", req_id);
    assert(strlen(tmp_buf)==17);
    fwrite(tmp_buf, sizeof(char), 17, fout);
    //  17    2  Observation Request ID Blanch Number
    //  19    1  Blank
    fwrite("** ", sizeof(char), 3, fout);
    //  20    3  Sensor Name: "PSM"
    //  23    1  Blank
    snprintf(tmp_buf, 32, "%s ", sensor);
    assert(strlen(tmp_buf)==4);
    fwrite(tmp_buf, sizeof(char), 4, fout);
    //  24    8  Observation Date (UTC)
    //  32    1  Blank
    snprintf(tmp_buf, 32, "%ld ", date);
    assert(strlen(tmp_buf)==9);
    fwrite(tmp_buf, sizeof(char), 9, fout);
    //  33    5  Path Number
    //  38    1  Blank
    snprintf(tmp_buf, 32, " %3d  ", path);
    assert(strlen(tmp_buf)==6);
    fwrite(tmp_buf, sizeof(char), 6, fout);
    //  39    7  Observation Beginning Latitude
    //  46    1  Blank
    snprintf(tmp_buf, 32, "%7.3f ", start_lat);
    assert(strlen(tmp_buf)==8);
    fwrite(tmp_buf, sizeof(char), 8, fout);
    //  47    1  Asc/Desc-ending Flag
    //  48    1  Blank
    snprintf(tmp_buf, 32, "%1d ", direction_converter(direction));
    assert(strlen(tmp_buf)==2);
    fwrite(tmp_buf, sizeof(char), 2, fout);
    //  49    4  Observation Time
    //  53    1  Blank
    snprintf(tmp_buf, 32, "%4d ", duration);
    assert(strlen(tmp_buf)==5);
    fwrite(tmp_buf, sizeof(char), 5, fout);
    //  54    3  Operation Mode
    //  57    1  Blank
    snprintf(tmp_buf, 32, "%s ", observation_mode);
    assert(strlen(tmp_buf)==4);
    fwrite(tmp_buf, sizeof(char), 4, fout);
    //  58    2  Observation Purpose
    //  60    1  Blank
    snprintf(tmp_buf, 32, "%2d ", observation_purpose);
    assert(strlen(tmp_buf)==3);
    fwrite(tmp_buf, sizeof(char), 3, fout);
}

static void write_prism(FILE *fout,
                        double prism_nadir_angle, double prism_forward_angle,
                        double prism_backward_angle, int prism_nadir_gain,
                        int prism_forward_gain, int prism_backward_gain)
{
    char tmp_buf[32];
    //  61    6  Angle Of Nadir View Image
    //  67    1  Blank
    if (meta_is_valid_double(prism_nadir_angle))
        snprintf(tmp_buf, 32, "%6.2f ", prism_nadir_angle);
    else
        strcpy(tmp_buf, "****** ");
    assert(strlen(tmp_buf)==7);
    fwrite(tmp_buf, sizeof(char), 7, fout);
    //  68    6  Angle Of Forward View Image
    //  74    1  Blank
    if (meta_is_valid_double(prism_forward_angle))
        snprintf(tmp_buf, 32, "%6.2f ", prism_forward_angle);
    else
        strcpy(tmp_buf, "****** ");
    assert(strlen(tmp_buf)==7);
    fwrite(tmp_buf, sizeof(char), 7, fout);
    //  75    6  Angle Of Backward View Image
    //  81    1  Blank
    if (meta_is_valid_double(prism_backward_angle))
        snprintf(tmp_buf, 32, "%6.2f ", prism_backward_angle);
    else
        strcpy(tmp_buf, "****** ");
    assert(strlen(tmp_buf)==7);
    fwrite(tmp_buf, sizeof(char), 7, fout);
    //  82    1  Gain (Nadir)
    //  83    1  Blank
    if (prism_nadir_gain == MAGIC_UNSET_INT)
        strcpy(tmp_buf, "* ");
    else
        snprintf(tmp_buf, 32, "%d ", prism_nadir_gain);
    assert(strlen(tmp_buf)==2);
    fwrite(tmp_buf, sizeof(char), 2, fout);
    //  84    1  Gain (Forward)
    //  85    1  Blank
    if (prism_forward_gain == MAGIC_UNSET_INT)
        strcpy(tmp_buf, "* ");
    else
        snprintf(tmp_buf, 32, "%d ", prism_forward_gain);
    assert(strlen(tmp_buf)==2);
    fwrite(tmp_buf, sizeof(char), 2, fout);
    //  86    1  Gain (Backward)
    if (prism_backward_gain == MAGIC_UNSET_INT)
        strcpy(tmp_buf, "*");
    else
        snprintf(tmp_buf, 32, "%d", prism_backward_gain);
    assert(strlen(tmp_buf)==1);
    fwrite(tmp_buf, sizeof(char), 1, fout);
    //  87    1  HEX 0A
    tmp_buf[0] = 0x0A;
    fwrite(tmp_buf, sizeof(char), 1, fout);
}

static void write_avnir(FILE *fout, double avnir_pointing_angle,
                        int avnir_gain, int avnir_exposure)
{
    char tmp_buf[32];
    //  61    6  Pointing Angle (degrees)
    //  67    1  Blank
    if (meta_is_valid_double(avnir_pointing_angle))
        snprintf(tmp_buf, 32, "%6.2f ", avnir_pointing_angle);
    else
        strcpy(tmp_buf, "****** ");
    assert(strlen(tmp_buf)==7);
    fwrite(tmp_buf, sizeof(char), 7, fout);
    //  68    4  Gain Status
    //  72    1  Blank
    if (avnir_gain == MAGIC_UNSET_INT)
        strcpy(tmp_buf, "**** ");
    else
        snprintf(tmp_buf, 32, "%4d ", avnir_gain);
    assert(strlen(tmp_buf)==5);
    fwrite(tmp_buf, sizeof(char), 5, fout);
    //  73    4  Exposure Status
    if (avnir_exposure == MAGIC_UNSET_INT)
        strcpy(tmp_buf, "****");
    else
        snprintf(tmp_buf, 32, "%4d", avnir_exposure);
    assert(strlen(tmp_buf)==4);
    fwrite(tmp_buf, sizeof(char), 4, fout);
    //  77    1  HEX 0A
    tmp_buf[0] = 0x0A;
    fwrite(tmp_buf, sizeof(char), 1, fout);
}

static void write_palsar(FILE *fout, int palsar_table_number)
{
    char tmp_buf[32];
    //  61    3  Table Number
    //  64    1  Blank
    //  65    1  "0"
    snprintf(tmp_buf, 32, "%3d 0", palsar_table_number);
    assert(strlen(tmp_buf)==5);
    fwrite(tmp_buf, sizeof(char), 5, fout);
    //  66    1  HEX 0A
    tmp_buf[0] = 0x0A;
    fwrite(tmp_buf, sizeof(char), 1, fout);
}

void process(const char *csv_file, const char *req_file, int is_emergency,
             int *req_id)
{
    printf("Process> %s -> %s\n", csv_file, req_file);

    char req_type = is_emergency ? 'E' : 'W';

    FILE *fin = fopen(csv_file, "r");
    if (!fin) {
        message_box("Failed to open CSV file: %s\n", csv_file);
        return;
    }

    char line[1024];

    // read header line
    fgets(line, 1024, fin);
    const char *expected_header = 
        "Sensor,Date,Path,Start Lat,Direction,Duration,Observation_Mode,"
        "Observation_Purpose,PRISM_Nadir_Angle,PRISM_Forward_Angle,"
        "PRISM_Backward_Angle,PRISM_Gain_Nadir,PRISM_Gain_Forward,"
        "PRISM_Gain_Backward,AVNIR_Pointing_Angle,AVNIR_Gain,"
        "AVNIR_Exposure,PALSAR_Table_Number\n";

    // where we will put all the above, when processing a line
    char sensor[256];
    long date;
    int path;
    double start_lat;
    int direction;
    int duration;
    char observation_mode[256];
    int observation_purpose;
    double prism_nadir_angle;
    double prism_forward_angle;
    double prism_backward_angle;
    int prism_gain_nadir;
    int prism_gain_forward;
    int prism_gain_backward;
    double avnir_pointing_angle;
    int avnir_gain;
    int avnir_exposure;
    int palsar_table_number;

    if (strcmp(line, expected_header) != 0)
    {
        message_box("CSV File header line differs from expected\nExp: %s\nGot: %s\n",
            expected_header, line);
        fclose(fin);
        return;
    }

    // read in the whole file, so we can determine the number of requests, the
    // start date, and the end date.
    long first_date, last_date;
    int n_requests = 0;
    int n_prism = 0, n_avnir = 0, n_palsar = 0;
    int line_no = 2; // start at 2, since we already read 1 line.
    while (fgets(line, 1024, fin) != NULL) {
        int valid =
            parse_line(line, sensor, &date, &path, &start_lat, &direction, &duration,
                observation_mode, &observation_purpose, &prism_nadir_angle,
                &prism_forward_angle, &prism_backward_angle, &prism_gain_nadir,
                &prism_gain_forward, &prism_gain_backward, &avnir_pointing_angle,
                &avnir_gain, &avnir_exposure, &palsar_table_number);

        if (valid) {
            if (n_requests++ == 0) {
                first_date = last_date = date;
            } else {
                if (date < first_date) first_date = date;
                if (date > last_date) last_date = date;
            }
            if (strcmp(sensor,"PSM")==0)
                ++n_prism;
            else if (strcmp(sensor,"AV2")==0)
                ++n_avnir;
            else if (strcmp(sensor,"PSR")==0)
                ++n_palsar;
            else
                assert(0); // was supposed to check for this above
        } else {
            printf("Invalid line %d in CSV file.\n", line_no);
        }
        ++line_no;
    }

    fclose(fin);

    printf("Found %d requests, range: %ld -> %ld\n",
        n_requests, first_date, last_date);

    // now, start working on the request file.
    // first, get current date/time, to stamp the request file.  FIXME: UTC
    time_t t;
    t = time(NULL);
    char time_stamp[10];
    strftime(time_stamp, 10, "%T", gmtime(&t)); // localtime(&t));
    char date_stamp[10];
    strftime(date_stamp, 10, "%Y%m%d", gmtime(&t)); // localtime(&t));

    // open output file
    FILE *fout = fopen(req_file, "wb");
    if (!fout) {
        message_box("Failed to open output file: %s\n", req_file);
        fclose(fout);
        return;
    }

    // write output file header lines
    char tmp_buf[32];

    // Header Structure:
    //   0   10   File Name: REQxnnnnnn
    char *basename = get_basename(req_file);
    assert(strlen(basename)==10);
    fwrite(basename, sizeof(char), 10, fout);
    free(basename);
    //  10    1   Blank
    //  11    6   Fixed string: "ALOS  "
    //  17    1   Blank
    fwrite(" ALOS   ", sizeof(char), 8, fout);
    //  18    4   Station Code (from): "AADN"
    //  22    1   Blank
    //  23    4   EOC MMO Code (to): "HMMO"
    //  27    1   Blank
    fwrite("AADN HMMO ", sizeof(char), 10, fout);
    //  28    8   File Creation Date (UTC): YYYMMDD
    assert(strlen(date_stamp)==8);
    fwrite(date_stamp, sizeof(char), 8, fout);
    //  36    1   Blank
    fwrite(" ", sizeof(char), 1, fout);
    //  37    8   File Creation Time (UTC): hh:mm:ss
    assert(strlen(time_stamp)==8);
    fwrite(time_stamp, sizeof(char), 8, fout);
    //  45    1   Blank
    //  46    4   Length of Data Record: "****" ???
    //  50    1   Blank
    fwrite(" **** ", sizeof(char), 6, fout);
    //  51    5   # of data recs (observation requests): NNNNN
    //  56    1   Blank
    snprintf(tmp_buf, 32, "%5d ", n_requests);
    assert(strlen(tmp_buf)==6);
    fwrite(tmp_buf, sizeof(char), 6, fout);
    //  57    8   Begin Date of Data (UTC): YYYYMMDD
    //  65    1   Blank
    snprintf(tmp_buf, 32, "%ld ", first_date);
    assert(strlen(tmp_buf)==9);
    fwrite(tmp_buf, sizeof(char), 9, fout);
    //  66    8   End Date of Data (UTC): YYYYMMDD
    //  74    1   Blank
    snprintf(tmp_buf, 32, "%ld ", last_date);
    assert(strlen(tmp_buf)==9);
    fwrite(tmp_buf, sizeof(char), 9, fout);
    //  75    8   File Format Version (date): YYYYMMDD "20051017"
    //  83    1   Blank
    //  84    3   File Format Version (number): VNN "V01"
    //  87    1   Blank
    //  88   39   All Blanks
    const char *s = "20051017 V01                                        ";
    assert(strlen(s)==52);
    fwrite(s, sizeof(char), 52, fout);
    // 127    1   HEX 0A
    tmp_buf[0] = 0x0A;
    fwrite(tmp_buf, sizeof(char), 1, fout);
    // END OF HEADER

    // BEGIN DESCRIPTOR
    // Descriptor scructure:
    //   0   4   PRISM Request Type: "REQW" or "REQE" or "****"
    //   1   4   Blank
    if (n_prism > 0)
        snprintf(tmp_buf, 32, "REQ%c ", req_type);
    else
        strcpy(tmp_buf, "**** ");
    assert(strlen(tmp_buf)==5);
    fwrite(tmp_buf, sizeof(char), 5, fout);
    //   5   5   Number of PRISM Observation requests
    //  10   1   Blank
    snprintf(tmp_buf, 32, "%5d ", n_prism);
    assert(strlen(tmp_buf)==6);
    fwrite(tmp_buf, sizeof(char), 6, fout);
    //  11   4   AVNIR Request Type: "REQW" or "REQE" or "****"
    //  15   1   Blank
    if (n_avnir > 0)
        snprintf(tmp_buf, 32, "REQ%c ", req_type);
    else
        strcpy(tmp_buf, "**** ");
    assert(strlen(tmp_buf)==5);
    fwrite(tmp_buf, sizeof(char), 5, fout);
    //  16   5   Number of AVNIR Observation requests
    //  21   1   Blank
    snprintf(tmp_buf, 32, "%5d ", n_avnir);
    assert(strlen(tmp_buf)==6);
    fwrite(tmp_buf, sizeof(char), 6, fout);
    //  22   4   PALSAR Request Type: "REQW" or "REQE" or "****"
    //  26   1   Blank
    if (n_palsar > 0)
        snprintf(tmp_buf, 32, "REQ%c ", req_type);
    else
        strcpy(tmp_buf, "**** ");
    assert(strlen(tmp_buf)==5);
    fwrite(tmp_buf, sizeof(char), 5, fout);
    //  27   5   Number of PALSAR Observation requests
    snprintf(tmp_buf, 32, "%5d", n_palsar);
    assert(strlen(tmp_buf)==5);
    fwrite(tmp_buf, sizeof(char), 5, fout);
    //  32   1   HEX 0A
    tmp_buf[0] = 0x0A;
    fwrite(tmp_buf, sizeof(char), 1, fout);
    // END DESCRIPTOR

    // now line-by-line in the input file to get the request details.
    // we have to write all the prism lines first, then the avnir lines,
    // and finally the palsar lines, so we will go through the files three
    // times, rather then queueing things up.

    int i, written_prism = 0, written_avnir = 0, written_palsar = 0;
    for (i=0; i<3; ++i) {
        // open up input file again, read header line again
        FILE *fin = fopen(csv_file, "r");
        fgets(line, 1024, fin);
        while (fgets(line, 1024, fin) != NULL) {

            // first read in all of the line's items
            int valid = 
                parse_line(line, sensor, &date, &path, &start_lat, &direction, &duration,
                    observation_mode, &observation_purpose, &prism_nadir_angle,
                    &prism_forward_angle, &prism_backward_angle, &prism_gain_nadir,
                    &prism_gain_forward, &prism_gain_backward, &avnir_pointing_angle,
                    &avnir_gain, &avnir_exposure, &palsar_table_number);
            if (valid) {
                if (i==0 && strcmp(sensor, "PSM")==0) {
                    ++written_prism;
                    write_common(fout, date, path, start_lat, direction, duration,
                                 observation_mode, observation_purpose, *req_id, sensor);
                    write_prism(fout, prism_nadir_angle, prism_forward_angle,
                        prism_backward_angle, prism_gain_nadir, prism_gain_forward,
                        prism_gain_backward);
                    *req_id += 1;
                }
                else if (i==1 && strcmp(sensor, "AV2")==0) {
                    ++written_avnir;
                    write_common(fout, date, path, start_lat, direction, duration,
                                 observation_mode, observation_purpose, *req_id, sensor);
                    write_avnir(fout, avnir_pointing_angle, avnir_gain,
                        avnir_exposure);
                    *req_id += 1;
                }
                else if (i==2 && strcmp(sensor, "PSR")==0) {
                    ++written_palsar;
                    write_common(fout, date, path, start_lat, direction, duration,
                                 observation_mode, observation_purpose, *req_id, sensor);
                    write_palsar(fout, palsar_table_number);
                    *req_id += 1;
                }
            }
            else {
                // we already complained about the invalid line...
                // take no further action other than ignoring it
                ;
            }

        }

        fclose(fin);
    }

    fclose(fout);

    // bunch of sanity checks
    if (written_prism != n_prism)
        printf(" *** Incorrect prism count: %d <-> %d\n", n_prism, written_prism);
    if (written_avnir != n_avnir)
        printf(" *** Incorrect prism count: %d <-> %d\n", n_prism, written_prism);
    if (written_palsar != n_palsar)
        printf(" *** Incorrect prism count: %d <-> %d\n", n_prism, written_prism);
    if (written_prism + written_avnir + written_palsar != n_requests)
        printf(" *** Incorrect total record count: %d+%d+%d != %d\n",
            written_prism, written_avnir, written_palsar, n_requests);

    int check_size = 1;
    if (check_size)
    {
        // ensure output size file was correct
        int expected_size = // header + descrip + per data recs
            128 + 33 + n_prism*88 + n_avnir*78 + n_palsar*67;

        if (expected_size < 32768) {
            char buf[32768];
            FILE *f = fopen(req_file, "rb");
            int bytes = fread(buf, sizeof(char), 32768, f);
            fclose(f);
            if (expected_size == bytes)
                sprintf(buf, "Size verify ok, %d bytes", expected_size);
            else
                sprintf(buf, "Size verify FAIL! Expected: %d, Got: %d",
                    expected_size, bytes);
            put_string_to_label("generate_label", buf);
        } else {
            printf("File too large to verify size.\n");
        }
    }
}

void gui_process(int for_real)
{
    char csv_file[1024];
    get_combo_box_entry_item("csv_dir_combobox", csv_file);    
    if (strlen(csv_file) > 0)
    {
        if (fileExists(csv_file)) {
            printf("Processing: %s\n", csv_file);
            int req_id = settings_get_next_req_id();
            int is_emergency = settings_get_is_emergency();
            char *outfile = get_output_file();
            process(csv_file, outfile, is_emergency, &req_id);
            put_file_in_textview(outfile, "output_textview");
            if (for_real) {
                // update request num, id!
                settings_set_next_req_id_and_incr_req_num(req_id);
                char lbl[256];
                sprintf(lbl, "Generated output file: %s", outfile);
                put_string_to_label("generate_label", lbl);
            } else {
                // delete temp file
                remove(outfile);
            }
        } else {
            message_box("File not found: %s\n", csv_file);
        }
    }
}

SIGNAL_CALLBACK void on_generate_button_clicked(GtkWidget *w)
{
    gui_process(TRUE);
}
