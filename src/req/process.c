#include "req.h"
#include <time.h>
#include <assert.h>
#include <ctype.h>
#include "asf_nan.h"
#include "asf_meta.h"

static int quiet=FALSE;

static void clear_messages()
{
    GtkWidget *tv = get_widget_checked("messages_textview");
    GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));

    if (gtk_text_buffer_get_char_count(tb) > 0)
    {
        GtkTextIter b, e;
        gtk_text_buffer_get_start_iter(tb, &b);
        gtk_text_buffer_get_end_iter(tb, &e);
        gtk_text_buffer_delete(tb, &b, &e);
    }
}

static int messages_len()
{
    GtkWidget *tv = get_widget_checked("messages_textview");
    GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));
    GtkTextIter start, end;
    gtk_text_buffer_get_start_iter(tb, &start);
    gtk_text_buffer_get_end_iter(tb, &end);
    return strlen(gtk_text_buffer_get_text(tb, &start, &end, FALSE));
}

static void show_messages_tab()
{
    GtkWidget *n = get_widget_checked("req_notebook");
    gtk_notebook_set_current_page(GTK_NOTEBOOK(n), 1);
}

static void msg(const char *format, ...)
{
    if (!quiet) {
        char buf[1024];
        va_list ap;
        va_start(ap, format);
        vsprintf(buf, format, ap);
        va_end(ap);

        GtkWidget *tv = get_widget_checked("messages_textview");
        GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));

        GtkTextIter end;
        gtk_text_buffer_get_end_iter(tb, &end);
        gtk_text_buffer_insert(tb, &end, buf, -1);
    }
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
    if (p && strlen(tmp)>0 && !is_all_asterisks(tmp))
        *i = atoi(tmp);
    else
        *i = MAGIC_UNSET_INT;
    return p;
}

static char *my_parse_long(char *p, long *l)
{
    char tmp[64];
    p = my_parse_string(p, tmp, 64);
    if (p && strlen(tmp)>0 && !is_all_asterisks(tmp))
        *l = atol(tmp);
    else
        *l = MAGIC_UNSET_INT;
    return p;
}

static char *my_parse_double(char *p, double *d)
{
    char tmp[64];
    p = my_parse_string(p, tmp, 64);
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
                        char *line, int request_type,
                        // outputs:
                        char *sensor, char *drf, long *date, int *path, double *start_lat,
                        int *direction, int *duration, char *observation_mode,
                        int *observation_purpose, double *prism_nadir_angle,
                        double *prism_forward_angle, double *prism_backward_angle,
                        int *prism_nadir_gain, int *prism_forward_gain,
                        int *prism_backward_gain, double *avnir_pointing_angle,
                        int *avnir_gain, int *avnir_exposure,
                        int *palsar_table_number)
{
    // cheat!  easier to validate if every piece will end with a comma
    line[strlen(line)+1] = '\0';
    line[strlen(line)] = ',';

    // iterator
    char *p = line;

    // some flags to help validation
    int is_prism = 0;
    int is_avnir = 0;
    int is_palsar = 0;

    // sensor
    p = my_parse_string(p, sensor, 256);
    if (!p) return FALSE;
    if (strcmp(sensor, "PSM") == 0)
        is_prism = 1;
    else if (strcmp(sensor, "AV2") == 0)
        is_avnir = 1;
    else if (strcmp(sensor, "PSR") == 0)
        is_palsar = 1;
    else {
        msg("  --> Invalid sensor string: %s\n", sensor);
        return FALSE;
    }
    assert(is_avnir + is_prism + is_palsar == 1);

    // request_type
    char tmp[64];
    p = my_parse_string(p, tmp, 64);
    if (!p) return FALSE;
    const char *t = request_type == OBSERVATION_REQUEST ? 
        "Observation" : "Acquisition";
    if (strcmp_case(tmp, t) != 0) {
        msg("Skipping non-%s Request_Type: %s\n", t, tmp);
        return FALSE;
    }

    // for acquisition requests only -- drf
    if (request_type == ACQUISITION_REQUEST) {
        p = my_parse_string(p, drf, 16);
        if (!p) return FALSE;
        if (strlen(drf)>4) {
            msg("Truncating DRF to 4 characters: %s\n", drf);
            drf[4] = '\0';
        } else if (strlen(drf)<4) {
            while (strlen(drf)<4) {
                drf[strlen(drf)+1] = '\0';
                drf[strlen(drf)] = '_';
            }
        }
    } else {
        if (settings_get_is_aadn())
            strcpy(drf, "AADN");
        else
            strcpy(drf, "TDRS");
        //strcpy(drf, settings_get_station_code());
    }
    assert(strlen(drf)==4);

    // date
    p = my_parse_long(p, date);
    if (!p) return FALSE;
    if (!is_valid_date(*date)) {
        msg("  --> Invalid date: %ld\n", *date);
        return FALSE;
    }
    sprintf(tmp, "%ld", *date);
    if (strlen(tmp) != 8 || tmp[0] != '2') { // need to update this in 2999
        msg("  --> Invalid date: %ld\n", *date);
        return FALSE;
    }

    // path
    p = my_parse_int(p, path);
    if (!p) return FALSE;
    if (*path > 671 || *path < 0) {
        msg("  --> Invalid path number: %d\n", *path);
        return FALSE;
    }

    // start_lat
    p = my_parse_double(p, start_lat);
    if (!p) return FALSE;

    // direction
    p = my_parse_int(p, direction);
    if (!p) return FALSE;
    if (*direction != 1 && *direction != 0) {
        msg("  --> Invalid direction: %d\n", *direction);
        return FALSE;
    }

    // duration
    p = my_parse_int(p, duration);
    if (!p) return FALSE;

    // observation mode
    p = my_parse_string(p, observation_mode, 256);
    if (!p) return FALSE;
    if (strlen(observation_mode) != 3) {
        msg("  -> Invalid observation mode: %s\n", observation_mode);
        return FALSE;
    }
 
    // observation purpose
    p = my_parse_int(p, observation_purpose);
    if (!p) return FALSE;
    if (*observation_purpose < 1 || *observation_purpose > 6) {
        msg("  --> Invalid observation purpose: %d\n",
            *observation_purpose);
        return FALSE;
    }

    // prism nadir angle
    p = my_parse_double(p, prism_nadir_angle);
    if (!p) return FALSE;
    if (meta_is_valid_double(*prism_nadir_angle) && !is_prism) {
        msg("  --> prism nadir angle set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && !valid_angle(*prism_nadir_angle, -1.5, 1.5)) {
        msg("  --> prism nadir angle appears invalid: %g\n", *prism_nadir_angle);
        return FALSE;
    }

    // prism forward angle
    p = my_parse_double(p, prism_forward_angle);
    if (!p) return FALSE;
    if (meta_is_valid_double(*prism_forward_angle) && !is_prism) {
        msg("  --> prism forward angle set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && !valid_angle(*prism_forward_angle, -1.5, 1.5)) {
        msg("  --> prism forward angle appears invalid: %g\n", *prism_forward_angle);
        return FALSE;
    }

    // prism backward angle
    p = my_parse_double(p, prism_backward_angle);
    if (!p) return FALSE;
    if (meta_is_valid_double(*prism_backward_angle) && !is_prism) {
        msg("  --> prism backward angle set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && !valid_angle(*prism_backward_angle, -1.5, 1.5)) {
        msg("  --> prism backward angle appears invalid: %g\n", *prism_backward_angle);
        return FALSE;
    }

    // prism nadir gain
    p = my_parse_int(p, prism_nadir_gain);
    if (!p) return FALSE;
    if (*prism_nadir_gain != MAGIC_UNSET_INT && !is_prism) {
        msg("  --> prism nadir gain set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && *prism_nadir_gain != MAGIC_UNSET_INT &&
            (*prism_nadir_gain < 1 || *prism_nadir_gain > 4)) {
        msg("  --> prism nadir invalid: %d\n", *prism_nadir_gain);
        return FALSE;
    }

    // prism forward gain
    p = my_parse_int(p, prism_forward_gain);
    if (!p) return FALSE;
    if (*prism_forward_gain != MAGIC_UNSET_INT && !is_prism) {
        msg("  --> prism forward gain set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && *prism_forward_gain != MAGIC_UNSET_INT &&
            (*prism_forward_gain < 1 || *prism_forward_gain > 4)) {
        msg("  --> prism forward invalid: %d\n", *prism_forward_gain);
        return FALSE;
    }

    // prism backward gain
    p = my_parse_int(p, prism_backward_gain);
    if (!p) return FALSE;
    if (*prism_backward_gain != MAGIC_UNSET_INT && !is_prism) {
        msg("  --> prism backward gain set for non-prism data\n");
        return FALSE;
    }
    if (is_prism && *prism_backward_gain != MAGIC_UNSET_INT &&
            (*prism_backward_gain < 1 || *prism_backward_gain > 4)) {
        msg("  --> prism backward invalid: %d\n", *prism_backward_gain);
        return FALSE;
    }

    // avnir pointing angle
    p = my_parse_double(p, avnir_pointing_angle);
    if (!p) return FALSE;
    if (meta_is_valid_double(*avnir_pointing_angle) && !is_avnir) {
        msg("  --> avnir pointing angle set for non-avnir data\n");
        return FALSE;
    }
    if (!valid_angle(*avnir_pointing_angle, -44, 44)) {
        msg("  --> avnir pointing angle appears invalid: %g\n", *avnir_pointing_angle);
        return FALSE;
    }
    if (!valid_avnir_pointing_angle(*avnir_pointing_angle)) {
        msg("  --> avnir pointing angle appears invalid: %g\n", *avnir_pointing_angle);
        msg("       (+/- 0.02 steps is absolutely specified)\n");
        return FALSE;
    }

    // avnir gain
    p = my_parse_int(p, avnir_gain);
    if (!p) return FALSE;
    if (*avnir_gain != MAGIC_UNSET_INT && !is_avnir) {
        msg("  --> avnir gain set for non-avnir data\n");
        return FALSE;
    }
    if (is_avnir && *avnir_gain != MAGIC_UNSET_INT &&
            (*avnir_gain < 1 || *avnir_gain > 4)) {
        msg("  --> avnir gain invalid: %d\n", *avnir_gain);
        return FALSE;
    }

    // anvir exposure
    p = my_parse_int(p, avnir_exposure);
    if (!p) return FALSE;
    if (*avnir_exposure != MAGIC_UNSET_INT && !is_avnir) {
        msg("  --> avnir exposure set for non-avnir data\n");
        return FALSE;
    }

    // palsar table number
    p = my_parse_int(p, palsar_table_number);
    if (!p) return FALSE;
    if (*palsar_table_number != MAGIC_UNSET_INT && !is_palsar) {
        msg("  --> palsar table number set (%d) for non-palsar data\n",
            *palsar_table_number);
        return FALSE;
    }
    if (is_palsar && *palsar_table_number != MAGIC_UNSET_INT &&
            (*palsar_table_number > 999 || *palsar_table_number < 1)) {
        msg("  --> palsar table number is invalid: %d\n", *palsar_table_number);
        return FALSE;
    }

    // end if the line!
    if (*p != '\0') // non-fatal error
        msg("  --> Line has extra characters\n");

    return TRUE;
}

static void write_common(FILE *fout, long date, int path, double start_lat,
                        int direction, int duration, char *observation_mode,
                        int observation_purpose, int req_id, char *sensor,
                        const char *station_code, int request_type)
{
    // Acquisition and Observation requests have the same format
    // up until byte 54, where Acq has some different stuff inserted in

    char tmp_buf[32];
    //   0   16  Observation Request ID
    //  16    1  Blank
    snprintf(tmp_buf, 32, "%s%011dD ", station_code, req_id);
    assert(strlen(tmp_buf)==17);
    fwrite(tmp_buf, sizeof(char), 17, fout);
    //  17    2  Observation Request ID Branch Number
    //  19    1  Blank
    fwrite("** ", sizeof(char), 3, fout);
    //  20    3  Sensor Name: "PSM"
    //  23    1  Blank
    snprintf(tmp_buf, 32, "%s ", sensor);
    assert(strlen(tmp_buf)==4);
    fwrite(tmp_buf, sizeof(char), 4, fout);

    if (request_type == OBSERVATION_REQUEST) {
        //  24    8  Observation Date (UTC)
        //  32    1  Blank
        snprintf(tmp_buf, 32, "%ld ", date);
        assert(strlen(tmp_buf)==9);
        fwrite(tmp_buf, sizeof(char), 9, fout);

        //  33    5  Path Number
        //  38    1  Blank
        snprintf(tmp_buf, 32, "%5d ", path);
        assert(strlen(tmp_buf)==6);
        fwrite(tmp_buf, sizeof(char), 6, fout);

        //  39    7  Observation Beginning Latitude
        //  46    1  Blank
        snprintf(tmp_buf, 32, "%7.3f ", start_lat);
        assert(strlen(tmp_buf)==8);
        fwrite(tmp_buf, sizeof(char), 8, fout);

        //  47    1  Asc/Desc-ending Flag
        //  48    1  Blank
        snprintf(tmp_buf, 32, "%1d ", direction);
        assert(strlen(tmp_buf)==2);
        fwrite(tmp_buf, sizeof(char), 2, fout);        

        //  49    4  Observation Time
        //  53    1  Blank
        snprintf(tmp_buf, 32, "%4d ", duration);
        assert(strlen(tmp_buf)==5);
        fwrite(tmp_buf, sizeof(char), 5, fout);
    } else {
        //  fields 7,9,11,13,15 are all *'s for r-t acq requests
        char *s = "******** ***** ******* * **** ";
        assert(strlen(s)==9+6+8+2+5);
        fwrite(s, sizeof(char), 30, fout);
    }

    if (request_type == ACQUISITION_REQUEST) {
        //  54    4  Acquisition Mode ("REAL")
        //  58    1  Blank
        fwrite("REAL ", sizeof(char), 5, fout);

        //  59    8  Acquisition Date (UTC) YYYYMMDD
        //  67    1  Blank
        snprintf(tmp_buf, 32, "%ld ", date);
        assert(strlen(tmp_buf)==9);
        fwrite(tmp_buf, sizeof(char), 9, fout);

        //  68    5  Acquisition Path Number
        //  73    1  Blank
        snprintf(tmp_buf, 32, "%5d ", path);
        assert(strlen(tmp_buf)==6);
        fwrite(tmp_buf, sizeof(char), 6, fout);

        //  74    7  Acquisition Start Latitude
        //  81    1  Blank
        snprintf(tmp_buf, 32, "%7.3f ", start_lat);
        assert(strlen(tmp_buf)==8);
        fwrite(tmp_buf, sizeof(char), 8, fout);
        
        //  82    1  Acquisition A/D: 0=asc, 1=desc.
        //  83    1  Blank
        snprintf(tmp_buf, 32, "%1d ", direction);
        assert(strlen(tmp_buf)==2);
        fwrite(tmp_buf, sizeof(char), 2, fout);        

        //  84    4  Acquisition Time (NNNN)
        //  88    1  Blank
        snprintf(tmp_buf, 32, "%4d ", duration);
        assert(strlen(tmp_buf)==5);
        fwrite(tmp_buf, sizeof(char), 5, fout);
    } else {
        // obs requests don't have the above fields at all
    }

    //  54    3  Operation Mode (acq:  89 3)
    //  57    1  Blank                 92 1)
    snprintf(tmp_buf, 32, "%s ", observation_mode);
    assert(strlen(tmp_buf)==4);
    fwrite(tmp_buf, sizeof(char), 4, fout);
    //  58    2  Observation Purpose (acq:  93 2)
    //  60    1  Blank                      95 1)
    snprintf(tmp_buf, 32, "%2d ", observation_purpose);
    assert(strlen(tmp_buf)==3);
    fwrite(tmp_buf, sizeof(char), 3, fout);
}

static void write_prism(FILE *fout,
                        double prism_nadir_angle, double prism_forward_angle,
                        double prism_backward_angle, int prism_nadir_gain,
                        int prism_forward_gain, int prism_backward_gain,
                        int direction, int request_type)
{
    char tmp_buf[32];

    if (request_type==OBSERVATION_REQUEST) {
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
    } else {
        assert(request_type==ACQUISITION_REQUEST);
        //  96    1   Compression Mode
        //  97    1   Blank
        fwrite("2 ", sizeof(char), 2, fout);

        //  98    2   Number of Slot Information
        fwrite(" 1", sizeof(char), 2, fout);
    }

    //  87    1  HEX 0A
    tmp_buf[0] = 0x0A;
    fwrite(tmp_buf, sizeof(char), 1, fout);

    // now write the slot info
    if (request_type==ACQUISITION_REQUEST) {
        //   0    7  Slot Change Begin Latitude
        //           "*" is set in the case of first slot
        //   7    1  Blank
        fwrite("******* ", sizeof(char), 8, fout);

        //   8    1  Ascending/Descending
        //   9    1  Blank
        snprintf(tmp_buf, 32, "%1d ", direction);
        assert(strlen(tmp_buf)==2);
        fwrite(tmp_buf, sizeof(char), 2, fout);        

        //  10    6  Angle Of Nadir View Image
        //  11    1  Blank
        if (meta_is_valid_double(prism_nadir_angle))
            snprintf(tmp_buf, 32, "%6.2f ", prism_nadir_angle);
        else
            strcpy(tmp_buf, "****** ");
        assert(strlen(tmp_buf)==7);
        fwrite(tmp_buf, sizeof(char), 7, fout);

        //  17    6  Angle Of Forward View Image
        //  23    1  Blank
        if (meta_is_valid_double(prism_forward_angle))
            snprintf(tmp_buf, 32, "%6.2f ", prism_forward_angle);
        else
            strcpy(tmp_buf, "****** ");
        assert(strlen(tmp_buf)==7);
        fwrite(tmp_buf, sizeof(char), 7, fout);

        //  24    6  Angle Of Backward View Image
        //  30    1  Blank
        if (meta_is_valid_double(prism_backward_angle))
            snprintf(tmp_buf, 32, "%6.2f ", prism_backward_angle);
        else
            strcpy(tmp_buf, "****** ");
        assert(strlen(tmp_buf)==7);
        fwrite(tmp_buf, sizeof(char), 7, fout);

        //  31    1  Gain (Nadir)
        //  32    1  Blank
        if (prism_nadir_gain == MAGIC_UNSET_INT)
            strcpy(tmp_buf, "* ");
        else
            snprintf(tmp_buf, 32, "%d ", prism_nadir_gain);
        assert(strlen(tmp_buf)==2);
        fwrite(tmp_buf, sizeof(char), 2, fout);

        //  33    1  Gain (Forward)
        //  34    1  Blank
        if (prism_forward_gain == MAGIC_UNSET_INT)
            strcpy(tmp_buf, "* ");
        else
            snprintf(tmp_buf, 32, "%d ", prism_forward_gain);
        assert(strlen(tmp_buf)==2);
        fwrite(tmp_buf, sizeof(char), 2, fout);

        //  35    1  Gain (Backward)
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
}

static void write_avnir(FILE *fout, double avnir_pointing_angle,
                        int avnir_gain, int avnir_exposure,
                        int direction, int request_type)
{
    char tmp_buf[32];

    if (request_type==OBSERVATION_REQUEST) {
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
    } else {
        assert(request_type==ACQUISITION_REQUEST);
        //  96    2   Number of Slot Information
        fwrite(" 1", sizeof(char), 2, fout);
    }
    //  77    1  HEX 0A   (Acq:   98   1)
    tmp_buf[0] = 0x0A;
    fwrite(tmp_buf, sizeof(char), 1, fout);

    // now write the slot info
    if (request_type==ACQUISITION_REQUEST) {
        //   0    7  Slot Change Begin Latitude
        //           "*" is set in the case of first slot
        //   7    1  Blank
        fwrite("******* ", sizeof(char), 8, fout);

        //   8    1  Ascending/Descending
        //   9    1  Blank
        snprintf(tmp_buf, 32, "%1d ", direction);
        assert(strlen(tmp_buf)==2);
        fwrite(tmp_buf, sizeof(char), 2, fout);        

        //  10    6  Pointing Angle (degrees)
        //  16    1  Blank
        if (meta_is_valid_double(avnir_pointing_angle))
            snprintf(tmp_buf, 32, "%6.2f ", avnir_pointing_angle);
        else
            strcpy(tmp_buf, "****** ");
        assert(strlen(tmp_buf)==7);
        fwrite(tmp_buf, sizeof(char), 7, fout);
        //  17    4  Gain Status
        //  21    1  Blank
        if (avnir_gain == MAGIC_UNSET_INT)
            strcpy(tmp_buf, "**** ");
        else
            snprintf(tmp_buf, 32, "%4d ", avnir_gain);
        assert(strlen(tmp_buf)==5);
        fwrite(tmp_buf, sizeof(char), 5, fout);
        //  22    4  Exposure Status
        if (avnir_exposure == MAGIC_UNSET_INT)
            strcpy(tmp_buf, "****");
        else
            snprintf(tmp_buf, 32, "%4d", avnir_exposure);
        assert(strlen(tmp_buf)==4);
        fwrite(tmp_buf, sizeof(char), 4, fout);

        //  26    1  HEX 0A
        tmp_buf[0] = 0x0A;
        fwrite(tmp_buf, sizeof(char), 1, fout);
    }
}

static void write_palsar(FILE *fout, int palsar_table_number, int request_type)
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

static void strip_end_whitesp(char *s)
{
    char *p = s + strlen(s) - 1;
    while (isspace(*p) && p>s)
        *p-- = '\0';
}

static char *fix_underscores(char *s)
{
    static char ret[64];
    strcpy(ret, s);

    int i;
    for (i=0; i<strlen(ret); ++i)
        if (ret[i]=='_') ret[i]=' ';
    return ret;
}

static
void acq_obs_process(FILE *fin, const char *csv_file, const char *req_file,
                     int is_emergency, int *req_id,
                     long start_date_user, long end_date_user,
                     const char *type_code, int request_type,
                     char **drf_ret)
{
    // temporary spot for the read line
    char line[1024];

    // where we will put all the above, when processing a line
    char sensor[256];
    char drf[16], drf2[16];
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

    // request code should be "REQ" or "RQT"
    assert(strlen(type_code)==3);
    if (request_type==OBSERVATION_REQUEST)
        assert(strcmp(type_code, "REQ")==0);
    else if (request_type==ACQUISITION_REQUEST)
        assert(strcmp(type_code, "RQT")==0);
    else
        assert(0);

    char req_type = is_emergency ? 'E' : 'W';

    // read in the whole file, so we can determine the number of requests, the
    // start date, and the end date.
    long first_date, last_date;
    int n_requests = 0;
    int n_prism = 0, n_avnir = 0, n_palsar = 0;
    int line_no = 2; // start at 2, since we already read 1 line.
    while (fgets(line, 1024, fin) != NULL) {
        strip_end_whitesp(line);

        int valid =
            parse_line(line, request_type, sensor, drf2, &date, &path,
                &start_lat, &direction, &duration, observation_mode, 
                &observation_purpose, &prism_nadir_angle,
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
            if (strcmp(sensor, "PSM")==0)
                ++n_prism;
            else if (strcmp(sensor, "AV2")==0)
                ++n_avnir;
            else if (strcmp(sensor, "PSR")==0)
                ++n_palsar;
            else
                assert(0); // was supposed to check for this above
        } else {
            msg("Invalid line %d in CSV file.\n", line_no);
        }

        if (n_requests == 1) {
            // first line is THE drf.
            strcpy(drf, drf2);
            if (drf_ret) { // return DRF to caller, if requested
                *drf_ret = MALLOC(sizeof(char)*10);
                strcpy(*drf_ret, drf2);
                *req_id = settings_get_next_req_id(request_type, drf);
                printf("For DRF %s, next request ID is: %d\n", drf, *req_id);
            }
        } else if (n_requests > 1) {
            // ensure all DRF values in the file are the same
            if (strcmp(drf, drf2) != 0) {
                msg("DRF value on line %d doesn't match the first: %s\n",
                    line_no, drf2);
            }
        } else {
            // First line must have been invalid.
            // Don't need to do anything, will initialize drf with the first
            // valid line that we find.
            ;
        }

        ++line_no;
    }

    fclose(fin);

    // now, start working on the request file.
    // first, get current date/time, to stamp the request file.
    time_t t;
    t = time(NULL);
    struct tm *ts = gmtime(&t);
    char time_stamp[10];
    strftime(time_stamp, 10, "%H:%I:%S", ts);
    char date_stamp[10];
    strftime(date_stamp, 10, "%Y%m%d", ts);

    if (n_requests == 0) {
        msg("No requests found!\n");
        strcpy(drf, "");
        first_date = last_date =
            date_to_long(ts->tm_year+1900, ts->tm_mon+1, ts->tm_mday);
    } else {
        printf("Found %d request%s, range: %ld -> %ld\n",
            n_requests, n_requests==1 ? "" : "s",
            first_date, last_date);
    }

    // open output file
    FILE *fout = fopen(req_file, "wb");
    if (!fout) {
        char buf[1024];
        sprintf(buf, "Failed to open output file: %s\n", req_file);
        put_string_in_textview("output_textview", buf);
        fclose(fout);
        return;
    }

    // see if the user specified an earlier/later start/end date
    if (start_date_user > 0) {
        if (start_date_user > first_date)
            alert("The start date is later than some of the request dates!");
        if (start_date_user > 99991231) {
            alert("The start date is invalid, larger than maximum allowed.");
            start_date_user = 99991231;
        }
        if (is_valid_date(start_date_user))
            first_date = start_date_user;
    }
    else {
        // back up to the previous MONDAY
        int n_days_back=0;
        while (get_day_of_week(first_date) > 1) {
            first_date=subtract_a_day(first_date);
            ++n_days_back;
        }
        if (n_days_back > 0)
            printf("Backed up %d day%s to previous Monday.\n", n_days_back,
                n_days_back==1 ? "" : "s");
    }

    if (end_date_user > 0) {
        if (end_date_user < last_date)
            alert("The end date is earlier than some of the request dates!");
        if (end_date_user > 99991231) {
            alert("The end date is invalid, larger than maximum allowed.");
            end_date_user = 99991231;
        }
        if (is_valid_date(end_date_user))
            last_date = end_date_user;
    }
    else {
        // ahead to the next SUNDAY
        int n_days_ahead=0;
        while (get_day_of_week(last_date) > 0) {
            last_date=add_a_day(last_date);
            ++n_days_ahead;
        }
        if (n_days_ahead > 0)
            printf("Moved ahead %d day%s to next Sunday.\n", n_days_ahead,
                n_days_ahead == 1 ? "" : "s");
    }

    if (first_date > last_date) {
        alert("Start date must be earlier than the end date.");
        first_date = last_date;
    }

    // write output file header lines
    char tmp_buf[32];

    // Header Structure:
    //   0   10   File Name: REQxnnnnnn (or RQTxnnnnnn)
    char *basename = get_basename(req_file);
    assert(strlen(basename)==10);
    fwrite(basename, sizeof(char), 10, fout);
    free(basename);
    //  10    1   Blank
    //  11    6   Fixed string: "ALOS  "
    //  17    1   Blank
    fwrite(" ALOS   ", sizeof(char), 8, fout);
    //  18    4   Station Code (from): e.g., "AADN"
    //  22    1   Blank
    //  23    4   EOC MMO Code (to): "HMMO"
    //  27    1   Blank
    snprintf(tmp_buf, 32, "%s HMMO ", fix_underscores(drf));
    assert(strlen(tmp_buf)==10);
    fwrite(tmp_buf, sizeof(char), 10, fout);
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
    //  51    5   # of data recs (obs/acq requests): NNNNN
    //  56    1   Blank
    snprintf(tmp_buf, 32, "%5d ", n_requests);
    assert(strlen(tmp_buf)==6);
    fwrite(tmp_buf, sizeof(char), 6, fout);
    //  57    8   Begin Date of Data (UTC): YYYYMMDD
    //  65    1   Blank
    snprintf(tmp_buf, 32, "%8ld ", first_date);
    assert(strlen(tmp_buf)==9);
    fwrite(tmp_buf, sizeof(char), 9, fout);
    //  66    8   End Date of Data (UTC): YYYYMMDD
    //  74    1   Blank
    snprintf(tmp_buf, 32, "%8ld ", last_date);
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
        snprintf(tmp_buf, 32, "%s%c ", type_code, req_type);
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
        snprintf(tmp_buf, 32, "%s%c ", type_code, req_type);
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
        snprintf(tmp_buf, 32, "%s%c ", type_code, req_type);
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

    // NOTE: The old script numbered the requests starting with the Avnir
    // data, even though these were the second set of items in the file
    // (after the Prism stuff), so we will do the same.  This is the
    // reason for the futzing around with the request ID, and the
    // "first_avnir" and "first_palsar" flags.
    *req_id += n_avnir;
    int first_avnir=TRUE, first_palsar=TRUE;

    // now run in "quiet" mode -- don't report errors, because we will
    // have already issued the messages on the first pass
    quiet=TRUE;

    int i, written_prism=0, written_avnir=0, written_palsar=0;
    for (i=0; i<3; ++i) {
        // open up input file again, read header line again
        FILE *fin = fopen(csv_file, "r");
        fgets(line, 1024, fin);
        while (fgets(line, 1024, fin) != NULL) {
            strip_end_whitesp(line);

            // first read in all of the line's items
            int valid =
                parse_line(line, request_type, sensor, drf, &date, &path,
                    &start_lat, &direction, &duration, observation_mode,
                    &observation_purpose, &prism_nadir_angle,
                    &prism_forward_angle, &prism_backward_angle, &prism_gain_nadir,
                    &prism_gain_forward, &prism_gain_backward, &avnir_pointing_angle,
                    &avnir_gain, &avnir_exposure, &palsar_table_number);

            if (valid) {
                if (i==0 && strcmp(sensor, "PSM")==0) {
                    ++written_prism;
                    write_common(fout, date, path, start_lat, direction, duration,
                                 observation_mode, observation_purpose, *req_id, sensor,
                                 drf, request_type);
                    write_prism(fout, prism_nadir_angle, prism_forward_angle,
                        prism_backward_angle, prism_gain_nadir, prism_gain_forward,
                        prism_gain_backward, direction, request_type);
                    *req_id += 1;
                }
                else if (i==1 && strcmp(sensor, "AV2")==0) {
                    ++written_avnir;
                    if (first_avnir) {
                        *req_id -= n_prism + n_avnir;
                        first_avnir = FALSE;
                    }
                    write_common(fout, date, path, start_lat, direction, duration,
                                 observation_mode, observation_purpose, *req_id, sensor,
                                 drf, request_type);
                    write_avnir(fout, avnir_pointing_angle, avnir_gain,
                        avnir_exposure, direction, request_type);
                    *req_id += 1;
                }
                else if (i==2 && strcmp(sensor, "PSR")==0) {
                    ++written_palsar;
                    if (first_palsar) {
                        *req_id += n_prism;
                        first_palsar = FALSE;
                    }
                    write_common(fout, date, path, start_lat, direction, duration,
                                 observation_mode, observation_purpose, *req_id, sensor,
                                 drf, request_type);
                    write_palsar(fout, palsar_table_number, request_type);
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
    quiet=FALSE;

    // bunch of sanity checks
    if (written_prism != n_prism)
        msg(" *** Incorrect prism count: %d <-> %d\n", n_prism, written_prism);
    if (written_avnir != n_avnir)
        msg(" *** Incorrect avnir count: %d <-> %d\n", n_avnir, written_avnir);
    if (written_palsar != n_palsar)
        msg(" *** Incorrect palsar count: %d <-> %d\n", n_palsar, written_palsar);
    if (written_prism + written_avnir + written_palsar != n_requests)
        msg(" *** Incorrect total record count: %d+%d+%d != %d\n",
            written_prism, written_avnir, written_palsar, n_requests);

    int check_size = 1;
    if (check_size)
    {
        // ensure output size file was correct
        int expected_size;
        
        if (request_type == OBSERVATION_REQUEST)
            expected_size = // header + descrip + per data recs
                128 + 33 + n_prism*88 + n_avnir*78 + n_palsar*67;
        else if (request_type == ACQUISITION_REQUEST)
            expected_size =
                128 + 33 + // header + descrip
                    n_prism*(101 + 37) + // regular + slot
                    n_avnir*(99 + 27) + // regular + slot
                    n_palsar*102; // no slot rec
        else {
            // impossible!!
            msg("Bad request type: %d\n", request_type);
            assert(0);
        }

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
            msg("File too large to verify size.\n");
        }
    }

    if (start_date_user < 0)
        settings_set_start_date(first_date);
    if (end_date_user < 0)
        settings_set_end_date(last_date);
}

void obs_process(FILE *fin, const char *csv_file, const char *req_file,
                 int is_emergency, int *req_id, long start_date_user,
                 long end_date_user)
{
    acq_obs_process(fin, csv_file, req_file, is_emergency, req_id,
        start_date_user, end_date_user, "REQ", OBSERVATION_REQUEST, NULL);
}

void acq_process(FILE *fin, const char *csv_file, const char *req_file,
                 int is_emergency, int *req_id, long start_date_user,
                 long end_date_user, char **drf)
{
    acq_obs_process(fin, csv_file, req_file, is_emergency, req_id,
        start_date_user, end_date_user, "RQT", ACQUISITION_REQUEST, drf);
}

int odl0_parse_line(char *line, char *downlink_segment_number,
                    char *l0_data_code)
{
    // cheat!  easier to validate if every piece will end with a comma
    line[strlen(line)+1] = '\0';
    line[strlen(line)] = ',';

    // iterator
    char *p = line;

    // downlink segment number
    p = my_parse_string(p, downlink_segment_number, 256);
    if (!p) return FALSE;
    if (strlen(downlink_segment_number)!=14) {
        msg(
            "  --> Invalid length of Downlink Segment No. (should be 14)\n"
            "      Downlink Segment No.: %s (length=%d)\n",
            downlink_segment_number, strlen(downlink_segment_number));
        return FALSE;
    }

    // L0 Data Code
    p = my_parse_string(p, l0_data_code, 256);
    if (!p) return FALSE;
    if (strlen(l0_data_code)!=4) {
        msg(
            "  --> Invalid length of Level 0 Data Code (should be 4)\n"
            "      Level 0 Data Code: %s (length=%d)\n",
            l0_data_code, strlen(l0_data_code));
        return FALSE;
    }

    // end if the line!
    if (*p != '\0') // non-fatal error
        msg("  --> Line has extra characters\n");

    return TRUE;
}

static void write_odl0(FILE *fout, char *downlink_segment_number,
                       char *l0_data_code)
{
    char tmp_buf[64];
    int i;

    snprintf(tmp_buf, 64, "%s,%s", downlink_segment_number, l0_data_code);
    assert(strlen(tmp_buf)==19);

    // pad the rest with blanks (length will be 40 characters)
    for (i=strlen(tmp_buf); i<40; ++i)
        tmp_buf[i] = ' ';
    tmp_buf[40] = '\0';
    assert(strlen(tmp_buf)==40);
    fwrite(tmp_buf, sizeof(char), 40, fout);

    // write record separator
    tmp_buf[0] = 0x0A;
    fwrite(tmp_buf, sizeof(char), 1, fout);
}

void odl0_process(FILE *fin, const char *csv_file, const char *req_file,
                 int is_emergency, int *req_id, long start_date_user,
                 long end_date_user)
{
    // temporary spot for the read line
    char line[1024];

    // what's in the csv file
    char downlink_segment_number[256];
    char l0_data_code[256];

    // figure out the # of requests in the file
    int n_requests = 0;
    int line_no = 2; // since we already read 1 line
    while (fgets(line, 1024, fin) != NULL) {
        strip_end_whitesp(line);

        int valid = odl0_parse_line(line, downlink_segment_number,
            l0_data_code);

        if (valid)
            ++n_requests;
        else
            msg("Invalid line %d in CSV file.\n", line_no);

        ++line_no;
    }
    fclose(fin);

    printf("Found %d requests.\n", n_requests);

    // now, start working on the request file.
    // first, get current date/time, to stamp the request file.
    time_t t;
    t = time(NULL);
    char time_stamp[10];
    strftime(time_stamp, 10, "%H:%I:%S", gmtime(&t));
    char date_stamp[10];
    strftime(date_stamp, 10, "%Y%m%d", gmtime(&t));

    // open output file
    FILE *fout = fopen(req_file, "wb");
    if (!fout) {
        char buf[1024];
        sprintf(buf, "Failed to open output file: %s\n", req_file);
        put_string_in_textview("output_textview", buf);
        fclose(fout);
        return;
    }

    // write output file header lines
    char tmp_buf[32];

    // Header Structure:
    //   0   10   File Name: REQxnnnnnn (or RQTxnnnnnn)
    char *basename = get_basename(req_file);
    assert(strlen(basename)==10);
    fwrite(basename, sizeof(char), 10, fout);
    free(basename);
    //  10    1   Blank
    //  11    6   Fixed string: "ALOS  "
    //  17    1   Blank
    fwrite(" ALOS   ", sizeof(char), 8, fout);
    //  18    4   Station Code (from): e.g., "AADN"
    //  22    1   Blank
    //  23    4   EOC MMO Code (to): "HCNT" (fixed)
    //  27    1   Blank
    snprintf(tmp_buf, 32, "%s HCNT ", settings_get_station_code());
    assert(strlen(tmp_buf)==10);
    fwrite(tmp_buf, sizeof(char), 10, fout);
    //  28    8   File Creation Date (UTC): YYYMMDD
    assert(strlen(date_stamp)==8);
    fwrite(date_stamp, sizeof(char), 8, fout);
    //  36    1   Blank
    fwrite(" ", sizeof(char), 1, fout);
    //  37    8   File Creation Time (UTC): hh:mm:ss
    assert(strlen(time_stamp)==8);
    fwrite(time_stamp, sizeof(char), 8, fout);
    //  45    1   Blank
    //  46    4   Length of Data Section: "  41" (fixed)
    //  50    1   Blank
    fwrite("   41 ", sizeof(char), 6, fout);
    //  51    5   # of data recs: " NNNN"
    //  56    1   Blank
    snprintf(tmp_buf, 32, " %4d ", n_requests);
    assert(strlen(tmp_buf)==6);
    fwrite(tmp_buf, sizeof(char), 6, fout);
    //  57    8   Stating Date of Data Valid Period: "********" fixed
    //  65    1   Blank
    //  66    8   Ending Date of Data Valid Peroid: "********" fixed
    //  74    1   Blank
    //  75    8   File Format Version (YYYYMMDD)
    //  83    1   Blank
    //  84    3   File Format Version (Vnn)
    const char *c1 = "******** ******** 20061024 V01";
    assert(strlen(c1)==30);
    fwrite(c1, sizeof(char), 30, fout);
    //  87    1   Blank
    //  88   39   All blanks
    const char *c2 = "                                        ";
    assert(strlen(c2)==40);
    fwrite(c2, sizeof(char), 40, fout);
    // 127    1   HEX 0A
    tmp_buf[0] = 0x0A;
    fwrite(tmp_buf, sizeof(char), 1, fout);
    // END OF HEADER
    
    // BEGIN DESCRIPTOR
    //   0   16   Data Distribution Request ID
    //            Arbitrary, we choose: "AADNnnnnnnnnnnnn" (nnnnnnnnnnn=req #)
    //  16    1   Blank
    snprintf(tmp_buf, 32, "AADN%012d ", *req_id);
    assert(strlen(tmp_buf)==17);
    fwrite(tmp_buf, sizeof(char), 17, fout);
    //  17    8   Data Request Type: "ALL     "
    //  25    1   Blank
    //  26    2   Data Request Code: "CD"
    //  28    1   Blank
    //  29    5   Distribution Type: "MD1  "
    //  34    1   Blank
    //  35    4   Destination Agency: "AADN"
    const char *c3 = "ALL      CD MD1   AADN";
    assert(strlen(c3)==22);
    fwrite(c3, sizeof(char), 22, fout);
    //  39    1   HEX 0A
    tmp_buf[0] = 0x0A;
    fwrite(tmp_buf, sizeof(char), 1, fout);
    // END DESCRIPTOR

    // Go through the input file again
    fin = fopen(csv_file, "r");
    fgets(line, 1024, fin); // skip header line
    while (fgets(line, 1024, fin) != NULL) {
        strip_end_whitesp(line);
        int valid = odl0_parse_line(line, downlink_segment_number,
            l0_data_code);
        if (valid) {
            write_odl0(fout, downlink_segment_number, l0_data_code);
        } else {
            // already complained about this in the first pass through,
            // can silently skip this time
            ;
        }
    }
    fclose(fin);
    fclose(fout);

    int check_size=1;
    if (check_size) {
        // ensure output size file was correct
        int expected_size;
        
        expected_size = // header + descrip + per data recs
            128 + 40 + n_requests*41;

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
            msg("File too large to verify size.\n");
        }
    }

    *req_id += 1;
}

void gui_process(int for_real)
{
    if (block_processing)
        return;

    clear_messages();
    char csv_file[1024], buf[1024];
    get_combo_box_entry_item("csv_dir_combobox", csv_file);    
    if (strlen(csv_file) > 0) {
        if (fileExists(csv_file)) {
            char *ext = findExt(csv_file);
            if (!ext || strcmp_case(ext, ".csv")!=0) {
                sprintf(buf, "File doesn't have a csv extension: %s\n", csv_file);
                put_string_in_textview("output_textview", buf);
            } else {
                int req_id, request_type;
                char *drf=NULL;
                char *outfile =
                    process(csv_file, settings_get_is_emergency(),
                        &req_id, &request_type, &drf,
                        settings_get_start_date(), settings_get_end_date());
                if (outfile) {
                    put_file_in_textview(outfile, "output_textview");
                    if (for_real) {
                        // update request num, id!
                        settings_set_next_req_id_and_incr_req_num(req_id, request_type, drf);
                        sprintf(buf, "Generated output file: %s", outfile);
                        put_string_to_label("generate_label", buf);
                    } else {
                        // delete temp file
                        remove(outfile);
                    }
                    free(outfile);
                } else {
                    // an error should already have been put into
                    // the textview - no action necessary

                }
                if (drf)
                    free(drf);
            }
        } else {
            sprintf(buf, "File not found: %s\n", csv_file);
            put_string_in_textview("output_textview", buf);
        }
    }

    if (messages_len() > 0)
        show_messages_tab();
}

SIGNAL_CALLBACK void on_generate_button_clicked(GtkWidget *w)
{
    gui_process(TRUE);
}

static char *update_request_type(int request_type)
{
    settings_set_request_type(request_type);
    update_output_file();
    return get_output_file();
}

char *process(const char *csv_file, int is_emergency,
              int *req_id, int *request_type, char **drf,
              long start_date_user, long end_date_user)
{
    printf("Process> %s\n", csv_file);

    FILE *fin = fopen(csv_file, "r");
    if (!fin) {
        char buf[1024];
        sprintf(buf, "Failed to open CSV file: %s\n", csv_file);
        put_string_in_textview("output_textview", buf);
        return NULL;
    }

    const char *obs_expected_header =
        "Sensor,Request_Type,Date,Path,Start Lat,Direction,Duration,"
        "Observation_Mode,Observation_Purpose,PRISM_Nadir_Angle,"
        "PRISM_Forward_Angle,PRISM_Backward_Angle,PRISM_Gain_Nadir,"
        "PRISM_Gain_Forward,PRISM_Gain_Backward,AVNIR_Pointing_Angle,"
        "AVNIR_Gain,AVNIR_Exposure,PALSAR_Table_Number";

    const char *acq_expected_header =
        "Sensor,Request_Type,DRF,Date,Path,Start Lat,Direction,Duration,"
        "Observation_Mode,Observation_Purpose,PRISM_Nadir_Angle,"
        "PRISM_Forward_Angle,PRISM_Backward_Angle,PRISM_Gain_Nadir,"
        "PRISM_Gain_Forward,PRISM_Gain_Backward,AVNIR_Pointing_Angle,"
        "AVNIR_Gain,AVNIR_Exposure,PALSAR_Table_Number";

    const char *l0_expected_header =
        "Downlink_Segment,Level0_Code";

    char line[1024];
    strcpy(line, "");

    // read header line
    fgets(line, 1024, fin);
    strip_end_whitesp(line);

    char *req_file=NULL;

    // request type needs to be figured out from the header
    if (strcmp(line, obs_expected_header) == 0) {
        printf("Processing as an Observation Request.\n");
        req_file = update_request_type(OBSERVATION_REQUEST);

        *request_type = OBSERVATION_REQUEST;
        *drf = STRDUP("");
        *req_id = settings_get_next_req_id(OBSERVATION_REQUEST, *drf);

        obs_process(fin, csv_file, req_file, is_emergency, req_id,
            start_date_user, end_date_user);
    }
    else if (strcmp(line, acq_expected_header) == 0) {
        printf("Processing as an Acquisition Request.\n");
        req_file = update_request_type(ACQUISITION_REQUEST);

        *request_type = ACQUISITION_REQUEST;

        acq_process(fin, csv_file, req_file, is_emergency, req_id,
            start_date_user, end_date_user, drf);
    } 
    else if (strcmp(line, l0_expected_header) == 0) {
        printf("Processing as an On-Demand Level 0 Request.\n");
        req_file = update_request_type(ON_DEMAND_LEVEL_0);

        *request_type = ON_DEMAND_LEVEL_0;
        *drf = STRDUP("");
        *req_id = settings_get_next_req_id(ON_DEMAND_LEVEL_0, *drf);

        odl0_process(fin, csv_file, req_file, is_emergency, req_id,
            start_date_user, end_date_user);
    }
    else {
        // couldn't figure out what this file is, from the header!
        msg("Failed.\n");
        settings_set_request_type(0);

        *request_type = UNSELECTED_REQUEST_TYPE;
        *req_id = 0;

        char buf[1024];
        if (strlen(line) == 0) {
            snprintf(buf, 1024, 
                        "No header line found.\n"
                        "Input file is empty, or begins with a blank line.\n");
        } else {
            snprintf(buf, 1024, "CSV File header line differs from expected!\n\n"
                        "Expected one of these:\n"
                        "  Observation: %s\n"
                        "  Acquisition: %s\n"
                        "  Level 0: %s\n\n"
                        "Got:\n%s\n",
                obs_expected_header, acq_expected_header, l0_expected_header, line);
        }
        put_string_in_textview("output_textview", buf);

        fclose(fin);
        return NULL;
    }

    assert(req_file);
    return req_file;
}
