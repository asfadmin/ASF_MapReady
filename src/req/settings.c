#include "req.h"
#include <ctype.h>

Settings *settings_new()
{
    Settings *s = MALLOC(sizeof(Settings));
    s->csv_dir = NULL;
    s->output_dir = NULL;
    s->req_num = 1;
    return s;
}

void settings_free(Settings *s)
{
    if (s->csv_dir) FREE(s->csv_dir);
    if (s->output_dir) FREE(s->output_dir);
    FREE(s);
}

static int matches(const char *buf, const char *key)
{
    return strncmp_case(buf, key, strlen(key)) == 0;
}

static void read_string_param(const char *buf, const char *key, char **value)
{
    if (matches(buf, key)) {
        printf("Matches: %s\n", key);
        char *p = strchr(buf, '=');

        // skip past = sign, eat whitespace
        ++p;
        while (isspace(*p))
            ++p;

        *value = MALLOC(sizeof(char)*(strlen(p)+1));
        strcpy(*value, p);

        // eat trailing whitespace, too
        p = *value + strlen(*value) - 1;
        while (isspace(*p))
            *p-- = '\0';

        printf("Value: %s\n", *value);
    }
}

static void read_int_param(const char *buf, const char *key, int *value)
{
    if (matches(buf, key)) {
        printf("Matches: %s\n", key);
        char *p = strchr(buf, '=') + 1;
        *value = atoi(p);
        printf("Value: %d\n", *value);
    }
}

Settings *settings_load()
{
    Settings *s = settings_new();

    char *sav_file = find_in_share("req_settings.txt");
    if (fileExists(sav_file)) {
        printf("Found settings file: %s\n", sav_file);
        FILE *fp = FOPEN(sav_file, "r");
        if (fp) {
            char buf[1024];
            while (fgets(buf, 1024, fp) != NULL) {
                printf("Line: %s\n", buf);
                read_string_param(buf, "csv directory", &s->csv_dir);
                read_string_param(buf, "output directory", &s->output_dir);
                read_int_param(buf, "next request number", &s->req_num);
            }
            FCLOSE(fp);
        } else {
            printf("Failed to open settings file: %s\n", sav_file);
        }
    } else {
        printf("No settings file found.\n");
    }

    FREE(sav_file);
    return s;
}

static void apply_settings_to_gui(Settings *s)
{
    put_string_to_entry("csv_directory_entry", s->csv_dir);
    put_string_to_entry("output_directory_entry", s->output_dir);
    put_int_to_entry("next_request_number_entry", s->req_num);
}

void apply_saved_settings()
{
    Settings *s = settings_load();
    apply_settings_to_gui(s);
    settings_free(s);
}

static void add_to_text(char **txt, int *cur_len, const char *format, ...)
{
    char buf[1024];

    va_list ap;
    va_start(ap, format);
    int len = vsnprintf(buf, sizeof(buf), format, ap);
    va_end(ap);

    if (len > 1020)
        printf("Warning! lengthy line may have been truncated.\n");

    if (strlen(*txt) + len >= *cur_len) {
        *cur_len += 1024;
        char *new_txt = MALLOC(sizeof(char)*(*cur_len));
        strcpy(new_txt, *txt);
        strcat(new_txt, buf);
        free(*txt);
        *txt = new_txt;
    } else {
        strcat(*txt, buf);
    }
}

void settings_save(Settings *s)
{
    char *sav_file = find_in_share("req_settings.txt");
    if (fileExists(sav_file)) {
        // update an existing file
        printf("Found settings file: %s\n", sav_file);
        char *new_sav_txt = MALLOC(sizeof(char)*1024);
        int len=1024;
        int wrote_csv=FALSE, wrote_output=FALSE, wrote_req_num=FALSE;
        FILE *fp = FOPEN(sav_file, "r");
        if (!fp) {
            message_box("Error opening output file!\n");
            return;
        }

        char buf[1024];
        while (fgets(buf, 1024, fp) != NULL) {
            if (s->csv_dir && matches(buf, "csv directory")) {
                add_to_text(&new_sav_txt, &len,
                    "csv directory = %s\r\n", s->csv_dir);
                wrote_csv = TRUE;
            } else if (s->output_dir && matches(buf, "output directory")) {
                add_to_text(&new_sav_txt, &len,
                    "output directory = %s\r\n", s->output_dir);
                wrote_output = TRUE;
            } else if (matches(buf, "next request number")) {
                add_to_text(&new_sav_txt, &len,
                    "next request number = %d\r\n", s->req_num);
                wrote_req_num = TRUE;
            } else {
                add_to_text(&new_sav_txt, &len, "%s", buf);
            }
        }
        FCLOSE(fp);

        if (s->csv_dir && !wrote_csv)
            add_to_text(&new_sav_txt, &len,
                "csv directory = %s\n", s->csv_dir);
        if (s->output_dir && !wrote_output)
            add_to_text(&new_sav_txt, &len,
                "output directory = %s\n", s->output_dir);
        if (!wrote_req_num && s->req_num > 1)
            add_to_text(&new_sav_txt, &len,
                "next request number = %d\n", s->req_num);

        fp = FOPEN(sav_file, "w");
        fprintf(fp, "%s", new_sav_txt);
        FCLOSE(fp);
    } else {
        // write a new file
        FILE *fp = FOPEN(sav_file, "w");
        if (s->csv_dir)
            fprintf(fp, "csv directory = %s\r\n", s->csv_dir);
        if (s->output_dir)
            fprintf(fp, "output directory = %s\r\n", s->output_dir);
        fprintf(fp, "next request number = %d\r\n", s->req_num);
        FCLOSE(fp);
    }
}

static char *mkstr(const char *s)
{
    char *ret=NULL;
    if (strlen(s) > 0)
        ret = STRDUP(s);
    return ret;
}

Settings *settings_new_from_gui()
{
    Settings *s = settings_new();

    s->csv_dir = settings_get_csv_dir();
    s->output_dir = settings_get_output_dir();

    s->req_num = atoi(get_string_from_entry("next_request_number_entry"));

    return s;
}

void save_settings()
{
    Settings *s = settings_new_from_gui();
    settings_save(s);
    settings_free(s);
}

char *settings_get_csv_dir()
{
    return mkstr(get_string_from_entry("csv_directory_entry"));
}

char *settings_get_output_dir()
{
    return mkstr(get_string_from_entry("output_directory_entry"));
}

SIGNAL_CALLBACK void on_save_button_clicked(GtkWidget *widget)
{
    save_settings();
    // FIXME: change this to a message that appears in the gui, no popup
    message_box("The new settings have been saved.");
}
