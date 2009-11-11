#include "req.h"
#include <time.h>

static void generate(char **dir, char **file)
{
    Settings *s = settings_load();
    if (s->output_dir)
        *dir = STRDUP(s->output_dir);
    else
        *dir = STRDUP("");

    *file = MALLOC(sizeof(char)*32);

    int request_type = settings_get_request_type();
    //int is_aadn = settings_get_is_aadn();

    if (request_type==ON_DEMAND_LEVEL_0) {
        time_t t = time(NULL);
        struct tm *ts = gmtime(&t);
        snprintf(*file, 32, "L0MR%02d%02d%02d",
            ts->tm_mon+1, ts->tm_mday, settings_get_sequence_number());
    } else if (request_type != UNSELECTED_REQUEST_TYPE) {
        char e = settings_get_is_emergency() ? 'E' : 'W';
        if (request_type == OBSERVATION_REQUEST)
            snprintf(*file, 32, "REQ%c%06d", e, s->obs_req_num);
        else if (request_type == ACQUISITION_REQUEST)
            snprintf(*file, 32, "RQT%c%06d", e, s->acq_req_num);
        else
            assert(FALSE); // can't happen
    } else {
        strcpy(*file, "???");
    }

    settings_free(s);
}

void update_output_file()
{
    char *dir, *file;
    generate(&dir, &file);

    put_string_to_entry("output_dir_entry", dir);
    put_string_to_entry("output_file_entry", file);

    FREE(dir);
    FREE(file);
}

char *get_output_file()
{
    char *dir, *file;
    generate(&dir, &file);
    char *ret = MALLOC(sizeof(char)*(strlen(dir)+strlen(file)+5));

#ifdef win32
    sprintf(ret, "%s\\%s", dir, file);
#else
    sprintf(ret, "%s/%s", dir, file);
#endif

    FREE(dir);
    FREE(file);

    return ret;
}
