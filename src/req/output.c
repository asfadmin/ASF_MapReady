#include "req.h"

static void generate(char **dir, char **file)
{
    Settings *s = settings_load();
    if (s->output_dir)
        *dir = STRDUP(s->output_dir);
    else
        *dir = STRDUP("");

    char e = settings_get_is_emergency() ? 'E' : 'W';
    char *request_type;
    switch (settings_get_request_type()) {
        case OBSERVATION_REQUEST:
            request_type="REQ"; break;
        case ACQUISITION_REQUEST:
            request_type="RQT"; break;
        case ON_DEMAND_LEVEL_0:
            request_type="L0MR"; break;
        default:
            // this will have to be filled in later, with what is detected
            request_type="???"; break;
    }

    *file = MALLOC(sizeof(char)*32);
    sprintf(*file, "%s%c%06d", request_type, e, s->req_num);

    settings_free(s);
}

void update_output_file()
{
    char *dir, *file;
    generate(&dir, &file);

    put_string_to_entry("output_dir_entry", dir);
    put_string_to_entry("output_file_entry", file);

    //printf("update output file to: %s\n", file);

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
