#include "req.h"

static void generate(char **dir, char **file)
{
    Settings *s = settings_load();
    if (s->output_dir)
        *dir = STRDUP(s->output_dir);
    else
        *dir = STRDUP("");

    char request_type = settings_get_is_emergency() ? 'E' : 'W';

    *file = MALLOC(sizeof(char)*32);
    sprintf(*file, "REQ%c%06d", request_type, s->req_num);

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
