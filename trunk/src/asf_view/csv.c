#include "winshl.h"
#include "asf_view.h"

#ifdef win32
// in excel.c
void open_excel(const char *csv_file);
#endif

// try to find a program we can use to view the generated csv files
const char * detect_csv_assoc()
{
    static char *csv_app = NULL;
    if (!csv_app) {
        char *csv_file = find_in_share("asf_view_cfg.csv");

#ifdef win32
        // On Windows, use the file association table to find out what we
        // can do with csv files.
        char path[1024];
        int ret = (int)FindExecutable((LPCTSTR)csv_file,
            (LPCTSTR)get_asf_share_dir(), (LPTSTR)path);
        if (ret > 32 && strlen(path) > 0) {
            csv_app = escapify(path);
            printf("Path to CSV Application: %s\n", csv_app);
        } else {
            if (ret==SE_ERR_FNF)
                printf("File not found: %s\n", csv_file);
            else if (ret==SE_ERR_NOASSOC)
                printf("No association for: %s\n", csv_file);
            else if (ret==SE_ERR_OOM)
                printf("Out of resources.\n");
            else
                printf("Unknown error! (return value: %d)\n", ret);

            csv_app = STRDUP("notepad.exe");
            printf("CSV Application not found -- using notepad.\n");
        }
        FREE(csv_file);
#else
        // On Linux, get the app from the configuration file
        FILE *cfg = fopen(csv_file, "r");
        if (cfg) {
            char tmp[1024];
            while (fgets(tmp, 1024, cfg) != NULL) {
                if (strncmp_case(tmp, "CSV,", 4) == 0) {
                    csv_app = trim_whitespace(tmp+4);
                    printf("CSV Application from config file: %s\n", csv_app);
                }
            }
            if (!csv_app)
                csv_app = STRDUP("");
            fclose(cfg);
        } else {
            printf("Failed to open %s: %s\n", csv_file, strerror(errno));
            csv_app = STRDUP("");
            printf("CSV Application not found.\n");
        }
#endif
    }
    return csv_app;
}

void open_csv(const char *csv_file)
{
#ifdef win32
        open_excel(csv_file);
#else
        const char *csv_app = detect_csv_assoc();
        int have_csv_viewer = csv_app && strlen(csv_app) > 0;
        if (have_csv_viewer) {
            int pid = fork();
            if (pid == 0) {
                asfSystem("\"%s\" \"%s\"", csv_app, csv_file);
                exit(EXIT_SUCCESS);
            }
        } else {
            char errbuf[1024];
            snprintf(errbuf, 1024, "Don't know how to load: %s", csv_file);
            message_box(errbuf);
            strcat(errbuf, "\n");
            printf(errbuf);
        }
#endif
}

