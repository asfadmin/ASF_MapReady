#if defined(win32)

#define BYTE __byte
#define POINT __point
#include "asf_meta.h"
#undef BYTE

#include <windows.h>
#include <process.h>

typedef DWORD WINAPI winForkFunc(void *);

#undef POINT
#else

#include "asf_meta.h"
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#endif

#include "c2v.h"
#include "asf_vector.h"
#include "asf_fork.h"
#include <ctype.h>

SIGNAL_CALLBACK void on_convert_button_clicked(GtkWidget *w)
{
  process();
}

const char *input_format_to_str(int input_format)
{
  switch (input_format) {
    case INPUT_META: return "META";
    case INPUT_LEADER: return "LEADER";
    case INPUT_POINT: return "POINT";
    case INPUT_POLYGON: return "POLYGON";
    case INPUT_SHAPE: return "SHAPE";
    case INPUT_KML: return "KML";
    case INPUT_GEOTIFF: return "GEOTIFF";
    case INPUT_ALOS_CSV: return "AUIG";
    case INPUT_GENERIC_CSV: return "CSV";
    case INPUT_URSA: return "URSA";
    case INPUT_TERRASAR: return "TERRASAR";
    case INPUT_STF: return "STF";
    default: assert(0); return "";
  }
}

const char *output_format_to_str(int output_format)
{
  switch (output_format) {
    case OUTPUT_TEXT: return "CSV";
    case OUTPUT_SHAPE: return "SHAPE";
    case OUTPUT_KML: return "KML";
    case OUTPUT_ALOS_CSV: return "AUIG";
    default: assert(0); return "";
  }
}

typedef struct
{
    char in_file[1024];
    char out_file[1024];
    char in_format[128];
    char out_format[128];
} child_params_t;

int child_func(void *params)
{
  child_params_t *cp = (child_params_t *)params;
  c2v_config *cfg = init_fill_c2v_config();

  char *lf = appendExt(cp->out_file, ".log");
  strcpy(logFile, lf);
  free(lf);

  quietflag = FALSE;
  logflag = TRUE;

  fLog = FOPEN(logFile, "a");

  strcpy(cfg->input_file, cp->in_file);
  strcpy(cfg->output_file, cp->out_file);
  strcpy(cfg->input_format, cp->in_format);
  strcpy(cfg->output_format, cp->out_format);
  int ret = convert2vector(cfg);

  asfPrintStatus("Successful completion!\n");

  FCLOSE(fLog);
  return ret==1 ? 0 : 1;
}

int parent_func(void *params)
{
  while (gtk_events_pending())
    gtk_main_iteration();

  g_usleep(50);
  return 0;
}

void process()
{
  clear_results_message();

  child_params_t *cp = MALLOC(sizeof(child_params_t));
  strcpy(cp->in_file, get_string_from_entry("input_file_entry"));

  char *odir = get_string_from_entry("output_directory_entry");
  char *ofile = get_string_from_entry("output_file_entry");

  if (strlen(odir) > 0) {
#ifdef win32
    if (odir[strlen(odir)-1]=='/' || odir[strlen(odir)-1]=='\\')
#else
    if (odir[strlen(odir)-1]=='/')
#endif
      sprintf(cp->out_file, "%s%s", odir, ofile);
    else
      sprintf(cp->out_file, "%s/%s", odir, ofile);
  }
  else {
    strcpy(cp->out_file, ofile);
  }

  if (strlen(cp->in_file)==0) {
    message_box("No input file specified!");
    free(cp);
    return;
  }
  else if (strlen(cp->out_file)==0) {
    message_box("No output file selected!");
    free(cp);
    return;
  }
  else if (!fileExists(cp->in_file)) {
    message_box("Input file \"%s\" not found!", cp->in_file);
    free(cp);
    return;
  }

  int input_format = get_combo_box_item("input_format_combobox");
  int output_format = get_combo_box_item("output_format_combobox");

  if (input_format==INPUT_AUTO) {
    select_defaults_by_file_type(cp->in_file, FALSE);
    input_format = get_combo_box_item("input_format_combobox");
    if (input_format==INPUT_AUTO) {
      message_box("Can't figure out which type of data this is.\n"
                  "Please select the input format manually.");
      free(cp);
      return;
    }
  }

  strcpy(cp->in_format, input_format_to_str(input_format));
  strcpy(cp->out_format, output_format_to_str(output_format));
  char *logFile = appendExt(cp->out_file, ".log");

#ifdef win32
    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    memset(&si, 0, sizeof(si));
    memset(&pi, 0, sizeof(pi));
    si.cb = sizeof(si);
    
    char *cmd = MALLOC(sizeof(char)*
        (strlen(cp->in_file) + strlen(cp->out_file) + strlen(get_asf_bin_dir_win()) + 512));
    sprintf(cmd,
        "\"%s/convert2vector.exe\" "
        "-log \"%s\" "
        "-input-format %s -output-format %s \"%s\" \"%s\"",
        get_asf_bin_dir_win(),
        logFile,
        cp->in_format, cp->out_format, cp->in_file, cp->out_file);

    // clear out any previously existing log file
    fLog = fopen(logFile, "a");
    FCLOSE(fLog);

    if (!CreateProcess(NULL, cmd, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi))
    {
        DWORD dw = GetLastError();
        //printf( "CreateProcess failed (%ld)\n", dw );

        LPVOID lpMsgBuf;
        FormatMessage(
          FORMAT_MESSAGE_ALLOCATE_BUFFER |
          FORMAT_MESSAGE_FROM_SYSTEM |
          FORMAT_MESSAGE_IGNORE_INSERTS,
          NULL,
          dw,
          MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT),
          (LPTSTR)&lpMsgBuf,
          0,
          NULL);

        printf("CreateProcess() failed with error %ld: %s\n",
            dw, (char*)lpMsgBuf);
        printf("Failed command: %s\n", cmd);
    }

    DWORD dwWaitResult;

    // now wait for process to finish
    do {
        while (gtk_events_pending())
            gtk_main_iteration();
        dwWaitResult = WaitForSingleObject(pi.hProcess, 50);
    }
    while (dwWaitResult == WAIT_TIMEOUT);
#else
  asfFork(child_func, (void*)cp, parent_func, NULL);
#endif

  char message[1024];
  strcpy(message, "-");

  int success=FALSE;
  FILE *lfp = fopen(logFile, "r");
  if (!lfp) {
    sprintf(message, "Error Opening Log File '%s': %s\n",
            logFile, strerror(errno));
  }
  else {
    char *output = NULL;
    char line[1024];
    while (fgets(line, 1024, lfp)) {
      if (output) {
        output = realloc(output, sizeof(char)*(strlen(output)+strlen(line)+1));
        strcat(output, line);
      }
      else {
        output = malloc(sizeof(char)*(strlen(line)+1));
        strcpy(output, line);
      }
    }
    fclose(lfp);
    //unlink(logFile);

    char *p = output, *q;
    while (p) {
      q = strchr(p+1, '\n');
      if (q) {
        *q = '\0';
        if (strstr(p, "Error")!=NULL || strstr(p, "ERROR")!=NULL) {
          *q = '\n';
          int i,n = 0;
          do {
            p = q + 1;
            q = strchr(p, '\n') - 1;
            while (isspace(*q)) --q;
            if (q - p > 2) {
              // First 220 characters of the error string, unless line ends
              // first.  Don't cut off in the middle of a word, though
              strcpy(message, "Error: ");
              strncat(message, p, 220);
              while (isalnum(message[strlen(message)-1]))
                message[strlen(message)-1] = '\0';
              for (n=0; n<strlen(message); ++n)
                if (message[n] == '\n' || message[n] == '*') message[n] = ' ';
              int eating=FALSE;
              for (n=0,i=0; n<strlen(message); ++n) {
                if (isspace(message[n])) {
                  if (!eating) {
                    eating=TRUE;
                    message[i++] = message[n];
                  }
                } else {
                  eating=FALSE;
                  message[i++] = message[n];
                }
              }
              message[i]='\0';
              char *eoe = strstr(message, "End of error");
              if (eoe)
                *eoe = '\0';
              else if (strlen(message)>200)
                strcat(message, " ...");
              break;
            }
          }
          while (*p != '*' && ++n<5); // * flags the end of the error message
        }

        if (strstr(p,"Successful completion!") != NULL) {
          strcpy(message, "Processed successfully!");
          success=TRUE;
          break;
        }

        *q = '\n';
      }
      p=q;
    }

    if (strlen(message)==0) {
      // Did not find an error message, or the success message!
      // So, assume there was an error, but we don't know why
      strcpy(message, "Processing failed!");
    }

    FREE(output);
  }

  int open_output = get_checked("open_output_checkbutton");
  if (!success) open_output = FALSE;

  put_string_to_label("result_label", message);
  asfPrintStatus(message);
  asfPrintStatus("\n\nDone.\n\n");

  if (open_output) {
    switch (output_format) {
      case OUTPUT_KML:
        open_in_google_earth(cp->out_file);
        break;
      case OUTPUT_ALOS_CSV:
        open_in_excel(cp->out_file);
      default:
        // do nothing, output type has no natural associated app
        break;
    }
  }

  free(cp);
}
