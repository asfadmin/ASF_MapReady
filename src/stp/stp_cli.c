#include "stp.h"

#include "asf.h"
#include "asf_meta.h"
#include "asf_import.h"
#if defined(DIR_SEPARATOR)
#undef DIR_SEPARATOR
#endif

#include "ardop_defs.h"

#ifdef win32
const char PATH_SEPARATOR = ';';
const char DIR_SEPARATOR = '\\';
#else
const char PATH_SEPARATOR = ':';
const char DIR_SEPARATOR = '/';
#endif

static stp_params_t *stp_params_from_args(int argc, char *argv[])
{
    stp_params_t *ret = malloc(sizeof(stp_params_t));

    if (argc != 12)
        return NULL;

    ret->status = atoi(argv[1]);
    //printf("ret->status: %d\n", ret->status);
    ret->fd = atof(argv[2]);
    //printf("ret->fd: %f\n", ret->fd);
    ret->fdd = atof(argv[3]);
    //printf("ret->fdd: %f\n", ret->fdd);
    ret->fddd = atof(argv[4]);
    //printf("ret->fddd: %f\n", ret->fddd);
    ret->fd_set = atoi(argv[5]);
    //printf("ret->fd_set: %d\n", ret->fd_set);
    ret->fdd_set = atoi(argv[6]);
    //printf("ret->fdd_set: %d\n", ret->fdd_set);
    ret->fddd_set = atoi(argv[7]);
    //printf("ret->fddd_set: %d\n", ret->fddd_set);
    ret->debug_flag = atoi(argv[8]);
    //printf("ret->debug_flag: %d\n", ret->debug_flag);
    ret->ifirstline = atoi(argv[9]);
    //printf("ret->ifirstline: %d\n", ret->ifirstline);
    strcpy(ret->input_file, argv[10]);
    //printf("ret->input_file: %s\n", ret->input_file);
    strcpy(ret->output_file, argv[11]);
    //printf("ret->output_file: %s\n", ret->output_file);

    return ret;
}

// This is the target of the CreateRemoteThread
int main(int argc, char *argv[])
{
  stp_params_t *stp_params = stp_params_from_args(argc, argv);

  if (!stp_params) {
    printf("Invalid command line.\n\n");
    printf("Please note that this tool is not intended to be run directly,\n");
    printf("you probably want to run stp.exe\n\n");
  }
  else {
    // running import, if necessary
    char *img_file;
    if (stp_params->status == STATUS_LDR_INSTEAD) {
        // the imported file will go where the output file is
        img_file = STRDUP(stp_params->output_file);
        asfPrintStatus("Importing Level 0 data...\n");
        import_ceos(stp_params->input_file, img_file, NULL, NULL, NULL,
                    NULL, NULL, 0, 0, -99, -99, NULL, r_AMP, FALSE,
                    FALSE, FALSE, FALSE, TRUE);
        asfPrintStatus("Import complete.\n");
    }
    else if (stp_params->status == STATUS_STF_INSTEAD) {
        // the imported file will go where the output file is
        img_file = appendExt(stp_params->output_file, "");
        asfPrintStatus("Importing STF data...\n");
        import_stf(stp_params->input_file, img_file, r_AMP, NULL,
                0, -99, -99, NULL);
        asfPrintStatus("Import complete.\n");
    }
    else {
        // no import necessary-- input to ardop is the original .img file
        img_file = change_extension(stp_params->input_file, "img");
    }

    // running ardop
    struct INPUT_ARDOP_PARAMS *params_in;
    params_in = get_input_ardop_params_struct(img_file, stp_params->output_file);

    float l_fd, l_fdd, l_fddd;
    if (stp_params->fd_set) {
        l_fd = stp_params->fd;
        params_in->fd = &l_fd;
    }
    if (stp_params->fdd_set) {
        l_fdd = stp_params->fdd;
        params_in->fdd = &l_fdd;
    }
    if (stp_params->fddd_set) {
        l_fddd = stp_params->fddd;
        params_in->fddd = &l_fddd;
    }
    sprintf(params_in->status, "%s.status", stp_params->output_file);

    char *tmp_dir = get_dirname(stp_params->output_file);
    set_asf_tmp_dir(tmp_dir);
    FREE(tmp_dir);

    // this stuff shouldn't cause collisions, local stack variables      
    int npatches = 1;
    params_in->iflag = &(stp_params->debug_flag);
    params_in->npatches = &npatches;
    params_in->ifirstline = &(stp_params->ifirstline);

    ardop(params_in);
    free(params_in);
    free(img_file);

    // success!
  }

  return 0;
}
