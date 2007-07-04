#include "ssv.h"

#include "asf_import.h"
#include "get_ceos_names.h"
#include "asf_nan.h"
#include "asf_endian.h"

#include "read_ceos.h"

// This isn't a full implementation of what is in read_template.c
// Here we re-use some of the stuff in read_ceos, since alos files are
// ceos.  We handle the different naming scheme here, but the data
// reading code is the ceos stuff.

int try_alos(const char *filename)
{
    // strip off path info
    char *file = get_filename(filename);
    int ret;

    if (strncmp_case(file, "LED-", 4) == 0 ||
        strncmp_case(file, "IMG-", 4) == 0)
    {
        ret = TRUE;
    }
    else {
        // try adding on the LED- prepension
        ret = try_prepension(filename, "LED-");
        free(file);
    }

    return ret;
}

meta_parameters *read_alos_meta(const char *meta_name)
{
    return meta_create(meta_name);
}

int handle_alos_file(const char *filename, const char *band,
                     char *meta_name, char *data_name, char **err)
{
    char *dir = MALLOC(sizeof(char)*(strlen(filename)+10));
    char *file = MALLOC(sizeof(char)*(strlen(filename)+10));
    split_dir_and_file(filename, dir, file);

    int ret = TRUE;

    if (strncmp_case(file, "IMG-", 4) == 0) {
        // user provided us with the data file
        strcpy(data_name, filename);
        // generate leader file name using alos convention
        // can't forget to prepend the dir info
        if (strlen(dir) > 0)
            strcpy(meta_name, dir);
        else
            strcpy(meta_name, "");
        strcat(meta_name, "LED-");
        char *p = strchr(file+5, '-') + 1;
        strcat(meta_name, p);
    } else {
        // either user provided the name of the LED- file
        // or user provided the basename
        // in either case, we will be looking for the leader file,
        // and from that must figure out the name of the IMG- file
        if (strncmp_case(file, "LED-", 4) == 0) {
            // user provided leader file name
            strcpy(meta_name, filename);
        }
        else {
            // user provided basename, we will add LED-
            if (strlen(dir) > 0)
                strcpy(meta_name, dir);
            else
                strcpy(meta_name, "");
            strcat(meta_name, "LED-");
            strcat(meta_name, file);
        }

        // ensure meta file exists before trying to figure out data name
        // since data name search will involve checking files exist, the
        // error message would be more informative if we look for what they
        // typed in first, if that was the problem
        if (!fileExists(meta_name)) {
            int l = sizeof(char)*(strlen(meta_name)+255);
            *err = MALLOC(l);
            snprintf(*err, l, "Error opening ALOS file: %s\n", meta_name); 
            ret = FALSE;
        } else {
            // now figure out data name
            char **dataName = MALLOC(sizeof(char*)*MAX_BANDS);
            int i,nBands;
            for (i=0; i<MAX_BANDS; ++i)
                dataName[i] = MALLOC(sizeof(char)*256);
            char *p = meta_name;
            if (strncmp_case(p, "LED-", 4) == 0) p += 4;
            get_ceos_data_name(p, dataName, &nBands);
            int which_band=-1;
            if (band) {
                for (i=0; i<nBands; ++i) {
                    if (strcmp(dataName[i], band) == 0) {
                        which_band=i;
                        break;
                    }
                }
            } else
                which_band = 0;
            if (which_band < 0) {
                int l = sizeof(char)*(strlen(filename)+strlen(band)+255);
                *err = MALLOC(l);
                snprintf(*err, l, "Error opening ALOS file: %s\n"
                    "  The requested band '%s' was not found.\n", filename, band);
                ret = FALSE;
            } else {
                strcpy(data_name, dataName[which_band]);
            }
        }   
    }

    free(dir);
    free(file);

    return ret;
}

// we couldn't use open_ceos_data, because of what needs to get
// passed to get_ifiledr -- for alos, must pass in the basename
int open_alos_data(const char *data_name, const char *meta_name,
                   const char *band, meta_parameters *meta, 
                   ReadClientFn **read_fn, ThumbFn **thumb_fn,
                   void **read_client_info)
{
    ReadCeosClientInfo *info = MALLOC(sizeof(ReadCeosClientInfo));

    int ns = meta->general->sample_count;

    *read_client_info = info;

    // for alos, use the ceos reader client functions
    *read_fn = read_ceos_client;
    *thumb_fn = get_ceos_thumbnail_data;

    struct IOF_VFDR image_fdr;
    get_ifiledr(meta_name, &image_fdr);

    int leftFill = image_fdr.lbrdrpxl;
    int rightFill = image_fdr.rbrdrpxl;

    info->headerBytes = firstRecordLen((char*)data_name) +
        (image_fdr.reclen - (ns + leftFill + rightFill)*image_fdr.bytgroup);

    info->reclen = image_fdr.reclen;

    return TRUE;
}
