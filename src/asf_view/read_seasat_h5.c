#include "asf_view.h"
#include "asf_iso_meta.h"
#include "hdf5.h"

typedef struct {
    hid_t file, space, dset;
} ReadSeasatClientInfo;

int try_seasat_h5(const char *filename, int try_extensions)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
        return strcmp_case(ext, ".h5") == 0 ||
               strcmp_case(ext, ".xml") == 0;
    } else if (try_extensions) {
        return try_ext(filename, ".h5");
    } else {
        return FALSE;
    }
}

int handle_seasat_h5_file(const char *filename, char *meta_name, char *data_name,
                          char **err)
{
    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int has_h5_ext = has_ext && strcmp_case(ext,".h5")==0;

    // either they gave us an HDF5 extension (.h5), or we try adding it
    // to a user-provided basename, and that file does exist
    if (has_h5_ext || try_ext(filename, ".h5"))
    {
        char *m = appendExt(filename, ".xml");
        char *d = appendExt(filename, ".h5");

        strcpy(meta_name, m);
        strcpy(data_name, d);

        int ret;
        if (!fileExists(meta_name) || !fileExists(data_name)) {
            int l = sizeof(char)*strlen(filename)*2+255;
            *err = MALLOC(l);
            snprintf(*err, l,
                "Error opening SEASAT HDF5 Format file.\n"
                "  Metadata file: %s - %s\n"
                "      Data file: %s - %s\n",
                m, fileExists(m) ? "Found" : "NOT FOUND",
                d, fileExists(d) ? "Found" : "NOT FOUND");

            ret = FALSE;
        }
        else
            ret = TRUE;

        free(m);
        free(d);

        return ret;
    }
    else {
        // in theory this shouldn't happen, if try_ext is working
        assert(!try_ext(filename, ".h5"));
        int l = sizeof(char)*strlen(filename)*2+255;
        *err = MALLOC(l);
        snprintf(*err, l,
            "Failed to open %s as a SEASAT HDF5 Format File.\n", filename);
        return FALSE;
    }

    // not reached
    assert(FALSE);
    return FALSE;
}

meta_parameters *read_seasat_h5_meta(const char *meta_name)
{
  iso_meta *iso = iso_meta_read(meta_name);
  meta_parameters *meta = iso2meta(iso);
  iso_meta_free(iso);

  return meta;
}

int read_seasat_h5_client(int row_start, int n_rows_to_get,
                          void *dest_void, void *read_client_info,
                          meta_parameters *meta, int data_type)
{
    ReadSeasatClientInfo *info = (ReadSeasatClientInfo*) read_client_info;

    float *dest = (float*)dest_void;

    H5Dread(info->dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dest);

    return TRUE;
}

void free_seasat_h5_client_info(void *read_client_info)
{
    ReadSeasatClientInfo *info = (ReadSeasatClientInfo*) read_client_info;
    H5Dclose(info->dset);
    H5Sclose(info->space);
    H5Fclose(info->file);
    free(info);
}

int open_seasat_h5_data(const char *filename,
                        meta_parameters *meta, ClientInterface *client)
{
    ReadSeasatClientInfo *info = MALLOC(sizeof(ReadSeasatClientInfo));

    info->file = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
    info->dset = H5Dopen(info->file, "/data/HH", H5P_DEFAULT);
    info->space = H5Dget_space(info->dset);
    
    client->read_client_info = info;
    client->read_fn = read_seasat_h5_client;
    client->thumb_fn = NULL;
    client->free_fn = free_seasat_h5_client_info;
    client->data_type = GREYSCALE_FLOAT;
    client->require_full_load = TRUE;

    return TRUE;
}
