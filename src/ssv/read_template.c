#include "ssv.h"

// A template file for supporting a new type of import.

// In addition to completing all of the functions in this file, you
// will need to add a bit to read.c/read_file().

// Here is where you'll keep all info necessary to read in data from
// the file.  You'll have the metadata structure passed, but anything
// else you'll need to store in here.  For example, the ASF Internal
// version of this struct holds the currently displayed band number.
// The CEOS struct has the header length.
typedef struct {
} ReadXClientInfo;

//----------------------------------------------------------------------
// try_X()

// 1. This function should return TRUE if the given filename appears to
//    be of this type.

// 2. The given filename may be just a basename, if this is the case,
//    you should add on the typical extension/prepensions, and see if
//    the files exist.  There is a function try_ext() in read.c that
//    might help.

// 3. If the given filename does have an extension/prepension that
//    suggests it is of this type, then you should return TRUE without
//    checking if the file exists.  When the handle_X_file() code
//    is called, that is the time to issue the "file not found"
//    error message.

// 4. The filename will have path information.  If you are looking for
//    prepensions, you must remember to strip off the path info, add
//    the prepension, put back on the path info, and then see if the
//    file exists.  There should be a function try_prepension() in
//    read.c that will do this.

// 5. Do not generate any output - just return TRUE or FALSE. 

int try_X(const char *filename)
{
    return FALSE;
}

//----------------------------------------------------------------------
// handle_X_file()

// INPUT:  filename
//              name of a file that try_X() has indicated is of this type

// OUTPUT: meta_name
//              preallocated array - name of the metadata file, this will
//              be passed to read_X_meta().
//         data_name
//              preallocated array - data file name, this will be passed
//              to open_X_data().
//         err
//              not preallocated - only populate this if an error occurs
//              loading the file.

// 1. This function needs to generate the metadata and data file names
//    for the given file, and check that they exist.

// 2. When the files don't exist, allocate and populate the "err" string
//    with information about what file was being looked for and couldn't
//    be found.

int handle_X_file(const char *filename, char *meta_name, char *data_name,
                    char **err)
{
    *err = STRDUP("File type not supported.\n");
    return FALSE;
}

//----------------------------------------------------------------------
// read_X_meta()

// Create & return a meta_parameters structure from the given file.
// Return NULL if the metadata could not be read.

meta_parameters *read_X_meta(const char *meta_name)
{
    return meta_read(meta_name);
}

//----------------------------------------------------------------------
// read_X_client()

// A function that will read the given rows from the file.

// [in] fp: An already open file pointer to the data file.
// [in] row_start: The row number of the desired row.
// [in] n_rows_to_get: How many rows to read in.
// [out] dest: where the data should be put.  Allocated by the caller.
// [in] read_client_info: A pointer to the ReadXClientInfo struct you
//                        created in open_X_data().
// [in] meta: The metdata from read_X_meta().

// return TRUE on success, FALSE on failure.

int read_X_client(FILE *fp, int row_start, int n_rows_to_get,
                  float *dest, void *read_client_info,
                  meta_parameters *meta)
{
    ReadXClientInfo *info = (ReadXClientInfo*)read_client_info;
    return FALSE;
}

//----------------------------------------------------------------------
// get_X_thumbnail_data()

// This is a performance improvement function - reads in a subset
// of the data to generate a thumbnail.  Theoretically it isn't needed
// but we can get the program started up much faster by having
// dedicated code to reading in the subset of data necessary to
// generate the preview (thumbnail) image.

// Read in a thumb_size_x (columns) by thumb_size_y (rows)
// thumbnail of the data, return TRUE on success, FALSE on failure.

// [in] fp: An already open file pointer to the data file.
// [in] thumb_size_x: Columns
// [in] thumb_size_y: Rows
// [in] meta: The metdata from read_X_meta().
// [in] read_client_info: A pointer to the ReadXClientInfo struct you
//                        created in open_X_data().
// [out] dest: where the data should be put.  Allocated by the caller.

// You should use an asfPercentMeter while reading in the data.

int get_X_thumbnail_data(FILE *fp, int thumb_size_x,
                         int thumb_size_y, meta_parameters *meta,
                         void *read_client_info, float *dest)
{
    ReadXClientInfo *info = (ReadXClientInfo*)read_client_info;
    return FALSE;
}

//----------------------------------------------------------------------
// open_X_data()

// You probably don't need to change this beyond updating "X"
// Frees the client info structure

void free_X_client_info(void *read_client_info)
{
    ReadXClientInfo *info = (ReadXClientInfo*) read_client_info;
    free(info);
}

//----------------------------------------------------------------------
// open_X_data()

// Open the data file, populate the read_client_info structure,
// and set pointers to the read_X_client and get_X_thumbnail_data
// functions.

// [in] data_name: the data file name, returned from handle_X_file().
// [in] meta_name: the meta file name, returned from handle_X_file().
// [in] band: Command-line band argument.  Ignore if you don't support
//            bands.  Will be NULL if no band argument was supplied, in
//            which case you should load the first band.
// [in] meta: The metdata from read_X_meta().
// [out] client: Structure of pointers to client functions,
//               including the read_client_info.  You should allocate
//               read_client_info, but ClientInterface is pre-allocated

int open_X_data(const char *data_name, const char *meta_name,
                const char *band, meta_parameters *meta,
                ClientInterface *client)
{
    ReadXClientInfo *info = MALLOC(sizeof(ReadXClientInfo));

    client->read_client_info = info;
    client->read_fn = read_X_client;
    client->thumb_fn = get_X_thumbnail_data;
    client->free_fn = get_X_thumbnail_data;

    return FALSE;
}
