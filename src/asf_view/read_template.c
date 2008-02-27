#include "asf_view.h"

// A template file for supporting a new type of import.

// In addition to completing all of the functions in this file, you
// will need to add a bit to read.c/read_file().

// Here is where you'll keep all info necessary to read in data from
// the file.  You'll have the metadata structure passed, but anything
// else you'll need to store in here.  For example, the ASF Internal
// version of this struct holds the currently displayed band number.
// The CEOS struct has the header length.

// Almost certainly, you'll need to keep the file pointer in here
typedef struct {
    FILE *fp;
} ReadXClientInfo;

//----------------------------------------------------------------------
// try_X()

// 1. This function should return TRUE if the given filename appears to
//    be of this type.

// 2. If "try_extensions" is TRUE, the given filename may be just 
//    a basename --- if this is the case, you should add on the typical
//    extension/prepensions, and see if the files exist.  There is a
//    function try_ext() in read.c that might help.

//    If "try_extensions" is FALSE, only check if the given filename
//    obeys the naming convention, and return TRUE if it does,
//    FALSE if it does not.

//    If the given filename does have an extension/prepension that
//    suggests it is of this type, then you should return TRUE without
//    checking if the file exists.  When the handle_X_file() code
//    is called, that is the time to issue the "file not found"
//    error message.

// 3. The filename will have path information.  If you are looking for
//    prepensions, you must remember to strip off the path info, add
//    the prepension, put back on the path info, and then see if the
//    file exists.  There should be a function try_prepension() in
//    read.c that will do this.

// 4. Do not generate any output - just return TRUE or FALSE. 

int try_X(const char *filename, int try_extensions)
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

// The only place this is called is in "read.c/read_file()", so you can
// customize the signature to fit your needs.  However, you do need to
// populate meta_name & data_name (even if the file doesn't actually
// separate the two).  I don't think you'd need to change this functions
// signature.
int handle_X_file(const char *filename, char *meta_name, char *data_name,
                    char **err)
{
    *err = STRDUP("File type not supported.\n");
    return FALSE;
}

//----------------------------------------------------------------------
// read_X_meta()

// Create & return a meta_parameters structure from the given file.
// Return NULL if the metadata could not be read, or there is no
// metadata for this particular format.

// The only place this is called is in "read.c/read_file()", so you can
// customize the signature to fit your needs.  The read_jpeg client, for
// example, eliminated this function entirely, and returned metadata
// with the "open_" function, below.
meta_parameters *read_X_meta(const char *meta_name)
{
    meta_parameters *meta = raw_init();

    // At least populate these fields:
    // General-->
    //      line_count
    //      sample_count
    //      band_count
    //      bands
    return meta;
}

//----------------------------------------------------------------------
// read_X_client()

// A function that will read the given rows from the file.

// [in] row_start: The row number of the desired row.
// [in] n_rows_to_get: How many rows to read in.
// [out] dest: where the data should be put.  Allocated by the caller.
// [in] read_client_info: A pointer to the ReadXClientInfo struct you
//                        created in open_X_data().
// [in] meta: The metdata from read_X_meta().
// [in] data_type: e.g., RGB_BYTE, GREYSCALE_FLOAT, etc.

// return TRUE on success, FALSE on failure.

// You aren't allowed to customize the interface, here -- any additional
// info you want needs to be placed into the ReadXClientInfo struct.
int read_X_client(int row_start, int n_rows_to_get,
                  void *dest_void, void *read_client_info,
                  meta_parameters *meta, int data_type)
{
    // pick one of these!  And populate it.
    //float *dest = (float*)dest_void;
    //unsigned char *dest = (unsigned char*)dest_void;

    ReadXClientInfo *info = (ReadXClientInfo*)read_client_info;
    FILE *fp = info->fp;

    // here is where you populate "dest"

    return FALSE; // TRUE;
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
// [in] data_type: e.g., RGB_BYTE, GREYSCALE_FLOAT, etc.
// [out] dest: where the data should be put.  Allocated by the caller.

// You should use an asfPercentMeter while reading in the data.

// You aren't allowed to customize the interface, here -- any additional
// info you want needs to be placed into the ReadXClientInfo struct.

// However, you are allowed to completely eliminate this -- set the
// thumbnail function pointer to NULL.  In that case, the read_X code
// will be called to generate the thumbnail.

int get_X_thumbnail_data(FILE *fp, int thumb_size_x, int thumb_size_y,
                         meta_parameters *meta, void *read_client_info,
                         void *dest_void, int data_type)
{
    // pick one of these!  And populate it.
    //float *dest = (float*)dest_void;
    //unsigned char *dest = (unsigned char*)dest_void;

    ReadXClientInfo *info = (ReadXClientInfo*)read_client_info;
    FILE *fp = info->fp;

    // here is where you populate "dest"
    return FALSE;
}

//----------------------------------------------------------------------
// free_X_client_info()

// You probably don't need to change this beyond updating "X"
// Frees the client info structure

// You aren't allowed to customize the interface, here -- the cache
// has a pointer to this function that is typedef'ed.
void free_X_client_info(void *read_client_info)
{
    ReadXClientInfo *info = (ReadXClientInfo*)read_client_info;
    if (info->fp) fclose(info->fp);
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

// The only place this is called is in "read.c/read_file()", so you can
// customize the signature to fit your needs.  The read_jpeg client, for
// example, combined this with "read_x_meta".
int open_X_data(const char *data_name, const char *meta_name,
                const char *band, meta_parameters *meta,
                ClientInterface *client)
{
    ReadXClientInfo *info = MALLOC(sizeof(ReadXClientInfo));
    
    info->fp = fopen(data_name, "rb");
    // populate the rest of the info block here

    client->read_client_info = info;
    client->read_fn = read_X_client;
    client->thumb_fn = get_X_thumbnail_data; // NULL is allowed
    client->free_fn = free_X_client_info;

    // You must set this to something!
    // Be sure what you return for "dest" in the read client
    // functions is of this type.  This is passed in as the
    // "data_type" parameter to the read client, and the thumbnail
    // loader.
    client->data_type = UNDEFINED;

    return FALSE;
}
