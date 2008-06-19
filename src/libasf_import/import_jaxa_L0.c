// Import a JAXA Level 0 (JL0) dataset into ASF Internal Format (.img, .meta)
// Applies only to ALOS PRISM and AVNIR-2 Level 0 data
//
#include <sys/stat.h>
#include <dirent.h>
#include <ctype.h>
#include "asf.h"
#include "asf_nan.h"
#include "asf_import.h"
#include "get_ceos_names.h" // For enum typdef with 'AVNIR', 'PRISM' etc in it
#include "import_jaxa_L0.h"

// Prototypes
int compare_file_key(const void* file1, const void *file2); // For stdlib qsort() function


// import_jaxa_L0()
// 1. inBaseName is the name of the folder that the data is in, i.e. W0306544001-01
// 2. The various pieces of ancilliary data and band (scan) data are in VCID subfolders, i.e.
//    Blue band (band 01) is VCID 45
//    Green band (band 02) is VCID 46
//    Red band (band 03) is VCID 47
//    Near-IR band (band 04) is VCID 48
// 3. Each band subfolder contains the scan data in a series of files, 'chunks'.  Each
//    'chunk' file contains JPEG type header information and scan data.  Individual scanlines
//    are distributed over several sub-scanlines within the 'chunk' files. Ex) 'chunk' files
//    contain 1100-byte lines of data (including line header) and several are typically included
//    in individual full scanlines of image data.
//
void import_jaxa_L0(const char *inBaseName, const char *outBaseName) {
    ceos_sensor_t sensor_type=AVNIR;

    asfPrintError("Ingest of JAXA Level 0 (PRISM and AVNIR-2 Level 0) data not yet supported.\n");

    asfRequire(inBaseName && strlen(inBaseName) > 0, "Invalid inBaseName\n");
    asfRequire(outBaseName && strlen(outBaseName) > 0, "Invalid outBaseName\n");

    // FIXME: Need to check if this is PRISM or AVNIR... But for now, we will NOT be
    // importing Prism JL0 format, so the following assumes AVNIR-2 data

    // Only support AVNIR JL0 format at this time
    if (sensor_type == PALSAR) {
        asfPrintError("Programming error.  No such thing as PALSAR JL0\n");
    }
    else if (sensor_type == PRISM) {
        asfPrintError("ALOS PRISM Level 0 not yet supported.\n");
    }

    // Build the subfolders where the various bands exist
    if (sensor_type == AVNIR) {
        char red_dir[JL0_DIR_LEN];
        char green_dir[JL0_DIR_LEN];
        char blue_dir[JL0_DIR_LEN];
        char nir_dir[JL0_DIR_LEN];
        sprintf(red_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_RED_VCID);
        sprintf(green_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_GREEN_VCID);
        sprintf(blue_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_BLUE_VCID);
        sprintf(nir_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_NIR_VCID);

        // Retrieve file names of each band's chunks (sub-files)
        int num_chunks = 0;
        char **red_chunks=NULL;
        char **green_chunks=NULL;
        char **blue_chunks=NULL;
        char **nir_chunks=NULL;
        get_avnir_chunk_names(red_dir, green_dir, blue_dir, nir_dir, &num_chunks,
                              &red_chunks, &green_chunks,
                              &blue_chunks, &nir_chunks);

        // Clean up
        free_avnir_chunk_names(num_chunks,
                               &red_chunks, &green_chunks,
                               &blue_chunks, &nir_chunks);
    }
    else if (sensor_type == PRISM) {
        asfPrintError("ALOS PRISM Level 0 not yet supported.\n");
    }
    else {
        asfPrintError("Invalid or unrecognized ALOS Level 0 format found.\n");
    }
}

void get_avnir_chunk_names(const char *red_dir, const char *green_dir,
                     const char *blue_dir, const char *nir_dir,
                     int *num_chunks, char ***red_chunks,
                     char ***green_chunks, char ***blue_chunks,
                     char ***nir_chunks)
{
    ///////////////////////////////////////////////////////////////////////
    // Systematically build and look for names of file chunks, populating
    // name arrays with a cardinal list of existent files for each band
    //
    file_key_t *red_files;
    file_key_t *green_files;
    file_key_t *blue_files;
    file_key_t *nir_files;
    int num_red_files=0;
    int num_green_files=0;
    int num_blue_files=0;
    int num_nir_files=0;

    // Count the files and allocate the file name arrays
    num_red_files   = numFiles(red_dir);
    num_green_files = numFiles(green_dir);
    num_blue_files  = numFiles(blue_dir);
    num_nir_files   = numFiles(nir_dir);
    if (num_red_files == num_green_files &&
        num_red_files == num_blue_files  &&
        num_red_files == num_nir_files)
    {
        *num_chunks = num_red_files; // Since all 4 bands have the same number, just pick one...
    }
    else {
        *num_chunks = (num_red_files < num_green_files) ? num_red_files  : num_green_files;
        *num_chunks = (*num_chunks > num_blue_files)    ? num_blue_files :
                      (*num_chunks > num_nir_files)     ? num_nir_files  : 0;
        asfPrintWarning("Not all bands have the same number of file chunks in their VCID folders.\n"
                "  VCID %d (blue): %d\n  VCID %d (green): %d\nVCID %d (red): %d\n  VCID %d (near-IR): %d\n"
                "Assuming that the first %d chunks in each VCID subdirectory go together\n"
                "and continuing...\n",
                       JL0_BLUE_VCID,  num_blue_files,
                       JL0_GREEN_VCID, num_green_files,
                       JL0_RED_VCID,   num_red_files,
                       JL0_NIR_VCID,   num_nir_files,
                       *num_chunks);
    }

    // Allocate arrays of names
    // (CALLING ROUTINE MUST FREE THIS MEMORY ...BEST TO USE free_avnir_chunk_names() FOR THAT
    int chunk;
    *red_chunks   = (char **)MALLOC(*num_chunks * sizeof(char*));
    *green_chunks = (char **)MALLOC(*num_chunks * sizeof(char*));
    *blue_chunks  = (char **)MALLOC(*num_chunks * sizeof(char*));
    *nir_chunks   = (char **)MALLOC(*num_chunks * sizeof(char*));
    red_files     = (file_key_t *)MALLOC(num_red_files * sizeof(file_key_t));
    green_files   = (file_key_t *)MALLOC(num_red_files * sizeof(file_key_t));
    blue_files    = (file_key_t *)MALLOC(num_red_files * sizeof(file_key_t));
    nir_files     = (file_key_t *)MALLOC(num_red_files * sizeof(file_key_t));
    for (chunk = 0; chunk < *num_chunks; chunk++) {
        (*red_chunks)[chunk]   = (char *)MALLOC((JL0_DIR_LEN + JL0_FILE_LEN + 1) * sizeof(char));
        (*green_chunks)[chunk] = (char *)MALLOC((JL0_DIR_LEN + JL0_FILE_LEN + 1) * sizeof(char));
        (*blue_chunks)[chunk]  = (char *)MALLOC((JL0_DIR_LEN + JL0_FILE_LEN + 1) * sizeof(char));
        (*nir_chunks)[chunk]   = (char *)MALLOC((JL0_DIR_LEN + JL0_FILE_LEN + 1) * sizeof(char));
    }

    // Populate the temporary arrays of (keyed) names with the actual file names found in each VCID folder
    get_jaxa_L0_files(red_files  , num_red_files  , red_dir);
    get_jaxa_L0_files(green_files, num_green_files, green_dir);
    get_jaxa_L0_files(blue_files , num_blue_files , blue_dir);
    get_jaxa_L0_files(nir_files  , num_nir_files  , nir_dir);

    // Numerically sort the filenames for each VCID according to the Signal Data File Number
    // and the Sequential Number for each Signal Data File Number
    qsort(red_files  , num_red_files  , sizeof(file_key_t), compare_file_key);
    qsort(green_files, num_green_files, sizeof(file_key_t), compare_file_key);
    qsort(blue_files , num_blue_files , sizeof(file_key_t), compare_file_key);
    qsort(nir_files  , num_nir_files  , sizeof(file_key_t), compare_file_key);

    // Copy the sorted names to the returned arrays
    for (chunk=0; chunk < *num_chunks; chunk++) {
        strcpy((*red_chunks)[chunk]  , red_files[chunk].file);
        strcpy((*green_chunks)[chunk], green_files[chunk].file);
        strcpy((*blue_chunks)[chunk] , blue_files[chunk].file);
        strcpy((*nir_chunks)[chunk]  , nir_files[chunk].file);
    }

    // Clean up
    FREE(red_files);
    FREE(green_files);
    FREE(blue_files);
    FREE(nir_files);
}

void free_avnir_chunk_names(int num_chunks,
                      char ***red_chunks, char ***green_chunks,
                      char ***blue_chunks, char ***nir_chunks)
{
    int chunk;

    if (*red_chunks != NULL && num_chunks > 0) {
        for (chunk = 0; chunk < num_chunks; chunk++) {
            FREE((*red_chunks)[chunk]);
        }
        FREE(*red_chunks);
        *red_chunks = NULL;
    }
    if (*green_chunks != NULL && num_chunks > 0) {
        for (chunk = 0; chunk < num_chunks; chunk++) {
            FREE((*green_chunks)[chunk]);
        }
        FREE(*green_chunks);
        *green_chunks = NULL;
    }
    if (*blue_chunks != NULL && num_chunks > 0) {
        for (chunk = 0; chunk < num_chunks; chunk++) {
            FREE((*blue_chunks)[chunk]);
        }
        FREE(*blue_chunks);
        *blue_chunks = NULL;
    }
    if (*nir_chunks != NULL && num_chunks > 0) {
        for (chunk = 0; chunk < num_chunks; chunk++) {
            FREE((*nir_chunks)[chunk]);
        }
        FREE(*nir_chunks);
        *nir_chunks = NULL;
    }
}

void get_jaxa_L0_files(file_key_t *files, int num_files, const char *path)
{
    struct stat statbuf;
    struct dirent *dp;
    DIR *dir;
    int fileCount = 0;

    if (path && strlen(path) > 0) {
        dir = opendir(path);
    }
    else {
        dir = opendir(".");
    }

    while ((dp = readdir(dir)) != NULL) {
        char file[1024];
        if (dp->d_name && strlen(dp->d_name)) {
            sprintf(file, "%s%c%s", path, DIR_SEPARATOR, dp->d_name);
            if (stat(file, &statbuf) < 0) {
                continue;
            }
            else if (S_ISREG(statbuf.st_mode)) {
                if (strncmp(dp->d_name, "AL_AV2_45", 9) == 0 ||
                    strncmp(dp->d_name, "AL_AV2_46", 9) == 0 ||
                    strncmp(dp->d_name, "AL_AV2_47", 9) == 0 ||
                    strncmp(dp->d_name, "AL_AV2_48", 9) == 0) // FIXME: Add PRISM VCID cases
                {
                    if (fileCount >= num_files) {
                        asfPrintError("Too many VCID file chunks found!\n");
                    }
                    strcpy(files[fileCount].file, file);
                    fileCount++;
                }
            }
        }
        else {
            continue;
        }
    }
    if (fileCount < num_files) {
        asfPrintError("Not enough VCID file chunks found!\n");
    }
    int i;
    char *s, *a;
    char ascii_key[JL0_FILE_LEN];
    for (i=0; i < num_files; i++) {
        a = ascii_key;
        s = strrchr(files[i].file, (int)'_');
        if (!s) {
            asfPrintError("Invalid VCID filename.\n");
        }
        while (*s) {
            if (isdigit((int)*s)) {
                *a = *s;
                a++;
            }
            s++;
        }
        *a = '\0';
        files[i].key = strtol(ascii_key, (char **)NULL, 10);
        if (files[i].key < 0) {
            asfPrintError("Invalid Signal Data File Number in VCID filename\n");
        }
    }
}

int compare_file_key(const void* file1, const void *file2)
{
    file_key_t *f1 = (file_key_t *)file1;
    file_key_t *f2 = (file_key_t *)file2;

    return (f1->key - f2->key);
}
