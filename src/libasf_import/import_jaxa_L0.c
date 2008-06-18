// Import a JAXA Level 0 (JL0) dataset into ASF Internal Format (.img, .meta)
// Applies only to ALOS PRISM and AVNIR-2 Level 0 data
//

#include "asf.h"
#include "asf_nan.h"
#include "asf_import.h"
#include "import_jaxa_L0.h"

// Defs
#define JL0_DIR_LEN     1024
#define JL0_FILE_LEN    256
#define JL0_BLUE_VCID   45
#define JL0_GREEN_VCID  46
#define JL0_RED_VCID    47
#define JL0_NIR_VCID    48

// Prototypes
void get_chunk_names(const char *red_dir, const char *green_dir,
                     const char *blue_dir, const char *nir_dir,
                     int *num_chunks, char ***red_chunks,
                     char ***green_chunks, char ***blue_chunks,
                     char ***nir_chunks);

void import_jaxa_L0(const char *inBaseName, const char *outBaseName) {
    asfPrintError("Ingest of JAXA Level 0 (PRISM and AVNIR-2 Level 0) data not yet supported.\n");
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

    asfRequire(inBaseName && strlen(inBaseName) > 0, "Invalid inBaseName\n");
    asfRequire(outBaseName && strlen(outBaseName) > 0, "Invalid outBaseName\n");

    // Build the subfolders where the various bands exist
    char red_dir[JL0_DIR_LEN];
    char green_dir[JL0_DIR_LEN];
    char blue_dir[JL0_DIR_LEN];
    char nir_dir[JL0_DIR_LEN];
    sprintf(red_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_RED_VCID);
    sprintf(green_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_GREEN_VCID);
    sprintf(blue_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_BLUE_VCID);
    sprintf(nir_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_NIR_VCID);

    // Build base data chunk names
    int num_chunks = 0;
    char **red_chunks=NULL;
    char **green_chunks=NULL;
    char **blue_chunks=NULL;
    char **nir_chunks=NULL;
    get_chunk_names(red_dir, green_dir, blue_dir, nir_dir, &num_chunks,
                    &red_chunks, &green_chunks,
                    &blue_chunks, &nir_chunks);

    // Clean up
    int i;
    for (i=0; i<num_chunks; i++) {
        FREE(red_chunks[i]);
        FREE(green_chunks[i]);
        FREE(blue_chunks[i]);
        FREE(nir_chunks[i]);
    }
    FREE(red_chunks);
    FREE(green_chunks);
    FREE(blue_chunks);
    FREE(nir_chunks);
}

void get_chunk_names(const char *red_dir, const char *green_dir,
                     const char *blue_dir, const char *nir_dir,
                     int *num_chunks, char ***red_chunks,
                     char ***green_chunks, char ***blue_chunks,
                     char ***nir_chunks)
{
    // Systematically build and look for names of file chunks, populating
    // name arrays with a cardinal list of existent files for each band

    /*char file[JL0_FILE_LEN];
    char red_path[JL0_DIR_LEN];
    char green_path[JL0_DIR_LEN];
    char blue_path[JL0_DIR_LEN];
    char nir_path[JL0_DIR_LEN];*/
    int num_red_files=0;
    int num_green_files=0;
    int num_blue_files=0;
    int num_nir_files=0;

    // Count the files and allocate the file name arrays
    num_red_files = numFiles(red_dir);
    num_green_files = numFiles(green_dir);
    num_blue_files = numFiles(blue_dir);
    num_nir_files = numFiles(nir_dir);
    if (!(num_red_files == num_green_files == num_blue_files == num_nir_files)) {
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
    else {
        *num_chunks = num_red_files; // Since all 4 bands have the same number, just pick one...
    }
}

