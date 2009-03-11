// Import a JAXA Level 0 (JL0) dataset into ASF Internal Format (.img, .meta)
// Applies only to ALOS PRISM and AVNIR-2 Level 0 data
//
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <limits.h>
#include <string.h>
#include <sys/stat.h>
#include <dirent.h>
#include <time.h>
#include <setjmp.h>
#include "asf_jpeg.h"
#include "asf.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "asf_import.h"
#include "get_ceos_names.h" // For enum typdef with 'AVNIR', 'PRISM' etc in it
#include "import_jaxa_L0.h"

// Defs
#ifdef MIN
#  undef MIN
#endif
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#ifdef MAX
#  undef MAX
#endif
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

// Prototypes
static void jpeg_error_handler(j_common_ptr cinfo);
//static void jpeg_error_message_handler(j_common_ptr cinfo);

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
// FIXME: Add save_intermediates flag to import_jaxa_L0() and carry through to
// import_jaxa_L0_avnir_band(); ...OR... Remove the whole save-intermediates thing since
// nobody can read those pesky little 16-line jpegs anyway ...and fixing asf_view
// to do it would let you see a tiny slice of an image and that's not useful...
// FIXME: Need to support asf_import -band option...
void import_jaxa_L0(const char *inBaseName, const char *outBaseName) {
    int save_intermediates = 0;
    ceos_sensor_t sensor_type = AVNIR;

    //asfPrintError("Ingest of JAXA Level 0 (PRISM and AVNIR-2 Level 0) data not yet supported.\n");

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
    // FIXME: Try checking for the 'basename' folder existence and error out if
    // it is not there.  The import must be run from one folder above
    // the basename folder.  Optionally be smart about running from one above or
    // actually IN the basename folder ...probably a better idea.  This would effect
    // the sprintf()'s below where the path is built, then all should run
    // fine after that.

    // Build the subfolders where the various bands exist
    if (sensor_type == AVNIR) {
        char red_dir[JL0_DIR_LEN];
        char green_dir[JL0_DIR_LEN];
        char blue_dir[JL0_DIR_LEN];
        char nir_dir[JL0_DIR_LEN];
        sprintf(red_dir, "%s%c%d",   inBaseName, DIR_SEPARATOR, JL0_RED_VCID);
        sprintf(green_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_GREEN_VCID);
        sprintf(blue_dir, "%s%c%d",  inBaseName, DIR_SEPARATOR, JL0_BLUE_VCID);
        sprintf(nir_dir, "%s%c%d",   inBaseName, DIR_SEPARATOR, JL0_NIR_VCID);

        // Retrieve (sorted) file names of each band's chunks (sub-files)
        int num_red_chunks   = 0;
        int num_green_chunks = 0;
        int num_blue_chunks  = 0;
        int num_nir_chunks   = 0;
        char **red_chunks   = NULL;
        char **green_chunks = NULL;
        char **blue_chunks  = NULL;
        char **nir_chunks   = NULL;
        get_avnir_chunk_names(red_dir, green_dir, blue_dir, nir_dir,
                              &num_red_chunks, &num_green_chunks,
                              &num_blue_chunks, &num_nir_chunks,
                              &red_chunks, &green_chunks,
                              &blue_chunks, &nir_chunks);

        // Import the 4 color bands into a single ASF Internal format file and metadata file (.img, .meta)
        int red_lines;      // Number of band 03 (red) data lines read
        int green_lines;    // Number of band 02 (green) data lines read
        int blue_lines;     // Number of band 01 (blue) data lines read
        int nir_lines;      // Number of band 04 (near-infrared) data lines read
        int line_count = 0; // Number of lines (each band) in finished .img file (sample_count is always 7100)
        char *tmp_filename1;
        char tmp_filename2[JL0_DIR_LEN + JL0_FILE_LEN + 1];
        char *out_file;

        tmp_filename1 = appendExt(outBaseName, ""); // Strips extension but keeps path info
        sprintf(tmp_filename2, "%s", tmp_filename1); // Append Level 0 indication
        out_file = appendExt(tmp_filename2, ".img"); // Append a .img extension
        FREE(tmp_filename1);

        FILE *tmp = FOPEN(out_file, "wb"); // Create an empty .img file.  The imports will append...
        FCLOSE(tmp);

        // Import the bands, frame-synchronized, missing or errored-out data padded with zeros, bad bands discarded
        char bands[1024]; // List of bands for the metadata
        int num_bands; // Number of valid bands imported
        import_jaxa_L0_avnir_bands(&red_lines, &green_lines, &blue_lines, &nir_lines,
                                   red_chunks, green_chunks, blue_chunks, nir_chunks,
                                   num_red_chunks, num_green_chunks, num_blue_chunks, num_nir_chunks,
                                   bands, &num_bands, save_intermediates, out_file);
        line_count = MIN(nir_lines, MIN(red_lines, MIN(blue_lines, green_lines)));
        if (red_lines != green_lines ||
            red_lines != blue_lines  ||
            red_lines != nir_lines)
        {
            // This code should never be reached.  Scanning is done across all 4 bands at once,
            // starts with the starting frame, missing or damaged frames padded (zero filled) in place,
            // and terminated when either a) the data runs out (end of file) or b) the desired number of
            // frames has been read.  In other words, when doing a frame-synchronized import, the 4 bands
            // start and end together and therefore the number of lines read for each should be the same
            // unless there was a programming error.
            asfPrintWarning("AVNIR-2 Level 0 files do not all have the same number of lines:\n"
                    "  Number of red lines:            %d\n"
                    "  Number of green lines:          %d\n"
                    "  Number of blue lines:           %d\n"
                    "  Number of near-infrared lines:  %d\n"
                    "If these files are merged into a single color image, then this\n"
                    "have to be taken into consideration (missing data?)\n",
                    red_lines, green_lines, blue_lines, nir_lines);
        }
        FREE(out_file);

        // At this point, all 4 bands have been written into a single .img file consecutively
        // and it's time to produce the matching .meta file, then the import process will be
        // complete
        asfPrintStatus("Creating metadata...\n");
        meta_parameters *meta = raw_init();
        meta->optical = meta_optical_init();
        meta->location = meta_location_init();
        meta_general *mg = meta->general;
        meta_optical *mo = meta->optical; // Convenience ptr
        //meta_location *ml = meta->location; // Convenience ptr

        strcpy(mg->basename, inBaseName);
        strcpy(mg->sensor, "ALOS");
        strcpy(mg->sensor_name, "AVNIR");
        strcpy(mg->mode, "1.0"); // Might need to be "1A".  FIXME: Need to put radiometric and geometric correction
                                 // coefficients into the metadata
        strcpy(mg->processor, "JAXA");
        mg->data_type = BYTE;
        mg->image_data_type = AMPLITUDE_IMAGE; // FIXME: Should this be RAW for Level 0 _optical_ data?
        mg->radiometry = r_AMP;
        strcpy(mg->acquisition_date, "dd-mmm-yy"); // FIXME: Get the acquisition date out of the metadata!
        mg->orbit = MAGIC_UNSET_INT; // FIXME: Fill out the orbit number
        mg->orbit_direction = MAGIC_UNSET_CHAR; // FIXME: Fill out orbit direction, 'A' or 'D'
        mg->frame = -1; // FIXME: Is there an AVNIR frame number for this?
        mg->band_count = num_bands;
        strcpy(mg->bands, bands);
        mg->line_count = line_count;
        mg->sample_count = JL0_AVNIR_SAMPLE_COUNT;
        mg->start_line = 0;
        mg->start_sample = 0;
//        mg->line_scaling = 1.0;
//        mg->sample_scaling = 1.0;
        mg->x_pixel_size = 10.0;  // FIXME This needs to be derived from VCID 32 (I believe)
        mg->y_pixel_size = 10.0; // FIXME  Ditto...
        mg->center_latitude = MAGIC_UNSET_DOUBLE; // FIXME
        mg->center_longitude = MAGIC_UNSET_DOUBLE; // FIXME
        mg->re_major = MAGIC_UNSET_DOUBLE; // FIXME
        mg->re_minor = MAGIC_UNSET_DOUBLE; // FIXME
        mg->bit_error_rate = MAGIC_UNSET_DOUBLE;
        mg->missing_lines = MAGIC_UNSET_INT;
        mg->no_data = MAGIC_UNSET_DOUBLE;

        strcpy(mo->pointing_direction, "Off-nadir");
        mo->off_nadir_angle = MAGIC_UNSET_DOUBLE; // FIXME
        strcpy(mo->correction_level, MAGIC_UNSET_STRING); // FIXME
        mo->cloud_percentage = MAGIC_UNSET_DOUBLE; // FIXME
        mo->sun_azimuth_angle = MAGIC_UNSET_DOUBLE; // FIXME
        mo->sun_elevation_angle = MAGIC_UNSET_DOUBLE; // FIXME

        // FIXME: Add location data if possible

        char *meta_outfile = appendExt(tmp_filename2, ".img");
        meta_write(meta, meta_outfile);

        // Clean up
        free_avnir_chunk_names(num_red_chunks, num_green_chunks,
                               num_blue_chunks, num_nir_chunks,
                               &red_chunks, &green_chunks,
                               &blue_chunks, &nir_chunks);
        meta_free(meta);
        FREE(meta_outfile);
    }
    else if (sensor_type == PRISM) {
        asfPrintError("ALOS PRISM Level 0 not yet supported.\n");
    }
    else {
        asfPrintError("Invalid or unrecognized ALOS Level 0 format found.\n");
    }
    asfPrintStatus("\n");
}

// Returns arrays of names in each VCID (band) subdirectory, sorted in numerical
// order according to the Signal Data File Number.Sequence Number, ready for
// ordered extraction of signal data.
void get_avnir_chunk_names(const char *red_dir, const char *green_dir,
                           const char *blue_dir, const char *nir_dir,
                           int *num_red_chunks, int *num_green_chunks,
                           int *num_blue_chunks, int *num_nir_chunks,
                           char ***red_chunks, char ***green_chunks,
                           char ***blue_chunks, char ***nir_chunks)
{
    ///////////////////////////////////////////////////////////////////////
    // Systematically build and look for names of file chunks, populating
    // name arrays with a cardinal list of existent files for each band
    //
    file_key_t *red_files;
    file_key_t *green_files;
    file_key_t *blue_files;
    file_key_t *nir_files;

    // Count the files and allocate the file name arrays
    *num_red_chunks   = numFiles(red_dir);
    *num_green_chunks = numFiles(green_dir);
    *num_blue_chunks  = numFiles(blue_dir);
    *num_nir_chunks   = numFiles(nir_dir);

    // Allocate arrays of names
    // (CALLING ROUTINE MUST FREE THIS MEMORY ...BEST TO USE free_avnir_chunk_names() FOR THAT
    int chunk;
    *red_chunks   = (char **)MALLOC(*num_red_chunks   * sizeof(char*));
    *green_chunks = (char **)MALLOC(*num_green_chunks * sizeof(char*));
    *blue_chunks  = (char **)MALLOC(*num_blue_chunks  * sizeof(char*));
    *nir_chunks   = (char **)MALLOC(*num_nir_chunks   * sizeof(char*));
    red_files     = (file_key_t *)MALLOC(*num_red_chunks   * sizeof(file_key_t));
    green_files   = (file_key_t *)MALLOC(*num_green_chunks * sizeof(file_key_t));
    blue_files    = (file_key_t *)MALLOC(*num_blue_chunks  * sizeof(file_key_t));
    nir_files     = (file_key_t *)MALLOC(*num_nir_chunks   * sizeof(file_key_t));
    for (chunk = 0; chunk < *num_red_chunks; chunk++) {
        (*red_chunks)[chunk]   = (char *)MALLOC((JL0_DIR_LEN + JL0_FILE_LEN + 1) * sizeof(char));
    }
    for (chunk = 0; chunk < *num_green_chunks; chunk++) {
        (*green_chunks)[chunk]   = (char *)MALLOC((JL0_DIR_LEN + JL0_FILE_LEN + 1) * sizeof(char));
    }
    for (chunk = 0; chunk < *num_blue_chunks; chunk++) {
        (*blue_chunks)[chunk]   = (char *)MALLOC((JL0_DIR_LEN + JL0_FILE_LEN + 1) * sizeof(char));
    }
    for (chunk = 0; chunk < *num_nir_chunks; chunk++) {
        (*nir_chunks)[chunk]   = (char *)MALLOC((JL0_DIR_LEN + JL0_FILE_LEN + 1) * sizeof(char));
    }

    // Populate the temporary arrays of (keyed) names with the actual file names found in each VCID folder
    get_jaxa_L0_files(red_files  , *num_red_chunks  , red_dir);
    get_jaxa_L0_files(green_files, *num_green_chunks, green_dir);
    get_jaxa_L0_files(blue_files , *num_blue_chunks , blue_dir);
    get_jaxa_L0_files(nir_files  , *num_nir_chunks  , nir_dir);

    // Numerically sort the filenames for each VCID according to the Signal Data File Number
    // and the Sequential Number for each Signal Data File Number
    qsort(red_files  , *num_red_chunks  , sizeof(file_key_t), compare_file_key);
    qsort(green_files, *num_green_chunks, sizeof(file_key_t), compare_file_key);
    qsort(blue_files , *num_blue_chunks , sizeof(file_key_t), compare_file_key);
    qsort(nir_files  , *num_nir_chunks  , sizeof(file_key_t), compare_file_key);

    // Copy the sorted names to the returned arrays
    for (chunk=0; chunk < *num_red_chunks; chunk++) {
        strcpy((*red_chunks)[chunk]    , red_files[chunk].file);
    }
    for (chunk=0; chunk < *num_green_chunks; chunk++) {
        strcpy((*green_chunks)[chunk]  , green_files[chunk].file);
    }
    for (chunk=0; chunk < *num_blue_chunks; chunk++) {
        strcpy((*blue_chunks)[chunk]   , blue_files[chunk].file);
    }
    for (chunk=0; chunk < *num_nir_chunks; chunk++) {
        strcpy((*nir_chunks)[chunk]    , nir_files[chunk].file);
    }

    // Clean up
    FREE(red_files);
    FREE(green_files);
    FREE(blue_files);
    FREE(nir_files);
}

void free_avnir_chunk_names(int num_red_chunks, int num_green_chunks,
                            int num_blue_chunks, int num_nir_chunks,
                            char ***red_chunks, char ***green_chunks,
                            char ***blue_chunks, char ***nir_chunks)
{
    int chunk;

    if (*red_chunks != NULL && num_red_chunks > 0) {
        for (chunk = 0; chunk < num_red_chunks; chunk++) {
            FREE((*red_chunks)[chunk]);
        }
        FREE(*red_chunks);
        *red_chunks = NULL;
    }
    if (*green_chunks != NULL && num_green_chunks > 0) {
        for (chunk = 0; chunk < num_green_chunks; chunk++) {
            FREE((*green_chunks)[chunk]);
        }
        FREE(*green_chunks);
        *green_chunks = NULL;
    }
    if (*blue_chunks != NULL && num_blue_chunks > 0) {
        for (chunk = 0; chunk < num_blue_chunks; chunk++) {
            FREE((*blue_chunks)[chunk]);
        }
        FREE(*blue_chunks);
        *blue_chunks = NULL;
    }
    if (*nir_chunks != NULL && num_nir_chunks > 0) {
        for (chunk = 0; chunk < num_nir_chunks; chunk++) {
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

// Reads one telemetry frame (a VCDU - virtual channel data unit) from the input file,
//   skips past header, returns the remaining line as data (there is no trailing info to
//   remove... the Reed-Solomon error correction code for correcting bit errors has already
//   been utilized by the downlink and then stripped from the telemetry frame as the frame
//   is written to storage ...a stored t-frame contains 6 bytes of CCSDS header followed by
//   1094 bytes of actual data, a total length of 1100 bytes.)
//
// CCSDS Header Contents:
//   - First 2 bytes,
//     - Version (2 bits), always 01b
//     - Spacecraft ID (6 bits), always 63h for ALOS
//     - VCID (virtual channel identification number), 45d for Avnir-2 Band 01 etc...
//   - Next 3 bytes,
//     - The VCDU counter value for this frame (should always be positive and never rolls over)
//   - Last byte,
//     - Replay Flag (1 bit) (Can be ignored)
//     - Spare bits (7 bits)
//
// IMPORTANT: THE CALLING FUNCTION MUST FREE THIS DATA LINE
size_t get_avnir_data_line(FILE *in, unsigned char **data, int first_vcdu, int *last_vcdu_ctr,
                           int *continuous, int *missing_bytes, int *vcdu_ctr) {
    size_t bytes_read = 0;
    unsigned char *line;
    int i;
    unsigned char *d=NULL;

    line  = (unsigned char *)MALLOC((JL0_AVNIR_TFRAME_LEN + 1) * sizeof(unsigned char));
    *data = (unsigned char *)MALLOC(JL0_AVNIR_TFRAME_DATA_LEN * sizeof(unsigned char));
    if (!feof(in)) {
        bytes_read = FREAD_CHECKED(line, 1, JL0_AVNIR_TFRAME_LEN, in, 1);
    }
    *vcdu_ctr = ((unsigned int)line[2]<<16) + ((unsigned int)line[3]<<8) + (unsigned int)line[4];
    *continuous = (!first_vcdu && *vcdu_ctr - *last_vcdu_ctr == 1) ? 1 : 0;
    *missing_bytes = (!(*continuous)) ? (*vcdu_ctr - *last_vcdu_ctr - 1) * JL0_AVNIR_TFRAME_DATA_LEN : 0;
    for (i = 0, d = line + JL0_AVNIR_CCSDS_HDR_LEN;
         bytes_read > (JL0_AVNIR_CCSDS_HDR_LEN + 1) && i < (bytes_read - JL0_AVNIR_CCSDS_HDR_LEN);
         i++, d++)
    {
        (*data)[i] = *d;
    }
    free(line);
    *last_vcdu_ctr = *vcdu_ctr;

    return bytes_read - JL0_AVNIR_CCSDS_HDR_LEN;
}

// Read each file (chunk) in the list of files for this band, converting
// each frame into ASF Internal format individually.
//
// IMPORTANT NOTES:
// 1. The Jaxa Level 0 AVNIR data exists across several sub-files (chunks),
//    each in their own subdirectory (by VCID number, i.e. ./<basename>/<vcid>)
// 2. Each subfile is made up from 1100-byte int lines of binary information,
//    the first 6-bytes of which are CCSDS header information.  These bytes can
//    be skipped over (ignored.)  The remaining 1094 bytes are to be parsed as
//    described below.  These 1100-byte lines are telemetry frames, not to be
//    confused with the lossless compression (Huffman) JPEG frames mentioned
//    in the following notes (just called 'frames' below.)
// 3. Each sub-file contains several frames of data,
// 4. Each frame (typically) contains 16 scanlines of actual image data.
// 5. Each frame is stored in the file(s) within their own jpeg 'image' format
//    (starting with JPEG SOI and ending with JPEG EOI markers.)  There may be
//    one to three 0x00 padding bytes after the EOI in order to make a frame land
//    on a 32-bit boundary (these bytes should be ignored.)
// 6. The frames can and do span sub-file boundaries.
// 7. Each line of data contains 7100 actual data pixels (bytes), but including other
//    information are actually 7152 bytes int.
// 8. Each line is divided into odd and even pixels, and there is other information before
//    and after the actual data.  The format of each line is as follows (number of bytes in
//    parenthesis):
//
//     a. Dummy bytes (4)
//     b. Optical black (4)
//     c. Optical white (4)
//     d. Valid ODD pixels (3550) <-- Here's the actual data (odd pixels)
//     e. Optical white (4)
//     f. Dummy bytes (2)
//     g. Electrical calibration (8)
//     h. Dummy bytes (4)
//     i. Optical black (4)
//     j. Optical white (4)
//     k. Valid EVEN pixels (3550)  <-- Here's the actual data (even pixels)
//     l. Optical white (4)
//     m. Dummy bytes (2)
//     n. Electrical calibration (8)
//
//    The odd and even pixels need to be interlaced to restore the data line.  Note that
//    this is NOT consistent with the JPEG standard.  Note also that the data lines are
//    compressed in the jpeg frames and therefore the data must be re-interlaced after
//    those data lines are decompressed (seems obvious, eh?).  See import_avnir_frame_jpeg_to_img()
//    for the pixel interlacing code.
// 9. The JAXA format also includes a non-standard JPEG marker and payload (the information
//    bytes which follow the 0xFF marker byte).  This marker is 0xFFF0 and is RESERVED in
//    the JPEG standard and is not supposed to be used.  In order for the JPEG library to
//    work this marker and payload must be ignored and not stored in any JPEG file.
//
int import_jaxa_L0_avnir_bands(int *red_lines, int *green_lines, int *blue_lines, int *nir_lines,
                               char **red_chunks, char **green_chunks, char **blue_chunks, char **nir_chunks,
                               int num_red_chunks, int num_green_chunks, int num_blue_chunks, int num_nir_chunks,
                               char *bands, int *num_bands, int save_intermediates, const char *out_file)
{
    int rframe_number, gframe_number, bframe_number, nframe_number;
    char jpegRed[JL0_DIR_LEN + JL0_FILE_LEN + 1];
    char red_all_chunks[JL0_DIR_LEN + JL0_FILE_LEN + 1];
    char jpegGreen[JL0_DIR_LEN + JL0_FILE_LEN + 1];
    char green_all_chunks[JL0_DIR_LEN + JL0_FILE_LEN + 1];
    char jpegBlue[JL0_DIR_LEN + JL0_FILE_LEN + 1];
    char blue_all_chunks[JL0_DIR_LEN + JL0_FILE_LEN + 1];
    char jpegNir[JL0_DIR_LEN + JL0_FILE_LEN + 1];
    char nir_all_chunks[JL0_DIR_LEN + JL0_FILE_LEN + 1];
    char tmp_folder[256];
    FILE *rin = NULL;
    FILE *gin = NULL;
    FILE *bin = NULL;
    FILE *nin = NULL;
    time_t t;
    char t_stamp[32];

    asfPrintStatus("\nImporting all available color bands...\n\n");

    // Make temporary folder for temporary files
    char *out_base = get_basename(out_file);
    t = time(NULL);
    strftime(t_stamp, 22, "%d%b%Y-%Hh_%Mm_%Ss", localtime(&t));
    sprintf(tmp_folder, "./asf_import_tmp_dir_%s_%s", out_base, t_stamp);
    if (!is_dir(tmp_folder)) {
        create_dir(tmp_folder);
    }
    else {
        // Should never reach here
        asfPrintError("Temporary folder already exists:\n    %s\n",
                      tmp_folder);
    }

    // Build input and output filenames
    sprintf(jpegRed, "%s%c%s_%s.jpg", tmp_folder, DIR_SEPARATOR, out_base, JL0_RED_BAND);
    sprintf(red_all_chunks, "%s%call_chunks_band_%s", tmp_folder, DIR_SEPARATOR, JL0_RED_BAND);
    sprintf(jpegGreen, "%s%c%s_%s.jpg", tmp_folder, DIR_SEPARATOR, out_base, JL0_GREEN_BAND);
    sprintf(green_all_chunks, "%s%call_chunks_band_%s", tmp_folder, DIR_SEPARATOR, JL0_GREEN_BAND);
    sprintf(jpegBlue, "%s%c%s_%s.jpg", tmp_folder, DIR_SEPARATOR, out_base, JL0_BLUE_BAND);
    sprintf(blue_all_chunks, "%s%call_chunks_band_%s", tmp_folder, DIR_SEPARATOR, JL0_BLUE_BAND);
    sprintf(jpegNir, "%s%c%s_%s.jpg", tmp_folder, DIR_SEPARATOR, out_base, JL0_NIR_BAND);
    sprintf(nir_all_chunks, "%s%call_chunks_band_%s", tmp_folder, DIR_SEPARATOR, JL0_NIR_BAND);

    // Open the output files
    // Note: out_file contains a complete path including the basename and .img extension
    char dirName[1024], fileName[1024], outFile[1024];
    split_dir_and_file(out_file, dirName, fileName);
    char *rout_file = (char *)MALLOC((strlen(out_file) + 64) * sizeof(char));
    char *gout_file = (char *)MALLOC((strlen(out_file) + 64) * sizeof(char));
    char *bout_file = (char *)MALLOC((strlen(out_file) + 64) * sizeof(char));
    char *nout_file = (char *)MALLOC((strlen(out_file) + 64) * sizeof(char));
    sprintf(outFile, "red_%s"  , fileName);
    sprintf(rout_file, "%s%s"  , dirName, outFile);
    sprintf(outFile, "green_%s"  , fileName);
    sprintf(gout_file, "%s%s"  , dirName, outFile);
    sprintf(outFile, "blue_%s"  , fileName);
    sprintf(bout_file, "%s%s"  , dirName, outFile);
    sprintf(outFile, "nir_%s"  , fileName);
    sprintf(nout_file, "%s%s"  , dirName, outFile);
    FILE *rout = (FILE *)FOPEN(rout_file, "wb");
    FILE *gout = (FILE *)FOPEN(gout_file, "wb");
    FILE *bout = (FILE *)FOPEN(bout_file, "wb");
    FILE *nout = (FILE *)FOPEN(nout_file, "wb");

    // Concatenate all chunks for each band into a single files while at the same
    // time preserving telemetry frame CCSDS headers.  These files will be processed
    // as a telemetry byte stream.  Putting all the chunks into a single temporary file
    // simplifies the stream processing code ...if disk space is at a premium, then the
    // function that reads bytes from the stream not only needs to handle telemetry frame
    // boundaries but also file boundaries.
    // FIXME: If we make fread_tframe() able to read not only from one telemetry frame to
    // the next (the current level of functionality), but also from one file to the
    // next, then there would be _no need_ to create these large all-chunks files
    // that contain all available telemetry frames for each band ...and we'd save disk
    // space (Operations would like that)
    asfPrintStatus("Building telemetry files...\n\nRed... ");
    concat_avnir_band_chunks(num_red_chunks,   (const char **)red_chunks,   red_all_chunks,   JL0_RED_BAND);
    asfPrintStatus("\nGreen... ");
    concat_avnir_band_chunks(num_green_chunks, (const char **)green_chunks, green_all_chunks, JL0_GREEN_BAND);
    asfPrintStatus("\nBlue... ");
    concat_avnir_band_chunks(num_blue_chunks,  (const char **)blue_chunks,  blue_all_chunks,  JL0_BLUE_BAND);
    asfPrintStatus("\nNear-Infrared...");
    concat_avnir_band_chunks(num_nir_chunks,   (const char **)nir_chunks,   nir_all_chunks,   JL0_NIR_BAND);

    asfPrintStatus("\nPerforming time-synchronized ingest of embedded AVNIR-2 jpeg-format\n"
            "frames and converting into ASF Internal format...\n\n");

    int soi_tries = 0;  // Zero means 'try to find an SOI as many times as you want'
    int r_eof = 0;      // True if red band reached end of file
    int g_eof = 0;      // True if green band reached end of file
    int b_eof = 0;      // True if blue band reached end of file
    int n_eof = 0;      // True if near-infrared band reached end of file

    // Determine scene extraction time
    // => Find first SOI in each band and take the latest one as the scene extraction
    // time.  FIXME: This is not the way the ALOS doc says to do it ...when fixing asf_import,
    // this should be fixed.  Here's the right way:
    //
    // t = t_0 - (F / 2) * s_r * 16, where
    //
    // t   == scene extraction start time
    // t_0 == center time of scene extraction
    // F   == total number of frames to extract
    // s_r == the imaging cycle (1.48 m sec/line for AVNIR, 0.37 m sec/line for PRISM)
    // 16  == number of data lines per jpeg frame (sixteen)
    //
    int rvalid, gvalid, bvalid, nvalid;
    float rtime = 0.0, gtime = 0.0, btime = 0.0, ntime = 0.0, target_time = 0.0, scene_extraction_time = 0.0;
    float last_rtime = 0.0, last_gtime = 0.0, last_btime = 0.0, last_ntime = 0.0;
    unsigned char rbuf[JL0_AVNIR_TFRAME_LEN+1], rhdr_buf[JL0_AVNIR_JPEG_HDR_LEN+1];
    unsigned char gbuf[JL0_AVNIR_TFRAME_LEN+1], ghdr_buf[JL0_AVNIR_JPEG_HDR_LEN+1];
    unsigned char bbuf[JL0_AVNIR_TFRAME_LEN+1], bhdr_buf[JL0_AVNIR_JPEG_HDR_LEN+1];
    unsigned char nbuf[JL0_AVNIR_TFRAME_LEN+1], nhdr_buf[JL0_AVNIR_JPEG_HDR_LEN+1];
    int ridx = -1, gidx = -1, bidx = -1, nidx = -1; // Negative initial value triggers reading the first VCDU
    int rlast_vcdu_ctr = 0, glast_vcdu_ctr = 0, blast_vcdu_ctr = 0, nlast_vcdu_ctr = 0;
    int valid_rSOI, valid_gSOI, valid_bSOI, valid_nSOI, first_SOI;
    int first_vcdu; // A flag that says we are looking for the very first telemetry frame in a file

    //// Find first SOI in the telemetry files
    // Time target of NULL (2nd parameter in find_next_avnir_SOI() function) below means "any valid frame
    // scene extraction time" rather than searching for a specific target time
    // Open the input files (the all-chunks files)
    rin = (FILE *)FOPEN(red_all_chunks  , "rb");
    gin = (FILE *)FOPEN(green_all_chunks, "rb");
    bin = (FILE *)FOPEN(blue_all_chunks , "rb");
    nin = (FILE *)FOPEN(nir_all_chunks  , "rb");
    first_vcdu = 1; // Turn on when finding first SOI (only)
    soi_tries = 0; // Zero means 'search until first valid SOI or end of file is found'
    target_time = 0.0;
    first_SOI = 1;

    asfPrintStatus("\nFinding start of image... \n    Red ...\n");
    r_eof = find_next_avnir_SOI(rin, target_time, soi_tries, &rvalid, &rtime,
                          &last_rtime, rbuf, &ridx, JL0_RED_BAND_NO, first_vcdu, first_SOI, &rlast_vcdu_ctr,
                          rhdr_buf, &valid_rSOI); // Find first SOI
    asfPrintStatus("        Green ...\n");
    g_eof = find_next_avnir_SOI(gin, target_time, soi_tries, &gvalid, &gtime,
                          &last_gtime, gbuf, &gidx, JL0_GREEN_BAND_NO, first_vcdu, first_SOI, &glast_vcdu_ctr,
                          ghdr_buf, &valid_gSOI); // Find first SOI
    asfPrintStatus("            Blue ...\n");
    b_eof = find_next_avnir_SOI(bin, target_time, soi_tries, &bvalid, &btime,
                          &last_btime, bbuf, &bidx, JL0_BLUE_BAND_NO, first_vcdu, first_SOI, &blast_vcdu_ctr,
                          bhdr_buf, &valid_bSOI); // Find first SOI
    asfPrintStatus("                Near-Infrared\n\n");
    n_eof = find_next_avnir_SOI(nin, target_time, soi_tries, &nvalid, &ntime,
                          &last_ntime, nbuf, &nidx, JL0_NIR_BAND_NO, first_vcdu, first_SOI, &nlast_vcdu_ctr,
                          nhdr_buf, &valid_nSOI); // Find first SOI
    first_SOI = 0; // Turn off for remainder of processing
    FCLOSE(rin);
    FCLOSE(gin);
    FCLOSE(bin);
    FCLOSE(nin);
    // Pick the scene extraction time as the latest of what occurred in the first valid jpeg frame.
    // Note: find_next_avnir_SOI() returns a time of 0.0 for invalid results
    scene_extraction_time = MAX(ntime, MAX(btime, MAX(rtime, gtime)));

    // Synchronize the start, i.e. leave file pointers pointing at first valid SOI for each band (or
    // invalidate the whole band if the start criteria was not met ...see note below)
    rin = (FILE *)FOPEN(red_all_chunks  , "rb"); // Now that we have a target start time, re-open the files
    gin = (FILE *)FOPEN(green_all_chunks, "rb"); // and begin anew...
    bin = (FILE *)FOPEN(blue_all_chunks , "rb");
    nin = (FILE *)FOPEN(nir_all_chunks  , "rb");
    first_vcdu = 1; // Turn on when finding first SOI (only)
    soi_tries = 2; // The ALOS spec says to check only the first 2 valid SOIs at or after the target synch time
    asfPrintStatus("Synchronizing bands to scene extraction start time...\n");
    r_eof = find_next_avnir_SOI(rin, scene_extraction_time, soi_tries, &rvalid, &rtime,
                        &last_rtime, rbuf, &ridx, JL0_RED_BAND_NO, first_vcdu, first_SOI, &rlast_vcdu_ctr, rhdr_buf,
                        &valid_rSOI); // Find target starting SOI
    g_eof = find_next_avnir_SOI(gin, scene_extraction_time, soi_tries, &gvalid, &gtime,
                        &last_gtime, gbuf, &gidx, JL0_GREEN_BAND_NO, first_vcdu, first_SOI, &glast_vcdu_ctr, ghdr_buf,
                        &valid_gSOI); // Find target starting SOI
    b_eof = find_next_avnir_SOI(bin, scene_extraction_time, soi_tries, &bvalid, &btime,
                        &last_btime, bbuf, &bidx, JL0_BLUE_BAND_NO, first_vcdu, first_SOI, &blast_vcdu_ctr, bhdr_buf,
                        &valid_bSOI); // Find target starting SOI
    n_eof = find_next_avnir_SOI(nin, scene_extraction_time, soi_tries, &nvalid, &ntime,
                        &last_ntime, nbuf, &nidx, JL0_NIR_BAND_NO, first_vcdu, first_SOI, &nlast_vcdu_ctr, nhdr_buf,
                        &valid_nSOI); // Find target starting SOI
    first_vcdu = 0; // Turn flag off for remainder of processing
    int rband_valid = (valid_rSOI && !r_eof) ? 1 : 0; // Valid if found a jpeg frame in 2 tries or less and it
    int gband_valid = (valid_gSOI && !g_eof) ? 1 : 0; // was in the correct target (time) window and the end
    int bband_valid = (valid_bSOI && !b_eof) ? 1 : 0; // of file was not hit (per ALOS spec)
    int nband_valid = (valid_nSOI && !n_eof) ? 1 : 0;
    rframe_number = 1;
    gframe_number = 1;
    bframe_number = 1;
    nframe_number = 1;

    // Update number of bands and the band string
    // Note: In ASF Internal Format, bands are typically written in blue->green->red->near-infrared order
    // (in order from shortest wavelengths to longest, just like the satellite), and the band are to be
    // labeled "01" for blue, "02" for green, "03" for red, and "04" for near-infrared ...invalidated or
    // missing bands are skipped
    *num_bands = ((rband_valid) ? 1 : 0) +
                 ((gband_valid) ? 1 : 0) +
                 ((bband_valid) ? 1 : 0) +
                 ((nband_valid) ? 1 : 0);
    sprintf(bands, "%s%s", (bband_valid)          ? JL0_BLUE_BAND  : "",
            (bband_valid && (gband_valid || rband_valid || nband_valid)) ? "," : "");
    sprintf(bands, "%s%s%s", bands, (gband_valid) ? JL0_GREEN_BAND : "",
            (gband_valid && (rband_valid || nband_valid))                ? "," : "");
    sprintf(bands, "%s%s%s", bands, (rband_valid) ? JL0_RED_BAND   : "",
            rband_valid && nband_valid                                   ? "," : "");
    sprintf(bands, "%s%s", bands, nband_valid     ? JL0_NIR_BAND   : "");

    // Write and convert first frame
    // Note: The find_next_avnir_SOI() function leaves the data index pointing at the
    // first byte after the SOS and the header buffer filled with all the original jpeg
    // header markers.
    // Note: If the band has been discarded, nothing is written to that band.  For valid
    // bands, if any errors exist or occur for the frame, then a frame's worth of padding
    // (0x00) bytes are written out instead.
    write_avnir_frame(bband_valid, valid_bSOI, bvalid, bin, bhdr_buf, bbuf, &bidx, JL0_BLUE_BAND_NO,
                      &bvalid, first_vcdu, &blast_vcdu_ctr, jpegBlue,  bout);
    write_avnir_frame(gband_valid, valid_gSOI, gvalid, gin, ghdr_buf, gbuf, &gidx, JL0_GREEN_BAND_NO,
                      &gvalid, first_vcdu, &glast_vcdu_ctr, jpegGreen, gout);
    write_avnir_frame(rband_valid, valid_rSOI, rvalid, rin, rhdr_buf, rbuf, &ridx, JL0_RED_BAND_NO,
                      &rvalid, first_vcdu, &rlast_vcdu_ctr, jpegRed,   rout);
    write_avnir_frame(nband_valid, valid_nSOI, nvalid, nin, nhdr_buf, nbuf, &nidx, JL0_NIR_BAND_NO,
                      &nvalid, first_vcdu, &nlast_vcdu_ctr, jpegNir, nout);

    // Read and convert the rest of the frames, padding missing frames exist and then
    // terminating when any single input file reaches it's end.  If frames are missing, the
    // only pad if the time calculation from current frame to last valid frame is a integer
    // scaler of the frame time else terminate ingest, i.e. only pad frames if the EXACT number
    // of lines of missing data can be calculated from the difference in frame times ...this
    // is risky and maybe we shouldn't be doing it?
    int done;
    target_time = 0.0;
    asfPrintStatus("Converting band data jpeg frames (16x7100) into ASF Internal Format...\n");
    while (!feof(rin) && !feof(gin) && !feof(bin) && !feof(nin) && !done) {
        // Write frames that are within tolerance of each other, padding those that are later
        // and out of tolerance.  If the difference between the earliest frame time and the last
        // is greater than one frame time (w/tolerance) then frames are missing ...write padding on
        // all valid bands IFF the difference in time is an exact multiple of frame time (within one
        // imaging cycle's time, i.e. azimuth time per pixel for AVNIR-2).  ONLY read new frames on each
        // cycle IF that band's frame WAS written, i.e. don't read new frames for a particular band
        // if it is waiting to re-synch.
        //
        r_eof = find_next_avnir_SOI(rin, target_time, soi_tries, &rvalid, &rtime,
                      &last_rtime, rbuf, &ridx, JL0_RED_BAND_NO, first_vcdu, first_SOI, &rlast_vcdu_ctr, rhdr_buf,
                      &valid_rSOI);
        g_eof = find_next_avnir_SOI(gin, target_time, soi_tries, &gvalid, &gtime,
                      &last_gtime, gbuf, &gidx, JL0_GREEN_BAND_NO, first_vcdu, first_SOI, &glast_vcdu_ctr, bhdr_buf,
                      &valid_gSOI);
        b_eof = find_next_avnir_SOI(bin, target_time, soi_tries, &bvalid, &btime,
                      &last_btime, bbuf, &bidx, JL0_BLUE_BAND_NO, first_vcdu, first_SOI, &blast_vcdu_ctr, ghdr_buf,
                      &valid_bSOI);
        n_eof = find_next_avnir_SOI(nin, target_time, soi_tries, &nvalid, &ntime,
                      &last_ntime, nbuf, &nidx, JL0_NIR_BAND_NO, first_vcdu, first_SOI, &nlast_vcdu_ctr, nhdr_buf,
                      &valid_nSOI);

        write_avnir_frame(bband_valid, valid_bSOI, bvalid, bin, bhdr_buf, bbuf, &bidx, JL0_BLUE_BAND_NO,
                          &bvalid, first_vcdu, &blast_vcdu_ctr, jpegBlue,  bout);
        write_avnir_frame(gband_valid, valid_gSOI, gvalid, gin, ghdr_buf, gbuf, &gidx, JL0_GREEN_BAND_NO,
                          &gvalid, first_vcdu, &glast_vcdu_ctr, jpegGreen, gout);
        write_avnir_frame(rband_valid, valid_rSOI, rvalid, rin, rhdr_buf, rbuf, &ridx, JL0_RED_BAND_NO,
                          &rvalid, first_vcdu, &rlast_vcdu_ctr, jpegRed,   rout);
        write_avnir_frame(nband_valid, valid_nSOI, nvalid, nin, nhdr_buf, nbuf, &nidx, JL0_NIR_BAND_NO,
                          &nvalid, first_vcdu, &nlast_vcdu_ctr, jpegNir, nout);
        asfPrintStatus(".");
    }

    // Close and concat the .img files
    FCLOSE(rout);
    FCLOSE(gout);
    FCLOSE(bout);
    FCLOSE(nout);
    char *img_files[*num_bands];
    int num_files = 0;
    if (bband_valid) img_files[num_files++] = bout_file;
    if (gband_valid) img_files[num_files++] = gout_file;
    if (rband_valid) img_files[num_files++] = rout_file;
    if (nband_valid) img_files[num_files]   = nout_file;
    num_files = *num_bands;
    asfPrintStatus("\nCombining band data into single ASF format file...\n\n");
    int tot_lines = concat_img_files(img_files, num_files, JL0_AVNIR_SAMPLE_COUNT, out_file);
    *red_lines = *green_lines = *blue_lines = *nir_lines = tot_lines;
    asfPrintStatus("\n\n");

    // Clean up
    FREE(out_base);
    char del_files[1024];
    int i;
    for (i = 0; i < num_files; i++) {
        sprintf(del_files, "rm -f %s", img_files[i]);
//        printf("\nWould have run this command: %s\n\n", del_files);
        asfSystem(del_files);
    }
    sprintf(del_files, "rm -rf %s", tmp_folder);
//    printf("\nWould have run this command: %s\n\n", del_files);
    asfSystem(del_files);

    return tot_lines;
}

// Reads a jpeg file and appends all data lines to the end of an .img file
// Assumes 'out' is opened for append, and it's up to the programmer to make
// sure that lines written in several calls to this function are all the same
// length (sample count) and data type.
//
// Uncompressed jpeg line format (see ALOS spec):
// -----------------------------------------------
// Each line is divided into odd and even pixels, and there is other information before
// and after the actual data.  The format of each line is as follows (number of bytes in
// parenthesis):
//
//     a. Dummy bytes (4)
//     b. Optical black (4)
//     c. Optical white (4)
//     d. Valid ODD pixels (3550) <-- Here's the actual data (odd pixels)
//     e. Optical white (4)
//     f. Dummy bytes (2)
//     g. Electrical calibration (8)
//     h. Dummy bytes (4)
//     i. Optical black (4)
//     j. Optical white (4)
//     k. Valid EVEN pixels (3550)  <-- Here's the actual data (even pixels)
//     l. Optical white (4)
//     m. Dummy bytes (2)
//     n. Electrical calibration (8)
//
// The odd and even pixels need to be interlaced to restore the data line.
//
static int jpeg_error_occurred = 0;
static int frame_lines_written = 0;
int import_avnir_frame_jpeg_to_img(const char *tmpJpegName, int good_frame, FILE *out)
{
    FILE *jpeg = NULL; // Input jpeg file
    JSAMPARRAY ibuf;
    unsigned char *dest = NULL;
    int num_lines = 0;
    struct jpeg_decompress_struct cinfo;
    my_error_mgr_t jerr;

    if (good_frame) {
        // Open and initialize jpeg
        jpeg = (FILE *)FOPEN(tmpJpegName, "rb");
        jerr.errmgr.error_exit = jpeg_error_handler;
//        jerr.errmgr.output_message = jpeg_error_message_handler;

        // Intialize jpeg decompression
        cinfo.err = jpeg_std_error(&jerr.errmgr);
        if (setjmp(jerr.escape)) {
            // If we get here, a jpeg library found an error
            jpeg_error_occurred++;
            if (jpeg_error_occurred > MAX_ALLOWED_JPEG_ERRORS) {
                // This will trap runaway errors, i.e. if they occur during jpeg_finish_decompress() or
                // in jpeg_destroy_decompress() ...or if there really are tons of bad jpeg frames!
                asfPrintError("Too many jpeg frame errors occurred (%d) ...aborting\n", jpeg_error_occurred);
            }
            jpeg_destroy_decompress (&cinfo);
            FCLOSE(jpeg);
            return frame_lines_written;
        }
        jpeg_create_decompress(&cinfo);
        jpeg_stdio_src(&cinfo, jpeg);
        jpeg_read_header(&cinfo, TRUE);
        jpeg_start_decompress(&cinfo);

        // Allocate input and output buffers
        if (cinfo.num_components != 1) {
            asfPrintError("Multi-component jpegs not supported for JAXA Level 0 AVNIR-2 products...\n");
        }
//        ibuf[0] = (unsigned char *)MALLOC(cinfo.output_width);
        ibuf = (*cinfo.mem->alloc_sarray)((j_common_ptr)&cinfo, JPOOL_IMAGE, cinfo.output_width, 1);
        dest = (unsigned char *)MALLOC((2 * 3550) * sizeof(unsigned char)); // greyscale, 1 component

        // Read/decompress/interlace/write jpeg data
        num_lines = cinfo.image_height;
        frame_lines_written = 0;
        while (cinfo.output_scanline < cinfo.image_height) {
            jpeg_read_scanlines(&cinfo, ibuf, 1); // Read and decompress one data line

            int j = 12;             // Point at odds
            int k = 12 + 3550 + 26; // Point at evens
            int m, n;
            for (m = n = 0; m < 3550; m++, j++, k++) {
                dest[n]     = (unsigned char)ibuf[0][j]; // Odd pixel
                dest[n + 1] = (unsigned char)ibuf[0][k]; // Even pixel
                n += 2;
            }
            FWRITE(dest, sizeof(unsigned char), 2 * 3550, out); // Write raw interlaced data to .img file
            frame_lines_written++;
        }

        // Clean up
        jpeg_finish_decompress(&cinfo);
        jpeg_destroy_decompress(&cinfo);
        FCLOSE(jpeg);
//        FREE(ibuf[0]);
        FREE(dest);
    }
    else {
        // Invalid jpeg frame, so just pad the output file with blank data (0x00)
        int i;
        dest = (unsigned char *)MALLOC((2 * 3550) * sizeof(unsigned char)); // greyscale, 1 component
        for (i = 0; i < JL0_AVNIR_SAMPLE_COUNT   ; i++) {
            dest[i] = PAD;
        }
        num_lines = JL0_AVNIR_LINES_PER_FRAME;
        for (i = 0; i < num_lines; i++) {
            FWRITE(dest, sizeof(unsigned char), 2 * 3550, out);
        }
        FREE(dest);
    }

    return num_lines;
}

// Concatenates all VCID chunks into a single file ..and preserves 1100-byte telemetry frame structure
// (including 6-byte CCSDS header at the beginning of each t-frame)
void concat_avnir_band_chunks(int num_chunks, const char **chunks, const char *all_chunks, const char *band)
{
    int chunk;
    int bytes_read;
    int lines_read = 0;
    unsigned char *data = NULL;
    FILE *in = NULL;
    FILE *out = NULL;

    asfPrintStatus("\n\nCombining sub-files for band %s...\n\n", band);

    out =(FILE *)FOPEN(all_chunks, "wb");
    data = (unsigned char *)MALLOC(JL0_AVNIR_TFRAME_LEN * sizeof(unsigned char));
    for (chunk = 0; chunk < num_chunks; chunk++) {
        in = (FILE *)FOPEN(chunks[chunk], "rb");
        bytes_read = FREAD_CHECKED(data, sizeof(unsigned char), JL0_AVNIR_TFRAME_LEN, in, 1);
        while (bytes_read == JL0_AVNIR_TFRAME_LEN && !feof(in)) {
            lines_read++;
            FWRITE(data, sizeof(unsigned char), JL0_AVNIR_TFRAME_LEN, out);
            bytes_read = FREAD_CHECKED(data, sizeof(unsigned char), JL0_AVNIR_TFRAME_LEN, in, 1);
            asfLineMeter(lines_read, JL0_AVNIR_CHUNK_SIZE);
        }
        FCLOSE(in);
        asfLineMeter(JL0_AVNIR_CHUNK_SIZE, JL0_AVNIR_CHUNK_SIZE);
        lines_read = 0;
    }
    FREE(data);
    FCLOSE(out);
}

// Returns non-zero if end of file is reached while searching for the next SOI
// A jpeg frame (starting with an SOI) is invalid if a) the time delay from the last
// valid frame to the current frame is longer than it should've been (missing frames), or
// b) the scene extraction time of the current frame is outside the window allowed by the
// target time plus a tolerance
int find_next_avnir_SOI(FILE *in, float time_target, int max_tries, int *valid_vcdu,
                        float *time, float *last_time, unsigned char *buf, int *idx,
                        int band_no, int first_vcdu, int first_SOI, int *last_vcdu_ctr,
                        unsigned char *hdr_buf, int *valid_SOI)
{
    unsigned char c, mID;
    int num_read, try;
    float min_time, max_time;

    *time = 0.0;
    *valid_SOI = 0;
    *valid_vcdu = 0;
    try = 0;
    max_tries = (max_tries <= 0) ? MAXIMUM_SOI_SEARCH_TRIES : max_tries;
    while (!feof(in) && max_tries > 0 && try < max_tries) {
        num_read = fread_avnir_tstream(&c, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, valid_vcdu);
        if (valid_vcdu && num_read > 0 && c == MARKER) {
            num_read = fread_avnir_tstream(&mID, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, valid_vcdu);
            if (valid_vcdu && num_read > 0 && mID == SOI) {
                hdr_buf[0] = MARKER;
                hdr_buf[1] = SOI;
                // Check validity of SOI (checks validity of jpeg header structure) and
                // get time from aux. data (if jpeg frame is valid)
                validate_avnir_SOI_and_get_frame_time (in, buf, idx, first_vcdu, first_SOI, last_vcdu_ctr,
                                                       hdr_buf, band_no, valid_SOI, time);

                if (*valid_SOI) {
                    if (!first_SOI) {
                        // If the jpeg frame structure was valid (valid_SOI), then check to see
                        // if the timing meets requirements ...returning the first jpeg frame that
                        // meets the timing requirements
                        min_time = time_target - JL0_AVNIR_FRAME_TIME_TOLERANCE;
                        max_time = time_target + JL0_AVNIR_FRAME_TIME_REPEATS * JL0_AVNIR_FRAME_TIME +
                                                 JL0_AVNIR_FRAME_TIME_TOLERANCE;
                        if (*time >= min_time && max_tries > 0) {
                            // Per ALOS spec ...some SOI searches are required to be found within
                            // a set number of tries at or greater the target time
                            try++;
                        }
                        if (time_target == 0.0  ||
                            (time_target > 0.0  && *time >= min_time && *time <= max_time))
                        {
                            // Scene extraction time for this jpeg frame was inside the target window
                            // so keep it...
                            *last_time = *time;
                            break;
                        }
                        else {
                            // Scene extraction time for this jpeg frame was outside the target window
                            *valid_SOI = 0;
                            *time = 0;
                        }
                    }
                    else {
                        // Always keep the very first SOI
                        *last_time = *time;
                        break;
                    }
                }
            }
        }
    }

    return (feof(in));
}

// fread_avnir_tstream() returns a byte (one byte only for now) from a pseudo-telemetry stream.
// It assumes that bytes come from a single file, the file being made up of a series of 1100-byte
// telemetry frames (6-byte CCSDS header followed by 1094 bytes of data).  fread_avnir_tstream()
// returns the number of characters read (should always be 1) along with a 'valid' flag that
// sets to untrue (zero) when either the end of file is reached without being able to read
// a byte or if a freshly-read telemetry frame has a non-sequential VCDU counter value (indicates
// missing telemetry frames)
//
// FIXME: It would be nice if...
//  a) the new t-frame could be read from the next file in a sequence, i.e. when the buffer
//     runs out being able to read t-frames from a set of files rather than requiring a single
//     concatenated temporary file, and
//  b) you could read any number of bytes from the the stream at once like the real fread() allows
//
// Note: Upon return, *idx is left pointing at the last character read
//
static int _valid_vcdu[4] = {0,0,0,0};
int fread_avnir_tstream(unsigned char *c, FILE *in, unsigned char *buf, int first_vcdu,
                        int *idx, int band_no, int *last_vcdu_ctr, int *valid_vcdu)
{
    int num_read = 0;
    int band = band_no - 1;

    // Check to see if we need to refresh the buffer, and refresh if necessary
    *valid_vcdu = _valid_vcdu[band];
    if (*idx < 0 || *idx > JL0_AVNIR_TFRAME_LEN - 1) {
        // NOTE: idx needs to be initialized to a negative number
        // to trigger the first read (see calling function)
        num_read = fread(buf, 1, JL0_AVNIR_TFRAME_LEN, in);
        if (feof(in) || num_read != JL0_AVNIR_TFRAME_LEN) {
            // Incomplete read of new telemetry frame
            num_read = 0;
            *valid_vcdu = _valid_vcdu[band] = 0; // Only update when reading a telemetry frame
            *c = PAD;
        }
        else {
            // Successful read of new telemetry frame ...but was it sequential?

            // Check for validity of the new t-frame, i.e. make sure the VCDU counter is sequential
            // after the last t-frame read (non-sequential indicates missing t-frames ...missing
            // data.  This invalidates the entire frame).  (Skip checking for continuity if the t-frame
            // is the first one read)
            int vcdu_ctr = ((unsigned int)buf[2]<<16) + ((unsigned int)buf[3]<<8) + (unsigned int)buf[4];
            *valid_vcdu  = _valid_vcdu[band] = (first_vcdu) ? 1 :
                                               (vcdu_ctr - *last_vcdu_ctr == 1) ? 1 : 0;
            *last_vcdu_ctr = vcdu_ctr;

            // Return a byte from the newly-read t-frame
            if (*valid_vcdu) {
                *idx = 6; // Step past CCSDS header, i.e. fread_avnir_tstream() only returns real data, not hdr
                *c = buf[(*idx)++];
                num_read = 1;
            }
            else {
                num_read = 0;
                *c = PAD;
            }
        }
    }
    else {
        // Current telemetry frame hasn't been read completely ...so use it.  Leave *valid_vcdu flag
        // at previous value (only change when reading a new t-frame ...so the validity value stays
        // the same for the entire t-frame as it should since that's what it represents)
        *c = buf[(*idx)++];
        num_read = 1;
    }

    return num_read;
}

// If the SOI is a valid SOI (not just data that contained a marker followed by the SOI value and is not corrup
// in some way) returns the scene extraction time for a single frame of data.  The valid_vcdu flag is set to
// zero (untrue, 'invalid') if the SOI is not a true one or is corrupt in some way ...in which case, the time
// value will also be set to zero (which by default makes it an invalid time since it is earlier than the
// required scene extraction start time for the entire image ...some code may depend on this!)
// NOTE: The header buffer contains 0xFFD8 (jpeg marker plus start of image id) and this function needs
// to populate the header buffer with the whole 256-byte (according to ALOS spec) jpeg header
static float _last_valid_frame_time[4] = {0,0,0,0};
void validate_avnir_SOI_and_get_frame_time (FILE *in, unsigned char *buf, int *idx, int first_vcdu,
                                            int first_SOI, int *last_vcdu_ctr, unsigned char *hdr_buf,
                                            int band_no, int *valid_SOI, float *time)
{
    int num_read;
    int valid_vcdu;
    int band = band_no - 1; // Bands numbered 01 - 04, last valid frame time array indexed from 0 - 3
    int hdr_idx = 2; // header buffer already contains 0xFFD8 (SOI - start of fram marker, 2 bytes)
    unsigned char c, mID;

    num_read = fread_avnir_tstream(&c, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, valid_SOI);
    hdr_buf[hdr_idx++] = c;
    if (num_read == 1 && *valid_SOI) {
        // Cheap validity check ...if the jpeg frame starts with 0xFFD8FFC3 or FFD8FFC0, then
        // assume this is a valid frame.  If it is NOT actually valid, then it is highly likely
        // that the time calculation (below) will produce a very whacky time ...and consequently,
        // this frame will get tossed as being invalid anyway.
        num_read = fread_avnir_tstream(&mID, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, valid_SOI);
        hdr_buf[hdr_idx++] = mID;
        *valid_SOI = (num_read == 1 && *valid_SOI && (mID == SOF0 || mID == SOF3)) ? 1 : 0;
    }
    if (*valid_SOI) {
        // SOI marker and SOFx markers are valid and in the right spot ...call it good
        // Now move to APP0 (note: The ALOS docs often mistakenly call it APP1) and read the
        // scene extraction time for this frame
        //
        // fseek() 18 bytes forward...
        int i;
        for (i=0; i<18; i++) { // 11 byte SOFx payload + marker & ID + 5 JPG0 payload
            num_read = fread_avnir_tstream(&c, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
            if (num_read == 1 && valid_vcdu) {
                hdr_buf[hdr_idx++] = c;
            }
        }

        num_read = 0;
        if (valid_vcdu) {
            num_read = fread_avnir_tstream(&c, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
            if (num_read == 1 && valid_vcdu) {
                hdr_buf[hdr_idx++] = c;
            }
        }
        if (num_read == 1 && valid_vcdu && c == MARKER) {
            num_read = fread_avnir_tstream(&mID, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
            hdr_buf[hdr_idx++] = mID;
            if (num_read == 1 && valid_vcdu && mID == APP0) {
                // Read APP0 payload length (2 bytes) and error byte (1 byte)
                unsigned char c1, c2, c3, error_byte;
                num_read  = fread_avnir_tstream(&c1, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
                num_read += fread_avnir_tstream(&c2, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
                num_read += fread_avnir_tstream(&error_byte, in, buf, first_vcdu, idx, band_no,
                        last_vcdu_ctr, &valid_vcdu);
                hdr_buf[hdr_idx++] = c1;
                hdr_buf[hdr_idx++] = c2;
                hdr_buf[hdr_idx++] = error_byte;
                if (num_read == 3 && valid_vcdu &&
                    !(error_byte & APP0_AUX_MEMORY_BUFFER_OVERFLOW)) // The ALOS spec only says to check for overflow
                {
                    // Ok ...looks like a valid APP0 was found, so let's derive the scene extraction time
                    // for this frame.  Read and discard 2 bytes (GPS Week), then read and keep the
                    // next 3 bytes to get the GPS seconds, then read 2 more bytes to get the line
                    // counter.  Time is equal to GPS seconds plus lines * 1.48 msec (Avnir-2 image cycle time)
                    //
                    num_read  = fread_avnir_tstream(&c1, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
                    num_read += fread_avnir_tstream(&c2, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
                    hdr_buf[hdr_idx++] = c1;
                    hdr_buf[hdr_idx++] = c2;

                    num_read += fread_avnir_tstream(&c1, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
                    num_read += fread_avnir_tstream(&c2, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
                    num_read += fread_avnir_tstream(&c3, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
                    hdr_buf[hdr_idx++] = c1;
                    hdr_buf[hdr_idx++] = c2;
                    hdr_buf[hdr_idx++] = c3;
                    float gps_seconds = (float)(((unsigned long)c1<<16) + ((unsigned long)c2<<8) + (unsigned long)c3);

                    num_read += fread_avnir_tstream(&c1, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
                    num_read += fread_avnir_tstream(&c2, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
                    hdr_buf[hdr_idx++] = c1;
                    hdr_buf[hdr_idx++] = c2;
                    float line_ctr = (float)(((unsigned long)c1<<8) + (unsigned long)c2);

                    if (num_read == 7 && valid_vcdu) {
                        // Calculate the time (ignoring effect of electrical noise and imaging cycle
                        // timing errors).  Time comparisons must always use a tolerance window.
                        *time = gps_seconds + line_ctr * AVNIR_IMAGING_CYCLE;
                        if (first_SOI) {
                            *valid_SOI = 1;
                            _last_valid_frame_time[band] = *time;
                        }
                        else {
                            float delta_time = *time - _last_valid_frame_time[band];
                            if (*time < _last_valid_frame_time[band] - JL0_AVNIR_FRAME_TIME_TOLERANCE ||
                                 delta_time > (JL0_AVNIR_FRAME_TIME_REPEATS * JL0_AVNIR_FRAME_TIME) +
                                               JL0_AVNIR_FRAME_TIME_TOLERANCE)
                            {
                                // For now, discard out of sequence frames (frames with an earlier time
                                // stamp) and missing frames (delta time since last valid frame too large)
                                *valid_SOI = 0;
                                *time = 0;
                            }
                            else {
                                // Timing requirements were met, so save the current time as the 'last valid'
                                // time.
                                *valid_SOI = 1;
                                _last_valid_frame_time[band] = *time;
                            }
                        }
                    }
                    else {
                        // Either file was short or vcdu ctr was out of sequence
                        *valid_SOI = 0;
                        *time = 0;
                    }
                }
                else {
                    // Either file was short, vcdu ctr was out of sequence, or a memory buffer overflow occurred
                    *valid_SOI = 0;
                    *time = 0;
                }
            }
            else {
                // APP0 did not follow the SOF as stated in the ALOS spec (or the file was short, vcdu ctr
                // out of order etc)
                *valid_SOI = 0;
                *time = 0;
            }
        }
        else {
            // Either file was short, vcdu ctr was out of sequence, or no marker found yet
            *valid_SOI = 0;
            *time = 0;
        }
    }
    else {
        // Either file was short, vcdu ctr was out of sequence, or the SOI marker was not followed by
        // a SOFx marker, i.e. the jpeg header is invalid
        *time = 0;
    }

    // If all is valid so far, then populate the rest of the header buffer
    // This leaves the data idx pointing at the first byte after the start of scan, i.e. right after
    // the SOS marker ID byte
    int i;
    if (*valid_SOI && *time > 0) {
        for (i = hdr_idx; i < JL0_AVNIR_JPEG_HDR_LEN; i++) {
            num_read = fread_avnir_tstream(&c, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
            if (num_read == 1 && valid_vcdu) {
                hdr_buf[hdr_idx++] = c;
            }
            if (c == MARKER) {
                num_read = fread_avnir_tstream(&mID, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, &valid_vcdu);
                i++;
                if (num_read == 1 && valid_vcdu) {
                    hdr_buf[hdr_idx++] = mID;
                }
                if (mID == SOS) {
                    break;
                }
            }
        }
    }
}

int read_write_avnir_jpeg_frame(FILE *in, unsigned char *hdr_buf, unsigned char *buf, int *idx,
                                 int band_no, int *valid, int first_vcdu, int *last_vcdu_ctr, FILE *jpeg)
{
    int i, num_read, success = 1;
    unsigned char c, mID;
    unsigned int payload_length;

    // Write the jpeg header.  The header buffer contains the entire original jpeg header as
    // found in the ALOS JL0 files, which implies that the extra markers and payloads (one of
    // which is actually invalid since it uses a marker reserved for jpeg library usage only)
    // still exist and should not be written out.  The good news is that you can depend on the
    // header starting with the 0xFFD8 SOI marker combination and ending with the 0xFFDA SOS
    // (start of scan) marker combination... after the header, the data itself (up to the end
    // of image, EOI) needs to be read/written to the jpeg file (only)
    //
    // 1) Write the SOI to the file
    i = 0;
    FWRITE(&hdr_buf[i++], 1, 1, jpeg); // Should be 0xFF
    FWRITE(&hdr_buf[i++], 1, 1, jpeg); // Should be 0xD8
    // 2) Write the remainder of the header bytes, skipping the non-standard or meaningless
    //    marker/payload combinations
    while (i < JL0_AVNIR_JPEG_HDR_LEN) {
        if (hdr_buf[i] == MARKER) {
            if (i + 1 > JL0_AVNIR_JPEG_HDR_LEN) {
                success = 0;
                break;
            }
            mID = hdr_buf[i + 1];
            if (mID == JPG0 ||
                mID == APP0)
            {
                // Found a discardable marker and payload ...fseek() past it.
                if (i + 3 > JL0_AVNIR_JPEG_HDR_LEN) {
                    success = 0;
                    break;
                }
                payload_length = hdr_buf[i + 2];
                payload_length = payload_length << 8;
                payload_length += hdr_buf[i + 3];
                i += 2 + payload_length; // Move past this marker and payload without writing anything out
            }
            else if (mID == SOS) {
                // Found start of scan ...just write the marker out and let the loop (below) write all other
                // bytes (up to end of image, EOI)
                FWRITE(&hdr_buf[i++], 1, 1, jpeg);
                FWRITE(&hdr_buf[i++], 1, 1, jpeg);
            }
            else {
                // We are still in the block of header info that precedes the scan, so this must
                // be another marker ...one that we are not ignoring, so read/write the payload
                if (i + 4 > JL0_AVNIR_JPEG_HDR_LEN) {
                    success = 0;
                    break;
                }
                FWRITE(&hdr_buf[i++], 1, 1, jpeg); // Should be 0xFF
                FWRITE(&hdr_buf[i++], 1, 1, jpeg); // Should be the marker ID
                payload_length = hdr_buf[i]; // Shift left to give bits proper values
                payload_length = payload_length << 8;
                FWRITE(&hdr_buf[i++], 1, 1, jpeg); // Write upper byte of payload size
                payload_length += hdr_buf[i];
                FWRITE(&hdr_buf[i++], 1, 1, jpeg); // Write lower byte of payload size
                payload_length -= 2; // Length includes marker and id (already written ..need to skip these)
                if (i + payload_length > JL0_AVNIR_JPEG_HDR_LEN) {
                    success = 0;
                    break;
                }
                int byte;
                for (byte = 0; byte < payload_length && i < JL0_AVNIR_JPEG_HDR_LEN; byte++) {
                    // Write the rest of the marker's payload
                    FWRITE(&hdr_buf[i++], 1, 1, jpeg);
                }
                if (i >= JL0_AVNIR_JPEG_HDR_LEN && hdr_buf[i - 1] != SOS) {
                    success = 0;
                    break;
                }
            }
        }
        else {
            FWRITE(&hdr_buf[i++], 1, 1, jpeg);
        }
    }

    // Read/write the rest of the jpeg from the input t-stream
    if (success) {
        num_read = fread_avnir_tstream(&c, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, valid);
        if (num_read != 1 || !*valid) {
            success = 0;
        }
    }
    int EOI_found = 0;
    while (num_read == 1 && !feof(in) && success) {
        FWRITE(&c, 1, 1, jpeg);
        if (c == MARKER) {
            num_read = fread_avnir_tstream(&mID, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, valid);
            if (num_read == 1 && !feof(in) && success && *valid) {
                FWRITE(&mID, 1, 1, jpeg);
                if (mID == EOI) {
                    EOI_found = 1;
                    break;
                }
            }
            else {
                success = 0;
                break;
            }
        }
        num_read = fread_avnir_tstream(&c, in, buf, first_vcdu, idx, band_no, last_vcdu_ctr, valid);
        if (num_read != 1 || feof(in) || !*valid) {
            success = 0;
            break;
        }
    }

    // If the read/write loop terminated without finding the EOI then something is
    // wrong (end of file reached, invalid VCDU counter sequence, etc)... mark this
    // frame as a failure
    if (!EOI_found) {
        success = 0;
    }

    return success;
}

void write_avnir_frame(int band_valid, int valid_SOI, int valid_vcdu, FILE *in,
                       unsigned char *hdr_buf, unsigned char *buf, int *idx, int band_no, int *valid,
                       int first_vcdu, int *last_vcdu_ctr, char *jpegFile, FILE *out)
{
    int good_frame;
    if (valid_SOI && valid_vcdu) {
        // Read/Write jpeg
        FILE *jpeg = (FILE *)FOPEN(jpegFile, "wb");
        good_frame = read_write_avnir_jpeg_frame(in, hdr_buf,
                                                 buf, idx, band_no,
                                                 valid, first_vcdu, last_vcdu_ctr,
                                                 jpeg);
        FCLOSE(jpeg);
    }
    else {
        good_frame = 0;
    }

    // Convert temporary jpeg to .img format
    // Note: If the SOI is not valid or an invalid vcdu ctr was encountered (etc)
    // then import_avnir_frame_to_img() will skip using the jpeg and will just
    // write out the appropriate amount of pad bytes (0x00), i.e. blank data
    import_avnir_frame_jpeg_to_img(jpegFile, good_frame, out);
}

// Limitation:  Handles only BYTE images at this time
int concat_img_files(char **img_files, int num_files, int sample_count, const char *out_file) {
    FILE **file;
    FILE *out = NULL;
    int i, tot_lines, done;
    int *lines;
    unsigned char *buf;
    int sizeof_buf;

    file = (FILE **)MALLOC(num_files * sizeof(FILE *));
    lines = (int *)MALLOC(num_files * sizeof(int));
    sizeof_buf = sample_count * sizeof(unsigned char);
    buf = (unsigned char *)MALLOC(sizeof_buf);

    // Determine max number of lines that can be read from all files
    for (i = 0, tot_lines = INT_MAX; i < num_files; i++) {
        asfPrintStatus("Determining line count in:\n  %s\n", img_files[i]);
        lines[i] = 0;
        file[i] = (FILE *)FOPEN(img_files[i], "rb");
        lines[i] += fread(buf, sizeof_buf, 1, file[i]);
        while (!feof(file[i])) {
            lines[i] += fread(buf, sizeof_buf, 1, file[i]);
        }
        FCLOSE(file[i]);
        tot_lines = MIN(tot_lines, lines[i]);
        asfPrintStatus(".");
    }

    // Concatenate the files in sequential order
    done = 0;
    out = (FILE *)FOPEN(out_file, "wb");
    for (i = 0; i < num_files && !done; i++) {
        asfPrintStatus("\nProcessing file %s...\n", img_files[i]);
        file[i] = (FILE *)FOPEN(img_files[i], "rb");
        int line, num_read;
        for (line = 0; line < tot_lines; line++) {
            num_read = fread(buf, sizeof_buf, 1, file[i]);
            if (num_read == 1) {
                fwrite(buf, sizeof_buf, 1, out);
            }
            if (num_read != 1 || feof(file[i])) {
                // Should not reach this code since the max valid lines has already been
                // determined above and we aren't supposed to be reading past that limit
                asfPrintError("Programming error: File too short:\n  %s\n", img_files[i]);
            }
            asfLineMeter(line, tot_lines);
        }
        asfLineMeter(tot_lines, tot_lines);
        FCLOSE(file[i]);
    }
    FCLOSE(out);

    // Clean up
    FREE(lines);
    FREE(file);
    FREE(buf);

    return tot_lines;
}

static void jpeg_error_handler(j_common_ptr cinfo)
{
    my_error_mgr_t *err = (my_error_mgr_t *)cinfo->err;
    fprintf(stderr,"\n");
    (*cinfo->err->output_message)(cinfo);
    fprintf(stderr,"\n");
    longjmp(err->escape, 1);
}

//static void jpeg_error_message_handler(j_common_ptr cinfo)
//{
//    asfPrintStatus("\nBad JPEG frame detected ...and ignored\n\n");
//}

