// Import a JAXA Level 0 (JL0) dataset into ASF Internal Format (.img, .meta)
// Applies only to ALOS PRISM and AVNIR-2 Level 0 data
//
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <sys/stat.h>
#include <dirent.h>
#include <time.h>
#include <jpeglib.h>
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
#define MIN(a,b) ((a) <= (b))

// Prototypes
int compare_file_key(const void* file1, const void *file2); // For stdlib qsort() function
int import_jaxa_L0_avnir_band(char **chunks, int num_chunks, char *band,
                              int save_intermediates, const char *outBaseName);
int import_avnir_frame_jpeg_to_img(const char *tmpJpegName, FILE *out);

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
// import_jaxa_L0_avnir_band();
void import_jaxa_L0(const char *inBaseName, const char *outBaseName) {
    int save_intermediates = 0;
    ceos_sensor_t sensor_type=AVNIR;

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
        sprintf(red_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_RED_VCID);
        sprintf(green_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_GREEN_VCID);
        sprintf(blue_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_BLUE_VCID);
        sprintf(nir_dir, "%s%c%d", inBaseName, DIR_SEPARATOR, JL0_NIR_VCID);

        // Retrieve (sorted) file names of each band's chunks (sub-files)
        int num_chunks = 0;
        char **red_chunks=NULL;
        char **green_chunks=NULL;
        char **blue_chunks=NULL;
        char **nir_chunks=NULL;
        get_avnir_chunk_names(red_dir, green_dir, blue_dir, nir_dir, &num_chunks,
                              &red_chunks, &green_chunks,
                              &blue_chunks, &nir_chunks);

        // Import the 4 color bands into 4 band files in ASF Internal format (.img, .meta)
        // (appending "_L0_nn" to each file)
        int red_lines;
        int green_lines;
        int blue_lines;
        int nir_lines;
        int line_count = 0;
        char *tmp_filename1;
        char tmp_filename2[JL0_DIR_LEN + JL0_FILE_LEN + 1];
        char *out_file;

        tmp_filename1 = appendExt(outBaseName, ""); // Strips extension but keeps path info
        sprintf(tmp_filename2, "%s_L0", tmp_filename1); // Append Level 0 indication
        out_file = appendExt(tmp_filename2, ".img"); // Append a .img extension
        FILE *tmp = FOPEN(out_file, "wb"); // Create an empty .img file.  The imports will append...
        FCLOSE(tmp);
        FREE(tmp_filename1);

        // NOTE: The following MUST be in the correct order since each appends the proper
        // lines to the same .img file.  Correct order is: 01 (blue), 02 (green), 03 (red),
        // 04 (near infrared)
        blue_lines  = import_jaxa_L0_avnir_band(blue_chunks, num_chunks, JL0_BLUE_BAND,
                save_intermediates, out_file);
        green_lines = import_jaxa_L0_avnir_band(green_chunks, num_chunks, JL0_GREEN_BAND,
                                                save_intermediates, out_file);
        red_lines   = import_jaxa_L0_avnir_band(red_chunks, num_chunks, JL0_RED_BAND,
                save_intermediates, out_file);
        nir_lines   = import_jaxa_L0_avnir_band(nir_chunks, num_chunks, JL0_NIR_BAND,
                                                save_intermediates, out_file);
        line_count = MIN(nir_lines, MIN(red_lines, MIN(blue_lines, green_lines)));
        FREE(out_file);
        if (red_lines != green_lines ||
            red_lines != blue_lines  ||
            red_lines != nir_lines)
        {
            // FIXME: The importing of bands above should create 4 separate .img files, then
            // they should be combined into one that only has the minimum number of
            // lines available in it ...and the metadata to match.  Then this error could be just
            // a warning instead
            asfPrintWarning("AVNIR-2 Level 0 files do not all have the same number of lines:\n"
                    "  Number of red lines:            %d\n"
                    "  Number of green lines:          %d\n"
                    "  Number of blue lines:           %d\n"
                    "  Number of near-infrared lines:  %d\n"
                    "If these files are merged into a single color image, then this\n"
                    "have to be taken into consideration (missing data?)\n",
                    red_lines, green_lines, blue_lines, nir_lines);
        }

        // At this point, all 4 bands have been written into a single .img file consecutively
        // and it's time to produce the matching .meta file, then the import process will be
        // complete
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
        strcpy(mg->system, "lil-ieee");
        strcpy(mg->acquisition_date, "dd-mmm-yy"); // FIXME: Get the acquisition date out of the metadata!
        mg->orbit = MAGIC_UNSET_INT; // FIXME: Fill out the orbit number
        mg->orbit_direction = MAGIC_UNSET_CHAR; // FIXME: Fill out orbit direction, 'A' or 'D'
        mg->frame = -1; // FIXME: Is there an AVNIR frame number for this?
        mg->band_count = 4;
        strcpy(mg->bands, "01,02,03,04");
        mg->line_count = line_count;
        mg->sample_count = 7100;
        mg->start_line = 0;
        mg->start_sample = 0;
        mg->line_scaling = 1.0;
        mg->sample_scaling = 1.0;
        mg->x_pixel_size = MAGIC_UNSET_DOUBLE;  // FIXME
        mg->y_pixel_size = MAGIC_UNSET_DOUBLE; // FIXME
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
        free_avnir_chunk_names(num_chunks,
                               &red_chunks, &green_chunks,
                               &blue_chunks, &nir_chunks);
        meta_free(meta);
        FREE(out_file);
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

// Reads one telemetry frame from the input file, skips past header, returns the
// remaining line as data (there is no trailing info to remove... just a short header)
// CALLER MUST FREE THIS DATA LINE
size_t get_avnir_data_line(FILE *in, unsigned char **data) {
    size_t bytes_read = 0;
    unsigned char *line = (unsigned char *)MALLOC((JL0_AVNIR_TFRAME_LEN + 1) * sizeof(unsigned char));
    *data = (unsigned char *)MALLOC
                       ((JL0_AVNIR_TFRAME_LEN - JL0_AVNIR_CCSDS_HDR_LEN + 1) * sizeof(unsigned char));
    int i;
    unsigned char *d=NULL;
    if (!feof(in)) {
        bytes_read = FREAD_CHECKED(line, 1, JL0_AVNIR_TFRAME_LEN, in, 1);
    }
    for (i=0, d = line + JL0_AVNIR_CCSDS_HDR_LEN;
         bytes_read > (JL0_AVNIR_CCSDS_HDR_LEN + 1) && i < (bytes_read - JL0_AVNIR_CCSDS_HDR_LEN);
         i++, d++)
    {
        (*data)[i] = *d;
    }
    free(line);

    return bytes_read - JL0_AVNIR_CCSDS_HDR_LEN;
}

// Read each file (chunk) in the list of files for this band, converting
// each frame into ASF Internal format individually.
//
// IMPORTANT NOTES:
// 1. The Jaxa Level 0 AVNIR data exists across several sub-files (chunks),
//    each in their own subdirectory (by VCID number, i.e. ./<basename>/<vcid>)
// 2. Each subfile is made up from 1100-byte long lines of binary information,
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
//    information are actually 7152 bytes long.
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
//    this is NOT consistent with the JPEG standard.
// 9. The JAXA format also includes a non-standard JPEG marker and payload (the information
//    bytes which follow the 0xFF marker byte).  This marker is 0xFFF0 and is RESERVED in
//    the JPEG standard and is not supposed to be used.  In order for the JPEG library to
//    work this marker and payload must be ignored and not stored in any JPEG file.
//

//
// NOTE: out_file should already have the .img extension on it, but maybe not.
//       Use appendExt() to add or replace the ext with a new one, i.e. with .meta
//       The outfile will already have the level 0 identifier in it etc... don't
//       modify the name here.
//
int import_jaxa_L0_avnir_band(char **chunks, int num_chunks, char *band,
                              int save_intermediates, const char *out_file)
{
    char tmpJpegName[JL0_DIR_LEN + JL0_FILE_LEN + 1];
    char all_chunks[JL0_DIR_LEN + JL0_FILE_LEN + 1];
    char tmp_folder[256];
    FILE *in = NULL;
    FILE *out = NULL;
    FILE *jpeg = NULL;
    time_t t;
    char t_stamp[32];

    asfPrintStatus("\nImporting band %s from %d sub-files...\n\n", band, num_chunks);

    // Make temporary folder for temporary files
    t = time(NULL);
    strftime(t_stamp, 22, "%d%b%Y-%Hh_%Mm_%Ss", localtime(&t));
    sprintf(tmp_folder, "./asf_import_tmp_dir_%s_%s", get_basename(out_file), t_stamp);
    if (!is_dir(tmp_folder)) {
        mkdir(tmp_folder, S_IRWXU | S_IRWXG | S_IRWXO);
    }
    else {
        // Should never reach here
        asfPrintError("Temporary folder already exists:\n    %s\n",
                      tmp_folder);
    }

    // Build input and output filenames
    char *out_base = get_basename(out_file);
    sprintf(tmpJpegName, "%s%c%s_%s.jpg", tmp_folder, DIR_SEPARATOR, out_base, band);
    sprintf(all_chunks, "%s%call_chunks_band_%s", tmp_folder, DIR_SEPARATOR, band);
    FREE(out_base);

    // Concatenate all chunks for this band into a single file while at the same
    // time, stripping telemetry frame CCSDS headers out of the data.
    int chunk;
    int bytes_read;
    int lines_read = 0;
    unsigned char *data;
    out = (FILE *)FOPEN(all_chunks, "wb");
    asfPrintStatus("Combining sub-files and stripping CCSDS headers from telemetry frames...\n\n");
    for (chunk = 0; chunk < num_chunks; chunk++) {
        in = (FILE *)FOPEN(chunks[chunk], "rb");
        bytes_read = get_avnir_data_line(in, &data);
        lines_read++;
        while (!feof(in)) {
            if (bytes_read > 0) {
                int i;
                unsigned char *c;
                for (i = 0, c = data; i < bytes_read; i++, c++) {
                    FWRITE(c, 1, 1, out);
                }
            }
            FREE(data);
            bytes_read = get_avnir_data_line(in, &data);
            lines_read++;
            asfLineMeter(lines_read, 40000);
        }
        FCLOSE(in);
        asfLineMeter(40000, 40000);
        lines_read = 0;
    }
    FCLOSE(out);

    // Open output .img, temporary jpeg, and input files
    char *outFileName = appendExt(out_file, ".img"); // Just in case out_file had no .img extension
    out = (FILE *)FOPEN(outFileName, "wb");
    FCLOSE(out); // This zero's out the output file just in case it already existed
    out = (FILE *)FOPEN(outFileName, "ab"); // Open the .img file for append
    in = (FILE *)FOPEN(all_chunks, "rb"); // Open the all-chunks-in-one file

    asfPrintStatus("\nExtracting embedded AVNIR-2 jpeg-format frames\n"
            "for band %s and converting to ASF Internal format...\n\n", band);

    // Scan all chunks into frame (jpeg format) and convert to .img format
    unsigned char c; // last character read
    const unsigned char marker = MARKER; // for writing markers
    unsigned char mID; // last marker id read
    unsigned int payload_length;
    int tot_lines = 0;
    int num_read;
    while (!feof(in)) {
        // 1) Find the start of the image (the SOI marker).  This gets rid of extraneous bytes at
        //    the beginning of the file and also the 32-bit boundary 0-fill that occurs after the
        //    end of image (EOI) and prior to the next SOI in the data file.
        num_read = 0;
        while (!feof(in)) {
            FREAD_CHECKED(&c, 1, 1, in, 0);
            if (c == marker) {
                FREAD_CHECKED(&mID, 1, 1, in, 0);
                if (mID == SOI) {
                    jpeg = (FILE *)FOPEN(tmpJpegName, "wb"); // Open the temporary output jpeg
                    FWRITE(&marker, 1, 1, jpeg);
                    FWRITE(&mID, 1, 1, jpeg);
                    break;
                }
            }
            num_read++;
            //asfRunWatchDog(0.1);
        }

        // 2) Find the start of scan (SOS) ...We found the SOI, so read/write header markers and
        //    payloads ...but skip the non-standard and meaningless ones.  Continue until start of
        //    scan (SOS) is found.
        num_read = 0;
        while (!feof(in)) {
            FREAD_CHECKED(&c, 1, 1, in, 0);
            if (c == marker) {
                FREAD_CHECKED(&mID, 1, 1, in, 0);
                if (mID == JPG0)
                {
                    // Found a discardable marker and payload ...fseek() past it.
                    FREAD_CHECKED(&c, 1, 1, in, 0); // Read the upper byte of the length
                    payload_length = (unsigned int) c; // Shift left to give bits proper values
                    payload_length = payload_length << 8;
                    FREAD_CHECKED(&c, 1, 1, in, 0); // Read lower byte of the length
                    payload_length += (unsigned int) c;
                    payload_length -= 2; // Length includes marker and id (already read ..don't need to skip these)
                    FSEEK(in, payload_length, SEEK_CUR);  // Seek to first byte past marker payload
                }
                else {
                    // We are still in the block of header info that precedes the scan, so this must
                    // be another marker ...one that we are not ignoring, so read/write the payload
                    FWRITE(&marker, 1, 1, jpeg);
                    FWRITE(&mID, 1, 1, jpeg);
                    FREAD_CHECKED(&c, 1, 1, in, 0); // Read the upper byte of the length
                    FWRITE(&c, 1, 1, jpeg);
                    payload_length = (unsigned int) c; // Shift left to give bits proper values
                    payload_length = payload_length << 8;
                    FREAD_CHECKED(&c, 1, 1, in, 0); // Read lower byte of the length
                    FWRITE(&c, 1, 1, jpeg);
                    payload_length += (unsigned int) c;
                    payload_length -= 2; // Length includes marker and id (already read ..don't need to skip these)
                    int byte;
                    for (byte = 0; byte < payload_length; byte++) {
                        // Write the rest of the marker's payload
                        FREAD_CHECKED(&c, 1, 1, in, 0); // Read lower byte of the length
                        FWRITE(&c, 1, 1, jpeg);
                    }
                    if (mID == SOS) {
                        break;
                    }
                }
            }
            else {
                // There should be NO bytes in between markers and their payloads, in between
                // the end of a payload and the next marker, and after the SOI (0xFFD8) and
                // the first marker to follow.  In other words, writing bytes out right here,
                // if not within a marker's payload, is PROBABLY wrong ...this is an experiment
                // until more ALOS data is processed and I can prove that exceptions do or do not
                // exist and how they should be handled.
                FWRITE(&c, 1, 1, jpeg);
            }
            num_read++;
            //asfRunWatchDog(0.1);
        }

        // 3) Find the end of image (EOI).  We found the SOS, so read/write scan data now.  Continue until
        //    the end of image (EOI) is found.
        int nitems = 1;
        int EOI_found = 0;
        num_read = 0;
        while (!feof(in) && nitems > 0) {
            nitems = FREAD_CHECKED(&c, 1, 1, in, 1);
            if (nitems == 1) {
                FWRITE(&c, 1, 1, jpeg);
                if (c == marker) {
                    // Found a possible marker (might be a true 0xFF valued byte in the data, in
                    // which case the JPEG standard says it is followed by a 0-padding (0x00) byte
                    // and not actually a marker)
                    nitems = FREAD_CHECKED(&mID, 1, 1, in, 1);
                    if (nitems == 1) {
                        FWRITE(&mID, 1, 1, jpeg);
                        if (mID == EOI) {
                            EOI_found = 1;
                            break;
                        }
                    }
                }
            }
            num_read++;
            //asfRunWatchDog(0.1);
        }

        // Close the temporary jpeg file and convert it to a .img file (appending)
        FCLOSE(jpeg);
        if (EOI_found) {
            // Only convert completed temporary jpegs ...ALOS data may not end on a full
            // frame boundary ...in other words, the chunk data from the satellite may
            // end prematurely ...so discard incomplete frames.
            tot_lines += import_avnir_frame_jpeg_to_img(tmpJpegName, out);
        }
    }
    //asfStopWatchDog();
    FCLOSE(in);

    // Clean up
    char del_files[1024];
    sprintf(del_files, "rm -rf %s", tmp_folder);
    printf("\nWould have run this command: %s\n\n", del_files);
    //asfSystem(del_files);

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
int import_avnir_frame_jpeg_to_img(const char *tmpJpegName, FILE *out)
{
    struct jpeg_decompress_struct *cinfo =
            (struct jpeg_decompress_struct *)MALLOC(sizeof(struct jpeg_decompress_struct));
    struct jpeg_error_mgr mgr;
    unsigned char *dest;

    cinfo->err = jpeg_std_error(&mgr);
    cinfo->buffered_image = TRUE;
    jpeg_create_decompress(cinfo);

    FILE *fp = (FILE *)FOPEN(tmpJpegName, "rb");

    jpeg_stdio_src(cinfo, fp);
    jpeg_read_header(cinfo, TRUE);
    jpeg_start_decompress(cinfo);

    dest = (unsigned char *)MALLOC((2 * 3550) * sizeof(unsigned char)); // greyscale, 1 component
    JSAMPLE *buf = (JSAMPLE *)MALLOC(cinfo->image_width * sizeof(unsigned char));

    int i;
    for (i = 0; i < cinfo->image_height; i++) {
        jpeg_read_scanlines(cinfo, &buf, 1); // Read and uncompress one data line

        int j = 12;             // Point at odds
        int k = 12 + 3550 + 26; // Point at evens
        int m, n;
        for (m = n = 0; m < 3550; m++, j++, k++) {
            dest[n] = (unsigned char)buf[j]; // Odd pixel
            dest[n + 1] = (unsigned char)buf[k]; // Even pixel
            n += 2;
        }
        FWRITE(buf, sizeof(unsigned char), 2 * 3550, out); // Write raw interlaced data to .img file
    }

    FCLOSE(fp);

    // Clean up
    jpeg_finish_decompress(cinfo);
    jpeg_destroy_decompress(cinfo);
    FREE(cinfo);
    FREE(buf);

    return cinfo->image_height;
}
