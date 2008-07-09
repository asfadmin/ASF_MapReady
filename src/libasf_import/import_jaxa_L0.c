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
#include "asf_nan.h"
#include "asf_import.h"
#include "get_ceos_names.h" // For enum typdef with 'AVNIR', 'PRISM' etc in it
#include "import_jaxa_L0.h"

// Prototypes
int compare_file_key(const void* file1, const void *file2); // For stdlib qsort() function
int import_jaxa_L0_avnir_band(char **chunks, int num_chunks, char *band,
                              int save_intermediates, const char *outBaseName);
void import_avnir_frame_jpeg_to_img(const char *tmpJpegName, FILE *out);

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
        char *tmp_filename1;
        char tmp_filename2[JL0_DIR_LEN + JL0_FILE_LEN + 1];
        char *out_file;

        tmp_filename1 = appendExt(outBaseName, ""); // Strips extension but keeps path info
        sprintf(tmp_filename2, "%s_L0", tmp_filename1); // Append Level 0 indication
        out_file = appendExt(tmp_filename2, ".img"); // Append a .img extension
        FILE *tmp = FOPEN(out_file, "wb"); // Create an empty .img file.  The imports will append...
        FCLOSE(tmp);
        FREE(tmp_filename1);

        red_lines   = import_jaxa_L0_avnir_band(red_chunks, num_chunks, JL0_RED_BAND,
                                                save_intermediates, out_file);
        green_lines = import_jaxa_L0_avnir_band(green_chunks, num_chunks, JL0_GREEN_BAND,
                                                save_intermediates, out_file);
        blue_lines  = import_jaxa_L0_avnir_band(blue_chunks, num_chunks, JL0_BLUE_BAND,
                                                save_intermediates, out_file);
        nir_lines   = import_jaxa_L0_avnir_band(nir_chunks, num_chunks, JL0_NIR_BAND,
                                                save_intermediates, out_file);
        FREE(out_file);
        if (red_lines != green_lines ||
            red_lines != blue_lines  ||
            red_lines != nir_lines)
        {
            asfPrintWarning("AVNIR-2 Level 0 files do not all have the same number of lines:\n"
                    "  Number of red lines:            %d\n"
                    "  Number of green lines:          %d\n"
                    "  Number of blue lines:           %d\n"
                    "  Number of near-infrared lines:  %d\n"
                    "If these files are merged into a single color image, then this\n"
                    "have to be taken into consideration (missing data?)\n",
                    red_lines, green_lines, blue_lines, nir_lines);
        }

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
//     Open output .img file (will write metadata later)
//     Open first chunk, and then at each feof() close it and open/use the next until done
//     PROCESS EACH SOI->EOI FRAME:
//      Open temporary output file for write
//       Scan for SOI and write it out when found
//       Scan for SOF0 or SOF3 and write it out when found
//       Read number of lines (Y) from SOF payload and add to tot_lines
//       Read number of samples (X) from SOF and check to make sure it's 7152
//       Read number of image components and check for 0x01 (greyscale single band)
//       Scan for JPG0 (0xfff0)
//       Read version number from JPG0 and check to make sure it's 0x00
//       Read CAP0 and CAP1 from JPG0 and check for 0x00 and 0x21 respectively
//       Scan for SOS (APP1 DQT DHS DHT read/write along the way)
//       Read number of image components and check for 0x01 (greyscale single band)
//       While EOI is not found
//         Read 3550 odd pixels into buffer
//         Read 3550 even pixels into buffer
//         Write interlaced odd/even pixels to temporary jpeg file
//       End while
//      Write EOI to temporary jpeg file
//      Close temporary jpeg file
//      Use jpeg library to read data lines from the temporary jpeg file and write to .img file
//      Close temporary jpeg file (leaving the .img file open)
//      Continue reading/writing frames as described ...until the last chunk runs out
//      Close the .img file
//      Populate and write the metadata file
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
    int tot_lines = 0;
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
    sprintf(tmpJpegName, "%s%c%s_L0_%s.jpg", tmp_folder, DIR_SEPARATOR, out_file, band);
    sprintf(all_chunks, "%s%call_chunks_band_%s", tmp_folder, DIR_SEPARATOR, band);

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
    out = (FILE *)FOPEN(outFileName, "ab"); // .img file
    jpeg = (FILE *)FOPEN(tmpJpegName, "wb");
    in = (FILE *)FOPEN(all_chunks, "rb");

    asfPrintStatus("\nExtracting embedded AVNIR-2 jpeg-format frames\n"
            "for band %s and converting to ASF Internal format...\n\n", band);

    // Scan and discard bytes until start of image is found
    unsigned char c;
    int done = 0;
    while (!done) {
        FREAD_CHECKED(&c, 1, 1, in, 0);
        if (c == MARKER) {
            FREAD_CHECKED(&c, 1, 1, in, 0);
            if (c == SOI) {
                unsigned char val = MARKER;
                FWRITE(&val, 1, 1, jpeg); // SOI will get written in next loop by default...
                done = 1;
            }
        }
    }
    while (!feof(in)) {
        if (c == MARKER) {
            // Marker found...
            // ... Read marker ID
            FREAD_CHECKED(&c, 1, 1, in, 0);
            if (c == JPG0) {
                // Found the non-standard JPG0 marker from ALOS
                unsigned int payload_length;
                int i;
                FREAD_CHECKED(&c, 1, 1, in, 0);
                payload_length = (unsigned int) c;
                payload_length = payload_length << 8;
                FREAD_CHECKED(&c, 1, 1, in, 0);
                payload_length += (unsigned int) c;
                for (i = 0; i < payload_length - 2; i++) {
                    FREAD_CHECKED(&c, 1, 1, in, 0); // Read and discard payload from JPG0
                }
            }
            else if (c == EOI) {
                // Found end of image marker
                unsigned char val = MARKER;
                FWRITE(&val, 1, 1, jpeg);
                FWRITE(&c, 1, 1, jpeg);
                // Skip zero-fill that follows EOI markers (0 to 3 bytes of 0x00)
                FREAD_CHECKED(&c, 1, 1, in, 0);
                while (c == PAD) {
                    FREAD_CHECKED(&c, 1, 1, in, 0);
                }
                fseek(in, -1, SEEK_CUR); // Back up one byte
                FCLOSE(jpeg); // The temporary jpeg is now ready for import

                // Import the recently-read frame, appending it to the open .img file,
                // then zero out the temporary jpeg and leave it open to be used for the]
                // next frame
                import_avnir_frame_jpeg_to_img(tmpJpegName, out);
                jpeg = (FILE *)FOPEN(tmpJpegName, "wb");  // Zero-out the temporary jpeg

                // Scan forward until the next SOI is found (the next 16-line frame)
                done = 0;
                while (!done) {
                    FREAD_CHECKED(&c, 1, 1, in, 0);
                    if (c == MARKER) {
                        FREAD_CHECKED(&c, 1, 1, in, 0);
                        if (c == SOI) {
                            unsigned char val = MARKER;
                            FWRITE(&val, 1, 1, jpeg); // Write the jpeg marker to the temporary jpeg
                            FWRITE(&c, 1, 1, jpeg);   // Write the SOI to the temporary jpeg
                            done = 1;
                        }
                    }
                }
            }
            else if (c == SOS) {
                // Found start of scan marker NOTE 1: There is no end of scan marker ...the scan ends at
                // the end of image marker, EOI.  NOTE 2: Each 'image' in the ALOS JAXA Level 0 Avnir
                // files, and there are a great many, is just one frame... and a frame is 16 lines of
                // image data.  To create the entire image, you concatenate all the frames together.
                unsigned char val = MARKER;
                FWRITE(&val, 1, 1, jpeg);
                FWRITE(&c, 1, 1, jpeg);
                int i;
                unsigned char odds[3550];
                unsigned char evens[3550];
                // FIXME: Should use number of lines from frame marker
                // FIXME: Should read compressed jpeg line into a buffer THEN pick the
                //        data out!!!
                for (i = 0;
                     i < 16 && !feof(in);
                     i++)
                {
                    FREAD_CHECKED(odds, 1, 12, in, 0); // Read and discard 12 bytes
                    FREAD_CHECKED(odds, 1, 3550, in, 0); // Read the odd numbered bytes
                    FREAD_CHECKED(evens, 1, 26, in, 0); // Read and discard 26 bytes
                    FREAD_CHECKED(evens, 1, 3550, in, 0); // Read the even numbered bytes
                    // Write the data to the jpeg, interlacing odds/evens as you go
                    int byte;
                    for (byte = 0; byte < 3550; byte++) {
                        FWRITE(&odds[byte], 1, 1, jpeg);
                        FWRITE(&evens[byte], 1, 1, jpeg);
                    }
                    // FIXME: Should fix the number of samples in the frame marker payload rather
                    // than write zero-fill bytes here ...
                    unsigned char val = 0x00;
                    for (byte = 0; byte < 52; byte++) {
                        FWRITE(&val, 1, 1, jpeg);
                    }
                    FREAD_CHECKED(&odds, 1, 14, in, 0); // Read and discard 14 bytes
                }
            }
            else {
                // Write marker ID's from ignored markers
                unsigned char val = MARKER;
                FWRITE(&val, 1, 1, jpeg);
                FWRITE(&c, 1, 1, jpeg);
            }
        }
        else {
            FWRITE(&c, 1, 1, jpeg);
        }
        FREAD_CHECKED(&c, 1, 1, in, 1);
    }
    FCLOSE(in);

    // Clean up
    //sprintf(del_files, "rm -f %s", tmp);
    //asfSystem(del_files);
    //strcpy(tmp_basename, get_basename(tmp));

    return tot_lines;
}

// Reads a jpeg file and appends all data lines to the end of an .img file
// Assumes 'out' is opened for append, and it's up to the programmer to make
// sure that lines written in several calls to this function are all the same
// length (sample count) and data type.
void import_avnir_frame_jpeg_to_img(const char *tmpJpegName, FILE *out)
{
    struct jpeg_decompress_struct *cinfo =
            (struct jpeg_decompress_struct *)MALLOC(sizeof(struct jpeg_decompress_struct));
    unsigned char *dest;

    jpeg_create_decompress(cinfo);
    cinfo->buffered_image = TRUE;

    static struct jpeg_error_mgr mgr;
    cinfo->err = jpeg_std_error(&mgr);

    FILE *fp = (FILE *)FOPEN(tmpJpegName, "rb");

    jpeg_stdio_src(cinfo, fp);
    jpeg_read_header(cinfo, TRUE);
    jpeg_start_decompress(cinfo);

    dest = (unsigned char *)MALLOC(cinfo->image_width * sizeof(unsigned char)); // greyscale, 1 component
    JSAMPLE *buf = (JSAMPLE *)MALLOC(cinfo->image_width * sizeof(unsigned char));

    int i;
    for (i = 0; i < cinfo->image_height; i++) {
        jpeg_read_scanlines(cinfo, &buf, 1);

        int j;
        for (j = 0; j < cinfo->image_width; j++) {
            dest[j] = (unsigned char)buf[j];
        }
        FWRITE(buf, sizeof(unsigned char), cinfo->image_width, out);
    }

    FCLOSE(fp);

    jpeg_finish_decompress(cinfo);
    jpeg_destroy_decompress(cinfo);
    FREE(cinfo);

    FREE(buf);
}


















































