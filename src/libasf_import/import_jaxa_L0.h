#ifndef _IMPORT_JAXA_L0_H_
#define _IMPORT_JAXA_L0_H_

// Defns
#define MAX_ALLOWED_JPEG_ERRORS         2000
#define JL0_DIR_LEN                     1024
#define JL0_FILE_LEN                    256
#define JL0_BLUE_VCID                   45
#define JL0_GREEN_VCID                  46
#define JL0_RED_VCID                    47
#define JL0_NIR_VCID                    48
#define JL0_RED_BAND                    "03"
#define JL0_GREEN_BAND                  "02"
#define JL0_BLUE_BAND                   "01"
#define JL0_NIR_BAND                    "04"
#define JL0_RED_BAND_NO                 3
#define JL0_GREEN_BAND_NO               2
#define JL0_BLUE_BAND_NO                1
#define JL0_NIR_BAND_NO                 4
#define JL0_AVNIR_TFRAME_DATA_LEN       1094    /* Length of telemetry frame data section */
#define JL0_AVNIR_TFRAME_LEN            1100    /* Length of total telemetry frame (VCDU) */
#define JL0_AVNIR_CCSDS_HDR_LEN         6       /* Length of telemetry frame header       */
#define JL0_AVNIR_LINES_PER_FRAME       16
#define APP0_AUX_MEMORY_BUFFER_OVERFLOW 0x02    /* Second bit in 8-bit error byte is 'buffer overflow' */
#define AVNIR_IMAGING_CYCLE             0.00148 /* 1.48 msec for AVNIR-2 */
#define PRISM_IMAGING_CYCLE             0.00037 /* 0.37 msec for PRISM   */
#define JL0_AVNIR_FRAME_TIME            JL0_AVNIR_LINES_PER_FRAME * AVNIR_IMAGING_CYCLE
#define JL0_AVNIR_FRAME_TIME_TOLERANCE  (JL0_AVNIR_FRAME_TIME * 0.50)
#define JL0_AVNIR_FRAME_TIME_REPEATS    2.64358 /* Ave. number of frames with same sequential time stamp */
#define MAXIMUM_SOI_SEARCH_TRIES        1000000
#define JL0_AVNIR_SAMPLE_COUNT          7100
#define JL0_AVNIR_JPEG_HDR_LEN          256
#define JL0_AVNIR_CHUNK_SIZE            40000

// Magic jpeg defns
#define MARKER  ((unsigned char)0xff)
#define PAD     ((unsigned char)0x00)
#define SOI     ((unsigned char)0xd8)
#define SOF0    ((unsigned char)0xc0)
#define SOF3    ((unsigned char)0xc3)
#define JPG0    ((unsigned char)0xf0)
#define APP0    ((unsigned char)0xe0)
#define APP1    ((unsigned char)0xe1)
#define APP2    ((unsigned char)0xe2)
#define APP3    ((unsigned char)0xe3)
#define APP4    ((unsigned char)0xe4)
#define DQT     ((unsigned char)0xdb)
#define DQS     ((unsigned char)0xf6)
#define DHT     ((unsigned char)0xc4)
#define SOS     ((unsigned char)0xda)
#define EOI     ((unsigned char)0xd9)

// Types
typedef struct {
    char file[JL0_FILE_LEN];
    int  key;
} file_key_t;

typedef struct{
    struct jpeg_error_mgr errmgr;
    jmp_buf escape;
} my_error_mgr_t;


// Prototypes
void get_avnir_chunk_names(const char *red_dir, const char *green_dir,
                           const char *blue_dir, const char *nir_dir,
                           int *num_red_chunks, int *num_green_chunks,
                           int *num_blue_chunks, int *num_nir_chunks,
                           char ***red_chunks, char ***green_chunks,
                           char ***blue_chunks, char ***nir_chunks);
void free_avnir_chunk_names(int num_red_chunks, int num_green_chunks,
                            int num_blue_chunks, int num_nir_chunks,
                            char ***red_chunks, char ***green_chunks,
                            char ***blue_chunks, char ***nir_chunks);
void get_jaxa_L0_files(file_key_t *files, int num_files, const char *path);
int compare_file_key(const void* file1, const void *file2); // For stdlib qsort() function
int import_jaxa_L0_avnir_bands(int *red_lines, int *green_lines, int *blue_lines, int *nir_lines,
                               char **red_chunks, char **green_chunks, char **blue_chunks, char **nir_chunks,
                               int num_red_chunks, int num_green_chunks, int num_blue_chunks, int num_nir_chunks,
                               char *bands, int *num_bands, int save_intermediates, const char *outBaseName);
int import_avnir_frame_jpeg_to_img(const char *tmpJpegName, int good_frame, FILE *out);
size_t get_avnir_data_line(FILE *in, unsigned char **data, int first_vcdu, int *last_vcdu_ctr,
                           int *continuous, int *missing_bytes, int *vcdu_ctr);
void concat_avnir_band_chunks(int num_chunks, const char **chunks, const char *all_chunks, const char *band);
int find_next_avnir_SOI(FILE *in, float time_target, int max_tries, int *valid_vcdu,
                        float *time, float *last_time, unsigned char *buf, int *idx, int band_no,
                        int first_vcdu, int first_SOI, int *last_vcdu_ctr, unsigned char *hdr_buf, int *valid_SOI);
int fread_avnir_tstream(unsigned char *c, FILE *in, unsigned char *buf, int first_vcdu,
                        int *idx, int band_no, int *last_vcdu_ctr, int *valid_vcdu);
void validate_avnir_SOI_and_get_frame_time (FILE *in, unsigned char *buf, int *idx, int first_vcdu,
                                            int first_SOI, int *last_vcdu_ctr, unsigned char *hdr_buf,
                                            int band_no, int *valid_SOI, float *time);
int read_write_avnir_jpeg_frame(FILE *in, unsigned char *hdr_buf, unsigned char *buf, int *idx,
                                int band_no, int *valid, int first_vcdu, int *last_vcdu_ctr, FILE *jpeg);
void write_avnir_frame(int band_valid, int valid_SOI, int valid_vcdu, FILE *in,
                       unsigned char *hdr_buf, unsigned char *buf, int *idx, int band_no, int *valid,
                       int first_vcdu, int *blast_vcdu_ctr, char *jpegFile, FILE *out);
int concat_img_files(char **img_files, int num_files, int sample_count, const char *out_file);

#endif // _IMPORT_JAXA_L0_H_

