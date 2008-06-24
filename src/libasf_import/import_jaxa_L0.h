#ifndef _IMPORT_JAXA_L0_H_
#define _IMPORT_JAXA_L0_H_

// Defns
#define JL0_DIR_LEN     1024
#define JL0_FILE_LEN    256
#define JL0_BLUE_VCID   45
#define JL0_GREEN_VCID  46
#define JL0_RED_VCID    47
#define JL0_NIR_VCID    48
#define JL0_RED_BAND    "03"
#define JL0_GREEN_BAND  "02"
#define JL0_BLUE_BAND   "01"
#define JL0_NIR_BAND    "04"

// Magic defns
#define MARKER  ((unsigned char)0xff)
#define PAD     ((unsigned char)0x00)
#define SOI     ((unsigned char)0xd8)
#define SOF0    ((unsigned char)0xc0)
#define SOF3    ((unsigned char)0xc3)
#define JPG0    ((unsigned char)0xf0)
#define APP1    ((unsigned char)0xe0)
#define APP2    ((unsigned char)0xe1)
#define APP3    ((unsigned char)0xe2)
#define APP4    ((unsigned char)0xe3)
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

// Prototypes
void get_avnir_chunk_names(const char *red_dir, const char *green_dir,
                           const char *blue_dir, const char *nir_dir,
                           int *num_chunks, char ***red_chunks,
                           char ***green_chunks, char ***blue_chunks,
                           char ***nir_chunks);
void free_avnir_chunk_names(int num_chunks,
                            char ***red_chunks, char ***green_chunks,
                            char ***blue_chunks, char ***nir_chunks);
void get_jaxa_L0_files(file_key_t *files, int num_files, const char *path);
size_t get_data_line(FILE *in, unsigned char **data); char *uc(const char *string);


#endif // _IMPORT_JAXA_L0_H_

