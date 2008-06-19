#ifndef _IMPORT_JAXA_L0_H_
#define _IMPORT_JAXA_L0_H_

// Defns
#define JL0_DIR_LEN     1024
#define JL0_FILE_LEN    256
#define JL0_BLUE_VCID   45
#define JL0_GREEN_VCID  46
#define JL0_RED_VCID    47
#define JL0_NIR_VCID    48

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


#endif // _IMPORT_JAXA_L0_H_

