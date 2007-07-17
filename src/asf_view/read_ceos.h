#include "asf_import.h"
#include "get_ceos_names.h"
#include "asf_nan.h"
#include "asf_endian.h"

// common to CEOS and ALOS readers

typedef struct {
    FILE *fp;
    int headerBytes;
    int reclen;
} ReadCeosClientInfo;

meta_parameters *read_ceos_meta(const char *meta_name);
int read_ceos_client(int row_start, int n_rows_to_get,
                    void *dest, void *read_client_info,
                    meta_parameters *meta);
int get_ceos_thumbnail_data(int thumb_size_x,
                            int thumb_size_y, meta_parameters *meta,
                            void *read_client_info, void *dest);
void free_ceos_client_info(void *read_client_info);

