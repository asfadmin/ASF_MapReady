#include "ssv.h"
#include <ceos_io.h>
#include "asf_import.h"
#include "get_ceos_names.h"
#include <asf_nan.h>
#include "asf_endian.h"

// if an image takes up above this number of bytes, we store data
// with FloatImage instead of just an in-memory float array
static const int FLOAT_IMAGE_CUTOFF = 500*1024*1024;

static int use_float_image(int nl, int ns)
{
    if (nl*ns*4 > FLOAT_IMAGE_CUTOFF) {
        asfPrintStatus("Storing image data using disk cache (FloatImage).\n");
        return TRUE;
    } else {
        asfPrintStatus("Storing image data in memory.\n");
        return FALSE;
    }
}

static int try_ext(const char *filename, const char *ext)
{
    char *buf = MALLOC(sizeof(char)*(strlen(filename)+strlen(ext)+5));
    if (ext[0]=='.')
        sprintf(buf, "%s%s", filename, ext);
    else
        sprintf(buf, "%s.%s", filename, ext);

    int ret = fileExists(buf);
    free(buf);

    return ret;
}

static void clear_data()
{
    if (data) {
        free(data);
        data=NULL;
    }
    if (data_fi) {
        float_image_free(data_fi);
        data_fi=NULL;
    }
}

static void read_asf(const char *filename, const char *band)
{
    // assume metadata has already been read in
    assert(meta);
    clear_data();

    printf("Reading ASF Internal: %s\n", filename);
    nl = meta->general->line_count;
    ns = meta->general->sample_count;
    int b = 0;
    if (band)
        b = get_band_number(meta->general->bands,
                meta->general->band_count, (char*)band);
    if (b<0)
        asfPrintError("Band '%s' not found.\n");
    else if (band)
        asfPrintStatus("Reading band #%d: %s\n", b+1, band);

    FILE *fp = FOPEN(filename, "rb");

    if (use_float_image(nl,ns)) {
        data_fi = float_image_new(ns, nl);
        float *buf = MALLOC(sizeof(float)*ns);

        int i,j;
        for (i=0; i<nl; ++i) {
            get_float_line(fp, meta, i + b*nl, buf);
            for (j=0; j<ns; ++j)
                float_image_set_pixel(data_fi, j, i, buf[j]);
            asfPercentMeter((float)i/nl);
        }

        free(buf);
    } else {
        data = MALLOC(sizeof(float)*nl*ns);

        // get_float_lines(fp, meta, 0, nl, data);
        int i;
        for (i=0; i<nl; i+=128) {
            int l=128; if (i+128>nl) l=nl-i;
            get_float_lines(fp, meta, i + b*nl, l, data + i*ns);
            asfPercentMeter((float)i/nl);
        }
    }

    asfPercentMeter(1.0);
    fclose(fp);
}

static void read_ceos(struct IOF_VFDR *image_fdr, const char *filename)
{
    assert(meta);
    clear_data();

    nl = meta->general->line_count;
    ns = meta->general->sample_count;

    if (use_float_image(nl,ns))
        data_fi = float_image_new(ns, nl);
    else
        data = MALLOC(sizeof(float)*nl*ns);

    int leftFill = image_fdr->lbrdrpxl;
    int rightFill = image_fdr->rbrdrpxl;
    int headerBytes = firstRecordLen((char*)filename) +
        (image_fdr->reclen - (ns + leftFill + rightFill)*image_fdr->bytgroup);

    FILE *fp = fopen(filename, "rb");

    int ii,jj;
    if (meta->general->data_type == INTEGER16)
    {
        unsigned short *shorts = MALLOC(sizeof(unsigned short)*ns);
        for (ii=0; ii<nl; ++ii) {
            long long offset = (long long)(headerBytes + ii*image_fdr->reclen);

            FSEEK64(fp, offset, SEEK_SET);
            FREAD(shorts, sizeof(unsigned short), ns, fp);

            if (data) {
                for (jj = 0; jj < ns; ++jj) {
                    big16(shorts[jj]);
                    data[jj + ii*ns] = (float)(shorts[jj]);
                }
            } else {
                for (jj = 0; jj < ns; ++jj) {
                    big16(shorts[jj]);
                    float_image_set_pixel(data_fi, jj, ii,
                        (float)(shorts[jj]));
                }
            }

            asfPercentMeter((float)ii/nl);
        }
        free(shorts);
    }
    else if (meta->general->data_type == BYTE)
    {
        unsigned char *bytes = MALLOC(sizeof(unsigned char)*ns);
        for (ii=0; ii<nl; ++ii) {
            long long offset = (long long)(headerBytes + ii*image_fdr->reclen);

            FSEEK64(fp, offset, SEEK_SET);
            FREAD(bytes, sizeof(unsigned char), ns, fp);

            if (data) {
                for (jj = 0; jj < ns; ++jj)
                    data[jj + ii*ns] = (float)(bytes[jj]);
            } else {
                for (jj = 0; jj < ns; ++jj)
                    float_image_set_pixel(data_fi, jj, ii,
                        (float)(bytes[jj]));
            }

            asfPercentMeter((float)ii/nl);
        }
        free(bytes);
    }
    asfPercentMeter(1.0);
    fclose(fp);
}

static void read_alos(const char *basename, const char *img_name,
                      const char *meta_name)
{
    printf("Reading ALOS: %s\n", img_name);
    struct IOF_VFDR image_fdr;
    get_ifiledr(basename, &image_fdr);
    read_ceos(&image_fdr, img_name);
}

static void read_D(const char *filename)
{
    printf("Reading CEOS: %s\n", filename);
    char *meta_filename = appendExt(filename, ".L");
    struct IOF_VFDR image_fdr;
    get_ifiledr(meta_filename, &image_fdr);
    read_ceos(&image_fdr, filename);
    free(meta_filename);
}

void read_file(const char *filename_in, const char *band)
{
    char *filename = STRDUP(filename_in);

    // strip off a trailing "."
    if (filename[strlen(filename)-1] == '.')
        filename[strlen(filename)-1] = '\0';

    // first need to figure out what kind of file this is
    // we will do that based on the extension
    // user may have just given basename, so we may need to hunt
    char *img_file=NULL;

    char *ext = findExt(filename);

    if (!ext) {
        if (fileExists(filename)) {
            // no extension (e.g., ALOS)
            img_file = STRDUP(filename);
            ext = "";
        } else if (try_ext(filename, ".img")) {
            img_file = appendExt(filename, ".img");
            ext = ".img";
        } else if (try_ext(filename, ".D")) {
            img_file = appendExt(filename, ".D");
            ext = ".D";
        } else {
            // could be an ALOS basename...
            ext = "";
        }
    } else {
        // user gave extension
        img_file = STRDUP(filename);
    }

    if (meta)
        meta_free(meta);

    if (strcmp_case(ext, ".img") == 0) {
        assert(img_file);
        char *meta_filename = appendExt(filename, ".meta");
        if (fileExists(meta_filename)) {
            meta = meta_read(meta_filename);
        } else {
            asfPrintError("Cannot find metadata for: %s\n", meta_filename);
        }
        free(meta_filename);
        read_asf(img_file, band);
    } else if (strcmp_case(ext, ".D") == 0) {
        assert(img_file);
        char *meta_filename = appendExt(filename, ".L");
        if (fileExists(meta_filename)) {
            meta = meta_create(meta_filename);
        } else {
            asfPrintError("Cannot find metadata: %s\n", meta_filename);
        }
        free(meta_filename);
        if (band)
            asfPrintWarning("Band specification ignored.\n");
        read_D(img_file);
    } else if (strncmp_case(filename, "IMG-", 4) == 0) {
        assert(img_file);
        char *meta_filename = MALLOC(sizeof(char)*strlen(filename));
        strcpy(meta_filename, "LED-");
        char *p = strchr(filename+5, '-') + 1;
        strcat(meta_filename, p);
        if (fileExists(meta_filename)) {
            meta = meta_create(p);
        } else {
            asfPrintError("Cannot find metadata: %s\n", meta_filename);
        }
        if (band)
            asfPrintWarning("Band specification ignored.\n");
        read_alos(p, img_file, meta_filename);
        free(meta_filename);
    } else {
        // possibly an alos basename -- prepend "LED-" (if needed) and see
        char *meta_filename;
        if (strncmp_case(filename, "LED-", 4) == 0) {
            if (!fileExists(filename))
                asfPrintError("Cannot find: %s\n", filename);
            meta_filename = STRDUP(filename);
        } else {
            meta_filename = MALLOC(sizeof(char)*(10+strlen(filename)));
            strcpy(meta_filename, "LED-");
            strcat(meta_filename, filename);
        }
        if (fileExists(meta_filename)) {
            meta = meta_create(filename);
            char **dataName = MALLOC(sizeof(char*)*MAX_BANDS);
            int i,nBands;
            for (i=0; i<MAX_BANDS; ++i)
                dataName[i] = MALLOC(sizeof(char)*256);
            get_ceos_data_name(filename, dataName, &nBands);
            int which_band=-1;
            if (band) {
                for (i=0; i<nBands; ++i) {
                    if (strcmp(dataName[i], band) == 0) {
                        which_band=i;
                        break;
                    }
                }
            } else
                which_band = 0;
            if (which_band < 0)
                asfPrintError("Band '%s' not found.\n");
            read_alos(filename, dataName[which_band], meta_filename);
            FREE_BANDS(dataName);
        } else {
            asfPrintError("Unknown image type: %s\n", img_file);
        }
        free(meta_filename);
    }

    FREE(img_file);
    FREE(filename);

    assert(data||data_fi);

    center_samp = crosshair_samp = (double)ns/2.;
    center_line = crosshair_line = (double)nl/2.;
}
