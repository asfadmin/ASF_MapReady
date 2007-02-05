/* 
   A conversion of Joe's fabulous program to a library function
   we can call during terrain correction, to apply water masking
   automatically.
*/

#include "asf.h"
#include "asf_meta.h"

// after much discussion we decided that the convention for masks
// would be:
//   ==0  : Masked
//    >0  : Not Masked
// in other words, the user creates the mask such that the "region
// of interest" has the non-zero values.

// return TRUE if the given pixel value is MASKED
int
is_masked(double mask_val)
{
    // so we return TRUE when the given pixel value EQUALS zero.
    // (within the tolerance)
    return fabs(mask_val) < .001;
}

float masked_value() { return 0.0; }
float unmasked_value() { return 1.0; }

int
dem_to_mask(char *inDemFile, char *outMaskFile, float cutoff)
{
    meta_parameters *inDemMeta = meta_read(inDemFile);

    int x_size = inDemMeta->general->sample_count;
    int y_size = inDemMeta->general->line_count;

    float *maskbuffer = MALLOC(sizeof(float) * x_size);
    float *floatbuffer = MALLOC(sizeof(float) * x_size);

    FILE *in = fopenImage(inDemFile, "rb");
    FILE *out = fopenImage(outMaskFile, "wb");

    int y;
    for (y=0; y<y_size; y++) 
    {
        get_float_line(in, inDemMeta, y, floatbuffer);

        int x;
        float mv = masked_value();
        float umv = unmasked_value();

        for (x=0; x < x_size; x++)            
            maskbuffer[x] = floatbuffer[x] <= cutoff ? mv : umv;

        put_float_line(out, inDemMeta, y, maskbuffer);
        asfLineMeter(y, y_size);
    }

    FCLOSE(in);
    FCLOSE(out);

    FREE(floatbuffer);
    FREE(maskbuffer);

    meta_write(inDemMeta, outMaskFile);
    meta_free(inDemMeta);

    asfPrintStatus("Created Mask file '%s' from DEM '%s'.\n",
                   outMaskFile, inDemFile);

    return 0;
}
