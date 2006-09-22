/* 
   A conversion of Joe's fabulous program to a library function
   we can call during terrain correction, to apply water masking
   automatically.
*/

int
dem_to_mask(char *inDemFile, char *outMaskFile, float cutoff)
{
    meta_parameters *inDemMeta = meta_read(inDemFile);

    long x_size = inDemMeta->general->sample_count;
    long y_size = inDemMeta->general->line_count;
    
    float *maskbuffer = MALLOC(sizeof(float) * x_size);
    float *floatbuffer = MALLOC (sizeof(float) * x_size);

    FILE *in = fopenImage(infile, "rb");
    FILE *out = fopenImage(outfile, "wb");

    int line;
    for (line=0; line <= y_size ; line++) 
    {
        get_float_line(in,inDemMeta,line,floatbuffer);

        int x;
        for (x=0; x < x_size; x++)            
            maskbuffer[x] = floatbuffer[x] < cutoff ? 1 : 0;

        put_float_line(out,outMeta,line,maskbuffer);
    }

    FCLOSE(in);
    FCLOSE(out);

    FREE(floatbuffer);
    FREE(maskbuffer);

    meta_write(inDemMeta, outMaskFile);
    meta_free(inDemMeta);

    return 0;
}
