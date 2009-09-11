#include <stdio.h>
#include <stdlib.h>

#include "asf.h"
#include "asf_meta.h"

void usage()
{
    printf("Usage:\n"
           "    addapole [-sinc | -pole | -pyr] <line> <sample> <radius> "
           "<height> <infile> <outfile>\n");
    exit(1);
}

double dmax(double a, double b) { return a>b?a:b; }

int main(int argc, char *argv[])
{
    if (argc != 8) usage();

    int type = 0; // 0=?, 1=sinc, 2=pole, 3=pyr

    char *in = argv[6];
    char *out = argv[7];

    int line = atoi(argv[2]);
    int samp = atoi(argv[3]);
    int radius = atoi(argv[4]);
    float height = atof(argv[5]);

    if (strcmp(argv[1], "-sinc")==0) type=1;
    if (strcmp(argv[1], "-pole")==0) type=2;
    if (strcmp(argv[1], "-pyr")==0) type=3;
    if (type==0) { printf("Unknown pole type.\n"); usage(); }

    printf("Adding a pole:\n");
    printf("  Center point: (%d,%d)\n", line, samp);
    printf("  Radius: %d\n", radius);
    printf("  Height: %f\n", height);
    if (type!=2)
        printf("    Note that the height value is **added** to the existing "
               "terrain height.\n\n");

    printf("  Input file: %s\n", in);
    printf("  Output file: %s\n", out);

    printf("  Pole type: %s\n", argv[1]+1);

    meta_parameters *meta = meta_read(in);
    int ns = meta->general->sample_count;
    int nl = meta->general->line_count;

    FILE *inDEM = fopenImage(in, "rb");
    FILE *outDEM = fopenImage (out, "wb");

    float *demLine = (float*)MALLOC(sizeof(float)*ns);

    int i,l;
    for (l = 0; l < nl; ++l) {
        get_float_line(inDEM, meta, l, demLine);
        for (i = 0; i < ns; ++i) {
            double x;

            // sinc and pole use circular posts, real distance
            // pyr uses distance measured along the gridlines
            if (type==1 || type==2)
                x = hypot(i-samp, l-line);
            else if (type == 3)
                x = dmax(fabs(i-samp),fabs(l-line));

            if (x < (double)radius) {
                if (type==1) { // sinc
                    if (i==samp && l==line)
                        demLine[i] = height;
                    else {
                        x *= M_PI/radius;
                        demLine[i] += height * sin(x)/x;
                    }
                } else if (type==2) { // pole
                    demLine[i] = height;
                } else if (type==3) { // pyr
                    demLine[i] += height * (radius-x)/radius;
                } else {
                    printf("Impossible: type=%d\n", type);
                    exit(1);
                }
            }
        }
        put_float_line(outDEM, meta, l, demLine);
        asfLineMeter(l,nl);
    }

    meta_write(meta, out);
    fclose(inDEM);
    fclose(outDEM);
    return 0;
}
