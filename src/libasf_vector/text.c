#include <stdio.h>
#include "asf.h"
#include "asf_vector.h"

#define FILENAME_LINE_MAX   (1024)
#define FILENAME_LEN        (256)

void convert2text(char *inFile, FILE *outFP, format_type_t format);

int write_text(char *inFile, char *outfile, format_type_t format, int list)
{
    FILE *fpIn, *fpOut;
    char *filename, *outFile, *basename;

    // Convert to text
    if (inFile == NULL || outfile == NULL ||
        strlen(inFile) < 1 || strlen(outfile) < 1)
    {
        asfPrintError("write_text(): Bad inFile or outfile filename(s)\n");
    }
    basename = get_basename(outfile);
    outFile = (char*)MALLOC(sizeof(char)*FILENAME_LEN);
    sprintf(outFile, "%s.csv", basename);
    fpOut = FOPEN(outFile, "w");
    if (list) {
        filename = (char *)MALLOC(sizeof(char)*FILENAME_LINE_MAX);
        fpIn = FOPEN(inFile, "r");
        while (fgets(filename, FILENAME_LINE_MAX, fpIn)) {
            filename[strlen(filename)-1] = '\0';
            convert2text(filename, fpOut, format);
        }
        FCLOSE(fpIn);
        FREE(filename);
    }
    else {
        convert2text(inFile, fpOut, format);
    }

    FCLOSE(fpOut);
    FREE(basename);
    FREE(outFile);
    return 0;
}

void convert2text(char *inFile, FILE *outFP, format_type_t format)
{
    switch(format) {
        case META:
            meta2text(inFile, outFP);
            break;
        case SHAPEFILE:
            shape2text(inFile, outFP);
            break;
        case GEOTIFF_META:
            geotiff2text(inFile, outFP);
            break;
        case RGPS:
        case RGPS_GRID:
        case RGPS_WEATHER:
        case MULTIMATCH:
        case TEXT:
        case URSA:
        case KMLFILE:
        default:
            break;
    }

    return;
}
