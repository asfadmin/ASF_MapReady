#include <stdio.h>
#include "asf.h"
#include "asf_vector.h"

#define FILENAME_LINE_MAX   (1024)
#define FILENAME_LEN        (256)

void convert2text(char *inFile, char *outFile, format_type_t format);

int write_text(char *inFile, char *outfile, format_type_t format, int list)
{
    FILE *fpIn;
    char *filename, *outFile, *basename;

    // Convert to text
    if (inFile == NULL || outfile == NULL ||
        strlen(inFile) < 1 || strlen(outfile) < 1)
    {
        asfPrintError("write_text(): Bad inFile or outfile filename(s)\n");
    }
    outFile = (char*)MALLOC(sizeof(char)*FILENAME_LEN);
    int fileNum = 0;
    if (list) {
        filename = (char *)MALLOC(sizeof(char)*FILENAME_LINE_MAX);
        fpIn = FOPEN(inFile, "r");
        while (fgets(filename, FILENAME_LINE_MAX, fpIn)) {
            filename[strlen(filename)-1] = '\0';
            basename = get_basename(outfile);
            sprintf(outFile, "%s_%03d.csv", basename, fileNum);
            convert2text(filename, outFile, format);
            fileNum++;
        }
        FCLOSE(fpIn);
        FREE(filename);
    }
    else {
        basename = get_basename(outfile);
        sprintf(outFile, "%s.csv", basename);
        convert2text(inFile, outFile, format);
    }

    FREE(basename);
    FREE(outFile);
    return 0;
}

void convert2text(char *inFile, char *outFile, format_type_t format)
{
    FILE *outFP=NULL;

    switch(format) {
        case META:
            outFP = (FILE*)FOPEN(outFile, "w");
            meta2text(inFile, outFP);
            FCLOSE(outFP);
            break;
        case SHAPEFILE:
            shape2text(inFile, outFile);
            break;
        case GEOTIFF_META:
            outFP = (FILE*)FOPEN(outFile, "w");
            geotiff2text(inFile, outFP);
            FCLOSE(outFP);
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
