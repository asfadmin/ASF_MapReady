#include "asf_pp.h"
#include <stdlib.h>

main(int argc, char *argv[])
{
    double er, atpp;
    printf("Program: pp_kcuf <ASF L1 CEOS .L file>\n\n");
    if (argc<=1) exit(1);
    pp_get_corrected_values(argv[1], &er, &atpp);
    printf("PP Earth Radius: %.3f m\n", er);
    printf("PP seconds per azimuth line: %.9f s\n", atpp);
    return 0;
}
