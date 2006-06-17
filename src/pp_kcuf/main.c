#include "asf_pp.h"
#include <stdlib.h>

main(int argc, char *argv[])
{
    printf("Program: pp_kcuf <ASF L1 CEOS .L file>\n\n");
    if (argc<=1) exit(1);
    printf("PP Earth Radius: %.3f m\n", pp_get_earth_radius(argv[1]));
    return 0;
}
