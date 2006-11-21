#include <stdio.h>
#include "banded_float_image.h"
#include "asf.h"

static void
banded_float_image_test1(int nbands, size_t size_x, size_t size_y)
{
    BandedFloatImage *bfi = banded_float_image_new(nbands, size_x, size_y);

    int i,j,k;

    // verify basic stuff
    asfRequire(bfi->nbands == nbands, "nbands didn't match");
    asfRequire(bfi->images!=NULL, "images array was NULL");
    asfRequire(bfi->images[0]->size_x==size_x, "First image wrong size y");
    asfRequire(bfi->images[0]->size_y==size_y, "First image wrong size x");

    for (i=1; i<nbands; ++i) {
        asfRequire(bfi->images[i]->size_x==size_x, "Image %d wrong size y",i);
        asfRequire(bfi->images[i]->size_y==size_y, "Image %d wrong size x",i);

    }

    int skip = 1;
    if (size_x + size_y > 1000) skip = 10;

    // pixel test: get/set
    for (i=0; i<nbands; ++i)
        for (j=0; j<size_y; j += skip)
            for (k=0; k<size_x; k += skip)
                banded_float_image_set_pixel(bfi, i, j, k, i+j+k);

    for (k=0; k<size_x; k += skip)
        for (j=0; j<size_y; j += skip)
            for (i=0; i<nbands; ++i) {
                float v = banded_float_image_get_pixel(bfi, i, j, k);
                asfRequire(v == i+j+k, "Wrong pixel value at %d,%d,%d: %f,%f",
                           i,j,k,v,(float)(i+j+k));
            }

    banded_float_image_free(bfi);
}

static void
banded_float_image_test()
{
    asfPrintStatus("Test 1...\n");
    banded_float_image_test1(5,100,100);
    asfPrintStatus("Test 2...\n");
    banded_float_image_test1(1,1000,1000);
    asfPrintStatus("Test 3...\n");
    banded_float_image_test1(10,800,800);
    asfPrintStatus("Test 4...\n");
    banded_float_image_test1(20,10,10);
    asfPrintStatus("Test 5...\n");
    banded_float_image_test1(3,2000,2000);
    asfPrintStatus("Tests passed!\n");
}

int main(int argc, char **argv)
{
    banded_float_image_test();
    return 0;
}
