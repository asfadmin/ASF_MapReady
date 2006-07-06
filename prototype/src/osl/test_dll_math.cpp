#include <stdio.h>
#include "osl/dll.h"
#include "osl/dll.cpp"

typedef double (*cos_fn_t)(double);
int main() {
        void *libm=osl_dll_open("libm.so");
        cos_fn_t cos_fn=(cos_fn_t)osl_dll_lookup(libm,"cos");
        double arg=0.0;
        double result=(cos_fn)(arg);
        printf(" dynamically loaded cosine of %f is %f\n"
                " cosine is at %p, libm handle is %p\n",
                arg,result, (void *)cos_fn,libm);
        osl_dll_close(libm);
        return 0;
}
