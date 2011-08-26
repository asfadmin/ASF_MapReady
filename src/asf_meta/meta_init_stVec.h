#ifndef _META_INIT_STVEC_H_
#define _META_INIT_STVEC_H_

void ceos_read_stVecs(const char *fname, ceos_description *ceos, meta_parameters *meta);
void ceos_init_alos_stVec(const char *fName, ceos_description *ceos, 
			  meta_parameters *meta);

#endif // _META_INIT_STVEC_H_
