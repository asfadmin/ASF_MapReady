
void *MALLOC(size_t size);
void FREE(void *ptr);
FILE *FOPEN(const char *file,const char *mode);
size_t FREAD(void *ptr,size_t size,size_t nitems,FILE *stream);
size_t FWRITE(const void *ptr,size_t size,size_t nitems,FILE *stream);
int FSEEK(FILE *stream,int offset,int ptrname);
int FCLOSE(FILE *stream);
int FFLUSH(FILE *stream);

void programmer_error(char *mess);
