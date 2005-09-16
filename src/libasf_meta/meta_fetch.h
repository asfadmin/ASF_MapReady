#ifndef META_FETCH_H
#define META_FETCH_H

typedef enum {
  C=1, /* critical */
  N,   /* non-critical */
  P    /* populated */
} check_type_t;

typedef struct {
  int number;
  char **parameter;
  char **value;
} meta_parameter_t;

typedef struct {
  int number;
  char **parameter;
  char **check;
  char **value;
  double *range_low;
  double *range_high;
  double *moderate;    /* tolerance to 'moderate' */
  double *bad;         /* tolerance to 'bad' */
} meta_check_t;


/* Prototypes from meta_fetch.c */
double metaDouble(meta_parameter_t *meta, char *desiredParam);
int metaInt(meta_parameter_t *meta, char *desiredParam);
char metaChar(meta_parameter_t *meta, char *desiredParam);
char *metaString(meta_parameter_t *meta, char *desiredParam);
int check_parameters(char *inFile);
meta_parameter_t *extract_meta_parameters(char *inFile);
int is_empty(char *string);
void meta_put_string(FILE *meta_file,char *name,char *value,char *comment);
void meta_put_double(FILE *meta_file,char *name,double value,char *comment);
void meta_put_int(FILE *meta_file,char *name,int value,char *comment);
void meta_put_char(FILE *meta_file,char *name,char value,char *comment);

#endif
